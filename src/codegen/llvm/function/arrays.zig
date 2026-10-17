const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const layout = @import("../layout.zig");
const boxes = @import("boxes.zig");
const structs = @import("structs.zig");
const copies = @import("copies.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const ValueId = ir.ValueId;

// ── Arrays ──

pub fn arrayLen(self: *FunctionEmitter, array: []const u8) Error![]const u8 {
    const slot = try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 1", .{array});
    return self.tmp("load i64, ptr {s}", .{slot});
}

pub fn arrayNew(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, length: ?ValueId) Error!void {
    const ty = self.f.typeOf(inst.result.?).doxa;
    const element = ty.Array.element.*;
    if (ty.Array.size != null) {
        // A fixed array is a zeroed flat buffer in its arena.
        const bytes = layout.fixedBytes(&self.e.layout, ty);
        try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ self.name(arena), bytes });
        try self.line("call void @llvm.memset.p0.i64(ptr align 8 {s}, i8 0, i64 {d}, i1 false)", .{ self.name(inst.result.?), bytes });
        try structs.defaultStructs(self, self.name(inst.result.?), self.name(arena), ty);
        return;
    }
    try self.def(inst, "call ptr @doxa_array_new(ptr {s}, i64 {d}, i64 {d}, i64 {s})", .{
        self.name(arena), layout.elementSize(element), layout.elementTag(element), self.name(length.?),
    });
    const words = self.e.layout.skippedWords(element);
    if (words != 0) try self.line("call void @doxa_array_set_elem_words(ptr {s}, i64 {d})", .{ self.name(inst.result.?), words });
}

pub fn arrayFromFixed(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, operand: ValueId) Error!void {
    const from = self.doxaOf(operand);
    // Dimensions, outermost first, and the innermost element.
    var sizes: std.ArrayListUnmanaged(u64) = .empty;
    var cursor = from;
    while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) {
        try sizes.append(self.e.alloc, cursor.Array.size.?);
    }
    const element = cursor;
    if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
        if (sizes.items.len != 1) return self.e.faultf("converts a nested fixed array of flat structs", .{});
        try self.def(inst, "call ptr @doxa_array_from_fixed_structs(ptr {s}, ptr {s}, i64 {d}, i64 {d})", .{
            self.name(arena), self.name(operand), sizes.items[0], self.e.layout.structWordsOf(element.Struct),
        });
        return;
    }
    const dims = try self.alloca(try std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{sizes.items.len}));
    for (sizes.items, 0..) |n, i| {
        const at = try self.tmp("getelementptr inbounds [{d} x i64], ptr {s}, i64 0, i64 {d}", .{ sizes.items.len, dims, i });
        try self.line("store i64 {d}, ptr {s}", .{ n, at });
    }
    try self.def(inst, "call ptr @doxa_array_from_fixed(ptr {s}, ptr {s}, ptr {s}, i64 {d}, i64 {d}, i64 {d})", .{
        self.name(arena), self.name(operand), dims, sizes.items.len, layout.elementSize(element), layout.elementTag(element),
    });
}

/// Copy `array`'s elements back into the fixed array `fixed`, as many as
/// both hold: the inverse of `array.from_fixed`.
pub fn arrayCopyToFixed(self: *FunctionEmitter, fixed: ValueId, array: ValueId, arena: ValueId) Error!void {
    try copyBackElements(self, fixed, array);
    // The elements arrive as the callee left them, wherever it placed
    // them: each heap one is then copied into the fixed array's arena.
    const t = self.doxaOf(fixed);
    if (copies.holdsHeap(&self.e.layout, t)) try copies.deepenFixed(self, self.name(fixed), t, self.name(arena));
}

pub fn copyBackElements(self: *FunctionEmitter, fixed: ValueId, array: ValueId) Error!void {
    const t = self.doxaOf(fixed);
    var sizes: std.ArrayListUnmanaged(u64) = .empty;
    var cursor = t;
    while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) {
        try sizes.append(self.e.alloc, cursor.Array.size.?);
    }
    const element = cursor;
    if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
        if (sizes.items.len != 1) return self.e.faultf("copies into a nested fixed array of flat structs", .{});
        try self.line("call void @doxa_array_copy_to_fixed_structs(ptr {s}, ptr {s}, i64 {d}, i64 {d})", .{ self.name(array), self.name(fixed), sizes.items[0], self.e.layout.structWordsOf(element.Struct) });
        return;
    }
    const dims = try self.alloca(try std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{sizes.items.len}));
    for (sizes.items, 0..) |n, i| {
        const at = try self.tmp("getelementptr inbounds [{d} x i64], ptr {s}, i64 0, i64 {d}", .{ sizes.items.len, dims, i });
        try self.line("store i64 {d}, ptr {s}", .{ n, at });
    }
    try self.line("call void @doxa_array_copy_to_fixed(ptr {s}, ptr {s}, ptr {s}, i64 {d}, i64 {d})", .{ self.name(array), self.name(fixed), dims, sizes.items.len, layout.elementSize(element) });
}

/// The address of element `index` of `array`.
/// Which indexes a bounds check admits: an element's, `0 <= i < length`,
/// or an insertion point's, `0 <= i <= length`.
pub const Bound = enum { element, insertion };

/// Fault unless `index` is in bounds for `length` (`fixed`, when the
/// length is a constant). One unsigned compare also rejects a negative
/// index. Out-of-bounds access is defined in every mode
/// (`docs/performance.md`); the check is dropped only where the ranges
/// prove the index in bounds.
pub fn boundsCheck(self: *FunctionEmitter, index: ValueId, length: []const u8, fixed: ?u64, bound: Bound) Error!void {
    const r = self.ranges[@intFromEnum(index)];
    if (fixed) |n| {
        const limit: i64 = @intCast(if (bound == .element) n else n + 1);
        if (r.lo >= 0 and r.hi < limit) return;
    }
    const i = self.name(index);
    const out = try self.tmp("icmp {s} i64 {s}, {s}", .{ if (bound == .element) "uge" else "ugt", i, length });
    const fault_label = try self.split("oob.fault");
    const cont = try self.split("oob.cont");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ out, fault_label, cont });
    try self.enter(fault_label);
    try self.fault(.index_out_of_bounds, null, i, length);
    try self.enter(cont);
}

/// The bounds check of an element access to `array` at `index`.
pub fn elementCheck(self: *FunctionEmitter, array: ValueId, index: ValueId) Error!void {
    const t = self.doxaOf(array);
    if (t.Array.size) |n| {
        return boundsCheck(self, index, try std.fmt.allocPrint(self.e.alloc, "{d}", .{n}), n, .element);
    }
    return boundsCheck(self, index, try arrayLen(self, self.name(array)), null, .element);
}

pub fn elementPtr(self: *FunctionEmitter, array: ValueId, index: []const u8) Error![]const u8 {
    const t = self.doxaOf(array);
    if (t.Array.size != null) {
        return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {s}", .{ try layout.fixedStorageType(self.e.alloc, &self.e.layout, t), self.name(array), index });
    }
    const data_slot = try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 0", .{self.name(array)});
    const data = try self.tmp("load ptr, ptr {s}", .{data_slot});
    return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 {s}", .{ layout.elementStorageType(t.Array.element.*), data, index });
}

/// The address is computed in the IR, behind an in-IR bounds check, so
/// LLVM can hoist, unroll and vectorize the loop.
pub fn arrayGet(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: ValueId) Error!void {
    try elementCheck(self, array, index);
    const t = self.doxaOf(array);
    const element = t.Array.element.*;
    const at = try elementPtr(self, array, self.name(index));
    const fixed = t.Array.size != null;
    // An inline element — a nested fixed array, a flat struct — is its
    // address.
    if (fixed and ((element == .Array and element.Array.size != null) or (element == .Struct and self.e.layout.isFlat(element.Struct)))) {
        self.names[@intFromEnum(inst.result.?)] = at;
        return;
    }
    switch (element) {
        .Tetra => {
            const byte = try self.tmp("load i8, ptr {s}", .{at});
            try self.def(inst, "trunc i8 {s} to i2", .{byte});
        },
        .Nothing => self.names[@intFromEnum(inst.result.?)] = "0",
        else => try self.def(inst, "load {s}, ptr {s}", .{ layout.elementStorageType(element), at }),
    }
}

pub fn arraySet(self: *FunctionEmitter, array: ValueId, index: ValueId, value: ValueId) Error!void {
    try elementCheck(self, array, index);
    const t = self.doxaOf(array);
    if (t.Array.size == null) return arraySetDynamic(self, self.name(array), self.name(index), value);
    const element = t.Array.element.*;
    const at = try elementPtr(self, array, self.name(index));
    const v = self.name(value);
    if (element == .Array and element.Array.size != null) {
        try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ at, v, layout.fixedBytes(&self.e.layout, element) });
    } else if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
        try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ at, v, @as(u64, self.e.layout.structWordsOf(element.Struct)) * 8 });
    } else switch (element) {
        .Tetra => try self.line("store i8 {s}, ptr {s}", .{ try self.tmp("zext i2 {s} to i8", .{v}), at }),
        .Nothing => {},
        else => try self.line("store {s} {s}, ptr {s}", .{ layout.elementStorageType(element), v, at }),
    }
}

/// An element store into a dynamic array goes through the runtime, which
/// grows the array and re-homes a heap element into the array's arena.
pub fn arraySetDynamic(self: *FunctionEmitter, array: []const u8, index: []const u8, value: ValueId) Error!void {
    const element = self.doxaOf(value);
    const v = self.name(value);
    switch (element) {
        .String => {
            const s = try self.stringParts(v);
            try self.line("call void @doxa_array_set_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ array, index, s.ptr, s.len });
        },
        .Union, .Group => try self.line("call void @doxa_array_set_value(ptr {s}, i64 {s}, ptr {s})", .{ array, index, try self.spillValue(v) }),
        else => try self.line("call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})", .{ array, index, try boxes.storageBits(self, v, element) }),
    }
}

pub fn arrayPop(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
    const array = self.name(operand);
    const len = try arrayLen(self, array);
    const empty = try self.tmp("icmp eq i64 {s}, 0", .{len});
    const fault_label = try self.split("pop.fault");
    const cont = try self.split("pop.cont");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ empty, fault_label, cont });
    try self.enter(fault_label);
    try self.fault(.pop_empty, null, "0", "0");
    try self.enter(cont);
    const last = try self.tmp("sub i64 {s}, 1", .{len});
    try arrayRemoveAt(self, inst, operand, last);
}

pub fn arrayRemove(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: ValueId) Error!void {
    try boundsCheck(self, index, try arrayLen(self, self.name(array)), null, .element);
    try arrayRemoveAt(self, inst, array, self.name(index));
}

pub fn arrayRemoveAt(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: []const u8) Error!void {
    const element = self.doxaOf(array).Array.element.*;
    const a = self.name(array);
    switch (element) {
        .String => {
            const slots = try self.stringSlots();
            try self.line("call void @doxa_array_remove_str(ptr {s}, i64 {s}, ptr {s}, ptr {s})", .{ a, index, slots.ptr, slots.len });
            self.names[@intFromEnum(inst.result.?)] = try self.loadString(slots.ptr, slots.len);
        },
        .Union, .Group => {
            const slot = try self.alloca("%DoxaValue");
            try self.line("call void @doxa_array_remove_value(ptr {s}, i64 {s}, ptr {s})", .{ a, index, slot });
            try self.def(inst, "load %DoxaValue, ptr {s}", .{slot});
        },
        else => {
            const bits = try self.tmp("call i64 @doxa_array_remove(ptr {s}, i64 {s})", .{ a, index });
            self.names[@intFromEnum(inst.result.?)] = try boxes.fromBits(self, bits, element);
        },
    }
}

pub fn arrayInsert(self: *FunctionEmitter, array: ValueId, index: ValueId, value: ValueId) Error!void {
    try boundsCheck(self, index, try arrayLen(self, self.name(array)), null, .insertion);
    const element = self.doxaOf(value);
    const a = self.name(array);
    const i = self.name(index);
    const v = self.name(value);
    switch (element) {
        .String => {
            const s = try self.stringParts(v);
            try self.line("call void @doxa_array_insert_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a, i, s.ptr, s.len });
        },
        .Union, .Group => try self.line("call void @doxa_array_insert_value(ptr {s}, i64 {s}, ptr {s})", .{ a, i, try self.spillValue(v) }),
        else => try self.line("call void @doxa_array_insert(ptr {s}, i64 {s}, i64 {s})", .{ a, i, try boxes.storageBits(self, v, element) }),
    }
}
