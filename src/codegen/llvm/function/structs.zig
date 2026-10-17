const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const layout = @import("../layout.zig");
const boxes = @import("boxes.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

/// A fixed array of a struct with a descriptor holds pointers, each to a
/// zeroed struct of its own, registered in `arena`.
/// Nested fixed arrays are inline, so the struct pointers of every
/// dimension are one contiguous run.
pub fn defaultStructs(self: *FunctionEmitter, buffer: []const u8, arena: []const u8, ty: HIRType) Error!void {
    var count: u64 = 1;
    var cursor = ty;
    while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) count *= cursor.Array.size.?;
    if (cursor != .Struct or self.e.layout.isFlat(cursor.Struct)) return;
    const template = try defaultStruct(self, arena, cursor.Struct);
    try self.line("call void @doxa_fixed_structs_default(ptr {s}, ptr {s}, i64 {d}, ptr {s})", .{ arena, buffer, count, template });
}

/// A struct's default value, built in `arena`: zero for a scalar, `""`
/// for a string, and `nothing` boxed for a union or group field — the
/// member a declaration without an initializer holds.
pub fn defaultStruct(self: *FunctionEmitter, arena: []const u8, id: ir.StructId) Error![]const u8 {
    const field_types = self.e.layout.struct_fields[id];
    const object = try zeroedStruct(self, arena, id);
    for (field_types, 0..) |t, i| {
        if (!t.isBoxed()) continue;
        const header = try self.e.boxHeader(t);
        const index = boxes.memberIndex(self, t, .Nothing) catch return self.e.faultf("a {s} field without `nothing` has no default", .{@tagName(t)});
        const cell = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 24, i64 8)", .{arena});
        try self.line("store %DoxaValue {{ i32 {d}, i32 {d}, i64 0, i64 0 }}, ptr {s}", .{ @intFromEnum(rt.DoxaTag.Nothing), header | index, cell });
        const bits = try self.tmp("ptrtoint ptr {s} to i64", .{cell});
        try self.line("store i64 {s}, ptr {s}", .{ bits, try fieldPtr(self, object, id, layout.fieldOffset(field_types, @intCast(i))) });
    }
    return object;
}

/// A zeroed struct in `arena`. A described struct comes from the runtime
/// behind its header (its descriptor and owning arena); a
/// descriptor-skipped one is its bare words.
pub fn zeroedStruct(self: *FunctionEmitter, arena: []const u8, id: ir.StructId) Error![]const u8 {
    if (!self.e.layout.skip_descriptor[id]) {
        return self.tmp("call ptr @doxa_struct_new(ptr {s}, ptr {s})", .{ arena, try self.e.structDesc(id) });
    }
    const bytes = @as(u64, @max(1, layout.structWords(self.e.layout.struct_fields[id]))) * 8;
    const object = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ arena, bytes });
    try self.line("call void @llvm.memset.p0.i64(ptr align 8 {s}, i8 0, i64 {d}, i1 false)", .{ object, bytes });
    return object;
}

// ── Structs ──

pub fn structType(self: *FunctionEmitter, id: ir.StructId) Error![]const u8 {
    return std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{@max(1, self.e.layout.structWordsOf(id))});
}

pub fn fieldPtr(self: *FunctionEmitter, object: []const u8, id: ir.StructId, word: u32) Error![]const u8 {
    return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {d}", .{ try structType(self, id), object, word });
}

pub fn structNew(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, fields: []const ValueId) Error!void {
    const id = self.f.typeOf(inst.result.?).doxa.Struct;
    const field_types = self.e.layout.struct_fields[id];
    if (self.e.layout.skip_descriptor[id]) {
        const words = @max(1, layout.structWords(field_types));
        try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ self.name(arena), words * 8 });
    } else {
        try self.def(inst, "call ptr @doxa_struct_new(ptr {s}, ptr {s})", .{ self.name(arena), try self.e.structDesc(id) });
    }
    const object = self.name(inst.result.?);
    for (fields, field_types, 0..) |v, t, i| {
        try storeField(self, object, self.name(arena), id, @intCast(i), t, self.name(v));
    }
}

/// Store `v` into field `index` of `object`, which lives in `scope`: a
/// string is copied there, a box is given a cell there.
pub fn storeField(self: *FunctionEmitter, object: []const u8, scope: []const u8, id: ir.StructId, index: u32, t: HIRType, v: []const u8) Error!void {
    const field_types = self.e.layout.struct_fields[id];
    const word = layout.fieldOffset(field_types, index);
    switch (t) {
        .String => {
            const s = try self.stringParts(v);
            try self.line("store ptr {s}, ptr {s}", .{ s.ptr, try fieldPtr(self, object, id, word) });
            try self.line("store i64 {s}, ptr {s}", .{ s.len, try fieldPtr(self, object, id, word + 1) });
        },
        .Union, .Group => {
            const cell = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 24, i64 8)", .{scope});
            try self.line("store %DoxaValue {s}, ptr {s}", .{ v, cell });
            const bits = try self.tmp("ptrtoint ptr {s} to i64", .{cell});
            try self.line("store i64 {s}, ptr {s}", .{ bits, try fieldPtr(self, object, id, word) });
        },
        else => try self.line("store i64 {s}, ptr {s}", .{ try boxes.storageBits(self, v, t), try fieldPtr(self, object, id, word) }),
    }
}

pub fn fieldGet(self: *FunctionEmitter, inst: *const ir.Inst, object: ValueId, index: u32) Error!void {
    const id = self.doxaOf(object).Struct;
    const t = self.e.layout.struct_fields[id][index];
    const word = layout.fieldOffset(self.e.layout.struct_fields[id], index);
    const o = self.name(object);
    switch (t) {
        .String => {
            const p = try self.tmp("load ptr, ptr {s}", .{try fieldPtr(self, o, id, word)});
            const l = try self.tmp("load i64, ptr {s}", .{try fieldPtr(self, o, id, word + 1)});
            self.names[@intFromEnum(inst.result.?)] = try self.makeString(p, l);
        },
        .Union, .Group => {
            const bits = try self.tmp("load i64, ptr {s}", .{try fieldPtr(self, o, id, word)});
            const cell = try self.tmp("inttoptr i64 {s} to ptr", .{bits});
            try self.def(inst, "load %DoxaValue, ptr {s}", .{cell});
        },
        else => {
            const bits = try self.tmp("load i64, ptr {s}", .{try fieldPtr(self, o, id, word)});
            self.names[@intFromEnum(inst.result.?)] = try boxes.fromBits(self, bits, t);
        },
    }
}

/// A field store after construction. The value is already placed in the
/// struct's arena; a union field's cell is allocated there too, which a
/// struct with a heap field keeps in its header.
pub fn fieldSet(self: *FunctionEmitter, object: ValueId, index: u32, value: ValueId) Error!void {
    const id = self.doxaOf(object).Struct;
    const t = self.e.layout.struct_fields[id][index];
    const o = self.name(object);
    const scope = switch (t) {
        .Union, .Group => blk: {
            const at = try self.tmp("getelementptr inbounds i8, ptr {s}, i64 -8", .{o});
            break :blk try self.tmp("load ptr, ptr {s}, align 8", .{at});
        },
        else => "null",
    };
    try storeField(self, o, scope, id, index, t, self.name(value));
}
