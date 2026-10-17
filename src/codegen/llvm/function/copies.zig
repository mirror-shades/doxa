const ir = @import("../../hir/register/ir.zig");
const layout = @import("../layout.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

/// Whether a fixed array of type `t` holds heap values a copy must copy too:
/// strings, boxes, dynamic arrays, described structs, or fixed arrays of
/// them. Scalars and flat structs are copied with the array's bytes.
pub fn holdsHeap(l: *const layout.Layout, t: HIRType) bool {
    const element = t.Array.element.*;
    return switch (element) {
        .String, .Union, .Group => true,
        .Array => |a| if (a.size != null) holdsHeap(l, element) else true,
        .Struct => |id| !l.isFlat(id),
        else => false,
    };
}

/// Which copies (`clone`, `rehome`) are only stored inline into fixed
/// arrays as bytes holding no heap value (a flat struct, a fixed array
/// of scalars): the store's byte copy is the copy, so none is made.
pub fn storedInline(self: *FunctionEmitter) Error![]const bool {
    const f = self.f;
    const inline_use = try self.e.alloc.alloc(bool, f.values.len);
    const other_use = try self.e.alloc.alloc(bool, f.values.len);
    @memset(inline_use, false);
    @memset(other_use, false);
    const Mark = struct {
        other: []bool,
        fn use(ctx: @This(), v: ValueId) error{}!void {
            ctx.other[@intFromEnum(v)] = true;
        }
    };
    const mark = Mark{ .other = other_use };
    for (f.blocks) |block| {
        for (block.insts) |inst| switch (inst.op) {
            .debug_value => {},
            .array_set => |s| {
                if (isInlineBytes(self, s.array)) inline_use[@intFromEnum(s.value)] = true else other_use[@intFromEnum(s.value)] = true;
                other_use[@intFromEnum(s.array)] = true;
                other_use[@intFromEnum(s.index)] = true;
            },
            else => ir.forEachOperand(&inst.op, mark, Mark.use) catch unreachable,
        };
        ir.forEachTermOperand(&block.term, mark, Mark.use) catch unreachable;
    }
    const only = try self.e.alloc.alloc(bool, f.values.len);
    for (only, inline_use, other_use) |*o, i, x| o.* = i and !x;
    return only;
}

/// Whether the fixed array `array` stores its elements inline as bytes
/// that hold no heap value.
pub fn isInlineBytes(self: *FunctionEmitter, array: ValueId) bool {
    const t = self.doxaOf(array);
    if (t != .Array or t.Array.size == null) return false;
    const element = t.Array.element.*;
    return switch (element) {
        .Struct => |id| self.e.layout.isFlat(id),
        .Array => |a| a.size != null and !holdsHeap(&self.e.layout, element),
        else => false,
    };
}

/// Replace every heap element of the fixed array at `base`, of type `t`,
/// with a copy in `arena`: what makes a copy of the array its own.
pub fn deepenFixed(self: *FunctionEmitter, base: []const u8, t: HIRType, arena: []const u8) Error!void {
    const element = t.Array.element.*;
    const storage = try layout.fixedStorageType(self.e.alloc, &self.e.layout, t);
    const counter = try self.alloca("i64");
    try self.line("store i64 0, ptr {s}", .{counter});
    const head = try self.split("deepen.head");
    const body = try self.split("deepen.body");
    const done = try self.split("deepen.done");
    try self.line("br label %{s}", .{head});
    try self.enter(head);
    const i = try self.tmp("load i64, ptr {s}", .{counter});
    const more = try self.tmp("icmp ult i64 {s}, {d}", .{ i, t.Array.size.? });
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ more, body, done });
    try self.enter(body);
    const at = try self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {s}", .{ storage, base, i });
    switch (element) {
        .Array => |inner| if (inner.size != null) {
            try deepenFixed(self, at, element, arena);
        } else {
            try deepenPointer(self, at, "doxa_array_clone", arena);
        },
        .Struct => try deepenPointer(self, at, "doxa_struct_clone", arena),
        .String => {
            const s = try self.stringParts(try self.tmp("load %DoxaString, ptr {s}", .{at}));
            const copied = try self.stringCall("doxa_str_clone", "ptr {s}, ptr {s}, i64 {s}", .{ arena, s.ptr, s.len });
            try self.line("store %DoxaString {s}, ptr {s}", .{ copied, at });
        },
        .Union, .Group => try self.line("call void @doxa_clone_doxa_value(ptr {s}, ptr {s})", .{ arena, at }),
        else => unreachable, // `holdsHeap` admits no other
    }
    const next = try self.tmp("add i64 {s}, 1", .{i});
    try self.line("store i64 {s}, ptr {s}", .{ next, counter });
    try self.line("br label %{s}", .{head});
    try self.enter(done);
}

/// Replace the pointer at `at`, unless null, with `copy(arena, it)`.
pub fn deepenPointer(self: *FunctionEmitter, at: []const u8, comptime copy_fn: []const u8, arena: []const u8) Error!void {
    const p = try self.tmp("load ptr, ptr {s}", .{at});
    const none = try self.tmp("icmp eq ptr {s}, null", .{p});
    const copy_label = try self.split("deepen.copy");
    const skip = try self.split("deepen.skip");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ none, skip, copy_label });
    try self.enter(copy_label);
    const copied = try self.tmp("call ptr @" ++ copy_fn ++ "(ptr {s}, ptr {s})", .{ arena, p });
    try self.line("store ptr {s}, ptr {s}", .{ copied, at });
    try self.line("br label %{s}", .{skip});
    try self.enter(skip);
}

/// The arena a container lives in: an array header's `scope`, or the
/// word before a described struct's fields (`StructHeader.scope`). A
/// fixed array's the arena pass names statically, so none reaches here.
pub fn arenaOf(self: *FunctionEmitter, inst: *const ir.Inst, container: ValueId) Error!void {
    const c = self.name(container);
    const at = switch (self.doxaOf(container)) {
        .Array => |a| if (a.size != null)
            return self.e.faultf("names the arena of a fixed array the arena pass did not settle", .{})
        else
            try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 5", .{c}),
        .Struct => try self.tmp("getelementptr inbounds i8, ptr {s}, i64 -8", .{c}),
        else => unreachable, // the verifier admits only these
    };
    try self.def(inst, "load ptr, ptr {s}, align 8", .{at});
}

// ── Copies ──

pub const Copy = enum { clone, rehome };

/// A copy of a heap value in `arena`: always (`clone`), or only when it
/// does not already live in `arena` or an ancestor (`rehome`). A string
/// carries no arena of its own, so its `rehome` is a clone; a map is not
/// copied.
pub fn copy(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, operand: ValueId, kind: Copy) Error!void {
    const a = self.name(arena);
    const v = self.name(operand);
    const t = self.doxaOf(operand);
    switch (t) {
        .String => {
            const s = try self.stringParts(v);
            self.names[@intFromEnum(inst.result.?)] = try self.stringCall("doxa_str_clone", "ptr {s}, ptr {s}, i64 {s}", .{ a, s.ptr, s.len });
        },
        .Array => |arr| {
            if (arr.size != null) {
                // A fixed array is copied whole into `arena`, and each
                // heap element it holds after it.
                const bytes = layout.fixedBytes(&self.e.layout, t);
                try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ a, bytes });
                try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ self.name(inst.result.?), v, bytes });
                if (holdsHeap(&self.e.layout, t)) try deepenFixed(self, self.name(inst.result.?), t, a);
            } else {
                try self.def(inst, "call ptr @doxa_array_{s}(ptr {s}, ptr {s})", .{ @tagName(kind), a, v });
            }
        },
        .Struct => |id| {
            const words = self.e.layout.skippedWords(t);
            if (words != 0) {
                const which = if (kind == .clone) "clone_scalar" else "rehome_scalar";
                try self.def(inst, "call ptr @doxa_struct_{s}(ptr {s}, i64 {d}, ptr {s})", .{ which, a, words, v });
            } else {
                _ = id;
                try self.def(inst, "call ptr @doxa_struct_{s}(ptr {s}, ptr {s})", .{ @tagName(kind), a, v });
            }
        },
        .Union, .Group => {
            const slot = try self.spillValue(v);
            try self.line("call void @doxa_clone_doxa_value(ptr {s}, ptr {s})", .{ a, slot });
            try self.def(inst, "load %DoxaValue, ptr {s}", .{slot});
        },
        // A map lives in the root arena (the verifier's rule 4), so the
        // arena pass keeps every copy of one as the map itself.
        else => return self.e.faultf("copies a {s}", .{@tagName(t)}),
    }
}
