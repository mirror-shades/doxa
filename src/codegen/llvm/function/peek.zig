const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

// ── Peek ──

/// `v?`: the location, name and type, then the value, on stderr.
pub fn peek(self: *FunctionEmitter, operand: ValueId, display: ir.PeekDisplay, span: ir.SrcSpan) Error!void {
    const t = self.doxaOf(operand);
    const info = try self.alloca("%DoxaPeekInfo");
    const fields = [_][]const u8{ "file", "name", "type", "members" };
    const file = try self.e.string(self.e.module.program.files[span.module]);
    const label = try peekLabel(self, t, display);
    const values = [_][]const u8{
        file,
        if (display.path) |p| try self.e.string(p) else "null",
        try self.e.string(label),
        if (display.members) |m| try self.e.nameList(m) else "null",
    };
    for (fields, values, 0..) |_, v, i| {
        const at = try self.tmp("getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 {d}", .{ info, i });
        try self.line("store ptr {s}, ptr {s}", .{ v, at });
    }
    const count: usize = if (display.members) |m| m.len else 0;
    const active = if (t.isBoxed()) try activeMember(self, self.name(operand), display) else "-1";
    const ints = [_][]const u8{
        try std.fmt.allocPrint(self.e.alloc, "{d}", .{count}),
        active,
        "1",
        try std.fmt.allocPrint(self.e.alloc, "{d}", .{span.line}),
        try std.fmt.allocPrint(self.e.alloc, "{d}", .{span.column}),
    };
    for (ints, 4..) |v, i| {
        const at = try self.tmp("getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 {d}", .{ info, i });
        try self.line("store i32 {s}, ptr {s}", .{ v, at });
    }
    try self.line("call void @doxa_debug_peek(ptr {s})", .{info});
    try printValue(self, self.name(operand), t);
    try self.line("call void @doxa_peek_end()", .{});
}

/// The marker slot of the member a box holds: its member index, through
/// `member_slots` when written groups collapse members.
pub fn activeMember(self: *FunctionEmitter, v: []const u8, display: ir.PeekDisplay) Error![]const u8 {
    const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{v});
    const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
    const slots = display.member_slots orelse return index;
    var acc: []const u8 = "-1";
    for (slots, 0..) |slot, member| {
        const is = try self.tmp("icmp eq i32 {s}, {d}", .{ index, member });
        acc = try self.tmp("select i1 {s}, i32 {d}, i32 {s}", .{ is, slot, acc });
    }
    return acc;
}

/// The type a peek shows for a value of `t`.
pub fn peekLabel(self: *FunctionEmitter, t: HIRType, display: ir.PeekDisplay) Error![]const u8 {
    const p = &self.e.module.program;
    return switch (t) {
        .Int => "int",
        .Float => "float",
        .Byte => "byte",
        .Tetra => "tetra",
        .String => "string",
        .Nothing => "nothing",
        .Enum => |id| p.enum_names[id],
        .Struct => |id| p.struct_names[id],
        .Array => try typeText(self, t),
        .Union => if (display.members) |m| m[0] else "union",
        .Group => |gid| p.group_names[gid],
        .Function => "function",
        .Unknown, .Poison => unreachable,
    };
}

pub fn typeText(self: *FunctionEmitter, t: HIRType) Error![]const u8 {
    const p = &self.e.module.program;
    return switch (t) {
        .Array => |a| std.fmt.allocPrint(self.e.alloc, "{s}[]", .{try typeText(self, a.element.*)}),
        .Struct => |id| p.struct_names[id],
        .Enum => "enum",
        .Union => "union",
        .Group => |gid| p.group_names[gid],
        else => try peekLabel(self, t, .{ .path = null }),
    };
}

pub fn printValue(self: *FunctionEmitter, v: []const u8, t: HIRType) Error!void {
    switch (t) {
        .Int => try self.line("call void @doxa_print_i64(i64 {s})", .{v}),
        .Float => try self.line("call void @doxa_print_f64(double {s})", .{v}),
        .Byte => try self.line("call void @doxa_print_byte(i64 {s})", .{try self.tmp("zext i8 {s} to i64", .{v})}),
        .Tetra => try self.line("call void @doxa_print_tetra(i64 {s})", .{try self.tmp("zext i2 {s} to i64", .{v})}),
        .String => {
            const s = try self.stringParts(v);
            try self.line("call void @doxa_peek_string(ptr {s}, i64 {s})", .{ s.ptr, s.len });
        },
        .Nothing => {
            const text = try self.e.string("nothing");
            try self.line("call void @doxa_write_cstr(ptr {s}, i64 7)", .{text});
        },
        .Enum => |id| try self.line("call void @doxa_print_enum(ptr {s}, i64 {s})", .{ try self.e.enumDesc(id), v }),
        .Union, .Group => try self.line("call void @doxa_print_value(ptr {s})", .{try self.spillValue(v)}),
        .Struct => {
            const bits = try self.tmp("ptrtoint ptr {s} to i64", .{v});
            const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {d}, 0", .{@intFromEnum(rt.DoxaTag.Struct)});
            const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 0, 1", .{v0});
            const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, bits });
            const boxed = try self.tmp("insertvalue %DoxaValue {s}, i64 0, 3", .{v2});
            try self.line("call void @doxa_print_value(ptr {s})", .{try self.spillValue(boxed)});
        },
        .Array => |a| {
            if (a.size != null) return self.e.faultf("peeks a fixed array; the generator converts it first", .{});
            try self.line("call void @doxa_print_array_hdr(ptr {s})", .{v});
        },
        .Function => try self.line("call void @doxa_write_cstr(ptr {s}, i64 10)", .{try self.e.string("<function>")}),
        .Unknown, .Poison => unreachable,
    }
}
