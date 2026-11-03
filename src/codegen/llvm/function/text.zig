const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const ValueId = ir.ValueId;

// ── Text ──

/// `values`, the parts of a concatenation, as an array of `%DoxaValue`: what
/// `doxa_str_concat` and `doxa_str_append` read (`plan/strings.md`, Phase 3).
/// A string is its pointer and length; a scalar is its bits under its tag.
/// The array's address.
pub fn parts(self: *FunctionEmitter, values: []const ValueId) Error![]const u8 {
    const array_type = try std.fmt.allocPrint(self.e.alloc, "[{d} x %DoxaValue]", .{values.len});
    const array = try self.alloca(array_type);
    for (values, 0..) |v, i| {
        const name = self.name(v);
        const tag: rt.DoxaTag, const bits: []const u8, const len: []const u8 = switch (self.doxaOf(v)) {
            .String => blk: {
                const s = try self.stringParts(name);
                break :blk .{ .String, try self.tmp("ptrtoint ptr {s} to i64", .{s.ptr}), s.len };
            },
            .Int => .{ .Int, name, "0" },
            .Float => .{ .Float, try self.tmp("bitcast double {s} to i64", .{name}), "0" },
            .Byte => .{ .Byte, try self.tmp("zext i8 {s} to i64", .{name}), "0" },
            .Tetra => .{ .Tetra, try self.tmp("zext i2 {s} to i64", .{name}), "0" },
            else => unreachable, // the verifier admits only strings and scalars
        };
        const tagged = try self.tmp("insertvalue %DoxaValue {{ i32 {d}, i32 0, i64 0, i64 0 }}, i64 {s}, 2", .{ @intFromEnum(tag), bits });
        const part = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 3", .{ tagged, len });
        const slot = try self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {d}", .{ array_type, array, i });
        try self.line("store %DoxaValue {s}, ptr {s}", .{ part, slot });
    }
    return array;
}

/// The text of a value, in arena `arena`.
pub fn toString(self: *FunctionEmitter, arena: []const u8, operand: ValueId) Error![]const u8 {
    const v = self.name(operand);
    return switch (self.doxaOf(operand)) {
        .String => v,
        .Int => self.stringCall("doxa_int_to_string", "ptr {s}, i64 {s}", .{ arena, v }),
        .Float => self.stringCall("doxa_float_to_string", "ptr {s}, double {s}", .{ arena, v }),
        .Byte => self.stringCall("doxa_byte_to_string", "ptr {s}, i64 {s}", .{ arena, try self.tmp("zext i8 {s} to i64", .{v}) }),
        .Tetra => self.stringCall("doxa_tetra_to_string", "ptr {s}, i64 {s}", .{ arena, try self.tmp("zext i2 {s} to i64", .{v}) }),
        .Nothing => self.stringCall("doxa_nothing_to_string", "ptr {s}", .{arena}),
        .Enum => |id| self.stringCall("doxa_enum_to_string", "ptr {s}, ptr {s}, i64 {s}", .{ arena, try self.e.enumDesc(id), v }),
        .Struct => self.stringCall("doxa_struct_to_string", "ptr {s}, ptr {s}", .{ arena, v }),
        .Array => |a| if (a.size != null)
            self.e.faultf("renders a fixed array; the generator converts it first", .{})
        else
            self.stringCall("doxa_array_to_string", "ptr {s}, ptr {s}", .{ arena, v }),
        .Union, .Group => self.stringCall("doxa_value_to_string", "ptr {s}, ptr {s}", .{ arena, try self.spillValue(v) }),
        .Function => self.e.stringConstant("<function>"),
        .Unknown, .Poison => unreachable,
    };
}
