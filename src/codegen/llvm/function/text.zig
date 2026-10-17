const ir = @import("../../hir/register/ir.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const ValueId = ir.ValueId;

// ── Text ──

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
