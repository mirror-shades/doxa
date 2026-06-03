const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub fn exec(vm: anytype, c: anytype) !void {
    const b = try vm.stack.pop();
    const a_val = try vm.stack.pop();

    const result = switch (c.op) {
        .Eq => try compareEqual(vm, a_val, b),
        .Ne => !(try compareEqual(vm, a_val, b)),
        .Lt => try compareLess(vm, a_val, b),
        .Le => !(try compareGreater(vm, a_val, b)),
        .Gt => try compareGreater(vm, a_val, b),
        .Ge => !(try compareLess(vm, a_val, b)),
    };

    try vm.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
}

fn compareEqual(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm; // TODO: vm access not needed for pure comparison
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val == b_val,
            .byte => |b_val| a_val == b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
            else => false,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val == b_val,
            .byte => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
            .int => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
            else => false,
        },
        .tetra => |a_val| switch (b.value) {
            .tetra => |b_val| a_val == b_val,
            else => false,
        },
        .string => |a_val| switch (b.value) {
            .string => |b_val| blk: {
                const result = std.mem.eql(u8, a_val, b_val);
                break :blk result;
            },
            else => false,
        },
        .nothing => switch (b.value) {
            .nothing => true,
            else => false,
        },
        .byte => |a_val| switch (b.value) {
            .byte => |b_val| a_val == b_val,
            .int => |b_val| a_val == b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
            else => false,
        },
        .enum_variant => |a_val| switch (b.value) {
            .enum_variant => |b_val| {
                return a_val.variant_index == b_val.variant_index and
                    std.mem.eql(u8, a_val.type_name, b_val.type_name) and
                    std.mem.eql(u8, a_val.variant_name, b_val.variant_name);
            },
            else => false,
        },
        .array, .struct_instance, .map, .group_instance, .union_instance => false,
        .storage_id_ref => |a_storage_id| switch (b.value) {
            .storage_id_ref => |b_storage_id| a_storage_id == b_storage_id,
            else => false,
        },
    };
}

fn compareLess(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm; // TODO: vm access not needed for pure comparison
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val < b_val,
            .byte => |b_val| a_val < b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
            else => ErrorList.TypeError,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val < b_val,
            .byte => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
            .int => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
            else => ErrorList.TypeError,
        },
        .byte => |a_val| switch (b.value) {
            .byte => |b_val| a_val < b_val,
            .int => |b_val| a_val < b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
            else => ErrorList.TypeError,
        },
        .string => |a_val| switch (b.value) {
            .string => |b_val| blk: {
                const result = std.mem.order(u8, a_val, b_val) == .lt;
                break :blk result;
            },
            else => ErrorList.TypeError,
        },
        // compareLess: complex types
        .nothing => ErrorList.TypeError,
        .tetra, .array, .struct_instance, .map, .enum_variant, .group_instance, .union_instance, .storage_id_ref => ErrorList.TypeError,
    };
}

fn compareGreater(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm; // TODO: vm access not needed for pure comparison
    return switch (a.value) {
        .int => |a_int| switch (b.value) {
            .int => |b_int| a_int > b_int,
            .float => |b_float| @as(f64, @floatFromInt(a_int)) > b_float,
            .byte => |b_byte| a_int > @as(i64, @intCast(b_byte)),
            else => ErrorList.TypeError,
        },
        .float => |a_float| switch (b.value) {
            .float => |b_float| a_float > b_float,
            .int => |b_int| a_float > @as(f64, @floatFromInt(b_int)),
            .byte => |b_byte| a_float > @as(f64, @floatFromInt(@as(i64, @intCast(b_byte)))),
            else => ErrorList.TypeError,
        },
        .byte => |a_byte| switch (b.value) {
            .byte => |b_byte| a_byte > b_byte,
            .int => |b_int| @as(i64, @intCast(a_byte)) > b_int,
            .float => |b_float| @as(f64, @floatFromInt(@as(i64, @intCast(a_byte)))) > b_float,
            else => ErrorList.TypeError,
        },
        .string => |a_string| switch (b.value) {
            .string => |b_string| std.mem.order(u8, a_string, b_string) == .gt,
            else => ErrorList.TypeError,
        },
        // compareGreater: complex types
        .nothing => ErrorList.TypeError,
        .tetra, .array, .struct_instance, .map, .enum_variant, .group_instance, .union_instance, .storage_id_ref => ErrorList.TypeError,
    };
}
