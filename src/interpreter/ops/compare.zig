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

    // debug print removed

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
    _ = vm;
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
                const name_match = std.mem.eql(u8, a_val.variant_name, b_val.variant_name);
                const type_match = std.mem.eql(u8, a_val.type_name, b_val.type_name);
                const result = name_match and type_match;
                return result;
            },
            else => false,
        },
        .array, .struct_instance, .map => false,
        .storage_id_ref => |a_storage_id| switch (b.value) {
            .storage_id_ref => |b_storage_id| a_storage_id == b_storage_id,
            else => false,
        },
    };
}

fn compareLess(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm;
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
        .nothing => ErrorList.TypeError,
        .tetra, .array, .struct_instance, .map, .enum_variant, .storage_id_ref => ErrorList.TypeError,
    };
}

fn compareGreater(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm;
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val > b_val,
            .byte => |b_val| a_val > b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
            else => ErrorList.TypeError,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val > b_val,
            .byte => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
            .int => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
            else => ErrorList.TypeError,
        },
        .byte => |a_val| switch (b.value) {
            .byte => |b_val| a_val > b_val,
            .int => |b_val| a_val > b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
            else => ErrorList.TypeError,
        },
        .string => |a_val| switch (b.value) {
            .string => |b_val| blk: {
                const result = std.mem.order(u8, a_val, b_val) == .gt;
                break :blk result;
            },
            else => ErrorList.TypeError,
        },
        .nothing => ErrorList.TypeError,
        .tetra, .array, .struct_instance, .map, .enum_variant, .storage_id_ref => ErrorList.TypeError,
    };
}
