const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

// Execute the Compare instruction. Accepts the VM as `anytype` to avoid import cycles.
pub fn exec(vm: anytype, c: anytype) !void {
    const b = try vm.stack.pop();
    const a_val = try vm.stack.pop();

    const result = switch (c.op) {
        .Eq => try compareEqual(vm, a_val, b),
        .Ne => !(try compareEqual(vm, a_val, b)),
        .Lt => try compareLess(vm, a_val, b),
        .Le => !(try compareGreater(vm, a_val, b)), // a <= b  ≡  !(a > b)
        .Gt => try compareGreater(vm, a_val, b),
        .Ge => !(try compareLess(vm, a_val, b)), // a >= b  ≡  !(a < b)
    };

    try vm.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
}

/// Enhanced comparison with mixed int/float support from old VM
fn compareEqual(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm;
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val == b_val,
            .byte => |b_val| a_val == b_val,
            // Mixed int/float comparison (from old VM)
            .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
            else => false,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val == b_val,
            .byte => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
            // Mixed float/int comparison (from old VM)
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
            .enum_variant => |b_val| std.mem.eql(u8, a_val.variant_name, b_val.variant_name) and std.mem.eql(u8, a_val.type_name, b_val.type_name),
            else => false,
        },
        // Complex types - basic equality for now
        .array, .struct_instance, .map => false, // Complex equality not implemented yet
        .storage_id_ref => |a_storage_id| switch (b.value) {
            .storage_id_ref => |b_storage_id| a_storage_id == b_storage_id,
            else => false,
        },
    };
}

/// Enhanced less-than with mixed int/float support from old VM
fn compareLess(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm;
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val < b_val,
            .byte => |b_val| a_val < b_val,
            // Mixed int/float comparison (from old VM)
            .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
            else => ErrorList.TypeError,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val < b_val,
            .byte => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
            // Mixed float/int comparison (from old VM)
            .int => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
            else => ErrorList.TypeError,
        },
        .byte => |a_val| switch (b.value) {
            .byte => |b_val| a_val < b_val,
            .int => |b_val| a_val < b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
            else => ErrorList.TypeError,
        },
        // Complex types don't support comparison
        .tetra, .string, .nothing, .array, .struct_instance, .map, .enum_variant, .storage_id_ref => ErrorList.TypeError,
    };
}

/// Enhanced greater-than with mixed int/float support from old VM
fn compareGreater(vm: anytype, a: HIRFrame, b: HIRFrame) !bool {
    _ = vm;
    return switch (a.value) {
        .int => |a_val| switch (b.value) {
            .int => |b_val| a_val > b_val,
            .byte => |b_val| a_val > b_val,
            // Mixed int/float comparison (from old VM)
            .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
            else => ErrorList.TypeError,
        },
        .float => |a_val| switch (b.value) {
            .float => |b_val| a_val > b_val,
            .byte => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
            // Mixed float/int comparison (from old VM)
            .int => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
            else => ErrorList.TypeError,
        },
        .byte => |a_val| switch (b.value) {
            .byte => |b_val| a_val > b_val,
            .int => |b_val| a_val > b_val,
            .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
            else => ErrorList.TypeError,
        },
        // Complex types don't support comparison
        .tetra, .string, .nothing, .array, .struct_instance, .map, .enum_variant, .storage_id_ref => ErrorList.TypeError,
    };
}
