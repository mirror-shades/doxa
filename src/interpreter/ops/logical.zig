const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

fn tetraToClassical(t: u8) bool {
    return t == 1 or t == 2;
}

fn extractTetra(vm: anytype, frame: HIRFrame, op_name: []const u8, a_value: HIRValue, b_value: HIRValue) !u8 {
    return switch (frame.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform {s} on non-tetra values: {s} {s} {s}", .{ op_name, @tagName(a_value), op_name, @tagName(b_value) });
            return ErrorList.TypeError;
        },
    };
}

fn extractTetraUnary(vm: anytype, frame: HIRFrame, op_name: []const u8) !u8 {
    return switch (frame.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform {s} on a non-tetra value: {s}", .{ op_name, @tagName(frame.value) });
            return ErrorList.TypeError;
        },
    };
}

pub fn exec(vm: anytype, op: anytype) !void {
    const result = switch (op.op) {
        .Not => blk: {
            const a = try vm.stack.pop();
            break :blk try logicalNot(vm, a);
        },
        else => blk: {
            const b = try vm.stack.pop();
            const a = try vm.stack.pop();

            break :blk switch (op.op) {
                .And => try logicalAnd(vm, a, b),
                .Or => try logicalOr(vm, a, b),
                .Iff => try logicalIff(vm, a, b),
                .Xor => try logicalXor(vm, a, b),
                .Nand => try logicalNand(vm, a, b),
                .Nor => try logicalNor(vm, a, b),
                .Implies => try logicalImplies(vm, a, b),
                .Not => unreachable,
            };
        },
    };

    try vm.stack.push(HIRFrame.initTetra(result));
}

fn logicalAnd(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "AND", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "AND", a.value, b.value);
    return if (tetraToClassical(a_tetra) and tetraToClassical(b_tetra)) 1 else 0;
}

fn logicalOr(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "OR", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "OR", a.value, b.value);
    return if (tetraToClassical(a_tetra) or tetraToClassical(b_tetra)) 1 else 0;
}

fn logicalNot(vm: anytype, a: HIRFrame) !u8 {
    const a_tetra = try extractTetraUnary(vm, a, "NOT");
    return switch (a_tetra) {
        0 => 1,
        1 => 0,
        2 => 2,
        3 => 3,
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{a_tetra});
            return ErrorList.TypeError;
        },
    };
}

fn logicalIff(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "IFF", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "IFF", a.value, b.value);
    return if (tetraToClassical(a_tetra) == tetraToClassical(b_tetra)) 1 else 0;
}

fn logicalXor(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "XOR", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "XOR", a.value, b.value);
    return if (tetraToClassical(a_tetra) != tetraToClassical(b_tetra)) 1 else 0;
}

fn logicalNand(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "NAND", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "NAND", a.value, b.value);
    return if (tetraToClassical(a_tetra) and tetraToClassical(b_tetra)) 0 else 1;
}

fn logicalNor(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "NOR", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "NOR", a.value, b.value);
    return if (tetraToClassical(a_tetra) or tetraToClassical(b_tetra)) 0 else 1;
}

fn logicalImplies(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = try extractTetra(vm, a, "IMPLIES", a.value, b.value);
    const b_tetra = try extractTetra(vm, b, "IMPLIES", a.value, b.value);
    return if (tetraToClassical(a_tetra) and !tetraToClassical(b_tetra)) 0 else 1;
}
