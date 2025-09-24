const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

// Execute the LogicalOp instruction. Accepts the VM as `anytype` to avoid import cycles.
pub fn exec(vm: anytype, op: anytype) !void {
    const result = switch (op.op) {
        .Not => blk: {
            // Unary operation - only pop one value
            const a = try vm.stack.pop();
            break :blk try logicalNot(vm, a);
        },
        else => blk: {
            // Binary operations - pop two values
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
                .Not => unreachable, // Handled above
            };
        },
    };

    try vm.stack.push(HIRFrame.initTetra(result));
}

fn logicalAnd(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical AND
    // false=0, true=1, both=2, neither=3
    // both(2) contains true, so treat as true; neither(3) doesn't contain true, so treat as false
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical AND: true only if both inputs are true
    return if (a_classical and b_classical) 1 else 0;
}

fn logicalOr(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical OR
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical OR: true if either input is true
    return if (a_classical or b_classical) 1 else 0;
}

fn logicalNot(vm: anytype, a: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOT on a non-tetra value: {s}", .{@tagName(a.value)});
            return ErrorList.TypeError;
        },
    };

    // Proper tetra logic: not false == true, not true == false, not both == both, not neither == neither
    return switch (a_tetra) {
        0 => 1, // not false == true
        1 => 0, // not true == false
        2 => 2, // not both == both (both does NOT negate)
        3 => 3, // not neither == neither (neither does NOT negate)
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{a_tetra});
            return ErrorList.TypeError;
        },
    };
}

fn logicalIff(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical IFF
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical IFF: true if both inputs have the same truth value
    return if (a_classical == b_classical) 1 else 0;
}

fn logicalXor(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical XOR
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical XOR: true if inputs have different truth values
    return if (a_classical != b_classical) 1 else 0;
}

fn logicalNand(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical NAND
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical NAND: NOT(AND) - false only if both inputs are true
    return if (a_classical and b_classical) 0 else 1;
}

fn logicalNor(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical NOR
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical NOR: NOT(OR) - true only if both inputs are false
    return if (a_classical or b_classical) 0 else 1;
}

fn logicalImplies(vm: anytype, a: HIRFrame, b: HIRFrame) !u8 {
    const a_tetra = switch (a.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    const b_tetra = switch (b.value) {
        .tetra => |val| if (val <= 3) val else {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
            return ErrorList.TypeError;
        },
        else => {
            vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
            return ErrorList.TypeError;
        },
    };

    // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical IMPLIES
    const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
    const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

    // Classical IMPLIES: false only if true implies false, otherwise true
    return if (a_classical and !b_classical) 0 else 1;
}
