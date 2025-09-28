const std = @import("std");
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub const ControlFlowOps = struct {
    pub fn execJump(vm: anytype, j: anytype) !void {
        if (vm.label_map.get(j.label)) |target_ip| {
            vm.ip = target_ip;
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown label: {s}", .{j.label});
        }
    }

    pub fn execJumpCond(vm: anytype, j: anytype) !void {
        const condition = try vm.stack.pop();

        const should_jump = switch (condition.value) {
            .tetra => |t| switch (t) {
                0 => false, // false -> don't jump
                1 => true, // true -> jump
                2 => true, // both -> jump (contains true)
                3 => false, // neither -> don't jump (contains no truth)
                else => unreachable,
            },
            .int => |i| i != 0,
            .float => |f| f != 0.0,
            .nothing => false,
            else => true,
        };

        const target_label = if (should_jump) j.label_true else j.label_false;
        std.debug.print("JumpCond: condition={any}, should_jump={}, target_label='{s}'\n", .{ condition.value, should_jump, target_label });

        if (vm.label_map.get(target_label)) |target_ip| {
            std.debug.print("JumpCond: jumping to label '{s}' at IP {}\n", .{ target_label, target_ip });
            vm.ip = target_ip;
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown label: {s}", .{target_label});
        }
    }
};
