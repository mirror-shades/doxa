const std = @import("std");
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;

/// Execute stack operations (Dup, Pop, Swap)
pub const StackOps = struct {
    /// Duplicate top stack value
    pub fn execDup(vm: anytype) !void {
        if (vm.stack.sp == 0) {
            std.debug.print("DEBUG: execDup called with empty stack (sp=0)\n", .{});
            return ErrorList.StackUnderflow;
        }
        const value = try vm.stack.peek();
        try vm.stack.push(HIRFrame.initFromHIRValue(value.value));
    }

    /// Pop and discard top stack value
    pub fn execPop(vm: anytype) !void {
        const value = try vm.stack.pop();
        // HIRFrame doesn't need cleanup - it's just a simple wrapper
        _ = value;
    }

    /// Swap top two stack values
    pub fn execSwap(vm: anytype) !void {
        const top = try vm.stack.pop();
        const second = try vm.stack.pop();
        try vm.stack.push(top);
        try vm.stack.push(second);
    }
};
