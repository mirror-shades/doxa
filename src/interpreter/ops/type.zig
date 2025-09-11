const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;

/// Execute type operations (TypeCheck, Convert, TypeOf)
pub const TypeOps = struct {
    /// Type checking for union types and as expressions
    pub fn execTypeCheck(vm: anytype, tc: anytype) !void {
        const value = try vm.stack.pop();

        // Get the runtime type of the value
        const runtime_type = vm.getTypeString(value.value);

        // Compare with target type
        const type_match = std.mem.eql(u8, runtime_type, tc.target_type);

        if (vm.reporter.debug_mode) {
            vm.reporter.report(.Debug, .Hint, null, null, "TypeCheck: want='{s}' got='{s}' -> {s}", .{ tc.target_type, runtime_type, if (type_match) "match" else "no-match" });
        }

        // Push result as tetra (1 for match, 0 for no match)
        try vm.stack.push(HIRFrame.initTetra(if (type_match) 1 else 0));
    }

    /// Type conversion between different HIR types
    pub fn execConvert(vm: anytype, c: anytype) !void {
        // Pop the value to convert, perform conversion, and push result
        const frame = try vm.stack.pop();
        var out: HIRValue = frame.value;
        switch (c.to_type) {
            .Float => {
                // Convert ints/bytes to float; leave float as-is
                out = switch (frame.value) {
                    .int => |i| HIRValue{ .float = @as(f64, @floatFromInt(i)) },
                    .byte => |u| HIRValue{ .float = @as(f64, @floatFromInt(u)) },
                    .float => frame.value,
                    else => frame.value,
                };
            },
            .Int => {
                out = switch (frame.value) {
                    .int => frame.value,
                    .byte => |u| HIRValue{ .int = @as(i64, u) },
                    .float => |f| HIRValue{ .int = @as(i64, @intFromFloat(f)) },
                    else => frame.value,
                };
            },
            .Byte => {
                out = switch (frame.value) {
                    .byte => frame.value,
                    .int => |i| HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 },
                    .float => |f| blk: {
                        const i: i64 = @as(i64, @intFromFloat(f));
                        break :blk HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 };
                    },
                    else => frame.value,
                };
            },
            else => {
                // Other conversions not needed for current arithmetic paths
                out = frame.value;
            },
        }

        try vm.stack.push(HIRFrame.initFromHIRValue(out));
    }

    /// Type reflection operation (placeholder - not currently implemented in VM)
    pub fn execTypeOf(vm: anytype, t: anytype) !void {
        _ = vm;
        _ = t;
        // TODO: Implement TypeOf operation when needed
        return ErrorList.NotImplemented;
    }
};
