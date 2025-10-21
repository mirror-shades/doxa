const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const debug_print = @import("../../runtime/print.zig");

pub const TypeOps = struct {
    pub fn execTypeCheck(vm: anytype, tc: anytype) !void {
        const value = try vm.stack.pop();
        const runtime_type = debug_print.getTypeString(vm, value.value);
        const type_match = std.mem.eql(u8, runtime_type, tc.target_type);
        try vm.stack.push(HIRFrame.initTetra(if (type_match) 1 else 0));
    }

    pub fn execConvert(vm: anytype, c: anytype) !void {
        const frame = try vm.stack.pop();
        var out: HIRValue = frame.value;
        switch (c.to_type) {
            .Float => {
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
                out = frame.value;
            },
        }
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
    }

    pub fn execTypeOf(vm: anytype, t: anytype) !void {
        _ = t;

        const value = try vm.stack.pop();
        const type_string = debug_print.getTypeString(vm, value.value);
        try vm.stack.push(HIRFrame.initString(type_string));
    }
};
