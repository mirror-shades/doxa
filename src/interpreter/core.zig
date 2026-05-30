const std = @import("std");
const HIRValue = @import("../codegen/hir/soxa_values.zig").HIRValue;
const ErrorList = @import("../utils/errors.zig").ErrorList;

pub const HIRFrame = struct {
    value: HIRValue,

    pub fn initInt(x: i64) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .int = x } };
    }

    pub fn initFloat(x: f64) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .float = x } };
    }

    pub fn initString(x: []const u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .string = x } };
    }

    pub fn initTetra(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .tetra = x } };
    }

    pub fn initByte(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .byte = x } };
    }

    pub fn initNothing() HIRFrame {
        return HIRFrame{ .value = HIRValue.nothing };
    }

    pub fn initFromHIRValue(value: HIRValue) HIRFrame {
        return .{ .value = value };
    }

    pub fn asInt(self: HIRFrame) !i64 {
        return switch (self.value) {
            .int => |i| i,
            .byte => |b| @as(i64, b),
            .float => |f| @as(i64, @intFromFloat(f)),
            else => ErrorList.TypeError,
        };
    }

    pub fn asFloat(self: HIRFrame) !f64 {
        return switch (self.value) {
            .float => |f| f,
            .int => |i| @as(f64, @floatFromInt(i)),
            .byte => |b| @as(f64, @floatFromInt(b)),
            else => ErrorList.TypeError,
        };
    }

    pub fn asTetra(self: HIRFrame) !u8 {
        return switch (self.value) {
            .tetra => |t| t,
            .byte => |b| b,
            .int => |i| if (i >= 0 and i <= 3) @as(u8, @intCast(i)) else 0,
            else => ErrorList.TypeError,
        };
    }

    pub fn asString(self: HIRFrame) ![]const u8 {
        return switch (self.value) {
            .string => |s| s,
            else => ErrorList.TypeError,
        };
    }

    pub fn asByte(self: HIRFrame) !u8 {
        return switch (self.value) {
            .byte => |u| u,
            else => ErrorList.TypeError,
        };
    }
};
