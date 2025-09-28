const std = @import("std");
const HIRValue = @import("../codegen/hir/soxa_values.zig").HIRValue;
const ErrorList = @import("../utils/errors.zig").ErrorList;

const STACK_SIZE: u32 = 1024 * 1024;

pub const HotVar = struct {
    name: []const u8,
    storage_id: u32,
};

/// Simple cache that fully trusts and integrates with the memory module
/// Cache var_name -> storage_id, but invalidate on scope changes to maintain correctness
/// HIR-based VM Frame - simplified to work with memory management
pub const HIRFrame = struct {
    value: HIRValue,
    field_name: ?[]const u8 = null,
    scope_refs: u32 = 0,

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
        return .{
            .value = value,
            .field_name = null,
        };
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

    pub fn incrementScopeRefs(self: *HIRFrame) void {
        self.scope_refs += 1;
    }

    pub fn decrementScopeRefs(self: *HIRFrame) void {
        if (self.scope_refs > 0) {
            self.scope_refs -= 1;
        }
    }
};

pub const HIRStack = struct {
    data: []HIRFrame,
    sp: i64 = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !HIRStack {
        const data = try allocator.alloc(HIRFrame, STACK_SIZE);

        for (data) |*frame| {
            frame.* = HIRFrame.initNothing();
        }

        return HIRStack{
            .data = data,
            .sp = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HIRStack) void {
        self.allocator.free(self.data);
        self.sp = 0;
    }

    pub inline fn push(self: *HIRStack, value: HIRFrame) !void {
        if (std.debug.runtime_safety and self.sp >= STACK_SIZE) {
            std.debug.print("Stack overflow: Attempted to push at sp={}\n", .{self.sp});
            return ErrorList.StackOverflow;
        }
        self.data[@intCast(self.sp)] = value;
        self.sp += 1;
    }

    pub inline fn pop(self: *HIRStack) !HIRFrame {
        if (std.debug.runtime_safety and self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to pop at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        self.sp -= 1;
        return self.data[@intCast(self.sp)];
    }

    pub fn peek(self: HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to peek at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        return self.data[@intCast(self.sp - 1)];
    }

    pub fn print(self: HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to print at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        return self.data[@intCast(self.sp - 1)];
    }

    pub fn size(self: HIRStack) i64 {
        return self.sp;
    }

    pub fn isEmpty(self: HIRStack) bool {
        return self.sp == 0;
    }
};

pub const CallFrame = struct {
    return_ip: u32,
    function_name: []const u8,
    arg_count: u32 = 0,
    saved_sp: i64,
};

pub const CallStack = struct {
    frames: []CallFrame,
    sp: u32 = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !CallStack {
        const frames = try allocator.alloc(CallFrame, 1024);
        return CallStack{
            .frames = frames,
            .sp = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CallStack) void {
        self.allocator.free(self.frames);
    }

    pub fn push(self: *CallStack, frame: CallFrame) !void {
        if (self.sp >= self.frames.len) {
            const new_size = self.frames.len * 2;
            const new_frames = try self.allocator.realloc(self.frames, new_size);
            self.frames = new_frames;
        }
        self.frames[self.sp] = frame;
        self.sp += 1;
    }

    pub fn pop(self: *CallStack) !CallFrame {
        if (self.sp == 0) {
            return ErrorList.StackUnderflow;
        }
        self.sp -= 1;
        return self.frames[self.sp];
    }

    pub fn isEmpty(self: CallStack) bool {
        return self.sp == 0;
    }
};
