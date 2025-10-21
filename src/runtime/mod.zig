const std = @import("std");

// Aggregate core runtime modules
pub const runtime_print = @import("print.zig");

// Version and lifecycle hooks (optional)
pub const version: []const u8 = "0.1.0";
pub fn init() void {}
pub fn deinit() void {}
