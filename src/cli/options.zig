//! The parsed command line: build configuration plus the driver-owned fields.

const std = @import("std");
const Reporting = @import("../utils/reporting.zig");
const ReporterOptions = Reporting.ReporterOptions;
const build_config = @import("../build/config.zig");

pub const Options = struct {
    build: build_config.Config,
    reporter_options: ReporterOptions,
    script_path: ?[]const u8,
    profile: bool,
    profile_out: ?[]const u8,
    program_args: []const []const u8,

    pub fn init(allocator: std.mem.Allocator, mode: build_config.Mode) Options {
        return .{
            .build = build_config.Config.init(allocator, mode),
            .reporter_options = .{},
            .script_path = null,
            .profile = false,
            .profile_out = null,
            .program_args = &[_][]const u8{},
        };
    }

    pub fn deinit(self: *const Options, allocator: std.mem.Allocator) void {
        self.build.deinit(allocator);
        if (self.script_path) |p| allocator.free(p);
        if (self.profile_out) |p| allocator.free(p);
        for (self.program_args) |arg| allocator.free(arg);
        if (self.program_args.len > 0) allocator.free(self.program_args);
    }
};
