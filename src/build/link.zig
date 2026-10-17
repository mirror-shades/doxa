//! The final link: one `zig build-exe` over the runtime object, the program
//! object and every inline-Zig shim.

const std = @import("std");
const Build = @import("context.zig").Build;

pub fn exec(
    build: *const Build,
    obj_path: []const u8,
    runtime_obj_path: []const u8,
    inline_zig_wrapper_paths: []const []const u8,
) !void {
    build.profiler.begin("link");
    defer build.profiler.end();

    const allocator = build.allocator;
    var args_ln = std.array_list.Managed([]const u8).init(allocator);
    defer args_ln.deinit();
    try args_ln.append(build.zig_exe_path);
    try args_ln.append("build-exe");
    try args_ln.append(runtime_obj_path);
    try args_ln.append(obj_path);
    for (inline_zig_wrapper_paths) |p| {
        try args_ln.append(p);
    }
    const emit_flag = try std.fmt.allocPrint(allocator, "-femit-bin={s}", .{build.exe});
    defer allocator.free(emit_flag);
    try args_ln.append(emit_flag);
    try build.target.appendTargetArg(&args_ln);
    try build.target.appendCpuArg(&args_ln);
    try args_ln.append(build.config.opt.zigFlag());
    // `--strip` leaves no debug information in the binary at all: none of the
    // program's, and none of the runtime's or libc's, which is most of an ELF
    // binary's size.
    if (!build.config.debug_info) try args_ln.append("-fstrip");
    // A build ID: what gdb's `.build-id/` lookup, debuginfod and crash tooling
    // match a binary to its debug file by. A content hash, so the same program
    // gets the same ID.
    if (build.target.usesElf()) try args_ln.append("--build-id=sha1");
    try args_ln.append("-lc");
    if (build.target.isWindows() and inline_zig_wrapper_paths.len > 0) {
        try args_ln.appendSlice(&.{ "-lws2_32", "-lcrypt32" });
    }

    var manifest_link_flags = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (manifest_link_flags.items) |f| allocator.free(f);
        manifest_link_flags.deinit();
    }
    for (build.lib_dirs) |dir| {
        const flag = try std.fmt.allocPrint(allocator, "-L{s}", .{dir});
        try manifest_link_flags.append(flag);
        try args_ln.append(flag);
    }
    for (build.config.link_libs.items) |lib| {
        const flag = try std.fmt.allocPrint(allocator, "-l{s}", .{lib});
        try manifest_link_flags.append(flag);
        try args_ln.append(flag);
    }
    for (build.config.frameworks.items) |fw| {
        try args_ln.append("-framework");
        try args_ln.append(fw);
    }

    var child_ln = try std.process.spawn(build.io, .{
        .argv = args_ln.items,
        .cwd = .{ .path = build.cache_dir },
        .stdout = .inherit,
        .stderr = .inherit,
    });
    const term = try child_ln.wait(build.io);
    switch (term) {
        .exited => |code| if (code != 0) {
            build.reporter.reportBuildStepFailure("linking the executable", code);
            return error.Unexpected;
        },
        else => {
            build.reporter.reportBuildStepFailure("linking the executable", null);
            return error.Unexpected;
        },
    }
}
