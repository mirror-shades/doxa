//! Run the native binary produced for `doxa run`, and emit the final profile.

const std = @import("std");
const Profiler = @import("../utils/profiler.zig").Profiler;

// `program_args[0]` is the source path; skip it so the process argv[0] is the
// exe.
pub fn runExecutable(
    io: std.Io,
    allocator: std.mem.Allocator,
    environ_map: *const std.process.Environ.Map,
    exe_path: []const u8,
    program_args: []const []const u8,
) !u8 {
    var args = std.array_list.Managed([]const u8).init(allocator);
    defer args.deinit();
    try args.append(exe_path);
    if (program_args.len > 1) try args.appendSlice(program_args[1..]);

    // Point the compiled program at the real `doxa` executable so the std/build
    // library's `compileArtifact` re-invokes the compiler instead of recursing
    // into the compiled binary itself (`selfExePath` would resolve to it).
    var env = try environ_map.clone(allocator);
    defer env.deinit();
    if (std.process.executablePathAlloc(io, allocator)) |compiler_path| {
        defer allocator.free(compiler_path);
        env.put("DOXA_BIN", compiler_path) catch {};
    } else |_| {}

    var child = try std.process.spawn(io, .{
        .argv = args.items,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
        .environ_map = &env,
    });
    const term = try child.wait(io);
    return switch (term) {
        .exited => |code| code,
        else => error.Unexpected,
    };
}

// Emit the human-readable profile to stderr and, when requested, the JSON
// report to disk. Called on every successful compile (and before a run's
// non-zero exit is propagated).
pub fn finishProfile(profiler: *Profiler, profile_out: ?[]const u8) !void {
    try profiler.dump();
    if (profile_out) |path| try profiler.writeJsonReport(path);
}
