//! Path arithmetic shared by the build phases: naming the output binary,
//! resolving directories against the invocation directory, and removing a
//! side file a previous build may have left.

const std = @import("std");
const config = @import("config.zig");

pub fn fileStem(path: []const u8) []const u8 {
    const filename = std.fs.path.basename(path);
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot| {
        if (dot > 0) return filename[0..dot];
    }
    return filename;
}

pub fn withExeSuffix(allocator: std.mem.Allocator, path: []const u8, is_windows: bool) ![]u8 {
    if (is_windows) {
        if (std.mem.endsWith(u8, path, ".exe")) return allocator.dupe(u8, path);
        return std.fmt.allocPrint(allocator, "{s}.exe", .{path});
    }
    return allocator.dupe(u8, path);
}

// `doxa compile` writes the user-facing binary exactly where `-o` names it.
// `doxa run` writes into the cache directory and executes from there.
pub fn nativeOutputPath(
    allocator: std.mem.Allocator,
    mode: config.Mode,
    output_path: ?[]const u8,
    cache_dir: []const u8,
    script_path: []const u8,
    is_windows: bool,
) ![]u8 {
    switch (mode) {
        // The command line refuses `doxa compile` without `-o`.
        .compile => return withExeSuffix(allocator, output_path.?, is_windows),
        .run => {
            const raw = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cache_dir, fileStem(script_path) });
            defer allocator.free(raw);
            return withExeSuffix(allocator, raw, is_windows);
        },
    }
}

/// Each of `paths` resolved against `base`; an absolute path is kept as is.
pub fn resolveAll(allocator: std.mem.Allocator, base: []const u8, paths: []const []const u8) ![]const []const u8 {
    const resolved = try allocator.alloc([]const u8, paths.len);
    for (paths, resolved) |path, *out| out.* = try std.fs.path.resolve(allocator, &.{ base, path });
    return resolved;
}

pub fn openDirMaybeAbs(io: std.Io, path: []const u8, opts: std.Io.Dir.OpenOptions) !std.Io.Dir {
    return if (std.fs.path.isAbsolute(path))
        std.Io.Dir.openDirAbsolute(io, path, opts)
    else
        std.Io.Dir.cwd().openDir(io, path, opts);
}

/// Remove a side file a previous build left, if there is one.
pub fn removeIfExists(io: std.Io, path: []const u8) !void {
    std.Io.Dir.cwd().deleteFile(io, path) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };
}

test "compile names the binary exactly where -o points" {
    const alloc = std.testing.allocator;
    const cases = [_]struct { given: []const u8, windows: []const u8, other: []const u8 }{
        .{ .given = "repro", .windows = "repro.exe", .other = "repro" },
        .{ .given = "repro.exe", .windows = "repro.exe", .other = "repro.exe" },
        .{ .given = "out/repro", .windows = "out/repro.exe", .other = "out/repro" },
        .{ .given = "./repro", .windows = "./repro.exe", .other = "./repro" },
    };
    for (cases) |case| {
        const windows = try nativeOutputPath(alloc, .compile, case.given, "cache", "app.doxa", true);
        defer alloc.free(windows);
        try std.testing.expectEqualStrings(case.windows, windows);

        const other = try nativeOutputPath(alloc, .compile, case.given, "cache", "app.doxa", false);
        defer alloc.free(other);
        try std.testing.expectEqualStrings(case.other, other);
    }
}

test "run names the binary by the script's stem in the cache" {
    const alloc = std.testing.allocator;
    const path = try nativeOutputPath(alloc, .run, null, "cache", "src/app.doxa", true);
    defer alloc.free(path);
    try std.testing.expectEqualStrings("cache/app.exe", path);
}
