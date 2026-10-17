//! Path arithmetic shared by the build phases: naming the output binary,
//! resolving directories against the invocation directory, and removing a
//! side file a previous build may have left.

const std = @import("std");
const constants = @import("../common/constants.zig");
const config = @import("config.zig");

pub fn fileStem(path: []const u8) []const u8 {
    const filename = std.fs.path.basename(path);
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot| {
        if (dot > 0) return filename[0..dot];
    }
    return filename;
}

pub fn looksLikeOutputPath(path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) return true;
    if (std.mem.startsWith(u8, path, "./") or std.mem.startsWith(u8, path, "../")) return true;
    if (std.mem.startsWith(u8, path, ".\\") or std.mem.startsWith(u8, path, "..\\")) return true;
    return std.mem.indexOfAny(u8, path, "/\\") != null;
}

pub fn withExeSuffix(allocator: std.mem.Allocator, path: []const u8, is_windows: bool) ![]u8 {
    if (is_windows) {
        if (std.mem.endsWith(u8, path, ".exe")) return allocator.dupe(u8, path);
        return std.fmt.allocPrint(allocator, "{s}.exe", .{path});
    }
    return allocator.dupe(u8, path);
}

// `doxa compile` writes a user-facing binary (`-o`, or `bin/<stem>`).
// `doxa run` writes into the cache directory and executes from there.
pub fn nativeOutputPath(
    allocator: std.mem.Allocator,
    mode: config.Mode,
    output_path: ?[]const u8,
    cache_dir: []const u8,
    script_path: []const u8,
    is_windows: bool,
) ![]u8 {
    const raw = switch (mode) {
        .compile => blk: {
            const stem = output_path orelse fileStem(script_path);
            if (looksLikeOutputPath(stem)) break :blk try allocator.dupe(u8, stem);
            break :blk try std.fmt.allocPrint(allocator, "{s}/{s}", .{ constants.DEFAULT_BIN_DIR, stem });
        },
        .run => try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cache_dir, fileStem(script_path) }),
    };
    defer allocator.free(raw);
    return withExeSuffix(allocator, raw, is_windows);
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
