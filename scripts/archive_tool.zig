const std = @import("std");

const usage_text =
    \\Usage:
    \\  archive_tool unpack-zig-dep <archive> <dest-lib-dir>
    \\  archive_tool compress-releases --cwd <prefix-dir> <target-dir> [<target-dir> ...]
;

/// Marks a `lib/zig` tree as fully extracted. Written only after the extractor
/// exits successfully, so an extraction interrupted mid-run is detected and
/// redone rather than silently trusted.
const extraction_sentinel = ".doxa-zig-extracted";

fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("archive_tool: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

fn runCommand(
    io: std.Io,
    argv: []const []const u8,
    cwd: []const u8,
) !void {
    var child = try std.process.spawn(io, .{
        .argv = argv,
        .expand_arg0 = .expand,
        .cwd = .{ .path = cwd },
        .stdout = .inherit,
        .stderr = .inherit,
    });

    const term = try child.wait(io);

    switch (term) {
        .exited => |code| {
            if (code != 0) return error.CommandFailed;
        },
        else => return error.CommandFailed,
    }
}

fn runAnyCommand(
    io: std.Io,
    candidates: []const []const []const u8,
    cwd: ?[]const u8,
) !void {
    const effective_cwd = cwd orelse ".";
    for (candidates) |candidate| {
        runCommand(io, candidate, effective_cwd) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        return;
    }
    return error.FileNotFound;
}

fn unpackZigDependency(io: std.Io, allocator: std.mem.Allocator, archive_path: []const u8, destination_lib_dir: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    cwd.createDir(io, destination_lib_dir, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
    var destination_dir = try cwd.openDir(
        io,
        destination_lib_dir,
        .{},
    );
    defer destination_dir.close(io);

    // Unpack straight into the final `zig` name rather than extracting to the
    // archive's versioned folder and renaming it afterwards. On Windows the
    // rename of a freshly written ~20k-file tree is transiently denied by
    // antivirus/indexers, which made repeated builds fail until the lock
    // cleared. The sentinel tells a complete tree from one interrupted
    // mid-extraction, so only the latter is cleaned up and redone.
    if (destination_dir.openDir(io, "zig", .{})) |zig_dir_handle| {
        var zig_dir = zig_dir_handle;
        defer zig_dir.close(io);
        if (zig_dir.access(io, extraction_sentinel, .{})) |_| {
            return;
        } else |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        }
    } else |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    }

    try destination_dir.deleteTree(io, "zig");
    destination_dir.createDir(io, "zig", .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const is_zip = std.mem.endsWith(u8, archive_path, ".zip");
    const is_tar_xz = std.mem.endsWith(u8, archive_path, ".tar.xz");
    if (!is_zip and !is_tar_xz) return error.UnsupportedArchiveFormat;

    const zig_dir_path = try std.fs.path.join(allocator, &.{ destination_lib_dir, "zig" });
    defer allocator.free(zig_dir_path);

    const tar_cmd = [_][]const u8{ "tar", "--strip-components=1", "-xf", archive_path, "-C", zig_dir_path };
    const bsdtar_cmd = [_][]const u8{ "bsdtar", "--strip-components=1", "-xf", archive_path, "-C", zig_dir_path };
    try runAnyCommand(io, &[_][]const []const u8{
        &tar_cmd,
        &bsdtar_cmd,
    }, null);

    var zig_dir = try destination_dir.openDir(io, "zig", .{});
    defer zig_dir.close(io);

    // The sentinel must mean "the toolchain is actually here", so refuse to
    // write it unless the extractor produced the compiler binary.
    if (!try containsZigBinary(io, zig_dir)) return error.MissingZigBinary;
    var sentinel = try zig_dir.createFile(io, extraction_sentinel, .{});
    sentinel.close(io);
}

fn containsZigBinary(io: std.Io, zig_dir: std.Io.Dir) !bool {
    for ([_][]const u8{ "zig", "zig.exe" }) |name| {
        if (zig_dir.access(io, name, .{})) |_| return true else |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        }
    }
    return false;
}

fn compressReleaseDir(
    io: std.Io,
    allocator: std.mem.Allocator,
    cwd: []const u8,
    target_dir_name: []const u8,
) !void {
    const zip_filename = try std.fmt.allocPrint(allocator, "{s}.zip", .{target_dir_name});
    defer allocator.free(zip_filename);

    var root_dir = try std.Io.Dir.cwd().openDir(io, cwd, .{});
    defer root_dir.close(io);
    root_dir.deleteFile(io, zip_filename) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };

    const seven_zip_cmd = [_][]const u8{ "7z", "a", "-tzip", "-mx=9", zip_filename, target_dir_name };
    const zip_cmd = [_][]const u8{ "zip", "-r", "-q", zip_filename, target_dir_name };
    try runAnyCommand(io, &[_][]const []const u8{
        &seven_zip_cmd,
        &zip_cmd,
    }, cwd);
}

fn compressReleases(
    io: std.Io,
    allocator: std.mem.Allocator,
    cwd: []const u8,
    target_dirs: []const []const u8,
) !void {
    if (target_dirs.len == 0) {
        return error.InvalidArgument;
    }
    for (target_dirs) |target_dir_name| {
        try compressReleaseDir(io, allocator, cwd, target_dir_name);
    }
}

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.page_allocator;
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    if (args.len < 2) {
        fatal("{s}", .{usage_text});
    }

    if (std.mem.eql(u8, args[1], "unpack-zig-dep")) {
        if (args.len != 4) fatal("{s}", .{usage_text});
        unpackZigDependency(init.io, allocator, args[2], args[3]) catch |err| {
            fatal("unpack-zig-dep failed: {s}", .{@errorName(err)});
        };
        return;
    }

    if (std.mem.eql(u8, args[1], "compress-releases")) {
        if (args.len < 5 or !std.mem.eql(u8, args[2], "--cwd")) {
            fatal("{s}", .{usage_text});
        }
        compressReleases(init.io, allocator, args[3], args[4..]) catch |err| {
            fatal("compress-releases failed: {s}", .{@errorName(err)});
        };
        return;
    }

    fatal("{s}", .{usage_text});
}
