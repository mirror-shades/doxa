const std = @import("std");

const usage_text =
    \\Usage:
    \\  archive_tool unpack-zig-dep <archive> <dest-lib-dir> <extracted-folder-name>
    \\  archive_tool compress-releases --cwd <prefix-dir> <target-dir> [<target-dir> ...]
;

fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("archive_tool: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8, cwd: ?[]const u8) !void {
    var child = std.process.Child.init(argv, allocator);
    child.expand_arg0 = .expand;
    child.cwd = cwd;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = try child.spawnAndWait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) return error.CommandFailed;
        },
        else => return error.CommandFailed,
    }
}

fn runAnyCommand(
    allocator: std.mem.Allocator,
    candidates: []const []const []const u8,
    cwd: ?[]const u8,
) !void {
    var saw_command = false;
    for (candidates) |candidate| {
        runCommand(allocator, candidate, cwd) catch |err| switch (err) {
            error.FileNotFound => continue,
            error.CommandFailed => {
                saw_command = true;
                return err;
            },
            else => return err,
        };
        saw_command = true;
        return;
    }
    if (!saw_command) return error.FileNotFound;
}

fn unpackZigDependency(
    allocator: std.mem.Allocator,
    archive_path: []const u8,
    destination_lib_dir: []const u8,
    extracted_folder_name: []const u8,
) !void {
    try std.fs.cwd().makePath(destination_lib_dir);
    var destination_dir = try std.fs.cwd().openDir(destination_lib_dir, .{});
    defer destination_dir.close();

    if (destination_dir.access("zig", .{})) |_| {
        try destination_dir.deleteTree("zig");
    } else |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    }
    if (destination_dir.access(extracted_folder_name, .{})) |_| {
        try destination_dir.deleteTree(extracted_folder_name);
    } else |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    }

    if (std.mem.endsWith(u8, archive_path, ".zip")) {
        const output_arg = try std.fmt.allocPrint(allocator, "-o{s}", .{destination_lib_dir});
        defer allocator.free(output_arg);
        const tar_cmd = [_][]const u8{ "tar", "-xf", archive_path, "-C", destination_lib_dir };
        const bsdtar_cmd = [_][]const u8{ "bsdtar", "-xf", archive_path, "-C", destination_lib_dir };
        const unzip_cmd = [_][]const u8{ "unzip", "-q", archive_path, "-d", destination_lib_dir };
        const seven_zip_cmd = [_][]const u8{ "7z", "x", archive_path, output_arg, "-y" };
        try runAnyCommand(allocator, &[_][]const []const u8{
            &tar_cmd,
            &bsdtar_cmd,
            &unzip_cmd,
            &seven_zip_cmd,
        }, null);
    } else if (std.mem.endsWith(u8, archive_path, ".tar.xz")) {
        const tar_cmd = [_][]const u8{ "tar", "-xf", archive_path, "-C", destination_lib_dir };
        const bsdtar_cmd = [_][]const u8{ "bsdtar", "-xf", archive_path, "-C", destination_lib_dir };
        try runAnyCommand(allocator, &[_][]const []const u8{
            &tar_cmd,
            &bsdtar_cmd,
        }, null);
    } else {
        return error.UnsupportedArchiveFormat;
    }

    destination_dir.access(extracted_folder_name, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.ExtractedFolderMissing,
        else => return err,
    };
    try destination_dir.rename(extracted_folder_name, "zig");
}

fn compressReleaseDir(
    allocator: std.mem.Allocator,
    cwd: []const u8,
    target_dir_name: []const u8,
) !void {
    const zip_filename = try std.fmt.allocPrint(allocator, "{s}.zip", .{target_dir_name});
    defer allocator.free(zip_filename);

    var root_dir = try std.fs.cwd().openDir(cwd, .{});
    defer root_dir.close();
    root_dir.deleteFile(zip_filename) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };

    const seven_zip_cmd = [_][]const u8{ "7z", "a", "-tzip", "-mx=9", zip_filename, target_dir_name };
    const zip_cmd = [_][]const u8{ "zip", "-r", "-q", zip_filename, target_dir_name };
    try runAnyCommand(allocator, &[_][]const []const u8{
        &seven_zip_cmd,
        &zip_cmd,
    }, cwd);
}

fn compressReleases(
    allocator: std.mem.Allocator,
    cwd: []const u8,
    target_dirs: []const []const u8,
) !void {
    if (target_dirs.len == 0) {
        return error.InvalidArgument;
    }
    for (target_dirs) |target_dir_name| {
        try compressReleaseDir(allocator, cwd, target_dir_name);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        fatal("{s}", .{usage_text});
    }

    if (std.mem.eql(u8, args[1], "unpack-zig-dep")) {
        if (args.len != 5) fatal("{s}", .{usage_text});
        unpackZigDependency(allocator, args[2], args[3], args[4]) catch |err| {
            fatal("unpack-zig-dep failed: {s}", .{@errorName(err)});
        };
        return;
    }

    if (std.mem.eql(u8, args[1], "compress-releases")) {
        if (args.len < 5 or !std.mem.eql(u8, args[2], "--cwd")) {
            fatal("{s}", .{usage_text});
        }
        compressReleases(allocator, args[3], args[4..]) catch |err| {
            fatal("compress-releases failed: {s}", .{@errorName(err)});
        };
        return;
    }

    fatal("{s}", .{usage_text});
}
