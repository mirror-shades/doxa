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
    // The whole path: on a fresh output directory nothing above `lib` exists
    // yet, since the target's binary may not have been installed first.
    var destination_dir = try cwd.createDirPathOpen(io, destination_lib_dir, .{});
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

    var zig_dir = try destination_dir.openDir(io, "zig", .{});
    defer zig_dir.close(io);

    if (is_zip) {
        // In-process: GNU tar, the `tar` of most Linux hosts, cannot read a
        // zip, and no other extractor can be counted on.
        try unzipStripped(io, archive_path, zig_dir);
    } else {
        const tar_cmd = [_][]const u8{ "tar", "--strip-components=1", "-xf", archive_path, "-C", zig_dir_path };
        const bsdtar_cmd = [_][]const u8{ "bsdtar", "--strip-components=1", "-xf", archive_path, "-C", zig_dir_path };
        try runAnyCommand(io, &[_][]const []const u8{
            &tar_cmd,
            &bsdtar_cmd,
        }, null);
    }

    // The sentinel must mean "the toolchain is actually here", so refuse to
    // write it unless the extractor produced the compiler binary.
    if (!try containsZigBinary(io, zig_dir)) return error.MissingZigBinary;
    var sentinel = try zig_dir.createFile(io, extraction_sentinel, .{});
    sentinel.close(io);
}

/// Extracts the zip at `archive_path` into `dest` without its top-level
/// folder, as `tar --strip-components=1` does. `std.zip.extract` keeps every
/// entry's full name, and extracting it whole then renaming the folder is the
/// rename Windows denies (`unpackZigDependency`), so each entry is written
/// under its name with the first component removed.
fn unzipStripped(io: std.Io, archive_path: []const u8, dest: std.Io.Dir) !void {
    const zip = std.zip;
    const archive = try std.Io.Dir.cwd().openFile(io, archive_path, .{});
    defer archive.close(io);
    var read_buffer: [4096]u8 = undefined;
    var reader = archive.reader(io, &read_buffer);

    var entries = try zip.Iterator.init(&reader);
    var name_buffer: [std.fs.max_path_bytes]u8 = undefined;
    while (try entries.next()) |entry| {
        switch (entry.compression_method) {
            .store, .deflate => {},
            else => return error.UnsupportedCompressionMethod,
        }
        if (entry.filename_len > name_buffer.len) return error.ZipInsufficientBuffer;
        const name = name_buffer[0..entry.filename_len];
        try reader.seekTo(entry.header_zip_offset + @sizeOf(zip.CentralDirectoryFileHeader));
        try reader.interface.readSliceAll(name);
        std.mem.replaceScalar(u8, name, '\\', '/');

        // The top folder itself, or a name that would leave `dest`.
        const slash = std.mem.indexOfScalar(u8, name, '/') orelse return error.ZipEntryOutsideTopFolder;
        const path = name[slash + 1 ..];
        if (path.len == 0) continue;
        if (std.fs.path.isAbsolutePosix(path)) return error.ZipBadFilename;
        var components = std.mem.splitScalar(u8, path, '/');
        while (components.next()) |component| if (std.mem.eql(u8, component, "..")) return error.ZipBadFilename;

        if (path[path.len - 1] == '/') {
            try dest.createDirPath(io, path[0 .. path.len - 1]);
            continue;
        }

        try reader.seekTo(entry.file_offset);
        const local = try reader.interface.takeStruct(zip.LocalFileHeader, .little);
        if (!std.mem.eql(u8, &local.signature, &zip.local_file_header_sig)) return error.ZipBadFileOffset;
        try reader.seekTo(entry.file_offset + @sizeOf(zip.LocalFileHeader) + local.filename_len + local.extra_len);

        var parent = if (std.fs.path.dirnamePosix(path)) |dir| try dest.createDirPathOpen(io, dir, .{}) else dest;
        defer if (parent.handle != dest.handle) parent.close(io);
        const out = try parent.createFile(io, std.fs.path.basenamePosix(path), .{ .exclusive = true });
        defer out.close(io);
        var write_buffer: [64 * 1024]u8 = undefined;
        var writer = out.writer(io, &write_buffer);
        switch (entry.compression_method) {
            .store => reader.interface.streamExact64(&writer.interface, entry.uncompressed_size) catch |err| switch (err) {
                error.ReadFailed => return reader.err.?,
                error.WriteFailed => return writer.err.?,
                error.EndOfStream => return error.ZipDecompressTruncated,
            },
            .deflate => {
                var window: [std.compress.flate.max_window_len]u8 = undefined;
                var inflate: std.compress.flate.Decompress = .init(&reader.interface, .raw, &window);
                inflate.reader.streamExact64(&writer.interface, entry.uncompressed_size) catch |err| switch (err) {
                    error.ReadFailed => return reader.err orelse inflate.err.?,
                    error.WriteFailed => return writer.err.?,
                    error.EndOfStream => return error.ZipDecompressTruncated,
                };
            },
            else => unreachable,
        }
        try writer.end();
    }
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
