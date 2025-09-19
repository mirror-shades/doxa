const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var file_paths = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (file_paths.items) |path| {
            allocator.free(path);
        }
        file_paths.deinit();
    }

    // Find all .zig files in src
    var dir = try std.fs.cwd().openDir("src", .{ .iterate = true });
    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".zig")) {
            try file_paths.append(try allocator.dupe(u8, entry.path));
        }
    }

    // Hash the files
    var hasher = std.crypto.hash.Blake3.init(.{});
    var buf: [1024]u8 = undefined;

    for (file_paths.items) |path| {
        var file = try std.fs.cwd().openFile(try std.fmt.allocPrint(allocator, "src/{s}", .{path}), .{});
        defer file.close();
        var reader_buffer: [1024]u8 = undefined;
        var file_reader = file.reader(&reader_buffer);
        const reader = &file_reader.interface;
        while (true) {
            const n = reader.read(&buf) catch break;
            hasher.update(buf[0..n]);
        }
    }

    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    // Write the hash to src/compiler_hash.zig
    const hash_file_path = "src/compiler_hash.zig";
    var hash_file = try std.fs.cwd().createFile(hash_file_path, .{});
    defer hash_file.close();

    var writer_buffer: [1024]u8 = undefined;
    var file_writer = hash_file.writer(&writer_buffer);
    const writer = &file_writer.interface;
    try writer.print("pub const value = \"{s}\";\n", .{std.fmt.fmtSliceHexLower(&hash)});
    try writer.flush();

    // Print hash to stdout
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.print("{s}\n", .{std.fmt.fmtSliceHexLower(&hash)});
    try stdout.flush();
}
