//! Output redirected to a file. A program's stdout may be a seekable file that
//! child processes share; every write must append at the file's current
//! position rather than at an offset the writer tracks itself, or later writes
//! overwrite earlier ones. Drives the installed binary, so it runs with the
//! program suites (`suites.zig`).

const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

/// Writes through both stdout paths (`std.io.println` and the runtime's
/// `@print`) on each side of a child process that inherits the same stdout.
const source =
    \\module std from @std()
    \\std.io.println("before child")
    \\@print("print before child\n")
    \\std.process.exec("echo child-output")
    \\std.io.println("after child")
    \\@print("print after child\n")
;

test "redirect: stdout to a file keeps output spanning a child process" {
    const allocator = testing.allocator;
    const io = testing.io;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "probe.doxa", .data = source });

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];

    const slot: harness.Slot = try .claim(allocator, io, "redirect");
    defer slot.release(allocator, io);
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);

    {
        const out = try tmp.dir.createFile(io, "out.txt", .{});
        defer out.close(io);
        const capture = try doxa.run(allocator, io, .{
            .args = &.{ "run", "probe.doxa", cache_flag },
            .cwd = cwd,
            .stdout_file = out,
        });
        defer capture.deinit(allocator);
        if (!capture.succeeded()) {
            std.debug.print("doxa run failed ({any}):\n{s}\n", .{ capture.end, capture.stderr });
            return error.ProbeFailed;
        }
    }

    const written = try tmp.dir.readFileAlloc(io, "out.txt", allocator, .unlimited);
    defer allocator.free(written);
    // `echo` ends its line with CRLF under cmd.exe.
    const normalized = try std.mem.replaceOwned(u8, allocator, written, "\r\n", "\n");
    defer allocator.free(normalized);
    try testing.expectEqualStrings(
        "before child\nprint before child\nchild-output\nafter child\nprint after child\n",
        normalized,
    );
}
