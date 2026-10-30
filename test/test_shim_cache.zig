//! An inline-Zig shim object is keyed by the wrapper the compiler generates,
//! not by the `zig` block's source alone (`plan/inline-zig-structs.md`, step
//! 1). The wrapper also carries what the program's Doxa declarations fix: here
//! an error set's variant order, which maps each Zig error to its discriminant.
//! Reordering the enum leaves the block unchanged, so a key over the block
//! alone served the old object, which then reported the wrong variant. Drives
//! the installed binary, so it runs with the program suites (`suites.zig`).

const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

fn source(comptime variants: []const u8) []const u8 {
    return "enum Fail { " ++ variants ++ " }\n" ++
        \\zig Z {
        \\    pub fn pick() DoxaError_Fail!i64 {
        \\        return error.Second;
        \\    }
        \\}
        \\match Z.pick() {
        \\    Fail.First then @print("First\n"),
        \\    Fail.Second then @print("Second\n"),
        \\    else @print("ok\n"),
        \\}
        \\
    ;
}

test "shim cache: reordering an error set's enum builds a new shim" {
    const allocator = testing.allocator;
    const io = testing.io;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];

    const slot: harness.Slot = try .claim(allocator, io, "shim-cache");
    defer slot.release(allocator, io);
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);

    // Both orders compile into one cache, in both sequences, so whichever
    // order the slot already holds from an earlier run, each build follows
    // one built from the other order.
    for ([_][]const u8{ source("First, Second"), source("Second, First"), source("First, Second") }) |program| {
        try tmp.dir.writeFile(io, .{ .sub_path = "probe.doxa", .data = program });
        const capture = try doxa.run(allocator, io, .{
            .args = &.{ "run", "probe.doxa", cache_flag },
            .cwd = cwd,
        });
        defer capture.deinit(allocator);
        if (!capture.succeeded()) {
            std.debug.print("doxa run failed ({any}):\n{s}\n", .{ capture.end, capture.stderr });
            return error.ProbeFailed;
        }
        try testing.expectEqualStrings("Second\n", capture.stdout);
    }
}
