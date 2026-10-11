//! `--profile` byte accounting: the stderr tree states what its bytes mean, and
//! the `--profile-out` JSON carries bytes and peaks, with one span per module
//! record under each stage. Drives the installed binary, so it runs with the
//! program suites (`suites.zig`).

const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

const source =
    \\module std from @std()
    \\std.io.println("profiled")
;

test "profile: bytes and per-record spans reach the report" {
    const allocator = testing.allocator;
    const io = testing.io;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "probe.doxa", .data = source });

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];

    const slot: harness.Slot = try .claim(allocator, io, "profile");
    defer slot.release(allocator, io);
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);

    const capture = try doxa.run(allocator, io, .{
        .args = &.{ "run", "probe.doxa", cache_flag, "--profile", "--profile-out=profile.json" },
        .cwd = cwd,
    });
    defer capture.deinit(allocator);
    if (!capture.succeeded()) {
        std.debug.print("doxa run failed ({any}):\n{s}\n", .{ capture.end, capture.stderr });
        return error.ProbeFailed;
    }
    try testing.expect(std.mem.indexOf(u8, capture.stderr, "not its logical allocations") != null);

    const json = try tmp.dir.readFileAlloc(io, "profile.json", allocator, .unlimited);
    defer allocator.free(json);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json, .{});
    defer parsed.deinit();
    const report = parsed.value.object;

    const bytes = report.get("bytes").?.integer;
    const peak = report.get("peak").?.integer;
    try testing.expect(bytes > 0);
    try testing.expect(peak > 0 and peak <= bytes);

    var found: Found = .{};
    for (report.get("spans").?.array.items) |span| try found.visit(span, null);
    try testing.expect(found.entry);
    try testing.expect(found.std);
}

/// Which record spans appeared beneath a stage span.
const Found = struct {
    entry: bool = false,
    std: bool = false,

    fn visit(self: *Found, span: std.json.Value, parent: ?[]const u8) !void {
        const node = span.object;
        const name = node.get("name").?.string;
        // Every span carries its bytes, and none allocates more than its total.
        try testing.expect(node.get("self_bytes").?.integer <= node.get("bytes").?.integer);
        if (parent) |stage| if (isStage(stage)) {
            if (std.mem.eql(u8, name, "pkg//probe.doxa")) self.entry = true;
            if (std.mem.eql(u8, name, "std//std.doxa")) self.std = true;
        };
        if (node.get("children")) |children| {
            for (children.array.items) |child| try self.visit(child, name);
        }
    }

    fn isStage(name: []const u8) bool {
        for ([_][]const u8{ "Parse", "Declarations", "Types", "Analysis" }) |stage| {
            if (std.mem.eql(u8, name, stage)) return true;
        }
        return false;
    }
};
