const std = @import("std");
const testing = std.testing;

const test_run = @import("test/test_run.zig");
const test_compile = @import("test/test_compile.zig");
const test_compile_errors = @import("test/test_compile_errors.zig");

test "run suite" {
    const summary = try test_run.runAll(testing.allocator);
    try testing.expect(summary.failed == 0 and summary.untested == 0);
}

test "compile suite" {
    const summary = try test_compile.runAll(testing.allocator);
    try testing.expect(summary.failed == 0 and summary.untested == 0);
}

test "compile errors suite" {
    const summary = try test_compile_errors.runAll(testing.allocator);
    try testing.expect(summary.failed == 0 and summary.untested == 0);
}

test {
    _ = @import("test/test_inline_zig.zig");
    _ = @import("test/test_lazy_modules.zig");
    _ = @import("test/test_hashing.zig");
    _ = @import("test/test_lexer.zig");
}

// `zig build test` only runs tests reachable from this root, so a
// `test/*.zig` that is never imported here silently never runs. This guard
// fails when a new test file is added without wiring it in.
test "suite wiring: every test file is reachable" {
    const wired = [_][]const u8{
        "test_run.zig",
        "test_compile.zig",
        "test_compile_errors.zig",
        "test_inline_zig.zig",
        "test_lazy_modules.zig",
        "test_hashing.zig",
        "test_lexer.zig",
    };
    // Intentionally outside this root: shared helpers, and the LSP suite, which
    // is built as its own test executable (see build.zig).
    const external = [_][]const u8{
        "answers.zig",
        "harness.zig",
        "test_lsp.zig",
    };

    var dir = try std.Io.Dir.cwd().openDir(testing.io, "test", .{ .iterate = true });
    defer dir.close(testing.io);

    var orphans: usize = 0;
    var scanned: usize = 0;
    var it = dir.iterate();
    while (try it.next(testing.io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".zig")) continue;
        scanned += 1;
        if (inList(&wired, entry.name) or inList(&external, entry.name)) continue;
        std.debug.print("test/{s} is not reachable from test.zig\n", .{entry.name});
        orphans += 1;
    }
    try testing.expectEqual(@as(usize, 0), orphans);
    // Guard against a wrong/empty directory silently passing the check above.
    try testing.expectEqual(wired.len + external.len, scanned);
}

fn inList(list: []const []const u8, name: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, name)) return true;
    }
    return false;
}
