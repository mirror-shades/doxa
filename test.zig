const std = @import("std");
const testing = std.testing;

test {
    _ = @import("test/test_inline_zig.zig");
    _ = @import("test/test_lazy_modules.zig");
    _ = @import("test/test_hashing.zig");
    _ = @import("test/test_lexer.zig");
    _ = @import("test/test_profiler.zig");
    _ = @import("test/test_artifact_cache.zig");
    _ = @import("test/test_floored_arith.zig");
    _ = @import("test/test_overflow.zig");
    _ = @import("test/test_descriptor_skip.zig");
    _ = @import("test/test_target_tuning.zig");
}

// `zig build test` only runs tests reachable from this root, so a
// `test/*.zig` that is never imported here silently never runs. This guard
// fails when a new test file is added without wiring it in.
test "suite wiring: every test file is reachable" {
    const wired = [_][]const u8{
        "test_inline_zig.zig",
        "test_lazy_modules.zig",
        "test_hashing.zig",
        "test_lexer.zig",
        "test_profiler.zig",
        "test_artifact_cache.zig",
        "test_floored_arith.zig",
        "test_overflow.zig",
        "test_descriptor_skip.zig",
        "test_target_tuning.zig",
    };
    // Intentionally outside this root: shared helpers, the LSP suite (its own
    // executable), and the program suites, which drive the installed `doxa`
    // binary from `test/suites.zig` -- its own executable, run plainly rather
    // than through the test-runner protocol (see build.zig).
    const external = [_][]const u8{
        "answers.zig",
        "cases.zig",
        "harness.zig",
        "test_lsp.zig",
        "suites.zig",
        "test_run.zig",
        "test_compile.zig",
        "test_compile_errors.zig",
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
