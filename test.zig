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
    try testing.expect(summary.failed == 0);
}

test "compile errors suite" {
    const summary = try test_compile_errors.runAll(testing.allocator);
    try testing.expect(summary.failed == 0 and summary.untested == 0);
}

test {
    _ = @import("test/test_inline_zig.zig");
}
