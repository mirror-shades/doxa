const std = @import("std");
const testing = std.testing;

const test_run = @import("test_run.zig");
const test_compile = @import("test_compile.zig");
const test_compile_errors = @import("test_compile_errors.zig");
const test_build_script = @import("test_build_script.zig");

// The program suites drive the installed `doxa` binary through hundreds of
// subprocesses and take about a minute. They live in their own test executable
// which `build.zig` runs as a plain process rather than through the test
// runner's `--listen` protocol: a suite that spends a minute spawning children
// is a poor fit for the protocol's 60s response window between messages, and
// any child that outlives the suite would hold the protocol pipes open and
// stall the build runner. `platform.sealStdHandles` closes the second hole.

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

test "build script suite" {
    const summary = try test_build_script.runAll(testing.allocator);
    try testing.expect(summary.failed == 0 and summary.untested == 0);
}
