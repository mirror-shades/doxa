const std = @import("std");
const platform = @import("platform");

const cases = @import("cases.zig");
const harness = @import("harness.zig");

const Case = cases.Case;
const test_results = harness.Counts;
const CommandResult = harness.CommandResult;

fn runDoxaCommandEx(allocator: std.mem.Allocator, path: []const u8, input: ?[]const u8, extra_args: []const []const u8) !CommandResult {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const exe_path = try harness.doxaExePath(allocator);
    defer allocator.free(exe_path);

    var argv = std.array_list.Managed([]const u8).init(allocator);
    defer argv.deinit();
    try argv.appendSlice(&[_][]const u8{ exe_path, "run", path });
    if (extra_args.len > 0) {
        try argv.append("--");
        try argv.appendSlice(extra_args);
    }
    return try harness.runCommandCapture(allocator, argv.items, repo_root, input);
}

fn runTestCase(allocator: std.mem.Allocator, tc: Case) !test_results {
    if (tc.mode == .terminate) {
        const result = try runDoxaCommandEx(allocator, tc.path, tc.input, tc.extra_args);
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        var ok = result.exit_code != 0;
        if (tc.expect_code) |code| ok = ok and result.exit_code == code;
        if (tc.expect_stderr) |needle| ok = ok and std.mem.indexOf(u8, result.stderr, needle) != null;
        if (ok) return .{ .passed = 1, .failed = 0, .untested = 0 };
        std.debug.print(
            "Terminating case '{s}' failed:\n  exit={d}\n  stderr: {s}\n",
            .{ tc.name, result.exit_code, result.stderr },
        );
        return .{ .passed = 0, .failed = 1, .untested = 0 };
    }

    const result = runDoxaCommandEx(allocator, tc.path, tc.input, tc.extra_args) catch return error.CommandFailed;

    const output = switch (tc.mode) {
        .print => blk: {
            allocator.free(result.stderr);
            if (result.exit_code != 0) {
                allocator.free(result.stdout);
                return error.CommandFailed;
            }
            break :blk result.stdout;
        },
        .peek => blk: {
            allocator.free(result.stdout);
            if (result.exit_code != 0) {
                allocator.free(result.stderr);
                return error.CommandFailed;
            }
            break :blk result.stderr;
        },
        .terminate => unreachable,
    };
    defer allocator.free(output);

    return switch (tc.mode) {
        .print => try harness.validatePrintResults(output, tc.expected_print.?, allocator),
        .peek => try harness.validatePeekResults(output, tc.expected_peek.?, allocator),
        .terminate => unreachable,
    };
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    platform.enableUtf8Console();

    harness.printSection("RUN");
    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (cases.cases) |tc| {
        if (!tc.runsOn(.run)) continue;
        const result = try runTestCase(allocator, tc);
        harness.printCase(tc.name, result);
        if (!harness.isClean(result)) {
            std.debug.print("  path: {s}\n", .{tc.path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("RUN", summary);
    return summary;
}
