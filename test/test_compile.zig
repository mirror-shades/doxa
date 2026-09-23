const std = @import("std");
const platform = @import("platform");

const cases = @import("cases.zig");
const harness = @import("harness.zig");

const Case = cases.Case;
const test_results = harness.Counts;
const CommandResult = harness.CommandResult;

fn runCompiledBinaryEx(allocator: std.mem.Allocator, binary_path: []const u8, input: ?[]const u8, extra_args: []const []const u8) !CommandResult {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    var argv = std.array_list.Managed([]const u8).init(allocator);
    defer argv.deinit();
    try argv.append(binary_path);
    try argv.appendSlice(extra_args);
    return try harness.runCommandCapture(allocator, argv.items, repo_root, input);
}

fn compileDoxaSource(allocator: std.mem.Allocator, src: []const u8, out: []const u8) !void {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const exe_path = try harness.doxaExePath(allocator);
    defer allocator.free(exe_path);

    const argv = [_][]const u8{ exe_path, "compile", src, "-o", out };
    const result = try harness.runCommandCapture(allocator, &argv, repo_root, null);
    allocator.free(result.stdout);
    if (result.exit_code != 0) {
        std.debug.print("compile failed: {s} -> {s} (exit {d})\n{s}\n", .{ src, out, result.exit_code, result.stderr });
        allocator.free(result.stderr);
        return error.CommandFailed;
    }
    allocator.free(result.stderr);
}

fn runTestCase(allocator: std.mem.Allocator, tc: Case) !test_results {
    const output_path = try cases.outputPathFor(allocator, tc.path);
    const binary_path = try harness.getBinaryPath(allocator, output_path);

    if (tc.mode == .terminate) {
        const result = runCompiledBinaryEx(allocator, binary_path, tc.input, tc.extra_args) catch {
            return .{ .passed = 0, .failed = 0, .untested = 1 };
        };
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

    const exe_result = runCompiledBinaryEx(allocator, binary_path, tc.input, tc.extra_args) catch {
        return .{ .passed = 0, .failed = 0, .untested = tc.expectedCount() };
    };

    if (exe_result.exit_code != 0) {
        defer allocator.free(exe_result.stdout);
        defer allocator.free(exe_result.stderr);

        const stderr_lower = try allocator.dupe(u8, exe_result.stderr);
        defer allocator.free(stderr_lower);
        for (stderr_lower) |*c| {
            c.* = std.ascii.toLower(c.*);
        }

        if (std.mem.indexOf(u8, stderr_lower, "bad cpu type") != null or
            std.mem.indexOf(u8, stderr_lower, "cannot execute binary") != null or
            std.mem.indexOf(u8, stderr_lower, "exec format error") != null or
            std.mem.indexOf(u8, stderr_lower, "wrong architecture") != null)
        {
            return .{ .passed = 0, .failed = 0, .untested = tc.expectedCount() };
        }

        return error.CommandFailed;
    }

    const output = switch (tc.mode) {
        .print => blk: {
            allocator.free(exe_result.stderr);
            break :blk exe_result.stdout;
        },
        .peek => blk: {
            allocator.free(exe_result.stdout);
            break :blk exe_result.stderr;
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

    // Build every program referenced by a compile-pipeline case exactly once.
    var built = std.StringHashMap(void).init(allocator);
    defer built.deinit();
    for (cases.cases) |tc| {
        if (!tc.runsOn(.compile)) continue;
        if (built.contains(tc.path)) continue;
        try built.put(tc.path, {});
        const out = try cases.outputPathFor(allocator, tc.path);
        compileDoxaSource(allocator, tc.path, out) catch |err| {
            std.debug.print("Build step failed: {}\n", .{err});
            return err;
        };
    }
    harness.printCase("build test files", .{ .passed = 1, .failed = 0, .untested = 0 });

    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (cases.cases) |tc| {
        if (!tc.runsOn(.compile)) continue;
        const result = try runTestCase(allocator, tc);
        harness.printCase(tc.name, result);
        if (!harness.isClean(result)) {
            std.debug.print("  src: {s}\n", .{tc.path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("COMPILE", summary);
    return summary;
}
