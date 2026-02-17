const std = @import("std");
const builtin = @import("builtin");

const harness = @import("harness.zig");

const test_results = harness.Counts;

const ErrorExpectation = struct {
    exit_code: ?u8,
    contains_message: ?[]const u8,
    error_code: ?[]const u8,
};

const ErrorCase = struct {
    name: []const u8,
    path: []const u8,
    expected: ErrorExpectation,
};

const CommandResult = harness.CommandResult;

fn runDoxaCommandEx(allocator: std.mem.Allocator, path: []const u8, input: ?[]const u8) !CommandResult {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const exe_path = try harness.doxaExePath(allocator);
    defer allocator.free(exe_path);

    const argv = [_][]const u8{ exe_path, "run", path };
    return try harness.runCommandCapture(allocator, &argv, repo_root, input);
}

fn runErrorCase(allocator: std.mem.Allocator, tc: ErrorCase) !test_results {
    const result = try runDoxaCommandEx(allocator, tc.path, null);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const expected = tc.expected;

    var passed: usize = 0;
    var failed: usize = 0;

    if (expected.exit_code) |expected_code| {
        if (result.exit_code == expected_code) {
            passed += 1;
        } else {
            std.debug.print("Error test failed: expected exit code {}, got {}\n", .{ expected_code, result.exit_code });
            failed += 1;
        }
    } else {
        passed += 1;
    }

    if (expected.contains_message) |expected_msg| {
        if (std.mem.indexOf(u8, result.stderr, expected_msg) != null) {
            passed += 1;
        } else {
            std.debug.print("Error test failed: expected message \"{s}\" not found in stderr\n", .{expected_msg});
            std.debug.print("stderr: {s}\n", .{result.stderr});
            failed += 1;
        }
    } else {
        passed += 1;
    }

    if (expected.error_code) |expected_code| {
        if (std.mem.indexOf(u8, result.stderr, expected_code) != null) {
            passed += 1;
        } else {
            std.debug.print("Error test failed: expected error code \"{s}\" not found in stderr\n", .{expected_code});
            std.debug.print("stderr: {s}\n", .{result.stderr});
            failed += 1;
        }
    } else {
        passed += 1;
    }

    return .{ .passed = passed, .failed = failed, .untested = 0 };
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    const error_cases = [_]ErrorCase{
        .{
            .name = "syntax error",
            .path = "./test/syntax/equals_for_assign.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "equals sign '=' is not used for variable declarations", .error_code = "E2004" },
        },
        .{
            .name = "undefined variable",
            .path = "./test/misc/error_test.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Undefined variable", .error_code = "E1001" },
        },
        .{
            .name = "as fallback type mismatch",
            .path = "./test/syntax/as_fallback_type_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Fallback for 'as' must produce type float, got int", .error_code = "E1003" },
        },
        .{
            .name = "fixed array push requires dynamic storage",
            .path = "./test/syntax/fixed_array_push_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "const literal array push requires dynamic storage",
            .path = "./test/syntax/const_literal_push_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "nested const literal array push requires dynamic storage",
            .path = "./test/syntax/nested_const_literal_push_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "const alias array push requires dynamic storage",
            .path = "./test/syntax/const_alias_push_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "unreachable keyword",
            .path = "./test/misc/unreachable.doxa",
            .expected = .{ .exit_code = 2, .contains_message = "Reached unreachable code", .error_code = null },
        },
    };

    harness.printSection("ERROR");
    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (error_cases) |tc| {
        const result = try runErrorCase(allocator, tc);
        harness.printCase(tc.name, result);
        if (!harness.isClean(result)) {
            std.debug.print("  path: {s}\n", .{tc.path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("ERROR", summary);
    return summary;
}
