const std = @import("std");
const platform = @import("platform");

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

    var argv = std.array_list.Managed([]const u8).init(allocator);
    defer argv.deinit();
    try argv.appendSlice(&[_][]const u8{ exe_path, "run", path });
    if (repo_root) |rr| {
        try argv.append(try std.fmt.allocPrint(allocator, "--include={s}", .{rr}));
    }
    return try harness.runCommandCapture(allocator, argv.items, repo_root, input);
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

    platform.enableUtf8Console();
    platform.sealStdHandles();

    const verbose = harness.verboseFromEnv(allocator);

    const error_cases = [_]ErrorCase{
        .{
            .name = "syntax error",
            .path = "./test/syntax/equals_for_assign.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "equals sign '=' is not used for variable declarations", .error_code = "E2004" },
        },
        .{
            .name = "syntax error in imported module",
            .path = "./test/misc/module_syntax_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "expected an expression", .error_code = "E2001" },
        },
        .{
            .name = "alias argument on by-value parameter",
            .path = "./test/misc/alias_argument_not_needed.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "does not require an alias argument", .error_code = "E1028" },
        },
        .{
            .name = "alias argument on specifically imported by-value parameter",
            .path = "./test/misc/alias_specific_import.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "does not require an alias argument", .error_code = "E1028" },
        },
        .{
            .name = "undefined variable",
            .path = "./test/misc/error_test.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Undefined variable", .error_code = "E1001" },
        },
        .{
            .name = "const seeded from a const reference stays immutable",
            .path = "./test/syntax/const_reassign_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Cannot assign to immutable variable", .error_code = "E1015" },
        },
        .{
            .name = "method call with too few arguments",
            .path = "./test/misc/method_too_few_args.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Too few arguments: expected 2, got 1", .error_code = "E5006" },
        },
        .{
            .name = "method call argument type mismatch",
            .path = "./test/misc/method_argument_type_mismatch.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "String is not assignable to type Coord", .error_code = "E1003" },
        },
        .{
            .name = "undefined variable suggestion",
            .path = "./test/misc/undefined_variable_suggestion.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Did you mean 'total'?", .error_code = "E1001" },
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
            .name = "struct literal undeclared field",
            .path = "./test/misc/struct_literal_bad_field.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "struct 'Person' has no field 'kind'; declared fields: name, age", .error_code = "E1011" },
        },
        .{
            .name = "struct literal undeclared field in imported module",
            .path = "./test/misc/module_bad_struct_import.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "struct 'Item' has no field 'kind'; declared fields: name, count, tags", .error_code = "E1011" },
        },
        .{
            .name = "unreachable keyword",
            .path = "./test/misc/unreachable.doxa",
            .expected = .{ .exit_code = 2, .contains_message = "Reached unreachable code", .error_code = null },
        },
        .{
            .name = "group match not exhaustive",
            .path = "./test/syntax/group_non_exhaustive.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Match on group 'Palette' is not exhaustive: 'FileError' not covered", .error_code = "E1033" },
        },
        .{
            .name = "group cycle",
            .path = "./test/syntax/group_cycle.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Cycle detected in group: 'A' includes itself transitively", .error_code = "E1003" },
        },
        .{
            .name = "union arithmetic must be narrowed first",
            .path = "./test/misc/union_arith_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "Cannot use + operator on union type; narrow it with 'as' or match first", .error_code = "E1003" },
        },
        .{
            .name = "@pack requires byte[]",
            .path = "./test/syntax/pack_requires_byte_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "is not implicitly assignable to byte", .error_code = "E1003" },
        },
        .{
            .name = "@push on string requires string value",
            .path = "./test/syntax/push_string_value_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "@push on string requires string value", .error_code = "E1003" },
        },
        .{
            .name = "@insert on string requires string value",
            .path = "./test/syntax/insert_string_value_error.doxa",
            .expected = .{ .exit_code = 1, .contains_message = "@insert on string requires string value", .error_code = "E1003" },
        },
    };

    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (error_cases) |tc| {
        const result = try runErrorCase(allocator, tc);
        harness.printCase(tc.name, result, verbose);
        if (!harness.isClean(result)) {
            std.debug.print("  path: {s}\n", .{tc.path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("ERROR", summary, verbose);
    return summary;
}
