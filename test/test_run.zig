const std = @import("std");
const answers = @import("answers");
const platform = @import("platform");

const harness = @import("harness.zig");

const peek_result = answers.peek_result;
const print_result = answers.print_result;

const test_results = harness.Counts;

const Mode = enum {
    PEEK,
    PRINT,
};

const TestCase = struct {
    name: []const u8,
    path: []const u8,
    mode: Mode,
    input: ?[]const u8,
    expected_print: ?[]const print_result,
    expected_peek: ?[]const peek_result,
    extra_args: []const []const u8 = &.{},
};

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

//this function will pipe a char to a doxa command and return the output
fn runDoxaCommandWithInput(allocator: std.mem.Allocator, path: []const u8, input: []const u8) ![]const u8 {
    const result = try runDoxaCommandEx(allocator, path, input, &.{});
    allocator.free(result.stderr);
    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return error.CommandFailed;
    }
    return result.stdout;
}

fn runTestCase(allocator: std.mem.Allocator, tc: TestCase) !test_results {
    const input: ?[]const u8 = tc.input;
    const result = runDoxaCommandEx(allocator, tc.path, input, tc.extra_args) catch return error.CommandFailed;

    const output = switch (tc.mode) {
        .PRINT => blk: {
            allocator.free(result.stderr);
            if (result.exit_code != 0) {
                allocator.free(result.stdout);
                return error.CommandFailed;
            }
            break :blk result.stdout;
        },
        .PEEK => blk: {
            allocator.free(result.stdout);
            if (result.exit_code != 0) {
                allocator.free(result.stderr);
                return error.CommandFailed;
            }
            break :blk result.stderr;
        },
    };
    defer allocator.free(output);

    return switch (tc.mode) {
        .PRINT => try harness.validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try harness.validatePeekResults(output, tc.expected_peek.?, allocator),
    };
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    platform.enableUtf8Console();

    const test_cases = [_]TestCase{
        .{
            .name = "big file",
            .path = "./test/misc/bigfile.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_bigfile_results[0..],
        },
        .{
            .name = "brainfuck",
            .path = "./test/examples/brainfuck.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_brainfuck_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "complex print",
            .path = "./test/misc/complex_print.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_complex_print_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "array storage migration",
            .path = "./test/misc/array_storage_migration.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_array_storage_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "alias arrays",
            .path = "./test/misc/alias_arrays.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_alias_arrays_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "union narrow",
            .path = "./test/misc/union_narrow.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_union_narrow_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "nested struct return",
            .path = "./test/misc/nested_struct_return.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_nested_struct_return_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "reordered struct literal layout",
            .path = "./test/misc/struct_literal_reordered.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = &[_]print_result{
                .{ .value = "hi Alice" },
                .{ .value = "Alice 30" },
                .{ .value = "Alice 30" },
                .{ .value = "Bob 25" },
            },
            .expected_peek = null,
        },
        .{
            .name = "struct method call receivers",
            .path = "./test/misc/method_call_receiver.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = &[_]print_result{
                .{ .value = "hi Alice" },
                .{ .value = "hi Alice" },
                .{ .value = "hi Bob!" },
                .{ .value = "hi Eve" },
                .{ .value = "hi Frank!" },
                .{ .value = "hi A" },
                .{ .value = "hi B!" },
            },
            .expected_peek = null,
        },
        .{
            .name = "runtime const if",
            .path = "./test/misc/runtime_const_if.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_runtime_const_if_results[0..],
            .expected_peek = null,
            .extra_args = &[_][]const u8{"hello"},
        },
        .{
            .name = "global array push",
            .path = "./test/misc/global_array_push.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_global_array_push_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "module string interp",
            .path = "./test/misc/module_string_interp.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_module_string_interp_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "inline zig string",
            .path = "./test/misc/inline_zig_string.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = &[_]print_result{
                .{ .value = "abc" },
                .{ .value = "hi" },
            },
            .expected_peek = null,
        },
        .{
            .name = "inline zig test",
            .path = "./test/misc/inline_zig_test.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_inline_zig_test_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "zig import test",
            .path = "./test/misc/zig_import_test.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_zig_import_test_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "expressions",
            .path = "./test/misc/expressions.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_expressions_results[0..],
        },
        .{
            .name = "methods",
            .path = "./test/misc/methods.doxa",
            .mode = .PEEK,
            .input = "f\n",
            .expected_print = null,
            .expected_peek = answers.expected_methods_results[0..],
        },
        .{
            .name = "union enum return",
            .path = "./test/misc/union_enum_return.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_union_enum_return_results[0..],
        },
        .{
            .name = "module private call",
            .path = "./test/misc/module_private_call.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_module_private_call_results[0..],
        },
        .{
            .name = "import submodule",
            .path = "./test/misc/import_submodule.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = &[_]print_result{
                .{ .value = "submodule import works" },
                .{ .value = "true" },
            },
            .expected_peek = null,
        },
        .{
            .name = "logic",
            .path = "./test/misc/logic.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_logic_results[0..],
        },
        .{
            .name = "angel",
            .path = "./test/misc/angel.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_angel_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "import test",
            .path = "./test/misc/import_test.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_import_test_results[0..],
        },
        .{
            .name = "basic test",
            .path = "./test/misc/basic_test.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_basic_test_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "alias test",
            .path = "./test/misc/alias_test.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_alias_test_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "list",
            .path = "./test/misc/list.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_list_results[0..],
        },
    };

    harness.printSection("RUN");
    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (test_cases) |tc| {
        const result = try runTestCase(allocator, tc);
        harness.printCase(tc.name, result);
        if (!harness.isClean(result)) {
            std.debug.print("  path: {s}\n", .{tc.path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    // Dedicated calculator batch with a single summary
    var calc_passed: usize = 0;
    var calc_failed: usize = 0;
    for (answers.calculator_io_tests, 0..) |io, idx| {
        // Execute case without printing; compare first line only
        const out = try runDoxaCommandWithInput(allocator, "./test/examples/calculator.doxa", io.input);
        defer allocator.free(out);
        const lines = try harness.parsePrintOutput(out, allocator);
        defer lines.deinit();
        if (lines.items.len > 0 and std.mem.eql(u8, lines.items[0], io.expected_output)) {
            calc_passed += 1;
        } else {
            calc_failed += 1;
            const found_output = if (lines.items.len > 0) lines.items[0] else "(no output)";
            const failure_details = try std.fmt.allocPrint(allocator, "Calculator test case {d} failed:\n  Input: \"{s}\"\n  Expected: \"{s}\"\n  Found:    \"{s}\"", .{ idx + 1, std.mem.trim(u8, io.input, " \t\n\r"), io.expected_output, found_output });
            std.debug.print("{s}\n", .{failure_details});
        }
    }
    const calc_result = test_results{ .passed = calc_passed, .failed = calc_failed, .untested = 0 };
    harness.printCase("calculator", calc_result);
    passed += calc_passed;
    failed += calc_failed;

    // Dedicated terminating-methods batch. Each program must exit non-zero; a
    // specific exit code is asserted for @exit and a stderr substring for the
    // terminating diagnostics (@panic / @assert failure).
    var term_passed: usize = 0;
    var term_failed: usize = 0;
    const term_cases = [_]struct {
        name: []const u8,
        path: []const u8,
        expect_code: ?u8,
        expect_stderr: ?[]const u8,
    }{
        .{ .name = "panic", .path = "./test/misc/panic.doxa", .expect_code = null, .expect_stderr = "panic test message" },
        .{ .name = "exit", .path = "./test/misc/exit.doxa", .expect_code = 7, .expect_stderr = null },
        .{ .name = "assert fail", .path = "./test/misc/assert_fail.doxa", .expect_code = null, .expect_stderr = "assert test message" },
    };
    for (term_cases) |tc| {
        const result = try runDoxaCommandEx(allocator, tc.path, null, &.{});
        allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        var ok = result.exit_code != 0;
        if (tc.expect_code) |code| ok = ok and result.exit_code == code;
        if (tc.expect_stderr) |needle| ok = ok and std.mem.indexOf(u8, result.stderr, needle) != null;
        if (ok) {
            term_passed += 1;
        } else {
            term_failed += 1;
            std.debug.print("Terminating method '{s}' failed:\n  exit={d}\n  stderr: {s}\n", .{ tc.name, result.exit_code, result.stderr });
        }
    }
    const term_result = test_results{ .passed = term_passed, .failed = term_failed, .untested = 0 };
    harness.printCase("terminating methods", term_result);
    passed += term_passed;
    failed += term_failed;

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("RUN", summary);
    return summary;
}
