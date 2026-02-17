const std = @import("std");
const builtin = @import("builtin");
const answers = @import("answers");

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

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try runDoxaCommandEx(allocator, path, null);
    allocator.free(result.stderr);
    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return error.CommandFailed;
    }
    return result.stdout;
}

//this function will pipe a char to a doxa command and return the output
fn runDoxaCommandWithInput(allocator: std.mem.Allocator, path: []const u8, input: []const u8) ![]const u8 {
    const result = try runDoxaCommandEx(allocator, path, input);
    allocator.free(result.stderr);
    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return error.CommandFailed;
    }
    return result.stdout;
}

fn validatePrintResults(output: []const u8, expected_results: []const print_result, allocator: std.mem.Allocator) !test_results {
    const outputs = try parsePrintOutput(output, allocator);
    defer outputs.deinit();

    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;

    // Count how many tests we actually got results for
    const actual_count = outputs.items.len;
    const expected_count = expected_results.len;

    // Check each result we actually got
    var i: usize = 0;
    while (i < actual_count and i < expected_count) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i], expected_results[i].value)) {
            const found_output = outputs.items[i];
            std.debug.print("✗ Print test case {d} failed:\n  Expected: \"{s}\"\n  Found:    \"{s}\"\n", .{ i + 1, expected_results[i].value, found_output });
            failed += 1;
        } else {
            passed += 1;
        }
    }

    // Count any remaining expected results that we didn't get (untested)
    if (expected_count > actual_count) {
        untested = expected_count - actual_count;
        std.debug.print("⚠ {d} test case(s) were not executed (program may have crashed early)\n", .{untested});
    }

    return .{ .passed = passed, .failed = failed, .untested = untested };
}

fn validatePeekResults(output: []const u8, expected_results: []const peek_result, allocator: std.mem.Allocator) !test_results {
    const outputs = try parsePeekOutput(output, allocator);
    defer outputs.deinit();

    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;

    // Count how many tests we actually got results for
    const actual_count = outputs.items.len;
    const expected_count = expected_results.len;

    // Check each result we actually got
    var i: usize = 0;
    while (i < actual_count and i < expected_count) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i].type, expected_results[i].type) or
            !std.mem.eql(u8, outputs.items[i].value, expected_results[i].value))
        {
            if (i < outputs.items.len) {
                std.debug.print(
                    "✗ Peek test case {d} failed:\n  Expected: {s} = \"{s}\"\n  Found:    {s} = \"{s}\"\n",
                    .{ i + 1, expected_results[i].type, expected_results[i].value, outputs.items[i].type, outputs.items[i].value },
                );
            } else {
                std.debug.print(
                    "✗ Peek test case {d} failed:\n  Expected: {s} = \"{s}\"\n  Found:    (no output)\n",
                    .{ i + 1, expected_results[i].type, expected_results[i].value },
                );
            }
            failed += 1;
        } else {
            passed += 1;
        }
    }

    // Count any remaining expected results that we didn't get (untested)
    if (expected_count > actual_count) {
        untested = expected_count - actual_count;
        std.debug.print("⚠ {d} test case(s) were not executed (program may have crashed early)\n", .{untested});
    }

    return .{ .passed = passed, .failed = failed, .untested = untested };
}

fn runTestCase(allocator: std.mem.Allocator, tc: TestCase) !test_results {
    var output: []const u8 = undefined;
    if (tc.input) |inp| {
        output = try runDoxaCommandWithInput(allocator, tc.path, inp);
    } else {
        output = try runDoxaCommand(allocator, tc.path);
    }
    defer allocator.free(output);

    if (output.len == 0) return .{ .passed = 0, .untested = 0, .failed = 0 };

    return switch (tc.mode) {
        .PRINT => try harness.validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try harness.validatePeekResults(output, tc.expected_peek.?, allocator),
    };
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (builtin.os.tag == .windows) {
        // Set the console output code page to UTF-8 to enable Unicode support
        // I think this is only needed for Windows
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

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

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("RUN", summary);
    return summary;
}

fn parsePeekOutput(output: []const u8, allocator: std.mem.Allocator) !std.array_list.Managed(peek_result) {
    var outputs = std.array_list.Managed(peek_result).init(allocator);

    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        var j = std.mem.indexOf(u8, line, "]") orelse 0;
        if (j == 0) break;
        const lineWithVar = line[j + 1 ..];
        j = std.mem.indexOf(u8, lineWithVar, ":") orelse unreachable;
        const lineWithoutVar = lineWithVar[j + 3 ..];
        const foundType = grabType(lineWithoutVar);
        const foundValue = grabValue(lineWithoutVar);

        // Create and append a new result
        try outputs.append(.{
            .type = foundType,
            .value = foundValue,
        });
    }
    return outputs;
}

fn parsePrintOutput(output: []const u8, allocator: std.mem.Allocator) !std.array_list.Managed([]const u8) {
    var outputs = std.array_list.Managed([]const u8).init(allocator);

    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // For print output, we just get the raw value
        try outputs.append(line);
    }
    return outputs;
}

fn grabType(output: []const u8) []const u8 {
    var foundType: []const u8 = "";
    for (output, 0..) |_, i| {
        if (output[i] == ' ' and output[i + 1] == 'i' and output[i + 2] == 's') {
            foundType = output[0..i];
            break;
        }
    }
    return foundType;
}

fn grabValue(output: []const u8) []const u8 {
    const i = std.mem.indexOf(u8, output, "is") orelse 0;
    if (i == 0) unreachable;
    const trimmedLine = output[i + 3 ..];
    return trimmedLine;
}
