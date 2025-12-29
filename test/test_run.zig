const std = @import("std");
const builtin = @import("builtin");
const answers = @import("./answers.zig");
const testing = std.testing;
const process = std.process;
const fs = std.fs;

const IOTest = answers.IOTest;
const peek_result = answers.peek_result;
const print_result = answers.print_result;

const test_results = struct {
    passed: usize,
    failed: usize,
    untested: usize,
};

const Mode = enum {
    PEEK,
    PRINT,
    ERROR,
};

const error_result = struct {
    exit_code: ?u8,
    contains_message: ?[]const u8,
    error_code: ?[]const u8,
};

const TestCase = struct {
    name: []const u8,
    path: []const u8,
    mode: Mode,
    input: ?[]const u8,
    expected_print: ?[]const print_result,
    expected_peek: ?[]const peek_result,
    expected_error: ?error_result,
};

var total_passed: usize = 0;
var total_failed: usize = 0;

const CommandResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
};

fn runDoxaCommandEx(allocator: std.mem.Allocator, path: []const u8, input: ?[]const u8) !CommandResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const exe_path = blk: {
        if (process.getEnvVarOwned(allocator, "DOXA_BIN") catch null) |custom| {
            break :blk custom;
        }
        break :blk try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", "doxa" });
    };
    defer allocator.free(exe_path);

    var child = process.Child.init(&[_][]const u8{ exe_path, "run", path }, child_allocator);
    child.cwd = ".";
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    if (input != null) child.stdin_behavior = .Pipe;

    try child.spawn();

    if (input) |input_data| {
        var stdin_buffer: [1024]u8 = undefined;
        var stdin_writer = child.stdin.?.writer(&stdin_buffer);
        const stdin = &stdin_writer.interface;
        try stdin.writeAll(input_data);
        try stdin.flush();
        child.stdin.?.close();
        // Avoid double-close on Windows: Child.wait() will cleanup streams
        child.stdin = null;
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout = std.array_list.Managed(u8).initCapacity(child_allocator, 4096) catch return error.OutOfMemory;
    defer stdout.deinit();
    while (true) {
        const bytes_read = child.stdout.?.read(&stdout_buffer) catch |err| switch (err) {
            error.BrokenPipe => break,
            else => return err,
        };
        if (bytes_read == 0) break;
        stdout.appendSlice(stdout_buffer[0..bytes_read]) catch return error.OutOfMemory;
    }

    var stderr_buffer: [4096]u8 = undefined;
    var stderr = std.array_list.Managed(u8).initCapacity(child_allocator, 4096) catch return error.OutOfMemory;
    defer stderr.deinit();
    while (true) {
        const bytes_read = child.stderr.?.read(&stderr_buffer) catch |err| switch (err) {
            error.BrokenPipe => break,
            else => return err,
        };
        if (bytes_read == 0) break;
        stderr.appendSlice(stderr_buffer[0..bytes_read]) catch return error.OutOfMemory;
    }
    const term = try child.wait();

    const exit_code = switch (term) {
        .Exited => |code| code,
        .Signal => |signal| {
            std.debug.print("Command terminated with signal {}:\n", .{signal});
            std.debug.print("stderr: {s}\n", .{stderr.items});
            return error.CommandFailed;
        },
        .Stopped => |signal| {
            std.debug.print("Command stopped with signal {}:\n", .{signal});
            std.debug.print("stderr: {s}\n", .{stderr.items});
            return error.CommandFailed;
        },
        .Unknown => {
            std.debug.print("Command failed with unknown error:\n", .{});
            std.debug.print("stderr: {s}\n", .{stderr.items});
            return error.CommandFailed;
        },
    };

    return CommandResult{
        .stdout = try allocator.dupe(u8, stdout.items),
        .stderr = try allocator.dupe(u8, stderr.items),
        .exit_code = @intCast(exit_code),
    };
}

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try runDoxaCommandEx(allocator, path, null);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (result.exit_code != 0) return error.CommandFailed;
    return allocator.dupe(u8, result.stdout);
}

//this function will pipe a char to a doxa command and return the output
fn runDoxaCommandWithInput(allocator: std.mem.Allocator, path: []const u8, input: []const u8) ![]const u8 {
    const result = try runDoxaCommandEx(allocator, path, input);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (result.exit_code != 0) return error.CommandFailed;
    return allocator.dupe(u8, result.stdout);
}

fn runErrorTestCase(allocator: std.mem.Allocator, tc: TestCase) !test_results {
    const result = try runDoxaCommandEx(allocator, tc.path, tc.input);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const expected = tc.expected_error.?;

    var passed: usize = 0;
    var failed: usize = 0;

    // Check exit code
    if (expected.exit_code) |expected_code| {
        if (result.exit_code == expected_code) {
            passed += 1;
        } else {
            std.debug.print("✗ Error test failed: expected exit code {}, got {}\n", .{ expected_code, result.exit_code });
            failed += 1;
        }
    } else {
        passed += 1; // No exit code check required
    }

    // Check if stderr contains expected message
    if (expected.contains_message) |expected_msg| {
        if (std.mem.indexOf(u8, result.stderr, expected_msg) != null) {
            passed += 1;
        } else {
            std.debug.print("✗ Error test failed: expected message \"{s}\" not found in stderr\n", .{expected_msg});
            std.debug.print("stderr: {s}\n", .{result.stderr});
            failed += 1;
        }
    } else {
        passed += 1; // No message check required
    }

    // Check error code in stderr
    if (expected.error_code) |expected_code| {
        if (std.mem.indexOf(u8, result.stderr, expected_code) != null) {
            passed += 1;
        } else {
            std.debug.print("✗ Error test failed: expected error code \"{s}\" not found in stderr\n", .{expected_code});
            std.debug.print("stderr: {s}\n", .{result.stderr});
            failed += 1;
        }
    } else {
        passed += 1; // No error code check required
    }

    return .{ .passed = passed, .failed = failed, .untested = 0 };
}

// print test is for tests which use the print operator for output
fn runTest(allocator: std.mem.Allocator, test_name: []const u8, path: []const u8, expected_results: anytype, input: ?[]const u8, mode: Mode) !test_results {
    _ = test_name; // printing handled by caller

    var output: []const u8 = undefined;
    if (input != null) {
        output = try runDoxaCommandWithInput(allocator, path, input.?);
    } else {
        output = try runDoxaCommand(allocator, path);
    }

    defer allocator.free(output);

    if (output.len == 0) {
        return .{ .passed = 0, .failed = 0 };
    }

    if (mode == .PRINT) {
        return try validatePrintResults(output, expected_results, allocator);
    } else if (mode == .PEEK) {
        return try validatePeekResults(output, expected_results, allocator);
    }

    return .{ .passed = 0, .failed = 0 };
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
    if (tc.mode == .ERROR) {
        return try runErrorTestCase(allocator, tc);
    }

    var output: []const u8 = undefined;
    if (tc.input) |inp| {
        output = try runDoxaCommandWithInput(allocator, tc.path, inp);
    } else {
        output = try runDoxaCommand(allocator, tc.path);
    }
    defer allocator.free(output);

    if (output.len == 0) return .{ .passed = 0, .untested = 0, .failed = 0 };

    return switch (tc.mode) {
        .PRINT => try validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try validatePeekResults(output, tc.expected_peek.?, allocator),
        .ERROR => unreachable, // Handled above
    };
}

test "unified runner" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
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
            .expected_error = null,
        },
        .{
            .name = "brainfuck",
            .path = "./test/examples/brainfuck.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_brainfuck_results[0..],
            .expected_peek = null,
            .expected_error = null,
        },
        .{
            .name = "complex print",
            .path = "./test/misc/complex_print.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_complex_print_results[0..],
            .expected_peek = null,
            .expected_error = null,
        },
        .{
            .name = "array storage migration",
            .path = "./test/misc/array_storage_migration.doxa",
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_array_storage_results[0..],
            .expected_peek = null,
            .expected_error = null,
        },
        .{
            .name = "expressions",
            .path = "./test/misc/expressions.doxa",
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_expressions_results[0..],
            .expected_error = null,
        },
        .{
            .name = "methods",
            .path = "./test/misc/methods.doxa",
            .mode = .PEEK,
            .input = "f\n",
            .expected_print = null,
            .expected_peek = answers.expected_methods_results[0..],
            .expected_error = null,
        },
        .{
            .name = "syntax error",
            .path = "./test/syntax/equals_for_assign.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "equals sign '=' is not used for variable declarations", .error_code = "E2004" },
        },
        .{
            .name = "undefined variable",
            .path = "./test/misc/error_test.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "Undefined variable", .error_code = "E1001" },
        },
        .{
            .name = "as fallback type mismatch",
            .path = "./test/syntax/as_fallback_type_error.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "Fallback for 'as' must produce type float, got int", .error_code = "E1003" },
        },
        .{
            .name = "fixed array push requires dynamic storage",
            .path = "./test/syntax/fixed_array_push_error.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "const literal array push requires dynamic storage",
            .path = "./test/syntax/const_literal_push_error.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "nested const literal array push requires dynamic storage",
            .path = "./test/syntax/nested_const_literal_push_error.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "const alias array push requires dynamic storage",
            .path = "./test/syntax/const_alias_push_error.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "cannot be used with @push", .error_code = "E6018" },
        },
        .{
            .name = "unreachable keyword",
            .path = "./test/misc/unreachable.doxa",
            .mode = .ERROR,
            .input = null,
            .expected_print = null,
            .expected_peek = null,
            .expected_error = .{ .exit_code = 1, .contains_message = "Reached unreachable code", .error_code = null },
        },
    };

    std.debug.print("\n=== Running test suite ===\n", .{});
    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (test_cases) |tc| {
        std.debug.print("  Running: {s}\n", .{tc.name});
        std.debug.print("\n=== Running {s} test ===\n", .{tc.name});
        std.debug.print("Running doxa command with {s}...\n", .{tc.path});
        const result = try runTestCase(allocator, tc);
        if (tc.mode == .ERROR) {
            std.debug.print("Checking error conditions...\n", .{});
        } else {
            std.debug.print("Parsing output...\n", .{});
        }
        if (result.failed == 0 and result.untested == 0) {
            std.debug.print("✓ All {d} test cases passed\n", .{result.passed});
        } else {
            std.debug.print("Results: {d} passed, {d} failed, {d} untested\n", .{ result.passed, result.failed, result.untested });
        }
        std.debug.print("\n=== {s} test complete ===\n", .{tc.name});
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    // Dedicated calculator batch with a single summary
    std.debug.print("\n=== Running calculator test ===\n", .{});
    std.debug.print("Running doxa command with ./test/examples/calculator.doxa...\n", .{});
    var calc_passed: usize = 0;
    var calc_failed: usize = 0;
    for (answers.calculator_io_tests, 0..) |io, idx| {
        // Execute case without printing; compare first line only
        const out = try runDoxaCommandWithInput(allocator, "./test/examples/calculator.doxa", io.input);
        defer allocator.free(out);
        const lines = try parsePrintOutput(out, allocator);
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
    std.debug.print("Parsing output...\n", .{});
    if (calc_failed == 0) {
        std.debug.print("All {d} test cases passed\n", .{answers.calculator_io_tests.len});
    } else {
        std.debug.print("{d} test case(s) failed\n", .{calc_failed});
    }
    std.debug.print("\n=== calculator test complete ===\n", .{});
    passed += calc_passed;
    failed += calc_failed;

    std.debug.print("\nSuite summary: {d} passed, {d} failed, {d} untested\n", .{ passed, failed, untested });
    total_passed += passed;
    total_failed += failed;
    // Note: We'd need to add total_untested to track this globally if desired
}

test "summary" {
    std.debug.print("\n=============================\n", .{});
    std.debug.print(" TEST SUMMARY", .{});
    std.debug.print("\n=============================\n", .{});
    std.debug.print("Total test cases passed: {d}\n", .{total_passed});
    std.debug.print("Total test cases failed: {d}\n", .{total_failed});
    // Note: total_untested would need to be added as a global variable

    if (total_failed == 0) {
        std.debug.print("All tests passed!\n", .{});
    } else {
        std.debug.print("{d} test(s) failed!\n", .{total_failed});
    }
    std.debug.print("=============================\n", .{});
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

fn inferTypeFromValue(value: []const u8) []const u8 {
    if (std.mem.startsWith(u8, value, "0x")) {
        return "byte";
    } else if (std.mem.indexOf(u8, value, ".") != null) {
        return "float";
    } else if (std.mem.startsWith(u8, value, "\"") and std.mem.endsWith(u8, value, "\"")) {
        return "string";
    } else if (std.mem.eql(u8, value, "true") or std.mem.eql(u8, value, "false")) {
        return "tetra";
    } else {
        return "int";
    }
}
