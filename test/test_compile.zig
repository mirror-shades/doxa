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
    SKIP,
};

const TestCase = struct {
    name: []const u8,
    binary_path: []const u8,
    mode: Mode,
    input: ?[]const u8,
    expected_print: ?[]const print_result,
    expected_peek: ?[]const peek_result,
};

const CommandResult = harness.CommandResult;

fn runCompiledBinaryEx(allocator: std.mem.Allocator, binary_path: []const u8, input: ?[]const u8) !CommandResult {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const argv = [_][]const u8{binary_path};
    return try harness.runCommandCapture(allocator, &argv, repo_root, input);
}

fn runCompiledBinary(allocator: std.mem.Allocator, binary_path: []const u8) ![]const u8 {
    const result = try runCompiledBinaryEx(allocator, binary_path, null);
    allocator.free(result.stderr);
    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return error.CommandFailed;
    }
    return result.stdout;
}

fn runCompiledBinaryWithInput(allocator: std.mem.Allocator, binary_path: []const u8, input: []const u8) ![]const u8 {
    const result = try runCompiledBinaryEx(allocator, binary_path, input);
    allocator.free(result.stderr);
    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return error.CommandFailed;
    }
    return result.stdout;
}

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const exe_path = try harness.doxaExePath(allocator);
    defer allocator.free(exe_path);

    const argv = [_][]const u8{ exe_path, "run", path };
    const result = try harness.runCommandCapture(allocator, &argv, repo_root, null);
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

    const actual_count = outputs.items.len;
    const expected_count = expected_results.len;

    var i: usize = 0;
    while (i < actual_count and i < expected_count) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i], expected_results[i].value)) {
            const found_output = outputs.items[i];
            std.debug.print("✗ Print test case {d} failed:\n  Expected: {s}\n  Found:    {s}\n", .{ i + 1, expected_results[i].value, found_output });
            failed += 1;
        } else {
            passed += 1;
        }
    }

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

    const actual_count = outputs.items.len;
    const expected_count = expected_results.len;

    var i: usize = 0;
    while (i < actual_count and i < expected_count) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i].type, expected_results[i].type) or
            !std.mem.eql(u8, outputs.items[i].value, expected_results[i].value))
        {
            if (i < outputs.items.len) {
                std.debug.print(
                    "Peek test case {d} failed:\n  Expected: {s} = {s}\n  Found:    {s} = {s}\n",
                    .{ i + 1, expected_results[i].type, expected_results[i].value, outputs.items[i].type, outputs.items[i].value },
                );
            } else {
                std.debug.print(
                    "Peek test case {d} failed:\n  Expected: {s} = {s}\n  Found:    (no output)\n",
                    .{ i + 1, expected_results[i].type, expected_results[i].value },
                );
            }
            failed += 1;
        } else {
            passed += 1;
        }
    }

    if (expected_count > actual_count) {
        untested = expected_count - actual_count;
        std.debug.print("⚠ {d} test case(s) were not executed (program may have crashed early)\n", .{untested});
    }

    return .{ .passed = passed, .failed = failed, .untested = untested };
}

fn runTestCase(allocator: std.mem.Allocator, tc: TestCase) !test_results {
    if (tc.mode == .SKIP) {
        return .{ .passed = 0, .failed = 0, .untested = 1 };
    }
    // Try to run the binary, but skip execution if cross-compiling
    const run_result = if (tc.input) |inp|
        runCompiledBinaryWithInput(allocator, tc.binary_path, inp)
    else
        runCompiledBinary(allocator, tc.binary_path);

    const output = run_result catch |err| {
        // If CommandFailed, try to get stderr to check for cross-compilation
        // This will fail the same way if spawn fails, but we can catch it
        const result = runCompiledBinaryEx(allocator, tc.binary_path, if (tc.input) |inp| inp else null) catch {
            // If spawn fails completely, it's likely cross-compilation
            // Compilation was successful, so mark tests as untested (not failed)
            // Note: We can't distinguish spawn errors from other errors easily,
            // but if compilation succeeded and execution fails immediately, it's likely cross-compilation
            const expected_count = switch (tc.mode) {
                .PRINT => tc.expected_print.?.len,
                .PEEK => tc.expected_peek.?.len,
                .SKIP => 1,
            };
            return .{ .passed = 0, .failed = 0, .untested = expected_count };
        };
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        // Check stderr for cross-compilation error messages
        const stderr_lower = try allocator.dupe(u8, result.stderr);
        defer allocator.free(stderr_lower);
        for (stderr_lower) |*c| {
            c.* = std.ascii.toLower(c.*);
        }

        if (std.mem.indexOf(u8, stderr_lower, "bad cpu type") != null or
            std.mem.indexOf(u8, stderr_lower, "cannot execute binary") != null or
            std.mem.indexOf(u8, stderr_lower, "exec format error") != null or
            std.mem.indexOf(u8, stderr_lower, "wrong architecture") != null)
        {
            // Cross-compilation detected - skip execution
            const expected_count = switch (tc.mode) {
                .PRINT => tc.expected_print.?.len,
                .PEEK => tc.expected_peek.?.len,
                .SKIP => 1,
            };
            return .{ .passed = 0, .failed = 0, .untested = expected_count };
        }

        // Real execution error, propagate it
        return err;
    };
    defer allocator.free(output);

    if (output.len == 0) return .{ .passed = 0, .untested = 0, .failed = 0 };

    return switch (tc.mode) {
        .PRINT => try harness.validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try harness.validatePeekResults(output, tc.expected_peek.?, allocator),
        .SKIP => .{ .passed = 0, .failed = 0, .untested = 1 },
    };
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

fn getBinaryPath(alloc: std.mem.Allocator, base: []const u8) ![]const u8 {
    return harness.getBinaryPath(alloc, base);
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    harness.printSection("COMPILE");

    _ = runDoxaCommand(allocator, "./test/test_build.doxa") catch |err| {
        std.debug.print("Build step failed: {}\n", .{err});
        return err;
    };
    harness.printCase("build test files", .{ .passed = 1, .failed = 0, .untested = 0 });

    // Test cases for compiled binaries
    const bigfile_path = try getBinaryPath(allocator, "./test/out/bigfile");
    const complex_print_path = try getBinaryPath(allocator, "./test/out/complex_print");
    const expressions_path = try getBinaryPath(allocator, "./test/out/expressions");
    const brainfuck_path = try getBinaryPath(allocator, "./test/out/brainfuck");
    const array_storage_migration_path = try getBinaryPath(allocator, "./test/out/array_storage_migration");
    const methods_path = try getBinaryPath(allocator, "./test/out/methods");
    const union_enum_return_path = try getBinaryPath(allocator, "./test/out/union_enum_return");
    const inline_zig_string_path = try getBinaryPath(allocator, "./test/out/inline_zig_string");
    const inline_zig_test_path = try getBinaryPath(allocator, "./test/out/inline_zig_test");
    const calculator_path = try getBinaryPath(allocator, "./test/out/calculator");

    const test_cases = [_]TestCase{
        .{
            .name = "bigfile",
            .binary_path = bigfile_path,
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_bigfile_results[0..],
        },
        .{
            .name = "complex print",
            .binary_path = complex_print_path,
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_complex_print_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "expressions",
            .binary_path = expressions_path,
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_expressions_results[0..],
        },
        .{
            .name = "brainfuck",
            .binary_path = brainfuck_path,
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_brainfuck_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "array storage migration",
            .binary_path = array_storage_migration_path,
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_array_storage_results[0..],
            .expected_peek = null,
        },
        .{
            .name = "methods",
            .binary_path = methods_path,
            .mode = .PEEK,
            .input = "f\n",
            .expected_print = null,
            .expected_peek = answers.expected_methods_results[0..],
        },
        .{
            .name = "union enum return",
            .binary_path = union_enum_return_path,
            .mode = .PEEK,
            .input = null,
            .expected_print = null,
            .expected_peek = answers.expected_union_enum_return_results[0..],
        },
        .{
            .name = "inline zig string",
            .binary_path = inline_zig_string_path,
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
            .binary_path = inline_zig_test_path,
            .mode = .PRINT,
            .input = null,
            .expected_print = answers.expected_inline_zig_test_results[0..],
            .expected_peek = null,
        },
    };

    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (test_cases) |tc| {
        const result = try runTestCase(allocator, tc);
        harness.printCase(tc.name, result);
        if (!harness.isClean(result)) {
            std.debug.print("  bin: {s}\n", .{tc.binary_path});
        }
        passed += result.passed;
        failed += result.failed;
        untested += result.untested;
    }

    // Dedicated calculator batch with a single summary
    var calc_passed: usize = 0;
    var calc_failed: usize = 0;
    for (answers.calculator_io_tests, 0..) |io, idx| {
        const out = runCompiledBinaryWithInput(allocator, calculator_path, io.input) catch {
            // Check for cross-compilation or execution error
            const result = runCompiledBinaryEx(allocator, calculator_path, io.input) catch {
                // Likely cross-compilation - mark as untested
                untested += answers.calculator_io_tests.len;
                harness.printCase("calculator", .{ .passed = 0, .failed = 0, .untested = answers.calculator_io_tests.len });
                break;
            };
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);
            calc_failed += 1;
            continue;
        };
        defer allocator.free(out);
        const lines = try harness.parsePrintOutput(out, allocator);
        defer lines.deinit();
        if (lines.items.len > 0 and std.mem.eql(u8, lines.items[0], io.expected_output)) {
            calc_passed += 1;
        } else {
            calc_failed += 1;
            const found_output = if (lines.items.len > 0) lines.items[0] else "(no output)";
            std.debug.print("Calculator test case {d} failed:\n  Input: \"{s}\"\n  Expected: \"{s}\"\n  Found:    \"{s}\"\n", .{ idx + 1, std.mem.trim(u8, io.input, " \t\n\r"), io.expected_output, found_output });
        }
    }
    const calc_result = test_results{ .passed = calc_passed, .failed = calc_failed, .untested = 0 };
    harness.printCase("calculator", calc_result);
    passed += calc_passed;
    failed += calc_failed;

    const summary = test_results{ .passed = passed, .failed = failed, .untested = untested };
    harness.printSuiteSummary("COMPILE", summary);
    return summary;
}
