const std = @import("std");
const builtin = @import("builtin");
const answers = @import("answers");
const testing = std.testing;
const process = std.process;
const fs = std.fs;

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
};

const TestCase = struct {
    name: []const u8,
    binary_path: []const u8,
    mode: Mode,
    input: ?[]const u8,
    expected_print: ?[]const print_result,
    expected_peek: ?[]const peek_result,
};

var total_passed: usize = 0;
var total_failed: usize = 0;

const CommandResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
};

fn repoRootFromEnv(allocator: std.mem.Allocator) !?[]const u8 {
    return process.getEnvVarOwned(allocator, "DOXA_REPO_ROOT") catch null;
}

fn runCompiledBinaryEx(allocator: std.mem.Allocator, binary_path: []const u8, input: ?[]const u8) !CommandResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const repo_root = try repoRootFromEnv(child_allocator);

    var child = process.Child.init(&[_][]const u8{binary_path}, child_allocator);
    child.cwd = repo_root orelse ".";
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

fn runCompiledBinary(allocator: std.mem.Allocator, binary_path: []const u8) ![]const u8 {
    const result = try runCompiledBinaryEx(allocator, binary_path, null);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (result.exit_code != 0) return error.CommandFailed;
    return allocator.dupe(u8, result.stdout);
}

fn runCompiledBinaryWithInput(allocator: std.mem.Allocator, binary_path: []const u8, input: []const u8) ![]const u8 {
    const result = try runCompiledBinaryEx(allocator, binary_path, input);
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (result.exit_code != 0) return error.CommandFailed;
    return allocator.dupe(u8, result.stdout);
}

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const repo_root = try repoRootFromEnv(child_allocator);

    const exe_path = blk: {
        if (process.getEnvVarOwned(allocator, "DOXA_BIN") catch null) |custom| {
            break :blk custom;
        }
        const exe_name = if (builtin.os.tag == .windows) "doxa.exe" else "doxa";
        break :blk try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    };
    defer allocator.free(exe_path);

    var child = process.Child.init(&[_][]const u8{ exe_path, "run", path }, child_allocator);
    child.cwd = repo_root orelse ".";
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

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

    if (exit_code != 0) return error.CommandFailed;
    return allocator.dupe(u8, stdout.items);
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
    var output: []const u8 = undefined;
    if (tc.input) |inp| {
        output = try runCompiledBinaryWithInput(allocator, tc.binary_path, inp);
    } else {
        output = try runCompiledBinary(allocator, tc.binary_path);
    }
    defer allocator.free(output);

    if (output.len == 0) return .{ .passed = 0, .untested = 0, .failed = 0 };

    return switch (tc.mode) {
        .PRINT => try validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try validatePeekResults(output, tc.expected_peek.?, allocator),
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
    if (builtin.os.tag == .windows) {
        if (std.mem.endsWith(u8, base, ".exe")) return try alloc.dupe(u8, base);
        return try std.fmt.allocPrint(alloc, "{s}.exe", .{base});
    }
    return try alloc.dupe(u8, base);
}

test "compile and run tests" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    // First, run test_build.doxa to compile the files
    std.debug.print("\n=== Building test files ===\n", .{});
    std.debug.print("Running doxa command with ./test/test_build.doxa...\n", .{});
    _ = runDoxaCommand(allocator, "./test/test_build.doxa") catch |err| {
        std.debug.print("Failed to build test files: {}\n", .{err});
        return err;
    };
    std.debug.print("Build complete\n", .{});

    // Test cases for compiled binaries
    const bigfile_path = try getBinaryPath(allocator, "./test/out/bigfile");
    const complex_print_path = try getBinaryPath(allocator, "./test/out/complex_print");
    const expressions_path = try getBinaryPath(allocator, "./test/out/expressions");
    const brainfuck_path = try getBinaryPath(allocator, "./test/out/brainfuck");

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
    };

    std.debug.print("\n=== Running compiled binary tests ===\n", .{});
    var passed: usize = 0;
    var failed: usize = 0;
    var untested: usize = 0;
    for (test_cases) |tc| {
        std.debug.print("  Running: {s}\n", .{tc.name});
        std.debug.print("\n=== Running {s} test ===\n", .{tc.name});
        std.debug.print("Running compiled binary {s}...\n", .{tc.binary_path});
        const result = try runTestCase(allocator, tc);
        std.debug.print("Parsing output...\n", .{});
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

    std.debug.print("\nSuite summary: {d} passed, {d} failed, {d} untested\n", .{ passed, failed, untested });
    total_passed += passed;
    total_failed += failed;
}

test "summary" {
    std.debug.print("\n=============================\n", .{});
    std.debug.print(" COMPILE TEST SUMMARY\n", .{});
    std.debug.print("=============================\n", .{});
    std.debug.print("Total: {d} passed, {d} failed\n", .{ total_passed, total_failed });
    std.debug.print("=============================\n", .{});
}
