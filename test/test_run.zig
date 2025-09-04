const std = @import("std");
const testing = std.testing;
const process = std.process;
const fs = std.fs;

const test_results = struct {
    passed: usize,
    failed: usize,
};

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

const IOTest = struct {
    input: []const u8,
    expected_output: []const u8,
};

const peek_result = struct {
    type: []const u8,
    value: []const u8,
};

const print_result = struct {
    value: []const u8,
};

const expected_complex_print_results = [_]print_result{
    .{ .value = "The variable \"number\" is equal to 10" },
    .{ .value = "The variable name \"number\" has 6 letters" },
    .{ .value = "The variable \"number\" is equal to 10 and the name has 6 letters" },
};

const expected_brainfuck_results = [_]print_result{
    .{ .value = "Input: Output: 0x67" },
};

const expected_calculator_results = [_]print_result{
    .{ .value = "Output: 3" },
    .{ .value = "Output: -1" },
    .{ .value = "Output: 212" },
    .{ .value = "Output: 7" },
    .{ .value = "Output: 1" },
    .{ .value = "Output: 4" },
};

const calculator_io_tests = [_]IOTest{
    .{ .input = "5 + -2\n", .expected_output = "Output: 3" },
    .{ .input = "5 + -2 * 3\n", .expected_output = "Output: -1" },
    .{ .input = "1000 / 5 + 6 * 2\n", .expected_output = "Output: 212" },
    .{ .input = "2 * (3 + 2) - 6 / 2\n", .expected_output = "Output: 7" },
    .{ .input = "10 / 2 / 5\n", .expected_output = "Output: 1" },
    .{ .input = "20-16\n", .expected_output = "Output: 4" },
};

const expected_methods_results = [_]peek_result{
    .{ .type = "string", .value = "\"f\"" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "11" },
    .{ .type = "int[]", .value = "[1, 2, 3, 4, 5]" },
    .{ .type = "int[]", .value = "[1, 2, 3, 4, 5, 6]" },
    .{ .type = "byte[]", .value = "[0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64]" },
    .{ .type = "string", .value = "\"42\"" },
    .{ .type = "string", .value = "\"8.8\"" },
    .{ .type = "string", .value = "\"0x0C\"" },
    .{ .type = "string", .value = "\"[1, 2, 3, 4, 5, 6]\"" },
    .{ .type = "int", .value = "8" },
    .{ .type = "int", .value = "12" },
    .{ .type = "float", .value = "42.0" },
    .{ .type = "float", .value = "12.0" },
    .{ .type = "byte", .value = "0x08" },
    .{ .type = "byte", .value = "0x2A" },
    .{ .type = "string", .value = "\"int\"" },
    .{ .type = "string", .value = "\"byte\"" },
    .{ .type = "string", .value = "\"float\"" },
    .{ .type = "string", .value = "\"string\"" },
    .{ .type = "string", .value = "\"tetra\"" },
    .{ .type = "string", .value = "\"array\"" },
};

const expected_expressions_results = [_]peek_result{
    .{ .type = "int", .value = "25" },
    .{ .type = "int", .value = "30" },
    .{ .type = "float", .value = "11.0" },
    .{ .type = "float", .value = "16.0" },
    .{ .type = "int", .value = "16" },
    .{ .type = "int", .value = "64" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "3.3333333333333335" },
    .{ .type = "float", .value = "2.0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "float", .value = "10.0" },
    .{ .type = "int", .value = "50" },
    .{ .type = "int", .value = "80" },
    .{ .type = "int", .value = "146" },
    .{ .type = "int", .value = "26" },
    .{ .type = "int", .value = "10" },
    .{ .type = "int", .value = "24" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "512" },
    .{ .type = "float", .value = "7.5" },
    .{ .type = "float", .value = "2.5" },
    .{ .type = "float", .value = "5.0" },
    .{ .type = "int", .value = "16" },
    .{ .type = "float", .value = "4.0" },
    .{ .type = "int", .value = "1026" },
    .{ .type = "int", .value = "1" },
    .{ .type = "float", .value = "1000000.000001" },
    .{ .type = "int", .value = "1073741824" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "5" },
    .{ .type = "float", .value = "0.30000000000000004" },
    .{ .type = "float", .value = "0.3333333333333333" },
    .{ .type = "float", .value = "6.0" },
    .{ .type = "float", .value = "1.0" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "25" },
    .{ .type = "int", .value = "30" },
    .{ .type = "float", .value = "11.0" },
    .{ .type = "float", .value = "16.0" },
    .{ .type = "int", .value = "16" },
    .{ .type = "int", .value = "64" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "50.5" },
    .{ .type = "int", .value = "-75" },
    .{ .type = "float", .value = "3.3333333333333335" },
    .{ .type = "float", .value = "2.0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "float", .value = "10.0" },
    .{ .type = "int", .value = "50" },
    .{ .type = "int", .value = "80" },
    .{ .type = "int", .value = "146" },
    .{ .type = "int", .value = "26" },
    .{ .type = "int", .value = "10" },
    .{ .type = "int", .value = "24" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "512" },
    .{ .type = "float", .value = "7.5" },
    .{ .type = "float", .value = "2.5" },
    .{ .type = "float", .value = "5.0" },
    .{ .type = "int", .value = "16" },
    .{ .type = "float", .value = "4.0" },
    .{ .type = "int", .value = "1026" },
    .{ .type = "int", .value = "1" },
    .{ .type = "float", .value = "1000000.000001" },
    .{ .type = "int", .value = "1073741824" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "5" },
    .{ .type = "int", .value = "5" },
    .{ .type = "float", .value = "0.30000000000000004" },
    .{ .type = "float", .value = "0.3333333333333333" },
    .{ .type = "float", .value = "6.0" },
    .{ .type = "float", .value = "1.0" },
    .{ .type = "int", .value = "3" },
};

const expected_bigfile_results = [_]peek_result{
    .{ .type = "int", .value = "81" },
    .{ .type = "string", .value = "\"Overflow\"" },
    .{ .type = "int", .value = "-1" },
    .{ .type = "string", .value = "\"imported function\"" },
    .{ .type = "int", .value = "60" },
    .{ .type = "int", .value = "100" },
    .{ .type = "string", .value = "\"It's blue\"" },
    .{ .type = "string", .value = "\"It's something else\"" },

    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "10" },
    .{ .type = "int", .value = "8" },
    .{ .type = "int", .value = "6" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "2" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "1" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "4" },
    .{ .type = "int", .value = "4" },

    .{ .type = "string", .value = "\"buzz\"" },
    .{ .type = "int", .value = "101" },
    .{ .type = "string", .value = "\"fizz\"" },
    .{ .type = "int", .value = "103" },
    .{ .type = "int", .value = "104" },
    .{ .type = "string", .value = "\"fizzbuzz\"" },
    .{ .type = "int", .value = "106" },
    .{ .type = "int", .value = "107" },
    .{ .type = "string", .value = "\"fizz\"" },
    .{ .type = "int", .value = "109" },
    .{ .type = "string", .value = "\"buzz\"" },
    .{ .type = "string", .value = "\"fizz\"" },
    .{ .type = "int", .value = "112" },
    .{ .type = "int", .value = "113" },
    .{ .type = "string", .value = "\"fizz\"" },
    .{ .type = "string", .value = "\"buzz\"" },
    .{ .type = "string", .value = "\"buzzer\"" },
    .{ .type = "int", .value = "1001" },
    .{ .type = "string", .value = "\"fizzer\"" },
    .{ .type = "int", .value = "1003" },
    .{ .type = "int", .value = "1004" },
    .{ .type = "string", .value = "\"fizzbuzzer\"" },
    .{ .type = "int", .value = "1006" },
    .{ .type = "int", .value = "1007" },
    .{ .type = "string", .value = "\"fizzer\"" },
    .{ .type = "int", .value = "1009" },
    .{ .type = "string", .value = "\"buzzer\"" },
    .{ .type = "string", .value = "\"fizzer\"" },
    .{ .type = "int", .value = "1012" },
    .{ .type = "int", .value = "1013" },
    .{ .type = "string", .value = "\"fizzer\"" },
    .{ .type = "string", .value = "\"buzzer\"" },
    .{ .type = "int", .value = "660" },
    .{ .type = "int", .value = "726" },
    .{ .type = "int", .value = "792" },
    .{ .type = "int", .value = "858" },
    .{ .type = "int", .value = "924" },
    .{ .type = "int", .value = "2" },
    .{ .type = "string", .value = "\"/\"" },
    .{ .type = "int", .value = "-1" },
    .{ .type = "string", .value = "\"=\"" },
    .{ .type = "int", .value = "1" },
    .{ .type = "string", .value = "\"-\"" },
    .{ .type = "int", .value = "1" },
    .{ .type = "string", .value = "\"+\"" },
    .{ .type = "int", .value = "2" },
    .{ .type = "string", .value = "\"*\"" },
    .{ .type = "string", .value = "\"true\"" },
    .{ .type = "string", .value = "\"false\"" },
    .{ .type = "int", .value = "0" },
    .{ .type = "string", .value = "\"hello\"" },
    .{ .type = "int", .value = "1" },
    .{ .type = "string", .value = "\"world\"" },
    .{ .type = "int", .value = "2" },
    .{ .type = "string", .value = "\"foo\"" },
    .{ .type = "int", .value = "3" },
    .{ .type = "string", .value = "\"bar\"" },
    .{ .type = "string", .value = "\"true\"" },
    .{ .type = "int", .value = "10" },
    .{ .type = "int", .value = "20" },
    .{ .type = "int", .value = "30" },
    .{ .type = "float", .value = "0.0" },
    .{ .type = "int", .value = "50" },
    .{ .type = ">int | float | byte", .value = "10" },
    .{ .type = "int | >float | byte", .value = "12.345" },
    .{ .type = "int | float | >byte", .value = "0x0A" },
    .{ .type = "int", .value = "17" },
    .{ .type = "string", .value = "\"return\"" },
    .{ .type = "string", .value = "\"implicit\"" },
    .{ .type = "int", .value = "26" },
    .{ .type = "int", .value = "4" },
    .{ .type = "byte[]", .value = "[0x4D, 0x69, 0x6B, 0x65]" },
    .{ .type = "string", .value = "\"M\"" },
    .{ .type = "byte", .value = "0x4D" },
    .{ .type = "int", .value = "1000" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "both" },
    .{ .type = "tetra", .value = "neither" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "string", .value = "\"I am alive\"" },
    .{ .type = "string", .value = "\"I am dead\"" },
    .{ .type = "string", .value = "\"I am alive\"" },
    .{ .type = "string", .value = "\"I am dead\"" },
    .{ .type = "int[]", .value = "[111111, 222222, 333333, 444444, 555555]" },
    .{ .type = "int", .value = "444444" },
    .{ .type = "int", .value = "666666" },
    .{ .type = "int", .value = "420000" },
    .{ .type = "int", .value = "420000" },
    .{ .type = "int[]", .value = "[111111, 222222, 333333, 666666, 555555]" },
    .{ .type = "int[]", .value = "[1, 2, 3, 4, 5, 6]" },
    .{ .type = "int[]", .value = "[1, 2, 3, 4, 5, 6]" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "0" },
    .{ .type = "float", .value = "0.0" },
    .{ .type = "float", .value = "0.0" },
    .{ .type = "byte", .value = "0x00" },
    .{ .type = "byte", .value = "0x00" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "string", .value = "\"\"" },
    .{ .type = "string", .value = "\"\"" },
    .{ .type = "string", .value = "\"Fluffy\"" },
    .{ .type = "string", .value = ".LION" },
    .{ .type = "string", .value = "\"Sam\"" },
    .{ .type = "string", .value = ".PENGUIN" },
    .{ .type = "string", .value = "\"Nemo\"" },
    .{ .type = "string", .value = ".WHALE" },
    .{ .type = "int", .value = "779" },
    .{ .type = "int", .value = "782" },
    .{ .type = "string", .value = "\"integer\"" },
    .{ .type = "string", .value = "\"float\"" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "0" },
    .{ .type = "int", .value = "3" },
    .{ .type = "int", .value = "0" },
    .{ .type = "float", .value = "1.5" },
    .{ .type = "float", .value = "3.5" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "int", .value = "-58" },
    .{ .type = "string", .value = "\"I am alive\"" },
    .{ .type = "string", .value = "\"I am dead\"" },
    .{ .type = "string", .value = "\"I am alive\"" },
    .{ .type = "string", .value = "\"I am dead\"" },
    .{ .type = ">string | IndexError", .value = "\"hello\"" },
    .{ .type = ">string | IndexError", .value = "\"world\"" },
    .{ .type = "int", .value = "0" },
    .{ .type = "float", .value = "0.0" },
    .{ .type = "byte", .value = "0x00" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "string", .value = "\"\"" },
    .{ .type = "string", .value = "\"int\"" },
    .{ .type = "string", .value = "\"byte\"" },
    .{ .type = "string", .value = "\"float\"" },
    .{ .type = "string", .value = "\"string\"" },
    .{ .type = "string", .value = "\"tetra\"" },
    .{ .type = "string", .value = "\"array\"" },
    .{ .type = "string", .value = "\"struct\"" },
    .{ .type = "string", .value = "\"Employee\"" },
    .{ .type = "string", .value = "\"enum\"" },
    .{ .type = "string", .value = "\"Color\"" },
    .{ .type = "string", .value = "\"map\"" },
};

var total_passed: usize = 0;
var total_failed: usize = 0;

fn runDoxaCommandEx(allocator: std.mem.Allocator, path: []const u8, input: ?[]const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const exe_path = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", "doxa" });
    defer allocator.free(exe_path);

    var child = process.Child.init(&[_][]const u8{ exe_path, "run", path }, child_allocator);
    child.cwd = ".";
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    if (input != null) child.stdin_behavior = .Pipe;

    try child.spawn();

    if (input) |input_data| {
        try child.stdin.?.writer().writeAll(input_data);
        child.stdin.?.close();
        // Avoid double-close on Windows: Child.wait() will cleanup streams
        child.stdin = null;
    }

    const stdout = try child.stdout.?.reader().readAllAlloc(child_allocator, std.math.maxInt(usize));
    const stderr = try child.stderr.?.reader().readAllAlloc(child_allocator, std.math.maxInt(usize));
    const term = try child.wait();

    if (term.Exited != 0) {
        std.debug.print("Command failed with exit code {}:\n", .{term.Exited});
        std.debug.print("stderr: {s}\n", .{stderr});
        return error.CommandFailed;
    }

    return try allocator.dupe(u8, stdout);
}

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return runDoxaCommandEx(allocator, path, null);
}

//this function will pipe a char to a doxa command and return the output
fn runDoxaCommandWithInput(allocator: std.mem.Allocator, path: []const u8, input: []const u8) ![]const u8 {
    return runDoxaCommandEx(allocator, path, input);
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

    var i: usize = 0;
    while (i < outputs.items.len and i < expected_results.len) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i], expected_results[i].value)) {
            const found_output = outputs.items[i];
            std.debug.print("✗ Print test case {d} failed:\n  Expected: \"{s}\"\n  Found:    \"{s}\"\n", .{ i + 1, expected_results[i].value, found_output });
            return .{ .passed = i, .failed = 1 };
        }
    }
    return .{ .passed = i, .failed = 0 };
}

fn validatePeekResults(output: []const u8, expected_results: []const peek_result, allocator: std.mem.Allocator) !test_results {
    const outputs = try parsePeekOutput(output, allocator);
    defer outputs.deinit();

    var i: usize = 0;
    while (i < outputs.items.len and i < expected_results.len) : (i += 1) {
        if (!std.mem.eql(u8, outputs.items[i].value, expected_results[i].value)) {
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
            return .{ .passed = i, .failed = 1 };
        }
    }
    return .{ .passed = i, .failed = 0 };
}

fn runTestCase(allocator: std.mem.Allocator, tc: TestCase) !test_results {
    var output: []const u8 = undefined;
    if (tc.input) |inp| {
        output = try runDoxaCommandWithInput(allocator, tc.path, inp);
    } else {
        output = try runDoxaCommand(allocator, tc.path);
    }
    defer allocator.free(output);

    if (output.len == 0) return .{ .passed = 0, .failed = 0 };

    return switch (tc.mode) {
        .PRINT => try validatePrintResults(output, tc.expected_print.?, allocator),
        .PEEK => try validatePeekResults(output, tc.expected_peek.?, allocator),
    };
}

test "unified runner" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const test_cases = [_]TestCase{
        .{ .name = "big file", .path = "./test/misc/bigfile.doxa", .mode = .PEEK, .input = null, .expected_print = null, .expected_peek = expected_bigfile_results[0..] },
        .{ .name = "brainfuck", .path = "./test/examples/brainfuck.doxa", .mode = .PRINT, .input = "f\n", .expected_print = expected_brainfuck_results[0..], .expected_peek = null },
        .{ .name = "complex print", .path = "./test/misc/complex_print.doxa", .mode = .PRINT, .input = null, .expected_print = expected_complex_print_results[0..], .expected_peek = null },
        .{ .name = "expressions", .path = "./test/misc/expressions.doxa", .mode = .PEEK, .input = null, .expected_print = null, .expected_peek = expected_expressions_results[0..] },
        .{ .name = "methods", .path = "./test/misc/methods.doxa", .mode = .PEEK, .input = "f\n", .expected_print = null, .expected_peek = expected_methods_results[0..] },
    };

    std.debug.print("\n=== Running test suite ===\n", .{});
    var passed: usize = 0;
    var failed: usize = 0;
    for (test_cases) |tc| {
        std.debug.print("  Running: {s}\n", .{tc.name});
        std.debug.print("\n=== Running {s} test ===\n", .{tc.name});
        std.debug.print("Running doxa command with {s}...\n", .{tc.path});
        const result = try runTestCase(allocator, tc);
        std.debug.print("Parsing output...\n", .{});
        if (result.failed == 0) {
            std.debug.print("✓ All {d} test cases passed\n", .{result.passed});
        }
        std.debug.print("\n=== {s} test complete ===\n", .{tc.name});
        passed += result.passed;
        failed += result.failed;
    }

    // Dedicated calculator batch with a single summary
    std.debug.print("\n=== Running calculator test ===\n", .{});
    std.debug.print("Running doxa command with ./test/examples/calculator.doxa...\n", .{});
    var calc_passed: usize = 0;
    var calc_failed: usize = 0;
    for (calculator_io_tests, 0..) |io, idx| {
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
        std.debug.print("✓ All {d} test cases passed\n", .{calculator_io_tests.len});
    } else {
        std.debug.print("✗ {d} test case(s) failed\n", .{calc_failed});
    }
    std.debug.print("\n=== calculator test complete ===\n", .{});
    passed += calc_passed;
    failed += calc_failed;

    std.debug.print("\nSuite summary: {d} passed, {d} failed\n", .{ passed, failed });
    total_passed += passed;
    total_failed += failed;
}

test "summary" {
    std.debug.print("\n=============================\n", .{});
    std.debug.print(" TEST SUMMARY", .{});
    std.debug.print("\n=============================\n", .{});
    std.debug.print("Total test cases passed: {d}\n", .{total_passed});
    std.debug.print("Total test cases failed: {d}\n", .{total_failed});

    if (total_failed == 0) {
        std.debug.print("🎉 ALL TESTS PASSED! 🎉\n", .{});
    } else {
        std.debug.print("❌ {d} TEST(S) FAILED!\n", .{total_failed});
    }
    std.debug.print("=============================\n", .{});
}

fn parsePeekOutput(output: []const u8, allocator: std.mem.Allocator) !std.ArrayList(peek_result) {
    var outputs = std.ArrayList(peek_result).init(allocator);

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

fn parsePrintOutput(output: []const u8, allocator: std.mem.Allocator) !std.ArrayList([]const u8) {
    var outputs = std.ArrayList([]const u8).init(allocator);

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
