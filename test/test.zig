const std = @import("std");
const testing = std.testing;
const process = std.process;
const fs = std.fs;

fn runDoxaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    // Verify test file exists and print contents
    const file = fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Failed to open test file {s}: {}\n", .{ path, err });
        return error.FileNotFound;
    };
    defer file.close();

    const file_contents = try file.readToEndAlloc(child_allocator, std.math.maxInt(usize));
    defer child_allocator.free(file_contents);

    // Get the path to the para executable
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", "doxa" });
    defer allocator.free(exe_path);

    var child = process.Child.init(&[_][]const u8{ exe_path, path }, child_allocator);
    child.cwd = ".";
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();
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

test "big file" {
    std.debug.print("\n=== Running big file test ===\n", .{});
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    std.debug.print("Running doxa command with bigfile.doxa...\n", .{});
    const output = try runDoxaCommand(allocator, "./test/misc/bigfile.doxa");
    defer allocator.free(output);

    std.debug.print("Parsing output...\n", .{});
    const outputs = try parseOutput(output, allocator);

    std.debug.print("Verifying {} test cases...\n", .{outputs.items.len});

    // Test string values
    for (outputs.items, 0..) |item, i| {
        const expected = switch (i) {
            0 => "3",
            1 => "\"Overflow\"",
            2 => "-1",
            3 => "\"It's blue\"",
            4 => "\"It's something else\"",
            5 => "1",
            6 => "2",
            7 => "\"fizz\"",
            8 => "4",
            9 => "\"buzz\"",
            10 => "\"fizz\"",
            11 => "7",
            12 => "8",
            13 => "\"fizz\"",
            14 => "\"buzz\"",
            15 => "11",
            16 => "\"fizz\"",
            17 => "13",
            18 => "14",
            19 => "\"fizzbuzz\"",
            20 => "1",
            21 => "2",
            22 => "\"fizzer\"",
            23 => "4",
            24 => "\"buzzer\"",
            25 => "\"fizzer\"",
            26 => "7",
            27 => "8",
            28 => "\"fizzer\"",
            29 => "\"buzzer\"",
            30 => "11",
            31 => "\"fizzer\"",
            32 => "13",
            33 => "14",
            34 => "\"fizzbuzzer\"",
            35 => "1",
            36 => "2",
            37 => "3",
            38 => "4",
            39 => "5",
            40 => "\"return\"",
            41 => "26",
            42 => "4",
            43 => "\"M\"",
            44 => "1000",
            45 => "true",
            46 => "false",
            47 => "both",
            48 => "neither",
            49 => "false",
            50 => "false",
            51 => "[1, 2, 3, 4, 5]",
            52 => "4",
            53 => "6",
            54 => "42",
            55 => "true",
            56 => "false",
            57 => "\"true\"",
            58 => "3",
            59 => "6",
            60 => "10",
            61 => "20",
            62 => "30",
            63 => "1",
            64 => "3",
            65 => "\"can't do that\"",
            66 => "1",
            67 => "true",
            68 => "false",
            69 => "true",
            70 => "true",
            71 => "false",
            72 => "false",
            73 => "true",
            74 => "true",
            75 => "false",
            76 => "false",
            77 => "true",
            78 => "\"int\"",
            79 => "\"u8\"",
            80 => "\"float\"",
            81 => "\"string\"",
            82 => "\"tetra\"",
            83 => "\"array\"",
            84 => "\"struct\"",
            85 => "\"Employee\"",
            86 => "\"enum\"",
            87 => "\"enum_variant\"",
            88 => "\"tuple\"",
            89 => "\"map\"",
            else => unreachable,
        };
        if (!std.mem.eql(u8, item.value, expected)) {
            std.debug.print("Test case {} failed at line {s}:\n", .{ i, item.line });
            std.debug.print("  Expected: {s}\n", .{expected});
            std.debug.print("  Got:      {s}\n", .{item.value});
            try testing.expectEqualStrings(item.value, expected);
        }
    }

    std.debug.print("=== Big file test completed successfully ===\n\n", .{});
}

const Output = struct {
    name: []const u8,
    type: []const u8,
    value: []const u8,
    line: []const u8,
    col: []const u8,
};

fn parseOutput(output: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Output) {
    var outputs = std.ArrayList(Output).init(allocator);
    var toParse = output;
    while (toParse.len > 0) {
        // First get everything after the ]
        const after_bracket = grabBetween(toParse, "]", "\n");
        if (after_bracket.len == 0) break;

        // Get line and column numbers
        const line_col = grabBetween(toParse, ":", "]");
        var parts = std.mem.splitSequence(u8, line_col, ":");
        const line = parts.next() orelse "";
        const col = parts.next() orelse "";

        // Now parse the parts
        const name = std.mem.trim(u8, grabBetween(after_bracket, " ", "="), &std.ascii.whitespace);
        const value = std.mem.trim(u8, grabBetween(after_bracket, "=", "\n"), &std.ascii.whitespace);
        const typ = ""; // We'll need to determine the type from the value

        outputs.append(Output{
            .name = name,
            .type = typ,
            .value = value,
            .line = line,
            .col = col,
        }) catch unreachable;

        // Find the next line by looking for the next [
        const next_line = std.mem.indexOf(u8, toParse, "\n[") orelse break;
        toParse = toParse[next_line + 1 ..];
    }
    return outputs;
}

fn grabBetween(output: []const u8, start: []const u8, end: []const u8) []const u8 {
    const start_index = std.mem.indexOf(u8, output, start) orelse return "";
    const end_index = std.mem.indexOf(u8, output[start_index + start.len ..], end) orelse {
        // If no end marker found, return everything after start to end of string
        return output[start_index + start.len ..];
    };
    return output[start_index + start.len .. start_index + start.len + end_index];
}
