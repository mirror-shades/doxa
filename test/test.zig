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
            0 => "\"It's blue\"",
            1 => "\"It's something else\"",
            2 => "1",
            3 => "2",
            4 => "\"fizz\"",
            5 => "4",
            6 => "\"buzz\"",
            7 => "\"fizz\"",
            8 => "7",
            9 => "8",
            10 => "\"fizz\"",
            11 => "\"buzz\"",
            12 => "11",
            13 => "\"fizz\"",
            14 => "13",
            15 => "14",
            16 => "\"fizzbuzz\"",
            17 => "1",
            18 => "2",
            19 => "\"fizzer\"",
            20 => "4",
            21 => "\"buzzer\"",
            22 => "\"fizzer\"",
            23 => "7",
            24 => "8",
            25 => "\"fizzer\"",
            26 => "\"buzzer\"",
            27 => "11",
            28 => "\"fizzer\"",
            29 => "13",
            30 => "14",
            31 => "\"fizzbuzzer\"",
            32 => "1",
            33 => "2",
            34 => "3",
            35 => "4",
            36 => "5",
            37 => "\"return\"",
            38 => "26",
            39 => "4",
            40 => "\"M\"",
            41 => "1000",
            42 => "true",
            43 => "false",
            44 => "both",
            45 => "neither",
            46 => "false",
            47 => "false",
            48 => "[1, 2, 3, 4, 5]",
            49 => "4",
            50 => "6",
            51 => "42",
            52 => "true",
            53 => "false",
            54 => "\"true\"",
            55 => "3",
            56 => "6",
            57 => "10",
            58 => "20",
            59 => "30",
            60 => "1",
            61 => "3",
            62 => "\"can't do that\"",
            63 => "1",
            64 => "true",
            65 => "false",
            66 => "true",
            67 => "true",
            68 => "false",
            69 => "false",
            70 => "true",
            71 => "true",
            72 => "false",
            73 => "false",
            74 => "true",
            75 => "\"int\"",
            76 => "\"u8\"",
            77 => "\"float\"",
            78 => "\"string\"",
            79 => "\"tetra\"",
            80 => "\"array\"",
            81 => "\"struct\"",
            82 => "\"Employee\"",
            83 => "\"enum\"",
            84 => "\"enum_variant\"",
            85 => "\"tuple\"",
            86 => "\"map\"",
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
