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

    // Get the path to the doxa executable
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", "doxa" });
    defer allocator.free(exe_path);

    var child = process.Child.init(&[_][]const u8{ exe_path, "old", path }, child_allocator);
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
            0 => "81",
            1 => "\"Overflow\"",
            2 => "-1",
            3 => "\"It's blue\"",
            4 => "\"It's something else\"",
            5 => "\"buzz\"",
            6 => "101",
            7 => "\"fizz\"",
            8 => "103",
            9 => "104",
            10 => "\"fizzbuzz\"",
            11 => "106",
            12 => "107",
            13 => "\"fizz\"",
            14 => "109",
            15 => "\"buzz\"",
            16 => "\"fizz\"",
            17 => "112",
            18 => "113",
            19 => "\"fizz\"",
            20 => "\"buzz\"",
            21 => "\"buzzer\"",
            22 => "1001",
            23 => "\"fizzer\"",
            24 => "1003",
            25 => "1004",
            26 => "\"fizzbuzzer\"",
            27 => "1006",
            28 => "1007",
            29 => "\"fizzer\"",
            30 => "1009",
            31 => "\"buzzer\"",
            32 => "\"fizzer\"",
            33 => "1012",
            34 => "1013",
            35 => "\"fizzer\"",
            36 => "\"buzzer\"",
            37 => "660",
            38 => "726",
            39 => "792",
            40 => "858",
            41 => "924",
            42 => "\"return\"",
            43 => "26",
            44 => "4",
            45 => "[0x4D, 0x69, 0x6B, 0x65]",
            46 => "\"M\"",
            47 => "0x4D",
            48 => "1000",
            49 => "true",
            50 => "false",
            51 => "both",
            52 => "neither",
            53 => "false",
            54 => "false",
            55 => "[111111, 222222, 333333, 444444, 555555]",
            56 => "444444",
            57 => "666666",
            58 => "420000",
            59 => "true",
            60 => "false",
            61 => "\"true\"",
            62 => "779",
            63 => "782",
            64 => "1234",
            65 => "2345",
            66 => "true",
            67 => "1.5",
            68 => "3.5",
            69 => "true",
            70 => "false",
            71 => "true",
            72 => "true",
            73 => "false",
            74 => "false",
            75 => "true",
            76 => "true",
            77 => "false",
            78 => "false",
            79 => "true",
            80 => "\"int\"",
            81 => "\"byte\"",
            82 => "\"float\"",
            83 => "\"string\"",
            84 => "\"tetra\"",
            85 => "\"array\"",
            86 => "\"struct\"",
            87 => "\"Employee\"",
            88 => "\"enum\"",
            89 => "\"enum_variant\"",
            90 => "\"tuple\"",
            91 => "\"map\"",
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
