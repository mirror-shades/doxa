const std = @import("std");
const testing = std.testing;
const process = std.process;
const fs = std.fs;

const result = struct {
    type: []const u8,
    value: []const u8,
};

const expected_results = [_]result{
    .{ .type = "int", .value = "81" },
    .{ .type = "string", .value = "\"Overflow\"" },
    .{ .type = "int", .value = "-1" },
    .{ .type = "string", .value = "\"It's blue\"" },
    .{ .type = "string", .value = "\"It's something else\"" },
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
    .{ .type = "string", .value = "\"return\"" },
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
    .{ .type = "int[]", .value = "[111111, 222222, 333333, 444444, 555555]" },
    .{ .type = "int", .value = "444444" },
    .{ .type = "int", .value = "666666" },
    .{ .type = "int", .value = "420000" },
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
    .{ .type = "tetra", .value = "true" },
    .{ .type = "tetra", .value = "false" },
    .{ .type = "string", .value = "\"true\"" },
    .{ .type = "int", .value = "779" },
    .{ .type = "int", .value = "782" },
    .{ .type = "string", .value = "\"integer\"" },
    .{ .type = "string", .value = "\"integer\"" },
    .{ .type = "string", .value = "\"float\"" },
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

    var child = process.Child.init(&[_][]const u8{ exe_path, "run", path }, child_allocator);
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
    defer outputs.deinit();

    var i: usize = 0;
    while (i < outputs.items.len) : (i += 1) {
        if (std.mem.eql(u8, outputs.items[i].type, expected_results[i].type) and std.mem.eql(u8, outputs.items[i].value, expected_results[i].value)) {
            continue;
        }
        std.debug.print("Test {d} failed\n", .{i});
        std.debug.print("Expected: {s} {s} found {s} {s}\n", .{ expected_results[i].type, expected_results[i].value, outputs.items[i].type, outputs.items[i].value });
        break;
    }
    std.debug.print("verified {d} test cases\n", .{i});
    std.debug.print("\n=== Big file test complete ===\n", .{});
}

fn parseOutput(output: []const u8, allocator: std.mem.Allocator) !std.ArrayList(result) {
    var outputs = std.ArrayList(result).init(allocator);

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

fn grabType(output: []const u8) []const u8 {
    var foundType: []const u8 = "";
    for (output, 0..) |c, i| {
        if (c == ' ') {
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
