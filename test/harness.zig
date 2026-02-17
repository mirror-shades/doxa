const std = @import("std");
const builtin = @import("builtin");
const process = std.process;

pub const CommandResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
};

pub const PeekRow = struct {
    type: []const u8,
    value: []const u8,
};

pub const Counts = struct {
    passed: usize,
    failed: usize,
    untested: usize,
};

pub fn isClean(result: Counts) bool {
    return result.failed == 0 and result.untested == 0;
}

pub fn printSection(name: []const u8) void {
    std.debug.print("\n== {s} ==\n", .{name});
}

pub fn printCase(name: []const u8, result: Counts) void {
    if (isClean(result)) {
        std.debug.print("- {s}: ok ({d})\n", .{ name, result.passed });
        return;
    }
    if (result.failed == 0 and result.passed == 0 and result.untested > 0) {
        std.debug.print("- {s}: SKIP ({d})\n", .{ name, result.untested });
        return;
    }
    if (result.failed == 0 and result.untested > 0) {
        std.debug.print("- {s}: WARN ({d} ok, {d} untested)\n", .{ name, result.passed, result.untested });
        return;
    }
    std.debug.print(
        "- {s}: FAIL ({d} ok, {d} fail, {d} untested)\n",
        .{ name, result.passed, result.failed, result.untested },
    );
}

pub fn printSuiteSummary(name: []const u8, result: Counts) void {
    if (isClean(result)) {
        std.debug.print("{s}: {d} ok\n", .{ name, result.passed });
        return;
    }
    std.debug.print(
        "{s}: {d} ok, {d} fail, {d} untested\n",
        .{ name, result.passed, result.failed, result.untested },
    );
}

pub fn repoRootFromEnv(allocator: std.mem.Allocator) !?[]const u8 {
    return process.getEnvVarOwned(allocator, "DOXA_REPO_ROOT") catch null;
}

pub fn doxaExePath(allocator: std.mem.Allocator) ![]const u8 {
    if (process.getEnvVarOwned(allocator, "DOXA_BIN") catch null) |custom| {
        return custom;
    }
    const exe_name = if (builtin.os.tag == .windows) "doxa.exe" else "doxa";
    return try std.fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
}

pub fn runCommandCapture(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    cwd: ?[]const u8,
    input: ?[]const u8,
) !CommandResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    var child = process.Child.init(argv, child_allocator);
    if (cwd) |dir| child.cwd = dir;
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

    var stdout = std.ArrayList(u8).empty;
    defer stdout.deinit(child_allocator);
    var stderr = std.ArrayList(u8).empty;
    defer stderr.deinit(child_allocator);

    // Poll both pipes concurrently to avoid deadlock when one pipe fills up.
    try child.collectOutput(child_allocator, &stdout, &stderr, 8 * 1024 * 1024);

    const term = try child.wait();
    const exit_code: u8 = switch (term) {
        .Exited => |code| @intCast(code),
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

    return .{
        .stdout = try allocator.dupe(u8, stdout.items),
        .stderr = try allocator.dupe(u8, stderr.items),
        .exit_code = exit_code,
    };
}

pub fn parsePeekOutput(output: []const u8, allocator: std.mem.Allocator) !std.array_list.Managed(PeekRow) {
    var outputs = std.array_list.Managed(PeekRow).init(allocator);

    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        const close_bracket = std.mem.indexOfScalar(u8, line, ']') orelse break;
        const line_with_var = line[close_bracket + 1 ..];
        const colon = std.mem.indexOfScalar(u8, line_with_var, ':') orelse break;
        if (colon + 3 > line_with_var.len) break;
        const line_without_var = line_with_var[colon + 3 ..];

        try outputs.append(.{
            .type = grabType(line_without_var),
            .value = grabValue(line_without_var),
        });
    }
    return outputs;
}

pub fn parsePrintOutput(output: []const u8, allocator: std.mem.Allocator) !std.array_list.Managed([]const u8) {
    var outputs = std.array_list.Managed([]const u8).init(allocator);

    var lines = std.mem.splitScalar(u8, output, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try outputs.append(line);
    }
    return outputs;
}

pub fn grabType(output: []const u8) []const u8 {
    const idx = std.mem.indexOf(u8, output, " is ") orelse return output;
    return output[0..idx];
}

pub fn grabValue(output: []const u8) []const u8 {
    const idx = std.mem.indexOf(u8, output, " is ") orelse return "";
    return output[idx + 4 ..];
}

pub fn validatePrintResults(output: []const u8, expected_results: anytype, allocator: std.mem.Allocator) !Counts {
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
            std.debug.print(
                "Print test case {d} failed:\n  Expected: {s}\n  Found:    {s}\n",
                .{ i + 1, expected_results[i].value, outputs.items[i] },
            );
            failed += 1;
        } else {
            passed += 1;
        }
    }

    if (expected_count > actual_count) {
        untested = expected_count - actual_count;
        std.debug.print("WARN: {d} test case(s) were not executed (program may have crashed early)\n", .{untested});
    }

    return .{ .passed = passed, .failed = failed, .untested = untested };
}

pub fn validatePeekResults(output: []const u8, expected_results: anytype, allocator: std.mem.Allocator) !Counts {
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
            std.debug.print(
                "Peek test case {d} failed:\n  Expected: {s} = {s}\n  Found:    {s} = {s}\n",
                .{ i + 1, expected_results[i].type, expected_results[i].value, outputs.items[i].type, outputs.items[i].value },
            );
            failed += 1;
        } else {
            passed += 1;
        }
    }

    if (expected_count > actual_count) {
        untested = expected_count - actual_count;
        std.debug.print("WARN: {d} test case(s) were not executed (program may have crashed early)\n", .{untested});
    }

    return .{ .passed = passed, .failed = failed, .untested = untested };
}

pub fn getBinaryPath(alloc: std.mem.Allocator, base: []const u8) ![]const u8 {
    if (builtin.os.tag == .windows) {
        if (std.mem.endsWith(u8, base, ".exe")) return try alloc.dupe(u8, base);
        return try std.fmt.allocPrint(alloc, "{s}.exe", .{base});
    }
    return try alloc.dupe(u8, base);
}
