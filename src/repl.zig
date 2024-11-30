const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const main = @import("./main.zig");
const MemoryManager = @import("./memory.zig").MemoryManager;
const MAX_REPL_LINE_LENGTH = 1024;

pub fn runRepl(memory: *MemoryManager) !void {
    var interpreter = try Interpreter.init(memory);
    defer interpreter.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buf: [1024]u8 = undefined;

    while (true) {
        try stdout.writeAll("> ");

        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
            if (trimmed.len == 0) continue;

            if (memory.debug_enabled) {
                std.debug.print("Input line: '{s}'\n", .{trimmed});
            }

            // Add semicolon if missing
            const final_line = if (!std.mem.endsWith(u8, trimmed, ";"))
                try std.fmt.allocPrint(memory.getAllocator(), "{s};", .{trimmed})
            else
                try memory.getAllocator().dupe(u8, trimmed);

            defer memory.getAllocator().free(final_line);

            if (memory.debug_enabled) {
                std.debug.print("Processed line: '{s}'\n", .{final_line});
            }

            // Execute and get result
            const result = main.run(memory, &interpreter, final_line, true, "<repl>") catch |err| switch (err) {
                error.VariableNotFound => {
                    std.debug.print("Error: Variable not found\n", .{});
                    continue;
                },
                error.ExpectedSemicolon => {
                    std.debug.print("Error: Missing semicolon\n", .{});
                    continue;
                },
                else => {
                    std.debug.print("Error: {s}\n", .{@errorName(err)});
                    continue;
                },
            };

            // Print the result if it's not null
            if (result) |value| {
                switch (value) {
                    .int => |i| std.debug.print("{d}\n", .{i}),
                    .float => |f| std.debug.print("{d}\n", .{f}),
                    .boolean => |b| std.debug.print("{}\n", .{b}),
                    .string => |s| std.debug.print("{s}\n", .{s}),
                    .nothing => {},
                    else => std.debug.print("{any}\n", .{value}),
                }
            }
        } else {
            break;
        }
    }
}
