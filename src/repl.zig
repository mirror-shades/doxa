const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const main = @import("./main.zig");
const MAX_REPL_LINE_LENGTH = 1024;

pub fn runRepl(allocator: std.mem.Allocator) !void {
    // Create interpreter once at REPL start
    var interpreter = try Interpreter.init(allocator, main.debugParser);
    defer interpreter.deinit();

    const stdin = std.io.getStdIn().reader();
    while (true) {
        std.debug.print("> ", .{});
        const line = try stdin.readUntilDelimiterAlloc(allocator, '\n', MAX_REPL_LINE_LENGTH);
        defer allocator.free(line);
        if (line.len == 0) continue;
        try main.run(allocator, &interpreter, line);
        if (main.hadError) {
            std.debug.print("Error: {s}\n", .{line});
        }
    }
}
