const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;

pub fn startREPL(allocator: *std.mem.Allocator) !void {
    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();
    var interpreter = try Interpreter.init(allocator);
    defer interpreter.deinit();

    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterAlloc(allocator.*, '\n', 1024);
        defer allocator.free(line);

        // Skip empty lines
        if (line.len == 0 or std.mem.eql(u8, std.mem.trim(u8, line, " \t\r\n"), "")) {
            continue;
        }

        var lexer = Lexer.init(line);
        var parser = Parser.init(&lexer, allocator);

        const ast = parser.parseStatement() catch |err| {
            if (err == error.EndOfFile) continue;
            std.debug.print("Parse error: {!}\n", .{err});
            continue;
        };

        const result = interpreter.evaluate(ast) catch |err| {
            std.debug.print("Runtime error: {!}\n", .{err});
            try interpreter.debugPrintState();
            continue;
        };

        if (interpreter.shouldPrintResult(line)) {
            try stdout.print("{d}\n", .{result});
        }
    }
}