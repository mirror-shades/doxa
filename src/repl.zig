const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;

pub fn startREPL(allocator: *std.mem.Allocator) !void {
    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();
    var interpreter = Interpreter.init(allocator); // Initialize once

    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterAlloc(allocator.*, '\n', 1024);
        defer allocator.free(line);

        var lexer = Lexer.init(line);
        var parser = Parser.init(&lexer, allocator);
        const ast = parser.parseStatement() catch |err| {
            std.debug.print("Parse error: {!}\n", .{err});
            continue;
        };

        const result = interpreter.evaluate(ast) catch |err| {
            std.debug.print("Runtime error: {!}\n", .{err});
            continue;
        };

        if (interpreter.shouldPrintResult(line)) {
            try stdout.print("{any}\n", .{result});
        }
    }
}