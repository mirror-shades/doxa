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
        var parser = Parser.init(allocator, &lexer);

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
            switch (result) {
                .Int => |n| try stdout.print("{d}\n", .{n}),
                .Float => |n| try stdout.print("{d:.1}\n", .{n}),
                .String => |s| try stdout.print("{s}\n", .{s}),
                .Nothing => try stdout.print("nothing\n", .{}),
                .True => try stdout.print("true\n", .{}),
                .False => try stdout.print("false\n", .{}),
                .And => try stdout.print("and\n", .{}),
                .Or => try stdout.print("or\n", .{}),
                .Array => |arr| {
                    try stdout.print("[", .{});
                    var i: i64 = 0;
                    for (arr) |element| {
                        if (i != 0) try stdout.print(", ", .{});
                        try stdout.print("{any}", .{element});
                        i += 1;
                    }
                    try stdout.print("]\n", .{});
                },
            }
            try stdout.print("\n", .{});
        }
    }
}
