const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;
const repl = @import("repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    // Get command-line arguments
    var arg_iterator = try std.process.argsWithAllocator(allocator);
    defer arg_iterator.deinit();

    // Skip the program name
    _ = arg_iterator.next();

    // Check if we have any arguments
    const filename = arg_iterator.next();
    if (filename == null) {
        // Start REPL
        try repl.startREPL(&allocator);
        return;
    }

    // Check file extension
    if (!std.mem.endsWith(u8, filename.?, ".doxa")) {
        std.debug.print("Error: File must have .doxa extension\n", .{});
        std.process.exit(1);
        return;
    }

    // Open and read the file
    var file = try std.fs.cwd().openFile(filename.?, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, @intCast(file_size));
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Convert buffer to string slice
    const source_code = buffer;

    // Initialize the lexer for the entire source code
    var lexer = Lexer.init(source_code);

    // Initialize the parser
    var parser = Parser.init(&lexer, &allocator);

    // Initialize the interpreter once
    var interpreter = try Interpreter.init(&allocator);

    // Parse and evaluate until we reach the end of the file
    while (true) {
        const ast = parser.parseStatement() catch |err| {
            if (err == error.EndOfFile) break;
            if (err == error.UnexpectedToken) {
                // Check if we're at the end of input with only whitespace/semicolons remaining
                const token = lexer.peek();
                if (token == .EOF) break; // Exit if we're at EOF
                if (token == .Semicolon) {
                    _ = lexer.nextToken(); // consume the semicolon
                    continue; // Skip this iteration and continue parsing
                }
                std.debug.print("Unexpected token encountered\n", .{});
                return err;
            }
            return err;
        };

        // Now that we have the interpreter, evaluate the AST
        const result = try interpreter.evaluate(ast);

        if (interpreter.shouldPrintResult(source_code)) {
            const stdout = std.io.getStdOut().writer();
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
        }
    }
}
