const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const Environment = @import("./interpreter.zig").Environment;
const main = @import("./main.zig");
const MemoryManager = @import("./memory.zig").MemoryManager;
const ast = @import("ast.zig");
const token = @import("token.zig");

const MAX_REPL_LINE_LENGTH = 1024;

pub fn runRepl(memory: *MemoryManager) !void {
    var interpreter = try Interpreter.init(memory.getAllocator(), memory.debug_enabled);
    defer interpreter.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buf: [1024]u8 = undefined;

    // Create a persistent environment for the REPL
    var repl_env = Environment.init(memory.getAllocator(), null, memory.debug_enabled);
    defer repl_env.deinit();

    // Set up the environment chain: repl_env -> globals
    repl_env.enclosing = interpreter.globals;
    interpreter.environment = &repl_env;

    if (memory.debug_enabled) {
        std.debug.print("REPL environment setup:\n", .{});
        std.debug.print("  repl_env: {*}\n", .{&repl_env});
        std.debug.print("  globals: {*}\n", .{interpreter.globals});
        std.debug.print("  interpreter.environment: {*}\n", .{interpreter.environment});
    }

    while (true) {
        try stdout.writeAll("> ");

        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
            if (trimmed.len == 0) continue;

            if (memory.debug_enabled) {
                std.debug.print("\nInput line: '{s}'\n", .{trimmed});
                std.debug.print("Environment chain before execution:\n", .{});
                var env: ?*Environment = interpreter.environment;
                var depth: usize = 0;
                while (env != null) : ({
                    env = env.?.enclosing;
                    depth += 1;
                }) {
                    std.debug.print("  Depth {}: {*}\n", .{ depth, env });
                    std.debug.print("    Values: {any}\n", .{env.?.values.count()});
                    var it = env.?.values.iterator();
                    while (it.next()) |entry| {
                        std.debug.print("      {s} = {any}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
                    }
                }
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
            const result = main.run(memory, &interpreter, "<repl>") catch |err| switch (err) {
                error.VariableNotFound => {
                    std.debug.print("Error: Variable not found\n", .{});
                    if (memory.debug_enabled) {
                        std.debug.print("Environment state after error:\n", .{});
                        std.debug.print("  Values in repl_env: {any}\n", .{repl_env.values.count()});
                        var it = repl_env.values.iterator();
                        while (it.next()) |entry| {
                            std.debug.print("    {s} = {any}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
                        }
                    }
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
