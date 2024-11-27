const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Reporting = @import("reporting.zig");

const Token = @import("lexer.zig").Token;
const instructions = @import("instructions.zig");
const VM = @import("vm.zig").VM;
const Frame = @import("vm.zig").Frame;
const Interpreter = @import("interpreter.zig").Interpreter;

///==========================================================================
/// Constants
///==========================================================================
const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_REPL_LINE_LENGTH = 1024;
const MAX_FILE_SIZE = 1024 * 1024; // 1MB should be plenty for our interpreter
const DOXA_EXTENSION = ".doxa";

///==========================================================================
/// Variables
///==========================================================================
var hadError: bool = false;
var debugLexer: bool = false;
var debugParser: bool = false;
var compile: bool = false;
///==========================================================================
/// Types & Errors
///==========================================================================
const SourceFile = struct {
    allocator: std.mem.Allocator,
    contents: []u8,

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !SourceFile {
        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const file_size = try file.getEndPos();
        const buffer = try allocator.alloc(u8, file_size);
        const bytes_read = try file.readAll(buffer);

        return SourceFile{
            .allocator = allocator,
            .contents = buffer[0..bytes_read],
        };
    }

    pub fn deinit(self: *SourceFile) void {
        self.allocator.free(self.contents);
    }
};

///==========================================================================
/// Error Reporting
///==========================================================================
pub fn reportMinimalError(line: i32, message: []const u8) void {
    reportError(line, "", message);
}

pub fn reportError(line: i32, where: []const u8, message: []const u8) void {
    std.debug.print("[line {}] {s} Error: {s}\n", .{ line, where, message });
    hadError = true;
}

///==========================================================================
/// Run
///==========================================================================
fn run(allocator: std.mem.Allocator, interpreter: *Interpreter, source: []const u8) !void {
    var lexer = Lexer.init(allocator, source);
    defer lexer.deinit();

    try lexer.initKeywords();
    const token_list = try lexer.lexTokens();

    if (debugLexer) {
        for (token_list.items) |tok| {
            const type_str = @tagName(tok.type);
            std.debug.print("Token type: {s}\n", .{type_str});
        }
    }

    if (!hadError) {
        var parser_instance = try Parser.init(allocator, token_list.items, debugParser);
        defer parser_instance.deinit();

        const statements = try parser_instance.parse();
        defer {
            // Clean up AST nodes
            for (statements) |stmt| {
                switch (stmt) {
                    .Expression => |maybe_expr| {
                        if (maybe_expr) |expr| {
                            expr.deinit(allocator);
                            allocator.destroy(expr);
                        }
                    },
                    .VarDecl => |decl| {
                        if (decl.initializer) |init| {
                            init.deinit(allocator);
                            allocator.destroy(init);
                        }
                    },
                    .Block => |block_statements| {
                        for (block_statements) |*block_stmt| {
                            block_stmt.deinit(allocator);
                        }
                        allocator.free(block_statements);
                    },
                    .Function => |*f| {
                        for (f.params) |*param| {
                            param.deinit(allocator);
                        }
                        allocator.free(f.params);
                    },
                    .Return => |*r| {
                        if (r.value) |value| {
                            value.deinit(allocator);
                            allocator.destroy(value);
                        }
                    },
                }
            }
            allocator.free(statements);
        }

        if (compile) {
            //TODO: Compile to bytecode
        } else {
            // Execute statements directly in the global environment
            for (statements) |stmt| {
                try interpreter.executeStatement(&stmt);
            }
        }
    }
}

fn runRepl(allocator: std.mem.Allocator) !void {
    // Create interpreter once at REPL start
    var interpreter = try Interpreter.init(allocator, debugParser);
    defer interpreter.deinit();

    const stdin = std.io.getStdIn().reader();
    while (true) {
        std.debug.print("> ", .{});
        const line = try stdin.readUntilDelimiterAlloc(allocator, '\n', MAX_REPL_LINE_LENGTH);
        defer allocator.free(line);
        if (line.len == 0) continue;
        try run(allocator, &interpreter, line);
        if (hadError) {
            std.debug.print("Error: {s}\n", .{line});
        }
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    var interpreter = try Interpreter.init(allocator, debugParser);
    defer interpreter.deinit();

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(source);

    try run(allocator, &interpreter, source);
    if (hadError) {
        std.process.exit(65);
    }
}

///==========================================================================
/// Main
///==========================================================================
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--debug-lexer")) {
            debugLexer = true;
        } else if (std.mem.eql(u8, args[i], "--debug-parser")) {
            debugParser = true;
        } else if (std.mem.eql(u8, args[i], "--compile")) {
            compile = true;
        } else {
            // Assume it's a script path
            if (script_path != null) {
                std.debug.print("Usage: doxa [--debug-lexer] [script]\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            script_path = args[i];
        }
    }
    if (script_path) |path| {
        if (!std.mem.endsWith(u8, path, DOXA_EXTENSION)) {
            std.debug.print("Error: {s} is not a doxa file\n", .{path});
            std.process.exit(EXIT_CODE_USAGE);
        }
        try runFile(allocator, path);
    } else {
        try runRepl(allocator);
    }
}
