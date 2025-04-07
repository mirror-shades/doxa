const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Reporting = @import("reporting.zig").Reporting;
const MemoryManager = @import("memory.zig").MemoryManager;
const Token = @import("lexer.zig").Token;
const TokenLiteral = @import("lexer.zig").TokenLiteral;
const instructions = @import("instructions.zig");
const Interpreter = @import("interpreter.zig").Interpreter;

///==========================================================================
/// Constants
///==========================================================================
const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_FILE_SIZE = 1024 * 1024; // 1MB should be plenty for our interpreter
const DOXA_EXTENSION = ".doxa";

///==========================================================================
/// Variables
///==========================================================================
pub var hadError: bool = false;
var compile: bool = false;
var is_safe_repl: bool = false;
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
pub fn run(memory: *MemoryManager, interpreter: *Interpreter, source: []const u8, file_path: []const u8) !?TokenLiteral {
    if (memory.debug_enabled) {
        std.debug.print("\n=== Starting run ===\n", .{});
    }

    // Create a reporter instance
    var reporter = Reporting{
        .writer = std.io.getStdErr().writer(),
        .error_count = 0,
        .warning_count = 0,
        .had_error = false,
        .had_warning = false,
    };
    defer reporter.deinit();

    // Now use the unified source for lexing
    var lexer = Lexer.init(memory.getAllocator(), source, file_path);
    defer lexer.deinit();

    try lexer.initKeywords();
    const token_list = try lexer.lexTokens();

    if (memory.debug_enabled) {
        for (token_list.items) |token| {
            std.debug.print("Token: {s} ({s})\n", .{ @tagName(token.type), token.lexeme });
        }
    }

    // finish logical operators
    // async/await
    // generics

    // the file has been lexed, now we need to parse it
    // first pass goes over the file and creates forward declaration for all function
    // second pass processes the imports to resolve any modules that are imported
    // third pass builds the AST for the file and anything exposed by the module

    if (!hadError) {
        var parser_instance = Parser.init(
            memory.getAllocator(),
            token_list.items,
            file_path,
            memory.debug_enabled,
        );
        defer parser_instance.deinit();

        if (memory.debug_enabled) {
            std.debug.print("\n=== Starting parse ===\n", .{});
        }
        const statements = try parser_instance.execute();
        if (memory.debug_enabled) {
            std.debug.print("hadError parsing: {}\n", .{hadError});
            std.debug.print("\n=== Parse complete, statement count: {} ===\n", .{statements.len});
            for (statements, 0..) |stmt, i| {
                std.debug.print("Statement {}: {s}\n", .{ i, @tagName(stmt) });
            }
            if (parser_instance.has_entry_point) {
                std.debug.print("Entry point: {s}\n", .{parser_instance.entry_point_name.?});
            }
        }

        if (compile) {
            //TODO: Compile to bytecode
            return null;
        } else {
            if (memory.debug_enabled) {
                std.debug.print("\n=== Starting interpretation ===\n", .{});
            }

            // Transfer entry point information from parser to interpreter
            if (parser_instance.has_entry_point) {
                interpreter.entry_point_name = parser_instance.entry_point_name;
            }

            // Set the parser reference for imported module handling
            interpreter.parser = &parser_instance;

            try interpreter.interpret(statements);
            return interpreter.last_result;
        }
    }
    return null;
}

fn runFile(memory: *MemoryManager, path: []const u8) !void {
    if (memory.debug_enabled) {
        std.debug.print("Debug enabled in memory manager\n", .{});
    }

    var interpreter = try Interpreter.init(memory.getAllocator(), memory.debug_enabled);
    defer interpreter.deinit();

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(memory.getAllocator(), MAX_FILE_SIZE);
    defer memory.getAllocator().free(source);

    _ = try run(memory, &interpreter, source, path);
    if (interpreter.had_error) {
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
    var memory = MemoryManager.init(gpa.allocator(), false);
    defer memory.deinit();
    const args = try std.process.argsAlloc(memory.getAllocator());
    defer std.process.argsFree(memory.getAllocator(), args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--safe-repl")) {
            is_safe_repl = true;
        } else if (std.mem.eql(u8, args[i], "--debug")) {
            memory.debug_enabled = true;
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
        try runFile(&memory, path);
    } else {
        std.debug.print("Usage: doxa [--debug-lexer] [script]\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    }
}
