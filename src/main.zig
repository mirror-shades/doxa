const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Reporting = @import("reporting.zig");
const MemoryManager = @import("memory.zig").MemoryManager;

const Token = @import("lexer.zig").Token;
const TokenLiteral = @import("lexer.zig").TokenLiteral;
const instructions = @import("instructions.zig");
const VM = @import("vm.zig").VM;
const Frame = @import("vm.zig").Frame;
const Interpreter = @import("interpreter.zig").Interpreter;
const repl = @import("repl.zig");
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
var debugLexer: bool = false;
pub var debugParser: bool = false;
pub var debugInterpreter: bool = false;
var compile: bool = false;
var is_strict_repl: bool = false;
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
pub fn run(memory: *MemoryManager, interpreter: *Interpreter, source: []const u8, is_repl: bool) !?TokenLiteral {
    var lexer = Lexer.init(memory.getAllocator(), source);
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
        var parser_instance = try Parser.init(memory, token_list.items, debugParser, is_repl, is_strict_repl);
        defer parser_instance.deinit();

        const statements = try parser_instance.parse();

        if (compile) {
            //TODO: Compile to bytecode
            return null;
        } else {
            // Return the result of the last statement
            var last_result: ?TokenLiteral = null;
            for (statements) |stmt| {
                last_result = try interpreter.executeStatement(&stmt, debugInterpreter);
            }
            return last_result;
        }
    }
    return null;
}

fn runFile(memory: *MemoryManager, path: []const u8) !void {
    var interpreter = try Interpreter.init(memory);
    defer interpreter.deinit();

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(memory.getAllocator(), MAX_FILE_SIZE);
    defer memory.getAllocator().free(source);

    _ = try run(memory, &interpreter, source, false);
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
    var memory = MemoryManager.init(gpa.allocator(), false);
    defer memory.deinit();
    const args = try std.process.argsAlloc(memory.getAllocator());
    defer std.process.argsFree(memory.getAllocator(), args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--strict-repl")) {
            is_strict_repl = true;
        } else if (std.mem.eql(u8, args[i], "--debug-lexer")) {
            debugLexer = true;
        } else if (std.mem.eql(u8, args[i], "--debug-parser")) {
            debugParser = true;
        } else if (std.mem.eql(u8, args[i], "--debug-interpreter")) {
            debugInterpreter = true;
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
        try repl.runRepl(&memory);
    }
}
