const std = @import("std");
const Lexer = @import("./lexer/lexer.zig").Lexer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Transpiler = @import("./transpiler/transpiler.zig").Transpiler;
const Reporting = @import("./utils/reporting.zig");
const Reporter = Reporting.Reporter;
const MemoryManager = @import("./utils/memory.zig").MemoryManager;
const Token = @import("./lexer/token.zig").Token;
const TokenLiteral = @import("./lexer/token.zig").TokenLiteral;
const Interpreter = @import("./interpreter/interpreter.zig").Interpreter;
const Environment = @import("./interpreter/interpreter.zig").Environment;

///==========================================================================
/// Constants
///==========================================================================
const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_FILE_SIZE = 1024 * 1024; // 1MB should be plenty for our interpreter
const DOXA_EXTENSION = ".doxa";
const DEFAULT_OUTPUT_FILE = "output.o";

///==========================================================================
/// Variables
///==========================================================================
pub var hadError: bool = false;
var compile: bool = false;
var is_safe_repl: bool = false;
var output_file: ?[]const u8 = null;
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
pub fn run(memory: *MemoryManager, source: []const u8, file_path: []const u8) !?TokenLiteral {
    if (memory.debug_enabled) {
        std.debug.print("\n=== Starting run ===\n", .{});
    }

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

    var parser_instance = Parser.init(
        memory.getAllocator(),
        token_list.items,
        file_path,
        memory.debug_enabled,
    );
    defer parser_instance.deinit();

    // Create the global environment
    var global_env = try memory.getAllocator().create(Environment);
    global_env.* = Environment.init(memory.getAllocator(), null, memory.debug_enabled, memory);
    defer {
        global_env.deinit();
        memory.getAllocator().destroy(global_env);
    }

    var interpreter = Interpreter.init(
        memory.getAllocator(),
        global_env,
        &parser_instance,
        memory.debug_enabled,
        memory,
    );
    defer interpreter.deinit();

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

    // var transpiler = Transpiler.init(memory.getAllocator(), &parser_instance, memory.debug_enabled);
    // defer transpiler.deinit();

    // const transpiled_code = try transpiler.transpile(statements);
    // std.debug.print("Transpiled code:\n {s}\n", .{transpiled_code});

    // try writeTo(transpiled_code, "output.zig");

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

fn writeTo(text: []const u8, path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{ .read = true });
    defer file.close();
    try file.writeAll(text);
}

fn runFile(memory: *MemoryManager, path: []const u8) !void {
    if (memory.debug_enabled) {
        std.debug.print("Debug enabled in memory manager\n", .{});
    }

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(memory.getAllocator(), MAX_FILE_SIZE);
    defer memory.getAllocator().free(source);

    _ = try run(memory, source, path);
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
    var memory = try MemoryManager.init(gpa.allocator(), false);
    defer memory.deinit();

    const args = try std.process.argsAlloc(memory.getAllocator());
    defer std.process.argsFree(memory.getAllocator(), args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (stringEquals(arg, "--debug")) {
            memory.debug_enabled = true;
        } else if (stringEquals(arg, "--compile")) {
            compile = true;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            output_file = args[i + 1];
            i += 1;
        } else {
            if (script_path != null) {
                std.debug.print("Usage: doxa [--debug] [--compile] [--output/-o file] [script]\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            script_path = arg;
        }
    }
    if (script_path) |path| {
        if (!stringEndsWith(path, DOXA_EXTENSION)) {
            std.debug.print("Error: {s} is not a doxa file\n", .{path});
            std.process.exit(EXIT_CODE_USAGE);
        }
        try runFile(&memory, path);
    } else {
        std.debug.print("Usage: doxa [--debug] [--compile] [--output/-o file] [script]\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    }
}

// Helper functions to avoid comptime issues with string comparisons
fn stringEquals(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, 0..) |char, i| {
        if (char != b[i]) return false;
    }
    return true;
}

fn stringEndsWith(str: []const u8, suffix: []const u8) bool {
    if (str.len < suffix.len) return false;
    const start = str.len - suffix.len;
    return stringEquals(str[start..], suffix);
}
