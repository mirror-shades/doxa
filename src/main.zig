const std = @import("std");
const Lexer = @import("./lexer/lexer.zig").Lexer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Reporting = @import("./utils/reporting.zig");
const Reporter = Reporting.Reporter;
const MemoryImport = @import("./utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Token = @import("./lexer/token.zig").Token;
const TokenLiteral = @import("./types/types.zig").TokenLiteral;
const InterpreterImport = @import("./interpreter/interpreter.zig");
const Interpreter = InterpreterImport.Interpreter;
const Environment = @import("./types/types.zig").Environment;
const astWriter = @import("./utils/ast_writer.zig");
const environment = @import("./interpreter/environment.zig");

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
fn runFile(memoryManager: *MemoryManager, path: []const u8) !void {
    if (memoryManager.debug_enabled) {
        std.debug.print("Debug enabled in memory manager\n", .{});
    }

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(memoryManager.getAllocator(), MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    var lexer = Lexer.init(memoryManager.getAllocator(), source, path);
    defer lexer.deinit();

    try lexer.initKeywords();
    const token_list = try lexer.lexTokens();

    if (memoryManager.debug_enabled) {
        for (token_list.items) |token| {
            std.debug.print("Token: {s} ({s})\n", .{ @tagName(token.type), token.lexeme });
        }
    }

    var parser_instance = Parser.init(
        memoryManager.getAllocator(),
        token_list.items,
        path,
        memoryManager.debug_enabled,
    );
    defer parser_instance.deinit();

    // Create the global environment
    var global_env = try memoryManager.getAllocator().create(Environment);
    global_env.* = environment.init(memoryManager.getAllocator(), null, memoryManager.debug_enabled, memoryManager);
    defer {
        global_env.deinit();
        memoryManager.getAllocator().destroy(global_env);
    }

    var interpreter = Interpreter.init(
        memoryManager.getAllocator(),
        global_env,
        &parser_instance,
        memoryManager.debug_enabled,
        memoryManager,
    );
    defer interpreter.deinit();

    const statements = try parser_instance.execute();
    if (memoryManager.debug_enabled) {
        std.debug.print("hadError parsing: {}\n", .{hadError});
        std.debug.print("\n=== Parse complete, statement count: {} ===\n", .{statements.len});
        for (statements, 0..) |stmt, i| {
            std.debug.print("Statement {}: {s}\n", .{ i, @tagName(stmt) });
        }
        if (parser_instance.has_entry_point) {
            std.debug.print("Entry point: {s}\n", .{parser_instance.entry_point_name.?});
        }
    }

    // Always write AST to file as an artifact
    const ast_path = try generateArtifactPath(path, ".ast");
    try astWriter.writeASTToFile(statements, ast_path);
    std.debug.print("AST artifact written to {s}\n", .{ast_path});

    // Add AST debug output if debug is enabled
    if (memoryManager.debug_enabled) {
        std.debug.print("\n=== AST Output ===\n", .{});
        for (statements, 0..) |stmt, i| {
            std.debug.print("\nStatement {}:\n", .{i});
            astWriter.printStatement(stmt, 1);
        }
        std.debug.print("\n=== End AST Output ===\n", .{});
    }

    if (memoryManager.debug_enabled) {
        std.debug.print("\n=== Starting interpretation ===\n", .{});
    }

    // Transfer entry point information from parser to interpreter
    if (parser_instance.has_entry_point) {
        interpreter.entry_point_name = parser_instance.entry_point_name;
    }

    // Set the parser reference for imported module handling
    interpreter.parser = &parser_instance;

    try interpreter.interpret(statements);
    _ = interpreter.last_result;
}

/// Generate a path for an artifact file based on the source file path
fn generateArtifactPath(source_path: []const u8, extension: []const u8) ![]u8 {
    // Find the last dot in the path
    var last_dot: ?usize = null;
    for (source_path, 0..) |c, i| {
        if (c == '.') last_dot = i;
    }

    if (last_dot) |dot| {
        // Allocate space for the new path
        const new_path = try std.heap.page_allocator.alloc(u8, dot + extension.len);
        @memcpy(new_path[0..dot], source_path[0..dot]);
        @memcpy(new_path[dot..], extension);
        return new_path;
    } else {
        // If no dot found, append extension
        const new_path = try std.heap.page_allocator.alloc(u8, source_path.len + extension.len);
        @memcpy(new_path[0..source_path.len], source_path);
        @memcpy(new_path[source_path.len..], extension);
        return new_path;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }
    var memoryManager = try MemoryManager.init(gpa.allocator(), false);
    defer memoryManager.deinit();

    const args = try std.process.argsAlloc(memoryManager.getAllocator());
    defer std.process.argsFree(memoryManager.getAllocator(), args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (stringEquals(arg, "--debug")) {
            memoryManager.debug_enabled = true;
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
        try runFile(&memoryManager, path);
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
