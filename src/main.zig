const std = @import("std");
const Lexer = @import("./lexer/lexer.zig").Lexer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Reporting = @import("./utils/reporting.zig");
const Reporter = Reporting.Reporter;
const MemoryImport = @import("./utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Token = @import("./lexer/token.zig").Token;
const TypesImport = @import("./types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const Environment = TypesImport.Environment;
const env = @import("./interpreter/environment.zig");
const InterpreterImport = @import("./interpreter/interpreter.zig");
const Interpreter = InterpreterImport.Interpreter;
const ASTWriter = @import("./utils/ast_writer.zig");
const ASTReader = @import("./codegen/ast_reader.zig");
const AST = @import("./ast/ast.zig");

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
var compile_file: bool = false;
var is_safe_repl: bool = false;
var output_file: ?[]const u8 = null;

//==========================================================================
/// CLI Options
//==========================================================================

const CLI = struct {
    debug: bool,
    compile: bool,
    output: ?[]const u8,
    script_path: ?[]const u8,
};

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
/// Compile
///==========================================================================
fn compile(_: std.mem.Allocator, _: []const u8) !void {
    // For now, just print that compilation is not supported
    std.debug.print("Compilation to native code is not supported yet.\n", .{});
    return error.CompilationNotSupported;
}

///==========================================================================
/// Interpret
///==========================================================================
fn interpret(memoryManager: *MemoryManager, parser: *Parser, statements: []AST.Stmt) !void {
    if (memoryManager.debug_enabled) {
        std.debug.print("Debug enabled in memory manager\n", .{});
    }

    var global_env = try memoryManager.getAllocator().create(Environment);
    global_env.* = env.init(memoryManager.getAllocator(), null, memoryManager.debug_enabled, memoryManager);
    defer {
        global_env.deinit();
        memoryManager.getAllocator().destroy(global_env);
    }

    var interpreter = Interpreter.init(
        memoryManager.getAllocator(),
        global_env,
        parser,
        memoryManager.debug_enabled,
        memoryManager,
    );
    defer interpreter.deinit();

    try interpreter.interpret(statements);
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

fn processFile(memoryManager: *MemoryManager, path: []const u8, should_compile: bool) !void {
    // Read source file
    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    // Lexical analysis
    var lexer = Lexer.init(memoryManager.getAllocator(), source, path);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();

    // Parsing phase
    var parser = Parser.init(memoryManager.getAllocator(), tokens.items, path, memoryManager.debug_enabled);
    defer parser.deinit();

    const statements = try parser.execute();

    // Write AST
    const ast_path = try generateArtifactPath(path, ".ast");

    // Either compile or interpret
    if (should_compile) {
        try ASTWriter.writeASTToFile(statements, ast_path);
        //try compile(memoryManager.getAllocator(), ast_path); l
    } else {
        try interpret(memoryManager, &parser, statements);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }

    // Parse args first
    const cli_options = try parseArgs(gpa.allocator());
    // Ensure script_path is freed
    defer if (cli_options.script_path) |path| {
        gpa.allocator().free(path);
    };

    // Initialize memory manager with debug setting
    var memoryManager = try MemoryManager.init(gpa.allocator(), cli_options.debug);
    defer memoryManager.deinit();

    if (cli_options.output) |out_file| {
        output_file = out_file;
    }

    // Process the script
    const path = cli_options.script_path.?; // Safe to unwrap since we validated it
    if (memoryManager.debug_enabled) {
        std.debug.print("Debug: Processing script: '{s}'\n", .{path});
    }

    if (!stringEndsWith(path, DOXA_EXTENSION)) {
        std.debug.print("Error: '{s}' is not a doxa file\n", .{path});
        std.process.exit(EXIT_CODE_USAGE);
    }

    // Convert relative path to absolute if needed
    var path_buffer: [std.fs.max_path_bytes]u8 = undefined; // Changed from MAX_PATH_BYTES to max_path_bytes
    const abs_path = try std.fs.cwd().realpath(path, &path_buffer);

    if (memoryManager.debug_enabled) {
        std.debug.print("Debug: Absolute path: '{s}'\n", .{abs_path});
    }

    try processFile(&memoryManager, abs_path, cli_options.compile);
}

fn parseArgs(allocator: std.mem.Allocator) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    std.debug.print("Debug: Command line arguments:\n", .{});
    for (args, 0..) |arg, i| {
        std.debug.print("  {d}: '{s}' (len: {d})\n", .{ i, arg, arg.len });
    }

    if (args.len < 2) {
        std.debug.print("Error: No arguments provided\n", .{});
        std.debug.print("Usage: doxa [--debug] [--compile] [--output/-o file] [script]\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    }

    var options = CLI{
        .debug = false,
        .compile = false,
        .output = null,
        .script_path = null,
    };

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        std.debug.print("Debug: Processing argument {d}: '{s}'\n", .{ i, arg });

        if (stringEquals(arg, "--debug")) {
            std.debug.print("Debug: Found debug flag\n", .{});
            options.debug = true;
        } else if (stringEquals(arg, "--compile")) {
            std.debug.print("Debug: Found compile flag\n", .{});
            options.compile = true;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            std.debug.print("Debug: Found output flag with value: '{s}'\n", .{args[i + 1]});
            options.output = args[i + 1];
            i += 1;
        } else if (options.script_path == null) {
            std.debug.print("Debug: Found script path: '{s}'\n", .{arg});
            // Make a copy of the path string since args will be freed
            options.script_path = try allocator.dupe(u8, arg);
        } else {
            std.debug.print("Error: Unexpected argument: '{s}'\n", .{arg});
            std.debug.print("Usage: doxa [--debug] [--compile] [--output/-o file] [script]\n", .{});
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    // Validate that we have a script path
    if (options.script_path == null) {
        std.debug.print("Error: No script file specified\n", .{});
        std.debug.print("Usage: doxa [--debug] [--compile] [--output/-o file] [script]\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    }

    return options;
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
    std.debug.print("Debug: Checking if '{s}' ends with '{s}'\n", .{ str, suffix });
    if (str.len < suffix.len) {
        std.debug.print("Debug: String too short\n", .{});
        return false;
    }
    const start = str.len - suffix.len;
    const result = stringEquals(str[start..], suffix);
    std.debug.print("Debug: Comparing '{s}' with '{s}': {}\n", .{ str[start..], suffix, result });
    return result;
}
