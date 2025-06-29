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
const DeprecatedInterpreterImport = @import("./interpreter/interpreter_deprecated.zig");
const DeprecatedInterpreter = DeprecatedInterpreterImport.Interpreter;
const ASTWriter = @import("./utils/ast_writer.zig");
const ASTReader = @import("./utils/ast_reader.zig");
const AST = @import("./ast/ast.zig");
const SoxaCompiler = @import("./codegen/soxa.zig");
const DoxaVM = @import("./interpreter/vm.zig").HIRVM;

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

const Command = enum {
    run, // doxa run file.doxa (HIR VM - primary execution)
    compile, // doxa compile file.doxa (LLVM pipeline)

    // Testing/development only - will be removed
    deprecated_interpreter, // doxa deprecated file.doxa (deprecated interpreter)
};

const CLI = struct {
    command: Command,
    debug: bool,
    keep_intermediate: bool, // Keep .soxa files for debugging
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
/// Interpret
///==========================================================================
fn interpreter_deprecated(memoryManager: *MemoryManager, parser: *Parser, statements: []AST.Stmt) !void {
    if (memoryManager.debug_enabled) {
        std.debug.print("Debug enabled in memory manager\n", .{});
    }

    var global_env = try memoryManager.getAllocator().create(Environment);
    global_env.* = env.init(memoryManager.getAllocator(), null, memoryManager.debug_enabled, memoryManager);
    defer {
        global_env.deinit();
        memoryManager.getAllocator().destroy(global_env);
    }

    var deprecated_interpreter = DeprecatedInterpreter.init(
        memoryManager.getAllocator(),
        global_env,
        parser,
        memoryManager.debug_enabled,
        memoryManager,
    );

    defer deprecated_interpreter.deinit();

    try deprecated_interpreter.interpret(statements);
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

/// Core production pipeline: .doxa → .soxa → [HIR VM | LLVM IR]
fn processSoxaPipeline(memoryManager: *MemoryManager, source_path: []const u8, cli_options: CLI) !void {
    // Determine output paths
    const soxa_path = try generateArtifactPath(source_path, ".soxa");
    defer std.heap.page_allocator.free(soxa_path);

    // Check if we need to recompile .doxa → .soxa
    const needs_recompile = try needsRecompilation(source_path, soxa_path);

    if (needs_recompile) {
        try compileDoxaToSoxa(memoryManager, source_path, soxa_path);
    } else if (memoryManager.debug_enabled) {
        std.debug.print("📚 Using cached SOXA: {s}\n", .{soxa_path});
    }

    // Execute based on command
    switch (cli_options.command) {
        .run => {
            std.debug.print("🚀 Executing with HIR VM\n", .{});
            try runSoxaFile(memoryManager, soxa_path);
        },

        .compile => {
            std.debug.print("🔥 Compiling to native binary\n", .{});
            const output_path = cli_options.output orelse try generateArtifactPath(source_path, "");
            defer if (cli_options.output == null) std.heap.page_allocator.free(output_path);

            try compileToNative(memoryManager, soxa_path, output_path);
        },

        .deprecated_interpreter => {
            // Temporary testing path - will be removed
            std.debug.print("🐛 AST Debug Mode (testing only)\n", .{});
            try runDeprecatedInterpreter(memoryManager, source_path);
        },
    }

    // Cleanup intermediate files unless requested to keep them
    if (!cli_options.keep_intermediate and !memoryManager.debug_enabled) {
        std.fs.cwd().deleteFile(soxa_path) catch |err| {
            if (err != error.FileNotFound and memoryManager.debug_enabled) {
                std.debug.print("⚠️  Could not cleanup {s}: {}\n", .{ soxa_path, err });
            }
        };
    }
}

/// Check if .soxa file needs to be regenerated
fn needsRecompilation(source_path: []const u8, soxa_path: []const u8) !bool {
    const source_stat = std.fs.cwd().statFile(source_path) catch return true;
    const soxa_stat = std.fs.cwd().statFile(soxa_path) catch return true;

    // Recompile if source is newer than soxa
    return source_stat.mtime >= soxa_stat.mtime;
}

/// Future: Compile SOXA to native binary via LLVM
fn compileToNative(memoryManager: *MemoryManager, soxa_path: []const u8, output_path: []const u8) !void {
    _ = memoryManager;

    // TODO: Implement LLVM pipeline
    std.debug.print("⚠️  LLVM compilation not implemented yet\n", .{});
    std.debug.print("📝 Would compile {s} → {s}\n", .{ soxa_path, output_path });
}

/// Temporary: AST debugging for testing (will be removed)
fn runDeprecatedInterpreter(memoryManager: *MemoryManager, path: []const u8) !void {
    // Read source file for AST interpretation
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

    // Write AST for debugging
    const ast_path = try generateArtifactPath(path, ".ast");
    defer std.heap.page_allocator.free(ast_path);
    try ASTWriter.writeASTToFile(statements, ast_path);

    // Execute with traditional AST interpreter
    try interpreter_deprecated(memoryManager, &parser, statements);
}

fn processFile(memoryManager: *MemoryManager, path: []const u8, cli_options: CLI) !void {
    switch (cli_options.command) {
        .run, .compile => {
            // Production pipeline: .doxa → .soxa → [HIR VM | LLVM IR]
            try processSoxaPipeline(memoryManager, path, cli_options);
        },

        .deprecated_interpreter => {
            // Temporary testing path
            try runDeprecatedInterpreter(memoryManager, path);
        },
    }
}

///==========================================================================
/// Compile DOXA to SOXA (HIR)
///==========================================================================
fn compileDoxaToSoxa(memoryManager: *MemoryManager, source_path: []const u8, soxa_path: []const u8) !void {
    // Read and parse the .doxa file
    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), source_path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    // Lexical analysis
    var lexer = Lexer.init(memoryManager.getAllocator(), source, source_path);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();

    // Parsing phase
    var parser = Parser.init(memoryManager.getAllocator(), tokens.items, source_path, memoryManager.debug_enabled);
    defer parser.deinit();
    const statements = try parser.execute();

    // Generate HIR from AST
    var reporter = Reporting.Reporter.init();
    defer reporter.deinit();

    var hir_generator = SoxaCompiler.HIRGenerator.init(memoryManager.getAllocator(), &reporter);
    defer hir_generator.deinit();

    var hir_program = try hir_generator.generateProgram(statements);
    defer hir_program.deinit();

    // Write the HIR program to .soxa file
    try SoxaCompiler.writeSoxaFile(&hir_program, soxa_path, memoryManager.getAllocator());

    if (memoryManager.debug_enabled) {
        std.debug.print("✅ Compiled {s} → {s} ({} HIR instructions)\n", .{ source_path, soxa_path, hir_program.instructions.len });
    }
}

///==========================================================================
/// Run SOXA file directly with HIR VM (for testing/comparison)
///==========================================================================
fn runSoxaFile(memoryManager: *MemoryManager, soxa_path: []const u8) !void {
    // Load HIR program from SOXA file
    var hir_program = try SoxaCompiler.readSoxaFile(soxa_path, memoryManager.getAllocator());
    defer hir_program.deinit();

    if (memoryManager.debug_enabled) {
        std.debug.print("🚀 Loaded SOXA: {} instructions, {} constants\n", .{ hir_program.instructions.len, hir_program.constant_pool.len });
    }

    // Create a reporter for HIR VM
    var reporter = Reporting.Reporter.init();

    // Create and run HIR VM directly
    var vm = try DoxaVM.init(memoryManager.getAllocator(), &hir_program, &reporter, memoryManager);
    defer vm.deinit();

    std.debug.print("🚀 Starting HIR VM execution...\n", .{});

    if (try vm.run()) |result| {
        std.debug.print("🎯 HIR VM Result: ", .{});
        try vm.printHIRValue(result.value);
        std.debug.print("\n", .{});
    } else {
        std.debug.print("✅ HIR VM completed successfully\n", .{});
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

    try processFile(&memoryManager, abs_path, cli_options);
}

fn parseArgs(allocator: std.mem.Allocator) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    var options = CLI{
        .command = .run, // Default to run execution
        .debug = false,
        .keep_intermediate = false,
        .output = null,
        .script_path = null,
    };

    var i: usize = 1;

    // Parse command (if any) and initial flags
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        // Handle global flags first
        if (stringEquals(arg, "--debug")) {
            options.debug = true;
            continue;
        } else if (stringEquals(arg, "--keep-intermediate")) {
            options.keep_intermediate = true;
            continue;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            options.output = args[i + 1];
            i += 1;
            continue;
        } else if (stringEquals(arg, "--help") or stringEquals(arg, "-h")) {
            printUsage();
            std.process.exit(0);
        }

        // Check for commands
        if (stringEquals(arg, "run")) {
            options.command = .run;
            i += 1; // Move to next argument (should be file)
            break;
        } else if (stringEquals(arg, "compile")) {
            options.command = .compile;
            i += 1; // Move to next argument (should be file)
            break;
        } else if (stringEquals(arg, "old")) {
            options.command = .deprecated_interpreter;
            i += 1; // Move to next argument (should be file)
            break;
        } else if (stringEndsWith(arg, DOXA_EXTENSION)) {
            // Direct file execution: doxa file.doxa -> defaults to run
            options.command = .run;
            options.script_path = try allocator.dupe(u8, arg);
            i += 1;
            break;
        } else {
            std.debug.print("Error: Unknown command or invalid file: '{s}'\n", .{arg});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    // Parse remaining arguments (file path and additional flags)
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (stringEquals(arg, "--debug")) {
            options.debug = true;
        } else if (stringEquals(arg, "--keep-intermediate")) {
            options.keep_intermediate = true;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            options.output = args[i + 1];
            i += 1;
        } else if (options.script_path == null and stringEndsWith(arg, DOXA_EXTENSION)) {
            // Found the script file
            options.script_path = try allocator.dupe(u8, arg);
        } else {
            std.debug.print("Error: Unexpected argument: '{s}'\n", .{arg});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    // Validate that we have a script path
    if (options.script_path == null) {
        std.debug.print("Error: No script file specified\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    return options;
}

fn printUsage() void {
    std.debug.print("Doxa Programming Language\n", .{});
    std.debug.print("\n", .{});
    std.debug.print("Usage:\n", .{});
    std.debug.print("  doxa [options] <file.doxa>              # Execute with HIR VM (default)\n", .{});
    std.debug.print("  doxa run [options] <file.doxa>          # Execute with HIR VM (explicit)\n", .{});
    std.debug.print("  doxa compile [options] <file.doxa>      # Compile to native binary\n", .{});
    std.debug.print("  doxa ast-debug [options] <file.doxa>    # Debug with AST interpreter (testing)\n", .{});
    std.debug.print("\n", .{});
    std.debug.print("Options:\n", .{});
    std.debug.print("  --debug                                 Enable debug output\n", .{});
    std.debug.print("  --keep-intermediate                     Keep .soxa files for debugging\n", .{});
    std.debug.print("  --output <file>, -o <file>             Specify output file (compile mode)\n", .{});
    std.debug.print("  --help, -h                             Show this help message\n", .{});
    std.debug.print("\n", .{});
    std.debug.print("Examples:\n", .{});
    std.debug.print("  doxa script.doxa                       # Execute with HIR VM\n", .{});
    std.debug.print("  doxa run script.doxa                   # Same as above (explicit)\n", .{});
    std.debug.print("  doxa compile --output app script.doxa  # Compile to native binary 'app'\n", .{});
    std.debug.print("  doxa ast-debug script.doxa             # Test with AST interpreter\n", .{});
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
