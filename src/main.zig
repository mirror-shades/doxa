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
const ConstantFolder = @import("./parser/constant_folder.zig").ConstantFolder;
const PeepholeOptimizer = @import("./codegen/peephole.zig").PeepholeOptimizer;

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
fn interpreter_deprecated(memoryManager: *MemoryManager, parser: *Parser, statements: []AST.Stmt, reporter: *Reporter) !void {
    var global_env = try memoryManager.getAllocator().create(Environment);
    global_env.* = env.init(memoryManager.getAllocator(), null, reporter.is_debug, memoryManager);
    defer {
        global_env.deinit();
        memoryManager.getAllocator().destroy(global_env);
    }

    var deprecated_interpreter = DeprecatedInterpreter.init(
        memoryManager.getAllocator(),
        global_env,
        parser, // Now we can pass the pointer directly
        reporter.is_debug,
        memoryManager,
        reporter,
    );

    defer deprecated_interpreter.deinit();

    try deprecated_interpreter.interpret(statements);
}

/// Generate a path for an artifact file based on the source file path
fn generateArtifactPath(source_path: []const u8, extension: []const u8) ![]u8 {
    // Extract filename from path
    var filename_start: usize = 0;
    for (source_path, 0..) |c, i| {
        if (c == '/' or c == '\\') filename_start = i + 1;
    }

    const filename = source_path[filename_start..];

    // Find the last dot in the filename
    var last_dot: ?usize = null;
    for (filename, 0..) |c, i| {
        if (c == '.') last_dot = i;
    }

    // Create output directory if it doesn't exist
    std.fs.cwd().makeDir("out") catch |err| switch (err) {
        error.PathAlreadyExists => {}, // Directory already exists, that's fine
        else => return err, // Other errors should be propagated
    };

    if (last_dot) |dot| {
        // Build path: out/filename_without_extension + extension
        const basename = filename[0..dot];
        const new_path = try std.heap.page_allocator.alloc(u8, "out/".len + basename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + basename.len)], basename);
        @memcpy(new_path[("out/".len + basename.len)..], extension);
        return new_path;
    } else {
        // If no dot found, append extension to full filename
        const new_path = try std.heap.page_allocator.alloc(u8, "out/".len + filename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + filename.len)], filename);
        @memcpy(new_path[("out/".len + filename.len)..], extension);
        return new_path;
    }
}

/// Core production pipeline: .doxa → .soxa → [HIR VM | LLVM IR]
fn processSoxaPipeline(memoryManager: *MemoryManager, source_path: []const u8, cli_options: CLI, reporter: *Reporter) !void {
    reporter.debug("=== ENTERING processSoxaPipeline ===\n", .{});
    // Determine output paths
    const soxa_path = try generateArtifactPath(source_path, ".soxa");
    defer std.heap.page_allocator.free(soxa_path);

    // TESTING: Always delete cached .soxa file to test fixes
    std.fs.cwd().deleteFile(soxa_path) catch {};

    // Check if we need to recompile .doxa → .soxa using cache validation
    const needs_recompile = try needsRecompilation(source_path, soxa_path, memoryManager.getAllocator());

    if (needs_recompile) {
        try compileDoxaToSoxa(memoryManager, source_path, soxa_path, reporter);
    } else {
        reporter.debug(">> Using cached SOXA: {s}\n", .{soxa_path});
    }

    // Execute based on command
    switch (cli_options.command) {
        .run => {
            reporter.debug(">> Executing with HIR VM\n", .{});
            // Try to run SOXA file, but recompile if it's incompatible
            runSoxaFile(memoryManager, soxa_path, reporter) catch |err| {
                if (err == error.EndOfStream or err == error.InvalidFormat) {
                    reporter.reportError("!! SOXA file incompatible, regenerating...\n", .{});
                    // Delete the incompatible SOXA file
                    std.fs.cwd().deleteFile(soxa_path) catch {};
                    // Recompile
                    try compileDoxaToSoxa(memoryManager, source_path, soxa_path, reporter);
                    // Try again
                    try runSoxaFile(memoryManager, soxa_path, reporter);
                } else {
                    return err;
                }
            };
        },

        .compile => {
            reporter.debug(">> Compiling to native binary\n", .{});
            const output_path = cli_options.output orelse try generateArtifactPath(source_path, "");
            defer if (cli_options.output == null) std.heap.page_allocator.free(output_path);

            try compileToNative(memoryManager, soxa_path, output_path, reporter);
        },

        .deprecated_interpreter => {
            // Skip SOXA generation for deprecated interpreter
            reporter.debug(">> Using deprecated interpreter\n", .{});
        },
    }
}

/// Check if .soxa file needs to be regenerated using cache validation
fn needsRecompilation(source_path: []const u8, soxa_path: []const u8, allocator: std.mem.Allocator) !bool {
    // Use embedded hash validation for robust cache checking
    return !(try SoxaCompiler.validateSoxaCache(soxa_path, source_path, allocator));
}

/// Future: Compile SOXA to native binary via LLVM
fn compileToNative(memoryManager: *MemoryManager, soxa_path: []const u8, output_path: []const u8, reporter: *Reporter) !void {
    _ = memoryManager;

    // TODO: Implement LLVM pipeline
    reporter.reportError("!! LLVM compilation not implemented yet\n", .{});
    reporter.debug(">> Would compile {s} -> {s}\n", .{ soxa_path, output_path });
}

fn processFile(memoryManager: *MemoryManager, path: []const u8, cli_options: CLI, reporter: *Reporter) !void {

    // Read source file for AST interpretation
    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    // Lexical analysis
    var lexer = Lexer.init(memoryManager.getAllocator(), source, path, reporter);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();

    // Parsing phase
    var parser = Parser.init(memoryManager.getAllocator(), tokens.items, path, reporter.is_debug, reporter);
    defer parser.deinit();
    const statements = try parser.execute();

    // Write AST for debugging
    const ast_path = try generateArtifactPath(path, ".ast");
    defer std.heap.page_allocator.free(ast_path);
    try ASTWriter.writeASTToFile(statements, ast_path);

    switch (cli_options.command) {
        .run, .compile => {
            // Production pipeline: .doxa → .soxa → [HIR VM | LLVM IR]
            try processSoxaPipeline(memoryManager, path, cli_options, reporter);
        },

        .deprecated_interpreter => {
            try interpreter_deprecated(memoryManager, &parser, statements, reporter);
        },
    }
}

///==========================================================================
/// Compile DOXA to SOXA (HIR)
///==========================================================================
fn compileDoxaToSoxa(memoryManager: *MemoryManager, source_path: []const u8, soxa_path: []const u8, reporter: *Reporter) !void {
    // Read and parse the .doxa file
    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), source_path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    // Lexical analysis
    var lexer = Lexer.init(memoryManager.getAllocator(), source, source_path, reporter);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();

    // Parsing phase
    var parser = Parser.init(memoryManager.getAllocator(), tokens.items, source_path, reporter.is_debug, reporter);
    defer parser.deinit();
    var statements = try parser.execute();

    // Constant folding optimization pass
    var constant_folder = ConstantFolder.init(memoryManager.getAllocator());
    for (statements, 0..) |*stmt, i| {
        statements[i] = try constant_folder.foldStmt(stmt);
    }

    reporter.debug(">> Constant folding applied: {} optimizations\n", .{constant_folder.getOptimizationCount()});

    var hir_generator = SoxaCompiler.HIRGenerator.init(memoryManager.getAllocator(), reporter);
    defer hir_generator.deinit();

    var hir_program = try hir_generator.generateProgram(statements);
    defer hir_program.deinit();

    // Apply peephole optimizations to HIR
    var peephole_optimizer = PeepholeOptimizer.init(memoryManager.getAllocator());
    const optimized_instructions = try peephole_optimizer.optimize(hir_program.instructions, reporter);
    defer memoryManager.getAllocator().free(optimized_instructions);

    // Replace the instructions in the HIR program
    memoryManager.getAllocator().free(hir_program.instructions);
    hir_program.instructions = optimized_instructions;

    reporter.debug(">> Peephole optimizations applied: {} HIR instruction optimizations\n", .{peephole_optimizer.getTotalOptimizations()});

    // Delete existing .soxa file if it exists
    std.fs.cwd().deleteFile(soxa_path) catch |err| {
        // It's fine if the file doesn't exist
        if (err != error.FileNotFound) {
            reporter.reportWarning("!! Warning: Could not delete existing SOXA file {s}: {}\n", .{ soxa_path, err });
        }
    };

    // Write the HIR program to .soxa file with cache validation
    try SoxaCompiler.writeSoxaFile(&hir_program, soxa_path, source_path, memoryManager.getAllocator());

    reporter.debug(">> Compiled {s} -> {s} ({} HIR instructions)\n", .{ source_path, soxa_path, hir_program.instructions.len });
}

///==========================================================================
/// Run SOXA file directly with HIR VM (optimized)
///==========================================================================
fn runSoxaFile(memoryManager: *MemoryManager, soxa_path: []const u8, reporter: *Reporter) !void {
    // Load HIR program from SOXA file
    var hir_program = try SoxaCompiler.readSoxaFile(soxa_path, memoryManager.getAllocator());
    defer hir_program.deinit();

    reporter.debug(">> Loaded SOXA: {} instructions, {} constants\n", .{ hir_program.instructions.len, hir_program.constant_pool.len });

    // Create and run HIR VM directly
    var vm = try DoxaVM.init(&hir_program, reporter, memoryManager, reporter.is_debug);
    defer vm.deinit();

    _ = try vm.run();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var reporter = Reporter.initWithAllocator(gpa.allocator());
    defer reporter.deinit();

    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) reporter.reportError("Warning: Memory leak detected!\n", .{});
    }

    // Parse args first
    const cli_options = try parseArgs(gpa.allocator(), &reporter);
    // Set debug mode based on CLI options
    reporter.is_debug = cli_options.debug;
    // Ensure script_path is freed
    defer if (cli_options.script_path) |path| {
        gpa.allocator().free(path);
    };

    reporter.debug("=== DOXA MAIN STARTED ===\n", .{});
    reporter.debug("=== CLI OPTIONS: command={s}, debug={}, keep_intermediate={} ===\n", .{ @tagName(cli_options.command), cli_options.debug, cli_options.keep_intermediate });

    // Initialize memory manager with debug setting
    var memoryManager = try MemoryManager.init(gpa.allocator(), cli_options.debug);
    defer memoryManager.deinit();

    if (cli_options.output) |out_file| {
        output_file = out_file;
    }

    // Process the script
    const path = cli_options.script_path.?; // Safe to unwrap since we validated it
    reporter.debug("Debug: Processing script: '{s}'\n", .{path});

    if (!stringEndsWith(path, DOXA_EXTENSION)) {
        reporter.reportError("Error: '{s}' is not a doxa file\n", .{path});
        std.process.exit(EXIT_CODE_USAGE);
    }

    // Convert relative path to absolute if needed
    var path_buffer: [std.fs.max_path_bytes]u8 = undefined; // Changed from MAX_PATH_BYTES to max_path_bytes
    const abs_path = try std.fs.cwd().realpath(path, &path_buffer);

    reporter.debug("Debug: Absolute path: '{s}'\n", .{abs_path});

    try processFile(&memoryManager, abs_path, cli_options, &reporter);
}

fn parseArgs(allocator: std.mem.Allocator, reporter: *Reporter) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage(reporter);
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
                reporter.reportError("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            options.output = args[i + 1];
            i += 1;
            continue;
        } else if (stringEquals(arg, "--help") or stringEquals(arg, "-h")) {
            printUsage(reporter);
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
            reporter.reportError("Error: Unknown command or invalid file: '{s}'\n", .{arg});
            printUsage(reporter);
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
                reporter.reportError("Error: --output/-o requires a filename\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            options.output = args[i + 1];
            i += 1;
        } else if (options.script_path == null and stringEndsWith(arg, DOXA_EXTENSION)) {
            // Found the script file
            options.script_path = try allocator.dupe(u8, arg);
        } else {
            reporter.reportError("Error: Unexpected argument: '{s}'\n", .{arg});
            printUsage(reporter);
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    // Validate that we have a script path
    if (options.script_path == null) {
        reporter.reportError("Error: No script file specified\n", .{});
        printUsage(reporter);
        std.process.exit(EXIT_CODE_USAGE);
    }

    return options;
}

fn printUsage(reporter: *Reporter) void {
    reporter.printf("Doxa Programming Language\n", .{});
    reporter.printl("");
    reporter.printf("Usage:\n", .{});
    reporter.printf("  doxa run [options] <file.doxa>          # Execute with HIR VM (explicit)\n", .{});
    reporter.printf("  doxa compile [options] <file.doxa>      # Compile to native binary\n", .{});
    reporter.printf("  doxa old [options] <file.doxa>          # Debug with old AST interpreter (deprecated)\n", .{});
    reporter.printl("");
    reporter.printf("Options:\n", .{});
    reporter.printf("  --debug                                 Enable debug output\n", .{});
    reporter.printf("  --keep-intermediate                     Keep .soxa files for debugging\n", .{});
    reporter.printf("  --output <file>, -o <file>             Specify output file (compile mode)\n", .{});
    reporter.printf("  --help, -h                             Show this help message\n", .{});
    reporter.printl("");
    reporter.printf("Examples:\n", .{});
    reporter.printf("  doxa script.doxa                       # Execute with HIR VM\n", .{});
    reporter.printf("  doxa run script.doxa                   # Same as above (explicit)\n", .{});
    reporter.printf("  doxa compile --output app script.doxa  # Compile to native binary 'app'\n", .{});
    reporter.printf("  doxa old script.doxa                   # Test with AST interpreter\n", .{});
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
    if (str.len < suffix.len) {
        return false;
    }
    const start = str.len - suffix.len;
    const result = stringEquals(str[start..], suffix);
    return result;
}
