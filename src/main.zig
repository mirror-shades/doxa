const std = @import("std");
const builtin = @import("builtin");
const LexicalAnalyzer = @import("./analysis/lexical.zig").LexicalAnalyzer;
const SemanticAnalyzer = @import("./analysis/semantic/semantic.zig").SemanticAnalyzer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Reporting = @import("./utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const ReporterOptions = Reporting.ReporterOptions;
const MemoryImport = @import("./utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Token = @import("./types/token.zig").Token;
const TypesImport = @import("./types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const Environment = TypesImport.Environment;
const env = @import("./interpreter/environment.zig");
const ASTWriter = @import("./utils/ast_writer.zig");
const ASTReader = @import("./utils/ast_reader.zig");
const AST = @import("./ast/ast.zig");
const ast = AST;
const SoxaCompiler = @import("./codegen/hir/soxa.zig");
const HIRGenerator = @import("./codegen/hir/soxa_generator.zig").HIRGenerator;
const SoxaTextParser = @import("./codegen/hir/soxa_parser.zig").SoxaTextParser;
const DoxaVM = @import("./interpreter/vm.zig").HIRVM;
const ConstantFolder = @import("./parser/constant_folder.zig").ConstantFolder;
const PeepholeOptimizer = @import("./codegen/hir/peephole.zig").PeepholeOptimizer;
const Errors = @import("./utils/errors.zig");
const ErrorCode = Errors.ErrorCode;

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
};

const CLI = struct {
    command: Command,
    debug_lexer: bool,
    reporter_options: ReporterOptions,
    keep_artifacts: bool,
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
/// Generate a path for an artifact file based on the source file path
fn generateArtifactPath(memoryManager: *MemoryManager, source_path: []const u8, extension: []const u8) ![]u8 {
    // Find the last path separator to extract the filename
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
        const new_path = try memoryManager.getAllocator().alloc(u8, "out/".len + basename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + basename.len)], basename);
        @memcpy(new_path[("out/".len + basename.len)..], extension);
        return new_path;
    } else {
        // If no dot found, append extension to full filename
        const new_path = try memoryManager.getAllocator().alloc(u8, "out/".len + filename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + filename.len)], filename);
        @memcpy(new_path[("out/".len + filename.len)..], extension);
        return new_path;
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
    reporter.reportInternalError("!! LLVM compilation not implemented yet\n", .{}, @src());
    reporter.debug(">> Would compile {s} -> {s}\n", .{ soxa_path, output_path }, @src());
}

///==========================================================================
/// Compile DOXA to SOXA (HIR) - using pre-parsed AST
///==========================================================================
fn compileDoxaToSoxaFromAST(memoryManager: *MemoryManager, statements: []ast.Stmt, parser: *Parser, semantic_analyzer: *SemanticAnalyzer, source_path: []const u8, soxa_path: []const u8, reporter: *Reporter) !void {

    // Apply constant folding to statements
    var constant_folder = ConstantFolder.init(memoryManager.getAllocator());
    var folded_statements = std.array_list.Managed(ast.Stmt).init(memoryManager.getAllocator());
    defer folded_statements.deinit();

    // Create mutable copies of statements for constant folding
    for (statements) |stmt| {
        var mutable_stmt = stmt;
        const folded_stmt = try constant_folder.foldStmt(&mutable_stmt);
        try folded_statements.append(folded_stmt);
    }

    reporter.debug(">> Constant folding applied: {} optimizations\n", .{constant_folder.getOptimizationCount()}, @src());

    // NEW: Pass custom type information to HIR generator
    // Use analysis arena for HIR generation since this happens during compilation phase
    var hir_generator = HIRGenerator.init(memoryManager.getAnalysisAllocator(), reporter, parser.module_namespaces, parser.imported_symbols);
    defer hir_generator.deinit();

    // NEW: Import custom types from semantic analysis
    const custom_types = semantic_analyzer.getCustomTypes();
    var custom_types_iter = custom_types.iterator();
    while (custom_types_iter.next()) |entry| {
        const custom_type = entry.value_ptr.*;
        const converted_type = try SemanticAnalyzer.convertCustomTypeInfo(custom_type, memoryManager.getAnalysisAllocator());
        try hir_generator.type_system.custom_types.put(custom_type.name, converted_type);
    }

    // NEW: Import struct methods from semantic analysis (deep copy tables to avoid double-free)
    const struct_methods = semantic_analyzer.getStructMethods();
    var struct_methods_iter = struct_methods.iterator();
    while (struct_methods_iter.next()) |entry| {
        const struct_name = entry.key_ptr.*;
        const method_table_src = entry.value_ptr.*;

        var method_table_dst = std.StringHashMap(@import("analysis/semantic/semantic.zig").StructMethodInfo).init(memoryManager.getAnalysisAllocator());
        var mi_it = method_table_src.iterator();
        while (mi_it.next()) |mi_entry| {
            const mname = mi_entry.key_ptr.*;
            const mi = mi_entry.value_ptr.*;
            try method_table_dst.put(mname, mi);
        }

        try hir_generator.struct_methods.put(struct_name, method_table_dst);
    }

    var hir_program = try hir_generator.generateProgram(folded_statements.items);
    defer hir_program.deinit();

    // // Apply peephole optimizations to HIR
    // // TODO: Re-enable this once we are more stable
    // var peephole_optimizer = PeepholeOptimizer.init(memoryManager.getAllocator(), reporter);
    // const optimized_instructions = try peephole_optimizer.optimize(hir_program.instructions);
    // defer memoryManager.getAllocator().free(optimized_instructions);

    // // Replace the instructions in the HIR program
    // memoryManager.getAllocator().free(hir_program.instructions);
    // hir_program.instructions = optimized_instructions;

    // reporter.debug(">> Peephole optimizations applied: {} HIR instruction optimizations\n", .{peephole_optimizer.getTotalOptimizations()}, @src());

    // Delete existing .soxa file if it exists
    std.fs.cwd().deleteFile(soxa_path) catch |err| {
        // It's fine if the file doesn't exist
        if (err != error.FileNotFound) {
            const location = Location{
                .file = soxa_path,
                .range = .{
                    .start_line = 0,
                    .start_col = 0,
                    .end_line = 0,
                    .end_col = 0,
                },
            };
            reporter.reportWarning(location, ErrorCode.COULD_NOT_DELETE_ARTIFACT, "Could not delete existing SOXA file {s}: {}\n", .{ soxa_path, err });
        }
    };

    // Write the HIR program to .soxa file with cache validation
    try SoxaCompiler.writeSoxaFile(&hir_program, soxa_path, source_path, memoryManager.getAllocator());

    reporter.debug(">> Compiled {s} -> {s} ({} HIR instructions)\n", .{ source_path, soxa_path, hir_program.instructions.len }, @src());
}

///==========================================================================
/// Run SOXA file directly with HIR VM (optimized)
///==========================================================================
fn runSoxaFile(memoryManager: *MemoryManager, soxa_path: []const u8, reporter: *Reporter) !void {
    // Load HIR program from SOXA file
    var hir_program = try SoxaCompiler.readSoxaFile(soxa_path, memoryManager.getAllocator());
    defer hir_program.deinit();

    reporter.debug(">> Loaded SOXA: {} instructions, {} constants\n", .{ hir_program.instructions.len, hir_program.constant_pool.len }, @src());

    // Create and run HIR VM directly
    var vm = try DoxaVM.init(&hir_program, reporter, memoryManager);
    defer vm.deinit();

    // NEW: Bridge custom types from memory manager to VM
    try memoryManager.bridgeTypesToVM(&vm);

    // Run the VM
    _ = try vm.run();
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
        .reporter_options = .{
            .max_diagnostics = 1000,
            .warn_as_error = false,
            .debug_mode = false,
        },
        .keep_artifacts = false,
        .debug_lexer = false,
        .output = null,
        .script_path = null,
    };

    var i: usize = 1;

    // Parse command (if any) and initial flags
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        // Handle global flags first
        if (stringEquals(arg, "--debug")) {
            options.reporter_options.debug_mode = true;
            continue;
        } else if (stringEquals(arg, "--debug-lexer")) {
            options.debug_lexer = true;
            continue;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                printUsage();
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
            std.debug.print("native compilation not yet implemented\n", .{});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
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
            options.reporter_options.debug_mode = true;
        } else if (stringEquals(arg, "--keep-intermediate")) {
            options.keep_artifacts = true;
        } else if (stringEquals(arg, "--output") or stringEquals(arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output/-o requires a filename\n", .{});
                printUsage();
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
    std.debug.print("\nUsage:\n", .{});
    std.debug.print("  doxa run [options] <file.doxa>          # Execute with HIR VM (explicit)\n", .{});
    std.debug.print("\nOptions:\n", .{});
    std.debug.print("  --debug-lexer                           Print lexer output\n", .{});
    std.debug.print("  --debug                                 Enable debug output\n", .{});
    std.debug.print("  --keep-intermediate                     Keep .soxa files for debugging\n", .{});
    std.debug.print("  --help, -h                             Show this help message\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  doxa script.doxa                       # Execute with HIR VM\n", .{});
    std.debug.print("  doxa run script.doxa                   # Same as above (explicit)\n", .{});
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
pub fn main() !void {
    //==========================================================================
    // Initialization
    //==========================================================================
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }

    if (builtin.os.tag == .windows) {
        // Set the console output code page to UTF-8 to enable Unicode support
        // I think this is only needed for Windows
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    // Parse args first
    const cli_options = try parseArgs(gpa.allocator());

    var reporter = Reporter.init(gpa.allocator(), cli_options.reporter_options);
    defer reporter.deinit();

    // Ensure script_path is freed
    defer if (cli_options.script_path) |path| {
        gpa.allocator().free(path);
    };

    // Initialize memory manager with debug setting
    var memoryManager = try MemoryManager.init(gpa.allocator());
    defer memoryManager.deinit();

    if (cli_options.output) |out_file| {
        output_file = out_file;
    }

    //==========================================================================
    // Process the script
    //==========================================================================

    // Process the script
    const path = cli_options.script_path.?; // Safe to unwrap since we validated it
    reporter.debug("Processing script: '{s}'\n", .{path}, @src());

    if (!stringEndsWith(path, DOXA_EXTENSION)) {
        const loc = Location{
            .file = path,
            .range = .{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
            },
        };
        reporter.reportCompileError(loc, null, "Error: '{s}' is not a doxa file\n", .{path});
        std.process.exit(EXIT_CODE_USAGE);
    }

    // Convert relative path to absolute if needed
    var path_buffer: [std.fs.max_path_bytes]u8 = undefined; // Changed from MAX_PATH_BYTES to max_path_bytes
    const abs_path = try std.fs.cwd().realpath(path, &path_buffer);

    reporter.debug("Absolute path: '{s}'\n", .{abs_path}, @src());

    if (cli_options.reporter_options.debug_mode) {
        std.debug.print("Debug mode enabled\n", .{});
    }
    reporter.debug("reporter debug method working\n", .{}, @src());

    // Read source file for AST interpretation
    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    //==========================================================================
    // Lexical analysis
    //==========================================================================

    // Lexical analysis
    var lexer = LexicalAnalyzer.init(memoryManager.getAllocator(), source, path, &reporter);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();

    if (cli_options.debug_lexer) {
        for (tokens.items) |token| {
            std.debug.print("{any} {s}\n", .{ token.type, token.lexeme });
        }
    }

    //==========================================================================
    // Parsing phase
    //==========================================================================

    // Parsing phase
    var parser = Parser.init(memoryManager.getAllocator(), tokens.items, path, &reporter);
    defer parser.deinit();
    const statements = try parser.execute();

    // Write AST for debugging
    const ast_path = try generateArtifactPath(&memoryManager, path, ".ast");
    defer memoryManager.getAllocator().free(ast_path);
    try ASTWriter.writeASTToFile(statements, ast_path);

    //==========================================================================
    // Semantic analysis
    //==========================================================================
    var semantic_analyzer = SemanticAnalyzer.init(memoryManager.getAnalysisAllocator(), &reporter, &memoryManager, &parser);
    defer semantic_analyzer.deinit();
    try semantic_analyzer.analyze(statements);

    //memoryManager.scope_manager.dumpState(0);

    //==========================================================================
    // Compile to SOXA
    //==========================================================================

    // Determine output paths
    const soxa_path = try generateArtifactPath(&memoryManager, path, ".soxa");
    defer memoryManager.getAllocator().free(soxa_path);

    // TESTING: Always delete cached .soxa file for now
    // TODO: Remove this once we have a proper cache system
    std.fs.cwd().deleteFile(soxa_path) catch {};

    // Check if we need to recompile .doxa → .soxa using cache validation
    const needs_recompile = try needsRecompilation(path, soxa_path, memoryManager.getAllocator());

    if (needs_recompile) {
        try compileDoxaToSoxaFromAST(&memoryManager, statements, &parser, &semantic_analyzer, path, soxa_path, &reporter);
    } else {
        reporter.debug(">> Using cached SOXA: {s}\n", .{soxa_path}, @src());
    }

    //==========================================================================
    // Execute
    //==========================================================================

    // Execute based on command
    switch (cli_options.command) {
        .run => {
            reporter.debug(">> Executing with HIR VM\n", .{}, @src());
            // Try to run SOXA file, but recompile if it's incompatible
            runSoxaFile(&memoryManager, soxa_path, &reporter) catch |err| {
                if (err == error.EndOfStream or err == error.InvalidFormat) {
                    reporter.reportCompileError(
                        Location{
                            .file = path,
                            .range = .{
                                .start_line = 0,
                                .start_col = 0,
                                .end_line = 0,
                                .end_col = 0,
                            },
                        },
                        ErrorCode.SOXA_FILE_INCOMPATIBLE,
                        "!! SOXA file incompatible, regenerating...\n",
                        .{},
                    );
                    // Delete the incompatible SOXA file
                    std.fs.cwd().deleteFile(soxa_path) catch {};
                    // Recompile
                    try compileDoxaToSoxaFromAST(&memoryManager, statements, &parser, &semantic_analyzer, path, soxa_path, &reporter);
                    // Try again
                    try runSoxaFile(&memoryManager, soxa_path, &reporter);
                } else {
                    return err;
                }
            };
        },

        .compile => {
            reporter.debug(">> Compiling to native binary\n", .{}, @src());
            const output_path = cli_options.output orelse try generateArtifactPath(&memoryManager, path, "");
            defer if (cli_options.output == null) memoryManager.getAllocator().free(output_path);

            try compileToNative(&memoryManager, soxa_path, output_path, &reporter);
        },
    }
}
