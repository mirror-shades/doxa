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
const AST = @import("./ast/ast.zig");
const ast = AST;
const SoxaCompiler = @import("./codegen/hir/soxa.zig");
const HIRGenerator = @import("./codegen/hir/soxa_generator.zig").HIRGenerator;
const SoxaTextParser = @import("./codegen/hir/soxa_parser.zig").SoxaTextParser;
const HIRProgram = @import("./codegen/hir/soxa_types.zig").HIRProgram;
const VM = @import("./interpreter/vm.zig").VM;
const ConstantFolder = @import("./parser/constant_folder.zig").ConstantFolder;
const Errors = @import("./utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ProfilerImport = @import("./utils/profiler.zig");
const Phase = ProfilerImport.Phase;
const Profiler = ProfilerImport.Profiler;
const BytecodeGenerator = @import("./codegen/bytecode/generator.zig").BytecodeGenerator;
const BytecodeWriter = @import("./codegen/bytecode/writer.zig");
const BytecodeModule = @import("./codegen/bytecode/module.zig").BytecodeModule;
const StructMethodInfo = @import("./analysis/semantic/semantic.zig").StructMethodInfo;
const LspServer = @import("./lsp/server.zig");

const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const EXIT_CODE_RUNTIME = 1;
const MAX_FILE_SIZE = 1024 * 1024;
const DOXA_EXTENSION = ".doxa";
const DEFAULT_OUTPUT_FILE = "output.o";

var compile_file: bool = false;
var is_safe_repl: bool = false;
var output_file: ?[]const u8 = null;

const Mode = enum {
    UNDEFINED,
    RUN,
    COMPILE,
};

const LspMode = enum {
    none,
    stdio,
    harness,
};

const CLI = struct {
    mode: Mode,
    reporter_options: ReporterOptions,
    script_path: ?[]const u8,
    profile: bool,
    output_path: ?[]const u8,
    target_arch: ?[]const u8,
    target_os: ?[]const u8,
    target_abi: ?[]const u8,
    opt_level: i32, // -1 for debug-peek, 0..3 for optimization
    lsp_mode: LspMode,
    lsp_debug_file: ?[]const u8,
    lsp_io_trace: bool,

    pub fn deinit(self: *const CLI, allocator: std.mem.Allocator) void {
        if (self.script_path) |p| allocator.free(p);
        if (self.output_path) |p| allocator.free(p);
        if (self.target_arch) |p| allocator.free(p);
        if (self.target_os) |p| allocator.free(p);
        if (self.target_abi) |p| allocator.free(p);
        if (self.lsp_debug_file) |p| allocator.free(p);
    }
};

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

fn generateArtifactPath(memoryManager: *MemoryManager, source_path: []const u8, extension: []const u8) ![]u8 {
    var filename_start: usize = 0;
    for (source_path, 0..) |c, i| {
        if (c == '/' or c == '\\') filename_start = i + 1;
    }

    const filename = source_path[filename_start..];

    var last_dot: ?usize = null;
    for (filename, 0..) |c, i| {
        if (c == '.') last_dot = i;
    }

    std.fs.cwd().makeDir("out") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    if (last_dot) |dot| {
        const basename = filename[0..dot];
        const new_path = try memoryManager.getAllocator().alloc(u8, "out/".len + basename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + basename.len)], basename);
        @memcpy(new_path[("out/".len + basename.len)..], extension);
        return new_path;
    } else {
        const new_path = try memoryManager.getAllocator().alloc(u8, "out/".len + filename.len + extension.len);
        @memcpy(new_path[0.."out/".len], "out/");
        @memcpy(new_path["out/".len..("out/".len + filename.len)], filename);
        @memcpy(new_path[("out/".len + filename.len)..], extension);
        return new_path;
    }
}

fn needsRecompilation(source_path: []const u8, soxa_path: []const u8, allocator: std.mem.Allocator) !bool {
    return !(try SoxaCompiler.validateSoxaCache(soxa_path, source_path, allocator));
}

fn writeBytecodeArtifact(bytecode_module: *BytecodeModule, path: []const u8) !void {
    if (std.fs.path.dirname(path)) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }

    try BytecodeWriter.writeBytecodeModuleToFile(bytecode_module, path);
}

fn generateHIRProgram(memoryManager: *MemoryManager, statements: []ast.Stmt, parser: *Parser, semantic_analyzer: *SemanticAnalyzer, reporter: *Reporter) !HIRProgram {
    var constant_folder = ConstantFolder.init(memoryManager.getAnalysisAllocator());
    var folded_statements = std.array_list.Managed(ast.Stmt).init(memoryManager.getAnalysisAllocator());
    defer folded_statements.deinit();

    for (statements) |stmt| {
        var mutable_stmt = stmt;
        const folded_stmt = try constant_folder.foldStmt(&mutable_stmt);
        try folded_statements.append(folded_stmt);
    }

    const function_return_types = semantic_analyzer.getFunctionReturnTypes();
    var hir_generator = HIRGenerator.init(memoryManager.getAnalysisAllocator(), reporter, parser.module_namespaces, parser.imported_symbols, function_return_types, semantic_analyzer);
    defer hir_generator.deinit();

    const custom_types = semantic_analyzer.getCustomTypes();
    var custom_types_iter = custom_types.iterator();
    while (custom_types_iter.next()) |entry| {
        const custom_type = entry.value_ptr.*;
        const converted_type = try SemanticAnalyzer.convertCustomTypeInfo(custom_type, memoryManager.getAnalysisAllocator());
        try hir_generator.type_system.custom_types.put(custom_type.name, converted_type);
    }

    // Manually register TokenType enum if not already registered
    if (hir_generator.type_system.custom_types.get("TokenType") == null) {
        const tokentype_variants = try memoryManager.getAnalysisAllocator().alloc([]const u8, 16);
        tokentype_variants[0] = try memoryManager.getAnalysisAllocator().dupe(u8, "INT_LITERAL");
        tokentype_variants[1] = try memoryManager.getAnalysisAllocator().dupe(u8, "FLOAT_LITERAL");
        tokentype_variants[2] = try memoryManager.getAnalysisAllocator().dupe(u8, "BYTE_LITERAL");
        tokentype_variants[3] = try memoryManager.getAnalysisAllocator().dupe(u8, "TETRA_LITERAL");
        tokentype_variants[4] = try memoryManager.getAnalysisAllocator().dupe(u8, "STRING_LITERAL");
        tokentype_variants[5] = try memoryManager.getAnalysisAllocator().dupe(u8, "NOTHING_LITERAL");
        tokentype_variants[6] = try memoryManager.getAnalysisAllocator().dupe(u8, "VAR");
        tokentype_variants[7] = try memoryManager.getAnalysisAllocator().dupe(u8, "CONST");
        tokentype_variants[8] = try memoryManager.getAnalysisAllocator().dupe(u8, "FUNCTION");
        tokentype_variants[9] = try memoryManager.getAnalysisAllocator().dupe(u8, "MAIN");
        tokentype_variants[10] = try memoryManager.getAnalysisAllocator().dupe(u8, "ENTRY");
        tokentype_variants[11] = try memoryManager.getAnalysisAllocator().dupe(u8, "ASSIGN");
        tokentype_variants[12] = try memoryManager.getAnalysisAllocator().dupe(u8, "MODULE");
        tokentype_variants[13] = try memoryManager.getAnalysisAllocator().dupe(u8, "IMPORT");
        tokentype_variants[14] = try memoryManager.getAnalysisAllocator().dupe(u8, "FROM");
        tokentype_variants[15] = try memoryManager.getAnalysisAllocator().dupe(u8, "IDENTIFIER");
        try hir_generator.registerEnumType("TokenType", tokentype_variants);
    }

    const struct_methods = semantic_analyzer.getStructMethods();
    var struct_methods_iter = struct_methods.iterator();
    while (struct_methods_iter.next()) |entry| {
        const struct_name = entry.key_ptr.*;
        const method_table_src = entry.value_ptr.*;

        var method_table_dst = std.StringHashMap(StructMethodInfo).init(memoryManager.getAnalysisAllocator());
        var mi_it = method_table_src.iterator();
        while (mi_it.next()) |mi_entry| {
            const mname = mi_entry.key_ptr.*;
            const mi = mi_entry.value_ptr.*;
            try method_table_dst.put(mname, mi);
        }

        try hir_generator.struct_methods.put(struct_name, method_table_dst);
    }

    const hir_program = try hir_generator.generateProgram(folded_statements.items);
    return hir_program;
}

fn compileDoxaToSoxaFromAST(memoryManager: *MemoryManager, statements: []ast.Stmt, parser: *Parser, semantic_analyzer: *SemanticAnalyzer, source_path: []const u8, soxa_path: []const u8, reporter: *Reporter) !void {
    var hir_program = try generateHIRProgram(memoryManager, statements, parser, semantic_analyzer, source_path, soxa_path, reporter);
    defer hir_program.deinit();
}

fn runBytecodeModule(memoryManager: *MemoryManager, bytecode_module: *BytecodeModule, reporter: *Reporter) !void {
    var vm = try VM.init(memoryManager.getAllocator(), bytecode_module, reporter, memoryManager);
    defer vm.deinit();

    try memoryManager.bridgeTypesToVM(&vm);

    vm.run() catch |err| switch (err) {
        error.RuntimeTrap => return error.RuntimeTrap,
        else => return err,
    };
}

fn parseArgs(allocator: std.mem.Allocator) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Error: No arguments provided\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    var options = CLI{
        .reporter_options = .{
            .max_diagnostics = 1000,
            .warn_as_error = false,
            .debug_lexer = false,
            .debug_parser = false,
            .debug_semantic = false,
            .debug_hir = false,
            .debug_bytecode = false,
            .debug_execution = false,
            .debug_verbose = false,
        },
        .mode = .UNDEFINED,
        .script_path = null,
        .profile = false,
        .output_path = null,
        .target_arch = null,
        .target_os = null,
        .target_abi = null,
        .opt_level = 0,
        .lsp_mode = .none,
        .lsp_debug_file = null,
        .lsp_io_trace = false,
    };

    if (stringEquals(args[1], "--lsp")) {
        options.lsp_mode = .stdio;
        if (args.len > 2) {
            for (args[2..]) |arg| {
                if (stringEquals(arg, "--lsp-debug-io")) {
                    options.lsp_io_trace = true;
                } else {
                    std.debug.print("Error: Unknown flag for --lsp: '{s}'\n", .{arg});
                    printUsage();
                    std.process.exit(EXIT_CODE_USAGE);
                }
            }
        }
        return options;
    } else if (stringEquals(args[1], "--lsp-debug")) {
        if (args.len < 3) {
            std.debug.print("Error: Provide a .doxa file for --lsp-debug\n", .{});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        } else if (args.len > 3) {
            std.debug.print("Error: Unexpected extra arguments for --lsp-debug\n", .{});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
        options.lsp_mode = .harness;
        if (!stringEndsWith(args[2], DOXA_EXTENSION)) {
            std.debug.print("Error: '{s}' is not a .doxa file\n", .{args[2]});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
        options.lsp_debug_file = try allocator.dupe(u8, args[2]);
        return options;
    }

    // Handle the mode argument (run or compile)
    if (stringEquals(args[1], "run")) {
        options.mode = .RUN;
    } else if (stringEquals(args[1], "compile")) {
        options.mode = .COMPILE;
    } else {
        std.debug.print("Error: specify `run` or `compile`\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    var options_list = std.array_list.Managed([]const u8).init(allocator);
    defer options_list.deinit();

    // Process remaining arguments starting from index 2
    for (args[2..]) |arg| {
        try options_list.append(arg);
    }

    var expecting_output: bool = false;
    for (options_list.items) |arg| {
        if (expecting_output) {
            options.output_path = try allocator.dupe(u8, arg);
            expecting_output = false;
            continue;
        }
        if (stringEquals(arg, "--debug-verbose")) {
            options.reporter_options.debug_verbose = true;
            continue;
        } else if (stringEquals(arg, "--debug-lexer")) {
            options.reporter_options.debug_lexer = true;
            continue;
        } else if (stringEquals(arg, "--debug-parser")) {
            options.reporter_options.debug_parser = true;
            continue;
        } else if (stringEquals(arg, "--debug-semantic")) {
            options.reporter_options.debug_semantic = true;
            continue;
        } else if (stringEquals(arg, "--debug-hir")) {
            options.reporter_options.debug_hir = true;
            continue;
        } else if (stringEquals(arg, "--debug-bytecode")) {
            options.reporter_options.debug_bytecode = true;
            continue;
        } else if (stringEquals(arg, "--debug-execution")) {
            options.reporter_options.debug_execution = true;
            continue;
        } else if (stringEquals(arg, "--profile")) {
            options.profile = true;
            continue;
        } else if (stringEquals(arg, "-o") or stringEquals(arg, "--output")) {
            // Set flag to expect output path in next argument
            expecting_output = true;
            continue;
        } else if (std.mem.startsWith(u8, arg, "--arch=")) {
            options.target_arch = try allocator.dupe(u8, arg[7..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--os=")) {
            options.target_os = try allocator.dupe(u8, arg[5..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--abi=")) {
            options.target_abi = try allocator.dupe(u8, arg[6..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--opt=")) {
            options.opt_level = std.fmt.parseInt(i32, arg[6..], 10) catch {
                std.debug.print("Error: invalid --opt level: {s}\n", .{arg});
                std.process.exit(EXIT_CODE_USAGE);
            };
            continue;
        } else if (arg.len >= 3 and arg[0] == '-' and arg[1] == 'O') {
            // Accept -O-1, -O0..-O3
            const lvl_str = arg[2..];
            options.opt_level = std.fmt.parseInt(i32, lvl_str, 10) catch {
                std.debug.print("Error: invalid optimization flag: {s}\n", .{arg});
                std.process.exit(EXIT_CODE_USAGE);
            };
            continue;
        } else if (stringEquals(arg, "--help") or stringEquals(arg, "-h")) {
            printUsage();
            std.process.exit(0);
        } else if (stringEndsWith(arg, DOXA_EXTENSION)) {
            options.script_path = try allocator.dupe(u8, arg);
            continue;
        } else {
            std.debug.print("Error: Unknown command or invalid file: '{s}'\n", .{arg});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    if (options.script_path == null) {
        std.debug.print("Error: No file specified\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    if (options.mode == .COMPILE and options.output_path == null) {
        std.debug.print("Error: compile mode requires -o/--output <path>\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    return options;
}

fn printUsage() void {
    std.debug.print("Doxa Programming Language\n", .{});
    std.debug.print("\nUsage:\n", .{});
    std.debug.print("  doxa run [general options] <file.doxa>\n", .{});
    std.debug.print("  doxa compile [general options] <file.doxa> -o <output> [compile options]\n", .{});
    std.debug.print("  doxa --lsp [--lsp-debug-io]     # Start the Language Server Protocol loop\n", .{});
    std.debug.print("  doxa --lsp-debug <file.doxa>    # Run the in-process LSP debug harness\n", .{});
    std.debug.print("\nGeneral options:\n", .{});
    std.debug.print("  --profile                         # Enable profiling\n", .{});
    std.debug.print("  --help, -h                        # Show this help message\n", .{});
    std.debug.print("  --debug-[stage]                   # Enable debug output for [stage]\n", .{});
    std.debug.print("                                    # lexer, parser, semantic, hir, bytecode, execution\n", .{});
    std.debug.print("  --debug-verbose                   # Enable all debug output\n", .{});
    std.debug.print("\nCompile options:\n", .{});
    std.debug.print("  -o, --output <path>               # Output executable path (required)\n", .{});
    std.debug.print("  --arch=<arch>                     # Target CPU architecture (default: host)\n", .{});
    std.debug.print("  --os=<os>                         # Target operating system (default: host)\n", .{});
    std.debug.print("  --abi=<abi>                       # Target ABI (optional)\n", .{});
    std.debug.print("  -O-1 | --opt=-1                   # Debug-aware codegen (peek dumps)\n", .{});
    std.debug.print("  -O0..-O3 | --opt=0..3             # LLVM and codegen optimization level\n", .{});
    std.debug.print("  --lsp-debug-io                    # Trace raw LSP I/O when used with --lsp\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  doxa run file.doxa\n", .{});
    std.debug.print("  doxa compile file.doxa -o out/myapp\n", .{});
    std.debug.print("  doxa compile file.doxa -o out/myapp --arch=x86_64 --os=linux -O2\n", .{});
}

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

fn lexicAnalysis(memoryManager: *MemoryManager, source: []const u8, path: []const u8, reporter: *Reporter) !std.array_list.Managed(Token) {
    var lexer = try LexicalAnalyzer.init(memoryManager.getAnalysisAllocator(), source, path, reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    return tokens;
}

pub fn main() !void {
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

    const cli_options = try parseArgs(gpa.allocator());
    defer cli_options.deinit(gpa.allocator());

    switch (cli_options.lsp_mode) {
        .none => {},
        .stdio => {
            try LspServer.run(gpa.allocator(), .{
                .reporter_options = cli_options.reporter_options,
                .trace_io = cli_options.lsp_io_trace,
            });
            return;
        },
        .harness => {
            const script = cli_options.lsp_debug_file orelse unreachable;
            try LspServer.runDebugHarness(gpa.allocator(), .{
                .reporter_options = cli_options.reporter_options,
                .script_path = script,
            });
            return;
        },
    }

    var reporter = Reporter.init(gpa.allocator(), cli_options.reporter_options);
    defer reporter.deinit();

    var memoryManager = try MemoryManager.init(gpa.allocator());
    defer memoryManager.deinit();

    var profiler = Profiler.init(gpa.allocator(), cli_options.profile);
    defer profiler.deinit();

    const path = cli_options.script_path.?; // Safe to unwrap since we validated it

    const path_uri = try reporter.ensureFileUri(path);

    if (!stringEndsWith(path, DOXA_EXTENSION)) {
        const loc = Location{
            .file = path,
            .file_uri = path_uri,
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

    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAnalysisAllocator(), path, MAX_FILE_SIZE);
    defer memoryManager.getAnalysisAllocator().free(source);

    profiler.startPhase(Phase.LEXIC_A);
    const lexedTokens = try lexicAnalysis(&memoryManager, source, path, &reporter);
    defer lexedTokens.deinit();
    profiler.stopPhase();

    profiler.startPhase(Phase.PARSING);
    var parser = Parser.init(memoryManager.getAnalysisAllocator(), lexedTokens.items, path, path_uri, &reporter);
    defer parser.deinit();
    const parsedStatements = try parser.execute();
    profiler.stopPhase();

    profiler.startPhase(Phase.SEMANTIC_A);
    var semantic_analyzer = SemanticAnalyzer.init(memoryManager.getAnalysisAllocator(), &reporter, &memoryManager, &parser);
    defer semantic_analyzer.deinit();
    try semantic_analyzer.analyze(parsedStatements);
    profiler.stopPhase();

    profiler.startPhase(Phase.GENERATE_S);

    // Generate the soxa path for intermediate files
    const soxa_path = try generateArtifactPath(&memoryManager, path, ".soxa");
    defer memoryManager.getExecutionAllocator().free(soxa_path);

    const hir_program_for_bytecode = try generateHIRProgram(&memoryManager, parsedStatements, &parser, &semantic_analyzer, &reporter);
    // Emit textual HIR (.soxa) for debugging/inspection
    try SoxaCompiler.writeSoxaFile(&hir_program_for_bytecode, soxa_path, path, memoryManager.getExecutionAllocator());
    profiler.stopPhase();

    if (cli_options.mode == .RUN) {
        profiler.startPhase(Phase.GENERATE_B);

        const hir_program = hir_program_for_bytecode;
        const artifact_stem = blk: {
            var filename_start: usize = 0;
            for (path, 0..) |c, i| {
                if (c == '/' or c == '\\') filename_start = i + 1;
            }
            const filename = path[filename_start..];
            if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot| {
                break :blk filename[0..dot];
            }
            break :blk filename;
        };

        var bytecode_generator = BytecodeGenerator.init(memoryManager.getExecutionAllocator(), "out", artifact_stem);
        bytecode_generator.source_path = path;
        var bytecode_module = try bytecode_generator.generate(&hir_program);
        defer bytecode_module.deinit();

        if (bytecode_module.artifact_path) |bc_path| {
            try writeBytecodeArtifact(&bytecode_module, bc_path);
        }
        profiler.stopPhase();

        profiler.startPhase(Phase.EXECUTION);

        runBytecodeModule(&memoryManager, &bytecode_module, &reporter) catch |err| switch (err) {
            error.RuntimeTrap => std.process.exit(EXIT_CODE_RUNTIME),
            else => return err,
        };

        profiler.stopPhase();
        try profiler.dump();
    }

    if (cli_options.mode == .COMPILE) {
        profiler.startPhase(Phase.GENERATE_L);

        const artifact_stem = blk: {
            if (cli_options.output_path) |op| break :blk op;
            var filename_start: usize = 0;
            for (path, 0..) |c, i| {
                if (c == '/' or c == '\\') filename_start = i + 1;
            }
            const filename = path[filename_start..];
            if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot| break :blk filename[0..dot];
            break :blk filename;
        };

        // Ensure out/ exists
        std.fs.cwd().makeDir("out") catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        // Normalize output directories and filenames
        var ir_path_buf: [512]u8 = undefined;
        var obj_path_buf: [512]u8 = undefined;
        var exe_path_buf: [512]u8 = undefined;

        // Determine executable path (add .exe on Windows)
        const raw_exe = blk2: {
            if (std.fs.path.isAbsolute(artifact_stem)) break :blk2 artifact_stem;
            // Check if it's a relative path starting with ./ or ../
            if (std.mem.startsWith(u8, artifact_stem, "./") or std.mem.startsWith(u8, artifact_stem, "../")) {
                break :blk2 artifact_stem;
            }
            // default to out/<stem>
            break :blk2 try std.fmt.bufPrint(&exe_path_buf, "out/{s}", .{artifact_stem});
        };
        const exe_path = blk3: {
            if (builtin.os.tag == .windows) {
                if (std.mem.endsWith(u8, raw_exe, ".exe")) break :blk3 raw_exe;
                var tmp: [520]u8 = undefined;
                break :blk3 try std.fmt.bufPrint(&tmp, "{s}.exe", .{raw_exe});
            }
            break :blk3 raw_exe;
        };

        // Ensure parent dir exists
        if (std.fs.path.dirname(exe_path)) |dir| {
            std.fs.cwd().makePath(dir) catch {};
        }

        // Derive IR/Object paths - always put intermediate files in out/ directory
        const stem_for_derivatives = blk4: {
            // Extract just the filename (without path) for intermediate files
            var filename_start: usize = 0;
            for (exe_path, 0..) |c, i| {
                if (c == '/' or c == '\\') filename_start = i + 1;
            }
            const filename = exe_path[filename_start..];
            if (builtin.os.tag == .windows and std.mem.endsWith(u8, filename, ".exe"))
                break :blk4 filename[0 .. filename.len - 4];
            break :blk4 filename;
        };
        const ir_path = try std.fmt.bufPrint(&ir_path_buf, "out/{s}.ll", .{stem_for_derivatives});
        {
            var printer = @import("./codegen/llvmir/ir_printer.zig").IRPrinter.init(memoryManager.getExecutionAllocator());
            try printer.emitToFile(&hir_program_for_bytecode, ir_path);
        }

        const obj_path = try std.fmt.bufPrint(&obj_path_buf, "out/{s}.o", .{stem_for_derivatives});
        {
            var args = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
            defer args.deinit();
            const ir_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.ll", .{stem_for_derivatives});
            defer std.heap.page_allocator.free(ir_filename);
            const obj_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.o", .{stem_for_derivatives});
            defer std.heap.page_allocator.free(obj_filename);
            try args.appendSlice(&[_][]const u8{ "zig", "cc", "-c", ir_filename, "-o", obj_filename });
            if (cli_options.target_arch != null or cli_options.target_os != null or cli_options.target_abi != null) {
                try args.append("-target");
                const arch = cli_options.target_arch orelse "";
                const os = cli_options.target_os orelse "";
                const abi = cli_options.target_abi orelse "";
                var triple_buf: [128]u8 = undefined;
                const triple = try std.fmt.bufPrint(&triple_buf, "{s}{s}{s}{s}{s}", .{
                    arch,
                    if (os.len > 0) "-" else "",
                    os,
                    if (abi.len > 0) "-" else "",
                    abi,
                });
                try args.append(triple);
            }
            const olvl = cli_options.opt_level;
            const oflag = if (olvl <= -1) "-O0" else if (olvl >= 3) "-O3" else switch (olvl) {
                0 => "-O0",
                1 => "-O1",
                2 => "-O2",
                else => "-O0",
            };
            try args.append(oflag);
            var child = std.process.Child.init(args.items, std.heap.page_allocator);
            child.cwd = "out";
            child.stdout_behavior = .Inherit;
            child.stderr_behavior = .Inherit;
            const term = try child.spawnAndWait();
            switch (term) {
                .Exited => |code| if (code != 0) return error.Unexpected,
                else => return error.Unexpected,
            }
        }

        // Link final executable with runtime helpers

        // Build runtime print object (relocatable) to avoid archive format issues
        var rt_obj_buf: [256]u8 = undefined;
        const rt_obj = try std.fmt.bufPrint(&rt_obj_buf, "out/doxa_rt.o", .{});
        {
            std.fs.cwd().makeDir("out") catch {};
            const emit_flag = try std.fmt.allocPrint(std.heap.page_allocator, "-femit-bin={s}", .{rt_obj});
            defer std.heap.page_allocator.free(emit_flag);

            // Prepare zig build-obj arguments (+ optional target)
            var args_list = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
            defer args_list.deinit();
            try args_list.appendSlice(&[_][]const u8{ "zig", "build-obj", "src/runtime/doxa_rt.zig", emit_flag });
            if (cli_options.target_arch != null or cli_options.target_os != null or cli_options.target_abi != null) {
                try args_list.append("-target");
                const arch = cli_options.target_arch orelse "";
                const os = cli_options.target_os orelse "";
                const abi = cli_options.target_abi orelse "";
                var triple_buf: [128]u8 = undefined;
                const triple = try std.fmt.bufPrint(&triple_buf, "{s}{s}{s}{s}{s}", .{
                    arch,
                    if (os.len > 0) "-" else "",
                    os,
                    if (abi.len > 0) "-" else "",
                    abi,
                });
                try args_list.append(triple);
            }

            var child_rt = std.process.Child.init(args_list.items, std.heap.page_allocator);
            child_rt.cwd = ".";
            child_rt.stdout_behavior = .Inherit;
            child_rt.stderr_behavior = .Inherit;
            const term = try child_rt.spawnAndWait();
            switch (term) {
                .Exited => |code| if (code != 0) {
                    std.debug.print("runtime build failed\n", .{});
                    return;
                },
                else => {
                    std.debug.print("runtime build failed\n", .{});
                    return;
                },
            }
        }

        // Link using zig cc (+ static runtime lib)
        {
            // Prepare zig cc arguments (+ optional target)
            var args_ln = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
            defer args_ln.deinit();
            try args_ln.append("zig");
            try args_ln.append("cc");
            try args_ln.append(obj_path);
            try args_ln.append(rt_obj);
            try args_ln.append("-o");
            try args_ln.append(exe_path);
            if (builtin.os.tag == .windows) {
                // Ensure the resulting binary is a console subsystem app so stdout is visible
                if (builtin.abi == .msvc) {
                    try args_ln.append("-Wl,/SUBSYSTEM:CONSOLE");
                } else {
                    try args_ln.append("-Wl,--subsystem,console");
                }
            }
            if (cli_options.target_arch != null or cli_options.target_os != null or cli_options.target_abi != null) {
                try args_ln.append("-target");
                const arch = cli_options.target_arch orelse "";
                const os = cli_options.target_os orelse "";
                const abi = cli_options.target_abi orelse "";
                var triple_buf2: [128]u8 = undefined;
                const triple2 = try std.fmt.bufPrint(&triple_buf2, "{s}{s}{s}{s}{s}", .{
                    arch,
                    if (os.len > 0) "-" else "",
                    os,
                    if (abi.len > 0) "-" else "",
                    abi,
                });
                try args_ln.append(triple2);
            }

            var child_ln = std.process.Child.init(args_ln.items, std.heap.page_allocator);
            child_ln.cwd = ".";
            child_ln.stdout_behavior = .Inherit;
            child_ln.stderr_behavior = .Inherit;
            const term2 = try child_ln.spawnAndWait();
            switch (term2) {
                .Exited => |code| if (code != 0) {
                    std.debug.print("link failed\n", .{});
                    return;
                },
                else => {
                    std.debug.print("link failed\n", .{});
                    return;
                },
            }
        }

        // Move PDB file from root to out/ directory if it exists
        const pdb_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.pdb", .{stem_for_derivatives});
        defer std.heap.page_allocator.free(pdb_filename);
        const pdb_out_path = try std.fmt.allocPrint(std.heap.page_allocator, "out/{s}.pdb", .{stem_for_derivatives});
        defer std.heap.page_allocator.free(pdb_out_path);

        // Try to move the PDB file from root to out/
        std.fs.cwd().rename(pdb_filename, pdb_out_path) catch |err| switch (err) {
            error.FileNotFound => {
                // PDB file doesn't exist, that's fine
            },
            else => {
                // Some other error occurred, but we'll continue
            },
        };

        profiler.stopPhase();
        try profiler.dump();
    }
}
