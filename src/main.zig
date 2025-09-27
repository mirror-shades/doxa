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
const HIRProgram = @import("./codegen/hir/soxa_types.zig").HIRProgram;
const VM = @import("./interpreter/vm.zig").VM;
const ConstantFolder = @import("./parser/constant_folder.zig").ConstantFolder;
const PeepholeOptimizer = @import("./codegen/hir/peephole.zig").PeepholeOptimizer;
const Errors = @import("./utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ProfilerImport = @import("./utils/profiler.zig");
const Phase = ProfilerImport.Phase;
const Profiler = ProfilerImport.Profiler;
const BytecodeGenerator = @import("./codegen/bytecode/generator.zig").BytecodeGenerator;
const BytecodeWriter = @import("./codegen/bytecode/writer.zig");
const BytecodeModule = @import("./codegen/bytecode/module.zig").BytecodeModule;
const StructMethodInfo = @import("./analysis/semantic/semantic.zig").StructMethodInfo;

const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_FILE_SIZE = 1024 * 1024;
const DOXA_EXTENSION = ".doxa";
const DEFAULT_OUTPUT_FILE = "output.o";

var compile_file: bool = false;
var is_safe_repl: bool = false;
var output_file: ?[]const u8 = null;

const CLI = struct {
    reporter_options: ReporterOptions,
    script_path: ?[]const u8,
    profile: bool,
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
        error.PathAlreadyExists => {}, // Directory already exists, that's fine
        else => return err, // Other errors should be propagated
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
    var constant_folder = ConstantFolder.init(memoryManager.getAllocator());
    var folded_statements = std.array_list.Managed(ast.Stmt).init(memoryManager.getAllocator());
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

    try vm.run();
}

fn parseArgs(allocator: std.mem.Allocator) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
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
        .script_path = null,
        .profile = false,
    };

    var i: usize = 1;

    while (i < args.len) : (i += 1) {
        const arg: [:0]u8 = args[i];

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
        } else if (stringEquals(arg, "--help") or stringEquals(arg, "-h")) {
            printUsage();
            std.process.exit(0);
        } else if (stringEndsWith(arg, DOXA_EXTENSION)) {
            options.script_path = try allocator.dupe(u8, arg);
            i += 1;
            break;
        } else {
            std.debug.print("Error: Unknown command or invalid file: '{s}'\n", .{arg});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
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
    var lexer = LexicalAnalyzer.init(memoryManager.getAllocator(), source, path, reporter);
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

    var reporter = Reporter.init(gpa.allocator(), cli_options.reporter_options);
    defer reporter.deinit();

    defer if (cli_options.script_path) |path| {
        gpa.allocator().free(path);
    };

    var memoryManager = try MemoryManager.init(gpa.allocator());
    defer memoryManager.deinit();

    var profiler = Profiler.init(gpa.allocator(), cli_options.profile);
    defer profiler.deinit();

    const path = cli_options.script_path.?; // Safe to unwrap since we validated it

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

    const source = try std.fs.cwd().readFileAlloc(memoryManager.getAllocator(), path, MAX_FILE_SIZE);
    defer memoryManager.getAllocator().free(source);

    profiler.startPhase(Phase.LEXIC_A);
    const lexedTokens = try lexicAnalysis(&memoryManager, source, path, &reporter);
    defer lexedTokens.deinit();
    profiler.stopPhase();

    profiler.startPhase(Phase.PARSING);
    var parser = Parser.init(memoryManager.getAllocator(), lexedTokens.items, path, &reporter);
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
    defer memoryManager.getAllocator().free(soxa_path);

    const hir_program_for_bytecode = try generateHIRProgram(&memoryManager, parsedStatements, &parser, &semantic_analyzer, &reporter);
    profiler.stopPhase();

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

    var bytecode_generator = BytecodeGenerator.init(memoryManager.getAllocator(), "out", artifact_stem);
    var bytecode_module = try bytecode_generator.generate(&hir_program);
    defer bytecode_module.deinit();

    if (bytecode_module.artifact_path) |bc_path| {
        try writeBytecodeArtifact(&bytecode_module, bc_path);
    }
    profiler.stopPhase();

    profiler.startPhase(Phase.EXECUTION);

    try runBytecodeModule(&memoryManager, &bytecode_module, &reporter);

    profiler.stopPhase();
    try profiler.dump();
}
