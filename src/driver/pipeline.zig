//! The compilation pipeline: lex, parse, analyze, lower, build, and (for
//! `doxa run`) execute.

const std = @import("std");
const constants = @import("../common/constants.zig");
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Profiler = @import("../utils/profiler.zig").Profiler;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const Parser = @import("../parser/parser_types.zig").Parser;
const AST = @import("../ast/ast.zig");
const Token = @import("../types/token.zig").Token;
const ModuleLoader = @import("../module/loader.zig").ModuleLoader;
const module_graph = @import("../module/graph.zig");
const install = @import("../install.zig");
const options_mod = @import("../cli/options.zig");
const Options = options_mod.Options;
const TargetTriple = @import("../build/target.zig").TargetTriple;
const build_paths = @import("../build/paths.zig");
const build_context = @import("../build/context.zig");
const build_native = @import("../build/native.zig");
const lower = @import("lower.zig");
const diagnostics = @import("diagnostics.zig");
const execute = @import("execute.zig");

pub const Pipeline = struct {
    io: std.Io,
    environ_map: *const std.process.Environ.Map,
    allocator: std.mem.Allocator,
    options: *const Options,
    script_path: []const u8,
    memory_manager: *MemoryManager,
    reporter: *Reporter,
    profiler: *Profiler,
    source: []const u8,

    pub fn run(self: *Pipeline) !void {
        const io = self.io;
        const allocator = self.allocator;
        const options = self.options;
        const memory_manager = self.memory_manager;
        const reporter = self.reporter;
        const profiler = self.profiler;
        const script_path = self.script_path;
        const source = self.source;

        profiler.begin("compile");

        profiler.begin("lex");
        const lexedTokens = lexicAnalysis(io, memory_manager, source, script_path, reporter) catch |err| {
            if (!reporter.hasCompileErrors()) {
                diagnostics.reportPhaseFailure(reporter, "lexer", err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };
        defer lexedTokens.deinit();
        if (options.reporter_options.debug_lexer) {
            for (lexedTokens.items) |token| {
                std.debug.print("{t} {s}\n", .{ token.type, token.lexeme });
            }
        }
        profiler.end();

        profiler.begin("parse");
        var graph_store = buildModuleGraph(io, allocator, options, script_path) catch |err| {
            if (!reporter.hasCompileErrors()) {
                diagnostics.reportPhaseFailure(reporter, "resolve", err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };
        defer graph_store.deinit();
        graph_store.profiler = profiler;

        var parser = Parser.init(io, memory_manager.getAnalysisAllocator(), lexedTokens.items, script_path, try reporter.ensureFileUri(io, script_path), reporter);
        const parsedStatements = parser.execute() catch |err| {
            if (!reporter.hasCompileErrors()) {
                parser.reportParseError(err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };

        // The entry file is record 0, registered before any import resolves, so
        // a module that imports it reaches this parse instead of a second one.
        var loader = ModuleLoader.init(io, memory_manager.getAnalysisAllocator(), reporter, &graph_store);
        const entry_record = loader.registerEntry(script_path, source, parsedStatements) catch |err| {
            if (!reporter.hasCompileErrors()) {
                diagnostics.reportPhaseFailure(reporter, "resolve", err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };
        profiler.end();
        diagnostics.exitIfCompileErrors(reporter);

        if (options.reporter_options.debug_parser) {
            var ast_dump = std.Io.Writer.Allocating.init(memory_manager.getAnalysisAllocator());
            defer ast_dump.deinit();
            AST.dumpStatements(&ast_dump.writer, parsedStatements) catch {};
            reporter.report(.Debug, .Hint, null, "AST", "{s}", .{ast_dump.written()});
        }

        profiler.begin("semantic");
        var semantic_analyzer = SemanticAnalyzer.init(memory_manager.getAnalysisAllocator(), reporter, memory_manager, &loader, entry_record.id, parser.entry_point_name);
        defer semantic_analyzer.deinit();
        semantic_analyzer.analyzeProgram() catch |err| {
            if (!reporter.hasCompileErrors()) {
                diagnostics.reportPhaseFailure(reporter, "semantic", err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };
        profiler.end();
        diagnostics.exitIfCompileErrors(reporter);

        if (options.reporter_options.debug_memory) {
            memory_manager.dumpState(reporter);
        }

        profiler.begin("hir");

        const module = lower.lowerProgram(memory_manager, &semantic_analyzer, reporter, profiler) catch |err| {
            if (!reporter.hasCompileErrors()) {
                diagnostics.reportPhaseFailure(reporter, "hir", err);
            }
            diagnostics.exitIfCompileErrors(reporter);
            return err;
        };
        diagnostics.exitIfCompileErrors(reporter);
        profiler.end();

        profiler.begin("native");
        var target = TargetTriple.resolve(allocator, options.build.target_arch, options.build.target_os, options.build.target_abi) catch |err| switch (err) {
            error.MissingTargetOs => {
                std.debug.print("Error: cross-compiling requires --os=<os>; a target without an OS would silently fall back to the host\n", .{});
                std.process.exit(constants.EXIT_CODE_USAGE);
            },
            error.MissingTargetArch => {
                std.debug.print("Error: cross-compiling requires --arch=<arch>; a target without an arch is not a valid triple\n", .{});
                std.process.exit(constants.EXIT_CODE_USAGE);
            },
            else => return err,
        };
        defer target.deinit(allocator);
        const exe_path = try build_paths.nativeOutputPath(allocator, options.build.mode, options.build.output_path, options.build.cache_dir, script_path, target.isWindows());
        defer allocator.free(exe_path);

        var backend = try build_context.Build.init(io, allocator, memory_manager, &options.build, reporter, profiler, exe_path, target);
        defer backend.deinit();
        try build_native.compile(&backend, &semantic_analyzer, &module);
        profiler.end();

        profiler.end();

        if (options.build.mode == .run) {
            profiler.begin("execute");
            const code = try execute.runExecutable(io, allocator, self.environ_map, exe_path, options.program_args);
            profiler.end();

            if (options.reporter_options.debug_memory) {
                memory_manager.dumpState(reporter);
            }
            try execute.finishProfile(profiler, options.profile_out);
            if (code != 0) std.process.exit(code);
            return;
        }

        try execute.finishProfile(profiler, options.profile_out);
    }
};

fn lexicAnalysis(io: std.Io, memoryManager: *MemoryManager, source: []const u8, path: []const u8, reporter: *Reporter) !std.array_list.Managed(Token) {
    var lexer = try LexicalAnalyzer.init(io, memoryManager.getAnalysisAllocator(), source, path, reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    return tokens;
}

/// The installed standard library and `--include=` directories become the
/// graph's roots alongside the entry file's own directory.
fn buildModuleGraph(io: std.Io, allocator: std.mem.Allocator, options: *const Options, script_path: []const u8) !module_graph.ModuleGraph {
    const std_dir = try install.stdDir(io, allocator);
    defer allocator.free(std_dir);
    return module_graph.ModuleGraph.initForEntry(io, allocator, script_path, std_dir, options.build.include_dirs.items);
}
