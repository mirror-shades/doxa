//! The command driver: dispatch a parsed command, own the compilation's
//! allocators and reporter, and run the pipeline.

const std = @import("std");
const constants = @import("../common/constants.zig");
const platform = @import("../utils/platform.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const Profiler = @import("../utils/profiler.zig").Profiler;
const CountingAllocator = @import("../utils/profiler.zig").CountingAllocator;
const source_cache = @import("../utils/source_cache.zig");
const LspServer = @import("../lsp/server.zig");
const LspHarness = @import("../lsp/harness.zig");
const report = @import("../instrument/report.zig");
const cli = @import("../cli/parse.zig");
const cli_init = @import("../cli/init.zig");
const Options = @import("../cli/options.zig").Options;
const source = @import("source.zig");
const Pipeline = @import("pipeline.zig").Pipeline;

pub fn run(init: std.process.Init) !void {
    platform.enableUtf8Console();

    var command = cli.parse(init) catch |err| switch (err) {
        error.Help => std.process.exit(0),
        error.Usage => std.process.exit(constants.EXIT_CODE_USAGE),
        else => return err,
    };
    defer command.deinit(init.gpa);

    switch (command) {
        .init => |name| std.process.exit(runInit(init.io, name)),
        .report => |args| std.process.exit(report.command(init.io, init.arena.allocator(), args, constants.DEFAULT_CACHE_DIR)),
        .lsp_stdio => |opts| std.process.exit(try LspServer.run(init.io, init.gpa, .{
            .reporter_options = opts.reporter_options,
            .trace_io = opts.trace_io,
        })),
        .lsp_harness => |opts| return LspHarness.run(init.io, init.gpa, .{
            .reporter_options = opts.reporter_options,
            .script_path = opts.script_path,
        }),
        .build => |*b| try build(init, &b.options),
    }
}

fn runInit(io: std.Io, project_name: ?[]const u8) u8 {
    return cli_init.run(io, project_name) catch |err| {
        std.debug.print("Error: could not initialize project: {s}\n", .{@errorName(err)});
        return constants.EXIT_CODE_USAGE;
    };
}

fn build(init: std.process.Init, options: *const Options) !void {
    // `--profile` counts every byte the compiler draws from here on. Without
    // it no counter exists, so an ordinary compile pays nothing.
    var counter = CountingAllocator.init(init.gpa);
    const gpa = if (options.profile) counter.allocator() else init.gpa;

    var memoryManager = try MemoryManager.init(gpa);
    defer memoryManager.deinit();

    var sourceCache = source_cache.SourceCache.init(gpa);
    defer sourceCache.deinit();

    var reporter = Reporter.init(init.io, gpa, .{}, &sourceCache);
    defer reporter.deinit();
    reporter.detectAnsi(init.environ_map);

    const script_path = options.script_path orelse unreachable;

    // The profiler keeps its own tree on the uncounted allocator.
    var profiler = Profiler.init(init.io, init.gpa, options.profile);
    defer profiler.deinit();
    if (options.profile) profiler.attachCounter(&counter);

    const text = try source.read(init.io, memoryManager.getAnalysisAllocator(), gpa, script_path);
    defer memoryManager.getAnalysisAllocator().free(text);
    try sourceCache.load(script_path, text);
    source.ensureDoxa(script_path, try reporter.ensureFileUri(init.io, script_path), &reporter);

    var pipeline = Pipeline{
        .io = init.io,
        .environ_map = init.environ_map,
        .allocator = gpa,
        .options = options,
        .script_path = script_path,
        .memory_manager = &memoryManager,
        .reporter = &reporter,
        .profiler = &profiler,
        .source = text,
    };
    pipeline.run() catch |err| {
        // Compile-time diagnostics exit inside the pipeline; reaching here
        // means an unexpected failure that would otherwise print a Zig stack
        // trace.
        if (!reporter.hasCompileErrors()) {
            std.debug.print("Doxa: compilation failed: {s}\n", .{@errorName(err)});
        }
        std.process.exit(constants.EXIT_CODE_USAGE);
    };
}
