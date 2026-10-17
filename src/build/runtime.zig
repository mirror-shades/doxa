//! The program-independent root+runtime object: read the runtime source
//! closure once, hash it into a cache key, stage it beside the generated Zig
//! entry root, and compile it only on a cache miss.

const std = @import("std");
const Build = @import("context.zig").Build;
const install = @import("../install.zig");
const instrument = @import("../runtime/instrument.zig");
const hashing = @import("../utils/hashing.zig");
const paths = @import("paths.zig");

const MAX_RUNTIME_SOURCE_BYTES = 4 * 1024 * 1024;

pub fn compile(build: *const Build) ![]u8 {
    build.profiler.begin("runtime");
    defer build.profiler.end();

    const io = build.io;
    const allocator = build.allocator;
    const runtime_src_dir = try install.runtimeSourceDir(io, allocator);
    defer allocator.free(runtime_src_dir);
    const runtime_sources = try readRuntimeSources(io, allocator, runtime_src_dir);
    defer freeRuntimeSources(allocator, runtime_sources);
    const root_source = try buildRootSource(allocator, build.config.instrument);
    defer allocator.free(root_source);

    const key = runtimeObjectKey(
        runtime_sources,
        root_source,
        build.toolchain,
        build.target_arg,
        build.target.cpuKey(build.native_cpu_key.bytes()),
        build.config.opt.zigFlag(),
        build.include_dirs,
    );
    if (build.artifacts.contains(key, build.obj_ext)) return build.artifacts.pathAlloc(key, build.obj_ext);

    build.profiler.begin("runtime-stage");
    const runtime_dst = try stageRuntimeSources(io, allocator, runtime_sources, build.cache_dir);
    defer allocator.free(runtime_dst);
    const root_path = try std.fmt.allocPrint(allocator, "{s}/__doxa_main.zig", .{build.cache_dir});
    defer allocator.free(root_path);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = root_path, .data = root_source });
    build.profiler.end();

    build.profiler.begin("runtime-build");
    defer build.profiler.end();
    const staged = try build.artifacts.stagingPathAlloc(key, build.obj_ext);
    defer allocator.free(staged);
    var args = std.array_list.Managed([]const u8).init(allocator);
    defer args.deinit();
    const emit_flag = try std.fmt.allocPrint(allocator, "-femit-bin={s}", .{staged});
    defer allocator.free(emit_flag);
    try args.appendSlice(&[_][]const u8{ build.zig_exe_path, "build-obj", root_path, emit_flag });
    try build.target.appendTargetArg(&args);
    try build.target.appendCpuArg(&args);
    try args.append(build.config.opt.zigFlag());
    try args.append("-lc");
    var include_flags = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (include_flags.items) |f| allocator.free(f);
        include_flags.deinit();
    }
    for (build.include_dirs) |dir| {
        const flag = try std.fmt.allocPrint(allocator, "-I{s}", .{dir});
        try include_flags.append(flag);
        try args.append(flag);
    }
    var child = try std.process.spawn(io, .{
        .argv = args.items,
        .cwd = .{ .path = build.cache_dir },
        .stdout = .inherit,
        .stderr = .inherit,
    });
    const term = try child.wait(io);
    switch (term) {
        .exited => |code| if (code != 0) {
            build.reporter.reportBuildStepFailure("compiling the runtime object", code);
            return error.Unexpected;
        },
        else => {
            build.reporter.reportBuildStepFailure("compiling the runtime object", null);
            return error.Unexpected;
        },
    }
    return build.artifacts.publish(key, build.obj_ext);
}

// A single runtime `.zig` source held in memory. The closure is read once so
// it can be hashed (for the object cache key) and staged without a second pass.
const RuntimeSource = struct {
    name: []const u8,
    content: []const u8,
};

fn lessRuntimeSource(_: void, a: RuntimeSource, b: RuntimeSource) bool {
    return std.mem.lessThan(u8, a.name, b.name);
}

// Read the runtime `.zig` sources into memory, sorted by name so the cache key
// is independent of directory iteration order.
fn readRuntimeSources(io: std.Io, allocator: std.mem.Allocator, src_dir: []const u8) ![]RuntimeSource {
    var src = try paths.openDirMaybeAbs(io, src_dir, .{ .iterate = true });
    defer src.close(io);

    var list = std.array_list.Managed(RuntimeSource).init(allocator);
    errdefer {
        for (list.items) |s| {
            allocator.free(s.name);
            allocator.free(s.content);
        }
        list.deinit();
    }

    var it = src.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".zig")) continue;
        const content = try src.readFileAlloc(io, entry.name, allocator, .limited(MAX_RUNTIME_SOURCE_BYTES));
        errdefer allocator.free(content);
        const name = try allocator.dupe(u8, entry.name);
        try list.append(.{ .name = name, .content = content });
    }

    std.mem.sort(RuntimeSource, list.items, {}, lessRuntimeSource);
    return list.toOwnedSlice();
}

fn freeRuntimeSources(allocator: std.mem.Allocator, sources: []RuntimeSource) void {
    for (sources) |s| {
        allocator.free(s.name);
        allocator.free(s.content);
    }
    allocator.free(sources);
}

// Write the runtime sources into `<cache_dir>/runtime/` so the generated root
// can `@import("runtime/doxa_rt.zig")` (Zig forbids imports outside the root
// file's directory). Returns the owned destination directory path.
fn stageRuntimeSources(io: std.Io, allocator: std.mem.Allocator, sources: []const RuntimeSource, cache_dir: []const u8) ![]u8 {
    const dst_dir = try std.fs.path.join(allocator, &.{ cache_dir, "runtime" });
    errdefer allocator.free(dst_dir);
    try std.Io.Dir.cwd().createDirPath(io, dst_dir);
    for (sources) |s| {
        const path = try std.fs.path.join(allocator, &.{ dst_dir, s.name });
        defer allocator.free(path);
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = s.content });
    }
    return dst_dir;
}

// The generated Zig root that owns the platform entry point and hands off to
// the Doxa program. Its content is constant for a given compiler build; hashing
// it into the object key keeps entry-glue changes from reusing stale objects.
// An instrumented build declares its members here, which makes each set its
// own runtime object; an uninstrumented root is unchanged.
fn buildRootSource(allocator: std.mem.Allocator, members: instrument.Members) ![]u8 {
    var content = std.array_list.Managed(u8).init(allocator);
    errdefer content.deinit();
    try content.appendSlice("const std = @import(\"std\");\n");
    try content.appendSlice("const doxa_rt = @import(\"runtime/doxa_rt.zig\");\n");

    try content.appendSlice("\nextern fn doxa_program_main() callconv(.c) void;\n");
    if (members.any()) {
        try content.print("\npub const doxa_instrument: doxa_rt.instrument.Members = .{{ .calls = {}, .lines = {}, .memtrace = {}, .memcheck = {} }};\n", .{ members.calls, members.lines, members.memtrace, members.memcheck });
    }
    try content.appendSlice("\npub fn main(init: std.process.Init) void {\n");
    if (members.any()) try content.appendSlice("    defer doxa_rt.instrument.flush();\n");
    try content.appendSlice("    const args = init.minimal.args.toSlice(init.arena.allocator()) catch {\n");
    try content.appendSlice("        doxa_program_main();\n");
    try content.appendSlice("        return;\n");
    try content.appendSlice("    };\n");
    try content.appendSlice("    const argv = init.arena.allocator().alloc([*:0]u8, args.len) catch {\n");
    try content.appendSlice("        doxa_program_main();\n");
    try content.appendSlice("        return;\n");
    try content.appendSlice("    };\n");
    try content.appendSlice("    for (args, argv) |arg, *slot| slot.* = @constCast(arg.ptr);\n");
    try content.appendSlice("    doxa_rt.doxa_set_args(@as(i32, @intCast(args.len)), argv.ptr);\n");
    try content.appendSlice("    doxa_rt.doxa_set_environ(init.environ_map);\n");
    try content.appendSlice("    doxa_program_main();\n");
    try content.appendSlice("}\n");

    return content.toOwnedSlice();
}

// Cache key for the compiled root+runtime object. Everything that can change
// the object participates: toolchain, target, optimization, the runtime source
// closure, and the generated root source.
pub fn runtimeObjectKey(
    sources: []const RuntimeSource,
    root_source: []const u8,
    toolchain: []const u8,
    target_arg: []const u8,
    cpu_key: []const u8,
    opt_flag: []const u8,
    include_dirs: []const []const u8,
) hashing.Digest {
    var kb = hashing.KeyBuilder.init("doxa-runtime-object-v2");
    kb.addBytes(toolchain);
    kb.addBytes(target_arg);
    kb.addBytes(cpu_key);
    kb.addBytes(opt_flag);
    kb.addBytes(root_source);
    for (include_dirs) |dir| kb.addBytes(dir);
    for (sources) |s| kb.addSource(.{ .name = s.name, .content = s.content });
    return kb.finish();
}
