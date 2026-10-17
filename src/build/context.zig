//! The native build context: every path, directory, toolchain identity and
//! cache handle one build needs, resolved once and shared by its phases.

const std = @import("std");
const ArtifactCache = @import("../utils/artifact_cache.zig").ArtifactCache;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const Profiler = @import("../utils/profiler.zig").Profiler;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const config_mod = @import("config.zig");
const Config = config_mod.Config;
const TargetTriple = @import("target.zig").TargetTriple;
const toolchain = @import("toolchain.zig");
const NativeCpuKey = toolchain.NativeCpuKey;
const paths = @import("paths.zig");

pub const Build = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    memory_manager: *MemoryManager,
    config: *const Config,
    reporter: *Reporter,
    profiler: *Profiler,
    target: TargetTriple,

    /// Owns every resolved path below; freed last.
    paths_arena: std.heap.ArenaAllocator,
    invocation_dir: []const u8,
    cache_dir: []const u8,
    exe: []const u8,
    /// The basename of `exe` without `.exe`, for cache artifacts (`<stem>.ll`).
    stem: []const u8,
    /// The absolute path of `exe` without `.exe`, for side files that live
    /// beside the binary (`<exe_stem>.profmap`).
    exe_stem: []const u8,
    ir_path: []const u8,
    include_dirs: []const []const u8,
    lib_dirs: []const []const u8,

    zig_exe_path: []u8,
    toolchain: []u8,
    target_arg: []u8,
    native_cpu_key: NativeCpuKey,
    artifacts: ArtifactCache,
    obj_ext: []const u8,

    pub fn init(
        io: std.Io,
        allocator: std.mem.Allocator,
        memory_manager: *MemoryManager,
        config: *const Config,
        reporter: *Reporter,
        profiler: *Profiler,
        exe_path: []const u8,
        target: TargetTriple,
    ) !Build {
        profiler.begin("resolve-zig");
        const zig_exe_path = try toolchain.resolveBundledZigExecutable(io, allocator);
        profiler.end();
        errdefer allocator.free(zig_exe_path);

        profiler.begin("dirs");
        const cwd = std.Io.Dir.cwd();
        cwd.createDir(io, config.cache_dir, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };
        if (std.fs.path.dirname(exe_path)) |dir| {
            try cwd.createDirPath(io, dir);
        }
        profiler.end();

        var build: Build = .{
            .io = io,
            .allocator = allocator,
            .memory_manager = memory_manager,
            .config = config,
            .reporter = reporter,
            .profiler = profiler,
            .target = target,
            .paths_arena = std.heap.ArenaAllocator.init(allocator),
            .invocation_dir = undefined,
            .cache_dir = undefined,
            .exe = undefined,
            .stem = undefined,
            .exe_stem = undefined,
            .ir_path = undefined,
            .include_dirs = undefined,
            .lib_dirs = undefined,
            .zig_exe_path = zig_exe_path,
            .toolchain = undefined,
            .target_arg = undefined,
            .native_cpu_key = undefined,
            .artifacts = undefined,
            .obj_ext = undefined,
        };
        errdefer build.paths_arena.deinit();

        const arena = build.paths_arena.allocator();
        build.invocation_dir = try cwd.realPathFileAlloc(io, ".", arena);
        build.cache_dir = try std.fs.path.resolve(arena, &.{ build.invocation_dir, config.cache_dir });
        build.exe = try std.fs.path.resolve(arena, &.{ build.invocation_dir, exe_path });
        build.include_dirs = try paths.resolveAll(arena, build.invocation_dir, config.include_dirs.items);
        build.lib_dirs = try paths.resolveAll(arena, build.invocation_dir, config.lib_dirs.items);

        build.exe_stem = if (target.isWindows() and std.mem.endsWith(u8, build.exe, ".exe"))
            build.exe[0 .. build.exe.len - 4]
        else
            build.exe;
        build.stem = std.fs.path.basename(build.exe_stem);
        build.ir_path = try std.fmt.allocPrint(arena, "{s}/{s}.ll", .{ build.cache_dir, build.stem });

        // Every artifact key includes the producing toolchain, so a swapped-in
        // Zig cannot resurrect objects built by a different compiler.
        profiler.begin("toolchain");
        build.toolchain = try toolchain.toolchainIdentity(io, allocator, zig_exe_path);
        profiler.end();
        errdefer allocator.free(build.toolchain);

        // Resolved once and shared by every key: a native build compiles for
        // the host CPU, so an object may only be reused on a host that would
        // produce the same one.
        build.native_cpu_key = toolchain.resolveNativeCpuKey(io);
        build.target_arg = try target.targetArgAlloc(allocator);
        errdefer allocator.free(build.target_arg);
        build.obj_ext = if (target.isWindows()) "obj" else "o";

        const artifacts_dir = try std.fs.path.join(allocator, &.{ build.cache_dir, "artifacts" });
        defer allocator.free(artifacts_dir);
        build.artifacts = try ArtifactCache.init(io, allocator, artifacts_dir);
        errdefer build.artifacts.deinit();

        return build;
    }

    pub fn deinit(self: *Build) void {
        self.artifacts.deinit();
        self.allocator.free(self.target_arg);
        self.allocator.free(self.toolchain);
        self.allocator.free(self.zig_exe_path);
        self.paths_arena.deinit();
    }
};
