//! What to build: the build-relevant half of the command line, owned by the
//! caller that parsed it. The build backend imports this, never the CLI.

const std = @import("std");
const constants = @import("../common/constants.zig");
const instrument = @import("../runtime/instrument.zig");

pub const Mode = enum {
    run,
    compile,
};

// Optimization is described on two axes that govern different parts of the
// toolchain:
//
//   * `mode` — the zig backend release mode for the runtime and the final
//     `zig build-exe` link (`Debug`, `ReleaseSafe`, `ReleaseFast`,
//     `ReleaseSmall`). `safe` keeps runtime safety checks on; `fast` turns
//     them off.
//   * `level` — the clang `-O` level applied to the generated `.ll` when it is
//     compiled to an object. This governs the program's own code, so the
//     numeric `-O` / `--opt=` flags mirror clang's levels directly.
//
// The named `--opt-mode=` presets tie the axes together the way zig's C code
// does (`safe` == `-O2`, `fast` == `-O3`, `small` == `-Oz`); the numeric form
// picks a clang level and the zig mode that suits it.

pub const OptLevel = enum {
    o0,
    o1,
    o2,
    o3,
    oz,

    fn clangFlag(self: OptLevel) []const u8 {
        return switch (self) {
            .o0 => "-O0",
            .o1 => "-O1",
            .o2 => "-O2",
            .o3 => "-O3",
            .oz => "-Oz",
        };
    }
};

pub const OptMode = enum {
    debug,
    safe,
    fast,
    small,

    // `zig build-exe` / `zig build-obj` optimization mode flag. Debug is the
    // default and passed explicitly for clarity.
    fn zigFlag(self: OptMode) []const u8 {
        return switch (self) {
            .debug => "-ODebug",
            .safe => "-OReleaseSafe",
            .fast => "-OReleaseFast",
            .small => "-OReleaseSmall",
        };
    }
};

pub const Opt = struct {
    mode: OptMode,
    level: OptLevel,

    // clang `cc` optimization flag for the `.ll` -> `.o` object step.
    pub fn clangFlag(self: Opt) []const u8 {
        return self.level.clangFlag();
    }

    // zig optimization mode flag, forwarded to the runtime/link steps.
    pub fn zigFlag(self: Opt) []const u8 {
        return self.mode.zigFlag();
    }

    pub fn fromMode(text: []const u8) ?Opt {
        return if (std.mem.eql(u8, text, "debug"))
            .{ .mode = .debug, .level = .o0 }
        else if (std.mem.eql(u8, text, "safe"))
            .{ .mode = .safe, .level = .o2 }
        else if (std.mem.eql(u8, text, "fast"))
            .{ .mode = .fast, .level = .o3 }
        else if (std.mem.eql(u8, text, "small"))
            .{ .mode = .small, .level = .oz }
        else
            null;
    }

    // Numeric levels mirror clang: `-O2` is clang `-O2`, matching a C build
    // with `zig cc -O2`. The runtime and link step switch to the unchecked
    // `fast` mode at `-O2` and up so a release Doxa binary stays comparable to
    // its C counterpart; `-O1` is a checked, lightly-optimized release.
    pub fn fromLevel(level: i32) Opt {
        return if (level <= 0)
            .{ .mode = .debug, .level = .o0 }
        else switch (level) {
            1 => .{ .mode = .safe, .level = .o1 },
            2 => .{ .mode = .fast, .level = .o2 },
            else => .{ .mode = .fast, .level = .o3 }, // 3+
        };
    }
};

pub const Config = struct {
    mode: Mode,
    cache_dir: []const u8,
    /// Whether `cache_dir` was allocated for this config (the default is a
    /// static string and must not be freed).
    cache_dir_owned: bool = false,
    output_path: ?[]const u8 = null,
    target_arch: ?[]const u8 = null,
    target_os: ?[]const u8 = null,
    target_abi: ?[]const u8 = null,
    opt: Opt,
    // Native linking directives forwarded to the final `zig build-exe` link
    // step (and the object `cc` step for includes). Each `--link/--libdir/
    // --framework/--include` occurrence appends one entry.
    link_libs: std.array_list.Managed([]const u8),
    lib_dirs: std.array_list.Managed([]const u8),
    frameworks: std.array_list.Managed([]const u8),
    include_dirs: std.array_list.Managed([]const u8),
    emit_opt_ir: bool = false,
    emit_asm: bool = false,
    emit_hir: bool = false,
    /// Debug metadata in the program's object, for an external debugger, a
    /// sampling profiler and a fault's backtrace: on unless `--strip`. It
    /// costs no run time (the code is the same) and no measurable compile
    /// time (`plan/diagnostics-and-observability.md`, step 15).
    debug_info: bool = true,
    /// `--instrument=`: counters the program keeps and writes at exit.
    instrument: instrument.Members = .{},
    /// `--emit-complexity`: write `<binary>.complexity`, the static findings
    /// `doxa report` renders.
    emit_complexity: bool = false,
    /// `--emit-memory`: write `<binary>.memory`, where each allocation lives
    /// and every copy the compiler placed, for `doxa report`.
    emit_memory: bool = false,

    pub fn init(allocator: std.mem.Allocator, mode: Mode) Config {
        return .{
            .mode = mode,
            .cache_dir = constants.DEFAULT_CACHE_DIR,
            .opt = Opt.fromLevel(0),
            .link_libs = std.array_list.Managed([]const u8).init(allocator),
            .lib_dirs = std.array_list.Managed([]const u8).init(allocator),
            .frameworks = std.array_list.Managed([]const u8).init(allocator),
            .include_dirs = std.array_list.Managed([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *const Config, allocator: std.mem.Allocator) void {
        if (self.cache_dir_owned) allocator.free(self.cache_dir);
        if (self.output_path) |p| allocator.free(p);
        if (self.target_arch) |p| allocator.free(p);
        if (self.target_os) |p| allocator.free(p);
        if (self.target_abi) |p| allocator.free(p);
        for (self.link_libs.items) |p| allocator.free(p);
        for (self.lib_dirs.items) |p| allocator.free(p);
        for (self.frameworks.items) |p| allocator.free(p);
        for (self.include_dirs.items) |p| allocator.free(p);
        @constCast(&self.link_libs).deinit();
        @constCast(&self.lib_dirs).deinit();
        @constCast(&self.frameworks).deinit();
        @constCast(&self.include_dirs).deinit();
    }
};
