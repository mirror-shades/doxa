//! The resolved codegen target for a build: arch/os/abi from the command
//! line, defaulting to the host when absent. This single resolution is
//! threaded through every toolchain step (`zig cc`, `build-obj`,
//! `build-exe`) and output naming so the whole pipeline agrees on one triple.

const std = @import("std");
const builtin = @import("builtin");

pub const TargetTriple = struct {
    /// Owned "arch-os-abi" triple (components omitted when empty); empty when
    /// compiling for the host.
    triple: []const u8,
    /// Effective target OS name: the requested OS when cross-compiling,
    /// otherwise the host OS. Drives target-true naming decisions (`.exe`
    /// suffix, object extension, platform link libs).
    os: []const u8,

    /// Resolve the command line's arch/os/abi flags. Explicitly naming any
    /// component while cross-compiling requires an arch and an OS: missing
    /// components would otherwise be silent host defaults, which are never
    /// correct for a cross build (an arch-less `-target linux` is not even a
    /// valid triple).
    pub fn resolve(
        allocator: std.mem.Allocator,
        arch: ?[]const u8,
        os: ?[]const u8,
        abi: ?[]const u8,
    ) !TargetTriple {
        const arch_text = arch orelse "";
        const os_text = os orelse "";
        const abi_text = abi orelse "";
        const is_cross = arch_text.len > 0 or os_text.len > 0 or abi_text.len > 0;
        if (is_cross and os_text.len == 0) return error.MissingTargetOs;
        if (is_cross and arch_text.len == 0) return error.MissingTargetArch;
        const effective_os = if (os_text.len > 0)
            os_text
        else switch (builtin.os.tag) {
            .windows => "windows",
            .linux => "linux",
            .macos => "macos",
            else => "",
        };
        const triple = try std.fmt.allocPrint(allocator, "{s}{s}{s}{s}{s}", .{
            arch_text,
            if (os_text.len > 0) "-" else "",
            os_text,
            if (abi_text.len > 0) "-" else "",
            abi_text,
        });
        errdefer allocator.free(triple);
        return .{ .triple = triple, .os = try allocator.dupe(u8, effective_os) };
    }

    pub fn deinit(self: *TargetTriple, allocator: std.mem.Allocator) void {
        allocator.free(self.triple);
        allocator.free(self.os);
    }

    pub fn isCross(self: TargetTriple) bool {
        return self.triple.len > 0;
    }

    pub fn isWindows(self: TargetTriple) bool {
        return std.mem.eql(u8, self.os, "windows");
    }

    /// A target whose binaries are ELF: neither Windows (PE) nor an Apple OS
    /// (Mach-O).
    pub fn usesElf(self: TargetTriple) bool {
        return !self.isWindows() and !self.isDarwin();
    }

    /// An Apple OS, whose ABI requires a frame record in every function.
    pub fn isDarwin(self: TargetTriple) bool {
        for ([_][]const u8{ "macos", "ios", "tvos", "watchos", "visionos" }) |os| {
            if (std.mem.eql(u8, self.os, os)) return true;
        }
        return false;
    }

    /// Append `-target <triple>` to `args`. Native builds still pass an
    /// explicit host triple so the bundled Zig resolves its own C runtime
    /// objects instead of the host's; some host `crt1.o` files carry
    /// relocations the bundled linker cannot handle.
    pub fn appendTargetArg(self: TargetTriple, args: *std.array_list.Managed([]const u8)) !void {
        if (self.isCross()) {
            try args.append("-target");
            try args.append(self.triple);
            return;
        }
        try args.append("-target");
        try args.append(std.fmt.comptimePrint("{s}-{s}-{s}", .{
            @tagName(builtin.target.cpu.arch),
            @tagName(builtin.target.os.tag),
            @tagName(builtin.target.abi),
        }));
    }

    /// CPU features for a *native* build. `zig cc` / `zig build-obj` stop
    /// auto-detecting the host CPU the moment an explicit `-target` is given
    /// (which the native path must pass so the bundled Zig resolves its own C
    /// runtime objects), so a native build has to ask for the host CPU
    /// explicitly. Without this, native Doxa binaries are baseline x86-64
    /// while a plain `zig cc` C twin gets the host's AVX2 — a target-feature
    /// mismatch that biases the benchmark suite against Doxa. Cross builds
    /// name their target's baseline; the CLI has no per-target CPU knob yet.
    pub fn appendCpuArg(self: TargetTriple, args: *std.array_list.Managed([]const u8)) !void {
        if (!self.isCross()) try args.append("-mcpu=native");
    }

    /// The CPU component of an artifact cache key.
    ///
    /// A native build compiles for the *running* host's CPU, so the key must
    /// name that CPU. The constant `"native"` is not a key: CI restores
    /// `.zig-cache` across runs and its runners are not CPU-homogeneous, so a
    /// cache populated on one machine would be served to another and the
    /// program would execute instructions that machine lacks — a trap in the
    /// middle of unrelated code. `nativeCpuKey` resolves the real model
    /// instead. A cross build compiles for its target's baseline, which the
    /// toolchain and triple already capture, so it contributes nothing.
    pub fn cpuKey(self: TargetTriple, native_cpu_key: []const u8) []const u8 {
        return if (self.isCross()) "" else native_cpu_key;
    }

    /// The string passed to `-target`, resolved the same way for native and
    /// cross builds. Used verbatim in cache keys so an artifact never outlives
    /// the target it was built for.
    pub fn targetArgAlloc(self: TargetTriple, allocator: std.mem.Allocator) ![]u8 {
        if (self.isCross()) return allocator.dupe(u8, self.triple);
        return std.fmt.allocPrint(allocator, "{s}-{s}-{s}", .{
            @tagName(builtin.target.cpu.arch),
            @tagName(builtin.target.os.tag),
            @tagName(builtin.target.abi),
        });
    }
};
