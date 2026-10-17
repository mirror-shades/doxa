//! The bundled Zig toolchain: locating its executable, naming it in artifact
//! keys, and resolving the host CPU a native build targets.

const std = @import("std");
const install = @import("../install.zig");
const hashing = @import("../utils/hashing.zig");
const constants = @import("../common/constants.zig");

pub fn resolveBundledZigExecutable(io: std.Io, allocator: std.mem.Allocator) ![]u8 {
    const zig_path = try install.zigExecutable(io, allocator);
    errdefer allocator.free(zig_path);

    const file = std.Io.Dir.openFileAbsolute(io, zig_path, .{}) catch |err| {
        std.debug.print(
            "Error: bundled Zig not found at '{s}' ({s})\n",
            .{ zig_path, @errorName(err) },
        );
        std.process.exit(constants.EXIT_CODE_USAGE);
    };
    defer file.close(io);

    return zig_path;
}

// Identity of the bundled toolchain, part of every artifact cache key: an
// artifact must be invalidated when the compiler that produced it changes.
// `zig version` is authoritative and cheap (~20ms); a failure degrades to a
// constant that disables cross-toolchain reuse rather than aborting the build.
pub fn toolchainIdentity(io: std.Io, allocator: std.mem.Allocator, zig_exe_path: []const u8) ![]u8 {
    const result = std.process.run(allocator, io, .{ .argv = &.{ zig_exe_path, "version" } }) catch {
        return allocator.dupe(u8, "unknown");
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| if (code != 0) return allocator.dupe(u8, "unknown"),
        else => return allocator.dupe(u8, "unknown"),
    }
    return allocator.dupe(u8, std.mem.trim(u8, result.stdout, " \t\r\n"));
}

/// A CPU identity, resolved from the running host, that participates in every
/// native artifact cache key.
pub const NativeCpuKey = struct {
    digest: hashing.Digest,
    /// True when host detection failed and the key is the fixed fallback. The
    /// key is still stable, so the cache stays correct on that host, but any
    /// prior native object must be presumed unsafe to reuse — an unknown CPU
    /// cannot be shown to match a known one.
    known: bool,

    /// This key's bytes. Takes the key by reference: the slice points into the
    /// key itself, so a by-value receiver would return a pointer into a dead
    /// copy and the key would vary with the stack.
    pub fn bytes(self: *const NativeCpuKey) []const u8 {
        return &self.digest;
    }
};

/// Resolve the CPU a native build targets.
///
/// This resolves the *running* host, which is deliberately not `builtin.cpu`:
/// `builtin` describes the machine the compiler was built for, and the two
/// differ exactly when a binary is built on one machine and run on another —
/// a cached CI artifact, or a developer on a different box. Keying on
/// `builtin` would reproduce the very bug this replaces.
///
/// `std.zig.system.resolveTargetQuery` is what the bundled Zig itself runs for
/// `-mcpu=native`, so this names the same CPU the object will be built for.
/// The feature set is folded in alongside the model name because Zig adds
/// features from the OS version (AVX on Windows 11, AVX-512 on the newest
/// Linux) that the name alone does not carry, and two hosts differing only
/// there must not share an object. It is hashed as raw bits rather than as
/// feature names: the set is a bitset, so its bytes are exactly its identity.
pub fn resolveNativeCpuKey(io: std.Io) NativeCpuKey {
    const query: std.Target.Query = .{ .cpu_model = .native };
    const resolved = std.zig.system.resolveTargetQuery(io, query) catch {
        // Detection can fail on a locked-down or exotic host. Rather than
        // widen the key to "anything", fall back to a distinct constant: every
        // host that cannot detect lands on the same key and still reuses
        // safely among themselves, while a host that can detect never shares
        // with one that could not.
        var kb = hashing.KeyBuilder.init("doxa-native-cpu-v1");
        kb.addBytes("native-undetected");
        return .{ .digest = kb.finish(), .known = false };
    };

    var kb = hashing.KeyBuilder.init("doxa-native-cpu-v1");
    kb.addBytes(@tagName(resolved.cpu.arch));
    kb.addBytes(resolved.cpu.model.name);
    kb.addBytes(std.mem.sliceAsBytes(&resolved.cpu.features.ints));
    return .{ .digest = kb.finish(), .known = true };
}
