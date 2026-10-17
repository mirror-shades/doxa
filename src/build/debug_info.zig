//! Debug information beside the linked binary: the split-off `<binary>.debug`
//! on ELF, and the `.dSYM` a compiled Apple binary carries.

const std = @import("std");
const Build = @import("context.zig").Build;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const debug_split = @import("../debug_split.zig");
const paths = @import("paths.zig");

pub fn write(build: *const Build) !void {
    const io = build.io;
    const allocator = build.allocator;

    // An ELF binary's debug information goes beside it, in `<binary>.debug`,
    // as Windows keeps a `.pdb` (`debug_split.zig`). A build without it
    // leaves no stale one.
    if (build.target.usesElf()) {
        const debug_path = try std.mem.concat(allocator, u8, &.{ build.exe, ".debug" });
        defer allocator.free(debug_path);
        if (build.config.debug_info) {
            build.profiler.begin("split-debug");
            defer build.profiler.end();
            try splitDebugInfo(io, allocator, build.reporter, build.exe, debug_path);
        } else try paths.removeIfExists(io, debug_path);
    }

    // An Apple binary points at the cache's objects for its DWARF. A compiled
    // one, which leaves this machine, also gets a `.dSYM` beside it when
    // `dsymutil` is installed; the runtime's backtrace still reads the objects
    // (Zig's reader has no `.dSYM` support). A `doxa run` binary never leaves
    // the cache, so it needs none.
    if (build.target.isDarwin()) {
        const dsym_path = try std.mem.concat(allocator, u8, &.{ build.exe, ".dSYM" });
        defer allocator.free(dsym_path);
        try std.Io.Dir.cwd().deleteTree(io, dsym_path);
        if (build.config.debug_info and build.config.mode == .compile) {
            build.profiler.begin("dsym");
            defer build.profiler.end();
            _ = try makeDsym(io, build.reporter, build.cache_dir, build.exe, dsym_path);
        }
    }
}

/// Write `<exe>.dSYM` with `dsymutil`: the program's DWARF gathered from the
/// cache's objects, which lldb, Instruments and crash reporters find by UUID
/// away from this machine. False when `dsymutil` is not installed (it ships
/// with Xcode's command-line tools), and the build goes on without one.
fn makeDsym(io: std.Io, reporter: *Reporter, cache_dir: []const u8, exe: []const u8, dsym_path: []const u8) !bool {
    var child = std.process.spawn(io, .{
        .argv = &.{ "dsymutil", exe, "-o", dsym_path },
        .cwd = .{ .path = cache_dir },
        .stdout = .inherit,
        .stderr = .inherit,
    }) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    switch (try child.wait(io)) {
        .exited => |code| if (code != 0) {
            reporter.reportBuildStepFailure("writing the .dSYM", code);
            return error.Unexpected;
        },
        else => {
            reporter.reportBuildStepFailure("writing the .dSYM", null);
            return error.Unexpected;
        },
    }
    return true;
}

/// Move `exe` to `debug_path` and write the binary to ship, without its debug
/// sections, in its place. Each file is whole before it takes its name.
fn splitDebugInfo(io: std.Io, allocator: std.mem.Allocator, reporter: *Reporter, exe: []const u8, debug_path: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    const linked = try cwd.readFileAlloc(io, exe, allocator, .unlimited);
    defer allocator.free(linked);
    const stripped = debug_split.strip(allocator, linked, std.fs.path.basename(debug_path), std.hash.crc.Crc32.hash(linked)) catch |err| switch (err) {
        // TODO: a big-endian target keeps its debug information inside.
        error.UnsupportedElf => return,
        error.MalformedElf => {
            reporter.reportInternal("the linker wrote an ELF file the debug-information split cannot read. This is a compiler bug, not an error in the program", .{}, @src());
            return error.Unexpected;
        },
        error.OutOfMemory => |e| return e,
    };
    defer allocator.free(stripped);

    const staged = try std.mem.concat(allocator, u8, &.{ exe, ".stripped" });
    defer allocator.free(staged);
    {
        const original = try cwd.openFile(io, exe, .{});
        defer original.close(io);
        const permissions = (try original.stat(io)).permissions;
        const file = try cwd.createFile(io, staged, .{ .permissions = permissions });
        defer file.close(io);
        var buffer: [64 * 1024]u8 = undefined;
        var writer = file.writer(io, &buffer);
        try writer.interface.writeAll(stripped);
        try writer.end();
    }
    try std.Io.Dir.rename(cwd, exe, cwd, debug_path, io);
    try std.Io.Dir.rename(cwd, staged, cwd, exe, io);
}
