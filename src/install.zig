//! The install layout: where the files a `doxa` executable ships with live,
//! relative to the executable itself (`<prefix>/bin/doxa`).
//!
//!   <prefix>/lib/std          the standard library — the `std` module root
//!   <prefix>/lib/runtime      the runtime sources compiled into every program
//!   <prefix>/lib/zig/zig      the bundled Zig toolchain
//!
//! Every path the compiler derives from its own location is derived here.

const std = @import("std");
const builtin = @import("builtin");

pub const Error = error{ ExecutableDirUnknown, OutOfMemory };

/// The standard library: the `std` root, and the directory `@std()` names.
pub fn stdDir(io: std.Io, allocator: std.mem.Allocator) Error![]u8 {
    return underPrefix(io, allocator, &.{ "lib", "std" });
}

/// The bundled Zig toolchain's executable. Whether it exists is the caller's
/// to check.
pub fn zigExecutable(io: std.Io, allocator: std.mem.Allocator) Error![]u8 {
    const name = if (builtin.os.tag == .windows) "zig.exe" else "zig";
    return underPrefix(io, allocator, &.{ "lib", "zig", name });
}

/// The directory holding the runtime sources (`doxa_rt.zig` and its
/// self-contained siblings). An install without them is broken, not a layout
/// to search past.
pub fn runtimeSourceDir(io: std.Io, allocator: std.mem.Allocator) (Error || error{RuntimeSourceNotFound})![]u8 {
    const dir = try underPrefix(io, allocator, &.{ "lib", "runtime" });
    errdefer allocator.free(dir);
    if (!dirContainsFile(io, allocator, dir, "doxa_rt.zig")) return error.RuntimeSourceNotFound;
    return dir;
}

/// `<prefix>/<parts...>`, where `<prefix>` is the executable's directory's
/// parent.
fn underPrefix(io: std.Io, allocator: std.mem.Allocator, parts: []const []const u8) Error![]u8 {
    const exe_dir = try exeDir(io, allocator);
    defer allocator.free(exe_dir);
    var full = std.array_list.Managed([]const u8).init(allocator);
    defer full.deinit();
    try full.appendSlice(&.{ exe_dir, ".." });
    try full.appendSlice(parts);
    return std.fs.path.resolve(allocator, full.items);
}

fn exeDir(io: std.Io, allocator: std.mem.Allocator) Error![]u8 {
    return std.process.executableDirPathAlloc(io, allocator) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.ExecutableDirUnknown,
    };
}

fn dirContainsFile(io: std.Io, allocator: std.mem.Allocator, dir: []const u8, name: []const u8) bool {
    const joined = std.fs.path.join(allocator, &.{ dir, name }) catch return false;
    defer allocator.free(joined);
    const file = (if (std.fs.path.isAbsolute(joined))
        std.Io.Dir.openFileAbsolute(io, joined, .{})
    else
        std.Io.Dir.cwd().openFile(io, joined, .{})) catch return false;
    file.close(io);
    return true;
}
