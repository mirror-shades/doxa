//! The Doxa runtime's public surface: the C ABI the emitted program links
//! against.
//!
//! This file is the root of the runtime object and the one import the generated
//! entry point names. The implementation lives in its siblings: `abi.zig` owns
//! the data layouts, and each subsystem owns its exported functions. The
//! `comptime` block at the bottom puts every sibling in the module graph, so
//! its exports are roots of the runtime object.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const fault = @import("fault.zig");
const numbers = @import("numbers.zig");

pub const instrument = @import("instrument.zig");
pub const FaultKind = fault.FaultKind;
pub const DoxaFaultSite = fault.DoxaFaultSite;

const abi = @import("abi.zig");
pub const DoxaString = abi.DoxaString;
pub const DoxaPeekInfo = abi.DoxaPeekInfo;
pub const DoxaValue = abi.DoxaValue;
pub const DoxaTag = abi.DoxaTag;
pub const DoxaBoxMeta = abi.DoxaBoxMeta;
pub const BoxDesc = abi.BoxDesc;
pub const ArrayHeader = abi.ArrayHeader;
pub const StructDesc = abi.StructDesc;
pub const StructHeader = abi.StructHeader;
pub const EnumDesc = abi.EnumDesc;

/// The number parsers are shared with the compiler's constant evaluator, so a
/// folded conversion and a run-time one cannot disagree.
pub const parseIntText = numbers.parseIntText;
pub const parseFloatText = numbers.parseFloatText;
pub const parseByteText = numbers.parseByteText;
pub const truncateToInt = numbers.truncateToInt;

const Scope = scope_arena.Scope;

/// The program root scope (`@root`), where globals live. `main` takes it once;
/// it is never exited.
pub export fn doxa_scope_root() callconv(.c) *Scope {
    return scope_arena.root();
}

/// Open a child of `parent`: `scope.enter` of the register HIR.
pub export fn doxa_scope_enter(parent: *Scope) callconv(.c) *Scope {
    return scope_arena.enter(parent);
}

/// Close `scope`, freeing its arena in O(1): `scope.exit`.
pub export fn doxa_scope_exit(scope: *Scope) callconv(.c) void {
    scope_arena.exit(scope);
}

/// Reclaim `scope`'s allocations and keep it open for reuse: `scope.reset`,
/// a loop body's arena at each iteration.
pub export fn doxa_scope_reset(scope: *Scope) callconv(.c) void {
    scope_arena.reset(scope);
}

/// Allocate `size` bytes in `scope`.
pub export fn doxa_scope_alloc(scope: *Scope, size: i64, alignment: i64) callconv(.c) *anyopaque {
    const align_val: u29 = if (alignment > 0) @intCast(alignment) else @alignOf(u64);
    return @ptrCast(scope_arena.alloc(scope, @intCast(size), .fromByteUnits(align_val), @returnAddress()));
}

var startup_argc: i32 = 0;
var startup_argv: ?[*][*:0]u8 = null;
var startup_environ: ?*const std.process.Environ.Map = null;

pub export fn doxa_set_args(argc: i32, argv: ?[*][*:0]u8) callconv(.c) void {
    startup_argc = argc;
    startup_argv = argv;
}

pub export fn doxa_argc() callconv(.c) i32 {
    return startup_argc;
}

pub export fn doxa_argv(index: i32) callconv(.c) ?[*:0]const u8 {
    if (index < 0 or index >= startup_argc) return null;
    const argv = startup_argv orelse return null;
    return argv[@intCast(index)];
}

pub export fn doxa_set_environ(env: ?*const std.process.Environ.Map) callconv(.c) void {
    startup_environ = env;
}

pub export fn doxa_environ() callconv(.c) ?*const std.process.Environ.Map {
    return startup_environ;
}

pub export fn doxa_getenv(name_ptr: ?[*]const u8, name_len: u64, out_len: *u64) callconv(.c) ?[*]const u8 {
    const env = startup_environ orelse return null;
    const name = if (name_ptr) |p| p[0..@intCast(name_len)] else return null;
    const value = env.get(name) orelse return null;
    out_len.* = value.len;
    return value.ptr;
}

/// Terminate the process with the given status code. Declared `noreturn` so the
/// optimizer treats every `@exit` site as a genuine control-flow end.
pub export fn doxa_exit(code: i64) callconv(.c) noreturn {
    instrument.flush();
    std.process.exit(@intCast(code));
}

/// Under memcheck: fault at `site` when the program reads `ptr` and it lies
/// in memory an arena reclaimed.
fn memcheckRead(ptr: ?*const anyopaque, site: *const DoxaFaultSite) callconv(.c) void {
    const p = ptr orelse return;
    if (scope_arena.isReclaimed(p)) fault.doxa_fault(site, null, 0, 0, 0);
}

comptime {
    if (instrument.memcheck) @export(&memcheckRead, .{ .name = "doxa_memcheck" });
}

// Every implementation file is a direct sibling because `src/build/runtime.zig`
// stages the directory non-recursively, and Zig analyzes a file's exported
// functions only when the file is reached.
comptime {
    _ = @import("io.zig");
    _ = @import("numbers.zig");
    _ = @import("strings.zig");
    _ = @import("values.zig");
    _ = @import("arrays.zig");
    _ = @import("structs.zig");
    _ = @import("clone.zig");
    _ = @import("print.zig");
}
