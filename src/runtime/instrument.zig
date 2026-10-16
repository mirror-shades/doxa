//! The instrumented runtime (`--instrument=`, `plan/diagnostics-and-observability.md`
//! step 4). An instrumented build's generated root declares `doxa_instrument`,
//! the members it was built with. That declaration is part of the root source,
//! so each set of members is its own runtime object, and an uninstrumented
//! runtime carries none of this.
//!
//! The counters live in the program's IR, numbered by the sites the emitter
//! placed; `doxa_program_main` hands them over with `doxa_instrument_register`.
//! At exit `flush` writes them, raw, to `<binary stem>.profdata` beside the
//! binary, where the compiler wrote the matching `.profmap`.

const std = @import("std");
const builtin = @import("builtin");
const root = @import("root");
const scope_arena = @import("scope_arena.zig");
const fault = @import("fault.zig");

/// What `--instrument=` asks for. The compiler parses the flag into this, and
/// the root carries it to the runtime.
pub const Members = struct {
    calls: bool = false,
    lines: bool = false,
    memtrace: bool = false,
    memcheck: bool = false,

    pub fn any(self: Members) bool {
        return self.calls or self.lines or self.memtrace or self.memcheck;
    }
};

pub const enabled = @hasDecl(root, "doxa_instrument");
/// `--instrument=memtrace`: the arenas count what is allocated in them, by
/// the site the program last named (`doxa_memtrace_site`).
pub const memtrace = if (enabled) root.doxa_instrument.memtrace else false;
/// `--instrument=memcheck`: a reclaimed arena's memory is poisoned and kept,
/// never reused, and the program's reads of heap values check that what they
/// read is not in it (`scope_arena.isReclaimed`).
pub const memcheck = if (enabled) root.doxa_instrument.memcheck else false;

/// The first line of each side file. A reader rejects any other.
pub const profmap_header = "doxa-profmap 1";
pub const profdata_header = "doxa-profdata 1";

/// The build a `.profmap` describes: a hash of its site lines. The emitter
/// bakes it into the program, and the program copies it into `.profdata`, so
/// a report never pairs counts with another build's sites.
pub fn buildHash(sites: []const u8) u64 {
    return std.hash.Wyhash.hash(0, sites);
}

var counts: []const i64 = &.{};
var build_hash: u64 = 0;
var flushed = false;

pub fn register(ptr: [*]const i64, len: u64, hash: u64) callconv(.c) void {
    counts = ptr[0..@intCast(len)];
    build_hash = hash;
    if (memtrace) {
        site_stats = std.heap.page_allocator.alloc(SiteStats, @intCast(len)) catch fault.outOfMemory();
        @memset(site_stats, .{});
    }
}

comptime {
    if (enabled) @export(&register, .{ .name = "doxa_instrument_register" });
    if (memtrace) @export(&doxa_memtrace_site, .{ .name = "doxa_memtrace_site" });
}

// ── memtrace ──

/// The site whose work is running: the emitter stores it before each
/// instruction that allocates, stores into a container, opens an arena or
/// re-homes a value. What the runtime allocates for it, nested allocations
/// included (a deep clone's), is counted to it. A single program thread
/// writes it (TODO: per-thread with threads, as the counters).
var doxa_memtrace_site: u32 = no_site;
const no_site = std.math.maxInt(u32);

/// What a site did, by `.profmap` site id. An allocating site fills the first
/// four; a `rehome` also `copied`/`kept`; an `arena` site (a `scope_enter`)
/// the last four, for every arena it opened.
pub const SiteStats = struct {
    allocs: u64 = 0,
    /// Bytes the program asked for.
    bytes: u64 = 0,
    /// Bytes of this site's still held by open arenas, and the most ever.
    live: u64 = 0,
    peak_live: u64 = 0,
    copied: u64 = 0,
    kept: u64 = 0,
    entries: u64 = 0,
    resets: u64 = 0,
    /// The most bytes one of its arenas held from the system (its capacity),
    /// and the bytes the program had asked of them when they were reclaimed.
    peak_held: u64 = 0,
    released: u64 = 0,
    /// Of `bytes`, those asked of the root arena: what this site made the
    /// root grow by, never reclaimed (retention, step 13).
    rooted: u64 = 0,
};

var site_stats: []SiteStats = &.{};
/// Work done before the program registered its sites, or for none.
var unattributed: SiteStats = .{};

fn current() *SiteStats {
    const id = doxa_memtrace_site;
    return if (id < site_stats.len) &site_stats[id] else &unattributed;
}

/// What an arena carries in a memtrace build: the site that opened it, and
/// the bytes each site has allocated in it since it was last reclaimed.
pub const ArenaTrace = struct {
    origin: u32 = no_site,
    by_site: std.AutoHashMapUnmanaged(u32, u64) = .empty,
    /// The root arena: what is allocated in it is counted as `rooted`.
    root: bool = false,

    /// A fresh arena, opened by the site now running.
    pub fn open(self: *ArenaTrace, is_root: bool) void {
        self.root = is_root;
        self.origin = doxa_memtrace_site;
        self.by_site.clearRetainingCapacity();
        if (self.origin < site_stats.len) site_stats[self.origin].entries += 1;
    }

    /// `bytes` allocated in this arena by the site now running: a new
    /// allocation, or an existing one grown in place.
    pub fn allocated(self: *ArenaTrace, bytes: usize, how: enum { new, grown }) void {
        const id = doxa_memtrace_site;
        const stats = current();
        if (how == .new) stats.allocs += 1;
        stats.bytes += bytes;
        if (self.root) stats.rooted += bytes;
        stats.live += bytes;
        stats.peak_live = @max(stats.peak_live, stats.live);
        const entry = self.by_site.getOrPut(std.heap.page_allocator, id) catch fault.outOfMemory();
        entry.value_ptr.* = if (entry.found_existing) entry.value_ptr.* + bytes else bytes;
    }

    /// An allocation of the site now running shrank in place by `bytes`: the
    /// program asked for less than it was first given.
    pub fn shrunk(self: *ArenaTrace, bytes: usize) void {
        const stats = current();
        stats.bytes -= bytes;
        if (self.root) stats.rooted -= bytes;
        stats.live -= bytes;
        if (self.by_site.getPtr(doxa_memtrace_site)) |held| held.* -= bytes;
    }

    /// The arena gives back everything in it; it held `capacity` bytes.
    pub fn reclaimed(self: *ArenaTrace, capacity: usize, how: enum { reset, exit }) void {
        var released: u64 = 0;
        var it = self.by_site.iterator();
        while (it.next()) |e| {
            const stats = if (e.key_ptr.* < site_stats.len) &site_stats[e.key_ptr.*] else &unattributed;
            stats.live -= e.value_ptr.*;
            released += e.value_ptr.*;
        }
        self.by_site.clearRetainingCapacity();
        if (self.origin >= site_stats.len) return;
        const arena = &site_stats[self.origin];
        if (how == .reset) arena.resets += 1;
        arena.peak_held = @max(arena.peak_held, capacity);
        arena.released += released;
    }
};

/// A `rehome` at the running site kept the value itself, or copied it.
pub fn rehomed(copied: bool) void {
    if (!memtrace) return;
    const stats = current();
    if (copied) stats.copied += 1 else stats.kept += 1;
}

/// Write the counters, once, on whichever exit comes first: the entry's
/// return, `@exit`, or a fault.
pub fn flush() void {
    if (!enabled or flushed) return;
    flushed = true;
    const io = std.Io.Threaded.global_single_threaded.io();
    const alloc = std.heap.page_allocator;
    const path = profdataPath(io, alloc) catch |err| return warn("cannot find the binary's path", err);
    var text: std.Io.Writer.Allocating = .init(alloc);
    text.writer.print("{s}\nhash {x:0>16}\n", .{ profdata_header, build_hash }) catch |err| return warn("out of memory", err);
    writeCounts(&text.writer) catch |err| return warn("out of memory", err);
    std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = text.written() }) catch |err| return warn(path, err);
}

/// One line per site: its counter, then, under memtrace, its `SiteStats` in
/// field order. Then, under memtrace, the work no site claimed and the bytes
/// the root arena holds at exit (what globals retain).
fn writeCounts(w: *std.Io.Writer) !void {
    for (counts, 0..) |count, i| {
        try w.print("{d}", .{count});
        if (memtrace) try writeStats(w, site_stats[i]);
        try w.writeAll("\n");
    }
    if (memtrace) {
        try w.writeAll("unattributed\t0");
        try writeStats(w, unattributed);
        try w.print("\nroot\t{d}\n", .{scope_arena.rootCapacity()});
    }
}

fn writeStats(w: *std.Io.Writer, stats: SiteStats) !void {
    inline for (std.meta.fields(SiteStats)) |field| try w.print(" {d}", .{@field(stats, field.name)});
}

/// `<binary stem>.profdata`, beside the running binary.
fn profdataPath(io: std.Io, alloc: std.mem.Allocator) ![]u8 {
    const exe = try std.process.executablePathAlloc(io, alloc);
    const stem = if (builtin.os.tag == .windows and std.ascii.endsWithIgnoreCase(exe, ".exe")) exe[0 .. exe.len - 4] else exe;
    return std.mem.concat(alloc, u8, &.{ stem, ".profdata" });
}

fn warn(what: []const u8, err: anyerror) void {
    std.debug.print("warning: --instrument counts not written ({s}: {s})\n", .{ what, @errorName(err) });
}
