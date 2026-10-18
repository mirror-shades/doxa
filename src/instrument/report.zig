//! `doxa report <binary>`: the one command that reads the side files a build
//! leaves beside its binary (`plan/diagnostics-and-observability.md` steps
//! 4-6). It joins the `.profmap` an instrumented build wrote with the
//! `.profdata` the binary wrote when it ran, and renders the `.complexity`
//! findings of `--emit-complexity` and the `.memory` rows of `--emit-memory`. The site table is the one the emitter
//! placed the counters from, and the findings the compiler's own records; the
//! report re-derives neither.

const std = @import("std");
const builtin = @import("builtin");
const instrument = @import("../runtime/instrument.zig");
const complexity = @import("complexity.zig");
const memory = @import("memory.zig");

pub const Site = struct {
    kind: []const u8,
    function: []const u8,
    file: []const u8,
    line: u32,
    column: u32,
    count: i64,
    /// Under `--instrument=memtrace`: what the site allocated, held and
    /// copied, or, for an `arena` site, what its arenas did.
    trace: ?instrument.SiteStats = null,
};

pub const Report = struct {
    /// The program's clang level (`-O2`).
    opt: []const u8,
    /// In site-id order.
    sites: []Site,
    /// Under memtrace: work no site claimed (the runtime's start-up), and the
    /// bytes the root arena held at exit.
    unattributed: ?instrument.SiteStats = null,
    root_held: ?u64 = null,
};

pub const Error = error{ Stale, Malformed, OutOfMemory };

/// The `.profmap` an instrumented build writes: a header naming the build
/// (the hash of its site lines) and its level, then the site lines.
pub fn writeProfmap(w: *std.Io.Writer, opt: []const u8, sites: []const u8) std.Io.Writer.Error!void {
    try w.print("{s}\nhash {x:0>16}\nopt {s}\n{s}", .{ instrument.profmap_header, instrument.buildHash(sites), opt, sites });
}

/// Join a `.profmap` and a `.profdata`. `Stale` when they describe two builds.
pub fn join(alloc: std.mem.Allocator, profmap: []const u8, profdata: []const u8) Error!Report {
    var map = std.mem.splitScalar(u8, profmap, '\n');
    if (!std.mem.eql(u8, map.next() orelse "", instrument.profmap_header)) return error.Malformed;
    const map_hash = try field(map.next(), "hash ");
    const opt = try field(map.next(), "opt ");
    const site_text = profmap[map.index orelse profmap.len ..];
    if (!std.mem.eql(u8, map_hash, &hex(instrument.buildHash(site_text)))) return error.Malformed;

    var data = std.mem.splitScalar(u8, profdata, '\n');
    if (!std.mem.eql(u8, data.next() orelse "", instrument.profdata_header)) return error.Malformed;
    if (!std.mem.eql(u8, try field(data.next(), "hash "), map_hash)) return error.Stale;

    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var lines = std.mem.splitScalar(u8, site_text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var cols = std.mem.splitScalar(u8, line, '\t');
        var site: Site = .{
            .kind = cols.next() orelse return error.Malformed,
            .function = cols.next() orelse return error.Malformed,
            .file = cols.next() orelse return error.Malformed,
            .line = try number(u32, cols.next()),
            .column = try number(u32, cols.next()),
            .count = 0,
        };
        var values = std.mem.splitScalar(u8, data.next() orelse return error.Stale, ' ');
        site.count = try number(i64, values.next());
        site.trace = try stats(&values);
        try sites.append(alloc, site);
    }
    var report: Report = .{ .opt = opt, .sites = sites.items };
    // Memtrace's totals follow the sites; any other line is another build's.
    while (data.next()) |extra| {
        if (extra.len == 0) continue;
        if (std.mem.startsWith(u8, extra, "unattributed\t0")) {
            var values = std.mem.splitScalar(u8, extra["unattributed\t0".len..], ' ');
            _ = values.next(); // the empty field before the first space
            report.unattributed = try statsRest(&values);
        } else if (std.mem.startsWith(u8, extra, "root\t")) {
            report.root_held = try number(u64, extra["root\t".len..]);
        } else return error.Stale;
    }
    return report;
}

/// The `SiteStats` after a site's count, in field order; null without
/// memtrace.
fn stats(values: *std.mem.SplitIterator(u8, .scalar)) Error!?instrument.SiteStats {
    if (values.peek() == null) return null;
    return try statsRest(values);
}

fn statsRest(values: *std.mem.SplitIterator(u8, .scalar)) Error!instrument.SiteStats {
    var out: instrument.SiteStats = .{};
    inline for (std.meta.fields(instrument.SiteStats)) |f| @field(out, f.name) = try number(u64, values.next());
    if (values.next() != null) return error.Malformed;
    return out;
}

fn field(line: ?[]const u8, comptime prefix: []const u8) Error![]const u8 {
    const text = line orelse return error.Malformed;
    if (!std.mem.startsWith(u8, text, prefix)) return error.Malformed;
    return text[prefix.len..];
}

fn number(comptime T: type, text: ?[]const u8) Error!T {
    // A site with no count: the data ran out, so it is another build's.
    const t = text orelse return error.Stale;
    if (t.len == 0) return error.Stale;
    return std.fmt.parseInt(T, t, 10) catch error.Malformed;
}

fn hex(value: u64) [16]u8 {
    var out: [16]u8 = undefined;
    _ = std.fmt.bufPrint(&out, "{x:0>16}", .{value}) catch unreachable;
    return out;
}

/// The tables, hottest first, in the `--profile` tree's style.
pub fn writeText(w: *std.Io.Writer, alloc: std.mem.Allocator, report: Report) !void {
    try writeCounts(w, alloc, report);
    try writeTrace(w, alloc, report);
}

/// Memtrace's tables: the allocating sites by bytes, the arenas by what they
/// held, and the runtime-checked copies.
fn writeTrace(w: *std.Io.Writer, alloc: std.mem.Allocator, report: Report) !void {
    var any = false;
    for (report.sites) |site| if (site.trace != null) {
        any = true;
    };
    if (!any) return;
    const order = try alloc.alloc(u32, report.sites.len);
    for (order, 0..) |*o, i| o.* = @intCast(i);

    std.mem.sort(u32, order, report.sites, moreBytes);
    try w.writeAll("\n==========================================\nAllocations by site\n==========================================\n");
    try w.writeAll("Bytes the program asked for; peak is the most this site held at once.\n\n");
    for (order) |i| {
        const s = report.sites[i];
        const t = s.trace orelse continue;
        if (eqlKind(s, "arena") or t.allocs == 0) continue;
        try w.print("  {d} B\t{d} alloc{s}\tpeak {d} B\t{s}:{d}:{d}\t{s}\t{s}\n", .{ t.bytes, t.allocs, if (t.allocs == 1) "" else "s", t.peak_live, s.file, s.line, s.column, s.function, s.kind });
    }

    std.mem.sort(u32, order, report.sites, moreHeld);
    try w.writeAll("\n==========================================\nArenas\n==========================================\n");
    try w.writeAll("Held is the most one arena took from the system; released, what the program had asked of it.\n\n");
    for (order) |i| {
        const s = report.sites[i];
        const t = s.trace orelse continue;
        if (!eqlKind(s, "arena") or t.entries == 0) continue;
        try w.print("  held {d} B\treleased {d} B\t{d} opened, {d} reset{s}\t{s}:{d}:{d}\t{s}\n", .{ t.peak_held, t.released, t.entries, t.resets, if (t.resets == 1) "" else "s", s.file, s.line, s.column, s.function });
    }
    if (report.root_held) |held| try w.print("  held {d} B\tthe root arena at exit: what globals retain\n", .{held});

    std.mem.sort(u32, order, report.sites, moreRooted);
    var grown = false;
    for (report.sites) |s| if (s.trace) |t| {
        if (t.rooted != 0) grown = true;
    };
    if (grown) {
        try w.writeAll("\n==========================================\nRoot growth by site\n==========================================\n");
        try w.writeAll("Bytes each site put into the root arena, which is never reclaimed.\n\n");
        for (order) |i| {
            const s = report.sites[i];
            const t = s.trace orelse continue;
            if (t.rooted == 0) continue;
            try w.print("  {d} B\t{s}:{d}:{d}\t{s}\t{s}\n", .{ t.rooted, s.file, s.line, s.column, s.function, s.kind });
        }
    }

    var copies = false;
    for (report.sites) |s| if (s.trace) |t| {
        if (eqlKind(s, "rehome") and t.copied + t.kept != 0) copies = true;
    };
    if (copies) {
        try w.writeAll("\n==========================================\nRuntime-checked copies\n==========================================\n");
        for (report.sites) |s| {
            const t = s.trace orelse continue;
            if (!eqlKind(s, "rehome") or t.copied + t.kept == 0) continue;
            try w.print("  {d} copied, {d} kept\t{s}:{d}:{d}\t{s}\n", .{ t.copied, t.kept, s.file, s.line, s.column, s.function });
        }
    }
    if (report.unattributed) |u| if (u.allocs != 0) {
        try w.print("\n{d} B in {d} allocations before the program's first site (the runtime's start-up).\n", .{ u.bytes, u.allocs });
    };
}

/// How often the statement at `at` (`file:line:col`) ran: the most any
/// line counter on its line counted. The join of a static finding, which
/// names a site, with an `--instrument=lines` run.
pub fn lineCount(sites: []const Site, at: []const u8) u64 {
    const colon = std.mem.lastIndexOfScalar(u8, at, ':') orelse return 0;
    const head = at[0..colon];
    const line_colon = std.mem.lastIndexOfScalar(u8, head, ':') orelse return 0;
    const file = head[0..line_colon];
    const line = std.fmt.parseInt(u32, head[line_colon + 1 ..], 10) catch return 0;
    var most: u64 = 0;
    for (sites) |s| {
        if (!std.mem.eql(u8, s.kind, "line") or s.line != line or !std.mem.eql(u8, s.file, file)) continue;
        most = @max(most, @as(u64, @intCast(@max(s.count, 0))));
    }
    return most;
}

/// Whether `sites` holds `--instrument=lines` counters.
pub fn hasLines(sites: []const Site) bool {
    for (sites) |s| if (std.mem.eql(u8, s.kind, "line")) return true;
    return false;
}

fn eqlKind(s: Site, kind: []const u8) bool {
    return std.mem.eql(u8, s.kind, kind);
}

fn moreStat(comptime stat_field: []const u8, sites: []const Site, a: u32, b: u32) bool {
    const x = if (sites[a].trace) |t| @field(t, stat_field) else 0;
    const y = if (sites[b].trace) |t| @field(t, stat_field) else 0;
    return if (x != y) x > y else a < b;
}

fn moreBytes(sites: []const Site, a: u32, b: u32) bool {
    return moreStat("bytes", sites, a, b);
}

fn moreRooted(sites: []const Site, a: u32, b: u32) bool {
    return moreStat("rooted", sites, a, b);
}

fn moreHeld(sites: []const Site, a: u32, b: u32) bool {
    return moreStat("peak_held", sites, a, b);
}

/// The counter tables (`calls`, `lines`), hottest first.
fn writeCounts(w: *std.Io.Writer, alloc: std.mem.Allocator, report: Report) !void {
    const order = try alloc.alloc(u32, report.sites.len);
    for (order, 0..) |*o, i| o.* = @intCast(i);
    std.mem.sort(u32, order, report.sites, hotter);

    try w.print("Built at {s}.", .{report.opt});
    // Step 5 item 4: an inlined callee runs no entry of its own.
    if (!std.mem.eql(u8, report.opt, "-O0")) try w.writeAll(" Inlining folds a callee's calls into its caller, so an inlined function counts fewer.");
    try w.writeAll("\n");
    for (sections) |section| {
        if (!hasKind(report.sites, section.kind)) continue;
        try w.print("\n==========================================\n{s}\n==========================================\n", .{section.title});
        for (order) |i| {
            const s = report.sites[i];
            if (!std.mem.eql(u8, s.kind, section.kind)) continue;
            // A call row leads with the function; a line row with its place.
            if (section.by_place) {
                try w.print("  {d}\t{s}:{d}:{d}\t{s}\n", .{ s.count, s.file, s.line, s.column, s.function });
            } else {
                try w.print("  {d}\t{s}\t{s}:{d}:{d}\n", .{ s.count, s.function, s.file, s.line, s.column });
            }
        }
    }
}

const Section = struct { kind: []const u8, title: []const u8, by_place: bool };
const sections = [_]Section{
    .{ .kind = "call", .title = "Calls", .by_place = false },
    .{ .kind = "line", .title = "Lines", .by_place = true },
};

fn hasKind(sites: []const Site, kind: []const u8) bool {
    for (sites) |s| if (std.mem.eql(u8, s.kind, kind)) return true;
    return false;
}

fn hotter(sites: []const Site, a: u32, b: u32) bool {
    if (sites[a].count != sites[b].count) return sites[a].count > sites[b].count;
    return a < b;
}

/// Everything a report found: the counts and the findings, whichever exist.
pub const Json = struct {
    profile: ?Report,
    complexity: ?[]const complexity.Finding,
    memory: ?[]const complexity.Finding,
};

pub fn writeJson(w: *std.Io.Writer, json: Json) !void {
    try std.json.Stringify.value(json, .{ .whitespace = .indent_2, .emit_null_optional_fields = false }, w);
    try w.writeAll("\n");
}

// ── The command ──

/// `doxa report <binary> [--json] [--cache-dir=<dir>]`. A path, or a filename
/// with an extension, names a `doxa compile` output; a bare stem a `doxa run`
/// build, in the cache.
pub fn command(io: std.Io, alloc: std.mem.Allocator, args: []const []const u8, default_cache: []const u8) u8 {
    var target: ?[]const u8 = null;
    var json = false;
    var cache_dir = default_cache;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json = true;
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            cache_dir = arg["--cache-dir=".len..];
        } else if (target == null and !std.mem.startsWith(u8, arg, "-")) {
            target = arg;
        } else {
            std.debug.print("Error: unexpected argument '{s}' (usage: doxa report <binary> [--json] [--cache-dir=<dir>])\n", .{arg});
            return 2;
        }
    }
    const given = target orelse {
        std.debug.print("Error: doxa report needs the instrumented binary (usage: doxa report <binary> [--json] [--cache-dir=<dir>])\n", .{});
        return 2;
    };
    const stem = stemOf(alloc, given, cache_dir) catch return oom();

    const map_path = std.mem.concat(alloc, u8, &.{ stem, ".profmap" }) catch return oom();
    const data_path = std.mem.concat(alloc, u8, &.{ stem, ".profdata" }) catch return oom();
    const findings_path = std.mem.concat(alloc, u8, &.{ stem, ".complexity" }) catch return oom();
    const memory_path = std.mem.concat(alloc, u8, &.{ stem, ".memory" }) catch return oom();

    // Counts, when the build was instrumented.
    const profile: ?Report = if (readIfPresent(io, alloc, map_path) orelse return 1) |profmap| blk: {
        const profdata = (readIfPresent(io, alloc, data_path) orelse return 1) orelse {
            std.debug.print("Error: {s} has no {s} yet; it is written when the program runs to its end, @exit or a fault\n", .{ map_path, data_path });
            return 1;
        };
        break :blk join(alloc, profmap, profdata) catch |err| {
            switch (err) {
                error.Stale => std.debug.print("Error: {s} and {s} describe different builds; run the program again\n", .{ map_path, data_path }),
                error.Malformed => std.debug.print("Error: {s} or {s} is not a profile this doxa wrote\n", .{ map_path, data_path }),
                error.OutOfMemory => return oom(),
            }
            return 1;
        };
    } else null;
    // Findings, when it was built with `--emit-complexity`.
    const findings: ?[]const complexity.Finding = if (readIfPresent(io, alloc, findings_path) orelse return 1) |text|
        complexity.parse(alloc, text) catch |err| switch (err) {
            error.Malformed => {
                std.debug.print("Error: {s} is not a findings file this doxa wrote\n", .{findings_path});
                return 1;
            },
            error.OutOfMemory => return oom(),
        }
    else
        null;
    // Memory rows, when it was built with `--emit-memory`.
    const memory_rows: ?[]const complexity.Finding = if (readIfPresent(io, alloc, memory_path) orelse return 1) |text|
        memory.parse(alloc, text) catch |err| switch (err) {
            error.Malformed => {
                std.debug.print("Error: {s} is not a memory report this doxa wrote\n", .{memory_path});
                return 1;
            },
            error.OutOfMemory => return oom(),
        }
    else
        null;
    if (profile == null and findings == null and memory_rows == null) {
        std.debug.print("Error: nothing to report for {s}: build it with --instrument=, --emit-complexity or --emit-memory\n", .{given});
        return 1;
    }

    var buffer: [4096]u8 = undefined;
    var out = std.Io.File.stdout().writerStreaming(io, &buffer);
    const w = &out.interface;
    (if (json) writeJson(w, .{ .profile = profile, .complexity = findings, .memory = memory_rows }) else text: {
        if (profile) |r| writeText(w, alloc, r) catch |err| break :text err;
        if (findings) |f| complexity.writeText(w, alloc, f, if (profile) |p| p.sites else null) catch |err| break :text err;
        if (memory_rows) |m| memory.writeText(w, m, if (profile) |r| r.sites else null) catch |err| break :text err;
    }) catch return 1;
    w.flush() catch return 1;
    return 0;
}

/// The binary's path without its extension, allocator-owned. A name with a
/// directory or an extension is the user's path, resolved against the
/// invocation directory; a bare stem names a `doxa run` build in the cache.
fn stemOf(alloc: std.mem.Allocator, given: []const u8, cache_dir: []const u8) ![]const u8 {
    const ext = std.fs.path.extension(given);
    if (ext.len != 0) {
        // A Windows binary's side files drop `.exe`; on another host it is
        // part of the name the build wrote.
        const bare = if (builtin.os.tag == .windows and std.ascii.eqlIgnoreCase(ext, ".exe"))
            given[0 .. given.len - ext.len]
        else
            given;
        return alloc.dupe(u8, bare);
    }
    if (std.fs.path.dirname(given) != null) return alloc.dupe(u8, given);
    return std.fs.path.join(alloc, &.{ cache_dir, given });
}

/// The file's contents, `.{null}` when there is none, or null after
/// reporting why it could not be read.
fn readIfPresent(io: std.Io, alloc: std.mem.Allocator, path: []const u8) ??[]const u8 {
    return std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .unlimited) catch |err| switch (err) {
        error.FileNotFound => @as(?[]const u8, null),
        else => {
            std.debug.print("Error: cannot read {s} ({s})\n", .{ path, @errorName(err) });
            return null;
        },
    };
}

fn oom() u8 {
    std.debug.print("Error: out of memory\n", .{});
    return 1;
}

test "a profile joins with its own build and refuses another's" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const sites = "call\tmain\tm.doxa\t3\t1\ncall\tget\tm.doxa\t1\t1\n";
    var map: std.Io.Writer.Allocating = .init(a);
    try writeProfmap(&map.writer, "-O0", sites);
    const data = try std.fmt.allocPrint(a, "{s}\nhash {x:0>16}\n1\n7\n", .{ instrument.profdata_header, instrument.buildHash(sites) });

    const report = try join(a, map.written(), data);
    try std.testing.expectEqualStrings("-O0", report.opt);
    try std.testing.expectEqual(@as(usize, 2), report.sites.len);
    try std.testing.expectEqualStrings("get", report.sites[1].function);
    try std.testing.expectEqual(@as(i64, 7), report.sites[1].count);

    var text: std.Io.Writer.Allocating = .init(a);
    try writeText(&text.writer, a, report);
    const hottest = std.mem.indexOf(u8, text.written(), "  7\tget\tm.doxa:1:1\n").?;
    try std.testing.expect(hottest < std.mem.indexOf(u8, text.written(), "  1\tmain\tm.doxa:3:1\n").?);

    const other = try std.fmt.allocPrint(a, "{s}\nhash {x:0>16}\n1\n7\n", .{ instrument.profdata_header, instrument.buildHash("call\tx\tm.doxa\t1\t1\n") });
    try std.testing.expectError(error.Stale, join(a, map.written(), other));
    try std.testing.expectError(error.Stale, join(a, map.written(), data[0 .. data.len - 2]));
}

test "a bare filename with an extension is a path, a bare stem is in the cache" {
    const alloc = std.testing.allocator;
    const exe_stem = if (builtin.os.tag == .windows) "repro" else "repro.exe";

    const exe = try stemOf(alloc, "repro.exe", ".doxa-cache");
    defer alloc.free(exe);
    try std.testing.expectEqualStrings(exe_stem, exe);

    const nested = try stemOf(alloc, "out/repro.exe", ".doxa-cache");
    defer alloc.free(nested);
    try std.testing.expectEqualStrings(
        if (builtin.os.tag == .windows) "out/repro" else "out/repro.exe",
        nested,
    );

    const explicit = try stemOf(alloc, "./repro", ".doxa-cache");
    defer alloc.free(explicit);
    try std.testing.expectEqualStrings("./repro", explicit);

    const cached = try stemOf(alloc, "repro", ".doxa-cache");
    defer alloc.free(cached);
    const expected = try std.fs.path.join(alloc, &.{ ".doxa-cache", "repro" });
    defer alloc.free(expected);
    try std.testing.expectEqualStrings(expected, cached);
}
