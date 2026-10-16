//! The static memory report (`--emit-memory`,
//! `plan/diagnostics-and-observability.md` step 9): where every allocation
//! lives and when its arena is reclaimed, and every copy the compiler placed,
//! kept or removed, with what it was for. It reads the HIR that ships (what
//! the arena pass kept) and the pass's own record of what it removed
//! (`ir.ArenaDecisions`); arena identity is the pass's (`arenas.Families`).
//!
//! `<stem>.memory` is a header line, then one tab-separated row per fact:
//!
//! - `alloc  <function>  <site>  <type>  <arena>  <reclaimed>`
//! - `copy  <function>  <site>  <type>  clone|rehome  <reason>` — kept
//! - `removed  <function>  <site>  <type>  outlives|dead  <reason>`
//! - `dropped  <function>  <site>  <role>` — an arena nothing allocates in
//! - `caller  <function>` — a `@caller` parameter dropped
//! - `opened  <function>  <count>` — the arenas a function still opens
//! - `retain  <function>  <site>  <type>  <loop site>  <landing>  <cause>` —
//!   retention (step 13): something a loop makes every iteration that no
//!   iteration reclaims. `landing` is `root`, `function`, or `loop <site>`
//!   (an enclosing loop's arena); `cause` is `global <name>` when it is kept
//!   by a global, else the copy reason or `allocated`.
//! - `plain  <function>  <site>  <type>  binding|element  <mutated>` — a
//!   copy B would add (step 14, `plain.zig`).

const std = @import("std");
const ir = @import("../codegen/hir/register/ir.zig");
const verify = @import("../codegen/hir/register/verify.zig");
const arenas = @import("../codegen/hir/register/arenas.zig");
const loops = @import("../codegen/hir/register/loops.zig");
const print = @import("../codegen/hir/register/print.zig");
const complexity = @import("complexity.zig");
const report = @import("report.zig");
const plain = @import("plain.zig");

pub const header = "doxa-memory 1";

pub fn write(alloc: std.mem.Allocator, w: *std.Io.Writer, module: *const ir.Module) !void {
    try w.print("{s}\n", .{header});
    for (module.functions) |*f| {
        var facts = try verify.facts(alloc, &module.program, f) orelse continue;
        const rows: Rows = .{ .alloc = alloc, .w = w, .module = module, .f = f, .facts = &facts, .families = try arenas.Families.init(alloc, f) };
        try rows.function();
        try plain.write(alloc, w, module, f);
    }
    const d = module.arena_decisions;
    for (d.removed_copies) |c| {
        const f = &module.functions[@intFromEnum(c.function)];
        try w.print("removed\t{s}\t", .{f.display_name});
        try site(w, module, c.span);
        try w.writeAll("\t");
        try print.writeHIRType(w, &module.program, c.ty);
        try w.print("\t{s}\t{s}\n", .{ @tagName(c.why), @tagName(c.reason) });
    }
    for (d.dropped_arenas) |a| {
        try w.print("dropped\t{s}\t", .{module.functions[@intFromEnum(a.function)].display_name});
        try site(w, module, a.span);
        try w.print("\t{s}\n", .{@tagName(a.role)});
    }
    for (d.dropped_callers) |id| try w.print("caller\t{s}\n", .{module.functions[@intFromEnum(id)].display_name});
}

fn site(w: *std.Io.Writer, module: *const ir.Module, span: ir.SrcSpan) !void {
    try w.print("{s}:{d}:{d}", .{ module.program.files[span.module], span.line, span.column });
}

const Rows = struct {
    alloc: std.mem.Allocator,
    w: *std.Io.Writer,
    module: *const ir.Module,
    f: *const ir.Function,
    facts: *const verify.Facts,
    families: arenas.Families,

    fn function(self: Rows) !void {
        var entered: u32 = 0;
        for (self.f.blocks) |block| {
            for (block.insts) |inst| {
                switch (inst.op) {
                    .scope_enter => entered += 1,
                    .clone, .rehome => try self.copy(inst),
                    else => {},
                }
                if (loops.allocArena(inst.op)) |arena| try self.allocation(inst, arena);
                try self.retention(inst);
            }
        }
        if (entered != 0) try self.w.print("opened\t{s}\t{d}\n", .{ self.f.display_name, entered });
    }

    fn allocation(self: Rows, inst: ir.Inst, arena: ir.ValueId) !void {
        try self.row("alloc", inst);
        try self.w.writeAll("\t");
        switch (self.facts.kind(arena)) {
            .root => try self.w.writeAll("the root arena\tnever"),
            .caller => try self.w.writeAll("the caller's arena\twhen the caller returns"),
            .alias => try self.w.writeAll("a ^ argument's arena\twhen its owner closes it"),
            .opened => try self.opened(arena),
        }
        try self.w.writeAll("\n");
    }

    /// An arena this function opened: whose it is, and every place it is
    /// closed or reset.
    fn opened(self: Rows, arena: ir.ValueId) !void {
        const family = self.families.of(arena);
        const enter = self.opening(family) orelse return self.w.writeAll("an arena\tunknown");
        switch (arenas.arenaRole(self.f, enter)) {
            .loop_body => {
                try self.w.writeAll("the arena of the loop at ");
                try self.loopSite(enter, .body);
                return self.w.writeAll("\tevery iteration");
            },
            .loop_state => {
                try self.w.writeAll("the own arena of the loop at ");
                try self.loopSite(enter, .state);
                try self.w.writeAll("\twhen the loop ends");
            },
            .body => try self.w.print("{s}'s arena\twhen it returns", .{self.f.display_name}),
            .block => try self.w.writeAll("a block's arena\twhen the block ends"),
        }
        try self.closings(family);
    }

    /// The `scope_enter` value that opens `family`.
    fn opening(self: Rows, family: u32) ?ir.ValueId {
        for (self.f.blocks) |block| {
            for (block.insts) |inst| switch (inst.op) {
                .scope_enter => if (self.families.of(inst.result.?) == family) return inst.result.?,
                else => {},
            };
        }
        return null;
    }

    /// `, at line L, L…`: where `family` is closed.
    fn closings(self: Rows, family: u32) !void {
        var lines: std.ArrayListUnmanaged(u32) = .empty;
        for (self.f.blocks) |block| {
            for (block.insts) |inst| {
                const closed = switch (inst.op) {
                    .scope_exit => |e| e.arena,
                    else => continue,
                };
                if (self.families.of(closed) != family) continue;
                // Falling off the end closes at the declaration: no line of
                // its own to name.
                if (inst.span.line == self.f.span.line) continue;
                if (std.mem.indexOfScalar(u32, lines.items, inst.span.line) == null) try lines.append(self.alloc, inst.span.line);
            }
        }
        if (lines.items.len == 0) return;
        std.mem.sort(u32, lines.items, {}, std.sort.asc(u32));
        try self.w.writeAll(" (line");
        if (lines.items.len > 1) try self.w.writeAll("s");
        for (lines.items, 0..) |line, i| try self.w.print("{s}{d}", .{ if (i == 0) " " else ", ", line });
        try self.w.writeAll(")");
    }

    /// The loop whose body arena is `enter` (`.body`) or whose body is opened
    /// under it (`.state`).
    fn loopSite(self: Rows, enter: ir.ValueId, which: enum { body, state }) !void {
        for (self.f.loops) |info| {
            const body = info.arena orelse continue;
            const match = switch (which) {
                .body => self.families.of(body) == self.families.of(enter),
                .state => self.facts.parentOf(body) == enter,
            };
            if (match) return site(self.w, self.module, info.span);
        }
        try self.w.writeAll("?");
    }

    /// A `retain` row for `inst` when it runs every iteration of a loop and
    /// what it makes lands where no iteration reclaims it: the root, the
    /// function's own arena, or an enclosing loop's. Where it lands is
    /// `loops.landingOf`'s, the same join step 6 reads.
    fn retention(self: Rows, inst: ir.Inst) !void {
        const loop = self.f.loop(inst.span.loop) orelse return;
        var value: ir.ValueId = undefined;
        const landing = if (loops.allocArena(inst.op)) |arena| blk: {
            value = inst.result orelse return;
            break :blk loops.landingOf(self.f, self.facts, inst.span.loop, arena);
        } else if (loops.storeInto(inst.op)) |st| blk: {
            if (!self.f.typeOf(st.value).isHeap()) return;
            value = st.value;
            const region = self.facts.region(st.container) orelse return;
            break :blk loops.landingOfRegion(self.f, self.facts, inst.span.loop, region);
        } else return;
        switch (landing) {
            .root, .function, .outer_loop => {},
            .iteration, .caller, .alias, .outer, .static => return,
        }
        try self.w.print("retain\t{s}\t", .{self.f.display_name});
        try site(self.w, self.module, inst.span);
        try self.w.writeAll("\t");
        try print.writeHIRType(self.w, &self.module.program, self.f.typeOf(value).doxa);
        try self.w.writeAll("\t");
        try site(self.w, self.module, loop.span);
        switch (landing) {
            .root => try self.w.writeAll("\troot"),
            .function => try self.w.writeAll("\tfunction"),
            .outer_loop => |l| {
                try self.w.writeAll("\tloop ");
                try site(self.w, self.module, self.f.loop(l).?.span);
            },
            else => unreachable,
        }
        if (self.keptBy(inst)) |g| {
            try self.w.print("\tglobal {s}\n", .{self.module.program.globals[@intFromEnum(g)].display_name});
        } else if (inst.result != null and (inst.op == .clone or inst.op == .rehome)) {
            try self.w.print("\t{s}\n", .{@tagName(arenas.copyReason(self.f, inst.result.?))});
        } else {
            try self.w.writeAll("\tallocated\n");
        }
    }

    /// The global that keeps what `inst` makes: the one its result is stored
    /// to, or the one whose container it stores into.
    fn keptBy(self: Rows, inst: ir.Inst) ?ir.GlobalId {
        if (loops.storeInto(inst.op)) |st| return self.globalLoaded(st.container);
        const result = inst.result orelse return null;
        for (self.f.blocks) |block| {
            for (block.insts) |use| switch (use.op) {
                .global_store => |g| if (g.value == result) return g.global,
                else => {},
            };
        }
        return null;
    }

    fn globalLoaded(self: Rows, value: ir.ValueId) ?ir.GlobalId {
        const at = switch (self.f.values[@intFromEnum(value)].def) {
            .inst => |at| at,
            .param => return null,
        };
        return switch (self.f.blocks[@intFromEnum(at.block)].insts[at.index].op) {
            .global_load => |g| g,
            else => null,
        };
    }

    fn copy(self: Rows, inst: ir.Inst) !void {
        try self.row("copy", inst);
        try self.w.print("\t{s}\t{s}\n", .{ @tagName(inst.op), @tagName(arenas.copyReason(self.f, inst.result.?)) });
    }

    fn row(self: Rows, kind: []const u8, inst: ir.Inst) !void {
        try self.w.print("{s}\t{s}\t", .{ kind, self.f.display_name });
        try site(self.w, self.module, inst.span);
        try self.w.writeAll("\t");
        try print.writeHIRType(self.w, &self.module.program, self.f.typeOf(inst.result.?).doxa);
    }
};

// ── Reading ──

pub fn parse(alloc: std.mem.Allocator, text: []const u8) error{ Malformed, OutOfMemory }![]complexity.Finding {
    return complexity.parseRows(alloc, text, header);
}

/// The report: a section per kind, then the summary. `sites`, from a
/// memtrace run of the same program, puts what each retention row held in
/// that run beside it.
pub fn writeText(w: *std.Io.Writer, rows: []const complexity.Finding, sites: ?[]const report.Site) !void {
    if (has(rows, "alloc")) {
        try banner(w, "Allocations");
        for (rows) |r| if (eql(r.kind, "alloc") and r.fields.len == 4) {
            try w.print("  {s}\t{s}\t{s}\tin {s}; reclaimed {s}\n", .{ r.fields[0], r.function, r.fields[1], r.fields[2], r.fields[3] });
        };
    }
    if (has(rows, "copy")) {
        try banner(w, "Copies kept");
        for (rows) |r| if (eql(r.kind, "copy") and r.fields.len == 4) {
            const how = if (eql(r.fields[2], "clone")) "copied here" else "copied at run time unless it already lives there";
            try w.print("  {s}\t{s}\t{s}\t{s}: {s}\n", .{ r.fields[0], r.function, r.fields[1], how, reasonText(r.fields[3]) });
        };
    }
    if (has(rows, "removed")) {
        try banner(w, "Copies removed");
        for (rows) |r| if (eql(r.kind, "removed") and r.fields.len == 4) {
            const why = if (eql(r.fields[2], "outlives")) "the value itself, which already lives there" else "dropped, as nothing used it";
            try w.print("  {s}\t{s}\t{s}\t{s}: {s}\n", .{ r.fields[0], r.function, r.fields[1], why, reasonText(r.fields[3]) });
        };
    }
    if (has(rows, "dropped") or has(rows, "caller")) {
        try banner(w, "Arenas dropped");
        for (rows) |r| if (eql(r.kind, "dropped") and r.fields.len == 2) {
            try w.print("  {s}\t{s}\t{s}: nothing allocates in it\n", .{ r.fields[0], r.function, roleText(r.fields[1]) });
        };
        for (rows) |r| if (eql(r.kind, "caller")) {
            try w.print("  {s}\tits @caller arena: it returns nothing that lives there\n", .{r.function});
        };
    }

    if (has(rows, "retain")) {
        try banner(w, "Retention");
        try w.writeAll("Made every iteration of a loop, and reclaimed by none of them.\n\n");
        for (rows) |r| if (eql(r.kind, "retain") and r.fields.len == 5) {
            try w.print("  {s}\t{s}\t{s}\tin the loop at {s}: ", .{ r.fields[0], r.function, r.fields[1], r.fields[2] });
            try retentionText(w, r.function, r.fields[3], r.fields[4]);
            if (sites) |all| try heldInRun(w, all, r.fields[0]);
            try w.writeAll("\n");
        };
    }

    if (has(rows, "plain")) try writePlain(w, rows, sites);

    var opened: u64 = 0;
    for (rows) |r| if (eql(r.kind, "opened") and r.fields.len == 1) {
        opened += std.fmt.parseInt(u64, r.fields[0], 10) catch 0;
    };
    var clones: u32 = 0;
    for (rows) |r| if (eql(r.kind, "copy") and r.fields.len == 4 and eql(r.fields[2], "clone")) {
        clones += 1;
    };
    const dropped = count(rows, "dropped");
    try w.print("\n{d} allocation sites. Arenas: {d} opened, {d} dropped; {d} @caller dropped. Copies: {d} kept ({d} clone, {d} runtime), {d} removed.\n", .{
        count(rows, "alloc"), opened + dropped, dropped, count(rows, "caller"),
        count(rows, "copy"),  clones,          count(rows, "copy") - clones, count(rows, "removed"),
    });
}

/// The copies B would add (`plain.zig`): each store, then the range. With a
/// `--instrument=lines` run, each is weighted by how often its line ran.
fn writePlain(w: *std.Io.Writer, rows: []const complexity.Finding, sites: ?[]const report.Site) !void {
    try banner(w, "Copies B would add");
    try w.writeAll("Stores of a value another binding still holds: B (plain stores copy) copies each.\n");
    try w.writeAll("A row marked ! is followed by a mutation, so no elision removes its copy.\n\n");
    var weighted = false;
    if (sites) |all| for (all) |s| {
        if (eql(s.kind, "line")) weighted = true;
    };
    var total = [2]u64{ 0, 0 };
    var mutated = [2]u64{ 0, 0 };
    for (rows) |r| if (eql(r.kind, "plain") and r.fields.len == 4) {
        const is_mutated = eql(r.fields[3], "1");
        const runs: u64 = if (weighted) report.lineCount(sites.?, r.fields[0]) else 0;
        try w.print("  {s} {s}\t{s}\t{s}\t{s}", .{ if (is_mutated) "!" else " ", r.fields[0], r.function, r.fields[1], if (eql(r.fields[2], "element")) "stored into a container" else "bound to a variable" });
        if (weighted) try w.print("\tran {d} time{s}", .{ runs, if (runs == 1) "" else "s" });
        try w.writeAll("\n");
        total[0] += 1;
        total[1] += runs;
        if (is_mutated) {
            mutated[0] += 1;
            mutated[1] += runs;
        }
    };
    try w.print("\nB adds between {d} and {d} copies at {d} sites", .{ mutated[0], total[0], total[0] });
    if (weighted) try w.print("; in this run, between {d} and {d} copies made", .{ mutated[1], total[1] });
    try w.writeAll(".\n");
}

/// Where a retained value lands, and the fix the compiler knows of.
fn retentionText(w: *std.Io.Writer, function: []const u8, landing: []const u8, cause: []const u8) !void {
    if (std.mem.startsWith(u8, cause, "global ")) {
        return w.print("escapes into global `{s}`, kept until the program ends", .{cause["global ".len..]});
    }
    if (eql(landing, "root")) return w.writeAll("lands in the root arena, kept until the program ends");
    if (std.mem.startsWith(u8, landing, "loop ")) {
        return w.print("lands in the arena of the loop at {s}, reset once per its iteration", .{landing["loop ".len..]});
    }
    try w.print("lands in {s}'s arena, reclaimed when it returns", .{function});
    // A value kept in a variable outside the loop: a block around both ends
    // its lifetime at the block's `}` (`docs/memory.md`).
    if (!eql(cause, "allocated")) try w.writeAll("; a block around the loop and the variable that keeps it reclaims it at its `}`");
}

/// `(this run: …)`: what the memtrace sites at `at` allocated, and how much
/// of it went into the root arena.
fn heldInRun(w: *std.Io.Writer, sites: []const report.Site, at: []const u8) !void {
    var bytes: u64 = 0;
    var allocs: u64 = 0;
    var rooted: u64 = 0;
    var any = false;
    for (sites) |s| {
        const t = s.trace orelse continue;
        var buf: [512]u8 = undefined;
        const here = std.fmt.bufPrint(&buf, "{s}:{d}:{d}", .{ s.file, s.line, s.column }) catch continue;
        if (!eql(here, at)) continue;
        any = true;
        bytes += t.bytes;
        allocs += t.allocs;
        rooted += t.rooted;
    }
    if (!any) return;
    try w.print(" (this run: {d} B in {d} allocation{s}", .{ bytes, allocs, if (allocs == 1) "" else "s" });
    if (rooted != 0) try w.print(", {d} B into the root arena", .{rooted});
    try w.writeAll(")");
}

fn reasonText(reason: []const u8) []const u8 {
    const map = .{
        .{ "store", "stored to a longer-lived variable" },
        .{ "global", "stored to a global" },
        .{ "element", "stored into a container" },
        .{ "argument", "passed as an argument" },
        .{ "return", "returned" },
        .{ "merge", "carried out of a branch or loop" },
    };
    inline for (map) |m| if (eql(reason, m[0])) return m[1];
    return "used";
}

fn roleText(role: []const u8) []const u8 {
    if (eql(role, "body")) return "the function's arena";
    if (eql(role, "loop_state")) return "a loop's own arena";
    if (eql(role, "loop_body")) return "a loop body's arena";
    return "a block's arena";
}

fn banner(w: *std.Io.Writer, title: []const u8) !void {
    try w.print("\n==========================================\n{s}\n==========================================\n", .{title});
}

fn has(rows: []const complexity.Finding, kind: []const u8) bool {
    return count(rows, kind) != 0;
}

fn count(rows: []const complexity.Finding, kind: []const u8) u32 {
    var n: u32 = 0;
    for (rows) |r| if (eql(r.kind, kind)) {
        n += 1;
    };
    return n;
}

fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
