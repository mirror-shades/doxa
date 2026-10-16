//! Static complexity findings (`--emit-complexity`,
//! `plan/diagnostics-and-observability.md` step 6): what each loop repeats,
//! read off the register HIR after the arena pass, the HIR that ships. The
//! loops are the ones lowering recorded and the arenas the ones the arena pass
//! kept (`loops.zig`); nothing is re-derived.
//!
//! `<stem>.complexity` is a header line, then one tab-separated row per
//! finding:
//!
//! - `nest  <function>  <depth>  <loop site> > <loop site> …` — a function's
//!   deepest nest, outermost first;
//! - `alloc  <function>  <site>  <op>  <loop site>  <landing>  <flag>` — an
//!   allocation or a heap store made every iteration, and where it lands;
//!   `flag` is `1` when no iteration reclaims it;
//! - `call  <function>  <site>  <callee>  <what>  <loop site>` — a call made
//!   every iteration to a function that itself allocates or loops;
//! - `rehome  <function>  <site>  <loop site>` — a copy left to a runtime
//!   check, made every iteration.

const std = @import("std");
const ir = @import("../codegen/hir/register/ir.zig");
const verify = @import("../codegen/hir/register/verify.zig");
const loops = @import("../codegen/hir/register/loops.zig");

pub const header = "doxa-complexity 1";

pub fn write(alloc: std.mem.Allocator, w: *std.Io.Writer, module: *const ir.Module) !void {
    try w.print("{s}\n", .{header});
    for (module.functions, 0..) |*f, i| {
        if (f.loops.len == 0) continue;
        // A function that does not verify was reported already.
        var facts = try verify.facts(alloc, &module.program, f) orelse continue;
        const writer: Writer = .{ .w = w, .module = module, .f = f, .facts = &facts };
        _ = i;
        try writer.nest();
        for (f.blocks) |block| {
            for (block.insts) |inst| try writer.inst(inst);
        }
    }
}

const Writer = struct {
    w: *std.Io.Writer,
    module: *const ir.Module,
    f: *const ir.Function,
    facts: *const verify.Facts,

    fn nest(self: Writer) !void {
        var deepest: ir.LoopId = .none;
        var most: u32 = 0;
        for (1..self.f.loops.len + 1) |n| {
            const id: ir.LoopId = @enumFromInt(n);
            const d = loops.depth(self.f, id);
            if (d > most) {
                most = d;
                deepest = id;
            }
        }
        try self.w.print("nest\t{s}\t{d}\t", .{ self.f.display_name, most });
        try self.chain(deepest);
        try self.w.writeAll("\n");
    }

    /// `loop`'s nest, outermost first.
    fn chain(self: Writer, loop: ir.LoopId) !void {
        const info = self.f.loop(loop) orelse return;
        if (info.parent != .none) {
            try self.chain(info.parent);
            try self.w.writeAll(" > ");
        }
        try self.site(info.span);
    }

    fn inst(self: Writer, i: ir.Inst) !void {
        const loop = i.span.loop;
        const info = self.f.loop(loop) orelse return;
        if (i.op == .rehome) {
            try self.row("rehome", i.span);
            try self.w.writeAll("\t");
            try self.site(info.span);
            try self.w.writeAll("\n");
        }
        if (loops.allocArena(i.op)) |arena| {
            try self.alloc(i, @tagName(i.op), loops.landingOf(self.f, self.facts, loop, arena), info.span);
        } else if (loops.storeInto(i.op)) |s| {
            if (self.f.typeOf(s.value).isHeap()) {
                const region = self.facts.region(s.container) orelse return;
                try self.alloc(i, @tagName(i.op), loops.landingOfRegion(self.f, self.facts, loop, region), info.span);
            }
        } else if (i.op == .call) {
            const callee = &self.module.functions[@intFromEnum(i.op.call.callee)];
            const allocates = allocatesIn(callee);
            const repeats = callee.loops.len != 0;
            if (!allocates and !repeats) return;
            try self.row("call", i.span);
            try self.w.print("\t{s}\t{s}\t", .{ callee.display_name, if (allocates and repeats) "allocates, loops" else if (allocates) "allocates" else "loops" });
            try self.site(info.span);
            try self.w.writeAll("\n");
        }
    }

    fn alloc(self: Writer, i: ir.Inst, op: []const u8, landing: loops.Landing, loop_span: ir.SrcSpan) !void {
        if (landing == .static) return;
        try self.row("alloc", i.span);
        try self.w.print("\t{s}\t", .{op});
        try self.site(loop_span);
        try self.w.writeAll("\t");
        try self.landingText(landing);
        try self.w.print("\t{d}\n", .{@intFromBool(!landing.perIteration())});
    }

    /// Where it lands, said as a fact: whose arena, and when it is reclaimed.
    fn landingText(self: Writer, landing: loops.Landing) !void {
        switch (landing) {
            .iteration => try self.w.writeAll("this loop's arena, reset every iteration"),
            .outer_loop => |l| {
                try self.w.writeAll("the arena of the loop at ");
                try self.site(self.f.loop(l).?.span);
                try self.w.writeAll(", reset once per its iteration");
            },
            .function => try self.w.print("{s}'s arena, reclaimed when it returns", .{self.f.display_name}),
            .caller => try self.w.print("its caller's arena, reclaimed when the caller of {s} returns", .{self.f.display_name}),
            .alias => try self.w.writeAll("a ^ argument's arena, owned by a caller"),
            .root => try self.w.writeAll("the root arena, never reclaimed"),
            .outer => try self.w.writeAll("a parameter's storage, owned by a caller"),
            .static => unreachable,
        }
    }

    fn row(self: Writer, kind: []const u8, span: ir.SrcSpan) !void {
        try self.w.print("{s}\t{s}\t", .{ kind, self.f.display_name });
        try self.site(span);
    }

    fn site(self: Writer, span: ir.SrcSpan) !void {
        try self.w.print("{s}:{d}:{d}", .{ self.module.program.files[span.module], span.line, span.column });
    }
};

/// Whether `f` allocates in its own body. Direct only: a callee's callees
/// are not followed (TODO: a call-graph fixpoint for transitive findings).
fn allocatesIn(f: *const ir.Function) bool {
    for (f.blocks) |block| {
        for (block.insts) |i| if (loops.allocArena(i.op) != null) return true;
    }
    return false;
}

// ── Reading ──

pub const Finding = struct {
    kind: []const u8,
    function: []const u8,
    /// The rest of the row, as written.
    fields: []const []const u8,
};

pub fn parse(alloc: std.mem.Allocator, text: []const u8) error{ Malformed, OutOfMemory }![]Finding {
    return parseRows(alloc, text, header);
}

/// The rows of a side file that starts with `first`: `kind`, `function`, then
/// the rest. The memory report (`memory.zig`) shares the shape.
pub fn parseRows(alloc: std.mem.Allocator, text: []const u8, first: []const u8) error{ Malformed, OutOfMemory }![]Finding {
    var lines = std.mem.splitScalar(u8, text, '\n');
    if (!std.mem.eql(u8, lines.next() orelse "", first)) return error.Malformed;
    var findings: std.ArrayListUnmanaged(Finding) = .empty;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var cols = std.mem.splitScalar(u8, line, '\t');
        const kind = cols.next().?;
        const function = cols.next() orelse return error.Malformed;
        var fields: std.ArrayListUnmanaged([]const u8) = .empty;
        while (cols.next()) |c| try fields.append(alloc, c);
        try findings.append(alloc, .{ .kind = kind, .function = function, .fields = fields.items });
    }
    return findings.items;
}

/// The findings, a section per kind, flagged allocations first.
pub fn writeText(w: *std.Io.Writer, findings: []const Finding) !void {
    if (has(findings, "nest")) {
        try banner(w, "Loop nesting");
        for (findings) |f| if (eql(f.kind, "nest") and f.fields.len == 2) {
            try w.print("  {s}\t{s}\t{s}\n", .{ f.fields[0], f.function, f.fields[1] });
        };
    }
    if (has(findings, "alloc")) {
        try banner(w, "Allocations in loops");
        try w.writeAll("A row marked ! lands where no iteration of its loop reclaims it.\n\n");
        for ([_][]const u8{ "1", "0" }) |flag| {
            for (findings) |f| if (eql(f.kind, "alloc") and f.fields.len == 5 and eql(f.fields[4], flag)) {
                try w.print("  {s} {s}\t{s}\t{s} in the loop at {s}: {s}\n", .{ if (eql(flag, "1")) "!" else " ", f.fields[0], f.function, f.fields[1], f.fields[2], f.fields[3] });
            };
        }
    }
    if (has(findings, "call")) {
        try banner(w, "Calls in loops");
        for (findings) |f| if (eql(f.kind, "call") and f.fields.len == 4) {
            try w.print("  {s}\t{s}\tcalls {s} ({s}) in the loop at {s}\n", .{ f.fields[0], f.function, f.fields[1], f.fields[2], f.fields[3] });
        };
    }
    if (has(findings, "rehome")) {
        try banner(w, "Runtime copies in loops");
        for (findings) |f| if (eql(f.kind, "rehome") and f.fields.len == 2) {
            try w.print("  {s}\t{s}\tin the loop at {s}: a copy the arena pass left to a runtime check\n", .{ f.fields[0], f.function, f.fields[1] });
        };
    }
}

fn banner(w: *std.Io.Writer, title: []const u8) !void {
    try w.print("\n==========================================\n{s}\n==========================================\n", .{title});
}

fn has(findings: []const Finding, kind: []const u8) bool {
    for (findings) |f| if (eql(f.kind, kind)) return true;
    return false;
}

fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
