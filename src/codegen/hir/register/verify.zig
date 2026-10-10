//! The register-HIR verifier (`plan/register-hir.md`, "Verifier"). It runs
//! between generation and emission in every build mode. A fault is a compiler
//! bug: the function, block and instruction are named, and `writeFault`
//! quotes the block in the textual form.
//!
//! 1. Definition: every operand's definition dominates its use.
//! 2. Types: every instruction and terminator is applied to operands of the
//!    types it takes, and defines a value of the type it yields.
//! 3. Stores: a stored value has its destination's exact type.
//! 4. Arenas: scopes close innermost first and are all closed at a return;
//!    no value is used once its arena is closed or reset; a heap value stored
//!    into a slot, a global or a `^` target, or returned, lives in an arena
//!    that outlives the destination — or reaches it through `clone`/`rehome`.
//!    Container stores re-home at runtime and are exempt.
//! 5. Aliases: a `ref` is the address of a slot or a `^` parameter, is never
//!    a block parameter, and no call takes two `^` arguments naming one slot;
//!    a `^` argument travels with the arena that owns its slot.

const std = @import("std");
const ir = @import("ir.zig");
const print = @import("print.zig");

const ValueId = ir.ValueId;
const BlockId = ir.BlockId;
const HIRType = ir.HIRType;
const Type = ir.Type;

pub const Fault = struct {
    block: ?BlockId,
    /// The instruction's index in its block; `null` with a block is its
    /// terminator.
    inst: ?u32,
    message: []const u8,
};

/// Check `f`. Returns the first fault, or null. `alloc` holds the verifier's
/// scratch and the fault's message; an arena is the intended allocator.
pub fn verify(alloc: std.mem.Allocator, program: *const ir.Program, f: *const ir.Function) std.mem.Allocator.Error!?Fault {
    var v = Verifier{ .alloc = alloc, .program = program, .f = f };
    v.run() catch |err| switch (err) {
        error.Fault => return v.found.?,
        error.OutOfMemory => return error.OutOfMemory,
    };
    return null;
}

/// What the verifier proves about a function's arenas and heap values: the
/// arena each arena value is, and the region each heap value lives in. Passes
/// that refine copies and drop dead arenas (`arenas.zig`) decide by these, so
/// they agree with the checks their output must pass.
pub const Facts = struct {
    v: Verifier,

    /// Where the heap value `value` lives; null for a value that is not heap.
    pub fn region(self: *const Facts, value: ValueId) ?Region {
        return self.v.region[@intFromEnum(value)];
    }

    /// Whether a value living in `r` is sure to outlive the arena `dest`.
    pub fn outlives(self: *const Facts, r: Region, dest: ValueId) bool {
        return self.v.outlives(r, dest);
    }

    /// The arena `arena` was opened in, if this function opened it.
    pub fn parentOf(self: *const Facts, arena: ValueId) ?ValueId {
        return self.v.parentOf(arena);
    }
};

/// The facts of a function that passes verification; null when it does not.
pub fn facts(alloc: std.mem.Allocator, program: *const ir.Program, f: *const ir.Function) std.mem.Allocator.Error!?Facts {
    var v = Verifier{ .alloc = alloc, .program = program, .f = f };
    v.run() catch |err| switch (err) {
        error.Fault => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
    return .{ .v = v };
}

/// The fault, then the textual form of the block it is in.
pub fn writeFault(w: *std.Io.Writer, program: *const ir.Program, f: *const ir.Function, fault: Fault) std.Io.Writer.Error!void {
    try w.print("malformed HIR in '{s}'", .{f.name});
    if (fault.block) |b| {
        try w.print(", b{d}", .{@intFromEnum(b)});
        if (fault.inst) |i| try w.print(" instruction {d}", .{i}) else try w.writeAll(" terminator");
    }
    try w.print(": {s}\n", .{fault.message});
    if (fault.block) |b| {
        if (@intFromEnum(b) < f.blocks.len) try print.writeBlock(w, program, f, b, &f.blocks[@intFromEnum(b)]);
    }
}

const Error = std.mem.Allocator.Error || error{Fault};

/// Where a heap value lives.
pub const Region = union(enum) {
    /// Static storage (a string literal); outlives every arena.
    static,
    /// The program root arena: a global's value.
    root,
    /// Owned outside the function: a parameter's value. It lives in the
    /// caller's innermost arena or an ancestor of it, so it outlives
    /// `@caller` and everything the function opens.
    outer,
    arena: ValueId,

    fn eql(a: Region, b: Region) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .static, .root, .outer => true,
            .arena => |x| x == b.arena,
        };
    }
};

/// What an arena value is.
const ArenaKind = union(enum) {
    root,
    caller,
    alias,
    /// Opened in this function, as a child of `parent`.
    opened: ValueId,
};

/// Whether a fixed array's elements of `from` become `to` when
/// `array.from_fixed` copies them: equal, or a nested fixed array the copy
/// turns dynamic.
fn fixedConverts(from: HIRType, to: HIRType) bool {
    if (from.eql(to)) return true;
    if (from != .Array or to != .Array or from.Array.size == null or to.Array.size != null) return false;
    return fixedConverts(from.Array.element.*, to.Array.element.*);
}

const Verifier = struct {
    alloc: std.mem.Allocator,
    program: *const ir.Program,
    f: *const ir.Function,
    found: ?Fault = null,
    block: ?BlockId = null,
    inst: ?u32 = null,

    preds: [][]BlockId = &.{},
    rpo: []BlockId = &.{},
    idom: []u32 = &.{},
    arena_kind: []?ArenaKind = &.{},
    region: []?Region = &.{},

    fn fail(self: *Verifier, comptime fmt: []const u8, args: anytype) Error {
        self.found = .{
            .block = self.block,
            .inst = self.inst,
            .message = try std.fmt.allocPrint(self.alloc, fmt, args),
        };
        return error.Fault;
    }

    fn typeOf(self: *const Verifier, v: ValueId) Type {
        return self.f.typeOf(v);
    }

    fn run(self: *Verifier) Error!void {
        try self.checkShape();
        try self.computeDominators();
        try self.checkDominance();
        try self.checkTypes();
        try self.computeArenas();
        try self.computeRegions();
        try self.checkArenaFlow();
        try self.checkAliases();
    }

    // ── Shape ──

    fn checkShape(self: *Verifier) Error!void {
        const f = self.f;
        if (f.blocks.len == 0) return self.fail("has no blocks", .{});
        if (f.roles.len != f.params().len) return self.fail("has {d} parameter roles for {d} parameters", .{ f.roles.len, f.params().len });
        for (f.params(), f.roles, 0..) |param, role, i| {
            const ty = self.typeOf(param);
            switch (role) {
                .value => if (ty != .doxa) return self.fail("parameter {d} is a value of type {s}", .{ i, @tagName(ty) }),
                .caller_arena, .alias_arena => if (ty != .arena) return self.fail("parameter {d} is an arena but has type {s}", .{ i, @tagName(ty) }),
                .alias => |a| {
                    if (ty != .ref) return self.fail("^ parameter {d} has type {s}", .{ i, @tagName(ty) });
                    if (a.arena >= f.roles.len or f.roles[a.arena] != .alias_arena) return self.fail("^ parameter {d} names parameter {d} as its arena, which is not one", .{ i, a.arena });
                },
            }
        }
        if (ir.isHeapType(f.ret) and f.callerArena() == null) return self.fail("returns a heap value but takes no @caller arena to return it in", .{});

        for (f.values, 0..) |info, n| {
            const id: ValueId = @enumFromInt(n);
            const defined_here = switch (info.def) {
                .param => |p| @intFromEnum(p.block) < f.blocks.len and p.index < f.blocks[@intFromEnum(p.block)].params.len and
                    f.blocks[@intFromEnum(p.block)].params[p.index] == id,
                .inst => |d| @intFromEnum(d.block) < f.blocks.len and d.index < f.blocks[@intFromEnum(d.block)].insts.len and
                    f.blocks[@intFromEnum(d.block)].insts[d.index].result == id,
            };
            if (!defined_here) return self.fail("value %{d} is not defined where its definition says", .{n});
        }
        for (f.blocks[1..], 1..) |block, b| {
            self.block = @enumFromInt(b);
            // The entry block's parameters are the function's, where a ref
            // is what a `^` parameter is.
            for (block.params) |param| {
                if (self.typeOf(param) == .ref) return self.fail("block parameter %{d} is a ref", .{@intFromEnum(param)});
            }
            self.inst = null;
            try block.term.forEachSuccessor(self, checkBlockCall);
        }
        self.block = null;
        for (f.slots, 0..) |slot, i| {
            if (self.typeOf(slot.arena) != .arena) return self.fail("slot s{d}'s arena %{d} is not an arena", .{ i, @intFromEnum(slot.arena) });
        }
    }

    fn checkBlockCall(self: *Verifier, call: ir.BlockCall) Error!void {
        if (@intFromEnum(call.block) >= self.f.blocks.len) return self.fail("jumps to b{d}, which does not exist", .{@intFromEnum(call.block)});
        const params = self.f.blocks[@intFromEnum(call.block)].params;
        if (params.len != call.args.len) return self.fail("passes {d} argument(s) to b{d}, which takes {d}", .{ call.args.len, @intFromEnum(call.block), params.len });
        for (call.args, params) |arg, param| {
            if (!self.typeOf(arg).eql(self.typeOf(param))) {
                return self.fail("passes %{d} to b{d}'s parameter %{d} of another type", .{ @intFromEnum(arg), @intFromEnum(call.block), @intFromEnum(param) });
            }
        }
    }

    // ── Rule 1: dominance ──

    fn computeDominators(self: *Verifier) Error!void {
        const n = self.f.blocks.len;
        const preds = try self.alloc.alloc(std.ArrayListUnmanaged(BlockId), n);
        for (preds) |*p| p.* = .empty;
        for (self.f.blocks, 0..) |block, b| {
            const Collect = struct {
                preds: []std.ArrayListUnmanaged(BlockId),
                from: BlockId,
                alloc: std.mem.Allocator,
                fn add(c: @This(), call: ir.BlockCall) Error!void {
                    try c.preds[@intFromEnum(call.block)].append(c.alloc, c.from);
                }
            };
            try block.term.forEachSuccessor(Collect{ .preds = preds, .from = @enumFromInt(b), .alloc = self.alloc }, Collect.add);
        }
        self.preds = try self.alloc.alloc([]BlockId, n);
        for (preds, self.preds) |p, *out| out.* = p.items;

        // Reverse postorder, iteratively.
        const visited = try self.alloc.alloc(bool, n);
        @memset(visited, false);
        var post: std.ArrayListUnmanaged(BlockId) = .empty;
        const Frame = struct { block: BlockId, succ: std.ArrayListUnmanaged(BlockId), next: usize };
        var stack: std.ArrayListUnmanaged(Frame) = .empty;
        try stack.append(self.alloc, .{ .block = .entry, .succ = try self.successors(.entry), .next = 0 });
        visited[0] = true;
        while (stack.items.len != 0) {
            const top = &stack.items[stack.items.len - 1];
            if (top.next < top.succ.items.len) {
                const s = top.succ.items[top.next];
                top.next += 1;
                if (!visited[@intFromEnum(s)]) {
                    visited[@intFromEnum(s)] = true;
                    try stack.append(self.alloc, .{ .block = s, .succ = try self.successors(s), .next = 0 });
                }
            } else {
                try post.append(self.alloc, top.block);
                _ = stack.pop();
            }
        }
        for (visited, 0..) |seen, b| {
            if (!seen) {
                self.block = @enumFromInt(b);
                return self.fail("no path from the entry reaches this block", .{});
            }
        }
        std.mem.reverse(BlockId, post.items);
        self.rpo = post.items;

        // Cooper, Harvey, Kennedy: "A Simple, Fast Dominance Algorithm".
        const order = try self.alloc.alloc(u32, n);
        for (self.rpo, 0..) |b, i| order[@intFromEnum(b)] = @intCast(i);
        const undefined_idom = std.math.maxInt(u32);
        self.idom = try self.alloc.alloc(u32, n);
        @memset(self.idom, undefined_idom);
        self.idom[0] = 0;
        var changed = true;
        while (changed) {
            changed = false;
            for (self.rpo[1..]) |b| {
                var new_idom: u32 = undefined_idom;
                for (self.preds[@intFromEnum(b)]) |p| {
                    const pi = @intFromEnum(p);
                    if (self.idom[pi] == undefined_idom) continue;
                    if (new_idom == undefined_idom) {
                        new_idom = pi;
                        continue;
                    }
                    var x = pi;
                    var y = new_idom;
                    while (x != y) {
                        while (order[x] > order[y]) x = self.idom[x];
                        while (order[y] > order[x]) y = self.idom[y];
                    }
                    new_idom = x;
                }
                if (self.idom[@intFromEnum(b)] != new_idom) {
                    self.idom[@intFromEnum(b)] = new_idom;
                    changed = true;
                }
            }
        }
    }

    fn successors(self: *Verifier, b: BlockId) Error!std.ArrayListUnmanaged(BlockId) {
        var out: std.ArrayListUnmanaged(BlockId) = .empty;
        const Collect = struct {
            out: *std.ArrayListUnmanaged(BlockId),
            alloc: std.mem.Allocator,
            fn add(c: @This(), call: ir.BlockCall) Error!void {
                try c.out.append(c.alloc, call.block);
            }
        };
        try self.f.blocks[@intFromEnum(b)].term.forEachSuccessor(Collect{ .out = &out, .alloc = self.alloc }, Collect.add);
        return out;
    }

    fn dominates(self: *const Verifier, a: BlockId, b: BlockId) bool {
        var x: u32 = @intFromEnum(b);
        while (true) {
            if (x == @intFromEnum(a)) return true;
            if (x == 0) return false;
            x = self.idom[x];
        }
    }

    const Use = struct {
        v: *Verifier,
        block: BlockId,
        /// The using instruction's index; the block's length for its terminator.
        index: u32,

        fn check(use: Use, value: ValueId) Error!void {
            const self = use.v;
            if (@intFromEnum(value) >= self.f.values.len) return self.fail("uses %{d}, which does not exist", .{@intFromEnum(value)});
            const ok = switch (self.f.values[@intFromEnum(value)].def) {
                .param => |p| self.dominates(p.block, use.block),
                .inst => |d| if (d.block == use.block) d.index < use.index else self.dominates(d.block, use.block),
            };
            if (!ok) return self.fail("uses %{d} where its definition does not dominate", .{@intFromEnum(value)});
        }
    };

    fn checkDominance(self: *Verifier) Error!void {
        for (self.f.blocks, 0..) |block, b| {
            const id: BlockId = @enumFromInt(b);
            self.block = id;
            for (block.insts, 0..) |*inst, i| {
                self.inst = @intCast(i);
                try ir.forEachOperand(&inst.op, Use{ .v = self, .block = id, .index = @intCast(i) }, Use.check);
            }
            self.inst = null;
            try ir.forEachTermOperand(&block.term, Use{ .v = self, .block = id, .index = @intCast(block.insts.len) }, Use.check);
        }
        self.block = null;
    }

    // ── Rules 2 and 3: types ──

    fn checkTypes(self: *Verifier) Error!void {
        for (self.f.blocks, 0..) |block, b| {
            self.block = @enumFromInt(b);
            for (block.insts, 0..) |*inst, i| {
                self.inst = @intCast(i);
                try self.checkInst(inst);
            }
            self.inst = null;
            try self.checkTerminator(&block.term);
        }
        self.block = null;
    }

    fn doxa(self: *Verifier, v: ValueId) Error!HIRType {
        return switch (self.typeOf(v)) {
            .doxa => |t| t,
            else => |t| self.fail("%{d} is a {s}, not a value", .{ @intFromEnum(v), @tagName(t) }),
        };
    }

    fn expect(self: *Verifier, v: ValueId, want: Type) Error!void {
        if (!self.typeOf(v).eql(want)) return self.fail("%{d} has the wrong type for this operand", .{@intFromEnum(v)});
    }

    fn expectDoxa(self: *Verifier, v: ValueId, want: HIRType) Error!void {
        try self.expect(v, .{ .doxa = want });
    }

    fn expectArena(self: *Verifier, v: ValueId) Error!void {
        if (self.typeOf(v) != .arena) return self.fail("%{d} is not an arena", .{@intFromEnum(v)});
    }

    fn expectTag(self: *Verifier, v: ValueId, comptime tag: std.meta.Tag(HIRType)) Error!@FieldType(HIRType, @tagName(tag)) {
        const t = try self.doxa(v);
        if (t != tag) return self.fail("%{d} is not a {s}", .{ @intFromEnum(v), @tagName(tag) });
        return @field(t, @tagName(tag));
    }

    /// The element type of a dynamic array: the only kind whose length an
    /// instruction may change.
    fn expectDynamicArray(self: *Verifier, v: ValueId) Error!HIRType {
        const array = try self.expectTag(v, .Array);
        if (array.size) |size| return self.fail("%{d} is a fixed array of {d}; its length cannot change", .{ @intFromEnum(v), size });
        return array.element.*;
    }

    fn result(self: *Verifier, inst: *const ir.Inst) Error!Type {
        const r = inst.result orelse return self.fail("defines no value, but this instruction yields one", .{});
        return self.typeOf(r);
    }

    fn resultDoxa(self: *Verifier, inst: *const ir.Inst) Error!HIRType {
        return switch (try self.result(inst)) {
            .doxa => |t| t,
            else => |t| self.fail("yields a {s}, not a value", .{@tagName(t)}),
        };
    }

    fn noResult(self: *Verifier, inst: *const ir.Inst) Error!void {
        if (inst.result != null) return self.fail("defines a value, but this instruction yields none", .{});
    }

    fn resultIs(self: *Verifier, inst: *const ir.Inst, want: Type) Error!void {
        if (!(try self.result(inst)).eql(want)) return self.fail("yields a value of the wrong type", .{});
    }

    fn isNumeric(t: HIRType) bool {
        return t == .Int or t == .Byte or t == .Float;
    }

    fn members(self: *Verifier, boxed: HIRType) Error![]const HIRType {
        const count = switch (boxed) {
            .Union => |u| u.members.len,
            .Group => |g| if (g < self.program.group_members.len) self.program.group_members[g].len else 0,
            else => return self.fail("a {s} is not a union or a group", .{@tagName(boxed)}),
        };
        const buf = try self.alloc.alloc(HIRType, count);
        return self.program.boxMembers(boxed, buf) orelse self.fail("a box type with no recorded members", .{});
    }

    fn isMember(self: *Verifier, boxed: HIRType, t: HIRType) Error!bool {
        for (try self.members(boxed)) |m| if (m.eql(t)) return true;
        return false;
    }

    fn structFields(self: *Verifier, sid: ir.StructId) Error![]const HIRType {
        if (sid >= self.program.struct_fields.len) return self.fail("struct S{d} has no recorded fields", .{sid});
        return self.program.struct_fields[sid];
    }

    fn checkCall(self: *Verifier, inst: *const ir.Inst, sig: ir.Signature, args: []const ValueId) Error!void {
        if (args.len != sig.params.len) return self.fail("passes {d} argument(s) to a callee taking {d}", .{ args.len, sig.params.len });
        for (args, sig.params, 0..) |arg, param, i| {
            if (!self.typeOf(arg).eql(param)) return self.fail("argument {d} (%{d}) has the wrong type", .{ i, @intFromEnum(arg) });
        }
        if (sig.ret == .Nothing) return self.noResult(inst);
        try self.resultIs(inst, .{ .doxa = sig.ret });
    }

    fn checkInst(self: *Verifier, inst: *const ir.Inst) Error!void {
        switch (inst.op) {
            .constant => |c| {
                const t = try self.resultDoxa(inst);
                const ok = switch (c) {
                    .int => t == .Int,
                    .byte => t == .Byte,
                    .float => t == .Float,
                    .tetra => t == .Tetra,
                    .nothing => t == .Nothing,
                    .enum_variant => t == .Enum,
                    .string => t == .String,
                    .function => |fid| @intFromEnum(fid) < self.program.functions.len and t == .Function,
                };
                if (!ok) return self.fail("a {s} constant typed as {s}", .{ @tagName(c), @tagName(t) });
            },
            .arith => |a| {
                const t = try self.doxa(a.lhs);
                if (!isNumeric(t)) return self.fail("arithmetic on a {s}", .{@tagName(t)});
                try self.expectDoxa(a.rhs, t);
                try self.resultIs(inst, .{ .doxa = t });
            },
            .neg => |u| {
                const t = try self.doxa(u.operand);
                if (!isNumeric(t)) return self.fail("negates a {s}", .{@tagName(t)});
                try self.resultIs(inst, .{ .doxa = t });
            },
            .convert => |u| {
                const from = try self.doxa(u.operand);
                const to = try self.resultDoxa(inst);
                if (!isNumeric(from) or !isNumeric(to) or from.eql(to)) return self.fail("converts a {s} to a {s}", .{ @tagName(from), @tagName(to) });
            },
            .cmp => |c| {
                const t = try self.doxa(c.lhs);
                try self.expectDoxa(c.rhs, t);
                if (c.op != .Eq and c.op != .Ne and !isNumeric(t) and t != .String) return self.fail("orders values of type {s}", .{@tagName(t)});
                try self.resultIs(inst, .cond);
            },
            .tetra_binary => |t| {
                try self.expectDoxa(t.lhs, .Tetra);
                try self.expectDoxa(t.rhs, .Tetra);
                try self.resultIs(inst, .{ .doxa = .Tetra });
            },
            .tetra_not => |u| {
                try self.expectDoxa(u.operand, .Tetra);
                try self.resultIs(inst, .{ .doxa = .Tetra });
            },
            .tetra_from_cond => |u| {
                try self.expect(u.operand, .cond);
                try self.resultIs(inst, .{ .doxa = .Tetra });
            },
            .tetra_holds => |u| {
                try self.expectDoxa(u.operand, .Tetra);
                try self.resultIs(inst, .cond);
            },
            .cond_not => |u| {
                try self.expect(u.operand, .cond);
                try self.resultIs(inst, .cond);
            },
            .cond_binary => |c| {
                try self.expect(c.lhs, .cond);
                try self.expect(c.rhs, .cond);
                try self.resultIs(inst, .cond);
            },
            .box => |u| {
                const boxed = try self.resultDoxa(inst);
                const member = try self.doxa(u.operand);
                if (!try self.isMember(boxed, member)) return self.fail("boxes a {s} as a type that has no such member", .{@tagName(member)});
            },
            .unbox => |u| {
                const boxed = try self.doxa(u.operand);
                const member = try self.resultDoxa(inst);
                if (!try self.isMember(boxed, member)) return self.fail("unboxes a {s} its box type does not hold", .{@tagName(member)});
            },
            .repack => |u| {
                const from = try self.doxa(u.operand);
                const to = try self.resultDoxa(inst);
                if (from.eql(to)) return self.fail("re-packs a box as its own type", .{});
                var shared = false;
                for (try self.members(from)) |m| {
                    if (try self.isMember(to, m)) shared = true;
                }
                if (!shared) return self.fail("re-packs a box as a type holding none of its members", .{});
            },
            .member_test => |m| {
                const boxed = try self.doxa(m.operand);
                const count = (try self.members(boxed)).len;
                for (m.members) |index| if (index >= count) return self.fail("tests member {d} of a box with {d}", .{ index, count });
                try self.resultIs(inst, .cond);
            },
            .member_index => |u| {
                _ = try self.members(try self.doxa(u.operand));
                try self.resultIs(inst, .{ .doxa = .Int });
            },
            .str_concat => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.lhs, .String);
                try self.expectDoxa(s.rhs, .String);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_len => |u| {
                try self.expectDoxa(u.operand, .String);
                try self.resultIs(inst, .{ .doxa = .Int });
            },
            .str_substring => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.string, .String);
                try self.expectDoxa(s.start, .Int);
                try self.expectDoxa(s.length, .Int);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_last, .str_drop_last => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.string, .String);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_insert => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.string, .String);
                try self.expectDoxa(s.index, .Int);
                try self.expectDoxa(s.insert, .String);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_remove, .str_char => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.string, .String);
                try self.expectDoxa(s.index, .Int);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_find => |s| {
                try self.expectDoxa(s.string, .String);
                try self.expectDoxa(s.needle, .String);
                try self.resultIs(inst, .{ .doxa = .Int });
            },
            .str_to_int, .str_to_float, .str_to_byte => |u| {
                try self.expectDoxa(u.operand, .String);
                const want: HIRType = switch (inst.op) {
                    .str_to_int => .Int,
                    .str_to_float => .Float,
                    else => .Byte,
                };
                try self.resultIs(inst, .{ .doxa = want });
            },
            .str_pack => |s| {
                try self.expectArena(s.arena);
                const bytes = try self.expectTag(s.bytes, .Array);
                if (bytes.element.* != .Byte) return self.fail("packs an array that is not a byte[]", .{});
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .str_unpack => |s| {
                try self.expectArena(s.arena);
                try self.expectDoxa(s.string, .String);
                const element = (try self.resultDoxa(inst));
                if (element != .Array or element.Array.size != null or element.Array.element.* != .Byte) return self.fail("unpacks to something other than a byte[]", .{});
            },
            .to_string => |s| {
                try self.expectArena(s.arena);
                _ = try self.doxa(s.operand);
                try self.resultIs(inst, .{ .doxa = .String });
            },
            .array_new => |a| {
                try self.expectArena(a.arena);
                const t = try self.resultDoxa(inst);
                if (t != .Array) return self.fail("array.new yields a non-array", .{});
                // A fixed array's length is its type's; a dynamic one's is an operand.
                if (t.Array.size == null) {
                    if (a.length) |len| try self.expectDoxa(len, .Int);
                } else if (a.length != null) return self.fail("array.new of a fixed array takes no length", .{});
            },
            .array_from_fixed => |a| {
                try self.expectArena(a.arena);
                const from = try self.expectTag(a.operand, .Array);
                if (from.size == null) return self.fail("array.from_fixed of a dynamic array", .{});
                const to = try self.resultDoxa(inst);
                if (to != .Array or to.Array.size != null) return self.fail("array.from_fixed yields something other than a dynamic array", .{});
                if (!fixedConverts(from.element.*, to.Array.element.*)) return self.fail("array.from_fixed changes the element type", .{});
            },
            .array_range => |a| {
                try self.expectArena(a.arena);
                try self.expectDoxa(a.start, .Int);
                try self.expectDoxa(a.end, .Int);
                const t = try self.resultDoxa(inst);
                if (t != .Array or t.Array.size != null or t.Array.element.* != .Int) return self.fail("a range yields something other than an int[]", .{});
            },
            .array_get => |a| {
                const array = try self.expectTag(a.array, .Array);
                try self.expectDoxa(a.index, .Int);
                try self.resultIs(inst, .{ .doxa = array.element.* });
            },
            .array_set => |a| {
                const array = try self.expectTag(a.array, .Array);
                try self.expectDoxa(a.index, .Int);
                try self.storesAs(a.value, array.element.*);
                try self.noResult(inst);
            },
            .array_len => |u| {
                _ = try self.expectTag(u.operand, .Array);
                try self.resultIs(inst, .{ .doxa = .Int });
            },
            .array_push => |a| {
                const element = try self.expectDynamicArray(a.array);
                try self.storesAs(a.value, element);
                try self.noResult(inst);
            },
            .array_pop => |u| {
                const element = try self.expectDynamicArray(u.operand);
                try self.resultIs(inst, .{ .doxa = element });
            },
            .array_insert => |a| {
                const element = try self.expectDynamicArray(a.array);
                try self.expectDoxa(a.index, .Int);
                try self.storesAs(a.value, element);
                try self.noResult(inst);
            },
            .array_remove => |a| {
                const element = try self.expectDynamicArray(a.array);
                try self.expectDoxa(a.index, .Int);
                try self.resultIs(inst, .{ .doxa = element });
            },
            .array_clear => |u| {
                _ = try self.expectDynamicArray(u.operand);
                try self.noResult(inst);
            },
            .array_slice => |a| {
                try self.expectArena(a.arena);
                const t = try self.doxa(a.array);
                if (t != .Array) return self.fail("slices a {s}", .{@tagName(t)});
                try self.expectDoxa(a.start, .Int);
                try self.expectDoxa(a.length, .Int);
                try self.resultIs(inst, .{ .doxa = .{ .Array = .{ .element = t.Array.element } } });
            },
            .array_concat => |a| {
                try self.expectArena(a.arena);
                const t = try self.doxa(a.lhs);
                if (t != .Array or t.Array.size != null) return self.fail("concatenates a {s}", .{@tagName(t)});
                try self.expectDoxa(a.rhs, t);
                try self.resultIs(inst, .{ .doxa = t });
            },
            .array_find => |a| {
                const array = try self.expectTag(a.array, .Array);
                try self.expectDoxa(a.value, array.element.*);
                try self.resultIs(inst, .{ .doxa = .Int });
            },
            .map_new => |m| {
                try self.expectArena(m.arena);
                const t = try self.resultDoxa(inst);
                if (t != .Map) return self.fail("map.new yields a non-map", .{});
                if (m.else_value) |e| try self.storesAs(e, t.Map.value.*);
            },
            .map_get => |m| {
                const map = try self.expectTag(m.map, .Map);
                try self.expectDoxa(m.key, map.key.*);
                // A map with an `else` yields its value type; one without
                // yields `value | nothing`, which the analyzer typed.
                const t = try self.resultDoxa(inst);
                if (!t.eql(map.value.*) and !(t == .Union and try self.isMember(t, map.value.*) and try self.isMember(t, .Nothing))) {
                    return self.fail("map.get yields neither the value type nor value | nothing", .{});
                }
            },
            .map_set => |m| {
                const map = try self.expectTag(m.map, .Map);
                try self.expectDoxa(m.key, map.key.*);
                try self.storesAs(m.value, map.value.*);
                try self.noResult(inst);
            },
            .struct_new => |s| {
                try self.expectArena(s.arena);
                const t = try self.resultDoxa(inst);
                if (t != .Struct) return self.fail("struct.new yields a non-struct", .{});
                const fields = try self.structFields(t.Struct);
                if (fields.len != s.fields.len) return self.fail("builds a struct of {d} fields from {d} values", .{ fields.len, s.fields.len });
                for (s.fields, fields) |value, field| try self.storesAs(value, field);
            },
            .field_get => |g| {
                const sid = try self.expectTag(g.object, .Struct);
                const fields = try self.structFields(sid);
                if (g.index >= fields.len) return self.fail("reads field {d} of a struct with {d}", .{ g.index, fields.len });
                try self.resultIs(inst, .{ .doxa = fields[g.index] });
            },
            .field_set => |s| {
                const sid = try self.expectTag(s.object, .Struct);
                const fields = try self.structFields(sid);
                if (s.index >= fields.len) return self.fail("writes field {d} of a struct with {d}", .{ s.index, fields.len });
                try self.storesAs(s.value, fields[s.index]);
                try self.noResult(inst);
            },
            .slot_addr => |s| {
                if (@intFromEnum(s) >= self.f.slots.len) return self.fail("addresses slot s{d}, which does not exist", .{@intFromEnum(s)});
                try self.resultIs(inst, .{ .ref = self.f.slots[@intFromEnum(s)].ty });
            },
            .global_addr => |g| {
                if (@intFromEnum(g.global) >= self.program.globals.len) return self.fail("addresses global {d}, which does not exist", .{@intFromEnum(g.global)});
                try self.expectArena(g.root);
                try self.resultIs(inst, .{ .ref = self.program.globals[@intFromEnum(g.global)].ty });
            },
            .array_copy_to_fixed => |c| {
                const fixed = try self.expectTag(c.fixed, .Array);
                const array = try self.expectTag(c.array, .Array);
                if (fixed.size == null or array.size != null or !fixedConverts(fixed.element.*, array.element.*)) {
                    return self.fail("copies a dynamic array into an array that is not its fixed form", .{});
                }
                try self.noResult(inst);
            },
            .load => |u| {
                const t = switch (self.typeOf(u.operand)) {
                    .ref => |t| t,
                    else => return self.fail("loads through %{d}, which is not a ref", .{@intFromEnum(u.operand)}),
                };
                try self.resultIs(inst, .{ .doxa = t });
            },
            .store => |s| {
                const t = switch (self.typeOf(s.ref)) {
                    .ref => |t| t,
                    else => return self.fail("stores through %{d}, which is not a ref", .{@intFromEnum(s.ref)}),
                };
                try self.storesAs(s.value, t);
                try self.noResult(inst);
            },
            .global_load => |g| {
                const info = try self.globalInfo(g);
                try self.resultIs(inst, .{ .doxa = info.ty });
            },
            .global_store => |g| {
                const info = try self.globalInfo(g.global);
                try self.storesAs(g.value, info.ty);
                try self.noResult(inst);
            },
            .root_arena => try self.resultIs(inst, .arena),
            .scope_enter => |s| {
                try self.expectArena(s.parent);
                try self.resultIs(inst, .arena);
            },
            .scope_exit => |s| {
                try self.expectArena(s.arena);
                try self.noResult(inst);
            },
            .scope_reset => |s| {
                try self.expectArena(s.arena);
                try self.resultIs(inst, .arena);
            },
            .clone, .rehome => |c| {
                try self.expectArena(c.arena);
                const t = try self.doxa(c.operand);
                if (!ir.isHeapType(t)) return self.fail("copies a {s}, which owns no heap storage", .{@tagName(t)});
                try self.resultIs(inst, .{ .doxa = t });
            },
            .call => |c| {
                if (@intFromEnum(c.callee) >= self.program.functions.len) return self.fail("calls function {d}, which does not exist", .{@intFromEnum(c.callee)});
                try self.checkCall(inst, self.program.functions[@intFromEnum(c.callee)], c.args);
            },
            .call_zig => |c| {
                if (@intFromEnum(c.callee) >= self.program.zig_functions.len) return self.fail("calls Zig function {d}, which does not exist", .{@intFromEnum(c.callee)});
                try self.checkCall(inst, self.program.zig_functions[@intFromEnum(c.callee)], c.args);
            },
            .print => |u| {
                try self.expectDoxa(u.operand, .String);
                try self.noResult(inst);
            },
            .peek => |p| {
                _ = try self.doxa(p.operand);
                try self.noResult(inst);
            },
        }
    }

    /// Rule 3: a stored value has its destination's exact type.
    fn storesAs(self: *Verifier, value: ValueId, slot: HIRType) Error!void {
        const t = try self.doxa(value);
        if (!t.eql(slot)) return self.fail("stores %{d}, a {s}, where a {s} is held", .{ @intFromEnum(value), @tagName(t), @tagName(slot) });
    }

    fn globalInfo(self: *Verifier, g: ir.GlobalId) Error!ir.Global {
        if (@intFromEnum(g) >= self.program.globals.len) return self.fail("names global {d}, which does not exist", .{@intFromEnum(g)});
        return self.program.globals[@intFromEnum(g)];
    }

    fn checkTerminator(self: *Verifier, term: *const ir.Terminator) Error!void {
        switch (term.*) {
            .jump => {},
            .branch => |b| try self.expect(b.cond, .cond),
            .@"switch" => |s| {
                const t = try self.doxa(s.operand);
                if (t != .Int and t != .Enum and t != .Byte) return self.fail("switches on a {s}", .{@tagName(t)});
                for (s.cases, 0..) |case, i| {
                    for (s.cases[0..i]) |earlier| if (earlier.value == case.value) return self.fail("switch case {d} appears twice", .{case.value});
                }
            },
            .@"return" => |v| {
                if (v) |value| {
                    try self.expectDoxa(value, self.f.ret);
                } else if (self.f.ret != .Nothing) {
                    return self.fail("returns nothing from a function returning a {s}", .{@tagName(self.f.ret)});
                }
            },
            .@"unreachable" => {},
            .panic => |v| try self.expectDoxa(v, .String),
            .exit => |v| try self.expectDoxa(v, .Int),
            .assert_fail => |a| if (a.message) |m| try self.expectDoxa(m, .String),
        }
    }

    // ── Rule 4: arenas ──

    /// What each arena value is, and each opened arena's parent.
    fn computeArenas(self: *Verifier) Error!void {
        const f = self.f;
        self.arena_kind = try self.alloc.alloc(?ArenaKind, f.values.len);
        @memset(self.arena_kind, null);
        for (f.params(), f.roles) |param, role| switch (role) {
            .caller_arena => self.arena_kind[@intFromEnum(param)] = .caller,
            .alias_arena => self.arena_kind[@intFromEnum(param)] = .alias,
            .value, .alias => {},
        };
        for (f.blocks) |block| for (block.insts) |inst| switch (inst.op) {
            .root_arena => self.arena_kind[@intFromEnum(inst.result.?)] = .root,
            .scope_enter => |s| self.arena_kind[@intFromEnum(inst.result.?)] = .{ .opened = s.parent },
            else => {},
        };
        // A reset arena and an arena block parameter share their parent with
        // what they replace. Both may chain, so iterate to a fixpoint.
        var changed = true;
        while (changed) {
            changed = false;
            for (f.blocks, 0..) |block, b| {
                self.block = @enumFromInt(b);
                for (block.insts) |inst| if (inst.op == .scope_reset) {
                    const from = self.arena_kind[@intFromEnum(inst.op.scope_reset.arena)] orelse continue;
                    if (from != .opened) return self.fail("resets an arena it did not open", .{});
                    const r = @intFromEnum(inst.result.?);
                    if (self.arena_kind[r] == null) {
                        self.arena_kind[r] = from;
                        changed = true;
                    }
                };
                for (block.params, 0..) |param, p| {
                    if (self.typeOf(param) != .arena) continue;
                    for (self.preds[b]) |pred| {
                        const kind = (try self.argInto(pred, @enumFromInt(b), p)) orelse continue;
                        const incoming = self.arena_kind[@intFromEnum(kind)] orelse continue;
                        const current = &self.arena_kind[@intFromEnum(param)];
                        if (current.*) |existing| {
                            if (!std.meta.eql(existing, incoming)) return self.fail("arena parameter %{d} merges arenas with different parents", .{@intFromEnum(param)});
                        } else {
                            if (incoming != .opened) return self.fail("arena parameter %{d} merges an arena it did not open", .{@intFromEnum(param)});
                            current.* = incoming;
                            changed = true;
                        }
                    }
                }
            }
        }
        self.block = null;
        for (f.values, 0..) |info, n| {
            if (info.ty == .arena and self.arena_kind[n] == null) return self.fail("arena %{d} has no origin", .{n});
        }
    }

    /// The argument the edge from `pred` passes to parameter `p` of `block`.
    fn argInto(self: *Verifier, pred: BlockId, block: BlockId, p: usize) Error!?ValueId {
        const Find = struct {
            block: BlockId,
            p: usize,
            found: *?ValueId,
            fn visit(c: @This(), call: ir.BlockCall) Error!void {
                if (call.block == c.block) c.found.* = call.args[c.p];
            }
        };
        var found: ?ValueId = null;
        try self.f.blocks[@intFromEnum(pred)].term.forEachSuccessor(Find{ .block = block, .p = p, .found = &found }, Find.visit);
        return found;
    }

    fn parentOf(self: *const Verifier, arena: ValueId) ?ValueId {
        return switch (self.arena_kind[@intFromEnum(arena)].?) {
            .opened => |p| p,
            .root, .caller, .alias => null,
        };
    }

    /// The function-level arena an arena descends from: the root, `@caller`,
    /// or a `^` argument's.
    fn baseOf(self: *const Verifier, arena: ValueId) ValueId {
        var a = arena;
        while (self.parentOf(a)) |p| a = p;
        return a;
    }

    fn isFunctionLevel(self: *const Verifier, arena: ValueId) bool {
        return self.parentOf(arena) == null;
    }

    /// Whether a value living in `region` is sure to outlive the arena
    /// `dest`.
    fn outlives(self: *const Verifier, region: Region, dest: ValueId) bool {
        return switch (region) {
            .static, .root => true,
            .outer => self.arena_kind[@intFromEnum(self.baseOf(dest))].? == .caller,
            .arena => |a| blk: {
                if (self.arena_kind[@intFromEnum(a)].? == .root) break :blk true;
                var d = dest;
                while (true) {
                    if (d == a) break :blk true;
                    d = self.parentOf(d) orelse break :blk false;
                }
            },
        };
    }

    /// Whether a value living in `a` is sure to outlive one living in `b`.
    fn regionOutlives(self: *const Verifier, a: Region, b: Region) bool {
        return switch (b) {
            .static => a == .static,
            .root => a == .static or a == .root or (a == .arena and self.arena_kind[@intFromEnum(a.arena)].? == .root),
            .outer => a == .static or a == .root or a == .outer or (a == .arena and self.arena_kind[@intFromEnum(a.arena)].? == .root),
            .arena => |d| self.outlives(a, d),
        };
    }

    /// The shorter-lived of two regions, or null when neither is sure to
    /// outlive the other.
    fn meet(self: *const Verifier, a: Region, b: Region) ?Region {
        if (self.regionOutlives(a, b)) return b;
        if (self.regionOutlives(b, a)) return a;
        return null;
    }

    /// Where every heap value lives.
    fn computeRegions(self: *Verifier) Error!void {
        const f = self.f;
        self.region = try self.alloc.alloc(?Region, f.values.len);
        @memset(self.region, null);
        for (f.params(), f.roles) |param, role| {
            if (role == .value and self.typeOf(param).isHeap()) self.region[@intFromEnum(param)] = .outer;
        }
        var changed = true;
        while (changed) {
            changed = false;
            for (self.rpo) |b| {
                self.block = b;
                const block = f.blocks[@intFromEnum(b)];
                for (block.params, 0..) |param, p| {
                    if (b == .entry or !self.typeOf(param).isHeap()) continue;
                    var joined: ?Region = null;
                    for (self.preds[@intFromEnum(b)]) |pred| {
                        const arg = (try self.argInto(pred, b, p)) orelse continue;
                        const incoming = self.region[@intFromEnum(arg)] orelse continue;
                        joined = if (joined) |j| self.meet(j, incoming) orelse
                            return self.fail("parameter %{d} merges heap values from unrelated arenas", .{@intFromEnum(param)}) else incoming;
                    }
                    if (joined) |r| {
                        const slot = &self.region[@intFromEnum(param)];
                        if (slot.* == null or !slot.*.?.eql(r)) {
                            slot.* = r;
                            changed = true;
                        }
                    }
                }
                for (block.insts, 0..) |inst, i| {
                    self.inst = @intCast(i);
                    const r = inst.result orelse continue;
                    if (!self.typeOf(r).isHeap()) continue;
                    const region = try self.regionOf(inst) orelse continue;
                    const slot = &self.region[@intFromEnum(r)];
                    if (slot.* == null or !slot.*.?.eql(region)) {
                        slot.* = region;
                        changed = true;
                    }
                }
                self.inst = null;
            }
        }
        self.block = null;
    }

    /// The region of the heap value `inst` defines; null while an operand's
    /// region is still unknown in the fixpoint.
    fn regionOf(self: *Verifier, inst: ir.Inst) Error!?Region {
        return switch (inst.op) {
            .constant => .static,
            .str_concat => |s| .{ .arena = s.arena },
            .str_substring => |s| .{ .arena = s.arena },
            .str_last, .str_drop_last => |s| .{ .arena = s.arena },
            .str_insert => |s| .{ .arena = s.arena },
            .str_remove, .str_char => |s| .{ .arena = s.arena },
            .str_pack => |s| .{ .arena = s.arena },
            .str_unpack => |s| .{ .arena = s.arena },
            .to_string => |s| .{ .arena = s.arena },
            .array_new => |a| .{ .arena = a.arena },
            .array_from_fixed => |a| .{ .arena = a.arena },
            .array_range => |a| .{ .arena = a.arena },
            .array_slice => |a| .{ .arena = a.arena },
            .array_concat => |a| .{ .arena = a.arena },
            .map_new => |m| .{ .arena = m.arena },
            .struct_new => |s| .{ .arena = s.arena },
            .clone, .rehome => |c| .{ .arena = c.arena },
            // An element lives in its container's arena.
            .array_get => |a| self.region[@intFromEnum(a.array)],
            .array_pop => |u| self.region[@intFromEnum(u.operand)],
            .array_remove => |a| self.region[@intFromEnum(a.array)],
            .map_get => |m| self.region[@intFromEnum(m.map)],
            .field_get => |g| self.region[@intFromEnum(g.object)],
            // A box shares its member's storage.
            .box, .unbox, .repack => |u| if (self.typeOf(u.operand).isHeap()) self.region[@intFromEnum(u.operand)] else .static,
            .load => |u| .{ .arena = self.refArena(u.operand) },
            .global_load => .root,
            .call => |c| try self.callResultRegion(self.program.functions[@intFromEnum(c.callee)], c.args),
            .call_zig => |c| try self.callResultRegion(self.program.zig_functions[@intFromEnum(c.callee)], c.args),
            else => self.fail("an instruction that yields no heap value yields one", .{}),
        };
    }

    /// Where a call's heap result lives: the `@caller` arena it was given.
    /// A callee given none allocated nothing its result could hold, so the
    /// result has no heap storage of its own (a box of scalars).
    fn callResultRegion(self: *Verifier, sig: ir.Signature, args: []const ValueId) Error!Region {
        _ = self;
        const index = sig.callerArena() orelse return .static;
        return .{ .arena = args[index] };
    }

    /// The arena that owns the slot a ref addresses.
    fn refArena(self: *const Verifier, ref: ValueId) ValueId {
        return switch (self.f.values[@intFromEnum(ref)].def) {
            .param => |p| self.f.params()[self.f.roles[p.index].alias.arena],
            .inst => |d| switch (self.f.blocks[@intFromEnum(d.block)].insts[d.index].op) {
                .slot_addr => |s| self.f.slots[@intFromEnum(s)].arena,
                .global_addr => |g| g.root,
                else => unreachable, // a ref is a parameter or an address
            },
        };
    }

    const OpenArenas = []const ValueId;

    /// Scopes close innermost first; no value is used outside its arena's
    /// lifetime; stores and returns keep heap values in arenas that outlive
    /// their destinations.
    fn checkArenaFlow(self: *Verifier) Error!void {
        const f = self.f;
        const entry_state = try self.alloc.alloc(?OpenArenas, f.blocks.len);
        @memset(entry_state, null);
        entry_state[0] = &.{};
        var work: std.ArrayListUnmanaged(BlockId) = .empty;
        try work.append(self.alloc, .entry);
        while (work.pop()) |b| {
            self.block = b;
            const block = f.blocks[@intFromEnum(b)];
            var open: std.ArrayListUnmanaged(ValueId) = .empty;
            try open.appendSlice(self.alloc, entry_state[@intFromEnum(b)].?);
            for (block.params) |param| try self.checkLive(open.items, param);
            for (block.insts, 0..) |inst, i| {
                self.inst = @intCast(i);
                try ir.forEachOperand(&inst.op, LiveCheck{ .v = self, .open = open.items }, LiveCheck.check);
                try self.arenaEffect(&open, inst);
            }
            self.inst = null;
            try ir.forEachTermOperand(&block.term, LiveCheck{ .v = self, .open = open.items }, LiveCheck.check);
            switch (block.term) {
                .@"return" => |v| {
                    if (open.items.len != 0) return self.fail("returns with {d} scope(s) still open", .{open.items.len});
                    if (v) |value| if (self.region[@intFromEnum(value)]) |region| {
                        const caller = f.params()[f.callerArena().?];
                        if (!self.outlives(region, caller)) return self.fail("returns %{d}, which does not outlive @caller; rehome or clone it there", .{@intFromEnum(value)});
                    };
                },
                else => {},
            }
            const Flow = struct {
                v: *Verifier,
                open: []const ValueId,
                entry_state: []?OpenArenas,
                work: *std.ArrayListUnmanaged(BlockId),
                fn visit(c: @This(), call: ir.BlockCall) Error!void {
                    const target = c.v.f.blocks[@intFromEnum(call.block)];
                    const state = try c.v.alloc.alloc(ValueId, c.open.len);
                    // An arena passed to an arena parameter is that parameter
                    // in the target.
                    for (c.open, state) |arena, *out| {
                        out.* = arena;
                        for (call.args, target.params) |arg, param| {
                            if (arg == arena and c.v.typeOf(param) == .arena) out.* = param;
                        }
                    }
                    const slot = &c.entry_state[@intFromEnum(call.block)];
                    if (slot.*) |existing| {
                        if (!std.mem.eql(ValueId, existing, state)) return c.v.fail("reaches b{d} with different scopes open than another path", .{@intFromEnum(call.block)});
                    } else {
                        slot.* = state;
                        try c.work.append(c.v.alloc, call.block);
                    }
                }
            };
            try block.term.forEachSuccessor(Flow{ .v = self, .open = open.items, .entry_state = entry_state, .work = &work }, Flow.visit);
        }
        self.block = null;
    }

    const LiveCheck = struct {
        v: *Verifier,
        open: []const ValueId,

        fn check(c: LiveCheck, value: ValueId) Error!void {
            try c.v.checkLive(c.open, value);
        }
    };

    fn isOpen(self: *const Verifier, open: []const ValueId, arena: ValueId) bool {
        if (self.isFunctionLevel(arena)) return true;
        for (open) |o| if (o == arena) return true;
        return false;
    }

    /// `value` may be used: an arena it is or lives in is open.
    fn checkLive(self: *Verifier, open: []const ValueId, value: ValueId) Error!void {
        if (self.typeOf(value) == .arena) {
            if (!self.isOpen(open, value)) return self.fail("uses arena %{d} after it is closed", .{@intFromEnum(value)});
            return;
        }
        const region = self.region[@intFromEnum(value)] orelse return;
        if (region == .arena and !self.isOpen(open, region.arena)) {
            return self.fail("uses %{d}, whose arena %{d} is closed", .{ @intFromEnum(value), @intFromEnum(region.arena) });
        }
    }

    fn arenaEffect(self: *Verifier, open: *std.ArrayListUnmanaged(ValueId), inst: ir.Inst) Error!void {
        switch (inst.op) {
            .scope_enter => |s| {
                if (open.items.len != 0) {
                    if (s.parent != open.items[open.items.len - 1]) return self.fail("opens a scope under %{d}, which is not the innermost open arena", .{@intFromEnum(s.parent)});
                } else if (!self.isFunctionLevel(s.parent)) {
                    return self.fail("opens a scope under %{d}, which is closed", .{@intFromEnum(s.parent)});
                }
                try open.append(self.alloc, inst.result.?);
            },
            .scope_exit => |s| {
                if (open.items.len == 0 or open.items[open.items.len - 1] != s.arena) return self.fail("closes %{d}, which is not the innermost open arena", .{@intFromEnum(s.arena)});
                _ = open.pop();
            },
            .scope_reset => |s| {
                if (open.items.len == 0 or open.items[open.items.len - 1] != s.arena) return self.fail("resets %{d}, which is not the innermost open arena", .{@intFromEnum(s.arena)});
                open.items[open.items.len - 1] = inst.result.?;
            },
            .store => |s| try self.checkStore(s.value, self.refArena(s.ref)),
            .global_store => |g| if (self.typeOf(g.value).isHeap()) {
                const region = self.region[@intFromEnum(g.value)].?;
                if (!self.regionOutlives(region, .root)) return self.fail("stores %{d} into a global without re-homing it to the root arena", .{@intFromEnum(g.value)});
            },
            .slot_addr => |slot| {
                const arena = self.f.slots[@intFromEnum(slot)].arena;
                if (!self.isOpen(open.items, arena)) return self.fail("addresses slot s{d}, whose arena %{d} is closed", .{ @intFromEnum(slot), @intFromEnum(arena) });
            },
            .call => |c| try self.checkCallerArena(self.program.functions[@intFromEnum(c.callee)], c.args, open.items),
            .call_zig => |c| try self.checkCallerArena(self.program.zig_functions[@intFromEnum(c.callee)], c.args, open.items),
            else => {},
        }
    }

    fn checkStore(self: *Verifier, value: ValueId, dest: ValueId) Error!void {
        const region = self.region[@intFromEnum(value)] orelse return;
        if (!self.outlives(region, dest)) {
            return self.fail("stores %{d} where it may outlive its arena; rehome or clone it into %{d}", .{ @intFromEnum(value), @intFromEnum(dest) });
        }
    }

    /// A callee's `@caller` is this function's innermost open arena.
    fn checkCallerArena(self: *Verifier, sig: ir.Signature, args: []const ValueId, open: []const ValueId) Error!void {
        const index = sig.callerArena() orelse return;
        const passed = args[index];
        if (open.len != 0) {
            if (passed != open[open.len - 1]) return self.fail("passes %{d} as @caller, which is not the innermost open arena", .{@intFromEnum(passed)});
        } else {
            const kind = self.arena_kind[@intFromEnum(passed)].?;
            if (kind != .caller and kind != .root) return self.fail("passes %{d} as @caller with no scope open", .{@intFromEnum(passed)});
        }
    }

    // ── Rule 5: aliases ──

    fn checkAliases(self: *Verifier) Error!void {
        for (self.f.blocks, 0..) |block, b| {
            self.block = @enumFromInt(b);
            for (block.insts, 0..) |inst, i| {
                self.inst = @intCast(i);
                switch (inst.op) {
                    .call => |c| try self.checkAliasArgs(self.program.functions[@intFromEnum(c.callee)], c.args),
                    .call_zig => |c| try self.checkAliasArgs(self.program.zig_functions[@intFromEnum(c.callee)], c.args),
                    else => {},
                }
            }
        }
        self.block = null;
        self.inst = null;
    }

    /// Which storage a ref names: a slot of this function, or a `^`
    /// parameter (passed on unchanged).
    const Storage = union(enum) { slot: ir.SlotId, param: u32, global: ir.GlobalId };

    fn storageOf(self: *const Verifier, ref: ValueId) Storage {
        return switch (self.f.values[@intFromEnum(ref)].def) {
            .param => |p| .{ .param = p.index },
            .inst => |d| switch (self.f.blocks[@intFromEnum(d.block)].insts[d.index].op) {
                .slot_addr => |s| .{ .slot = s },
                .global_addr => |g| .{ .global = g.global },
                else => unreachable, // a ref is a parameter or an address
            },
        };
    }

    fn checkAliasArgs(self: *Verifier, sig: ir.Signature, args: []const ValueId) Error!void {
        for (sig.roles, 0..) |role, i| {
            const a = switch (role) {
                .alias => |a| a,
                else => continue,
            };
            const storage = self.storageOf(args[i]);
            for (sig.roles[0..i], 0..) |earlier, j| {
                if (earlier != .alias) continue;
                if (std.meta.eql(self.storageOf(args[j]), storage)) return self.fail("lends one slot to two ^ parameters of one call", .{});
            }
            if (args[a.arena] != self.refArena(args[i])) return self.fail("passes ^ argument {d} with an arena that does not own its slot", .{i});
        }
    }
};
