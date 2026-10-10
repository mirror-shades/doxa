//! Integer value ranges over the register HIR (`plan/register-hir.md`,
//! "The emitter"): a forward pass over a function's values. A block
//! parameter's range is the hull of what every jump passes it, iterated to a
//! fixpoint and widened so a loop-carried accumulator settles on a one-sided
//! bound. A call's range is its callee's return range under the caller's
//! argument ranges, memoized and depth-limited. The emitter reads the result
//! to pick floored-arithmetic shapes and to skip overflow checks the ranges
//! discharge (`int_range.zig`).
//!
//! Every transfer is sound: a value the pass proves nothing about is the whole
//! of `i64`, which only costs a cheaper lowering, never correctness.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const int_range = @import("int_range.zig");

const IntRange = int_range.IntRange;
const ValueId = ir.ValueId;

/// How deep a call chain is followed for a return range.
const max_depth: usize = 6;
/// Iterations before a block parameter's moving bound is widened away.
const widen_after: usize = 2;

pub const Ranges = struct {
    alloc: std.mem.Allocator,
    module: *const ir.Module,
    /// Return ranges by callee and argument ranges.
    summaries: std.ArrayListUnmanaged(Summary) = .empty,
    /// Callees being analyzed, for recursion.
    active: std.ArrayListUnmanaged(ir.FunctionId) = .empty,

    const Summary = struct { callee: ir.FunctionId, args: []const IntRange, ret: IntRange };

    pub fn init(alloc: std.mem.Allocator, module: *const ir.Module) Ranges {
        return .{ .alloc = alloc, .module = module };
    }

    /// The range of every value of `id`, its parameters unknown.
    pub fn function(self: *Ranges, id: ir.FunctionId) ![]const IntRange {
        const f = &self.module.functions[@intFromEnum(id)];
        const params = try self.alloc.alloc(IntRange, f.params().len);
        @memset(params, .unknown());
        return (try self.analyze(id, params, 0)).values;
    }

    const Result = struct { values: []IntRange, ret: IntRange };

    fn analyze(self: *Ranges, id: ir.FunctionId, params: []const IntRange, depth: usize) std.mem.Allocator.Error!Result {
        const f = &self.module.functions[@intFromEnum(id)];
        const values = try self.alloc.alloc(IntRange, f.values.len);
        @memset(values, .unknown());
        // Null until some jump reaches the parameter.
        const seen = try self.alloc.alloc(?IntRange, f.values.len);
        @memset(seen, null);
        const visits = try self.alloc.alloc(usize, f.values.len);
        @memset(visits, 0);
        for (f.params(), params) |p, r| {
            values[@intFromEnum(p)] = r;
            seen[@intFromEnum(p)] = r;
        }

        try self.active.append(self.alloc, id);
        defer _ = self.active.pop();

        const order = try reversePostorder(self.alloc, f);
        var ret: ?IntRange = null;
        var changed = true;
        while (changed) {
            changed = false;
            for (order) |b| {
                const block = f.blocks[@intFromEnum(b)];
                // A block whose parameters no jump has reached yet waits for
                // one; reverse postorder reaches it after its forward edges.
                if (b != .entry and block.params.len != 0 and seen[@intFromEnum(block.params[0])] == null) continue;
                for (block.params) |p| {
                    if (seen[@intFromEnum(p)]) |r| values[@intFromEnum(p)] = r;
                }
                for (block.insts) |inst| {
                    const r = inst.result orelse continue;
                    values[@intFromEnum(r)] = try self.transfer(f, inst.op, values, depth);
                }
                switch (block.term) {
                    .@"return" => |v| if (v) |value| {
                        const r = values[@intFromEnum(value)];
                        ret = if (ret) |prev| IntRange.hull(prev, r) else r;
                    },
                    else => {},
                }
                const Ctx = struct {
                    f: *const ir.Function,
                    values: []IntRange,
                    seen: []?IntRange,
                    visits: []usize,
                    changed: *bool,

                    fn edge(ctx: @This(), call: ir.BlockCall) error{}!void {
                        const target = ctx.f.blocks[@intFromEnum(call.block)];
                        for (target.params, call.args) |p, arg| {
                            const slot = &ctx.seen[@intFromEnum(p)];
                            const incoming = ctx.values[@intFromEnum(arg)];
                            const next = if (slot.*) |old| widen(old, IntRange.hull(old, incoming), ctx.visits[@intFromEnum(p)]) else incoming;
                            if (slot.* == null or !slot.*.?.eql(next)) {
                                slot.* = next;
                                ctx.visits[@intFromEnum(p)] += 1;
                                ctx.changed.* = true;
                            }
                        }
                    }
                };
                block.term.forEachSuccessor(Ctx{ .f = f, .values = values, .seen = seen, .visits = visits, .changed = &changed }, Ctx.edge) catch unreachable;
            }
        }
        return .{ .values = values, .ret = ret orelse .unknown() };
    }

    fn transfer(self: *Ranges, f: *const ir.Function, op: ir.Op, values: []const IntRange, depth: usize) std.mem.Allocator.Error!IntRange {
        const at = struct {
            fn get(vs: []const IntRange, v: ValueId) IntRange {
                return vs[@intFromEnum(v)];
            }
        }.get;
        return switch (op) {
            .constant => |c| switch (c) {
                .int => |v| .exact(v),
                .byte => |v| .exact(v),
                .enum_variant => |v| .exact(v),
                else => .unknown(),
            },
            .arith => |a| switch (f.typeOf(a.lhs).doxa) {
                .Int => int_range.arithRange(a.op, .Int, at(values, a.lhs), at(values, a.rhs)),
                .Byte => int_range.arithRange(a.op, .Byte, at(values, a.lhs), at(values, a.rhs)),
                else => .unknown(),
            },
            .neg => |u| if (f.typeOf(u.operand).doxa == .Int) IntRange.negate(at(values, u.operand)) else .unknown(),
            // A byte widened to an int holds `[0, 256)`.
            .convert => |u| if (f.typeOf(u.operand).eql(.of(.Byte))) IntRange.below(256) else .unknown(),
            .array_len, .str_len, .member_index => .{ .lo = 0, .lo_known = true },
            .call => |c| try self.callRange(c.callee, c.args, values, depth),
            else => .unknown(),
        };
    }

    fn callRange(self: *Ranges, callee: ir.FunctionId, args: []const ValueId, values: []const IntRange, depth: usize) std.mem.Allocator.Error!IntRange {
        const f = &self.module.functions[@intFromEnum(callee)];
        if (f.ret != .Int) return .unknown();
        if (depth >= max_depth) return .unknown();
        for (self.active.items) |a| {
            if (a == callee) return .unknown();
        }
        const arg_ranges = try self.alloc.alloc(IntRange, args.len);
        for (args, arg_ranges) |a, *r| r.* = values[@intFromEnum(a)];
        for (self.summaries.items) |s| {
            if (s.callee != callee or s.args.len != arg_ranges.len) continue;
            const same = for (s.args, arg_ranges) |x, y| {
                if (!x.eql(y)) break false;
            } else true;
            if (same) return s.ret;
        }
        const result = try self.analyze(callee, arg_ranges, depth + 1);
        try self.summaries.append(self.alloc, .{ .callee = callee, .args = arg_ranges, .ret = result.ret });
        return result.ret;
    }
};

/// The blocks of `f` in reverse postorder from the entry.
pub fn reversePostorder(alloc: std.mem.Allocator, f: *const ir.Function) ![]const ir.BlockId {
    const visited = try alloc.alloc(bool, f.blocks.len);
    @memset(visited, false);
    var post: std.ArrayListUnmanaged(ir.BlockId) = .empty;
    const Frame = struct { block: ir.BlockId, succ: []const ir.BlockId, next: usize };
    var stack: std.ArrayListUnmanaged(Frame) = .empty;
    visited[0] = true;
    try stack.append(alloc, .{ .block = .entry, .succ = try successors(alloc, f, .entry), .next = 0 });
    while (stack.items.len > 0) {
        const top = &stack.items[stack.items.len - 1];
        if (top.next < top.succ.len) {
            const s = top.succ[top.next];
            top.next += 1;
            if (!visited[@intFromEnum(s)]) {
                visited[@intFromEnum(s)] = true;
                try stack.append(alloc, .{ .block = s, .succ = try successors(alloc, f, s), .next = 0 });
            }
            continue;
        }
        try post.append(alloc, top.block);
        _ = stack.pop();
    }
    std.mem.reverse(ir.BlockId, post.items);
    return post.items;
}

fn successors(alloc: std.mem.Allocator, f: *const ir.Function, b: ir.BlockId) ![]const ir.BlockId {
    var out: std.ArrayListUnmanaged(ir.BlockId) = .empty;
    const Collect = struct {
        list: *std.ArrayListUnmanaged(ir.BlockId),
        alloc: std.mem.Allocator,
        fn add(ctx: @This(), call: ir.BlockCall) std.mem.Allocator.Error!void {
            try ctx.list.append(ctx.alloc, call.block);
        }
    };
    try f.blocks[@intFromEnum(b)].term.forEachSuccessor(Collect{ .list = &out, .alloc = alloc }, Collect.add);
    return out.items;
}

/// The join of a parameter's old range with a new incoming one, widened once
/// it has moved `widen_after` times: an end that moved outward is unbounded.
/// Each end widens at most once, so the fixpoint terminates.
fn widen(old: IntRange, joined: IntRange, visits: usize) IntRange {
    if (visits < widen_after) return joined;
    var out = joined;
    if (old.lo_known and joined.lo_known and joined.lo < old.lo) out.lo_known = false;
    if (old.hi_known and joined.hi_known and joined.hi > old.hi) out.hi_known = false;
    if (!out.lo_known) out.lo = std.math.minInt(i64);
    if (!out.hi_known) out.hi = std.math.maxInt(i64);
    if (!(old.konst != null and joined.konst == old.konst)) out.konst = null;
    return out;
}
