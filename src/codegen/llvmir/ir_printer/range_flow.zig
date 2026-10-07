const std = @import("std");
const HIR = @import("../../hir/soxa_types.zig");
const HIRInstruction = @import("../../hir/soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("../../hir/soxa_values.zig").HIRValue;
const IntRange = @import("int_range.zig").IntRange;

// ---------------------------------------------------------------------------
// Phase D-1 follow-on — loop-carried and interprocedural value ranges
// ---------------------------------------------------------------------------
//
// Phase D attaches an `IntRange` to each value the emitter produces, but the
// emitter's range bookkeeping is a single linear walk that refuses to trust a
// range recorded in another basic block (the D-0 landing notes explain why: at
// a use inside a loop body the loop-carried store has not been emitted yet, so
// the variable still looks like its pre-loop initializer). That conservatism is
// exactly what keeps `call`'s `sum % 997` on the six-instruction sign-corrected
// shape: `sum` is loop-carried and its range is therefore unknown.
//
// This module answers the two questions the emitter cannot:
//
//   * What is each variable's range at a loop head? — a small fixpoint over the
//     loop body, widened so a monotone accumulator gets a one-sided bound
//     (`sum` from `0` incremented by positive values is `[0, +inf)`).
//   * What does a call return, given its argument ranges? — a straight-line
//     interpreter over the callee body (`leaf_add(a, b) = a + b` becomes an
//     `IntRange.add`, which is what lets the loop fixpoint see `sum` grow).
//
// The interpreter is a *stack machine* mirroring the emitter's own transfer
// exactly, and it shares `int_range.arithRange` so the two cannot drift. It is
// deliberately conservative around anything it does not model: an unsupported
// instruction, an internal branch, or recursion makes the whole function fall
// back to the emitter's existing (sound) behavior rather than risk a wrong
// bound. A producer that forgets a case loses an optimization, never
// correctness — the same contract `IntRange` was built on.

const MAX_DEPTH: usize = 6;
const MAX_FIXPOINT: usize = 16;

/// The analysis only ever fails on allocation; naming the error set breaks the
/// `run` <-> `handleLoop` inference cycle.
const FlowError = std.mem.Allocator.Error;

/// Instruction indices and function boundaries for one whole program. Built
/// once before emission; borrowed by every analysis.
pub const Context = struct {
    alloc: std.mem.Allocator,
    hir: *const HIR.HIRProgram,
    label_index: std.StringHashMap(usize),
    func_start: std.StringHashMap(void),

    pub fn init(alloc: std.mem.Allocator, hir: *const HIR.HIRProgram) !Context {
        var ctx = Context{
            .alloc = alloc,
            .hir = hir,
            .label_index = std.StringHashMap(usize).init(alloc),
            .func_start = std.StringHashMap(void).init(alloc),
        };
        errdefer ctx.deinit();
        for (hir.instructions, 0..) |inst, i| {
            if (inst == .Label) {
                if (!ctx.label_index.contains(inst.Label.name)) {
                    try ctx.label_index.put(inst.Label.name, i);
                }
            }
        }
        for (hir.function_table) |func| {
            try ctx.func_start.put(func.start_label, {});
        }
        return ctx;
    }

    pub fn deinit(self: *Context) void {
        self.label_index.deinit();
        self.func_start.deinit();
    }

    /// The instruction range `[start, end)` of a function: its start label to
    /// the next function's start label (or the end of the stream).
    pub fn funcRange(self: *const Context, func: HIR.HIRProgram.HIRFunction) ?struct { start: usize, end: usize } {
        const start = self.label_index.get(func.start_label) orelse return null;
        var end: usize = self.hir.instructions.len;
        var i: usize = start + 1;
        while (i < self.hir.instructions.len) : (i += 1) {
            const inst = self.hir.instructions[i];
            if (inst == .Label and self.func_start.contains(inst.Label.name)) {
                end = i;
                break;
            }
        }
        return .{ .start = start, .end = end };
    }
};

/// Computes the loop-head variable ranges for one function. Keys are the
/// `loop_start_*` label names, so the emitter can look one up as it emits that
/// label. Returns an empty map (no ranges) for any function the interpreter
/// cannot model — the emitter then behaves exactly as before.
pub fn analyzeLoops(ctx: *const Context, alloc: std.mem.Allocator, func: HIR.HIRProgram.HIRFunction, start: usize, end: usize) FlowError!std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)) {
    var out = std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)).init(alloc);
    var guard = std.StringHashMap(void).init(alloc);
    defer guard.deinit();

    var interp = Interp{
        .ctx = ctx,
        .alloc = alloc,
        .env = std.AutoHashMap(HIR.Slot, IntRange).init(alloc),
        .guard = &guard,
        .loop_envs = &out,
        .allow_return = true,
    };
    defer interp.deinit();

    // The emitter pre-pushes the by-value parameters; mirror that so the
    // parameter `StoreVar`s at the top of the body consume the right slots.
    var p: u32 = 0;
    while (p < func.arity) : (p += 1) try interp.push(.unknown());

    interp.run(start + 1, end) catch {
        deinitLoopEnvs(&out);
        return std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)).init(alloc);
    };
    if (!interp.ok) {
        deinitLoopEnvs(&out);
        return std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)).init(alloc);
    }
    return out;
}

/// The range a call to `func_idx` returns for the given argument ranges, or
/// `null` when the callee cannot be modelled (non-scalar return, recursion,
/// unsupported body). A `null` simply leaves the emitter's result range
/// unknown.
pub fn returnRange(
    ctx: *const Context,
    alloc: std.mem.Allocator,
    func_idx: usize,
    args: []const IntRange,
    guard: *std.StringHashMap(void),
    depth: usize,
) ?IntRange {
    if (depth > MAX_DEPTH) return null;
    if (func_idx >= ctx.hir.function_table.len) return null;
    const func = ctx.hir.function_table[func_idx];
    if (func.return_type != .Int and func.return_type != .Byte) return null;
    if (guard.contains(func.start_label)) return null;
    const range = ctx.funcRange(func) orelse return null;

    guard.put(func.start_label, {}) catch return null;
    defer _ = guard.remove(func.start_label);

    var loop_envs = std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)).init(alloc);
    defer deinitLoopEnvs(&loop_envs);

    var interp = Interp{
        .ctx = ctx,
        .alloc = alloc,
        .env = std.AutoHashMap(HIR.Slot, IntRange).init(alloc),
        .guard = guard,
        .loop_envs = &loop_envs,
        .depth = depth,
        .allow_return = true,
    };
    defer interp.deinit();

    for (args) |r| interp.push(r) catch return null;
    interp.run(range.start + 1, range.end) catch return null;
    if (!interp.ok or !interp.saw_return) return null;
    return interp.return_range;
}

/// A stack-machine abstract interpreter over the arithmetic subset of the HIR.
/// It carries one `IntRange` per operand and per variable, mirroring the
/// emitter's transfer (shared through `arithRange`). Anything it does not model
/// sets `ok = false`.
const Interp = struct {
    ctx: *const Context,
    alloc: std.mem.Allocator,
    env: std.AutoHashMap(HIR.Slot, IntRange),
    stack: std.ArrayListUnmanaged(IntRange) = .empty,
    guard: *std.StringHashMap(void),
    loop_envs: *std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)),
    depth: usize = 0,
    ok: bool = true,
    allow_return: bool,
    saw_return: bool = false,
    return_range: IntRange = .unknown(),

    fn deinit(self: *Interp) void {
        self.env.deinit();
        self.stack.deinit(self.alloc);
    }

    fn fail(self: *Interp) void {
        self.ok = false;
    }

    fn push(self: *Interp, r: IntRange) !void {
        try self.stack.append(self.alloc, r);
    }

    fn pop(self: *Interp) IntRange {
        return self.stack.pop() orelse {
            self.ok = false;
            return .unknown();
        };
    }

    fn run(self: *Interp, start: usize, end: usize) FlowError!void {
        var pc = start;
        while (pc < end) : (pc += 1) {
            const inst = self.ctx.hir.instructions[pc];
            if (inst == .Label) {
                const name = inst.Label.name;
                if (std.mem.startsWith(u8, name, "loop_start")) {
                    const next = (try self.handleLoop(name, pc, end)) orelse {
                        self.ok = false;
                        return;
                    };
                    pc = next - 1;
                    continue;
                }
                continue;
            }
            if (inst == .Return) {
                if (!self.allow_return) {
                    self.ok = false;
                    return;
                }
                const ret = inst.Return;
                if (ret.has_value) {
                    const r = self.pop();
                    if (!self.ok) return;
                    self.return_range = if (self.saw_return) IntRange.hull(self.return_range, r) else r;
                }
                self.saw_return = true;
                return;
            }
            try self.step(inst);
            if (!self.ok) return;
        }
    }

    fn step(self: *Interp, inst: HIRInstruction) FlowError!void {
        switch (inst) {
            .Const => |c| {
                try self.push(constRange(self.ctx.hir.constant_pool[c.constant_id]));
            },
            .Dup => {
                if (self.stack.items.len == 0) return self.fail();
                try self.push(self.stack.items[self.stack.items.len - 1]);
            },
            .Swap => {
                if (self.stack.items.len < 2) return self.fail();
                const n = self.stack.items.len;
                std.mem.swap(IntRange, &self.stack.items[n - 1], &self.stack.items[n - 2]);
            },
            .Pop => _ = self.pop(),
            .LoadVar => |lv| {
                if (lv.scope_kind == .GlobalLocal or lv.scope_kind == .ModuleGlobal or lv.scope_kind == .ImportedModule) {
                    try self.push(.unknown());
                } else {
                    try self.push(self.env.get(lv.slot) orelse .unknown());
                }
            },
            .StoreVar => |sv| {
                const r = self.pop();
                if (!self.ok) return;
                try self.env.put(sv.slot, r);
            },
            .StoreDecl => |sd| {
                const r = self.pop();
                if (!self.ok) return;
                try self.env.put(sd.slot, r);
            },
            .Arith => |a| {
                const rhs = self.pop();
                const lhs = self.pop();
                if (!self.ok) return;
                try self.push(@import("int_range.zig").arithRange(a.op, a.operand_type, lhs, rhs));
            },
            .Compare => {
                _ = self.pop();
                _ = self.pop();
                if (!self.ok) return;
                try self.push(.unknown());
            },
            .LogicalOp => |lop| {
                _ = self.pop();
                if (lop.op != .Not) _ = self.pop();
                if (!self.ok) return;
                try self.push(.unknown());
            },
            .Convert => {
                _ = self.pop();
                if (!self.ok) return;
                try self.push(.unknown());
            },
            .EnterScope, .ExitScope, .ResetScope => {},
            .Call => |c| {
                const n = c.arg_count;
                if (self.stack.items.len < n) return self.fail();
                const base = self.stack.items.len - n;
                const args = self.stack.items[base..];
                const result = if (c.call_kind == .DoxaFunction)
                    returnRange(self.ctx, self.alloc, c.function_index orelse std.math.maxInt(usize), args, self.guard, self.depth + 1)
                else
                    null;
                self.stack.items.len = base;
                try self.push(result orelse .unknown());
            },
            else => self.fail(),
        }
    }

    /// A natural loop headed by `loop_start_*` at `label_pc`. The body is
    /// `[loop_body_*, first back edge)`, which must be straight-line (the body
    /// sub-interpreter fails on any internal branch). Returns the pc just past
    /// `loop_exit_*`, or `null` if the loop cannot be modelled.
    fn handleLoop(self: *Interp, label_name: []const u8, label_pc: usize, end: usize) FlowError!?usize {
        var body_pc: ?usize = null;
        var exit_pc: ?usize = null;
        var i = label_pc + 1;
        while (i < end) : (i += 1) {
            const inst = self.ctx.hir.instructions[i];
            if (inst != .Label) continue;
            const name = inst.Label.name;
            if (body_pc == null and std.mem.startsWith(u8, name, "loop_body")) body_pc = i;
            if (std.mem.startsWith(u8, name, "loop_exit")) {
                exit_pc = i;
                break;
            }
        }
        const body = body_pc orelse return null;
        const exit = exit_pc orelse return null;

        // The last `Jump loop_start` before the exit is the outer back edge;
        // nested loops target their own headers, so they do not match.
        var back: ?usize = null;
        i = body;
        while (i < exit) : (i += 1) {
            const inst = self.ctx.hir.instructions[i];
            if (inst == .Jump and std.mem.eql(u8, inst.Jump.label, label_name)) back = i;
        }
        const back_pc = back orelse return null;

        var iteration: usize = 0;
        while (iteration < MAX_FIXPOINT) : (iteration += 1) {
            var trial = Interp{
                .ctx = self.ctx,
                .alloc = self.alloc,
                .env = try copyEnv(self.alloc, &self.env),
                .guard = self.guard,
                .loop_envs = self.loop_envs,
                .depth = self.depth,
                .allow_return = false,
            };
            defer trial.deinit();
            try trial.run(body, back_pc);
            if (!trial.ok) return null;

            var next = try widenJoinEnv(self.alloc, &self.env, &trial.env);
            const stable = envEqual(&next, &self.env);
            self.env.deinit();
            self.env = next;
            if (stable) break;
        }
        if (iteration == MAX_FIXPOINT) return null;

        try self.loop_envs.put(label_name, try copyEnv(self.alloc, &self.env));
        return exit + 1;
    }
};

fn constRange(v: HIRValue) IntRange {
    return switch (v) {
        .int => |i| IntRange.exact(i),
        .byte => IntRange.below(256),
        else => .unknown(),
    };
}

fn copyEnv(alloc: std.mem.Allocator, src: *const std.AutoHashMap(HIR.Slot, IntRange)) !std.AutoHashMap(HIR.Slot, IntRange) {
    var out = std.AutoHashMap(HIR.Slot, IntRange).init(alloc);
    var it = src.iterator();
    while (it.next()) |e| try out.put(e.key_ptr.*, e.value_ptr.*);
    return out;
}

fn widenJoinEnv(alloc: std.mem.Allocator, old: *const std.AutoHashMap(HIR.Slot, IntRange), new: *const std.AutoHashMap(HIR.Slot, IntRange)) !std.AutoHashMap(HIR.Slot, IntRange) {
    var out = try copyEnv(alloc, old);
    var it = new.iterator();
    while (it.next()) |e| {
        if (out.getPtr(e.key_ptr.*)) |slot| {
            slot.* = widenRange(slot.*, e.value_ptr.*);
        } else {
            try out.put(e.key_ptr.*, e.value_ptr.*);
        }
    }
    return out;
}

/// The join, widened so a bound that moved outward on this iteration becomes
/// unbounded. Each end can widen at most once, which is what makes the loop
/// fixpoint terminate: a non-decreasing accumulator reaches `[init, +inf)`.
fn widenRange(old: IntRange, new: IntRange) IntRange {
    var out = IntRange.hull(old, new);
    if (old.lo_known and out.lo_known and out.lo < old.lo) {
        out.lo = std.math.minInt(i64);
        out.lo_known = false;
    }
    if (old.hi_known and out.hi_known and out.hi > old.hi) {
        out.hi = std.math.maxInt(i64);
        out.hi_known = false;
    }
    out.konst = null;
    return out;
}

fn envEqual(a: *const std.AutoHashMap(HIR.Slot, IntRange), b: *const std.AutoHashMap(HIR.Slot, IntRange)) bool {
    if (a.count() != b.count()) return false;
    var it = a.iterator();
    while (it.next()) |e| {
        const bv = b.get(e.key_ptr.*) orelse return false;
        if (!IntRange.eql(e.value_ptr.*, bv)) return false;
    }
    return true;
}

fn deinitLoopEnvs(map: *std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange))) void {
    var it = map.iterator();
    while (it.next()) |e| e.value_ptr.deinit();
    map.deinit();
}
