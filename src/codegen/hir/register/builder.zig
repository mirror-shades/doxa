//! Builds one register-HIR function. The generator emits into a *current
//! block*; locals that are never address-taken are SSA variables, read and
//! written through `useVar`/`defVar`, and the builder places the block
//! parameters their merges need with the algorithm of Braun et al., "Simple
//! and Efficient Construction of Static Single Assignment Form" (CC 2013) —
//! the one Cranelift's frontend uses, block parameters in place of phis.
//!
//! A block is *sealed* once every jump to it has been emitted; a read in an
//! unsealed block adds a parameter whose arguments are filled in when it is
//! sealed. `finish` drops unreachable blocks, removes parameters every edge
//! passes the same value to, and numbers the remaining values densely.

const std = @import("std");
const ir = @import("ir.zig");

const ValueId = ir.ValueId;
const BlockId = ir.BlockId;
const Type = ir.Type;

pub const Var = enum(u32) { _ };

pub const Error = std.mem.Allocator.Error || error{
    /// A variable is read on a path where it was never written.
    UndefinedVariable,
    /// A read or an instruction in a block no jump reaches.
    DeadBlock,
    /// An instruction emitted after the current block's terminator.
    BlockTerminated,
    /// A jump to a block already sealed.
    SealedTarget,
    /// An explicit parameter declared after a jump to the block.
    LateParam,
    /// A jump whose arguments do not match the target's explicit parameters.
    ArgumentCount,
    /// `finish` with a block left open or unsealed.
    Incomplete,
};

const Edge = struct {
    from: BlockId,
    to: BlockId,
    args: std.ArrayListUnmanaged(ValueId),
};

const PendingTerm = union(enum) {
    jump: u32,
    branch: struct { cond: ValueId, then: u32, @"else": u32 },
    @"switch": struct { operand: ValueId, cases: []const PendingCase, default: u32 },
    @"return": ?ValueId,
    @"unreachable",
    panic: ValueId,
    exit: ValueId,
    assert_fail: struct { message: ?ValueId, location: ir.Location },
};

const PendingCase = struct { value: i64, edge: u32 };

const BlockState = struct {
    params: std.ArrayListUnmanaged(ValueId) = .empty,
    /// How many of `params` the generator declared; the rest are variables.
    explicit_params: u32 = 0,
    insts: std.ArrayListUnmanaged(ir.Inst) = .empty,
    term: ?PendingTerm = null,
    preds: std.ArrayListUnmanaged(u32) = .empty,
    sealed: bool = false,
    incomplete: std.ArrayListUnmanaged(Incomplete) = .empty,

    const Incomplete = struct { variable: Var, param: ValueId };
};

const VarBlock = struct { variable: Var, block: BlockId };

pub const Target = struct {
    block: BlockId,
    args: []const ValueId = &.{},
};

pub const Case = struct {
    value: i64,
    target: Target,
};

pub const FunctionBuilder = struct {
    /// Owns everything the builder makes, and the finished function.
    alloc: std.mem.Allocator,
    name: []const u8,
    return_type: ir.HIRType,
    roles: std.ArrayListUnmanaged(ir.ParamRole) = .empty,
    values: std.ArrayListUnmanaged(ir.ValueInfo) = .empty,
    blocks: std.ArrayListUnmanaged(BlockState) = .empty,
    edges: std.ArrayListUnmanaged(Edge) = .empty,
    slots: std.ArrayListUnmanaged(ir.SlotInfo) = .empty,
    var_types: std.ArrayListUnmanaged(Type) = .empty,
    defs: std.AutoHashMapUnmanaged(VarBlock, ValueId) = .empty,
    current: BlockId = .entry,
    location: ?ir.Location = null,

    /// `alloc` should be an arena: nothing the builder allocates is freed
    /// before the function is.
    pub fn init(alloc: std.mem.Allocator, name: []const u8, return_type: ir.HIRType) Error!FunctionBuilder {
        var self = FunctionBuilder{ .alloc = alloc, .name = name, .return_type = return_type };
        try self.blocks.append(alloc, .{ .sealed = true });
        return self;
    }

    // ── Parameters, blocks, slots ──

    /// A parameter of the function: a parameter of the entry block. All of
    /// them come before anything is emitted.
    pub fn addParam(self: *FunctionBuilder, ty: Type, role: ir.ParamRole) Error!ValueId {
        const entry = &self.blocks.items[0];
        std.debug.assert(entry.insts.items.len == 0);
        const value = try self.newValue(ty, .{ .param = .{ .block = .entry, .index = @intCast(entry.params.items.len) } });
        try entry.params.append(self.alloc, value);
        entry.explicit_params += 1;
        try self.roles.append(self.alloc, role);
        return value;
    }

    pub fn createBlock(self: *FunctionBuilder) Error!BlockId {
        const id: BlockId = @enumFromInt(self.blocks.items.len);
        try self.blocks.append(self.alloc, .{});
        return id;
    }

    /// A parameter the generator passes explicitly: the value of an `if` or
    /// a `match`, a short-circuit result. Declared before any jump to `block`.
    pub fn blockParam(self: *FunctionBuilder, block: BlockId, ty: Type) Error!ValueId {
        const state = self.blockState(block);
        if (state.preds.items.len != 0 or state.params.items.len != state.explicit_params) return error.LateParam;
        const value = try self.newValue(ty, .{ .param = .{ .block = block, .index = @intCast(state.params.items.len) } });
        try state.params.append(self.alloc, value);
        state.explicit_params += 1;
        return value;
    }

    pub fn addSlot(self: *FunctionBuilder, ty: ir.HIRType, arena: ValueId, name: []const u8) Error!ir.SlotId {
        const id: ir.SlotId = @enumFromInt(self.slots.items.len);
        try self.slots.append(self.alloc, .{ .ty = ty, .arena = arena, .name = name });
        return id;
    }

    pub fn switchTo(self: *FunctionBuilder, block: BlockId) void {
        self.current = block;
    }

    pub fn currentBlock(self: *const FunctionBuilder) BlockId {
        return self.current;
    }

    /// The current block has its terminator: the code that follows is dead
    /// until the generator switches to another block.
    pub fn isTerminated(self: *const FunctionBuilder) bool {
        return self.blocks.items[@intFromEnum(self.current)].term != null;
    }

    /// `block` is sealed and nothing jumps to it.
    pub fn isDead(self: *const FunctionBuilder, block: BlockId) bool {
        const s = self.blocks.items[@intFromEnum(block)];
        return block != .entry and s.sealed and s.preds.items.len == 0;
    }

    /// Every jump to `block` has been emitted: fill in the arguments of the
    /// parameters its reads added while it was open.
    pub fn seal(self: *FunctionBuilder, block: BlockId) Error!void {
        const s = self.blockState(block);
        if (s.sealed) return;
        var i: usize = 0;
        while (i < s.incomplete.items.len) : (i += 1) {
            const pending = self.blockState(block).incomplete.items[i];
            try self.fillParam(block, pending.variable);
        }
        self.blockState(block).incomplete.clearRetainingCapacity();
        self.blockState(block).sealed = true;
    }

    // ── Variables ──

    pub fn declareVar(self: *FunctionBuilder, ty: Type) Error!Var {
        const variable: Var = @enumFromInt(self.var_types.items.len);
        try self.var_types.append(self.alloc, ty);
        return variable;
    }

    pub fn defVar(self: *FunctionBuilder, variable: Var, value: ValueId) Error!void {
        try self.defs.put(self.alloc, .{ .variable = variable, .block = self.current }, value);
    }

    pub fn useVar(self: *FunctionBuilder, variable: Var) Error!ValueId {
        return self.useVarIn(variable, self.current);
    }

    fn useVarIn(self: *FunctionBuilder, variable: Var, start: BlockId) Error!ValueId {
        var block = start;
        // A run of sealed single-predecessor blocks is walked, not recursed.
        while (true) {
            if (self.defs.get(.{ .variable = variable, .block = block })) |value| return value;
            const s = self.blockState(block);
            if (!s.sealed) {
                const param = try self.varParam(block, variable);
                try self.blockState(block).incomplete.append(self.alloc, .{ .variable = variable, .param = param });
                return self.cacheDef(variable, start, block, param);
            }
            switch (s.preds.items.len) {
                0 => return if (block == .entry) error.UndefinedVariable else error.DeadBlock,
                1 => block = self.edges.items[s.preds.items[0]].from,
                else => {
                    const param = try self.varParam(block, variable);
                    try self.defs.put(self.alloc, .{ .variable = variable, .block = block }, param);
                    try self.fillParam(block, variable);
                    return self.cacheDef(variable, start, block, param);
                },
            }
        }
    }

    fn cacheDef(self: *FunctionBuilder, variable: Var, start: BlockId, found: BlockId, value: ValueId) Error!ValueId {
        try self.defs.put(self.alloc, .{ .variable = variable, .block = found }, value);
        if (start != found) try self.defs.put(self.alloc, .{ .variable = variable, .block = start }, value);
        return value;
    }

    fn varParam(self: *FunctionBuilder, block: BlockId, variable: Var) Error!ValueId {
        const s = self.blockState(block);
        const value = try self.newValue(self.var_types.items[@intFromEnum(variable)], .{ .param = .{ .block = block, .index = @intCast(s.params.items.len) } });
        try s.params.append(self.alloc, value);
        return value;
    }

    /// Append `variable`'s value on every edge into `block`, for the parameter
    /// just added for it.
    fn fillParam(self: *FunctionBuilder, block: BlockId, variable: Var) Error!void {
        var i: usize = 0;
        while (i < self.blockState(block).preds.items.len) : (i += 1) {
            const edge_index = self.blockState(block).preds.items[i];
            const from = self.edges.items[edge_index].from;
            const value = try self.useVarIn(variable, from);
            try self.edges.items[edge_index].args.append(self.alloc, value);
        }
    }

    // ── Instructions ──

    /// Emit an instruction that defines a value of type `ty`.
    pub fn define(self: *FunctionBuilder, op: ir.Op, ty: Type) Error!ValueId {
        const s = try self.open();
        const result = try self.newValue(ty, .{ .inst = .{ .block = self.current, .index = @intCast(s.insts.items.len) } });
        try self.blockState(self.current).insts.append(self.alloc, .{ .op = op, .result = result });
        return result;
    }

    /// Emit an instruction that defines no value.
    pub fn effect(self: *FunctionBuilder, op: ir.Op) Error!void {
        const s = try self.open();
        try s.insts.append(self.alloc, .{ .op = op, .result = null });
    }

    fn open(self: *FunctionBuilder) Error!*BlockState {
        const s = self.blockState(self.current);
        if (s.term != null) return error.BlockTerminated;
        if (self.isDead(self.current)) return error.DeadBlock;
        return s;
    }

    // ── Terminators ──

    pub fn jump(self: *FunctionBuilder, target: Target) Error!void {
        _ = try self.open();
        const edge = try self.addEdge(target);
        self.blockState(self.current).term = .{ .jump = edge };
    }

    pub fn branch(self: *FunctionBuilder, cond: ValueId, then: Target, @"else": Target) Error!void {
        _ = try self.open();
        const then_edge = try self.addEdge(then);
        const else_edge = try self.addEdge(@"else");
        self.blockState(self.current).term = .{ .branch = .{ .cond = cond, .then = then_edge, .@"else" = else_edge } };
    }

    pub fn switchOn(self: *FunctionBuilder, operand: ValueId, cases: []const Case, default: Target) Error!void {
        _ = try self.open();
        const pending = try self.alloc.alloc(PendingCase, cases.len);
        for (cases, pending) |case, *slot| slot.* = .{ .value = case.value, .edge = try self.addEdge(case.target) };
        const default_edge = try self.addEdge(default);
        self.blockState(self.current).term = .{ .@"switch" = .{ .operand = operand, .cases = pending, .default = default_edge } };
    }

    pub fn ret(self: *FunctionBuilder, result: ?ValueId) Error!void {
        (try self.open()).term = .{ .@"return" = result };
    }

    pub fn unreachableTerm(self: *FunctionBuilder) Error!void {
        (try self.open()).term = .@"unreachable";
    }

    pub fn panic(self: *FunctionBuilder, message: ValueId) Error!void {
        (try self.open()).term = .{ .panic = message };
    }

    pub fn exit(self: *FunctionBuilder, code: ValueId) Error!void {
        (try self.open()).term = .{ .exit = code };
    }

    pub fn assertFail(self: *FunctionBuilder, message: ?ValueId, location: ir.Location) Error!void {
        (try self.open()).term = .{ .assert_fail = .{ .message = message, .location = location } };
    }

    fn addEdge(self: *FunctionBuilder, target: Target) Error!u32 {
        const to = self.blockState(target.block);
        if (to.sealed) return error.SealedTarget;
        if (target.args.len != to.explicit_params) return error.ArgumentCount;
        const index: u32 = @intCast(self.edges.items.len);
        var args: std.ArrayListUnmanaged(ValueId) = .empty;
        try args.appendSlice(self.alloc, target.args);
        try self.edges.append(self.alloc, .{ .from = self.current, .to = target.block, .args = args });
        try self.blockState(target.block).preds.append(self.alloc, index);
        return index;
    }

    // ── Finishing ──

    pub fn finish(self: *FunctionBuilder) Error!ir.Function {
        for (self.blocks.items) |s| {
            if (!s.sealed) return error.Incomplete;
        }
        const reachable = try self.reachableBlocks();
        const alias = try self.removeTrivialParams(reachable);
        return self.compact(reachable, alias);
    }

    fn reachableBlocks(self: *FunctionBuilder) Error![]bool {
        const reachable = try self.alloc.alloc(bool, self.blocks.items.len);
        @memset(reachable, false);
        var work: std.ArrayListUnmanaged(BlockId) = .empty;
        try work.append(self.alloc, .entry);
        reachable[0] = true;
        while (work.pop()) |block| {
            // A block a path reaches is complete; one none reaches is dropped.
            if (self.blocks.items[@intFromEnum(block)].term == null) return error.Incomplete;
            var it = self.successorEdges(block);
            while (it.next()) |edge_index| {
                const to = @intFromEnum(self.edges.items[edge_index].to);
                if (!reachable[to]) {
                    reachable[to] = true;
                    try work.append(self.alloc, @enumFromInt(to));
                }
            }
        }
        return reachable;
    }

    /// A parameter every live edge passes the same value to (or itself) is
    /// that value. Removing one can make another trivial, so repeat until
    /// none is. Returns, per value, what it stands for (itself if kept).
    fn removeTrivialParams(self: *FunctionBuilder, reachable: []const bool) Error![]ValueId {
        const alias = try self.alloc.alloc(ValueId, self.values.items.len);
        for (alias, 0..) |*a, i| a.* = @enumFromInt(i);
        var changed = true;
        while (changed) {
            changed = false;
            for (self.blocks.items[1..], 1..) |*s, block_index| {
                if (!reachable[block_index]) continue;
                var p: usize = 0;
                while (p < s.params.items.len) {
                    const param = s.params.items[p];
                    var same: ?ValueId = null;
                    var trivial = true;
                    for (s.preds.items) |edge_index| {
                        const edge = self.edges.items[edge_index];
                        if (!reachable[@intFromEnum(edge.from)]) continue;
                        const arg = resolve(alias, edge.args.items[p]);
                        if (arg == param) continue;
                        if (same) |v| {
                            if (v != arg) {
                                trivial = false;
                                break;
                            }
                        } else same = arg;
                    }
                    // `same == null`: every live edge passes the parameter
                    // itself, which only a block no path enters can do.
                    if (!trivial or same == null) {
                        p += 1;
                        continue;
                    }
                    alias[@intFromEnum(param)] = same.?;
                    _ = s.params.orderedRemove(p);
                    if (p < s.explicit_params) s.explicit_params -= 1;
                    for (s.preds.items) |edge_index| _ = self.edges.items[edge_index].args.orderedRemove(p);
                    changed = true;
                }
            }
        }
        return alias;
    }

    fn resolve(alias: []const ValueId, start: ValueId) ValueId {
        var v = start;
        while (alias[@intFromEnum(v)] != v) v = alias[@intFromEnum(v)];
        return v;
    }

    /// Number the live values densely, in block order, and build the result.
    fn compact(self: *FunctionBuilder, reachable: []const bool, alias: []const ValueId) Error!ir.Function {
        const none = std.math.maxInt(u32);
        const renumber = try self.alloc.alloc(u32, self.values.items.len);
        @memset(renumber, none);
        const block_number = try self.alloc.alloc(u32, self.blocks.items.len);
        @memset(block_number, none);

        var values: std.ArrayListUnmanaged(ir.ValueInfo) = .empty;
        var live_blocks: u32 = 0;
        for (self.blocks.items, 0..) |_, i| {
            if (!reachable[i]) continue;
            block_number[i] = live_blocks;
            live_blocks += 1;
        }
        for (self.blocks.items, 0..) |s, i| {
            if (!reachable[i]) continue;
            const block: BlockId = @enumFromInt(block_number[i]);
            for (s.params.items, 0..) |param, p| {
                renumber[@intFromEnum(param)] = @intCast(values.items.len);
                var info = self.values.items[@intFromEnum(param)];
                info.def = .{ .param = .{ .block = block, .index = @intCast(p) } };
                try values.append(self.alloc, info);
            }
            for (s.insts.items, 0..) |inst, n| {
                const result = inst.result orelse continue;
                renumber[@intFromEnum(result)] = @intCast(values.items.len);
                var info = self.values.items[@intFromEnum(result)];
                info.def = .{ .inst = .{ .block = block, .index = @intCast(n) } };
                try values.append(self.alloc, info);
            }
        }

        const map = Remap{ .alias = alias, .renumber = renumber, .alloc = self.alloc };
        const blocks = try self.alloc.alloc(ir.Block, live_blocks);
        for (self.blocks.items, 0..) |s, i| {
            if (!reachable[i]) continue;
            const params = try self.alloc.alloc(ValueId, s.params.items.len);
            for (s.params.items, params) |param, *out| out.* = try map.value(param);
            const insts = try self.alloc.alloc(ir.Inst, s.insts.items.len);
            for (s.insts.items, insts) |inst, *out| {
                out.* = .{
                    .op = try map.op(inst.op),
                    .result = if (inst.result) |r| try map.value(r) else null,
                };
            }
            blocks[block_number[i]] = .{
                .params = params,
                .insts = insts,
                .term = try self.terminator(s.term.?, map, block_number),
            };
        }

        const slots = try self.alloc.alloc(ir.SlotInfo, self.slots.items.len);
        for (self.slots.items, slots) |slot, *out| {
            out.* = slot;
            out.arena = try map.value(slot.arena);
        }

        return .{
            .name = self.name,
            .roles = self.roles.items,
            .ret = self.return_type,
            .values = values.items,
            .blocks = blocks,
            .slots = slots,
        };
    }

    fn terminator(self: *FunctionBuilder, term: PendingTerm, map: Remap, block_number: []const u32) Error!ir.Terminator {
        return switch (term) {
            .jump => |edge| .{ .jump = try self.blockCall(edge, map, block_number) },
            .branch => |b| .{ .branch = .{
                .cond = try map.value(b.cond),
                .then = try self.blockCall(b.then, map, block_number),
                .@"else" = try self.blockCall(b.@"else", map, block_number),
            } },
            .@"switch" => |s| blk: {
                const cases = try self.alloc.alloc(ir.SwitchCase, s.cases.len);
                for (s.cases, cases) |case, *out| out.* = .{ .value = case.value, .target = try self.blockCall(case.edge, map, block_number) };
                break :blk .{ .@"switch" = .{
                    .operand = try map.value(s.operand),
                    .cases = cases,
                    .default = try self.blockCall(s.default, map, block_number),
                } };
            },
            .@"return" => |v| .{ .@"return" = if (v) |value_id| try map.value(value_id) else null },
            .@"unreachable" => .@"unreachable",
            .panic => |v| .{ .panic = try map.value(v) },
            .exit => |v| .{ .exit = try map.value(v) },
            .assert_fail => |a| .{ .assert_fail = .{
                .message = if (a.message) |m| try map.value(m) else null,
                .location = a.location,
            } },
        };
    }

    fn blockCall(self: *FunctionBuilder, edge_index: u32, map: Remap, block_number: []const u32) Error!ir.BlockCall {
        const edge = self.edges.items[edge_index];
        const args = try self.alloc.alloc(ValueId, edge.args.items.len);
        for (edge.args.items, args) |arg, *out| out.* = try map.value(arg);
        return .{ .block = @enumFromInt(block_number[@intFromEnum(edge.to)]), .args = args };
    }

    const Remap = struct {
        alias: []const ValueId,
        renumber: []const u32,
        alloc: std.mem.Allocator,

        fn value(self: Remap, v: ValueId) Error!ValueId {
            const n = self.renumber[@intFromEnum(resolve(self.alias, v))];
            // A use of a value whose definition was dropped: an instruction in
            // a live block reads a value only a dead block defines.
            if (n == std.math.maxInt(u32)) return error.DeadBlock;
            return @enumFromInt(n);
        }

        fn op(self: Remap, original: ir.Op) Error!ir.Op {
            var result = original;
            switch (result) {
                inline else => |*payload| {
                    const P = @TypeOf(payload.*);
                    if (P == ValueId) {
                        payload.* = try self.value(payload.*);
                    } else if (@typeInfo(P) == .@"struct") {
                        inline for (std.meta.fields(P)) |field| {
                            const slot = &@field(payload.*, field.name);
                            switch (field.type) {
                                ValueId => slot.* = try self.value(slot.*),
                                ?ValueId => if (slot.*) |v| {
                                    slot.* = try self.value(v);
                                },
                                []const ValueId => {
                                    const out = try self.alloc.alloc(ValueId, slot.len);
                                    for (slot.*, out) |v, *o| o.* = try self.value(v);
                                    slot.* = out;
                                },
                                else => {},
                            }
                        }
                    }
                },
            }
            return result;
        }
    };

    // ── Helpers ──

    fn blockState(self: *FunctionBuilder, block: BlockId) *BlockState {
        return &self.blocks.items[@intFromEnum(block)];
    }

    fn newValue(self: *FunctionBuilder, ty: Type, def: ir.Def) Error!ValueId {
        const id: ValueId = @enumFromInt(self.values.items.len);
        try self.values.append(self.alloc, .{ .ty = ty, .def = def, .location = self.location });
        return id;
    }

    const EdgeIterator = struct {
        term: PendingTerm,
        index: usize = 0,

        fn next(it: *EdgeIterator) ?u32 {
            defer it.index += 1;
            return switch (it.term) {
                .jump => |e| if (it.index == 0) e else null,
                .branch => |b| switch (it.index) {
                    0 => b.then,
                    1 => b.@"else",
                    else => null,
                },
                .@"switch" => |s| if (it.index < s.cases.len) s.cases[it.index].edge else if (it.index == s.cases.len) s.default else null,
                .@"return", .@"unreachable", .panic, .exit, .assert_fail => null,
            };
        }
    };

    fn successorEdges(self: *const FunctionBuilder, block: BlockId) EdgeIterator {
        return .{ .term = self.blocks.items[@intFromEnum(block)].term.? };
    }
};
