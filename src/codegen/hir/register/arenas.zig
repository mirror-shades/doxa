//! Arena refinement over the register HIR, run between generation and
//! verification (`plan/register-hir.md`, "Arenas"). The generator places
//! every heap value that may cross into a longer-lived home with a `rehome`;
//! this pass decides what a static region already settles, and drops the
//! arenas nothing allocates in:
//!
//! - **Copies.** A `rehome` of a value whose region outlives the destination
//!   is the value itself; one whose region is an arena the destination
//!   outlives is a `clone`. Any other stays a `rehome`, the one copy decided
//!   at runtime.
//! - **Arenas.** An arena this function opens — its `scope.enter`, the
//!   `scope.reset`s of it, and the block parameters that carry it — is dead
//!   when no instruction names it but those, so the scope is not entered at
//!   all. A function's `@caller` is dead when nothing uses it and the function
//!   returns no heap value; it is dropped from the signature and from every
//!   call, which can leave a caller's own arena dead, so the pass runs to a
//!   fixpoint over the call graph. This replaces the stack HIR's scope-elision
//!   walk.
//! - **Values.** An instruction whose value nothing uses and that has no
//!   effect is removed.
//!
//! What it removes it records on the module (`ir.ArenaDecisions`): each copy
//! kept as the value itself or dropped as dead, each arena and `@caller`
//! dropped. What it keeps is the HIR itself. `--emit-memory` reads both.

const std = @import("std");
const ir = @import("ir.zig");
const verify = @import("verify.zig");

const ValueId = ir.ValueId;
const BlockId = ir.BlockId;
const Alloc = std.mem.Allocator;

pub fn run(alloc: Alloc, module: *ir.Module) Alloc.Error!void {
    const functions = try alloc.dupe(ir.Function, module.functions);
    const signatures = try alloc.dupe(ir.Signature, module.program.functions);
    module.functions = functions;
    module.program.functions = signatures;

    var record: Record = .{ .alloc = alloc };
    var changed = true;
    while (changed) {
        changed = false;
        for (functions, 0..) |*f, i| {
            record.function = @enumFromInt(i);
            if (try refineCopies(alloc, &module.program, f, &record)) |next| {
                f.* = next;
                changed = true;
            }
            if (try removeDeadValues(alloc, f, &record)) |next| {
                f.* = next;
                changed = true;
            }
        }
        if (try dropDeadArenas(alloc, functions, signatures, &record)) changed = true;
    }
    module.arena_decisions = .{
        .removed_copies = record.copies.items,
        .dropped_arenas = record.arenas.items,
        .dropped_callers = record.callers.items,
    };
}

/// The decisions `run` makes, as it makes them.
const Record = struct {
    alloc: Alloc,
    function: ir.FunctionId = @enumFromInt(0),
    copies: std.ArrayListUnmanaged(ir.RemovedCopy) = .empty,
    arenas: std.ArrayListUnmanaged(ir.DroppedArena) = .empty,
    callers: std.ArrayListUnmanaged(ir.FunctionId) = .empty,

    fn copy(self: *Record, f: *const ir.Function, inst: ir.Inst, why: @FieldType(ir.RemovedCopy, "why")) Alloc.Error!void {
        const result = inst.result.?;
        try self.copies.append(self.alloc, .{ .function = self.function, .span = inst.span, .ty = f.typeOf(result).doxa, .why = why, .reason = copyReason(f, result) });
    }
};

/// What the copy `value` is for: the first use of it that needed a copy.
pub fn copyReason(f: *const ir.Function, value: ValueId) ir.CopyReason {
    for (f.blocks) |block| {
        for (block.insts) |inst| {
            const reason: ?ir.CopyReason = switch (inst.op) {
                .store => |st| if (st.value == value) .store else null,
                .global_store => |g| if (g.value == value) .global else null,
                .array_set => |st| if (st.value == value) .element else null,
                .array_push => |st| if (st.value == value) .element else null,
                .array_insert => |st| if (st.value == value) .element else null,
                .field_set => |st| if (st.value == value) .element else null,
                .call => |c| if (std.mem.indexOfScalar(ValueId, c.args, value) != null) .argument else null,
                .call_zig => |c| if (std.mem.indexOfScalar(ValueId, c.args, value) != null) .argument else null,
                else => null,
            };
            if (reason) |r| return r;
        }
        switch (block.term) {
            .@"return" => |r| if (r == value) return .@"return",
            else => {},
        }
        const Edge = struct {
            value: ValueId,
            found: *bool,
            fn visit(ctx: @This(), call: ir.BlockCall) error{}!void {
                if (std.mem.indexOfScalar(ValueId, call.args, ctx.value) != null) ctx.found.* = true;
            }
        };
        var found = false;
        block.term.forEachSuccessor(Edge{ .value = value, .found = &found }, Edge.visit) catch unreachable;
        if (found) return .merge;
    }
    return .other;
}

/// What the arena opened by the `scope_enter` defining `arena` is for.
pub fn arenaRole(f: *const ir.Function, arena: ValueId) ir.ArenaRole {
    for (f.loops) |info| {
        const body = info.arena orelse continue;
        if (body == arena) return .loop_body;
        if (opening(f, body)) |enter| if (enter.parent == arena) return .loop_state;
    }
    const enter = opening(f, arena) orelse return .block;
    return switch (f.values[@intFromEnum(enter.parent)].def) {
        // Opened under a parameter (`@caller`) or the root: the body's.
        .param => .body,
        .inst => |at| if (f.blocks[@intFromEnum(at.block)].insts[at.index].op == .root_arena) .body else .block,
    };
}

fn opening(f: *const ir.Function, arena: ValueId) ?@FieldType(ir.Op, "scope_enter") {
    const at = switch (f.values[@intFromEnum(arena)].def) {
        .inst => |at| at,
        .param => return null,
    };
    return switch (f.blocks[@intFromEnum(at.block)].insts[at.index].op) {
        .scope_enter => |e| e,
        else => null,
    };
}

// ── Copies ──

fn refineCopies(alloc: Alloc, program: *const ir.Program, f: *const ir.Function, record: *Record) Alloc.Error!?ir.Function {
    // A function that does not verify is left for the verifier to report.
    const facts = try verify.facts(alloc, program, f) orelse return null;
    var edit = try Edit.init(alloc, f);
    var any = false;
    for (f.blocks, 0..) |block, b| {
        for (block.insts, 0..) |inst, i| {
            const c = switch (inst.op) {
                .rehome => |c| c,
                else => continue,
            };
            const region = facts.region(c.operand) orelse continue;
            if (facts.outlives(region, c.arena)) {
                // Already placed: the value itself.
                try record.copy(f, inst, .outlives);
                edit.alias[@intFromEnum(inst.result.?)] = c.operand;
                edit.drop_inst[b][i] = true;
                any = true;
            } else if (region == .arena and facts.outlives(.{ .arena = c.arena }, region.arena)) {
                // It lives in an arena the destination outlives: a copy.
                edit.replace[b][i] = .{ .clone = c };
                any = true;
            }
        }
    }
    if (!any) return null;
    return try edit.apply(alloc, f);
}

// ── Values ──

/// An instruction whose value nothing reads, and that does nothing else, is
/// gone: a constant, a read, an allocation. Arithmetic stays, since an
/// overflow traps in a checked build; so does every call and store.
fn isRemovable(op: ir.Op) bool {
    return switch (op) {
        .constant, .root_arena, .slot_addr, .global_addr, .box, .unbox, .repack, .member_test, .member_index,
        .cmp, .tetra_binary, .tetra_not, .tetra_from_cond, .tetra_holds, .cond_not, .cond_binary, .convert,
        .str_concat, .str_len, .str_substring, .str_last, .str_drop_last, .str_find, .str_insert, .str_remove,
        .str_char, .str_to_int, .str_to_float, .str_to_byte, .str_pack, .str_unpack, .to_string,
        .array_new, .array_from_fixed, .array_range, .array_get, .array_len, .array_slice, .array_concat, .array_find,
        .struct_new, .field_get, .load, .global_load, .clone, .rehome,
        => true,
        else => false,
    };
}

fn removeDeadValues(alloc: Alloc, f: *const ir.Function, record: *Record) Alloc.Error!?ir.Function {
    const used = try alloc.alloc(bool, f.values.len);
    var edit = try Edit.init(alloc, f);
    var any = false;
    var changed = true;
    while (changed) {
        changed = false;
        @memset(used, false);
        try markUses(f, used, &edit);
        for (f.blocks, 0..) |block, b| {
            for (block.insts, 0..) |inst, i| {
                if (edit.drop_inst[b][i]) continue;
                const r = inst.result orelse continue;
                if (used[@intFromEnum(r)] or !isRemovable(inst.op)) continue;
                switch (inst.op) {
                    .clone, .rehome => try record.copy(f, inst, .dead),
                    else => {},
                }
                edit.drop_inst[b][i] = true;
                changed = true;
                any = true;
            }
        }
    }
    if (!any) return null;
    // A `debug_value` names a value that is now dead: drop it with the value,
    // or `Edit.apply` keeps a use of a dropped value.
    for (f.blocks, 0..) |block, b| {
        for (block.insts, 0..) |inst, i| {
            if (edit.drop_inst[b][i]) continue;
            if (std.meta.activeTag(inst.op) != .debug_value) continue;
            if (survives(f, &edit, inst.op.debug_value.value)) continue;
            edit.drop_inst[b][i] = true;
        }
    }
    return try edit.apply(alloc, f);
}

/// Whether `v` still has a definition after `edit`: a parameter always does
/// (this pass drops none), an instruction unless it was dropped.
fn survives(f: *const ir.Function, edit: *const Edit, v: ValueId) bool {
    return switch (f.values[@intFromEnum(edit.resolve(v))].def) {
        .param => true,
        .inst => |d| !edit.drop_inst[@intFromEnum(d.block)][d.index],
    };
}

/// Mark every value an instruction or terminator kept by `edit` reads.
fn markUses(f: *const ir.Function, used: []bool, edit: *const Edit) Alloc.Error!void {
    const Mark = struct {
        used: []bool,
        fn mark(self: @This(), v: ValueId) error{}!void {
            self.used[@intFromEnum(v)] = true;
        }
    };
    const m = Mark{ .used = used };
    for (f.slots) |slot| used[@intFromEnum(slot.arena)] = true;
    for (f.blocks, 0..) |block, b| {
        for (block.insts, 0..) |inst, i| {
            if (edit.drop_inst[b][i]) continue;
            // A debug name is not a use: a value only a debugger reads is dead.
            if (std.meta.activeTag(inst.op) == .debug_value) continue;
            ir.forEachOperand(&inst.op, m, Mark.mark) catch unreachable;
        }
        ir.forEachTermOperand(&block.term, m, Mark.mark) catch unreachable;
    }
}

// ── Arenas ──

fn dropDeadArenas(alloc: Alloc, functions: []ir.Function, signatures: []ir.Signature, record: *Record) Alloc.Error!bool {
    // Which callees' `@caller` is dead: the greatest fixpoint. Every
    // candidate starts dead and is revived when its arena is used even with
    // the other dead ones dropped, so a recursive cycle that allocates
    // nothing — each member lending its arena only to the next — loses its
    // arenas together.
    const dead_caller = try alloc.alloc(bool, functions.len);
    for (functions, dead_caller) |*f, *dead| {
        dead.* = f.callerArena() != null and !ir.isHeapType(f.ret);
    }
    const families = try alloc.alloc(Families, functions.len);
    for (functions, families) |*f, *fam| fam.* = try Families.init(alloc, f);
    var changed = true;
    while (changed) {
        changed = false;
        for (functions, families, 0..) |*f, *fam, i| {
            if (!dead_caller[i]) continue;
            fam.markUsed(f, dead_caller, signatures);
            if (!fam.used[fam.of(f.params()[f.callerArena().?])]) continue;
            dead_caller[i] = false;
            changed = true;
        }
    }

    var any = false;
    for (functions, families, 0..) |*f, *fam, i| {
        fam.markUsed(f, dead_caller, signatures);
        var edit = try Edit.init(alloc, f);
        var touched = false;
        for (f.blocks, 0..) |block, b| {
            for (block.params, 0..) |p, k| {
                if (f.typeOf(p) != .arena or fam.used[fam.of(p)]) continue;
                if (b == 0) continue; // the entry's are parameters, handled below
                edit.drop_param[b][k] = true;
                touched = true;
            }
            for (block.insts, 0..) |inst, n| {
                switch (inst.op) {
                    .scope_enter => if (!fam.used[fam.of(inst.result.?)]) {
                        try record.arenas.append(alloc, .{ .function = @enumFromInt(i), .span = inst.span, .role = arenaRole(f, inst.result.?) });
                        edit.drop_inst[b][n] = true;
                        touched = true;
                    },
                    .scope_exit => |s| if (!fam.used[fam.of(s.arena)]) {
                        edit.drop_inst[b][n] = true;
                        touched = true;
                    },
                    .scope_reset => |s| if (!fam.used[fam.of(s.arena)]) {
                        edit.drop_inst[b][n] = true;
                        touched = true;
                    },
                    .call => |c| if (dead_caller[@intFromEnum(c.callee)]) {
                        const index = signatures[@intFromEnum(c.callee)].callerArena().?;
                        const args = try alloc.alloc(ValueId, c.args.len - 1);
                        @memcpy(args[0..index], c.args[0..index]);
                        @memcpy(args[index..], c.args[index + 1 ..]);
                        edit.replace[b][n] = .{ .call = .{ .callee = c.callee, .args = args } };
                        touched = true;
                    },
                    else => {},
                }
            }
        }
        if (dead_caller[i]) {
            try record.callers.append(alloc, @enumFromInt(i));
            edit.drop_param[0][f.callerArena().?] = true;
            touched = true;
        }
        if (!touched) continue;
        f.* = try edit.apply(alloc, f);
        any = true;
    }
    for (signatures, dead_caller) |*sig, dead| {
        if (!dead) continue;
        const index = sig.callerArena().?;
        const roles = try alloc.dupe(ir.ParamRole, try without(alloc, ir.ParamRole, sig.roles, index));
        // A `^` parameter names its arena parameter by index, which shifts
        // past the dropped one.
        for (roles) |*role| switch (role.*) {
            .alias => |*a| if (a.arena > index) {
                a.arena -= 1;
            },
            else => {},
        };
        sig.* = .{
            .params = try without(alloc, ir.Type, sig.params, index),
            .roles = roles,
            .ret = sig.ret,
        };
    }
    return any;
}

fn without(alloc: Alloc, comptime T: type, items: []const T, index: usize) Alloc.Error![]const T {
    const out = try alloc.alloc(T, items.len - 1);
    @memcpy(out[0..index], items[0..index]);
    @memcpy(out[index..], items[index + 1 ..]);
    return out;
}

/// The arena values of a function grouped by the arena they are: an opened
/// arena with its resets and the block parameters that carry it.
pub const Families = struct {
    parent: []u32,
    used: []bool,

    pub fn init(alloc: Alloc, f: *const ir.Function) Alloc.Error!Families {
        const parent = try alloc.alloc(u32, f.values.len);
        for (parent, 0..) |*p, i| p.* = @intCast(i);
        var self = Families{ .parent = parent, .used = try alloc.alloc(bool, f.values.len) };
        for (f.blocks) |block| {
            for (block.insts) |inst| switch (inst.op) {
                .scope_reset => |s| self.join(inst.result.?, s.arena),
                else => {},
            };
            const Join = struct {
                fam: *Families,
                f: *const ir.Function,
                fn edge(ctx: @This(), call: ir.BlockCall) error{}!void {
                    for (ctx.f.blocks[@intFromEnum(call.block)].params, call.args) |p, a| {
                        if (ctx.f.typeOf(p) == .arena) ctx.fam.join(p, a);
                    }
                }
            };
            block.term.forEachSuccessor(Join{ .fam = &self, .f = f }, Join.edge) catch unreachable;
        }
        return self;
    }

    pub fn of(self: *const Families, v: ValueId) u32 {
        var x = @intFromEnum(v);
        while (self.parent[x] != x) x = self.parent[x];
        return x;
    }

    fn join(self: *Families, a: ValueId, b: ValueId) void {
        const ra = self.of(a);
        const rb = self.of(b);
        if (ra != rb) self.parent[ra] = rb;
    }

    /// Which families an instruction names other than to open, reset or
    /// close them; an opened family that is used makes its parent used.
    fn markUsed(self: *Families, f: *const ir.Function, dead_caller: []const bool, signatures: []const ir.Signature) void {
        @memset(self.used, false);
        const Use = struct {
            fam: *Families,
            f: *const ir.Function,
            fn mark(ctx: @This(), v: ValueId) error{}!void {
                if (ctx.f.typeOf(v) == .arena) ctx.fam.used[ctx.fam.of(v)] = true;
            }
        };
        const use = Use{ .fam = self, .f = f };
        for (f.slots) |slot| use.mark(slot.arena) catch unreachable;
        for (f.blocks) |block| {
            for (block.insts) |inst| switch (inst.op) {
                .scope_enter, .scope_exit, .scope_reset, .debug_value => {},
                .call => |c| for (c.args, 0..) |a, k| {
                    const sig = signatures[@intFromEnum(c.callee)];
                    if (dead_caller[@intFromEnum(c.callee)] and k == sig.callerArena().?) continue;
                    use.mark(a) catch unreachable;
                },
                else => ir.forEachOperand(&inst.op, use, Use.mark) catch unreachable,
            };
            // Arguments carried into an arena parameter join a family; any
            // other terminator operand is a use.
            switch (block.term) {
                .@"return" => |v| if (v) |value| use.mark(value) catch unreachable,
                .branch => |br| use.mark(br.cond) catch unreachable,
                .@"switch" => |sw| use.mark(sw.operand) catch unreachable,
                .panic, .exit => |v| use.mark(v) catch unreachable,
                .assert_fail => |a| if (a.message) |m| use.mark(m) catch unreachable,
                .jump, .@"unreachable" => {},
            }
        }
        // A used opened arena keeps the arena it was opened in.
        var changed = true;
        while (changed) {
            changed = false;
            for (f.blocks) |block| for (block.insts) |inst| switch (inst.op) {
                .scope_enter => |s| if (self.used[self.of(inst.result.?)] and !self.used[self.of(s.parent)]) {
                    self.used[self.of(s.parent)] = true;
                    changed = true;
                },
                else => {},
            };
        }
    }
};

// ── Rewriting ──

/// Edits to one function, applied all at once: instructions dropped or
/// replaced, block parameters dropped, and results aliased to another value.
const Edit = struct {
    alias: []ValueId,
    drop_inst: [][]bool,
    replace: [][]?ir.Op,
    drop_param: [][]bool,

    fn init(alloc: Alloc, f: *const ir.Function) Alloc.Error!Edit {
        const alias = try alloc.alloc(ValueId, f.values.len);
        for (alias, 0..) |*a, i| a.* = @enumFromInt(i);
        const drop_inst = try alloc.alloc([]bool, f.blocks.len);
        const replace = try alloc.alloc([]?ir.Op, f.blocks.len);
        const drop_param = try alloc.alloc([]bool, f.blocks.len);
        for (f.blocks, drop_inst, replace, drop_param) |block, *d, *r, *p| {
            d.* = try alloc.alloc(bool, block.insts.len);
            @memset(d.*, false);
            r.* = try alloc.alloc(?ir.Op, block.insts.len);
            @memset(r.*, null);
            p.* = try alloc.alloc(bool, block.params.len);
            @memset(p.*, false);
        }
        return .{ .alias = alias, .drop_inst = drop_inst, .replace = replace, .drop_param = drop_param };
    }

    fn resolve(self: *const Edit, v: ValueId) ValueId {
        var x = v;
        while (self.alias[@intFromEnum(x)] != x) x = self.alias[@intFromEnum(x)];
        return x;
    }

    /// The function with the edits made, its values numbered densely in
    /// block order.
    fn apply(self: *const Edit, alloc: Alloc, f: *const ir.Function) Alloc.Error!ir.Function {
        const none = std.math.maxInt(u32);
        const renumber = try alloc.alloc(u32, f.values.len);
        @memset(renumber, none);
        var values: std.ArrayListUnmanaged(ir.ValueInfo) = .empty;
        for (f.blocks, 0..) |block, b| {
            for (block.params, 0..) |p, k| {
                if (self.drop_param[b][k]) continue;
                renumber[@intFromEnum(p)] = @intCast(values.items.len);
                try values.append(alloc, f.values[@intFromEnum(p)]);
            }
            for (block.insts, 0..) |inst, n| {
                if (self.drop_inst[b][n]) continue;
                const r = inst.result orelse continue;
                renumber[@intFromEnum(r)] = @intCast(values.items.len);
                try values.append(alloc, f.values[@intFromEnum(r)]);
            }
        }

        const Map = struct {
            edit: *const Edit,
            renumber: []const u32,
            fn value(ctx: @This(), v: ValueId) error{}!ValueId {
                const n = ctx.renumber[@intFromEnum(ctx.edit.resolve(v))];
                std.debug.assert(n != std.math.maxInt(u32)); // a kept use of a dropped value
                return @enumFromInt(n);
            }
        };
        const map = Map{ .edit = self, .renumber = renumber };

        const blocks = try alloc.alloc(ir.Block, f.blocks.len);
        var roles: std.ArrayListUnmanaged(ir.ParamRole) = .empty;
        for (f.blocks, blocks, 0..) |block, *out, b| {
            var params: std.ArrayListUnmanaged(ValueId) = .empty;
            for (block.params, 0..) |p, k| {
                if (self.drop_param[b][k]) continue;
                const id = try map.value(p);
                values.items[@intFromEnum(id)].def = .{ .param = .{ .block = @enumFromInt(b), .index = @intCast(params.items.len) } };
                try params.append(alloc, id);
                if (b == 0) try roles.append(alloc, f.roles[k]);
            }
            var insts: std.ArrayListUnmanaged(ir.Inst) = .empty;
            // A dropped statement head hands its mark to the next kept
            // instruction of the same statement, so the statement keeps a
            // place a debugger stops.
            var head: ?u32 = null;
            for (block.insts, 0..) |inst, n| {
                if (self.drop_inst[b][n]) {
                    if (inst.span.is_stmt) head = inst.span.stmt;
                    continue;
                }
                const op = self.replace[b][n] orelse inst.op;
                const result: ?ValueId = if (inst.result) |r| try map.value(r) else null;
                if (result) |r| values.items[@intFromEnum(r)].def = .{ .inst = .{ .block = @enumFromInt(b), .index = @intCast(insts.items.len) } };
                try insts.append(alloc, .{ .op = try ir.mapOperands(alloc, op, map, Map.value), .result = result, .span = inheritHead(inst.span, &head) });
            }
            out.* = .{ .params = params.items, .insts = insts.items, .term = try self.terminator(alloc, f, block.term, map), .term_span = inheritHead(block.term_span, &head) };
        }

        // A role naming its arena parameter by index follows the renumbering.
        const kept = try alloc.alloc(u32, f.roles.len);
        var next: u32 = 0;
        for (f.roles, 0..) |_, k| {
            kept[k] = next;
            if (!self.drop_param[0][k]) next += 1;
        }
        for (roles.items) |*role| switch (role.*) {
            .alias => |*a| a.arena = kept[a.arena],
            else => {},
        };

        const slots = try alloc.alloc(ir.SlotInfo, f.slots.len);
        for (f.slots, slots) |slot, *out| {
            out.* = slot;
            out.arena = try map.value(slot.arena);
        }
        // A loop whose arena was dropped allocates nothing in it.
        const loops = try alloc.alloc(ir.LoopInfo, f.loops.len);
        for (f.loops, loops) |info, *out| {
            out.* = info;
            out.arena = if (info.arena) |a| if (renumber[@intFromEnum(self.resolve(a))] == none) null else try map.value(a) else null;
        }
        // A store of a value the pass removed stores nothing.
        var plain_stores: std.ArrayListUnmanaged(ir.PlainStore) = .empty;
        for (f.plain_stores) |st| {
            if (renumber[@intFromEnum(self.resolve(st.value))] == none) continue;
            try plain_stores.append(alloc, .{ .span = st.span, .value = try map.value(st.value) });
        }
        return .{ .name = f.name, .display_name = f.display_name, .span = f.span, .statements = f.statements, .roles = roles.items, .ret = f.ret, .values = values.items, .blocks = blocks, .slots = slots, .locals = f.locals, .loops = loops, .plain_stores = plain_stores.items };
    }

    fn terminator(self: *const Edit, alloc: Alloc, f: *const ir.Function, term: ir.Terminator, map: anytype) Alloc.Error!ir.Terminator {
        const call = struct {
            fn args(edit: *const Edit, a: Alloc, fun: *const ir.Function, c: ir.BlockCall, m: anytype) Alloc.Error!ir.BlockCall {
                _ = fun;
                var out: std.ArrayListUnmanaged(ValueId) = .empty;
                for (c.args, 0..) |arg, k| {
                    if (edit.drop_param[@intFromEnum(c.block)][k]) continue;
                    try out.append(a, m.value(arg) catch unreachable);
                }
                return .{ .block = c.block, .args = out.items };
            }
        }.args;
        return switch (term) {
            .jump => |c| .{ .jump = try call(self, alloc, f, c, map) },
            .branch => |br| .{ .branch = .{ .cond = map.value(br.cond) catch unreachable, .then = try call(self, alloc, f, br.then, map), .@"else" = try call(self, alloc, f, br.@"else", map) } },
            .@"switch" => |sw| blk: {
                const cases = try alloc.alloc(ir.SwitchCase, sw.cases.len);
                for (sw.cases, cases) |c, *out| out.* = .{ .value = c.value, .target = try call(self, alloc, f, c.target, map) };
                break :blk .{ .@"switch" = .{ .operand = map.value(sw.operand) catch unreachable, .cases = cases, .default = try call(self, alloc, f, sw.default, map) } };
            },
            .@"return" => |v| .{ .@"return" = if (v) |value| map.value(value) catch unreachable else null },
            .@"unreachable" => .@"unreachable",
            .panic => |v| .{ .panic = map.value(v) catch unreachable },
            .exit => |v| .{ .exit = map.value(v) catch unreachable },
            .assert_fail => |a| .{ .assert_fail = .{ .message = if (a.message) |m| map.value(m) catch unreachable else null } },
        };
    }
};

/// `span`, marked as its statement's head when the head was dropped just
/// before it. The mark is spent either way: a statement whose head was the
/// last of its instructions in the block has no other to take it.
fn inheritHead(span: ir.SrcSpan, head: *?u32) ir.SrcSpan {
    var out = span;
    if (head.*) |stmt| {
        if (span.stmt == stmt) out.is_stmt = true;
    }
    head.* = null;
    return out;
}
