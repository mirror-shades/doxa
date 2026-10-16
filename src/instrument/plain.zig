//! The copies B would add (`plan/diagnostics-and-observability.md` step 14):
//! B makes a plain store copy, so that only `^` shares. Today a stored value
//! is shared, so B copies exactly where the stored value is one another
//! binding already holds. This reads the plain stores lowering recorded
//! (`Function.plain_stores`) and the element, entry and field stores in the
//! HIR, and writes a `plain` row to `.memory` for each store B would copy:
//!
//!   `plain  <function>  <site>  <type>  binding|element  <mutated>`
//!
//! `mutated` is `1` when the stored value, or what holds it, is mutated
//! somewhere control can reach from where the value is made: those copies
//! keep the two bindings apart, so no elision removes them. The rows with
//! `0` are the ones a move elision might. A mutation in the statement that
//! makes the value is its construction (an array literal's pushes) and does
//! not count. Reaching from where the value is made, not from the store,
//! over-counts the `1` rows by the mutations between the two: a store
//! records no position, only its span. B is not adopted; nothing here
//! changes the program.

const std = @import("std");
const ir = @import("../codegen/hir/register/ir.zig");
const loops = @import("../codegen/hir/register/loops.zig");
const print = @import("../codegen/hir/register/print.zig");

const ValueId = ir.ValueId;

pub fn write(alloc: std.mem.Allocator, w: *std.Io.Writer, module: *const ir.Module, f: *const ir.Function) !void {
    const program = &module.program;
    for (f.plain_stores, 0..) |st, i| {
        if (!mutable(program, f.typeOf(st.value).doxa)) continue;
        if (!held(f, st.value) and !boundBefore(f, st.value, i)) continue;
        try row(alloc, w, module, f, st.span, st.value, "binding");
    }
    for (f.blocks) |block| {
        for (block.insts) |inst| {
            const st = loops.storeInto(inst.op) orelse continue;
            if (!mutable(program, f.typeOf(st.value).doxa)) continue;
            if (!held(f, st.value) and !bound(f, st.value)) continue;
            try row(alloc, w, module, f, inst.span, st.value, "element");
        }
    }
}

fn row(alloc: std.mem.Allocator, w: *std.Io.Writer, module: *const ir.Module, f: *const ir.Function, span: ir.SrcSpan, value: ValueId, kind: []const u8) !void {
    try w.print("plain\t{s}\t{s}:{d}:{d}\t", .{ f.display_name, module.program.files[span.module], span.line, span.column });
    try print.writeHIRType(w, &module.program, f.typeOf(value).doxa);
    try w.print("\t{s}\t{d}\n", .{ kind, @intFromBool(try mutatedAfter(alloc, f, value)) });
}

/// Whether a value of `t` can be changed in place: an array, a map, a struct,
/// or a box that can hold one. A string is immutable, so a copy of one is
/// never observable.
fn mutable(program: *const ir.Program, t: ir.HIRType) bool {
    return switch (t) {
        .Array, .Map, .Struct => true,
        .Union => |u| for (u.members) |m| {
            if (mutable(program, m.*)) break true;
        } else false,
        .Group => |gid| if (gid < program.group_members.len) for (program.group_members[gid]) |m| {
            if (mutable(program, m)) break true;
        } else false else true,
        else => false,
    };
}

/// Whether `value` is one some storage already holds: a parameter or a
/// variable's merge, a read of a slot, a global, an element or a field, or
/// a box or view of one.
fn held(f: *const ir.Function, value: ValueId) bool {
    const at = switch (f.values[@intFromEnum(value)].def) {
        .param => return true,
        .inst => |at| at,
    };
    return switch (f.blocks[@intFromEnum(at.block)].insts[at.index].op) {
        .load, .global_load, .field_get, .array_get, .map_get => true,
        .box, .unbox, .repack => |u| held(f, u.operand),
        else => false,
    };
}

/// Whether a binding before plain store `index` already holds `value`.
fn boundBefore(f: *const ir.Function, value: ValueId, index: usize) bool {
    for (f.plain_stores[0..index]) |st| if (st.value == value) return true;
    return false;
}

/// Whether any binding holds `value`.
fn bound(f: *const ir.Function, value: ValueId) bool {
    return boundBefore(f, value, f.plain_stores.len);
}

/// Whether `value`, or a value derived from it (a merge it flows into, a
/// field or element read of it, a box of it, a slot it is stored in), is
/// mutated where control can reach from where `value` is made.
fn mutatedAfter(alloc: std.mem.Allocator, f: *const ir.Function, value: ValueId) !bool {
    // Every value that is `value` or a view into it.
    const views = try alloc.alloc(bool, f.values.len);
    defer alloc.free(views);
    @memset(views, false);
    views[@intFromEnum(value)] = true;
    // Slots a view is stored in: lending one with `^` may mutate it.
    var slots: std.AutoHashMapUnmanaged(ir.SlotId, void) = .empty;
    defer slots.deinit(alloc);
    var changed = true;
    while (changed) {
        changed = false;
        for (f.blocks) |block| {
            for (block.insts) |inst| {
                const derived: ?ValueId = switch (inst.op) {
                    .field_get => |g| if (views[@intFromEnum(g.object)]) inst.result else null,
                    .array_get => |g| if (views[@intFromEnum(g.array)]) inst.result else null,
                    .map_get => |g| if (views[@intFromEnum(g.map)]) inst.result else null,
                    .box, .unbox, .repack => |u| if (views[@intFromEnum(u.operand)]) inst.result else null,
                    .store => |s| blk: {
                        if (!views[@intFromEnum(s.value)]) break :blk null;
                        if (slotOf(f, s.ref)) |slot| {
                            if (!slots.contains(slot)) {
                                try slots.put(alloc, slot, {});
                                changed = true;
                            }
                        }
                        break :blk null;
                    },
                    .load => |u| if (slotOf(f, u.operand)) |slot| (if (slots.contains(slot)) inst.result else null) else null,
                    else => null,
                };
                if (derived) |d| if (!views[@intFromEnum(d)]) {
                    views[@intFromEnum(d)] = true;
                    changed = true;
                };
            }
            // A merge a view flows into is the variable holding it.
            const Edge = struct {
                f: *const ir.Function,
                views: []bool,
                changed: *bool,
                fn visit(ctx: @This(), call: ir.BlockCall) error{}!void {
                    const params = ctx.f.blocks[@intFromEnum(call.block)].params;
                    for (call.args, params) |arg, param| {
                        if (ctx.views[@intFromEnum(arg)] and !ctx.views[@intFromEnum(param)]) {
                            ctx.views[@intFromEnum(param)] = true;
                            ctx.changed.* = true;
                        }
                    }
                }
            };
            try block.term.forEachSuccessor(Edge{ .f = f, .views = views, .changed = &changed }, Edge.visit);
        }
    }

    const reach = try reachable(alloc, f, value);
    defer alloc.free(reach.blocks);
    for (f.blocks, 0..) |block, b| {
        for (block.insts, 0..) |inst, i| {
            if (!reach.covers(b, i)) continue;
            if (inst.span.stmt == reach.def_stmt) continue;
            if (mutates(f, inst.op, views, &slots)) return true;
        }
    }
    return false;
}

/// Whether `op` changes a view in place, or lends a slot holding one.
fn mutates(f: *const ir.Function, op: ir.Op, views: []const bool, slots: *const std.AutoHashMapUnmanaged(ir.SlotId, void)) bool {
    const target: ValueId = switch (op) {
        .array_set => |s| s.array,
        .array_push => |s| s.array,
        .array_insert => |s| s.array,
        .array_remove => |s| s.array,
        .array_pop, .array_clear => |u| u.operand,
        .array_copy_to_fixed => |s| s.fixed,
        .map_set => |s| s.map,
        .field_set => |s| s.object,
        .call => |c| return lendsView(f, c.args, slots),
        .call_zig => |c| return lendsView(f, c.args, slots),
        else => return false,
    };
    return views[@intFromEnum(target)];
}

fn lendsView(f: *const ir.Function, args: []const ValueId, slots: *const std.AutoHashMapUnmanaged(ir.SlotId, void)) bool {
    for (args) |arg| if (slotOf(f, arg)) |slot| if (slots.contains(slot)) return true;
    return false;
}

/// The slot a `slot_addr` value addresses.
fn slotOf(f: *const ir.Function, ref: ValueId) ?ir.SlotId {
    const at = switch (f.values[@intFromEnum(ref)].def) {
        .param => return null,
        .inst => |at| at,
    };
    return switch (f.blocks[@intFromEnum(at.block)].insts[at.index].op) {
        .slot_addr => |slot| slot,
        else => null,
    };
}

/// Where control can be once `value` is made: the rest of its block, and
/// every block reachable from it (all of its own block, when a loop returns
/// to it).
const Reach = struct {
    blocks: []bool,
    def_block: usize,
    def_index: usize,
    /// The statement that makes the value, when an instruction does.
    def_stmt: ?u32,

    fn covers(self: Reach, b: usize, i: usize) bool {
        if (self.blocks[b]) return true;
        return b == self.def_block and i >= self.def_index;
    }
};

fn reachable(alloc: std.mem.Allocator, f: *const ir.Function, value: ValueId) !Reach {
    const blocks = try alloc.alloc(bool, f.blocks.len);
    @memset(blocks, false);
    const def_block: usize, const def_index: usize, const def_stmt: ?u32 = switch (f.values[@intFromEnum(value)].def) {
        .param => |p| .{ @intFromEnum(p.block), 0, null },
        .inst => |at| .{ @intFromEnum(at.block), at.index, f.blocks[@intFromEnum(at.block)].insts[at.index].span.stmt },
    };
    var stack: std.ArrayListUnmanaged(usize) = .empty;
    defer stack.deinit(alloc);
    const Push = struct {
        alloc: std.mem.Allocator,
        stack: *std.ArrayListUnmanaged(usize),
        fn visit(ctx: @This(), call: ir.BlockCall) std.mem.Allocator.Error!void {
            try ctx.stack.append(ctx.alloc, @intFromEnum(call.block));
        }
    };
    try f.blocks[def_block].term.forEachSuccessor(Push{ .alloc = alloc, .stack = &stack }, Push.visit);
    while (stack.pop()) |b| {
        if (blocks[b]) continue;
        blocks[b] = true;
        try f.blocks[b].term.forEachSuccessor(Push{ .alloc = alloc, .stack = &stack }, Push.visit);
    }
    return .{ .blocks = blocks, .def_block = def_block, .def_index = def_index, .def_stmt = def_stmt };
}
