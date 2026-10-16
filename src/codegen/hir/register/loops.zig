//! Where the work a loop repeats lands: the join of `SrcSpan.loop`, the loops
//! lowering recorded (`Function.loops`), and the verifier's arena facts.
//! `--emit-complexity` reads it (`src/instrument/complexity.zig`), and memory
//! retention (`plan/diagnostics-and-observability.md`, steps 9 and 13) is to read the same
//! join rather than build its own. Nothing here re-derives a loop or an
//! arena: both are the decisions lowering and the arena pass made.

const std = @import("std");
const ir = @import("ir.zig");
const verify = @import("verify.zig");

const ValueId = ir.ValueId;

/// The storage something allocated or stored in a loop ends up in.
pub const Landing = union(enum) {
    /// The arena of the loop it runs in, or one opened inside it: reset every
    /// iteration.
    iteration,
    /// The body arena of an enclosing loop: reset once per iteration of that
    /// loop, not of this one.
    outer_loop: ir.LoopId,
    /// An arena the function opened outside every loop: reclaimed when the
    /// function returns.
    function,
    /// `@caller`: the caller's arena, where a returned value lives.
    caller,
    /// A `^` argument's arena.
    alias,
    /// The arena of the container it is stored into (`arena_of`).
    held,
    /// The root arena: never reclaimed.
    root,
    /// A parameter's storage, owned outside the function.
    outer,
    /// Static storage; nothing was allocated.
    static,

    /// Whether every iteration of its loop reclaims it.
    pub fn perIteration(self: Landing) bool {
        return self == .iteration;
    }
};

/// Where an allocation in `arena`, made in `loop`, lands.
pub fn landingOf(f: *const ir.Function, facts: *const verify.Facts, loop: ir.LoopId, arena: ValueId) Landing {
    var at = loop;
    while (f.loop(at)) |info| : (at = info.parent) {
        const body = info.arena orelse continue;
        if (descends(facts, arena, body)) return if (at == loop) .iteration else .{ .outer_loop = at };
    }
    return switch (facts.kind(arena)) {
        .opened => .function,
        .caller => .caller,
        .alias => .alias,
        .held => .held,
        .root => .root,
    };
}

/// Where a store in `loop` into a container in `region` places the value.
pub fn landingOfRegion(f: *const ir.Function, facts: *const verify.Facts, loop: ir.LoopId, region: verify.Region) Landing {
    return switch (region) {
        .static => .static,
        .root => .root,
        .outer => .outer,
        .arena => |a| landingOf(f, facts, loop, a),
    };
}

/// Whether `arena` is `body` (any incarnation of it: a reset is the same
/// arena) or opened inside it. Every incarnation of a loop's body arena is a
/// child of the loop's own state arena.
fn descends(facts: *const verify.Facts, arena: ValueId, body: ValueId) bool {
    const state = facts.parentOf(body) orelse return false;
    var x = arena;
    while (facts.parentOf(x)) |p| : (x = p) {
        if (p == state) return true;
    }
    return false;
}

/// The arena `op` allocates its result in, if it allocates.
pub fn allocArena(op: ir.Op) ?ValueId {
    return switch (op) {
        // These name an arena to close or reset, not to allocate in.
        .scope_exit, .scope_reset => null,
        inline else => |payload| blk: {
            const P = @TypeOf(payload);
            if (@typeInfo(P) != .@"struct" or !@hasField(P, "arena")) break :blk null;
            if (@FieldType(P, "arena") != ValueId) break :blk null;
            break :blk payload.arena;
        },
    };
}

/// The container a store places a value in, and the value: element, entry
/// and field stores re-home a heap value into the container's arena.
pub fn storeInto(op: ir.Op) ?struct { container: ValueId, value: ValueId } {
    return switch (op) {
        .array_push => |s| .{ .container = s.array, .value = s.value },
        .array_set => |s| .{ .container = s.array, .value = s.value },
        .array_insert => |s| .{ .container = s.array, .value = s.value },
        .field_set => |s| .{ .container = s.object, .value = s.value },
        else => null,
    };
}

/// How deep `loop` is nested: 1 for an outermost loop.
pub fn depth(f: *const ir.Function, loop: ir.LoopId) u32 {
    var n: u32 = 0;
    var at = loop;
    while (f.loop(at)) |info| : (at = info.parent) n += 1;
    return n;
}
