const std = @import("std");

/// Scope arenas, the runtime half of the arena memory model
/// (`docs/memory.md`). A scope is a value the program holds: `enter(parent)`
/// opens a child of `parent`, `exit` reclaims it in O(1), and `reset` reclaims
/// everything allocated in it while keeping it open (a loop body). Every
/// allocation names the scope it is made in; there is no implicit current
/// scope. The register HIR states each arena (`plan/register-hir.md`, "Arenas")
/// and its verifier checks that scopes open and close innermost first.
///
/// Exited nodes are kept on a spare list instead of being returned to the OS,
/// so a scoped call costs no page-allocator round trip. A spare node's arena is
/// rewound and keeps at most `retained_bytes` of its buffers, and at most
/// `max_spare_nodes` nodes are kept, so the memory held for reuse is bounded
/// by their product however deep the program once recursed.
const ScopeNode = struct {
    arena: std.heap.ArenaAllocator,
    /// The scope this one was opened in; null for the root.
    parent: ?*ScopeNode,
    /// The set of open scopes, for `ownerOf`, linked through every node
    /// between its `enter` and its `exit`.
    live_prev: ?*ScopeNode,
    live_next: ?*ScopeNode,
};

/// One Windows allocation granule; the page allocator reserves no less.
const retained_bytes = 64 * 1024;
/// Comfortably past ordinary call depth.
const max_spare_nodes = 64;

var spare: ?*ScopeNode = null;
var spare_count: usize = 0;
var live: ?*ScopeNode = null;
var root_node: ?*ScopeNode = null;

/// Opaque handle to a scope: what an `Arena` value of the register HIR is at
/// runtime. Containers record the scope they were allocated in, so a heap
/// element stored into them can be re-homed to the same arena.
pub const Scope = opaque {};

fn nodeOf(scope: *Scope) *ScopeNode {
    return @ptrCast(@alignCast(scope));
}

fn scopeOf(node: *ScopeNode) *Scope {
    return @ptrCast(node);
}

/// The program root scope, where globals live. Created on first use and never
/// exited.
pub fn root() *Scope {
    if (root_node == null) root_node = nodeOf(open(null));
    return scopeOf(root_node.?);
}

/// Open a child of `parent`.
pub fn enter(parent: *Scope) *Scope {
    return open(nodeOf(parent));
}

fn open(parent: ?*ScopeNode) *Scope {
    const node = if (spare) |reused| blk: {
        spare = reused.parent;
        spare_count -= 1;
        break :blk reused;
    } else blk: {
        const fresh = std.heap.page_allocator.create(ScopeNode) catch @panic("scope_arena: OOM");
        fresh.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        break :blk fresh;
    };
    node.parent = parent;
    node.live_prev = null;
    node.live_next = live;
    if (live) |first| first.live_prev = node;
    live = node;
    return scopeOf(node);
}

/// Close `scope`, reclaiming everything allocated in it.
pub fn exit(scope: *Scope) void {
    const node = nodeOf(scope);
    if (node.live_prev) |prev| prev.live_next = node.live_next else live = node.live_next;
    if (node.live_next) |next| next.live_prev = node.live_prev;
    if (spare_count == max_spare_nodes) {
        node.arena.deinit();
        std.heap.page_allocator.destroy(node);
        return;
    }
    // A loop's `reset` keeps its full capacity, so a rewound arena may still
    // hold more than a spare node is allowed to.
    if (!isRewound(&node.arena) or node.arena.queryCapacity() > retained_bytes) {
        _ = node.arena.reset(.{ .retain_with_limit = retained_bytes });
    }
    node.parent = spare;
    spare = node;
    spare_count += 1;
}

/// Reclaim every allocation in `scope` while keeping it open, with its
/// buffers, for reuse: the physical form of a scope whose lifetime repeats
/// (a loop body).
pub fn reset(scope: *Scope) void {
    const node = nodeOf(scope);
    if (!isRewound(&node.arena)) _ = node.arena.reset(.retain_capacity);
}

/// True when nothing has been allocated from `arena` since it was created or
/// last reset, so a reset would leave it exactly as it is. A reset keeps at
/// most one buffer per list and rewinds it to `end_index == 0`; an allocation
/// either advances that index or pushes a fresh buffer onto `used_list`. Most
/// scopes allocate nothing, and this spares them the reset's walk of both
/// buffer lists.
fn isRewound(arena: *const std.heap.ArenaAllocator) bool {
    const first = arena.state.used_list orelse return true;
    return first.next == null and first.end_index == 0;
}

pub fn allocator(scope: *Scope) std.mem.Allocator {
    return nodeOf(scope).arena.allocator();
}

/// True when `child` is `ancestor` or a scope opened inside it: a value in
/// `ancestor` outlives `child`. This is `rehome`'s test, the one copy decided
/// at runtime — for a value whose arena the compiler cannot name.
pub fn isEqualOrDescendant(child: *Scope, ancestor: ?*Scope) bool {
    const anc = nodeOf(ancestor orelse return false);
    var node: ?*ScopeNode = nodeOf(child);
    while (node) |n| : (node = n.parent) {
        if (n == anc) return true;
    }
    return false;
}

/// The open scope whose arena holds `ptr`, or null when none does (the
/// address is on the stack, in a global, or inside an allocation such as a
/// fixed array's buffer that was not handed out by an arena). Walks every open
/// scope's buffers, so it is for the rare paths that need an owner they were
/// not told, not for every allocation.
pub fn ownerOf(ptr: *const anyopaque) ?*Scope {
    const addr = @intFromPtr(ptr);
    var node = live;
    while (node) |n| : (node = n.live_next) {
        var buf = n.arena.state.used_list;
        while (buf) |b| : (buf = b.next) {
            // The low bit of `size` is the arena's resize flag, not size.
            const size = @as(usize, @bitCast(b.size)) & ~@as(usize, 1);
            const start = @intFromPtr(b);
            if (addr >= start and addr < start + size) return scopeOf(n);
        }
    }
    return null;
}

pub fn alloc(scope: *Scope, len: usize, alignment: std.mem.Alignment, ret_addr: usize) [*]u8 {
    // An empty allocation owns no bytes: any aligned, non-null address will
    // do, and the arena is not asked for one.
    if (len == 0) return @ptrFromInt(alignment.toByteUnits());
    return allocator(scope).rawAlloc(len, alignment, ret_addr) orelse @panic("scope_arena: OOM");
}

pub fn create(scope: *Scope, comptime T: type) *T {
    return @ptrCast(@alignCast(alloc(scope, @sizeOf(T), .fromByteUnits(@alignOf(T)), @returnAddress())));
}

pub fn allocSlice(scope: *Scope, comptime T: type, n: usize) []T {
    const raw = alloc(scope, @sizeOf(T) * n, .fromByteUnits(@alignOf(T)), @returnAddress());
    return @as([*]T, @ptrCast(@alignCast(raw)))[0..n];
}

test "reset keeps the scope open for reuse" {
    const scope = enter(root());
    defer exit(scope);

    _ = allocSlice(scope, u8, 128);
    reset(scope);
    try std.testing.expect(isRewound(&nodeOf(scope).arena));
    _ = allocSlice(scope, u8, 128);
}

test "ownerOf finds the scope whose arena holds an allocation" {
    const outer = enter(root());
    defer exit(outer);
    const in_outer = create(outer, u64);

    const inner = enter(outer);
    defer exit(inner);
    const in_inner = create(inner, u64);

    try std.testing.expectEqual(@as(?*Scope, outer), ownerOf(in_outer));
    try std.testing.expectEqual(@as(?*Scope, inner), ownerOf(in_inner));
    var on_stack: u64 = 0;
    try std.testing.expectEqual(@as(?*Scope, null), ownerOf(&on_stack));
}

test "an exited scope is not an owner" {
    const outer = enter(root());
    defer exit(outer);
    const inner = enter(outer);
    const in_inner = create(inner, u64);
    exit(inner);
    // The exited node keeps its rewound buffer on the spare list, but it is
    // no longer open, so nothing in it has an owner.
    try std.testing.expectEqual(@as(?*Scope, null), ownerOf(in_inner));
}

test "a child descends from its parent and the root" {
    const outer = enter(root());
    defer exit(outer);
    const inner = enter(outer);
    defer exit(inner);

    try std.testing.expect(isEqualOrDescendant(inner, outer));
    try std.testing.expect(isEqualOrDescendant(inner, root()));
    try std.testing.expect(isEqualOrDescendant(inner, inner));
    try std.testing.expect(!isEqualOrDescendant(outer, inner));
}

test "an exited scope node is reused by the next enter" {
    const first = enter(root());
    exit(first);

    const second = enter(root());
    defer exit(second);
    try std.testing.expectEqual(first, second);
}

test "a reused scope keeps at most retained_bytes" {
    const first = enter(root());
    _ = allocSlice(first, u8, 4 * retained_bytes);
    exit(first);

    const second = enter(root());
    defer exit(second);
    try std.testing.expect(nodeOf(second).arena.queryCapacity() <= retained_bytes);
}

test "a scope rewound by a loop reset still sheds its excess on exit" {
    const first = enter(root());
    _ = allocSlice(first, u8, 4 * retained_bytes);
    reset(first);
    exit(first);

    const second = enter(root());
    defer exit(second);
    try std.testing.expect(nodeOf(second).arena.queryCapacity() <= retained_bytes);
}

test "the spare list never grows past max_spare_nodes" {
    const depth = max_spare_nodes + 16;
    var scopes: [depth]*Scope = undefined;
    var parent = root();
    for (&scopes) |*scope| {
        scope.* = enter(parent);
        parent = scope.*;
    }
    var i: usize = depth;
    while (i > 0) {
        i -= 1;
        exit(scopes[i]);
    }

    try std.testing.expectEqual(@as(usize, max_spare_nodes), spare_count);
}
