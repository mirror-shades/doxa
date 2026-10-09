const std = @import("std");

/// Scope-arena stack backing the language's "every block is an arena" memory
/// model. `enter` pushes a child arena; `exit` reclaims the top arena in O(1).
/// Heap values are allocated from the current (top) arena and are reclaimed
/// when the scope that allocated them exits.
///
/// Exited nodes are kept on a spare list instead of being returned to the OS,
/// so a scoped call costs no page-allocator round trip. A spare node's arena is
/// rewound and keeps at most `retained_bytes` of its buffers, and at most
/// `max_spare_nodes` nodes are kept, so the memory held for reuse is bounded
/// by their product however deep the program once recursed.
const ScopeNode = struct {
    arena: std.heap.ArenaAllocator,
    prev: ?*ScopeNode,
};

/// One Windows allocation granule; the page allocator reserves no less.
const retained_bytes = 64 * 1024;
/// Comfortably past ordinary call depth.
const max_spare_nodes = 64;

var head: ?*ScopeNode = null;
var spare: ?*ScopeNode = null;
var spare_count: usize = 0;

/// Allocator for the current scope. Lazily creates a root scope so allocations
/// emitted before the first explicit `enter` (e.g. module-level globals) are
/// valid for the program's lifetime.
pub fn allocator() std.mem.Allocator {
    if (head == null) enter();
    return head.?.arena.allocator();
}

/// Allocator for a scope that is still live on the scope stack.
pub fn allocatorInScope(scope: ?*Scope) std.mem.Allocator {
    const node: *ScopeNode = @ptrCast(@alignCast(scope orelse return allocator()));
    return node.arena.allocator();
}

pub fn enter() void {
    const node = if (spare) |reused| blk: {
        spare = reused.prev;
        spare_count -= 1;
        break :blk reused;
    } else blk: {
        const fresh = std.heap.page_allocator.create(ScopeNode) catch @panic("scope_arena: OOM");
        fresh.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        break :blk fresh;
    };
    node.prev = head;
    head = node;
}

pub fn exit() void {
    const node = head orelse return;
    head = node.prev;
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
    node.prev = spare;
    spare = node;
    spare_count += 1;
}

/// Reclaim every allocation in the current scope while keeping the scope node
/// and its arena available for reuse. This is the physical implementation of a
/// lexical scope whose lifetime repeats (for example, a loop body).
pub fn reset() void {
    const node = head orelse return;
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

/// Opaque handle to a scope. Arrays record the scope they were allocated in so
/// heap elements pushed into them can be re-homed to the same arena.
pub const Scope = opaque {};

pub fn currentScope() ?*Scope {
    return if (head) |h| @ptrCast(h) else null;
}

/// The program-root arena: the oldest node on the scope stack. Globals live
/// here; `doxa_program_main` never exits this scope.
pub fn rootScope() ?*Scope {
    if (head == null) enter();
    var node = head;
    while (node) |n| {
        if (n.prev == null) return @ptrCast(n);
        node = n.prev;
    }
    return null;
}

/// True when `child` is `ancestor` or a nested arena under it. Used to skip
/// identity-breaking clones when a heap value already lives in a scope that
/// outlives the destination.
pub fn isEqualOrDescendant(child: ?*Scope, ancestor: ?*Scope) bool {
    const anc: ?*ScopeNode = @ptrCast(@alignCast(ancestor orelse return child == null));
    var node: ?*ScopeNode = @ptrCast(@alignCast(child));
    while (node) |n| {
        if (n == anc) return true;
        node = n.prev;
    }
    return false;
}

/// The live scope whose arena holds `ptr`, or null when no live scope does
/// (the address is on the stack, in a global, inside another allocation such
/// as a fixed array's buffer that was not handed out by an arena, or already
/// freed). Walks every live scope's buffers, so it is for the rare paths that
/// need an owner they were not told, not for every allocation.
pub fn ownerOf(ptr: *const anyopaque) ?*Scope {
    const addr = @intFromPtr(ptr);
    var node = head;
    while (node) |n| : (node = n.prev) {
        var buf = n.arena.state.used_list;
        while (buf) |b| : (buf = b.next) {
            // The low bit of `size` is the arena's resize flag, not size.
            const size = @as(usize, @bitCast(b.size)) & ~@as(usize, 1);
            const start = @intFromPtr(b);
            if (addr >= start and addr < start + size) return @ptrCast(n);
        }
    }
    return null;
}

pub fn scopeAt(levels: usize) ?*Scope {
    var node = head;
    var i: usize = 0;
    while (i < levels) : (i += 1) {
        node = (node orelse return null).prev;
    }
    return if (node) |n| @ptrCast(n) else null;
}

pub fn allocInScope(scope: ?*Scope, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
    const node: *ScopeNode = @ptrCast(@alignCast(scope orelse return null));
    return node.arena.allocator().rawAlloc(len, alignment, ret_addr);
}

/// Allocate from the arena `levels` above the current scope (0 = current).
/// Used to clone a heap value into the scope that a variable was declared in,
/// so it survives the exit of the intervening scopes.
pub fn allocAt(levels: usize, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
    var node = head;
    var i: usize = 0;
    while (i < levels) : (i += 1) {
        node = (node orelse return null).prev;
    }
    const target = node orelse return null;
    return target.arena.allocator().rawAlloc(len, alignment, ret_addr);
}

pub fn createAt(levels: usize, comptime T: type) *T {
    const raw = allocAt(levels, @sizeOf(T), .fromByteUnits(@alignOf(T)), @returnAddress()) orelse @panic("scope_arena: OOM");
    return @ptrCast(@alignCast(raw));
}

pub fn allocSliceAt(levels: usize, comptime T: type, n: usize) []T {
    const raw = allocAt(levels, @sizeOf(T) * n, .fromByteUnits(@alignOf(T)), @returnAddress()) orelse @panic("scope_arena: OOM");
    return @as([*]T, @ptrCast(@alignCast(raw)))[0..n];
}

pub fn createInScope(scope: ?*Scope, comptime T: type) *T {
    const raw = allocInScope(scope, @sizeOf(T), .fromByteUnits(@alignOf(T)), @returnAddress()) orelse @panic("scope_arena: OOM");
    return @ptrCast(@alignCast(raw));
}

pub fn allocSliceInScope(scope: ?*Scope, comptime T: type, n: usize) []T {
    const raw = allocInScope(scope, @sizeOf(T) * n, .fromByteUnits(@alignOf(T)), @returnAddress()) orelse @panic("scope_arena: OOM");
    return @as([*]T, @ptrCast(@alignCast(raw)))[0..n];
}

test "reset reuses the current scope node" {
    enter();
    defer exit();

    const scope = currentScope();
    _ = allocator().alloc(u8, 128) catch unreachable;
    reset();

    try std.testing.expectEqual(scope, currentScope());
    _ = allocator().alloc(u8, 128) catch unreachable;
}

test "ownerOf finds the scope whose arena holds an allocation" {
    enter();
    defer exit();
    const outer = currentScope();
    const in_outer = allocator().create(u64) catch unreachable;

    enter();
    defer exit();
    const in_inner = allocator().create(u64) catch unreachable;

    try std.testing.expectEqual(outer, ownerOf(in_outer));
    try std.testing.expectEqual(currentScope(), ownerOf(in_inner));
    var on_stack: u64 = 0;
    try std.testing.expectEqual(@as(?*Scope, null), ownerOf(&on_stack));
}

test "an exited scope node is reused by the next enter" {
    enter();
    const first = currentScope();
    exit();

    enter();
    defer exit();
    try std.testing.expectEqual(first, currentScope());
}

test "a reused scope keeps at most retained_bytes" {
    enter();
    _ = allocator().alloc(u8, 4 * retained_bytes) catch unreachable;
    exit();

    enter();
    defer exit();
    const node: *ScopeNode = @ptrCast(@alignCast(currentScope().?));
    try std.testing.expect(node.arena.queryCapacity() <= retained_bytes);
}

test "an untouched scope is rewound and a used one is not" {
    enter();
    defer exit();
    const node: *ScopeNode = @ptrCast(@alignCast(currentScope().?));
    reset();
    try std.testing.expect(isRewound(&node.arena));
    _ = allocator().alloc(u8, 16) catch unreachable;
    try std.testing.expect(!isRewound(&node.arena));
    reset();
    try std.testing.expect(isRewound(&node.arena));
}

test "a scope rewound by a loop reset still sheds its excess on exit" {
    enter();
    _ = allocator().alloc(u8, 4 * retained_bytes) catch unreachable;
    reset();
    exit();

    enter();
    defer exit();
    const node: *ScopeNode = @ptrCast(@alignCast(currentScope().?));
    try std.testing.expect(node.arena.queryCapacity() <= retained_bytes);
}

test "the spare list never grows past max_spare_nodes" {
    const depth = max_spare_nodes + 16;
    for (0..depth) |_| enter();
    for (0..depth) |_| exit();

    try std.testing.expectEqual(@as(usize, max_spare_nodes), spare_count);
}
