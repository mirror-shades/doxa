const std = @import("std");

/// Scope-arena stack backing the language's "every block is an arena" memory
/// model. `enter` pushes a child arena; `exit` frees the top arena in O(1).
/// Heap values are allocated from the current (top) arena and are reclaimed
/// when the scope that allocated them exits.
const ScopeNode = struct {
    arena: std.heap.ArenaAllocator,
    prev: ?*ScopeNode,
};

var head: ?*ScopeNode = null;

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
    const node = std.heap.page_allocator.create(ScopeNode) catch @panic("scope_arena: OOM");
    node.* = .{ .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator), .prev = head };
    head = node;
}

pub fn exit() void {
    const node = head orelse return;
    head = node.prev;
    node.arena.deinit();
    std.heap.page_allocator.destroy(node);
}

/// Reclaim every allocation in the current scope while keeping the scope node
/// and its arena available for reuse. This is the physical implementation of a
/// lexical scope whose lifetime repeats (for example, a loop body).
pub fn reset() void {
    const node = head orelse return;
    _ = node.arena.reset(.retain_capacity);
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
