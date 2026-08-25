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

/// Opaque handle to a scope. Arrays record the scope they were allocated in so
/// heap elements pushed into them can be re-homed to the same arena.
pub const Scope = opaque {};

pub fn currentScope() ?*Scope {
    return if (head) |h| @ptrCast(h) else null;
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
