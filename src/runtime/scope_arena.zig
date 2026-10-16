const std = @import("std");
const instrument = @import("instrument.zig");
const fault = @import("fault.zig");

/// Scope arenas, the runtime half of the arena memory model
/// (`docs/memory.md`). A scope is a value the program holds: `enter(parent)`
/// opens a child of `parent`, `exit` reclaims it in O(1), and `reset` reclaims
/// everything allocated in it while keeping it open (a loop body). Every
/// allocation names the scope it is made in; there is no implicit current
/// scope. The register HIR states each arena (`plan/register-hir.md`, "Arenas")
/// and its verifier checks that scopes open and close innermost first.
///
/// Under `--instrument=memcheck`, a reset or exit does not reclaim an arena's
/// buffers for reuse: it poisons them and keeps them aside (`reclaimed`), so a
/// read of a value that lived there is caught (`isReclaimed`) rather than
/// passing on reused pages. The kept bytes are capped at `reclaimed_limit`,
/// oldest freed first; a stale read into a freed buffer is not caught.
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
    /// Under `--instrument=memtrace`, what was allocated in it and by whom.
    trace: if (instrument.memtrace) instrument.ArenaTrace else void,
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
        const fresh = std.heap.page_allocator.create(ScopeNode) catch fault.outOfMemory();
        fresh.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        if (instrument.memtrace) fresh.trace = .{};
        break :blk fresh;
    };
    if (instrument.memtrace) node.trace.open(parent == null);
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
    if (instrument.memtrace) node.trace.reclaimed(node.arena.queryCapacity(), .exit);
    if (instrument.memcheck) bury(node);
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
    if (instrument.memtrace) node.trace.reclaimed(node.arena.queryCapacity(), .reset);
    if (instrument.memcheck) return bury(node);
    if (!isRewound(&node.arena)) _ = node.arena.reset(.retain_capacity);
}

/// The poison a reclaimed buffer is filled with.
pub const poison: u8 = 0xDD;
const reclaimed_limit = 256 * 1024 * 1024;

const Reclaimed = struct { start: usize, len: usize };
/// Oldest first.
var reclaimed: std.ArrayListUnmanaged(Reclaimed) = .empty;
var reclaimed_bytes: usize = 0;

/// Take every buffer `node`'s arena has handed out, poison it, and keep it
/// aside; the arena starts over with fresh ones.
fn bury(node: *ScopeNode) void {
    var buf = node.arena.state.used_list;
    node.arena.state.used_list = null;
    while (buf) |b| {
        const next = b.next;
        // The low bit of `size` is the arena's resize flag, not size.
        const size = @as(usize, @bitCast(b.size)) & ~@as(usize, 1);
        const bytes = @as([*]u8, @ptrCast(b))[0..size];
        @memset(bytes, poison);
        reclaimed.append(std.heap.page_allocator, .{ .start = @intFromPtr(bytes.ptr), .len = size }) catch fault.outOfMemory();
        reclaimed_bytes += size;
        buf = next;
    }
    while (reclaimed_bytes > reclaimed_limit) {
        const oldest = reclaimed.orderedRemove(0);
        reclaimed_bytes -= oldest.len;
        std.heap.page_allocator.rawFree(@as([*]u8, @ptrFromInt(oldest.start))[0..oldest.len], .of(usize), @returnAddress());
    }
}

/// Whether `ptr` points into memory an arena has reclaimed (memcheck only).
pub fn isReclaimed(ptr: *const anyopaque) bool {
    const addr = @intFromPtr(ptr);
    var i = reclaimed.items.len;
    while (i > 0) {
        i -= 1;
        const r = reclaimed.items[i];
        if (addr >= r.start and addr < r.start + r.len) return true;
    }
    return false;
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
    if (instrument.memtrace) return .{ .ptr = nodeOf(scope), .vtable = &traced };
    return nodeOf(scope).arena.allocator();
}

/// The bytes the root arena holds from the system: what globals retain.
pub fn rootCapacity() usize {
    return nodeOf(root()).arena.queryCapacity();
}

/// Under memtrace, an arena's allocator counts what it hands out, however it
/// is reached (an allocation, a buffer grown or shrunk in place), to the
/// running site.
/// A free is the arena's no-op and is not counted: an arena gives back only
/// on reset or exit.
const traced: std.mem.Allocator.VTable = .{
    .alloc = struct {
        fn f(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
            const node: *ScopeNode = @ptrCast(@alignCast(ctx));
            const mem = node.arena.allocator().rawAlloc(len, alignment, ret_addr) orelse return null;
            node.trace.allocated(len, .new);
            return mem;
        }
    }.f,
    .resize = struct {
        fn f(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
            const node: *ScopeNode = @ptrCast(@alignCast(ctx));
            if (!node.arena.allocator().rawResize(memory, alignment, new_len, ret_addr)) return false;
            if (new_len > memory.len) node.trace.allocated(new_len - memory.len, .grown) else node.trace.shrunk(memory.len - new_len);
            return true;
        }
    }.f,
    .remap = struct {
        fn f(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
            const node: *ScopeNode = @ptrCast(@alignCast(ctx));
            const mem = node.arena.allocator().rawRemap(memory, alignment, new_len, ret_addr) orelse return null;
            if (new_len > memory.len) node.trace.allocated(new_len - memory.len, .grown) else node.trace.shrunk(memory.len - new_len);
            return mem;
        }
    }.f,
    .free = struct {
        fn f(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
            const node: *ScopeNode = @ptrCast(@alignCast(ctx));
            node.arena.allocator().rawFree(memory, alignment, ret_addr);
        }
    }.f,
};

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
    return allocator(scope).rawAlloc(len, alignment, ret_addr) orelse fault.outOfMemory();
}

pub fn create(scope: *Scope, comptime T: type) *T {
    return @ptrCast(@alignCast(alloc(scope, @sizeOf(T), .fromByteUnits(@alignOf(T)), @returnAddress())));
}

pub fn allocSlice(scope: *Scope, comptime T: type, n: usize) []T {
    const raw = alloc(scope, @sizeOf(T) * n, .fromByteUnits(@alignOf(T)), @returnAddress());
    return @as([*]T, @ptrCast(@alignCast(raw)))[0..n];
}

test "memcheck: a buried arena's memory is poisoned, kept and recognised" {
    const scope = enter(root());
    defer exit(scope);
    const before = allocSlice(scope, u8, 64);
    @memset(before, 7);
    try std.testing.expect(!isReclaimed(before.ptr));

    bury(nodeOf(scope));
    try std.testing.expect(isReclaimed(before.ptr));
    try std.testing.expectEqual(poison, before[0]);
    // The arena carries on with fresh memory, which is live.
    const after = allocSlice(scope, u8, 64);
    try std.testing.expect(!isReclaimed(after.ptr));
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
