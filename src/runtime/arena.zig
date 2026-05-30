const std = @import("std");

pub const Arena = struct {
    parent_arena: ?*Arena = null,
    inner: std.heap.ArenaAllocator,

    pub fn init(child_allocator: std.mem.Allocator) Arena {
        return .{
            .inner = std.heap.ArenaAllocator.init(child_allocator),
        };
    }
};

fn arenaAllocFn(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
    const arena: *Arena = @ptrCast(@alignCast(ctx));
    return arena.inner.allocator().rawAlloc(len, alignment, ret_addr);
}

fn arenaResizeFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ret_addr;
    return false;
}

fn arenaFreeFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = ret_addr;
}

fn arenaRemapFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ret_addr;
    return null;
}

const arena_vtable: std.mem.Allocator.VTable = .{
    .alloc = arenaAllocFn,
    .resize = arenaResizeFn,
    .free = arenaFreeFn,
    .remap = arenaRemapFn,
};

var global_active_arena: ?*Arena = null;

pub fn getActiveArena() ?*Arena {
    return global_active_arena;
}

pub fn currentAllocator() std.mem.Allocator {
    if (getActiveArena()) |arena| {
        return std.mem.Allocator{
            .ptr = @ptrCast(arena),
            .vtable = &arena_vtable,
        };
    }
    return std.heap.page_allocator;
}

pub export fn doxa_scope_enter(min_size: i64) callconv(.c) void {
    _ = min_size;
    const new_arena = std.heap.page_allocator.create(Arena) catch @panic("oom");
    new_arena.* = Arena.init(std.heap.page_allocator);
    new_arena.parent_arena = global_active_arena;

    global_active_arena = new_arena;
}

pub export fn doxa_scope_exit() callconv(.c) void {
    const current = global_active_arena orelse return;
    if (current.parent_arena == null) return;
    global_active_arena = current.parent_arena;
    current.inner.deinit();
    std.heap.page_allocator.destroy(current);
}

pub export fn doxa_scope_alloc(size: i64, alignment: i64) callconv(.c) ?*anyopaque {
    const sz: usize = @intCast(size);
    const align_val: u29 = if (alignment > 0) @intCast(alignment) else @alignOf(u64);
    const alignment_enum = std.mem.Alignment.fromByteUnits(align_val);
    if (global_active_arena) |active| {
        return active.inner.allocator().rawAlloc(sz, alignment_enum, @returnAddress()) orelse @panic("oom");
    }
    return std.heap.page_allocator.rawAlloc(sz, alignment_enum, @returnAddress());
}
