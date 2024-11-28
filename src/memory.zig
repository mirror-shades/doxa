// this is a placeholder for the memory module
// it will handle a centralized arena allocator
// this will be replaced with a more sophisticated memory manager system later

const std = @import("std");

pub const MemoryManager = struct {
    arena: std.heap.ArenaAllocator,
    debug_enabled: bool,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) MemoryManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .debug_enabled = debug_enabled,
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.debug_enabled) {
            std.debug.print("Cleaning up memory manager...\n", .{});
        }
        self.arena.deinit();
    }

    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn reset(self: *MemoryManager) void {
        if (self.debug_enabled) {
            std.debug.print("Resetting memory manager...\n", .{});
        }
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.arena.child_allocator);
    }
};
