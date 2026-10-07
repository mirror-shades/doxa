//! Identity bookkeeping shared by the struct, enum, and group tables.
//!
//! A type's identity is its `TypeRef`; its table id is an implementation index
//! assigned the first time the type is seen and never renumbered. Codegen and
//! the emitter name types by their canonical key — the `t`-kind mangled symbol
//! (`ModuleGraph.typeKey`), assigned once the module graph is final — so the
//! index also maps that key back to the id.

const std = @import("std");
const ids = @import("../module/ids.zig");
const TypeRef = ids.TypeRef;

pub fn TypeIndex(comptime Id: type) type {
    return struct {
        const Self = @This();

        by_ref: std.HashMapUnmanaged(TypeRef, Id, ids.TypeRefContext, std.hash_map.default_max_load_percentage) = .empty,
        by_key: std.StringHashMapUnmanaged(Id) = .empty,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.by_ref.deinit(allocator);
            self.by_key.deinit(allocator);
        }

        pub fn get(self: *const Self, ref: TypeRef) ?Id {
            return self.by_ref.get(ref);
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, ref: TypeRef, id: Id) !void {
            try self.by_ref.put(allocator, ref, id);
        }

        pub fn getByKey(self: *const Self, key: []const u8) ?Id {
            return self.by_key.get(key);
        }

        pub fn putKey(self: *Self, allocator: std.mem.Allocator, key: []const u8, id: Id) !void {
            try self.by_key.put(allocator, key, id);
        }
    };
}
