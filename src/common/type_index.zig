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
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

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

/// The entries of one named-type table: identity, insertion order, and the
/// canonical key each entry acquires once the module graph is final. `id_base`
/// is 0 for enums and groups, 1 for structs (which reserve id 0 for "unknown").
/// `Entry` must declare `id`, `ref`, `key`, `declared`, and `deinit`.
pub fn TypeTable(comptime Id: type, comptime Entry: type, comptime id_base: u32) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        entries: std.ArrayListUnmanaged(Entry) = .empty,
        index: TypeIndex(Id) = .{},

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            for (self.entries.items) |*e| e.deinit(self.allocator);
            self.entries.deinit(self.allocator);
            self.index.deinit(self.allocator);
        }

        /// The id of `ref`, allocating a placeholder the first time it is seen.
        pub fn idFor(self: *Self, ref: TypeRef) !Id {
            if (self.index.get(ref)) |id| return id;
            const id: Id = @intCast(self.entries.items.len + id_base);
            try self.entries.append(self.allocator, .{ .id = id, .ref = ref });
            try self.index.put(self.allocator, ref, id);
            return id;
        }

        /// Give every entry its canonical key. Called once the graph is final.
        pub fn assignKeys(self: *Self, graph: *const ModuleGraph) !void {
            for (self.entries.items) |*e| {
                const key = try graph.typeKey(self.allocator, e.ref);
                e.key = key;
                try self.index.putKey(self.allocator, key, e.id);
            }
        }

        pub fn getEntryById(self: *Self, id: Id) ?*Entry {
            const at = self.indexOf(id) orelse return null;
            return &self.entries.items[at];
        }

        pub fn entry(self: *const Self, id: Id) ?*const Entry {
            const at = self.indexOf(id) orelse return null;
            return &self.entries.items[at];
        }

        pub fn isDeclared(self: *const Self, id: Id) bool {
            const entry_ptr = self.entry(id) orelse return false;
            return entry_ptr.declared;
        }

        pub fn idOf(self: *const Self, ref: TypeRef) ?Id {
            return self.index.get(ref);
        }

        pub fn idByKey(self: *const Self, key: []const u8) ?Id {
            return self.index.getByKey(key);
        }

        pub fn refOf(self: *const Self, id: Id) ?TypeRef {
            const entry_ptr = self.entry(id) orelse return null;
            return entry_ptr.ref;
        }

        pub fn keyOf(self: *const Self, id: Id) ?[]const u8 {
            const entry_ptr = self.entry(id) orelse return null;
            return entry_ptr.key;
        }

        pub fn displayName(self: *const Self, id: Id) ?[]const u8 {
            const ref = self.refOf(id) orelse return null;
            return ref.name;
        }

        fn indexOf(self: *const Self, id: Id) ?usize {
            if (id_base == 1 and id == 0) return null;
            const at: usize = if (id_base == 1) @intCast(id - 1) else @intCast(id);
            return if (at < self.entries.items.len) at else null;
        }
    };
}
