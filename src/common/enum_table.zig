const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/types.zig");
const TypeIndex = @import("type_index.zig").TypeIndex;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const EnumId = HIRTypes.EnumId;
const TypeRef = ast.TypeRef;

/// Every enum type of the compilation, by identity: a stable id and the
/// variant names in declaration order.
pub const EnumTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .empty,
    index: TypeIndex(EnumId) = .{},

    pub const Variant = struct {
        name: []const u8,
        index: u32,
    };

    pub const Entry = struct {
        id: EnumId,
        ref: TypeRef,
        /// Canonical codegen key (`ModuleGraph.typeKey`); set by `assignKeys`.
        key: ?[]const u8 = null,
        declared: bool = false,
        variants: []Variant,
    };

    pub fn init(allocator: std.mem.Allocator) EnumTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *EnumTable) void {
        for (self.entries.items) |entry| self.allocator.free(entry.variants);
        self.entries.deinit(self.allocator);
        self.index.deinit(self.allocator);
    }

    /// The id of `ref`, allocating a placeholder the first time it is seen.
    pub fn idFor(self: *EnumTable, ref: TypeRef) !EnumId {
        if (self.index.get(ref)) |id| return id;
        const id: EnumId = @intCast(self.entries.items.len);
        try self.entries.append(self.allocator, .{ .id = id, .ref = ref, .variants = &.{} });
        try self.index.put(self.allocator, ref, id);
        return id;
    }

    /// Record the variants of `ref`. An enum's variants are fixed by its one
    /// declaration, so a second registration leaves them unchanged.
    pub fn registerEnum(self: *EnumTable, ref: TypeRef, variant_names: []const []const u8) !EnumId {
        const id = try self.idFor(ref);
        const entry = &self.entries.items[id];
        if (entry.declared) return id;

        const stored = try self.allocator.alloc(Variant, variant_names.len);
        for (variant_names, 0..) |name, i| {
            stored[i] = .{ .name = try self.allocator.dupe(u8, name), .index = @intCast(i) };
        }
        entry.variants = stored;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *EnumTable, graph: *const ModuleGraph) !void {
        for (self.entries.items) |*entry| {
            const key = try graph.typeKey(self.allocator, entry.ref);
            entry.key = key;
            try self.index.putKey(self.allocator, key, entry.id);
        }
    }

    pub fn getEntryById(self: *EnumTable, id: EnumId) ?*Entry {
        if (id >= self.entries.items.len) return null;
        return &self.entries.items[id];
    }

    pub fn idOf(self: *const EnumTable, ref: TypeRef) ?EnumId {
        return self.index.get(ref);
    }

    pub fn idByKey(self: *const EnumTable, key: []const u8) ?EnumId {
        return self.index.getByKey(key);
    }

    pub fn variants(self: *const EnumTable, id: EnumId) ?[]const Variant {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].variants;
    }

    pub fn refOf(self: *const EnumTable, id: EnumId) ?TypeRef {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].ref;
    }

    pub fn keyOf(self: *const EnumTable, id: EnumId) ?[]const u8 {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].key;
    }

    pub fn displayName(self: *const EnumTable, id: EnumId) ?[]const u8 {
        const ref = self.refOf(id) orelse return null;
        return ref.name;
    }
};
