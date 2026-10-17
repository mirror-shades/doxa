const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/types.zig");
const TypeTable = @import("type_index.zig").TypeTable;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const EnumId = HIRTypes.EnumId;
const TypeRef = ast.TypeRef;

/// Every enum type of the compilation, by identity: a stable id and the
/// variant names in declaration order.
pub const EnumTable = struct {
    table: TypeTable(EnumId, Entry, 0),

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
        variants: []Variant = &.{},

        pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
            for (self.variants) |variant| allocator.free(variant.name);
            allocator.free(self.variants);
        }
    };

    pub fn init(allocator: std.mem.Allocator) EnumTable {
        return .{ .table = .init(allocator) };
    }

    pub fn deinit(self: *EnumTable) void {
        self.table.deinit();
    }

    /// The id of `ref`, allocating a placeholder the first time it is seen.
    pub fn idFor(self: *EnumTable, ref: TypeRef) !EnumId {
        return self.table.idFor(ref);
    }

    /// Record the variants of `ref`. An enum's variants are fixed by its one
    /// declaration, so a second registration leaves them unchanged.
    pub fn registerEnum(self: *EnumTable, ref: TypeRef, variant_names: []const []const u8) !EnumId {
        const id = try self.table.idFor(ref);
        const entry = self.table.getEntryById(id).?;
        if (entry.declared) return id;

        const stored = try self.table.allocator.alloc(Variant, variant_names.len);
        for (variant_names, 0..) |name, i| {
            stored[i] = .{ .name = try self.table.allocator.dupe(u8, name), .index = @intCast(i) };
        }
        entry.variants = stored;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *EnumTable, graph: *const ModuleGraph) !void {
        return self.table.assignKeys(graph);
    }

    pub fn getEntryById(self: *EnumTable, id: EnumId) ?*Entry {
        return self.table.getEntryById(id);
    }

    pub fn idOf(self: *const EnumTable, ref: TypeRef) ?EnumId {
        return self.table.idOf(ref);
    }

    pub fn idByKey(self: *const EnumTable, key: []const u8) ?EnumId {
        return self.table.idByKey(key);
    }

    pub fn variants(self: *const EnumTable, id: EnumId) ?[]const Variant {
        const entry = self.table.entry(id) orelse return null;
        return entry.variants;
    }

    pub fn refOf(self: *const EnumTable, id: EnumId) ?TypeRef {
        return self.table.refOf(id);
    }

    pub fn keyOf(self: *const EnumTable, id: EnumId) ?[]const u8 {
        return self.table.keyOf(id);
    }

    pub fn displayName(self: *const EnumTable, id: EnumId) ?[]const u8 {
        return self.table.displayName(id);
    }
};
