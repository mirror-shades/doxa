const std = @import("std");
const HIRTypes = @import("../codegen/hir/soxa_types.zig");

const EnumId = HIRTypes.EnumId;

/// Global registry that tracks every enum that appears during semantic analysis.
/// This mirrors `StructTable` but for enums: it provides a stable ID and a
/// compact list of variant names for each enum type.
pub const EnumTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .{},
    name_to_id: std.StringHashMapUnmanaged(EnumId) = .{},

    pub const Variant = struct {
        name: []const u8,
        index: u32,
    };

    pub const Entry = struct {
        id: EnumId,
        qualified_name: []const u8,
        variants: []Variant,
    };

    pub fn init(allocator: std.mem.Allocator) EnumTable {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *EnumTable) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.qualified_name);
            self.allocator.free(entry.variants);
        }
        self.entries.deinit(self.allocator);
        self.name_to_id.deinit(self.allocator);
    }

    /// Register an enum and its variants. If the enum name is already
    /// registered, this returns the existing id and does not overwrite
    /// the variants.
    pub fn registerEnum(self: *EnumTable, qualified_name: []const u8, variant_names: []const []const u8) !EnumId {
        if (self.name_to_id.get(qualified_name)) |existing| {
            return existing;
        }

        const owned_name = try self.allocator.dupe(u8, qualified_name);
        const id: EnumId = @intCast(self.entries.items.len);

        var stored_variants = try self.allocator.alloc(Variant, variant_names.len);
        for (variant_names, 0..) |vname, i| {
            stored_variants[i] = .{
                .name = try self.allocator.dupe(u8, vname),
                .index = @intCast(i),
            };
        }

        try self.entries.append(self.allocator, Entry{
            .id = id,
            .qualified_name = owned_name,
            .variants = stored_variants,
        });

        try self.name_to_id.put(self.allocator, owned_name, id);
        return id;
    }

    pub fn getEntryById(self: *EnumTable, id: EnumId) ?*Entry {
        if (id >= self.entries.items.len) return null;
        return &self.entries.items[id];
    }

    pub fn getIdByName(self: *const EnumTable, qualified_name: []const u8) ?EnumId {
        return self.name_to_id.get(qualified_name);
    }

    pub fn variants(self: *const EnumTable, id: EnumId) ?[]const Variant {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].variants;
    }

    pub fn getName(self: *const EnumTable, id: EnumId) ?[]const u8 {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].qualified_name;
    }
};
