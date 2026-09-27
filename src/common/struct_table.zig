const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/soxa_types.zig");

const StructId = HIRTypes.StructId;
const HIRType = HIRTypes.HIRType;

/// Global registry that tracks every struct that appears during semantic analysis.
/// The frontend owns this metadata and other stages (HIR/LLVM/VM) consume it by ID.
pub const StructTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .empty,
    name_to_id: std.StringHashMapUnmanaged(StructId) = .empty,

    pub const Entry = struct {
        id: StructId,
        qualified_name: []const u8,
        fields: []Field,
    };

    pub const Field = struct {
        name: []const u8,
        type_info: *ast.TypeInfo,
        hir_type: HIRType = .Unknown,
        index: u32,
        nested_struct_id: ?StructId = null,
    };

    pub const FieldInput = struct {
        name: []const u8,
        type_info: *ast.TypeInfo,
    };

    pub fn init(allocator: std.mem.Allocator) StructTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *StructTable) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.qualified_name);
            for (entry.fields) |field| {
                self.allocator.free(field.name);
            }
            self.allocator.free(entry.fields);
        }
        self.entries.deinit(self.allocator);
        self.name_to_id.deinit(self.allocator);
    }

    pub fn registerStruct(self: *StructTable, qualified_name: []const u8, field_inputs: []const FieldInput) !StructId {
        if (self.name_to_id.get(qualified_name)) |existing| {
            return try self.refreshStruct(existing, field_inputs);
        }

        const owned_name = try self.allocator.dupe(u8, qualified_name);
        // Reserve 0 as a sentinel for "unknown/unset", so real IDs start from 1.
        const id: StructId = @intCast(self.entries.items.len + 1);

        var new_fields = try self.allocator.alloc(Field, field_inputs.len);
        for (field_inputs, 0..) |input, field_index| {
            new_fields[field_index] = .{
                .name = try self.allocator.dupe(u8, input.name),
                .type_info = input.type_info,
                .hir_type = .Unknown,
                .index = @intCast(field_index),
                .nested_struct_id = null,
            };
        }

        try self.entries.append(self.allocator, Entry{
            .id = id,
            .qualified_name = owned_name,
            .fields = new_fields,
        });

        try self.name_to_id.put(self.allocator, owned_name, id);
        return id;
    }

    /// A struct is first registered while its field types are still
    /// placeholders and again once they resolve. Replacing the fields in place
    /// keeps the id — and every HIR detail already recorded against it — while
    /// letting the resolved AST types land, so a reader of `fields` never sees
    /// the placeholder the semantic analyzer has since moved past.
    fn refreshStruct(self: *StructTable, id: StructId, field_inputs: []const FieldInput) !StructId {
        const entry = self.getEntryById(id) orelse return id;

        var new_fields = try self.allocator.alloc(Field, field_inputs.len);
        for (field_inputs, 0..) |input, field_index| {
            var hir_type: HIRType = .Unknown;
            var nested_struct_id: ?StructId = null;
            if (field_index < entry.fields.len and std.mem.eql(u8, entry.fields[field_index].name, input.name)) {
                hir_type = entry.fields[field_index].hir_type;
                nested_struct_id = entry.fields[field_index].nested_struct_id;
            }
            new_fields[field_index] = .{
                .name = try self.allocator.dupe(u8, input.name),
                .type_info = input.type_info,
                .hir_type = hir_type,
                .index = @intCast(field_index),
                .nested_struct_id = nested_struct_id,
            };
        }

        for (entry.fields) |field| self.allocator.free(field.name);
        self.allocator.free(entry.fields);
        entry.fields = new_fields;
        return id;
    }

    pub fn getEntryById(self: *StructTable, id: StructId) ?*Entry {
        if (id == 0 or id > self.entries.items.len) return null;
        return &self.entries.items[id - 1];
    }

    pub fn getIdByName(self: *const StructTable, qualified_name: []const u8) ?StructId {
        return self.name_to_id.get(qualified_name);
    }

    pub fn fields(self: *const StructTable, id: StructId) ?[]const Field {
        if (id == 0 or id > self.entries.items.len) return null;
        return self.entries.items[id - 1].fields;
    }

    pub fn getName(self: *const StructTable, id: StructId) ?[]const u8 {
        if (id == 0 or id > self.entries.items.len) return null;
        return self.entries.items[id - 1].qualified_name;
    }

    pub fn setFieldHIRType(self: *StructTable, id: StructId, field_index: u32, ty: HIRType) void {
        if (self.getEntryById(id)) |entry| {
            if (field_index < entry.fields.len) {
                entry.fields[field_index].hir_type = ty;
            }
        }
    }

    pub fn setNestedStructId(self: *StructTable, id: StructId, field_index: u32, nested_id: StructId) void {
        if (self.getEntryById(id)) |entry| {
            if (field_index < entry.fields.len) {
                entry.fields[field_index].nested_struct_id = nested_id;
            }
        }
    }
};
