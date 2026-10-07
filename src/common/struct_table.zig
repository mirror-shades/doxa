const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/soxa_types.zig");
const TypeIndex = @import("type_index.zig").TypeIndex;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const StructId = HIRTypes.StructId;
const HIRType = HIRTypes.HIRType;
const TypeRef = ast.TypeRef;

/// Every struct type of the compilation, by identity. The frontend owns this
/// metadata; HIR and the emitter consume it by id or canonical key.
pub const StructTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .empty,
    index: TypeIndex(StructId) = .{},

    pub const Entry = struct {
        id: StructId,
        ref: TypeRef,
        /// Canonical codegen key (`ModuleGraph.typeKey`); set by `assignKeys`.
        key: ?[]const u8 = null,
        /// False while the id is allocated but the declaration has not been
        /// registered yet.
        declared: bool = false,
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
            for (entry.fields) |field| self.allocator.free(field.name);
            self.allocator.free(entry.fields);
        }
        self.entries.deinit(self.allocator);
        self.index.deinit(self.allocator);
    }

    /// The id of `ref`, allocating a placeholder entry the first time the type
    /// is seen. Ids are stable from first sight, so a reference to a struct in a
    /// module whose types are not registered yet lowers to its final id.
    /// Id 0 is reserved for "unknown".
    pub fn idFor(self: *StructTable, ref: TypeRef) !StructId {
        if (self.index.get(ref)) |id| return id;
        const id: StructId = @intCast(self.entries.items.len + 1);
        try self.entries.append(self.allocator, .{ .id = id, .ref = ref, .fields = &.{} });
        try self.index.put(self.allocator, ref, id);
        return id;
    }

    /// Record the declared fields of `ref`. A struct registered again (its
    /// field types resolved since) keeps its id and the nested struct ids
    /// already recorded against a field of the same name. Field HIR types are
    /// set once analysis is complete (`lowerStructFieldTypes`).
    pub fn registerStruct(self: *StructTable, ref: TypeRef, field_inputs: []const FieldInput) !StructId {
        const id = try self.idFor(ref);
        const entry = self.getEntryById(id).?;

        const new_fields = try self.allocator.alloc(Field, field_inputs.len);
        for (field_inputs, 0..) |input, field_index| {
            var nested_struct_id: ?StructId = null;
            if (field_index < entry.fields.len and std.mem.eql(u8, entry.fields[field_index].name, input.name)) {
                nested_struct_id = entry.fields[field_index].nested_struct_id;
            }
            new_fields[field_index] = .{
                .name = try self.allocator.dupe(u8, input.name),
                .type_info = input.type_info,
                .index = @intCast(field_index),
                .nested_struct_id = nested_struct_id,
            };
        }

        for (entry.fields) |field| self.allocator.free(field.name);
        self.allocator.free(entry.fields);
        entry.fields = new_fields;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *StructTable, graph: *const ModuleGraph) !void {
        for (self.entries.items) |*entry| {
            const key = try graph.typeKey(self.allocator, entry.ref);
            entry.key = key;
            try self.index.putKey(self.allocator, key, entry.id);
        }
    }

    pub fn getEntryById(self: *StructTable, id: StructId) ?*Entry {
        if (id == 0 or id > self.entries.items.len) return null;
        return &self.entries.items[id - 1];
    }

    pub fn idOf(self: *const StructTable, ref: TypeRef) ?StructId {
        return self.index.get(ref);
    }

    pub fn idByKey(self: *const StructTable, key: []const u8) ?StructId {
        return self.index.getByKey(key);
    }

    pub fn fields(self: *const StructTable, id: StructId) ?[]const Field {
        if (id == 0 or id > self.entries.items.len) return null;
        return self.entries.items[id - 1].fields;
    }

    pub fn refOf(self: *const StructTable, id: StructId) ?TypeRef {
        if (id == 0 or id > self.entries.items.len) return null;
        return self.entries.items[id - 1].ref;
    }

    /// The canonical key of `id` (codegen identity).
    pub fn keyOf(self: *const StructTable, id: StructId) ?[]const u8 {
        if (id == 0 or id > self.entries.items.len) return null;
        return self.entries.items[id - 1].key;
    }

    /// The declared name of `id`, for display.
    pub fn displayName(self: *const StructTable, id: StructId) ?[]const u8 {
        const ref = self.refOf(id) orelse return null;
        return ref.name;
    }

    pub fn setNestedStructId(self: *StructTable, id: StructId, field_index: u32, nested_id: StructId) void {
        if (self.getEntryById(id)) |entry| {
            if (field_index < entry.fields.len) entry.fields[field_index].nested_struct_id = nested_id;
        }
    }
};
