const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/types.zig");
const TypeTable = @import("type_index.zig").TypeTable;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const StructId = HIRTypes.StructId;
const HIRType = HIRTypes.HIRType;
const TypeRef = ast.TypeRef;

/// Every struct type of the compilation, by identity. The frontend owns this
/// metadata; HIR and the emitter consume it by id or canonical key.
pub const StructTable = struct {
    table: TypeTable(StructId, Entry, 1),

    pub const Entry = struct {
        id: StructId,
        ref: TypeRef,
        /// Canonical codegen key (`ModuleGraph.typeKey`); set by `assignKeys`.
        key: ?[]const u8 = null,
        /// False while the id is allocated but the declaration has not been
        /// registered yet.
        declared: bool = false,
        fields: []Field = &.{},

        pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
            for (self.fields) |field| allocator.free(field.name);
            allocator.free(self.fields);
        }
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
        return .{ .table = .init(allocator) };
    }

    pub fn deinit(self: *StructTable) void {
        self.table.deinit();
    }

    /// The id of `ref`, allocating a placeholder entry the first time the type
    /// is seen. Ids are stable from first sight, so a reference to a struct in a
    /// module whose types are not registered yet lowers to its final id.
    /// Id 0 is reserved for "unknown".
    pub fn idFor(self: *StructTable, ref: TypeRef) !StructId {
        return self.table.idFor(ref);
    }

    /// Record the declared fields of `ref`. A struct registered again (its
    /// field types resolved since) keeps its id and the nested struct ids
    /// already recorded against a field of the same name. Field HIR types are
    /// set once analysis is complete (`lowerStructFieldTypes`).
    pub fn registerStruct(self: *StructTable, ref: TypeRef, field_inputs: []const FieldInput) !StructId {
        const id = try self.table.idFor(ref);
        const entry = self.table.getEntryById(id).?;

        const new_fields = try self.table.allocator.alloc(Field, field_inputs.len);
        for (field_inputs, 0..) |input, field_index| {
            var nested_struct_id: ?StructId = null;
            if (field_index < entry.fields.len and std.mem.eql(u8, entry.fields[field_index].name, input.name)) {
                nested_struct_id = entry.fields[field_index].nested_struct_id;
            }
            new_fields[field_index] = .{
                .name = try self.table.allocator.dupe(u8, input.name),
                .type_info = input.type_info,
                .index = @intCast(field_index),
                .nested_struct_id = nested_struct_id,
            };
        }

        for (entry.fields) |field| self.table.allocator.free(field.name);
        self.table.allocator.free(entry.fields);
        entry.fields = new_fields;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *StructTable, graph: *const ModuleGraph) !void {
        return self.table.assignKeys(graph);
    }

    pub fn getEntryById(self: *StructTable, id: StructId) ?*Entry {
        return self.table.getEntryById(id);
    }

    pub fn idOf(self: *const StructTable, ref: TypeRef) ?StructId {
        return self.table.idOf(ref);
    }

    pub fn idByKey(self: *const StructTable, key: []const u8) ?StructId {
        return self.table.idByKey(key);
    }

    pub fn fields(self: *const StructTable, id: StructId) ?[]const Field {
        const entry = self.table.entry(id) orelse return null;
        return entry.fields;
    }

    pub fn refOf(self: *const StructTable, id: StructId) ?TypeRef {
        return self.table.refOf(id);
    }

    /// The canonical key of `id` (codegen identity).
    pub fn keyOf(self: *const StructTable, id: StructId) ?[]const u8 {
        return self.table.keyOf(id);
    }

    /// The declared name of `id`, for display.
    pub fn displayName(self: *const StructTable, id: StructId) ?[]const u8 {
        return self.table.displayName(id);
    }

    pub fn setNestedStructId(self: *StructTable, id: StructId, field_index: u32, nested_id: StructId) void {
        if (self.table.getEntryById(id)) |entry| {
            if (field_index < entry.fields.len) entry.fields[field_index].nested_struct_id = nested_id;
        }
    }
};
