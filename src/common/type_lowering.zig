//! The one lowering of an analyzed `ast.TypeInfo` to an `HIRType`. Analysis
//! and codegen both lower through `TypeLowering.lower`; they differ only in
//! how a named type finds its id. Analysis assigns an id on first sight, since
//! a type may be named before its module registers it; codegen runs after
//! every type is registered and only looks one up.
//!
//! A type the lowering is handed is complete: a named type resolves to a
//! declaration, an array names its element type, a map its key and value
//! types, a union its members, a function its signature. Anything less is an
//! analysis bug, reported as `error.IncompleteType` rather than lowered to a
//! guess. An unannotated parameter is `.Nothing`, which lowers as itself.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRType = @import("../codegen/hir/types.zig").HIRType;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;
const StructTable = @import("struct_table.zig").StructTable;
const EnumTable = @import("enum_table.zig").EnumTable;
const GroupTable = @import("group_table.zig").GroupTable;
const UnionTable = @import("union_table.zig").UnionTable;
const TypeRef = ast.TypeRef;

pub const Error = UnionTable.Error || error{IncompleteType};

pub const TypeLowering = struct {
    allocator: std.mem.Allocator,
    graph: *const ModuleGraph,
    names: UnionTable.Names,
    unions: *UnionTable,
    /// The tables a named type is assigned an id in, during analysis. Null in
    /// codegen, which only looks ids up.
    assign: ?Tables,

    pub const Tables = struct {
        structs: *StructTable,
        enums: *EnumTable,
        groups: *GroupTable,
    };

    pub fn lower(self: TypeLowering, ti: *const ast.TypeInfo) Error!HIRType {
        return switch (ti.base) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            .Array => .{ .Array = .{
                .element = try self.lowered(ti.array_type orelse return error.IncompleteType),
                // Only a fixed array is sized; a `const` bound to a literal
                // is a dynamic array the analyzer tags `const_literal`.
                .size = if (ti.array_storage == .fixed) @intCast(ti.array_size.?) else null,
            } },
            .Map => .{ .Map = .{
                .key = try self.lowered(ti.map_key_type orelse return error.IncompleteType),
                .value = try self.lowered(ti.map_value_type orelse return error.IncompleteType),
            } },
            .Enum, .Struct, .Custom => self.named((ti.custom_type orelse return error.IncompleteType).resolved()),
            .Function => blk: {
                const ft = ti.function_type orelse return error.IncompleteType;
                const params = try self.allocator.alloc(*const HIRType, ft.params.len);
                for (ft.params, params) |*param, *slot| slot.* = try self.lowered(param);
                break :blk .{ .Function = .{ .params = params, .ret = try self.lowered(ft.return_type) } };
            },
            .Union => blk: {
                const ut = ti.union_type orelse return error.IncompleteType;
                const members = try self.allocator.alloc(*const HIRType, ut.types.len);
                defer self.allocator.free(members);
                for (ut.types, members) |member, *slot| slot.* = try self.lowered(member);
                break :blk try self.unions.intern(self.names, members);
            },
        };
    }

    /// The type a named type's declaration declares, by its table id.
    pub fn named(self: TypeLowering, ref: TypeRef) Error!HIRType {
        const decl = self.graph.declOf(.{ .module = ref.module, .name = ref.name, .kind = .Type }) orelse return error.IncompleteType;
        return switch (decl) {
            .@"struct" => .{ .Struct = if (self.assign) |t| try t.structs.idFor(ref) else self.names.structs.idOf(ref) orelse return error.IncompleteType },
            .@"enum" => .{ .Enum = if (self.assign) |t| try t.enums.idFor(ref) else self.names.enums.idOf(ref) orelse return error.IncompleteType },
            .group => .{ .Group = if (self.assign) |t| try t.groups.idFor(ref) else self.names.groups.idOf(ref) orelse return error.IncompleteType },
            else => error.IncompleteType,
        };
    }

    fn lowered(self: TypeLowering, ti: *const ast.TypeInfo) Error!*const HIRType {
        const ptr = try self.allocator.create(HIRType);
        ptr.* = try self.lower(ti);
        return ptr;
    }
};
