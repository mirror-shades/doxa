const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const TypeSystem = @import("../../codegen/hir/type_system.zig").TypeSystem;

pub const CustomTypeInfo = struct {
    name: []const u8,
    kind: CustomTypeKind,
    enum_variants: ?[]EnumVariant = null,
    struct_fields: ?[]StructField = null,

    pub const CustomTypeKind = enum {
        Struct,
        Enum,
    };

    pub const EnumVariant = struct {
        name: []const u8,
        index: u32,
    };

    pub const StructField = struct {
        name: []const u8,
        field_type_info: *ast.TypeInfo,
        custom_type_name: ?[]const u8 = null, // For custom types like Person
        index: u32,
        is_public: bool = false,
    };
};

pub const StructMethodInfo = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
    return_type: *ast.TypeInfo,
};

pub fn hirTypeToAstType(hir_type: HIRType) ast.Type {
    return switch (hir_type) {
        .Int => .Int,
        .Byte => .Byte,
        .Float => .Float,
        .String => .String,
        .Tetra => .Tetra,
        .Nothing => .Nothing,
        .Array => .Array,
        .Struct => .Struct,
        .Map => .Map,
        .Enum => .Enum,
        .Function => .Function,
        .Union => .Union,
        .Unknown => .Nothing, // TODO: This feels hacky
    };
}

pub fn astTypeToHirType(ast_type: ast.Type) HIRType {
    return switch (ast_type) {
        .Int => .Int,
        .Byte => .Byte,
        .Float => .Float,
        .String => .String,
        .Tetra => .Tetra,
        .Nothing => .Nothing,
        .Array => .Array,
        .Struct => .Struct,
        .Map => .Map,
        .Enum => .Enum,
        .Function => .Function,
        .Union => .Union,
        .Custom => .Unknown, // Custom types map to Unknown in HIR
    };
}

fn convertTypeInfoToHirType(type_info: *ast.TypeInfo, allocator: std.mem.Allocator) !HIRType {
    _ = allocator; // May be needed for complex type conversions in the future
    return astTypeToHirType(type_info.base);
}
