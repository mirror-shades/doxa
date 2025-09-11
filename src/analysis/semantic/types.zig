const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const TypeSystem = @import("../../codegen/hir/type_system.zig").TypeSystem;

/// Information about custom types (structs and enums) defined in the program
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

/// Information about struct methods
pub const StructMethodInfo = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
    return_type: *ast.TypeInfo,
};

/// Helper function to convert HIRType to ast.Type
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

/// Helper function to convert ast.Type to HIRType
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

/// Helper function to convert SemanticAnalyzer.CustomTypeInfo to TypeSystem.CustomTypeInfo
pub fn convertCustomTypeInfo(semantic_type: CustomTypeInfo, allocator: std.mem.Allocator) !TypeSystem.CustomTypeInfo {
    var hir_type = TypeSystem.CustomTypeInfo{
        .name = semantic_type.name,
        .kind = switch (semantic_type.kind) {
            .Struct => .Struct,
            .Enum => .Enum,
        },
    };

    if (semantic_type.kind == .Enum and semantic_type.enum_variants != null) {
        const variants = semantic_type.enum_variants.?;
        const converted_variants = try allocator.alloc(TypeSystem.CustomTypeInfo.EnumVariant, variants.len);
        for (variants, 0..) |variant, i| {
            converted_variants[i] = .{
                .name = variant.name,
                .index = variant.index,
            };
        }
        hir_type.enum_variants = converted_variants;
    }

    if (semantic_type.kind == .Struct and semantic_type.struct_fields != null) {
        const fields = semantic_type.struct_fields.?;
        const converted_fields = try allocator.alloc(TypeSystem.CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field, i| {
            // Convert the field type info
            const converted_field_type = try convertTypeInfoToHirType(field.field_type_info, allocator);

            converted_fields[i] = .{
                .name = field.name,
                .field_type = converted_field_type,
                .custom_type_name = field.custom_type_name,
                .index = field.index,
            };
        }
        hir_type.struct_fields = converted_fields;
    }

    return hir_type;
}

/// Helper function to convert ast.TypeInfo to HIRType
fn convertTypeInfoToHirType(type_info: *ast.TypeInfo, allocator: std.mem.Allocator) !HIRType {
    _ = allocator; // May be needed for complex type conversions in the future
    return astTypeToHirType(type_info.base);
}
