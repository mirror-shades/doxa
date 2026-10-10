const std = @import("std");
const ast = @import("../ast/ast.zig");
const FunctionParam = ast.FunctionParam;
const TokenImport = @import("./token.zig");
const TokenType = TokenImport.TokenType;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Scope = MemoryImport.Scope;
const HIRType = @import("../codegen/hir/types.zig").HIRType;
const Reporting = @import("../utils/reporting.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub const StructField = struct {
    name: []const u8,
    field_type_info: *ast.TypeInfo,
    index: u32,
    is_public: bool = false,
};

pub const Tetra = enum {
    true,
    false,
    both,
    neither,
};

pub const TokenLiteral = union(enum) {
    int: i64,
    byte: u8,
    float: f64,
    string: []const u8,
    tetra: Tetra,
    nothing: void,
    array: []TokenLiteral,
    map: std.StringHashMap(TokenLiteral),
};

pub const CustomTypeKind = enum {
    Struct,
    Enum,
    Group,
};

/// A group member as its declaration writes it: the qualifier the group uses
/// for it and the identity of the member type.
pub const GroupMemberSource = struct {
    qualifier: []const u8,
    ref: ast.TypeRef,
};

pub const EnumVariant = struct {
    name: []const u8,
    index: u32,
};

pub fn enumVariantIndex(variants: []const EnumVariant, variant_name: []const u8) ?u32 {
    for (variants) |variant| {
        if (std.mem.eql(u8, variant.name, variant_name)) return variant.index;
    }
    return null;
}

pub fn structFieldIndex(fields: anytype, field_name: []const u8) ?u32 {
    for (fields) |field| {
        if (std.mem.eql(u8, field.name, field_name)) return field.index;
    }
    return null;
}

pub const CustomTypeInfo = struct {
    /// The type's identity; `ref.name` is its declared name.
    ref: ast.TypeRef,
    kind: CustomTypeKind,
    enum_variants: ?[]EnumVariant = null,
    struct_fields: ?[]StructField = null,
    group_members: ?[]GroupMemberSource = null,

    pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
        if ((self.kind != .Enum and self.kind != .Group) or self.enum_variants == null) return null;
        return enumVariantIndex(self.enum_variants.?, variant_name);
    }

    pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
        if (self.kind != .Struct or self.struct_fields == null) return null;
        return structFieldIndex(self.struct_fields.?, field_name);
    }
};
