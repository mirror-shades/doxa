const std = @import("std");
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;

/// Shared custom type information structure used by both semantic analysis and HIR generation
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
        field_type: HIRType,
        index: u32,
    };

    /// Get the index of an enum variant by name
    pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
        if (self.kind != .Enum or self.enum_variants == null) return null;

        for (self.enum_variants.?) |variant| {
            if (std.mem.eql(u8, variant.name, variant_name)) {
                return variant.index;
            }
        }
        return null;
    }

    /// Get the index of a struct field by name
    pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
        if (self.kind != .Struct or self.struct_fields == null) return null;

        for (self.struct_fields.?) |field| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return field.index;
            }
        }
        return null;
    }
};
