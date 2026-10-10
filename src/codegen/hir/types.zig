//! The type of a Doxa value as codegen sees it: what `TypeLowering`
//! (`src/common/type_lowering.zig`) makes of an analyzed type. Named types
//! are table ids; the register HIR adds its own internal types around these
//! (`register/ir.zig`, `Type`).

const std = @import("std");

pub const StructId = u32;
pub const EnumId = u32;
pub const UnionId = u32;
pub const GroupId = u32;

pub const HIRType = union(enum) {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,

    /// `size` is a fixed array's length (`int[3]`); null for a dynamic
    /// array. The two are distinct types with distinct representations, and
    /// fixed-to-dynamic is the explicit `array_from_fixed` conversion.
    Array: struct { element: *const HIRType, size: ?u32 = null },
    Map: struct { key: *const HIRType, value: *const HIRType },

    Struct: StructId,
    Enum: EnumId,
    Group: GroupId,

    Function: struct {
        params: []const *const HIRType,
        ret: *const HIRType,
    },

    Union: struct {
        id: UnionId,
        members: []const *const HIRType,
    },

    Unknown,
    Poison,

    /// Structural equality. Named types are equal by table id and unions by
    /// id: `UnionTable` interns every union, so equal unions share one.
    pub fn eql(a: HIRType, b: HIRType) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .Int, .Byte, .Float, .String, .Tetra, .Nothing, .Unknown, .Poison => true,
            .Array => |array| array.size == b.Array.size and array.element.eql(b.Array.element.*),
            .Map => |map| map.key.eql(b.Map.key.*) and map.value.eql(b.Map.value.*),
            .Struct => |id| id == b.Struct,
            .Enum => |id| id == b.Enum,
            .Group => |id| id == b.Group,
            .Function => |function| {
                if (function.params.len != b.Function.params.len) return false;
                for (function.params, b.Function.params) |pa, pb| {
                    if (!pa.eql(pb.*)) return false;
                }
                return function.ret.eql(b.Function.ret.*);
            },
            .Union => |u| u.id == b.Union.id,
        };
    }

    /// A value of this type is a `%DoxaValue` box naming its member.
    pub fn isBoxed(self: HIRType) bool {
        return self == .Union or self == .Group;
    }
};
