//! How register-HIR types are laid out in LLVM and in the runtime's memory.
//! The representation of a value is a function of its type (`plan/
//! register-hir.md`, "Types and representations"); every answer here is read
//! off the type, never off how a value was produced.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const rt = @import("../../runtime/doxa_rt.zig");

const HIRType = ir.HIRType;

/// The LLVM type of a value of `t`.
pub fn llvmType(t: ir.Type) []const u8 {
    return switch (t) {
        .doxa => |d| doxaType(d),
        .cond => "i1",
        .arena, .ref => "ptr",
    };
}

pub fn doxaType(t: HIRType) []const u8 {
    return switch (t) {
        .Int, .Enum => "i64",
        .Float => "double",
        .Byte => "i8",
        .Tetra => "i2",
        .String => "%DoxaString",
        .Union, .Group => "%DoxaValue",
        .Array, .Map, .Struct, .Function => "ptr",
        // `nothing` has one value and no storage; it is an `i1 0` wherever a
        // value of it must be spelled.
        .Nothing => "i1",
        .Unknown, .Poison => unreachable, // analysis hands codegen complete types
    };
}

/// What a function returning `t` returns in LLVM.
pub fn returnType(t: HIRType) []const u8 {
    return if (t == .Nothing) "void" else doxaType(t);
}

// ── Runtime element tags (`ArrayHeader.elem_tag`, `StructDesc.field_tags`) ──

pub const Tag = struct {
    pub const int: u64 = 0;
    pub const byte: u64 = 1;
    pub const float: u64 = 2;
    pub const string: u64 = 3;
    pub const tetra: u64 = 4;
    pub const nothing: u64 = 5;
    pub const array: u64 = 6;
    pub const structure: u64 = 7;
    pub const enumeration: u64 = 8;
    pub const value: u64 = 9;
};

/// The tag a dynamic array element or a struct field of `t` carries.
pub fn elementTag(t: HIRType) u64 {
    return switch (t) {
        .Int => Tag.int,
        .Byte => Tag.byte,
        .Float => Tag.float,
        .String => Tag.string,
        .Tetra => Tag.tetra,
        .Nothing => Tag.nothing,
        .Array => Tag.array,
        .Struct => Tag.structure,
        .Enum => Tag.enumeration,
        .Union, .Group => Tag.value,
        // A map or function element is a pointer the runtime never walks.
        .Map, .Function => 255,
        .Unknown, .Poison => unreachable,
    };
}

/// Bytes one element of `t` occupies in a dynamic array.
pub fn elementSize(t: HIRType) u64 {
    return switch (t) {
        .Int, .Float, .Enum, .Array, .Struct, .Map, .Function => 8,
        .Byte, .Tetra => 1,
        .String => 16,
        .Union, .Group => @sizeOf(rt.DoxaValue),
        .Nothing => 0,
        .Unknown, .Poison => unreachable,
    };
}

/// The LLVM type of one element of a dynamic array of `t`, as the runtime
/// stores it. A tetra is a byte in memory.
pub fn elementStorageType(t: HIRType) []const u8 {
    return switch (t) {
        .Tetra => "i8",
        .Nothing => "i8",
        else => doxaType(t),
    };
}

/// The `DoxaValue.tag` of a box holding a member of `t`.
pub fn valueTag(t: HIRType) rt.DoxaTag {
    return switch (t) {
        .Int => .Int,
        .Float => .Float,
        .Byte => .Byte,
        .String => .String,
        .Array => .Array,
        .Struct => .Struct,
        .Enum => .Enum,
        .Tetra => .Tetra,
        .Nothing => .Nothing,
        .Function => .Function,
        .Map => .Map,
        .Union, .Group, .Unknown, .Poison => unreachable, // a box holds no box
    };
}

// ── Structs ──

/// A struct is a block of `i64` words: a string field takes two (pointer and
/// length), every other field one. A union or group field is a pointer to a
/// `%DoxaValue` box; a float is its bit pattern.
pub fn fieldWords(t: HIRType) u32 {
    return if (t == .String) 2 else 1;
}

pub fn structWords(fields: []const HIRType) u32 {
    var total: u32 = 0;
    for (fields) |f| total += fieldWords(f);
    return total;
}

pub fn fieldOffset(fields: []const HIRType, index: u32) u32 {
    var offset: u32 = 0;
    for (fields[0..index]) |f| offset += fieldWords(f);
    return offset;
}

/// A struct's runtime descriptor tag for a field of `t`.
pub fn fieldTag(t: HIRType) u64 {
    return elementTag(t);
}

/// Whether every field of a struct is a scalar: such a struct is copied word
/// for word, with nothing to re-home.
pub fn allScalar(fields: []const HIRType) bool {
    if (fields.len == 0) return false;
    for (fields) |t| switch (t) {
        .Int, .Byte, .Float, .Tetra, .Enum, .Nothing => {},
        else => return false,
    };
    return true;
}

// ── Fixed arrays ──

/// The LLVM type of a fixed array of `t`'s storage: nested fixed arrays are
/// stored inline; a struct whose descriptor is skipped is stored flat, as its
/// words, so an element is the struct itself.
pub fn fixedStorageType(alloc: std.mem.Allocator, layout: *const Layout, t: HIRType) std.mem.Allocator.Error![]const u8 {
    std.debug.assert(t == .Array and t.Array.size != null);
    return std.fmt.allocPrint(alloc, "[{d} x {s}]", .{ t.Array.size.?, try fixedElementType(alloc, layout, t.Array.element.*) });
}

pub fn fixedElementType(alloc: std.mem.Allocator, layout: *const Layout, element: HIRType) std.mem.Allocator.Error![]const u8 {
    if (element == .Array and element.Array.size != null) return fixedStorageType(alloc, layout, element);
    if (element == .Struct and layout.isFlat(element.Struct)) return std.fmt.allocPrint(alloc, "[{d} x i64]", .{layout.structWordsOf(element.Struct)});
    return elementStorageType(element);
}

/// Bytes a fixed array of `t` occupies.
pub fn fixedBytes(layout: *const Layout, t: HIRType) u64 {
    return t.Array.size.? * fixedElementBytes(layout, t.Array.element.*);
}

pub fn fixedElementBytes(layout: *const Layout, element: HIRType) u64 {
    if (element == .Array and element.Array.size != null) return fixedBytes(layout, element);
    if (element == .Struct and layout.isFlat(element.Struct)) return @as(u64, layout.structWordsOf(element.Struct)) * 8;
    return elementSize(element);
}

/// Program-wide layout facts: each struct's field types, and which structs
/// skip their runtime descriptor.
pub const Layout = struct {
    /// Field types by `StructId`.
    struct_fields: []const []const HIRType,
    /// By `StructId`: the struct has no runtime descriptor. Its fields are
    /// all scalar, no reflection site reaches it, and no runtime operation
    /// looks it up by address, so it is copied word for word and stored flat
    /// in a fixed array.
    skip_descriptor: []const bool,

    pub fn structWordsOf(self: *const Layout, id: ir.StructId) u32 {
        return structWords(self.struct_fields[id]);
    }

    /// A fixed array of this struct stores its elements inline.
    pub fn isFlat(self: *const Layout, id: ir.StructId) bool {
        return self.skip_descriptor[id];
    }

    /// The word count a descriptor-free struct is copied by; 0 for one with a
    /// descriptor.
    pub fn skippedWords(self: *const Layout, t: HIRType) u64 {
        if (t != .Struct or !self.skip_descriptor[t.Struct]) return 0;
        return self.structWordsOf(t.Struct);
    }

    /// Which structs may skip their descriptor. A struct needs one when its
    /// fields are not all scalar, when a reflection site reaches it, or when
    /// a runtime operation finds it by address: as a map key or value, a box
    /// member, an element of a nested dynamic array, or a field of a struct
    /// that keeps its descriptor and is reflected.
    pub fn init(alloc: std.mem.Allocator, module: *const ir.Module) !Layout {
        const fields = module.program.struct_fields;
        const needs = try alloc.alloc(bool, fields.len);
        @memset(needs, module.force_struct_descriptors);
        const reflected = try alloc.alloc(bool, fields.len);
        @memset(reflected, false);
        for (module.reflected_structs) |key| {
            for (module.program.struct_keys, 0..) |k, id| {
                if (std.mem.eql(u8, k, key)) reflected[id] = true;
            }
        }

        var visitor = NeedsVisitor{ .needs = needs, .program = &module.program };
        for (module.functions) |f| {
            for (f.values) |v| switch (v.ty) {
                .doxa, .ref => |t| visitor.value(t),
                .cond, .arena => {},
            };
        }
        for (module.program.globals) |g| visitor.value(g.ty);
        // A field of a reflected struct is printed through the registry.
        for (fields, 0..) |field_types, id| {
            if (!reflected[id]) continue;
            for (field_types) |t| visitor.nested(t);
        }

        const skip = try alloc.alloc(bool, fields.len);
        for (skip, fields, needs, reflected, 0..) |*s, field_types, need, refl, id| {
            s.* = id != 0 and !need and !refl and allScalar(field_types);
        }
        return .{ .struct_fields = fields, .skip_descriptor = skip };
    }

    const NeedsVisitor = struct {
        needs: []bool,
        program: *const ir.Program,

        /// A value of `t`: a struct here is copied by word count, so only
        /// structs inside containers the runtime walks need a descriptor.
        fn value(self: *NeedsVisitor, t: HIRType) void {
            switch (t) {
                .Array => |a| self.element(a.element.*, a.size != null),
                .Map => |m| {
                    self.nested(m.key.*);
                    self.nested(m.value.*);
                },
                .Union => |u| for (u.members) |m| self.nested(m.*),
                .Group => |gid| for (self.program.group_members[gid]) |m| self.nested(m),
                .Function => |f| {
                    for (f.params) |p| self.nested(p.*);
                    self.nested(f.ret.*);
                },
                else => {},
            }
        }

        /// An array element: a struct of a one-dimensional array is sized by
        /// the header (`elem_words`) or stored flat when fixed.
        fn element(self: *NeedsVisitor, t: HIRType, fixed: bool) void {
            switch (t) {
                .Struct => {},
                .Array => |a| if (fixed and a.size != null) self.element(a.element.*, true) else self.nested(t),
                else => self.nested(t),
            }
        }

        fn nested(self: *NeedsVisitor, t: HIRType) void {
            switch (t) {
                .Struct => |id| self.needs[id] = true,
                .Array => |a| self.nested(a.element.*),
                .Map => |m| {
                    self.nested(m.key.*);
                    self.nested(m.value.*);
                },
                .Union => |u| for (u.members) |m| self.nested(m.*),
                .Group => |gid| for (self.program.group_members[gid]) |m| self.nested(m),
                .Function => |f| {
                    for (f.params) |p| self.nested(p.*);
                    self.nested(f.ret.*);
                },
                else => {},
            }
        }
    };
};
