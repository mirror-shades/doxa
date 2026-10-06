const std = @import("std");
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;
const module_graph = @import("../../module/graph.zig");

pub const StructId = u32;
pub const EnumId = u32;
pub const UnionId = u32;
pub const GroupId = u32;

pub const ArrayStorageKind = enum {
    dynamic,
    fixed,
    const_literal,
};

pub const HIRType = union(enum) {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,

    Array: *const HIRType,
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
};

pub fn arrayInnermostElementType(element_type: HIRType) ?HIRType {
    var cursor = element_type;
    var found_nested = false;
    while (true) {
        switch (cursor) {
            .Array => |inner| {
                cursor = inner.*;
                found_nested = true;
            },
            else => {
                if (found_nested) {
                    return cursor;
                }
                return null;
            },
        }
    }
}

pub const ArrayTypeInfo = struct {
    element_type: HIRType,
    size: ?u32,
    nested_element_type: ?*ArrayTypeInfo = null,
};

pub const StructTypeInfo = struct {
    name: []const u8,
    fields: []StructFieldInfo,
};

pub const StructFieldInfo = struct {
    name: []const u8,
    field_type: HIRType,
    offset: u32,
};

pub const MapTypeInfo = struct {
    key_type: HIRType,
    value_type: HIRType,
    has_else: bool,
};

pub const EnumTypeInfo = struct {
    name: []const u8,
    variants: [][]const u8,
};

pub const ScopeKind = enum {
    Local,
    GlobalLocal,
    ModuleGlobal,
    ImportedModule,
    Builtin,
};

/// How a heap value is copied when stored into a variable.
///
/// `rehome` keeps identity when the source already outlives the destination
/// (e.g. `each n in global_array` — `n` should alias the element). `snapshot`
/// always clones; used for by-value function parameters so the callee cannot
/// mutate the caller's struct. `keep` stores the pointer as-is after an
/// in-place mutation (`SetField`, `@push`) — cloning would disconnect the
/// write from the object that was just updated.
pub const HeapCopyKind = enum {
    rehome,
    snapshot,
    keep,
};

pub const CallKind = enum {
    /// A Doxa function of the program, defined in its function table.
    DoxaFunction,
    /// A function of an inline `zig` block or a `.zig` file: an external
    /// wrapper symbol called through the inline-Zig ABI.
    ZigFunction,
    BuiltinFunction,
};

/// Metadata for a compiled Doxa function: a top-level function or a struct
/// method of any module. Shared by HIRGenerator (codegen) and TypeSystem (type
/// inference).
pub const FunctionInfo = struct {
    /// The emitted link name (`ModuleGraph.mangle`).
    name: []const u8,
    /// The struct whose instance method this is, which binds `this`; null for a
    /// top-level function or a static method.
    receiver: ?StructId = null,
    arity: u32,
    return_type: HIRType,
    start_label: []const u8,
    body_label: ?[]const u8 = null,
    local_var_count: u32,
    is_entry: bool,
    param_is_alias: []bool,
    param_is_readonly: []bool,
    param_types: []HIRType,
};

/// Signatures keyed by defining `(ModuleId, declared name)` — `fn` for a
/// top-level function, `Struct.method` for a method.
pub const FunctionSignatureMap = std.HashMap(
    module_graph.SymbolKey,
    FunctionInfo,
    module_graph.SymbolKeyContext,
    std.hash_map.default_max_load_percentage,
);

pub const HIRProgram = struct {
    instructions: []HIRInstruction,
    constant_pool: []HIRValue,
    string_pool: [][]const u8,
    function_table: []HIRProgram.HIRFunction,
    allocator: std.mem.Allocator,
    /// B2: struct type names reaching a reflection site. Owned by the program
    /// (a copy of the generator's set) so it stays valid after the generator is
    /// deinited; the inner name slices are borrowed from the struct table, which
    /// outlives the program.
    reflected_structs: ?std.StringHashMap(void) = null,
    /// B2: a group/unknown reflection target disables per-type descriptor skips.
    force_struct_descriptors: bool = false,
    /// The inline-Zig functions the program calls, each with the parameter
    /// types its generated wrapper takes.
    zig_functions: []const ZigFunction = &.{},

    pub const ZigFunction = struct {
        link_name: []const u8,
        param_types: []const HIRType,
        return_type: HIRType,
    };

    pub fn deinit(self: *HIRProgram) void {
        self.allocator.free(self.instructions);
        self.allocator.free(self.constant_pool);

        for (self.string_pool) |str| {
            self.allocator.free(str);
        }
        self.allocator.free(self.string_pool);

        self.allocator.free(self.function_table);
        for (self.zig_functions) |function| self.allocator.free(function.param_types);
        self.allocator.free(self.zig_functions);
        if (self.reflected_structs) |*reflected| reflected.deinit();
    }

    pub const HIRFunction = struct {
        /// The emitted link name.
        qualified_name: []const u8,
        /// The struct whose instance method this is (binds `this`).
        receiver: ?StructId = null,
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        body_label: ?[]const u8 = null,
        start_ip: u32 = 0,
        body_ip: ?u32 = null,
        local_var_count: u32,
        is_entry: bool,
        param_is_alias: []bool,
        param_is_readonly: []bool,
        param_types: []HIRType,
    };

};
