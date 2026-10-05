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
    LocalFunction,
    ModuleFunction,
    BuiltinFunction,
};

/// Metadata for a compiled user-defined or module function.
/// Shared by HIRGenerator (codegen) and TypeSystem (type inference).
pub const FunctionInfo = struct {
    name: []const u8,
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

/// Signatures keyed by defining `(ModuleId, declared name)`. This is the
/// authoritative store; `name` remains the temporary emitted link spelling
/// until Phase 6 renders the versioned mangling from the same key.
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
    module_map: std.StringHashMap(ModuleInfo),
    allocator: std.mem.Allocator,
    /// B2: struct type names reaching a reflection site. Owned by the program
    /// (a copy of the generator's set) so it stays valid after the generator is
    /// deinited; the inner name slices are borrowed from the struct table, which
    /// outlives the program.
    reflected_structs: ?std.StringHashMap(void) = null,
    /// B2: a group/unknown reflection target disables per-type descriptor skips.
    force_struct_descriptors: bool = false,

    pub fn deinit(self: *HIRProgram) void {
        self.allocator.free(self.instructions);
        self.allocator.free(self.constant_pool);

        for (self.string_pool) |str| {
            self.allocator.free(str);
        }
        self.allocator.free(self.string_pool);

        self.allocator.free(self.function_table);
        self.module_map.deinit();
        if (self.reflected_structs) |*reflected| reflected.deinit();
    }

    pub const HIRFunction = struct {
        name: []const u8,
        qualified_name: []const u8,
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

    pub const ModuleInfo = struct {
        name: []const u8,
        imports: [][]const u8,
        exports: [][]const u8,
        global_var_count: u32,
    };
};
