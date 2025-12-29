const std = @import("std");
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;

pub const StructId = u32;
pub const EnumId = u32;
pub const UnionId = u32;

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

pub const CallKind = enum {
    LocalFunction,
    ModuleFunction,
    BuiltinFunction,
};

pub const HIRProgram = struct {
    instructions: []HIRInstruction,
    constant_pool: []HIRValue,
    string_pool: [][]const u8,
    function_table: []HIRProgram.HIRFunction,
    module_map: std.StringHashMap(ModuleInfo),
    allocator: std.mem.Allocator,

    pub fn deinit(self: *HIRProgram) void {
        self.allocator.free(self.instructions);
        self.allocator.free(self.constant_pool);

        for (self.string_pool) |str| {
            self.allocator.free(str);
        }
        self.allocator.free(self.string_pool);

        self.allocator.free(self.function_table);
        self.module_map.deinit();
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
        param_types: []HIRType,
    };

    pub const ModuleInfo = struct {
        name: []const u8,
        imports: [][]const u8,
        exports: [][]const u8,
        global_var_count: u32,
    };
};
