const std = @import("std");
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;

//==================================================================
// SUPPORTING TYPES
//==================================================================

pub const StructId = u32;
pub const EnumId = u32;

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

    // Arrays carry element type metadata
    Array: *const HIRType,

    // Map carries both key & value types
    Map: struct { key: *const HIRType, value: *const HIRType },

    // Store IDs; look up details in a table
    Struct: StructId,
    Enum: EnumId,

    // Optional: carry function sig by pointers/ids
    Function: struct {
        params: []const *const HIRType,
        ret: *const HIRType,
    },

    // OK: slice is a pointer+len; members are pointers (no by-value recursion)
    Union: []const *const HIRType,

    Unknown,
    Poison, // Poison type for malformed/invalid types
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

// Additional type information for complex types
pub const ArrayTypeInfo = struct {
    element_type: HIRType,
    size: ?u32, // null = dynamic array
    nested_element_type: ?*ArrayTypeInfo = null, // For nested arrays like int[][]
};

pub const StructTypeInfo = struct {
    name: []const u8,
    fields: []StructFieldInfo,
};

pub const StructFieldInfo = struct {
    name: []const u8,
    field_type: HIRType,
    offset: u32, // Byte offset for efficient access
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
    Local, // Function-local variable
    GlobalLocal, // Script-level global (accessible from functions, but local to script execution)
    ModuleGlobal, // Module-level global (persistent across modules)
    ImportedModule, // Variable from imported module
    Builtin, // Built-in system variable
};

pub const CallKind = enum {
    LocalFunction, // Function in current module
    ModuleFunction, // Function in imported module
    BuiltinFunction, // Built-in function
};

//==================================================================
// HIR PROGRAM REPRESENTATION
//==================================================================

pub const HIRProgram = struct {
    instructions: []HIRInstruction,
    constant_pool: []HIRValue, // VM: Direct constant access
    string_pool: [][]const u8, // Shared string literals
    function_table: []HIRProgram.HIRFunction, // VM: Function metadata
    module_map: std.StringHashMap(ModuleInfo), // LLVM: Module context
    allocator: std.mem.Allocator, // NEW: Keep allocator for cleanup

    pub fn deinit(self: *HIRProgram) void {
        self.allocator.free(self.instructions);
        self.allocator.free(self.constant_pool);

        // Free individual strings
        for (self.string_pool) |str| {
            self.allocator.free(str);
        }
        self.allocator.free(self.string_pool);

        self.allocator.free(self.function_table);
        self.module_map.deinit();
    }

    pub const HIRFunction = struct {
        name: []const u8,
        qualified_name: []const u8, // module.function for LLVM
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        body_label: ?[]const u8 = null, // For tail call optimization - jumps here to skip parameter setup
        // Cached instruction indices for fast calls (filled by VM at init)
        start_ip: u32 = 0,
        body_ip: ?u32 = null,
        local_var_count: u32, // VM: stack frame sizing
        is_entry: bool,
        param_is_alias: []bool, // NEW: Track which parameters are aliases
        param_types: []HIRType, // NEW: Track parameter types for VM binding
    };

    pub const ModuleInfo = struct {
        name: []const u8,
        imports: [][]const u8,
        exports: [][]const u8,
        global_var_count: u32,
    };
};
