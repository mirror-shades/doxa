const std = @import("std");
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;

//==================================================================
// SUPPORTING TYPES
//==================================================================

pub const HIRType = enum {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,
    Array,
    Struct,
    Map,
    Enum,
    Function,
    Union,
    Auto, // will throw error in VM, Soxa is explicit
};

// Additional type information for complex types
pub const ArrayTypeInfo = struct {
    element_type: HIRType,
    size: ?u32, // null = dynamic array
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
};

pub const EnumTypeInfo = struct {
    name: []const u8,
    variants: [][]const u8,
};

pub const ScopeKind = enum {
    Local, // Function-local variable
    ModuleGlobal, // Module-level global
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
        local_var_count: u32, // VM: stack frame sizing
        is_entry: bool,
    };

    pub const ModuleInfo = struct {
        name: []const u8,
        imports: [][]const u8,
        exports: [][]const u8,
        global_var_count: u32,
    };
};
