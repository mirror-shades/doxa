const std = @import("std");
const SoxaTypes = @import("soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const ScopeKind = SoxaTypes.ScopeKind;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;

pub const SymbolTable = struct {
    variables: std.StringHashMap(u32),
    local_variables: std.StringHashMap(u32),
    variable_count: u32,
    local_variable_count: u32,

    variable_types: std.StringHashMap(HIRType),
    variable_custom_types: std.StringHashMap([]const u8),

    variable_array_element_types: std.StringHashMap(HIRType),
    variable_array_storage: std.StringHashMap(ArrayStorageKind),

    variable_union_members_by_index: std.AutoHashMap(u32, [][]const u8),

    alias_parameters: std.StringHashMap(void),

    current_function: ?[]const u8,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return SymbolTable{
            .variables = std.StringHashMap(u32).init(allocator),
            .local_variables = std.StringHashMap(u32).init(allocator),
            .variable_count = 0,
            .local_variable_count = 0,
            .variable_types = std.StringHashMap(HIRType).init(allocator),
            .variable_custom_types = std.StringHashMap([]const u8).init(allocator),
            .variable_array_element_types = std.StringHashMap(HIRType).init(allocator),
            .variable_array_storage = std.StringHashMap(ArrayStorageKind).init(allocator),
            .variable_union_members_by_index = std.AutoHashMap(u32, [][]const u8).init(allocator),
            .alias_parameters = std.StringHashMap(void).init(allocator),
            .current_function = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        self.variables.deinit();
        self.local_variables.deinit();
        self.variable_types.deinit();
        self.variable_custom_types.deinit();
        self.variable_array_element_types.deinit();
        self.variable_array_storage.deinit();
        self.variable_union_members_by_index.deinit();
        self.alias_parameters.deinit();
    }

    /// Enter function scope - resets local variable tracking
    pub fn enterFunctionScope(self: *SymbolTable, function_name: []const u8) !void {
        self.current_function = function_name;

        // Reset per-function local variable tracking to avoid cross-function collisions
        self.local_variables.deinit();
        self.local_variables = std.StringHashMap(u32).init(self.allocator);
        self.local_variable_count = 0;

        // Clear alias parameters for the new function scope
        self.alias_parameters.deinit();
        self.alias_parameters = std.StringHashMap(void).init(self.allocator);
    }

    /// Exit function scope
    pub fn exitFunctionScope(self: *SymbolTable) void {
        self.current_function = null;
    }

    /// Get or create a variable index, handling local vs global scope
    pub fn getOrCreateVariable(self: *SymbolTable, name: []const u8) !u32 {
        // When inside a function, check local variables first, then global
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
            // Check if variable exists globally
            if (self.variables.get(name)) |global_idx| {
                return global_idx;
            }
            // Variable doesn't exist anywhere, create new local variable
            const idx = self.local_variable_count;
            try self.local_variables.put(name, idx);
            self.local_variable_count += 1;
            return idx;
        }

        // Global scope
        if (self.variables.get(name)) |idx| {
            return idx;
        }
        const idx = self.variable_count;
        try self.variables.put(name, idx);
        self.variable_count += 1;
        return idx;
    }

    /// Create a new variable index, always creating a local variable when inside a function
    /// This is used for variable declarations to ensure they shadow global variables
    pub fn createVariable(self: *SymbolTable, name: []const u8) !u32 {
        if (self.current_function != null) {
            // Inside function: always create local variable to shadow any global
            const idx = self.local_variable_count;
            try self.local_variables.put(name, idx);
            self.local_variable_count += 1;
            return idx;
        } else {
            // Global scope: create global variable
            const idx = self.variable_count;
            try self.variables.put(name, idx);
            self.variable_count += 1;
            return idx;
        }
    }

    /// Get existing variable index if it exists
    pub fn getVariable(self: *SymbolTable, name: []const u8) ?u32 {
        // When in function scope, check local variables first, then global
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
            // Also check global variables for cross-scope access
            return self.variables.get(name);
        }
        return self.variables.get(name);
    }

    /// Track a variable's type when it's declared or assigned
    pub fn trackVariableType(self: *SymbolTable, var_name: []const u8, var_type: HIRType) !void {
        try self.variable_types.put(var_name, var_type);
    }

    /// Determine the correct scope for a variable based on where it exists
    pub fn determineVariableScope(self: *SymbolTable, var_name: []const u8) ScopeKind {
        if (self.current_function != null) {
            // Inside function: check if it's a local variable first
            const in_local = self.local_variables.get(var_name);
            const in_global = self.variables.get(var_name);

            if (in_local) |_| {
                return .Local;
            } else if (in_global) |_| {
                // Found in global scope, but we're inside a function
                // This means we're accessing a script-level global variable from within a function
                return .GlobalLocal;
            } else {
                // New variable in function scope
                return .Local;
            }
        } else {
            // Not in function scope - top-level script variables should be GlobalLocal
            // ModuleGlobal should only be used for truly persistent module-level variables
            return .GlobalLocal;
        }
    }

    /// Determine the correct scope for a variable based on where it exists, with module context
    pub fn determineVariableScopeWithModuleContext(self: *SymbolTable, var_name: []const u8, is_module_context: bool) ScopeKind {
        if (self.current_function != null) {
            // Inside function: check if it's a local variable first
            const in_local = self.local_variables.get(var_name);
            const in_global = self.variables.get(var_name);

            if (in_local) |_| {
                return .Local;
            } else if (in_global) |_| {
                // Found in global scope, but we're inside a function
                // This means we're accessing a script-level global variable from within a function
                return .GlobalLocal;
            } else {
                // New variable in function scope
                return .Local;
            }
        } else {
            // Not in function scope
            if (is_module_context) {
                // Module variables should be stored as ModuleGlobal
                return .ModuleGlobal;
            } else {
                // Script variables should be GlobalLocal
                return .GlobalLocal;
            }
        }
    }

    /// Get tracked variable type
    pub fn getTrackedVariableType(self: *SymbolTable, var_name: []const u8) ?HIRType {
        return self.variable_types.get(var_name);
    }

    /// Track a variable's custom type name (for enums/structs)
    pub fn trackVariableCustomType(self: *SymbolTable, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.variable_custom_types.put(var_name, custom_type_name);
    }

    /// Get tracked custom type name for a variable
    pub fn getVariableCustomType(self: *SymbolTable, var_name: []const u8) ?[]const u8 {
        return self.variable_custom_types.get(var_name);
    }

    /// Track array element type for a variable
    pub fn trackArrayElementType(self: *SymbolTable, var_name: []const u8, elem_type: HIRType) !void {
        try self.variable_array_element_types.put(var_name, elem_type);
    }

    /// Get tracked array element type
    pub fn getTrackedArrayElementType(self: *SymbolTable, var_name: []const u8) ?HIRType {
        return self.variable_array_element_types.get(var_name);
    }

    pub fn trackArrayStorageKind(self: *SymbolTable, var_name: []const u8, storage: ArrayStorageKind) !void {
        try self.variable_array_storage.put(var_name, storage);
    }

    pub fn getTrackedArrayStorageKind(self: *SymbolTable, var_name: []const u8) ?ArrayStorageKind {
        return self.variable_array_storage.get(var_name);
    }

    /// Track union member type names by variable index to avoid name collisions
    pub fn trackVariableUnionMembersByIndex(self: *SymbolTable, var_index: u32, members: [][]const u8) !void {
        try self.variable_union_members_by_index.put(var_index, members);
    }

    /// Track an alias parameter
    pub fn trackAliasParameter(self: *SymbolTable, var_name: []const u8) !void {
        try self.alias_parameters.put(var_name, {});
    }

    /// Check if a variable is an alias parameter
    pub fn isAliasParameter(self: *SymbolTable, var_name: []const u8) bool {
        return self.alias_parameters.contains(var_name);
    }

    /// Get union members by variable index
    pub fn getUnionMembersByIndex(self: *SymbolTable, var_index: u32) ?[][]const u8 {
        return self.variable_union_members_by_index.get(var_index);
    }
};
