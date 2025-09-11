const std = @import("std");
const HIRType = @import("soxa_types.zig").HIRType;

/// Manages variable declarations, types, and metadata for HIR generation
pub const SymbolTable = struct {
    // Variable index mappings
    variables: std.StringHashMap(u32), // Global scope: name -> index mapping
    local_variables: std.StringHashMap(u32), // Function scope: name -> index mapping
    variable_count: u32,
    local_variable_count: u32,

    // Type tracking
    variable_types: std.StringHashMap(HIRType), // Track inferred types of variables
    variable_custom_types: std.StringHashMap([]const u8), // Track custom type names for variables

    // Array element type tracking (for better index/peek typing)
    variable_array_element_types: std.StringHashMap(HIRType),

    // Union member tracking (for union type variables)
    variable_union_members: std.StringHashMap([][]const u8), // Legacy, may cause collisions across scopes
    variable_union_members_by_index: std.AutoHashMap(u32, [][]const u8), // Index-based to avoid name collisions

    // Function context
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
            .variable_union_members = std.StringHashMap([][]const u8).init(allocator),
            .variable_union_members_by_index = std.AutoHashMap(u32, [][]const u8).init(allocator),
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
        self.variable_union_members.deinit();
        self.variable_union_members_by_index.deinit();
    }

    /// Enter function scope - resets local variable tracking
    pub fn enterFunctionScope(self: *SymbolTable, function_name: []const u8) !void {
        self.current_function = function_name;

        // Reset per-function local variable tracking to avoid cross-function collisions
        self.local_variables.deinit();
        self.local_variables = std.StringHashMap(u32).init(self.allocator);
        self.local_variable_count = 0;
    }

    /// Exit function scope
    pub fn exitFunctionScope(self: *SymbolTable) void {
        self.current_function = null;
    }

    /// Get or create a variable index, handling local vs global scope
    pub fn getOrCreateVariable(self: *SymbolTable, name: []const u8) !u32 {
        // When inside a function, keep variable indices local to the function to avoid collisions
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
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

    /// Get existing variable index if it exists
    pub fn getVariable(self: *SymbolTable, name: []const u8) ?u32 {
        if (self.current_function != null) {
            return self.local_variables.get(name);
        }
        return self.variables.get(name);
    }

    /// Track a variable's type when it's declared or assigned
    pub fn trackVariableType(self: *SymbolTable, var_name: []const u8, var_type: HIRType) !void {
        try self.variable_types.put(var_name, var_type);
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

    /// Track union member type names by variable index to avoid name collisions
    pub fn trackVariableUnionMembersByIndex(self: *SymbolTable, var_index: u32, members: [][]const u8) !void {
        try self.variable_union_members_by_index.put(var_index, members);
    }

    /// Get union members by variable index
    pub fn getUnionMembersByIndex(self: *SymbolTable, var_index: u32) ?[][]const u8 {
        return self.variable_union_members_by_index.get(var_index);
    }

    /// Track union member type names by variable name (legacy support)
    pub fn trackVariableUnionMembers(self: *SymbolTable, var_name: []const u8, members: [][]const u8) !void {
        try self.variable_union_members.put(var_name, members);
    }

    /// Get union members by variable name (legacy support)
    pub fn getUnionMembersByName(self: *SymbolTable, var_name: []const u8) ?[][]const u8 {
        return self.variable_union_members.get(var_name);
    }
};
