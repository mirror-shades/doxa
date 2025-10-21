const std = @import("std");
const HIRType = @import("soxa_types.zig").HIRType;
const ScopeKind = @import("soxa_types.zig").ScopeKind;

pub const SymbolTable = struct {
    variables: std.StringHashMap(u32),
    local_variables: std.StringHashMap(u32),
    variable_count: u32,
    local_variable_count: u32,

    variable_types: std.StringHashMap(HIRType),
    variable_custom_types: std.StringHashMap([]const u8),

    variable_array_element_types: std.StringHashMap(HIRType),

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
        self.variable_union_members_by_index.deinit();
        self.alias_parameters.deinit();
    }

    pub fn enterFunctionScope(self: *SymbolTable, function_name: []const u8) !void {
        self.current_function = function_name;

        self.local_variables.deinit();
        self.local_variables = std.StringHashMap(u32).init(self.allocator);
        self.local_variable_count = 0;

        self.alias_parameters.deinit();
        self.alias_parameters = std.StringHashMap(void).init(self.allocator);
    }

    pub fn exitFunctionScope(self: *SymbolTable) void {
        self.current_function = null;
    }

    pub fn getOrCreateVariable(self: *SymbolTable, name: []const u8) !u32 {
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
            if (self.variables.get(name)) |global_idx| {
                return global_idx;
            }
            const idx = self.local_variable_count;
            try self.local_variables.put(name, idx);
            self.local_variable_count += 1;
            return idx;
        }

        if (self.variables.get(name)) |idx| {
            return idx;
        }
        const idx = self.variable_count;
        try self.variables.put(name, idx);
        self.variable_count += 1;
        return idx;
    }

    pub fn createVariable(self: *SymbolTable, name: []const u8) !u32 {
        if (self.current_function != null) {
            const idx = self.local_variable_count;
            try self.local_variables.put(name, idx);
            self.local_variable_count += 1;
            return idx;
        } else {
            const idx = self.variable_count;
            try self.variables.put(name, idx);
            self.variable_count += 1;
            return idx;
        }
    }

    pub fn getVariable(self: *SymbolTable, name: []const u8) ?u32 {
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
            return self.variables.get(name);
        }
        return self.variables.get(name);
    }

    pub fn trackVariableType(self: *SymbolTable, var_name: []const u8, var_type: HIRType) !void {
        try self.variable_types.put(var_name, var_type);
    }

    pub fn determineVariableScope(self: *SymbolTable, var_name: []const u8) ScopeKind {
        if (self.current_function != null) {
            const in_local = self.local_variables.get(var_name);
            const in_global = self.variables.get(var_name);

            if (in_local) |_| {
                return .Local;
            } else if (in_global) |_| {
                return .GlobalLocal;
            } else {
                return .Local;
            }
        } else {
            return .GlobalLocal;
        }
    }

    pub fn getTrackedVariableType(self: *SymbolTable, var_name: []const u8) ?HIRType {
        return self.variable_types.get(var_name);
    }

    pub fn trackVariableCustomType(self: *SymbolTable, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.variable_custom_types.put(var_name, custom_type_name);
    }

    pub fn getVariableCustomType(self: *SymbolTable, var_name: []const u8) ?[]const u8 {
        return self.variable_custom_types.get(var_name);
    }

    pub fn trackArrayElementType(self: *SymbolTable, var_name: []const u8, elem_type: HIRType) !void {
        try self.variable_array_element_types.put(var_name, elem_type);
    }

    pub fn getTrackedArrayElementType(self: *SymbolTable, var_name: []const u8) ?HIRType {
        return self.variable_array_element_types.get(var_name);
    }

    pub fn trackVariableUnionMembersByIndex(self: *SymbolTable, var_index: u32, members: [][]const u8) !void {
        try self.variable_union_members_by_index.put(var_index, members);
    }

    pub fn trackAliasParameter(self: *SymbolTable, var_name: []const u8) !void {
        try self.alias_parameters.put(var_name, {});
    }

    pub fn isAliasParameter(self: *SymbolTable, var_name: []const u8) bool {
        return self.alias_parameters.contains(var_name);
    }

    pub fn getUnionMembersByIndex(self: *SymbolTable, var_index: u32) ?[][]const u8 {
        return self.variable_union_members_by_index.get(var_index);
    }
};
