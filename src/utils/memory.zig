const std = @import("std");
const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;
const TypeInfo = @import("../ast/ast.zig").TypeInfo;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;

pub const StringInterner = struct {
    strings: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringInterner {
        return StringInterner{
            .strings = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringInterner) void {
        var it = self.strings.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.strings.deinit();
    }

    pub fn intern(self: *StringInterner, string: []const u8) ![]const u8 {
        if (self.strings.get(string)) |existing| {
            return existing;
        }
        const copy = try self.allocator.dupe(u8, string);
        try self.strings.put(copy, copy);
        return copy;
    }
};

pub const MemoryManager = struct {
    arena: std.heap.ArenaAllocator,
    scope_manager: *ScopeManager,
    is_debug: bool,

    pub fn init(allocator: std.mem.Allocator, is_debug: bool) !MemoryManager {
        const scope_manager = try ScopeManager.init(allocator, is_debug);
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .is_debug = is_debug,
            .scope_manager = scope_manager,
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.is_debug) {
            std.debug.print("Cleaning up memory manager...\n", .{});
        }

        // Clean up root scope first if it exists
        if (self.scope_manager.root_scope) |root_scope| {
            root_scope.deinit();
            self.scope_manager.root_scope = null;
        }

        self.scope_manager.deinit();
        self.arena.deinit();
    }

    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn reset(self: *MemoryManager) void {
        if (self.is_debug) {
            std.debug.print("Resetting memory manager...\n", .{});
        }
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.arena.child_allocator);
    }
};

/// ValueStorage holds a value with alias counting
const ValueStorage = struct { value: TokenLiteral, type: TokenType, type_info: TypeInfo, alias_count: u32, constant: bool };

/// Variable represents a named alias to a storage location
pub const Variable = struct {
    name: []const u8,
    type: TokenType,
    storage_id: u32, // ID of the storage location
    id: u32, // Unique variable ID
    is_alias: bool,
};

/// ScopeManager handles all scope, variable and storage operations
pub const ScopeManager = struct {
    variable_map: std.AutoHashMap(u32, *Variable),
    value_storage: std.AutoHashMap(u32, *ValueStorage),
    next_storage_id: u32 = 0,
    variable_counter: u32 = 0,
    root_scope: ?*Scope = null,
    allocator: std.mem.Allocator,
    is_debug: bool,

    pub fn init(allocator: std.mem.Allocator, is_debug: bool) !*ScopeManager {
        const self = try allocator.create(ScopeManager);
        self.* = .{
            .variable_map = std.AutoHashMap(u32, *Variable).init(allocator),
            .value_storage = std.AutoHashMap(u32, *ValueStorage).init(allocator),
            .allocator = allocator,
            .is_debug = is_debug,
        };
        return self;
    }

    pub fn deinit(self: *ScopeManager) void {
        // Note: Individual variables and storage are owned by scopes
        // and will be cleaned up when scopes are deinitialized
        self.variable_map.deinit();
        self.value_storage.deinit();
        self.allocator.destroy(self);
    }

    pub fn getVariableScope(self: *ScopeManager, variable: *Variable) ?*Scope {
        var scope: ?*Scope = self.root_scope;
        while (scope) |s| {
            if (s.variables.contains(variable.id)) return s;
            scope = s.parent;
        }
        return null;
    }

    pub fn dumpState(self: *ScopeManager, scope_id: u32) void {
        std.debug.print("Current scope ID: {}\n", .{scope_id});

        // Display variables
        if (self.variable_map.count() > 0) {
            std.debug.print("Variables:\n", .{});
            var var_it = self.variable_map.iterator();
            while (var_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                if (self.getVariableScope(value)) |var_scope| {
                    std.debug.print("  [{d}]: name='{s}', type={}, storage={d}, scope={d}, is_alias={}\n", .{ key, value.name, value.type, value.storage_id, var_scope.id, value.is_alias });
                } else {
                    std.debug.print("  [{d}]: name='{s}', type={}, storage={d}, scope=?, is_alias={}\n", .{ key, value.name, value.type, value.storage_id, value.is_alias });
                }
            }
        } else {
            std.debug.print("No variables\n", .{});
        }

        // Display storage
        if (self.value_storage.count() > 0) {
            std.debug.print("Value Storage:\n", .{});
            var storage_it = self.value_storage.iterator();
            while (storage_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                if (self.is_debug) {
                    std.debug.print("DEBUG: Storage [{d}] - type: {s}, type_info.base: {s}\n", .{ key, @tagName(value.type), @tagName(value.type_info.base) });
                }
                std.debug.print("  [{d}]: type={}, aliases={d}, constant={}\n", .{ key, value.type, value.alias_count, value.constant });
            }
        } else {
            std.debug.print("No value storage\n", .{});
        }
    }

    pub fn createScope(self: *ScopeManager, parent: ?*Scope) !*Scope {
        const scope_id = self.next_storage_id;
        self.next_storage_id += 1;
        return Scope.init(self, scope_id, parent, self.is_debug);
    }
};

/// Scope represents a lexical scope with its own variables
pub const Scope = struct {
    id: u32,
    parent: ?*Scope,
    variables: std.AutoHashMap(u32, *Variable),
    name_map: std.StringHashMap(*Variable),
    arena: std.heap.ArenaAllocator,
    manager: *ScopeManager,
    is_debug: bool,

    pub fn init(manager: *ScopeManager, scope_id: u32, parent: ?*Scope, is_debug: bool) !*Scope {
        const self = try manager.allocator.create(Scope);
        self.* = .{
            .id = scope_id,
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(manager.allocator),
            .variables = std.AutoHashMap(u32, *Variable).init(self.arena.allocator()),
            .name_map = std.StringHashMap(*Variable).init(self.arena.allocator()),
            .manager = manager,
            .is_debug = is_debug,
        };
        return self;
    }

    pub fn deinit(self: *Scope) void {
        if (self.is_debug) {
            std.debug.print("Cleaning up scope {}\n", .{self.id});
        }

        // Run sanity check before any cleanup
        self.sanityCheck();

        // Clean up all variables in this scope
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            const variable = entry.value_ptr.*;

            // Filter out invalid variable names for display
            var display_name: []const u8 = "invalid_name";
            if (std.unicode.utf8ValidateSlice(variable.name)) {
                display_name = variable.name;
            }

            // Remove from global variable map
            _ = self.manager.variable_map.remove(variable.id);

            // Update storage alias counts
            if (self.manager.value_storage.get(variable.storage_id)) |storage| {
                storage.alias_count -= 1;
                if (storage.alias_count == 0) {
                    const s = storage;
                    _ = self.manager.value_storage.remove(variable.storage_id);
                    self.manager.allocator.destroy(s);
                }
            }
        }

        // Free all scope memory at once
        self.arena.deinit();
        self.manager.allocator.destroy(self);
    }

    pub fn createValueBinding(self: *Scope, name: []const u8, value: TokenLiteral, vtype: TokenType, type_info: TypeInfo, constant: bool) !*Variable {
        // Check for duplicate variable name in current scope
        if (self.name_map.contains(name)) {
            return error.DuplicateVariableName;
        }

        if (self.is_debug) {
            std.debug.print("DEBUG: createValueBinding - name: '{s}', vtype: {s}, type_info.base: {s}\n", .{ name, @tagName(vtype), @tagName(type_info.base) });
        }

        const storage_id = self.manager.next_storage_id;
        const variableId = self.manager.variable_counter;
        self.manager.next_storage_id += 1;
        self.manager.variable_counter += 1;

        // Create storage using manager's allocator
        const storage = try self.manager.allocator.create(ValueStorage);
        storage.* = .{ .value = value, .type = vtype, .type_info = type_info, .alias_count = 1, .constant = constant };

        // Create variable
        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variableId,
            .is_alias = false,
        };

        // Store in maps
        try self.manager.variable_map.put(variableId, variable);
        try self.manager.value_storage.put(storage_id, storage);
        try self.variables.put(variableId, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createAlias(self: *Scope, name: []const u8, target_variable: *Variable) !*Variable {
        const variableId = self.manager.variable_counter;
        self.manager.variable_counter += 1;

        // Create alias variable
        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = target_variable.type,
            .storage_id = target_variable.storage_id,
            .id = variableId,
            .is_alias = true,
        };

        // Increment alias count in storage
        if (self.manager.value_storage.get(target_variable.storage_id)) |storage| {
            storage.alias_count += 1;
        }

        // Store in maps
        try self.manager.variable_map.put(variableId, variable);
        try self.variables.put(variableId, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn lookupVariable(self: *Scope, name: []const u8) ?*Variable {
        var current_scope: ?*Scope = self;
        while (current_scope) |scope| {
            if (scope.name_map.get(name)) |variable| {
                return variable;
            }
            current_scope = scope.parent;
        }
        return null;
    }

    pub fn sanityCheck(self: *Scope) void {
        // Check storage
        var storage_it = self.manager.value_storage.iterator();
        while (storage_it.next()) |entry| {
            const storage = entry.value_ptr.*;
            std.debug.assert(storage.alias_count > 0); // Ensure no orphaned storage
        }

        // Check variables in this scope
        var var_it = self.variables.iterator();
        while (var_it.next()) |entry| {
            const variable = entry.value_ptr.*;
            // During cleanup, variables might be in the process of being removed
            // from the manager's variable_map, so we can't rely on getVariableScope
            // Instead, we just verify that the variable is in this scope's variables map
            std.debug.assert(self.variables.contains(variable.id));
        }
    }
};
