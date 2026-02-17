const std = @import("std");
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const CustomTypeInfo = TypesImport.CustomTypeInfo;

pub const ValueStorage = struct {
    value: TokenLiteral,
    type: TokenType,
    type_info: *TypeInfo,
    constant: bool,
};

pub const Variable = struct {
    name: []const u8,
    type: TokenType,
    storage_id: u32,
    id: u32,
    is_alias: bool,
};

pub const Scope = struct {
    id: u32,
    parent: ?*Scope,
    variables: std.AutoHashMap(u32, *Variable),
    name_map: std.StringHashMap(*Variable),
    allocator: std.mem.Allocator,
    manager: *ScopeManager,
    memory_manager: *MemoryManager,
    arena: std.heap.ArenaAllocator,
    is_deinited: bool = false,

    pub fn init(scope_id: u32, parent: ?*Scope, memory_manager: *MemoryManager) Scope {
        return .{
            .id = scope_id,
            .parent = parent,
            .allocator = memory_manager.allocator,
            .variables = std.AutoHashMap(u32, *Variable).init(memory_manager.allocator),
            .name_map = std.StringHashMap(*Variable).init(memory_manager.allocator),
            .manager = memory_manager.scope_manager,
            .memory_manager = memory_manager,
            .arena = std.heap.ArenaAllocator.init(memory_manager.allocator),
            .is_deinited = false,
        };
    }

    pub fn deinit(self: *Scope) void {
        if (self.is_deinited) return;
        self.is_deinited = true;

        var it = self.variables.valueIterator();
        while (it.next()) |var_ptr| {
            const variable = var_ptr.*;
            _ = self.manager.variable_map.remove(variable.id);
            if (!variable.is_alias) {
                if (self.manager.value_storage.fetchRemove(variable.storage_id)) |kv| {
                    self.allocator.destroy(kv.value);
                }
            }
            self.allocator.destroy(variable);
        }
        self.variables.deinit();
        self.name_map.deinit();
        self.arena.deinit();
    }

    pub fn createValueBinding(
        self: *Scope,
        name: []const u8,
        value: TokenLiteral,
        vtype: TokenType,
        type_info: *TypeInfo,
        constant: bool,
    ) !*Variable {
        if (self.name_map.contains(name)) {
            return error.DuplicateVariableName;
        }

        const storage_id = self.manager.next_storage_id;
        const variable_id = self.manager.variable_counter;
        self.manager.next_storage_id += 1;
        self.manager.variable_counter += 1;

        const storage = try self.allocator.create(ValueStorage);
        storage.* = .{ .value = value, .type = vtype, .type_info = type_info, .constant = constant };

        const variable = try self.allocator.create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variable_id,
            .is_alias = false,
        };

        try self.manager.variable_map.put(variable_id, variable);
        try self.manager.value_storage.put(storage_id, storage);
        try self.variables.put(variable_id, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createAlias(self: *Scope, name: []const u8, target: *Variable) !*Variable {
        const variable_id = self.manager.variable_counter;
        self.manager.variable_counter += 1;

        const variable = try self.allocator.create(Variable);
        variable.* = .{
            .name = name,
            .type = target.type,
            .storage_id = target.storage_id,
            .id = variable_id,
            .is_alias = true,
        };

        try self.manager.variable_map.put(variable_id, variable);
        try self.variables.put(variable_id, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createAliasFromStorageId(
        self: *Scope,
        name: []const u8,
        storage_id: u32,
        vtype: TokenType,
        type_info: *TypeInfo,
    ) !*Variable {
        _ = type_info;
        const variable_id = self.manager.variable_counter;
        self.manager.variable_counter += 1;

        const variable = try self.allocator.create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variable_id,
            .is_alias = true,
        };

        try self.manager.variable_map.put(variable_id, variable);
        try self.variables.put(variable_id, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn lookupVariable(self: *Scope, name: []const u8) ?*Variable {
        if (self.name_map.get(name)) |v| return v;
        if (self.parent) |p| return p.lookupVariable(name);
        return null;
    }
};

pub const ScopeManager = struct {
    allocator: std.mem.Allocator,
    variable_map: std.AutoHashMap(u32, *Variable),
    value_storage: std.AutoHashMap(u32, *ValueStorage),
    next_storage_id: u32 = 0,
    next_scope_id: u32 = 0,
    variable_counter: u32 = 0,
    root_scope: ?*Scope = null,

    pub fn init(allocator: std.mem.Allocator) !*ScopeManager {
        const self = try allocator.create(ScopeManager);
        self.* = .{
            .allocator = allocator,
            .variable_map = std.AutoHashMap(u32, *Variable).init(allocator),
            .value_storage = std.AutoHashMap(u32, *ValueStorage).init(allocator),
        };
        return self;
    }

    pub fn deinit(self: *ScopeManager) void {
        self.variable_map.deinit();
        self.value_storage.deinit();
        self.allocator.destroy(self);
    }

    pub fn createScope(self: *ScopeManager, parent: ?*Scope, memory_manager: *MemoryManager) !*Scope {
        const scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        return memory_manager.createScope(scope_id, parent);
    }

    pub fn createScopeWithId(self: *ScopeManager, parent: ?*Scope, memory_manager: *MemoryManager, scope_id: u32) !*Scope {
        if (scope_id >= self.next_scope_id) {
            self.next_scope_id = scope_id + 1;
        }
        return memory_manager.createScope(scope_id, parent);
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

        if (self.value_storage.count() > 0) {
            std.debug.print("Storage:\n", .{});
            var storage_it = self.value_storage.iterator();
            while (storage_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                std.debug.print("  [{d}]: type={}, constant={}\n", .{ key, value.type, value.constant });
            }
        }
    }

    pub fn dumpStateWithReporter(self: *ScopeManager, reporter: anytype) void {
        const scope_id = if (self.root_scope) |rs| rs.id else 0;
        reporter.debugMemory(null, null, "Current scope ID: {}", .{scope_id});

        if (self.variable_map.count() > 0) {
            reporter.debugMemory(null, null, "Variables:", .{});
            var var_it = self.variable_map.iterator();
            while (var_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                if (self.getVariableScope(value)) |var_scope| {
                    reporter.debugMemory(null, null, "  [{d}]: name='{s}', type={}, storage={d}, scope={d}, is_alias={}", .{ key, value.name, value.type, value.storage_id, var_scope.id, value.is_alias });
                } else {
                    reporter.debugMemory(null, null, "  [{d}]: name='{s}', type={}, storage={d}, scope=?, is_alias={}", .{ key, value.name, value.type, value.storage_id, value.is_alias });
                }
            }
        } else {
            reporter.debugMemory(null, null, "No variables", .{});
        }

        if (self.value_storage.count() > 0) {
            reporter.debugMemory(null, null, "Storage:", .{});
            var storage_it = self.value_storage.iterator();
            while (storage_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                reporter.debugMemory(null, null, "  [{d}]: type={}, constant={}", .{ key, value.type, value.constant });
            }
        }
    }
};

pub const MemoryManager = struct {
    allocator: std.mem.Allocator,
    analysis_arena: std.heap.ArenaAllocator,
    execution_arena: std.heap.ArenaAllocator,
    scope_manager: *ScopeManager,
    scope_pool: std.ArrayListUnmanaged(*Scope),
    type_registry: std.StringHashMap(CustomTypeInfo),

    pub fn init(allocator: std.mem.Allocator) !MemoryManager {
        return .{
            .allocator = allocator,
            .analysis_arena = std.heap.ArenaAllocator.init(allocator),
            .execution_arena = std.heap.ArenaAllocator.init(allocator),
            .scope_manager = try ScopeManager.init(allocator),
            .scope_pool = .{},
            .type_registry = std.StringHashMap(CustomTypeInfo).init(allocator),
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.scope_manager.root_scope) |root_scope| {
            root_scope.deinit();
            self.scope_manager.root_scope = null;
        }

        for (self.scope_pool.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.scope_pool.deinit(self.allocator);

        self.scope_manager.deinit();
        self.type_registry.deinit();
        self.analysis_arena.deinit();
        self.execution_arena.deinit();
    }

    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.allocator;
    }

    pub fn getAnalysisAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.analysis_arena.allocator();
    }

    pub fn getExecutionAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.execution_arena.allocator();
    }

    pub fn createScope(self: *MemoryManager, scope_id: u32, parent: ?*Scope) !*Scope {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(scope_id, parent, self);
        try self.scope_pool.append(self.allocator, scope);
        return scope;
    }

    pub fn registerCustomType(self: *MemoryManager, type_info: CustomTypeInfo) !void {
        try self.type_registry.put(type_info.name, type_info);
    }

    pub fn getCustomType(self: *MemoryManager, type_name: []const u8) ?CustomTypeInfo {
        return self.type_registry.get(type_name);
    }

    pub fn bridgeTypesToVM(self: *MemoryManager, vm: anytype) !void {
        var it = self.type_registry.iterator();
        while (it.next()) |entry| {
            try vm.registerCustomType(entry.value_ptr.*);
        }
    }

    pub fn dumpState(self: *MemoryManager, reporter: anytype) void {
        self.scope_manager.dumpStateWithReporter(reporter);
    }

    pub fn transitionToGeneration(self: *MemoryManager) !void {
        _ = self;
    }

    pub fn transitionToExecution(self: *MemoryManager) !void {
        if (self.scope_manager.root_scope == null) {
            const scope = try self.scope_manager.createScope(null, self);
            self.scope_manager.root_scope = scope;
        }
    }

    pub fn reset(self: *MemoryManager) void {
        _ = self;
    }

    pub fn resetExecutionMemory(self: *MemoryManager) void {
        _ = self;
    }
};
