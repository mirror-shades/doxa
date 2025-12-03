const std = @import("std");
const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const CustomTypeInfo = TypesImport.CustomTypeInfo;
const CustomTypeInstanceData = TypesImport.CustomTypeInstanceData;
const HIRStruct = @import("../codegen/hir/soxa_values.zig").HIRStruct;
const HIREnum = @import("../codegen/hir/soxa_values.zig").HIREnum;
const HIRStructField = @import("../codegen/hir/soxa_values.zig").HIRStructField;

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
            self.allocator.free(entry.value_ptr.*);
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

pub const Phase = enum {
    Analysis,
    Generation,
    Execution,
};

pub const ProfileEntry = struct { count: u64 = 0, ns: u64 = 0 };

pub const HashMapManager = struct {
    string_hashes: std.array_list.Managed(std.StringHashMap([]const u8)),
    auto_hashes: std.array_list.Managed(std.AutoHashMap(u32, *Variable)),
    profile_hashes: std.array_list.Managed(std.StringHashMap(ProfileEntry)),

    scopes: std.array_list.Managed(*Scope),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) HashMapManager {
        return .{
            .string_hashes = std.array_list.Managed(std.StringHashMap([]const u8)).init(allocator),
            .auto_hashes = std.array_list.Managed(std.AutoHashMap(u32, *Variable)).init(allocator),
            .profile_hashes = std.array_list.Managed(std.StringHashMap(ProfileEntry)).init(allocator),
            .scopes = std.array_list.Managed(*Scope).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HashMapManager) void {
        for (self.string_hashes.items) |*hash| {
            hash.deinit();
        }
        self.string_hashes.deinit();

        for (self.auto_hashes.items) |*hash| {
            hash.deinit();
        }
        self.auto_hashes.deinit();

        for (self.profile_hashes.items) |*hash| {
            hash.deinit();
        }
        self.profile_hashes.deinit();

        for (self.scopes.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.scopes.deinit();
    }

    pub fn createStringHashMap(self: *HashMapManager) !std.StringHashMap([]const u8) {
        const hash = std.StringHashMap([]const u8).init(self.allocator);
        try self.string_hashes.append(hash);
        return hash;
    }

    pub fn createAutoHashMap(self: *HashMapManager) !std.AutoHashMap(u32, *Variable) {
        const hash = std.AutoHashMap(u32, *Variable).init(self.allocator);
        try self.auto_hashes.append(hash);
        return hash;
    }

    pub fn createProfileHashMap(self: *HashMapManager) !std.StringHashMap(ProfileEntry) {
        const hash = std.StringHashMap(ProfileEntry).init(self.allocator);
        try self.profile_hashes.append(hash);
        return hash;
    }

    pub fn createScope(self: *HashMapManager, scope_id: u32, parent: ?*Scope, memory_manager: *MemoryManager) !*Scope {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(scope_id, parent, memory_manager);
        try self.scopes.append(scope);
        return scope;
    }
};

pub const MemoryManager = struct {
    analysis_arena: std.heap.ArenaAllocator,
    execution_arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,

    scope_manager: *ScopeManager,
    hashmap_manager: HashMapManager,

    type_registry: std.StringHashMap(CustomTypeInfo),

    current_phase: Phase = .Analysis,

    pub fn init(allocator: std.mem.Allocator) !MemoryManager {
        const scope_manager = try ScopeManager.init(allocator);
        return .{
            .analysis_arena = std.heap.ArenaAllocator.init(allocator),
            .execution_arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
            .scope_manager = scope_manager,
            .hashmap_manager = HashMapManager.init(allocator),
            .type_registry = std.StringHashMap(CustomTypeInfo).init(allocator),
            .current_phase = .Analysis,
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.scope_manager.root_scope) |root_scope| {
            root_scope.deinit();
            self.scope_manager.root_scope = null;
        }

        self.scope_manager.deinit();
        self.hashmap_manager.deinit();
        self.type_registry.deinit();
        self.analysis_arena.deinit();
        self.execution_arena.deinit();
    }

    pub fn getAnalysisAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.analysis_arena.allocator();
    }

    pub fn getExecutionAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.execution_arena.allocator();
    }

    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.getExecutionAllocator();
    }

    pub fn transitionToGeneration(self: *MemoryManager) !void {
        if (self.current_phase != .Analysis) {
            return error.InvalidPhaseTransition;
        }
        self.current_phase = .Generation;
    }

    pub fn transitionToExecution(self: *MemoryManager) !void {
        if (self.current_phase != .Generation) {
            return error.InvalidPhaseTransition;
        }

        self.resetExecutionMemory();

        if (self.scope_manager.root_scope == null) {
            const execution_scope = try self.scope_manager.createScope(null, self);
            self.scope_manager.root_scope = execution_scope;
        }

        self.current_phase = .Execution;
    }

    pub fn resetExecutionMemory(self: *MemoryManager) void {
        self.execution_arena.deinit();
        self.execution_arena = std.heap.ArenaAllocator.init(self.allocator);

        // Note: custom_type_instances are cleaned up per-scope, not globally
        // Each scope's instances are cleaned up when that scope is deinitialized
    }

    pub fn reset(self: *MemoryManager) void {
        self.resetExecutionMemory();
    }

    pub fn registerCustomType(self: *MemoryManager, type_info: CustomTypeInfo) !void {
        try self.type_registry.put(type_info.name, type_info);
    }

    pub fn getCustomType(self: *MemoryManager, type_name: []const u8) ?CustomTypeInfo {
        return self.type_registry.get(type_name);
    }

    pub fn allocateStruct(self: *MemoryManager, scope: *Scope, type_name: []const u8, fields: []HIRStructField) !*HIRStruct {
        const struct_data = try self.execution_arena.allocator().create(HIRStruct);
        struct_data.* = .{
            .type_name = try self.execution_arena.allocator().dupe(u8, type_name),
            .fields = try self.execution_arena.allocator().dupe(HIRStructField, fields),
            .field_name = null,
            .path = null,
        };

        const instance_data = CustomTypeInstanceData{ .struct_instance = struct_data };
        _ = try scope.createCustomTypeInstance(type_name, instance_data);

        return struct_data;
    }

    pub fn allocateEnum(self: *MemoryManager, scope: *Scope, type_name: []const u8, variant_name: []const u8, variant_index: u32) !*HIREnum {
        const enum_data = try self.execution_arena.allocator().create(HIREnum);
        enum_data.* = .{
            .type_name = try self.execution_arena.allocator().dupe(u8, type_name),
            .variant_name = try self.execution_arena.allocator().dupe(u8, variant_name),
            .variant_index = variant_index,
            .path = null,
        };

        const instance_data = CustomTypeInstanceData{ .enum_instance = enum_data };
        _ = try scope.createCustomTypeInstance(type_name, instance_data);

        return enum_data;
    }

    pub fn bridgeTypesToVM(self: *MemoryManager, vm: anytype) !void {
        var type_iter = self.type_registry.iterator();
        while (type_iter.next()) |entry| {
            try vm.registerCustomType(entry.value_ptr.*);
        }
    }

    pub fn createStringHashMap(self: *MemoryManager) !std.StringHashMap([]const u8) {
        return self.hashmap_manager.createStringHashMap();
    }

    pub fn createAutoHashMap(self: *MemoryManager) !std.AutoHashMap(u32, *Variable) {
        return self.hashmap_manager.createAutoHashMap();
    }

    pub fn createProfileHashMap(self: *MemoryManager) !std.StringHashMap(ProfileEntry) {
        return self.hashmap_manager.createProfileHashMap();
    }

    pub fn createScope(self: *MemoryManager, scope_id: u32, parent: ?*Scope) !*Scope {
        return self.hashmap_manager.createScope(scope_id, parent, self);
    }

    pub fn dumpState(self: *MemoryManager, reporter: anytype) void {
        self.scope_manager.dumpStateWithReporter(reporter);
    }
};

const ValueStorage = struct { value: TokenLiteral, type: TokenType, type_info: *ast.TypeInfo, constant: bool };

pub const Variable = struct {
    name: []const u8,
    type: TokenType,
    storage_id: u32,
    id: u32,
    is_alias: bool,
};

pub const ScopeManager = struct {
    variable_map: std.AutoHashMap(u32, *Variable),
    value_storage: std.AutoHashMap(u32, *ValueStorage),
    next_storage_id: u32 = 0,
    next_scope_id: u32 = 0,
    variable_counter: u32 = 0,
    root_scope: ?*Scope = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*ScopeManager {
        const self = try allocator.create(ScopeManager);
        self.* = .{
            .variable_map = std.AutoHashMap(u32, *Variable).init(allocator),
            .value_storage = std.AutoHashMap(u32, *ValueStorage).init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *ScopeManager) void {
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
};

pub const Scope = struct {
    id: u32,
    parent: ?*Scope,
    variables: std.AutoHashMap(u32, *Variable),
    name_map: std.StringHashMap(*Variable),
    custom_type_instances: std.StringHashMap(CustomTypeInstanceData),
    arena: std.heap.ArenaAllocator,
    manager: *ScopeManager,
    memory_manager: *MemoryManager,

    variables_deinit: bool = false,
    name_map_deinit: bool = false,
    custom_type_instances_deinit: bool = false,

    scope_deinit: bool = false,

    pub fn init(scope_id: u32, parent: ?*Scope, memory_manager: *MemoryManager) Scope {
        return .{
            .id = scope_id,
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(memory_manager.allocator),
            .variables = std.AutoHashMap(u32, *Variable).init(memory_manager.allocator),
            .name_map = std.StringHashMap(*Variable).init(memory_manager.allocator),
            .custom_type_instances = std.StringHashMap(CustomTypeInstanceData).init(memory_manager.allocator),
            .manager = memory_manager.scope_manager,
            .memory_manager = memory_manager,
        };
    }

    pub fn deinit(self: *Scope) void {
        if (self.scope_deinit) {
            return;
        }
        self.scope_deinit = true;

        if (!self.variables_deinit) {
            self.variables.deinit();
            self.variables_deinit = true;
        }
        if (!self.name_map_deinit) {
            self.name_map.deinit();
            self.name_map_deinit = true;
        }
        if (!self.custom_type_instances_deinit) {
            self.custom_type_instances.deinit();
            self.custom_type_instances_deinit = true;
        }

        self.arena.deinit();
    }

    pub fn reserveVariableCapacity(self: *Scope, count: usize) void {
        self.variables.ensureTotalCapacity(@intCast(count)) catch {};
        self.name_map.ensureTotalCapacity(@intCast(count)) catch {};
    }

    pub fn createCrossScopeValueBinding(self: *Scope, name: []const u8, value: TokenLiteral, vtype: TokenType, type_info: *ast.TypeInfo, constant: bool) !*Variable {
        if (self.name_map.contains(name)) {
            return error.DuplicateVariableName;
        }

        var root_scope: *Scope = self;
        while (root_scope.parent) |parent| {
            root_scope = parent;
        }

        const storage_id = self.manager.next_storage_id;
        const variableId = self.manager.variable_counter;
        self.manager.next_storage_id += 1;
        self.manager.variable_counter += 1;

        const storage = try root_scope.arena.allocator().create(ValueStorage);
        storage.* = .{ .value = value, .type = vtype, .type_info = type_info, .constant = constant };

        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variableId,
            .is_alias = false,
        };

        try self.manager.variable_map.put(variableId, variable);
        try self.manager.value_storage.put(storage_id, storage);
        try self.variables.put(variableId, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createValueBinding(self: *Scope, name: []const u8, value: TokenLiteral, vtype: TokenType, type_info: *ast.TypeInfo, constant: bool) !*Variable {
        if (self.name_map.contains(name)) {
            return error.DuplicateVariableName;
        }

        const storage_id = self.manager.next_storage_id;
        const variableId = self.manager.variable_counter;
        self.manager.next_storage_id += 1;
        self.manager.variable_counter += 1;

        const storage = try self.arena.allocator().create(ValueStorage);
        storage.* = .{ .value = value, .type = vtype, .type_info = type_info, .constant = constant };

        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variableId,
            .is_alias = false,
        };

        try self.manager.variable_map.put(variableId, variable);
        try self.manager.value_storage.put(storage_id, storage);
        try self.variables.put(variableId, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createAlias(self: *Scope, name: []const u8, target_variable: *Variable) !*Variable {
        const variableId = self.manager.variable_counter;
        self.manager.variable_counter += 1;

        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = target_variable.type,
            .storage_id = target_variable.storage_id,
            .id = variableId,
            .is_alias = true,
        };

        try self.manager.variable_map.put(variableId, variable);
        try self.variables.put(variableId, variable);
        try self.name_map.put(name, variable);

        return variable;
    }

    pub fn createAliasFromStorageId(self: *Scope, name: []const u8, storage_id: u32, vtype: TokenType, type_info: *TypeInfo) !*Variable {
        _ = type_info;
        const variableId = self.manager.variable_counter;
        self.manager.variable_counter += 1;

        const variable = try self.arena.allocator().create(Variable);
        variable.* = .{
            .name = name,
            .type = vtype,
            .storage_id = storage_id,
            .id = variableId,
            .is_alias = true,
        };

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

    pub fn createCustomTypeInstance(self: *Scope, instance_name: []const u8, instance_data: CustomTypeInstanceData) !void {
        const name_copy = try self.arena.allocator().dupe(u8, instance_name);
        try self.custom_type_instances.put(name_copy, instance_data);
    }
};
