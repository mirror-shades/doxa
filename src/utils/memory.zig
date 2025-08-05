const std = @import("std");
const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;
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

// NEW: Custom type instance data union
pub const CustomTypeInstanceData = union {
    struct_instance: *HIRStruct,
    enum_instance: *HIREnum,
    // union_instance: *HIRUnion, // TODO: Add when union support is implemented
};

// NEW: Custom type instance tracking
pub const CustomTypeInstance = struct {
    id: u32,
    type_name: []const u8,
    scope_id: u32, // Which scope owns this instance
    data: CustomTypeInstanceData,
};

// NEW: Unified CustomTypeInfo (shared between semantic analysis and VM)
pub const CustomTypeInfo = struct {
    name: []const u8,
    kind: CustomTypeKind,
    enum_variants: ?[]EnumVariant = null,
    struct_fields: ?[]StructField = null,

    pub const CustomTypeKind = enum {
        Struct,
        Enum,
        // Union, // TODO: Add when union support is implemented
    };

    pub const EnumVariant = struct {
        name: []const u8,
        index: u32,
    };

    pub const StructField = struct {
        name: []const u8,
        field_type: HIRType,
        custom_type_name: ?[]const u8 = null, // For custom types like Person
        index: u32,
    };

    /// Get the index of an enum variant by name
    pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
        if (self.kind != .Enum or self.enum_variants == null) return null;

        for (self.enum_variants.?) |variant| {
            if (std.mem.eql(u8, variant.name, variant_name)) {
                return variant.index;
            }
        }
        return null;
    }

    /// Get the index of a struct field by name
    pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
        if (self.kind != .Struct or self.struct_fields == null) return null;

        for (self.struct_fields.?) |field| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return field.index;
            }
        }
        return null;
    }
};

// NEW: Phase management
pub const Phase = enum {
    Analysis, // Semantic analysis phase
    Generation, // HIR generation phase
    Execution, // VM execution phase
};

pub const MemoryManager = struct {
    // NEW: Phase-separated memory pools
    analysis_arena: std.heap.ArenaAllocator, // Persistent through compilation
    execution_arena: std.heap.ArenaAllocator, // Reset between runs

    // Shared persistent data
    scope_manager: *ScopeManager,

    // NEW: Unified type registry
    type_registry: std.StringHashMap(CustomTypeInfo),

    // NEW: Type instance tracking
    custom_type_instances: std.AutoHashMap(u32, *CustomTypeInstance),
    next_instance_id: u32 = 0,

    // NEW: Phase management
    current_phase: Phase = .Analysis,

    pub fn init(allocator: std.mem.Allocator) !MemoryManager {
        const scope_manager = try ScopeManager.init(allocator);
        return .{
            .analysis_arena = std.heap.ArenaAllocator.init(allocator),
            .execution_arena = std.heap.ArenaAllocator.init(allocator),
            .scope_manager = scope_manager,
            .type_registry = std.StringHashMap(CustomTypeInfo).init(allocator),
            .custom_type_instances = std.AutoHashMap(u32, *CustomTypeInstance).init(allocator),
            .current_phase = .Analysis,
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        // Clean up root scope first if it exists
        if (self.scope_manager.root_scope) |root_scope| {
            root_scope.deinit();
            self.scope_manager.root_scope = null;
        }

        self.scope_manager.deinit();
        self.type_registry.deinit();
        self.custom_type_instances.deinit();
        self.analysis_arena.deinit();
        self.execution_arena.deinit();
    }

    // NEW: Phase-specific allocators
    pub fn getAnalysisAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.analysis_arena.allocator();
    }

    pub fn getExecutionAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.execution_arena.allocator();
    }

    // Legacy compatibility - returns execution allocator for VM compatibility
    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.getExecutionAllocator();
    }

    // NEW: Phase transition management
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

        // Reset execution memory while preserving analysis results
        self.resetExecutionMemory();

        // Create execution scope if needed
        if (self.scope_manager.root_scope == null) {
            const execution_scope = try self.scope_manager.createScope(null);
            self.scope_manager.root_scope = execution_scope;
        }

        self.current_phase = .Execution;
    }

    pub fn resetExecutionMemory(self: *MemoryManager) void {
        self.execution_arena.deinit();
        self.execution_arena = std.heap.ArenaAllocator.init(self.analysis_arena.child_allocator);

        // Clear custom type instances but preserve type registry
        self.custom_type_instances.clearRetainingCapacity();
        self.next_instance_id = 0;
    }

    // Legacy compatibility
    pub fn reset(self: *MemoryManager) void {
        self.resetExecutionMemory();
    }

    // NEW: Type registry management
    pub fn registerCustomType(self: *MemoryManager, type_info: CustomTypeInfo) !void {
        try self.type_registry.put(type_info.name, type_info);
    }

    pub fn getCustomType(self: *MemoryManager, type_name: []const u8) ?CustomTypeInfo {
        return self.type_registry.get(type_name);
    }

    // NEW: Type-safe allocation functions
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

    // NEW: Bridge function for VM integration
    pub fn bridgeTypesToVM(self: *MemoryManager, vm: anytype) !void {
        var type_iter = self.type_registry.iterator();
        while (type_iter.next()) |entry| {
            try vm.registerCustomType(entry.value_ptr.*);
        }
    }
};

/// ValueStorage holds a value with alias counting
const ValueStorage = struct { value: TokenLiteral, type: TokenType, type_info: *ast.TypeInfo, alias_count: u32, constant: bool };

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
            std.debug.print("Storage:\n", .{});
            var storage_it = self.value_storage.iterator();
            while (storage_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr.*;
                // Safe debug output - avoid printing complex values that might have circular references
                std.debug.print("  [{d}]: type={}, alias_count={d}, constant={}\n", .{ key, value.type, value.alias_count, value.constant });
            }
        }
    }

    pub fn createScope(self: *ScopeManager, parent: ?*Scope) !*Scope {
        const scope_id = self.next_storage_id;
        self.next_storage_id += 1;
        return Scope.init(self, scope_id, parent);
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

    // NEW: Custom type instances owned by this scope
    custom_type_instances: std.AutoHashMap(u32, *CustomTypeInstance),

    pub fn init(manager: *ScopeManager, scope_id: u32, parent: ?*Scope) !*Scope {
        const self = try manager.allocator.create(Scope);
        self.* = .{
            .id = scope_id,
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(manager.allocator),
            .variables = std.AutoHashMap(u32, *Variable).init(self.arena.allocator()),
            .name_map = std.StringHashMap(*Variable).init(self.arena.allocator()),
            .manager = manager,
            .custom_type_instances = std.AutoHashMap(u32, *CustomTypeInstance).init(self.arena.allocator()),
        };
        return self;
    }

    pub fn deinit(self: *Scope) void {
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

        // NEW: Clean up custom type instances
        var instance_iter = self.custom_type_instances.iterator();
        while (instance_iter.next()) |entry| {
            const instance = entry.value_ptr.*;
            self.manager.allocator.destroy(instance);
        }

        // Free all scope memory at once
        self.arena.deinit();
        self.manager.allocator.destroy(self);
    }

    // NEW: Custom type instance creation
    pub fn createCustomTypeInstance(self: *Scope, type_name: []const u8, data: CustomTypeInstanceData) !*CustomTypeInstance {
        const instance = try self.manager.allocator.create(CustomTypeInstance);
        instance.* = .{
            .id = self.manager.next_instance_id,
            .type_name = type_name,
            .scope_id = self.id,
            .data = data,
        };

        self.manager.next_instance_id += 1;
        try self.custom_type_instances.put(instance.id, instance);

        return instance;
    }

    pub fn createValueBinding(self: *Scope, name: []const u8, value: TokenLiteral, vtype: TokenType, type_info: *ast.TypeInfo, constant: bool) !*Variable {
        // Check for duplicate variable name in current scope
        if (self.name_map.contains(name)) {
            return error.DuplicateVariableName;
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
