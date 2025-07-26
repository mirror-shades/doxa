const std = @import("std");
const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;
const TypeInfo = @import("../ast/ast.zig").TypeInfo;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;

pub const SourceLocation = struct {
    file: []const u8,
    line: u32,
};

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

pub const Variable = struct {
    name: []const u8,
    value: TokenLiteral,
    vtype: TokenType,
    type_info: TypeInfo,
    is_constant: bool,
    location: SourceLocation, // New field for error context
};

pub const Scope = struct {
    parent: ?*Scope,
    variables: []Variable,
    name_indices: std.StringHashMapUnmanaged(u32),
    variable_count: u32,
    capacity: u32,

    const INITIAL_CAPACITY = 8;

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) !*Scope {
        const scope = try allocator.create(Scope);
        const initial_vars = try allocator.alloc(Variable, INITIAL_CAPACITY);

        scope.* = .{
            .parent = parent,
            .variables = initial_vars,
            .name_indices = .{},
            .variable_count = 0,
            .capacity = INITIAL_CAPACITY,
        };

        return scope;
    }

    pub fn createVariable(
        self: *Scope,
        allocator: std.mem.Allocator,
        name: []const u8,
        value: TokenLiteral,
        vtype: TokenType,
        type_info: TypeInfo,
        is_constant: bool,
        location: SourceLocation,
    ) !u32 {
        // Check for duplicate in current scope only
        if (self.name_indices.get(name) != null) {
            return error.DuplicateVariableName;
        }

        // Grow if needed
        if (self.variable_count >= self.capacity) {
            const new_capacity = self.capacity * 2;
            self.variables = try allocator.realloc(self.variables, new_capacity);
            self.capacity = new_capacity;
        }

        const index = self.variable_count;
        self.variables[index] = .{
            .name = name,
            .value = value,
            .vtype = vtype,
            .type_info = type_info,
            .is_constant = is_constant,
            .location = location,
        };

        try self.name_indices.put(allocator, name, index);
        self.variable_count += 1;

        return index;
    }

    pub fn lookupVariable(self: *Scope, name: []const u8) ?*Variable {
        var current: ?*Scope = self;
        while (current) |scope| {
            if (scope.name_indices.get(name)) |index| {
                return &scope.variables[index];
            }
            current = scope.parent;
        }
        return null;
    }

    pub fn getVariable(self: *Scope, index: u32) ?*Variable {
        if (index >= self.variable_count) return null;
        return &self.variables[index];
    }
};

pub const MemoryManager = struct {
    arena: std.heap.ArenaAllocator,
    base_allocator: std.mem.Allocator,
    scopes: std.ArrayListUnmanaged(*Scope),
    root_scope: ?*Scope,
    current_scope: ?*Scope, // Tracks active scope
    is_debug: bool,

    pub fn init(allocator: std.mem.Allocator, is_debug: bool) MemoryManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .base_allocator = allocator,
            .scopes = .{},
            .root_scope = null,
            .current_scope = null,
            .is_debug = is_debug,
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.is_debug) {
            std.debug.print("Cleaning up memory manager with {} scopes\n", .{self.scopes.items.len});
        }

        // Note: name_indices hashmaps are arena-allocated, so they don't need manual cleanup
        // The arena cleanup will handle all Variable data, Scope structs, and hashmap storage

        self.scopes.deinit(self.base_allocator);

        // Arena cleanup handles all Variable data and Scope structs
        self.arena.deinit();
    }

    pub fn getAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn reset(self: *MemoryManager) void {
        if (self.is_debug) {
            std.debug.print("Resetting memory manager...\n", .{});
        }

        // Note: name_indices hashmaps are arena-allocated, so they don't need manual cleanup
        // The arena reset will handle all Variable data, Scope structs, and hashmap storage

        self.scopes.clearRetainingCapacity();
        self.root_scope = null;

        // Reset arena - this frees all scopes and variables at once
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.base_allocator);
    }

    // Public API - Simple and focused
    pub fn createScope(self: *MemoryManager, parent: ?*Scope) !*Scope {
        const scope = try Scope.init(self.arena.allocator(), parent);
        try self.scopes.append(self.base_allocator, scope);

        if (parent == null) {
            self.root_scope = scope;
        }

        return scope;
    }

    pub fn createVariable(
        self: *MemoryManager,
        name: []const u8,
        value: TokenLiteral,
        vtype: TokenType,
        type_info: TypeInfo,
        is_constant: bool,
        location: SourceLocation,
    ) !*Variable {
        const scope = self.current_scope orelse return error.NoActiveScope;
        const index = try scope.createVariable(
            self.arena.allocator(),
            name,
            value,
            vtype,
            type_info,
            is_constant,
            location,
        );
        return &scope.variables[index];
    }

    pub fn lookupVariable(self: *MemoryManager, scope: *Scope, name: []const u8) ?*Variable {
        _ = self; // unused
        return scope.lookupVariable(name);
    }

    pub fn setVariableValue(self: *MemoryManager, variable: *Variable, value: TokenLiteral) !void {
        _ = self; // unused
        if (variable.is_constant) {
            return error.CannotModifyConstant;
        }
        variable.value = value;
    }

    pub fn getRootScope(self: *MemoryManager) ?*Scope {
        return self.root_scope;
    }

    // Convenience methods for common patterns
    pub fn pushScope(self: *MemoryManager, parent: ?*Scope) !*Scope {
        const scope = try self.createScope(parent);
        self.current_scope = scope;
        return scope;
    }

    pub fn popScope(self: *MemoryManager) void {
        if (self.current_scope) |scope| {
            self.current_scope = scope.parent;
        }
    }

    pub fn withScope(self: *MemoryManager, parent: ?*Scope, comptime func: anytype, args: anytype) !@TypeOf(func(args, @as(*Scope, undefined))) {
        const scope = try self.pushScope(parent);
        defer self.popScope();
        return func(args, scope);
    }

    pub fn dumpState(self: *MemoryManager) void {
        std.debug.print("Memory Manager State:\n", .{});
        std.debug.print("  Total scopes: {}\n", .{self.scopes.items.len});

        if (self.is_debug) {
            var total_vars: u32 = 0;
            for (self.scopes.items, 0..) |scope, i| {
                std.debug.print("  Scope {}: {} variables\n", .{ i, scope.variable_count });
                total_vars += scope.variable_count;
            }
            std.debug.print("  Total variables: {}\n", .{total_vars});
        }
    }

    // Debug helper
    pub fn dumpScope(self: *MemoryManager, scope: *Scope, depth: u32) void {
        _ = self;
        const indent = "  " ** depth;
        std.debug.print("{}Scope with {} variables:\n", .{ indent, scope.variable_count });

        for (scope.variables[0..scope.variable_count]) |variable| {
            std.debug.print("{}  {s}: {} ({})\n", .{ indent, variable.name, variable.value, @tagName(variable.vtype) });
        }
    }
};
