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
    // ... existing fields ...
    /// Cleans up corrupted variable entries in the scope by removing variables with non-printable names.
    pub fn cleanCorruptedEntries(self: *Scope, allocator: std.mem.Allocator) void {
        if (self.variable_count > 0) {
            var needs_cleanup = false;
            for (self.variables[0..self.variable_count]) |variable| {
                for (variable.name) |char| {
                    if (char < 32 or char > 126) {
                        needs_cleanup = true;
                        break;
                    }
                }
                if (needs_cleanup) break;
            }
            if (needs_cleanup) {
                self.name_indices.clearRetainingCapacity();
                var valid_count: u32 = 0;
                for (self.variables[0..self.variable_count], 0..) |variable, i| {
                    var is_valid = true;
                    for (variable.name) |char| {
                        if (char < 32 or char > 126) {
                            is_valid = false;
                            break;
                        }
                    }
                    if (is_valid) {
                        if (valid_count != i) {
                            self.variables[valid_count] = variable;
                        }
                        self.name_indices.put(allocator, variable.name, valid_count) catch {};
                        valid_count += 1;
                    }
                }
                self.variable_count = valid_count;
            }
        }
    }
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
        // Check for corrupted entries and clean them up if needed
        if (self.variable_count > 0) {
            var needs_cleanup = false;
            for (self.variables[0..self.variable_count]) |variable| {
                // Check if variable name contains non-printable characters (corruption indicator)
                for (variable.name) |char| {
                    if (char < 32 or char > 126) {
                        needs_cleanup = true;
                        break;
                    }
                }
                if (needs_cleanup) break;
            }

            if (needs_cleanup) {
                // Clear the hash map and rebuild it with only valid entries
                self.name_indices.clearRetainingCapacity();
                var valid_count: u32 = 0;

                for (self.variables[0..self.variable_count], 0..) |variable, i| {
                    var is_valid = true;
                    for (variable.name) |char| {
                        if (char < 32 or char > 126) {
                            is_valid = false;
                            break;
                        }
                    }

                    if (is_valid) {
                        // Move valid variable to the front and update hash map
                        if (valid_count != i) {
                            self.variables[valid_count] = variable;
                        }
                        try self.name_indices.put(allocator, variable.name, valid_count);
                        valid_count += 1;
                    }
                }

                self.variable_count = valid_count;
            }
        }

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
    /// Cleans all scopes (including root) of corrupted variable entries.
    pub fn cleanAllScopes(self: *MemoryManager) void {
        for (self.scopes.items) |scope| {
            scope.cleanCorruptedEntries(self.arena.allocator());
        }
        if (self.is_debug) {
            std.debug.print("[MemoryManager] Cleaned all scopes of corrupted variable entries.\n", .{});
        }
    }
    arena: std.heap.ArenaAllocator,
    base_allocator: std.mem.Allocator,
    scopes: std.ArrayListUnmanaged(*Scope),
    root_scope: ?*Scope,
    current_scope: ?*Scope, // Tracks active scope
    is_debug: bool,
    string_interner: StringInterner,

    pub fn init(allocator: std.mem.Allocator, is_debug: bool) MemoryManager {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .base_allocator = allocator,
            .scopes = .{},
            .root_scope = null,
            .current_scope = null,
            .is_debug = is_debug,
            .string_interner = StringInterner.init(allocator),
        };
    }

    pub fn deinit(self: *MemoryManager) void {
        if (self.is_debug) {
            std.debug.print("Cleaning up memory manager with {} scopes\n", .{self.scopes.items.len});
        }

        // Clean up scopes first
        self.scopes.deinit(self.base_allocator);

        // Then clean up the string interner
        self.string_interner.deinit();

        // Finally, clean up the arena
        self.arena.deinit();
    }

    /// Intern a string to ensure consistent memory usage and comparison by pointer
    pub fn internString(self: *MemoryManager, str: []const u8) ![]const u8 {
        return self.string_interner.intern(str);
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

        // Reset arena and string interner
        self.arena.deinit();
        self.string_interner.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.base_allocator);
        self.string_interner = StringInterner.init(self.base_allocator);
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

        // First, make a copy of the name in the arena
        const name_copy = try self.arena.allocator().dupe(u8, name);

        // Then intern the copied string
        const interned_name = try self.internString(name_copy);

        const index = try scope.createVariable(
            self.arena.allocator(),
            interned_name, // Use the interned string here
            value,
            vtype,
            type_info,
            is_constant,
            location,
        );
        return &scope.variables[index];
    }

    pub fn createVariableInScope(
        self: *MemoryManager,
        scope: *Scope,
        name: []const u8,
        value: TokenLiteral,
        vtype: TokenType,
        type_info: TypeInfo,
        is_constant: bool,
        location: SourceLocation,
    ) !*Variable {
        // First, make a copy of the name in the arena
        const name_copy = try self.arena.allocator().dupe(u8, name);

        // Then intern the copied string
        const interned_name = try self.internString(name_copy);

        const index = try scope.createVariable(
            self.arena.allocator(),
            interned_name, // Use the interned string here
            value,
            vtype,
            type_info,
            is_constant,
            location,
        );

        return &scope.variables[index];
    }

    pub fn lookupVariable(self: *MemoryManager, scope: *Scope, name: []const u8) ?*Variable {
        // First, try to find the string in the interner
        if (self.string_interner.strings.get(name)) |interned_name| {
            // If found, use the interned string for the lookup
            return scope.lookupVariable(interned_name);
        }
        // If not found in interner, try direct lookup (for backward compatibility)
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
        if (self.is_debug) {
            std.debug.print("DEBUG: Pushed new scope as current (total scopes: {})\n", .{self.scopes.items.len});
        }
        return scope;
    }

    pub fn popScope(self: *MemoryManager) void {
        if (self.current_scope) |scope| {
            self.current_scope = scope.parent;
            if (self.is_debug) {
                std.debug.print("DEBUG: Popped scope, current scope now has {} variables\n", .{if (self.current_scope) |cs| cs.variable_count else 0});
            }
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
