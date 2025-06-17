const std = @import("std");
const TokenImport = @import("../lexer/token.zig");
const Token = TokenImport.Token;
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const Environment = TypesImport.Environment;
const TokenType = TokenImport.TokenType;
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Scope = MemoryImport.Scope;
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;

pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment, debug_enabled: bool, memory_manager: *MemoryManager) Environment {
    // If we have an enclosing environment, its scope should be the parent of our new scope
    var parent_scope: ?*Scope = null;
    if (enclosing != null) {
        parent_scope = memory_manager.scope_manager.root_scope;
    }
    return .{
        .values = std.StringHashMap(TokenLiteral).init(allocator),
        .types = std.StringHashMap(ast.TypeInfo).init(allocator),
        .enclosing = enclosing,
        .debug_enabled = debug_enabled,
        .allocator = allocator,
        .memory_manager = memory_manager,
    };
}

pub fn deinit(self: *Environment) void {
    var it = self.values.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.*) {
            .array => |arr| self.allocator.free(arr),
            .function => |f| {
                self.allocator.free(f.params);
            },
            else => {},
        }
    }
    self.values.deinit();
    self.types.deinit();
}

pub fn define(self: *Environment, key: []const u8, value: TokenLiteral, type_info: ast.TypeInfo) !void {
    if (self.debug_enabled) {
        std.debug.print("Attempting to define '{s}' = {any} in the memory manager\n", .{ key, value });
    }

    if (self.memory_manager.scope_manager.root_scope) |root_scope| {
        // Use createValueBinding instead of the undefined defineVariable
        // We need to supply constant flag - let's assume false initially, could be a parameter
        const is_constant = false;

        // Convert TypeInfo to TokenType if needed
        // This is a simplification - you may need to map between your TypeInfo and TokenType
        const token_type = switch (type_info.base) {
            .Int => TokenType.INT,
            .U8 => TokenType.U8,
            .Float => TokenType.FLOAT,
            .String => TokenType.STRING,
            .Array => TokenType.ARRAY,
            .Function => TokenType.FUNCTION,
            .Struct => TokenType.STRUCT,
            .Enum => TokenType.ENUM,
            .Map => TokenType.MAP,
            .Nothing => TokenType.NOTHING,
            .Auto => TokenType.AUTO,
            .Tuple => TokenType.TUPLE,
            .Custom => TokenType.ENUM_TYPE,
            .Tetra => TokenType.TETRA,
            else => unreachable,
        };

        // Create value binding
        _ = try root_scope.createValueBinding(key, value, token_type, type_info, is_constant);
        return;
    }
    return error.NoRootScope;
}

pub fn get(self: *Environment, name: []const u8) ErrorList!?TokenLiteral {
    if (self.memory_manager.scope_manager.root_scope) |root_scope| {
        if (root_scope.lookupVariable(name)) |variable| {
            if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                return storage.value;
            }
        }
    }

    // If not found in current environment, check enclosing if it exists
    if (self.enclosing) |enclosing| {
        return enclosing.get(name);
    }

    return null;
}

pub fn assign(self: *Environment, name: []const u8, value: TokenLiteral) !void {
    if (self.debug_enabled) {
        std.debug.print("Attempting to assign '{s}' = {any}\n", .{ name, value });
    }

    // Look up variable from root scope
    if (self.memory_manager.scope_manager.root_scope) |root_scope| {
        // Use lookupVariable to find the variable in any accessible scope
        if (root_scope.lookupVariable(name)) |variable| {
            // Get the storage location for this variable
            if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                // Check if the variable is constant
                if (storage.constant) {
                    return error.CannotAssignToConstant;
                }

                // Debug: show old value
                if (self.debug_enabled) {
                    std.debug.print("Updating variable '{s}' from {any} to {any}\n", .{ name, storage.value, value });
                }

                // Update the value in storage
                storage.value = value;

                // Verify update
                if (self.debug_enabled) {
                    std.debug.print("After update, storage value is: {any}\n", .{storage.value});
                }

                return;
            } else {
                return error.StorageNotFound;
            }
        }
    }

    return error.UndefinedVariable;
}

pub fn getTypeInfo(self: *Environment, name: []const u8) ErrorList!TypeInfo {
    if (self.memory_manager.scope_manager.root_scope) |root_scope| {
        if (root_scope.lookupVariable(name)) |variable| {
            if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                return storage.type_info;
            }
        }
    }
    if (self.enclosing) |enclosing| {
        return enclosing.getTypeInfo(name);
    }
    return error.UndefinedVariable;
}
