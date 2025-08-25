const std = @import("std");
const ast = @import("../ast/ast.zig");
const FunctionParam = ast.FunctionParam;
const TokenImport = @import("./token.zig");
const TokenType = TokenImport.TokenType;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Scope = MemoryImport.Scope;
const Reporting = @import("../utils/reporting.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

// Forward declaration for circular reference
pub const ModuleEnvironment = struct {
    module_name: []const u8,
    environment: *Environment,
    imports: std.StringHashMap([]const u8), // alias -> module_path

    pub fn init(allocator: std.mem.Allocator, module_name: []const u8, memory_manager: *MemoryManager, debug_enabled: bool) !*ModuleEnvironment {
        const self = try allocator.create(ModuleEnvironment);
        const env = try allocator.create(Environment);
        env.* = .{
            .values = std.StringHashMap(TokenLiteral).init(allocator),
            .types = std.StringHashMap(ast.TypeInfo).init(allocator),
            .enclosing = null, // Module environments don't have enclosing by default
            .debug_enabled = debug_enabled,
            .allocator = allocator,
            .memory_manager = memory_manager,
            .module = self, // Circular reference
        };

        self.* = .{
            .module_name = module_name,
            .environment = env,
            .imports = std.StringHashMap([]const u8).init(allocator),
        };

        return self;
    }

    pub fn deinit(self: *ModuleEnvironment, allocator: std.mem.Allocator) void {
        self.environment.deinit();
        allocator.destroy(self.environment);
        self.imports.deinit();
        allocator.destroy(self);
    }

    pub fn addImport(self: *ModuleEnvironment, alias: []const u8, module_path: []const u8) !void {
        try self.imports.put(alias, module_path);
    }
};

pub const Environment = struct {
    values: std.StringHashMap(TokenLiteral),
    types: std.StringHashMap(ast.TypeInfo),
    enclosing: ?*Environment,
    debug_enabled: bool,
    allocator: std.mem.Allocator,
    memory_manager: *MemoryManager,
    module: ?*ModuleEnvironment = null, // Reference to owning module if this is a module environment

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
        if (self.memory_manager.scope_manager.root_scope) |root_scope| {
            // Use the mutability information from type_info to determine if this is constant
            const is_constant = !type_info.is_mutable;

            // Convert TypeInfo to TokenType if needed
            const token_type = switch (type_info.base) {
                .Int => TokenType.INT,
                .Byte => TokenType.BYTE,
                .Float => TokenType.FLOAT,
                .String => TokenType.STRING,
                .Array => TokenType.ARRAY,
                .Function => TokenType.FUNCTION,
                .Struct => TokenType.STRUCT,
                .Enum => TokenType.ENUM,
                .Map => TokenType.MAP,
                .Nothing => TokenType.NOTHING,
                .Custom => TokenType.ENUM_TYPE,
                .Tetra => TokenType.TETRA,
                .Union => TokenType.UNION,
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

                    // Update the value in storage
                    storage.value = value;

                    return;
                } else {
                    return error.StorageNotFound;
                }
            }
        }

        return error.UndefinedVariable;
    }

    pub fn getTypeInfo(self: *Environment, name: []const u8) ErrorList!ast.TypeInfo {
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
};

pub const StructField = struct {
    name: []const u8,
    value: TokenLiteral,
};

pub const Tetra = enum {
    true,
    false,
    both,
    neither,
};

pub const TokenLiteral = union(enum) {
    int: i32,
    byte: u8,
    float: f64,
    string: []const u8,
    tetra: Tetra,
    nothing: void,
    array: []TokenLiteral,
    struct_value: struct {
        type_name: []const u8,
        fields: []StructField,
        path: ?[]const u8 = @as(?[]const u8, null),
    },
    map: std.StringHashMap(TokenLiteral),
    function: struct {
        params: []FunctionParam,
        body: []ast.Stmt,
        closure: *Environment,
        defining_module: ?*ModuleEnvironment, // Reference to the module where this function was defined
    },
    enum_variant: []const u8,
};
