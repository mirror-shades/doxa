const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const ReportingModule = @import("reporting.zig");
const ErrorList = ReportingModule.ErrorList;
const MemoryManager = @import("memory.zig").MemoryManager;
const TypeInfo = ast.TypeInfo;

const StringInterner = struct {
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

pub const Environment = struct {
    values: std.StringHashMap(token.TokenLiteral),
    types: std.StringHashMap(ast.TypeInfo),
    enclosing: ?*Environment,
    debug_enabled: bool,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment, debug_enabled: bool) Environment {
        return .{
            .values = std.StringHashMap(token.TokenLiteral).init(allocator),
            .types = std.StringHashMap(ast.TypeInfo).init(allocator),
            .enclosing = enclosing,
            .debug_enabled = debug_enabled,
            .allocator = allocator,
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

    pub fn define(self: *Environment, name: []const u8, value: token.TokenLiteral, type_info: ast.TypeInfo) !void {
        // Duplicate the name string to ensure we own it
        const key = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(key);

        // If we're replacing an existing value, free the old key
        if (self.values.getKey(name)) |old_key| {
            self.allocator.free(old_key);
        }

        try self.values.put(key, value);
        try self.types.put(key, type_info);

        if (self.debug_enabled) {
            std.debug.print("Defined variable '{s}' = {any}\n", .{ key, value });
        }
    }

    pub fn get(self: *Environment, name: []const u8) ?token.TokenLiteral {
        if (self.values.get(name)) |value| {
            return value;
        }
        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }
        return null;
    }

    pub fn assign(self: *Environment, name: []const u8, value: token.TokenLiteral) !void {
        if (self.debug_enabled) {
            std.debug.print("Attempting to assign '{s}' = {any}\n", .{ name, value });
        }

        // First check if the variable exists in current scope
        if (self.values.contains(name)) {
            // Free any existing array value before overwriting
            if (self.values.get(name)) |old_value| {
                if (old_value == .array) {
                    self.allocator.free(old_value.array);
                }
            }
            try self.values.put(name, value);
            return;
        }

        // If not in current scope but we have an enclosing scope, try there
        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }

        // If we get here, the variable doesn't exist in any scope
        return error.UndefinedVariable;
    }

    pub fn getTypeInfo(self: *Environment, name: []const u8) ErrorList!TypeInfo {
        if (self.types.get(name)) |type_info| {
            return type_info;
        }
        if (self.enclosing) |enclosing| {
            return enclosing.getTypeInfo(name);
        }
        return error.UndefinedVariable;
    }
};

pub const Interpreter = struct {
    memory: *MemoryManager,
    environment: *Environment,
    globals: *Environment,
    debug_enabled: bool,
    string_interner: StringInterner,
    stdout_buffer: std.ArrayList(u8),

    pub fn init(memory: *MemoryManager) !Interpreter {
        const globals = try memory.getAllocator().create(Environment);
        globals.* = Environment.init(memory.getAllocator(), null, memory.debug_enabled);

        return Interpreter{
            .memory = memory,
            .environment = globals,
            .globals = globals,
            .debug_enabled = memory.debug_enabled,
            .string_interner = StringInterner.init(memory.getAllocator()),
            .stdout_buffer = std.ArrayList(u8).init(memory.getAllocator()),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.string_interner.deinit();
        self.stdout_buffer.deinit();
    }

    pub fn executeBlock(self: *Interpreter, statements: []ast.Stmt, environment: *Environment) ErrorList!?token.TokenLiteral {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;

        // Handle empty blocks
        if (statements.len == 0) {
            return null;
        }

        // Execute all statements except the last one
        if (statements.len > 1) {
            for (statements[0 .. statements.len - 1]) |stmt| {
                _ = try self.executeStatement(&stmt, self.debug_enabled);
            }
        }

        // Handle the last statement
        const last = statements[statements.len - 1];
        if (last == .Expression) {
            return try self.executeStatement(&last, self.debug_enabled);
        }
        _ = try self.executeStatement(&last, self.debug_enabled);
        return null;
    }

    pub fn compare(a: anytype, b: anytype) i8 {
        var a_float: f32 = undefined;
        var b_float: f32 = undefined;
        if (@TypeOf(a) == i32 or @TypeOf(b) == i32) {
            a_float = @as(f32, @floatFromInt(a));
            b_float = @as(f32, @floatFromInt(b));
        } else if (@TypeOf(a) == f32 or @TypeOf(b) == f32) {
            a_float = a;
            b_float = b;
        } else {
            return error.InvalidType;
        }
        if (a_float < b_float) return -1;
        if (a_float > b_float) return 1;
        return 0;
    }

    pub fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt, debug: bool) ErrorList!?token.TokenLiteral {
        if (debug) {
            self.debug_enabled = true;
        }
        if (self.debug_enabled) {
            std.debug.print("Executing statement: {any}\n", .{stmt.*});
        }

        return switch (stmt.*) {
            .Expression => |expr| {
                if (self.debug_enabled) {
                    std.debug.print("Executing expression statement\n", .{});
                }

                if (expr) |e| {
                    const result = try self.evaluate(e);
                    if (self.debug_enabled) {
                        std.debug.print("Expression result: {any}\n", .{result});
                    }
                    return result;
                }
                return null;
            },
            .VarDecl => |decl| {
                // Handle array type declarations
                if (decl.type_info.base == .Array) {
                    // Store the array type for use during array initialization
                    const element_type = decl.type_info.element_type orelse .Dynamic;
                    try self.environment.types.put("current_array_type", .{ .base = element_type });
                    defer _ = self.environment.types.remove("current_array_type");
                }

                const value = if (decl.initializer) |i| blk: {
                    const init_value = try self.evaluate(i);

                    // Type checking for initialization
                    switch (decl.type_info.base) {
                        .Int => if (init_value != .int) {
                            std.debug.print("Type error: Cannot initialize int variable with {s}\n", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Float => if (init_value != .float and init_value != .int) {
                            std.debug.print("Type error: Cannot initialize float variable with {s}\n", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Boolean => if (init_value != .boolean) {
                            std.debug.print("Type error: Cannot initialize bool variable with {s}\n", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Array => {
                            if (init_value != .array) {
                                std.debug.print("Type error: Cannot initialize array variable with {s}\n", .{@tagName(init_value)});
                                return error.TypeError;
                            }
                            // Check array element types if element_type is specified
                            if (decl.type_info.element_type) |expected_type| {
                                for (init_value.array) |element| {
                                    const matches = switch (expected_type) {
                                        .Int => element == .int,
                                        .Float => element == .float,
                                        .String => element == .string,
                                        .Boolean => element == .boolean,
                                        else => true,
                                    };
                                    if (!matches) {
                                        return error.TypeError;
                                    }
                                }
                            }
                        },
                        .Dynamic => {},
                        else => {},
                    }
                    break :blk init_value;
                } else token.TokenLiteral{ .nothing = {} };

                try self.environment.define(decl.name.lexeme, value, decl.type_info);
                return null;
            },
            .Function => |func| {
                if (self.debug_enabled) {
                    std.debug.print("Defining function '{s}' with {} parameters\n", .{ func.name.lexeme, func.params.len });
                }

                // Make a copy of the parameters
                var params = try self.memory.getAllocator().alloc(ast.FunctionParam, func.params.len);
                errdefer {
                    self.memory.getAllocator().free(params);
                }

                // Deep copy each parameter
                for (func.params, 0..) |param, i| {
                    params[i] = .{
                        .name = param.name,
                        .type_info = param.type_info,
                    };
                    if (self.debug_enabled) {
                        std.debug.print("  Parameter {}: '{s}'\n", .{ i, param.name.lexeme });
                    }
                }

                const function = token.TokenLiteral{ .function = .{
                    .params = params,
                    .body = func.body,
                    .closure = self.environment,
                } };

                // Use return_type_info for function type info
                try self.environment.define(func.name.lexeme, function, func.return_type_info);
                return null;
            },
            .Block => |statements| {
                var block_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                defer block_env.deinit();
                const result = try self.executeBlock(statements, &block_env);
                if (self.debug_enabled and result != null) {
                    std.debug.print("{any}\n", .{result.?});
                }
                return result;
            },
            .Return => |ret| {
                if (ret.value) |value_expr| {
                    const return_value = try self.evaluate(value_expr);

                    // Get the function's return type from the current environment
                    const func_return_type = try self.environment.getTypeInfo("return_type");

                    // Type check the return value
                    switch (func_return_type.base) {
                        .Int => {
                            if (return_value != .int) {
                                return error.TypeError;
                            }
                        },
                        .Float => {
                            if (return_value != .float and return_value != .int) {
                                return error.TypeError;
                            }
                        },
                        .String => {
                            if (return_value != .string) {
                                return error.TypeError;
                            }
                        },
                        .Boolean => {
                            if (return_value != .boolean) {
                                return error.TypeError;
                            }
                        },
                        .Dynamic => {}, // Allow any return type
                        .Auto => {}, // Type is inferred from the return value
                        else => {},
                    }

                    try self.environment.define("return", return_value, func_return_type);
                    return error.ReturnValue;
                }
                return error.ReturnNothing;
            },
        };
    }

    pub fn evaluate(self: *Interpreter, expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        return switch (expr.*) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |binary| {
                const left = try self.evaluate(binary.left orelse return error.InvalidExpression);
                const right = try self.evaluate(binary.right orelse return error.InvalidExpression);

                return switch (binary.operator.type) {
                    .EQUALITY => switch (left) {
                        .int => token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) == 0 },
                        .float => token.TokenLiteral{ .boolean = left.float == right.float },
                        .boolean => token.TokenLiteral{ .boolean = left.boolean == right.boolean },
                        .string => token.TokenLiteral{ .boolean = std.mem.eql(u8, left.string, right.string) },
                        .nothing => token.TokenLiteral{ .boolean = right == .nothing },
                        .array => if (right == .array) blk: {
                            if (left.array.len != right.array.len) break :blk token.TokenLiteral{ .boolean = false };
                            for (left.array, right.array) |l, r| {
                                if (!std.meta.eql(l, r)) break :blk token.TokenLiteral{ .boolean = false };
                            }
                            break :blk token.TokenLiteral{ .boolean = true };
                        } else token.TokenLiteral{ .boolean = false },
                        .struct_value => if (right == .struct_value) blk: {
                            if (left.struct_value.fields.len != right.struct_value.fields.len) break :blk token.TokenLiteral{ .boolean = false };
                            for (left.struct_value.fields, right.struct_value.fields) |l, r| {
                                if (!std.mem.eql(u8, l.name, r.name)) break :blk token.TokenLiteral{ .boolean = false };
                                if (!self.valuesEqual(l.value, r.value)) break :blk token.TokenLiteral{ .boolean = false };
                            }
                            break :blk token.TokenLiteral{ .boolean = true };
                        } else token.TokenLiteral{ .boolean = false },
                        .function => token.TokenLiteral{ .boolean = false }, // Functions are never equal
                    },
                    .PLUS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int + right.int };
                        }
                        return error.TypeError;
                    },
                    .MINUS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int - right.int };
                        }
                        return error.TypeError;
                    },
                    .ASTERISK => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int * right.int };
                        }
                        return error.TypeError;
                    },
                    .SLASH => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return token.TokenLiteral{ .int = @divTrunc(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    .GREATER => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) > 0 };
                        }
                        if (left == .float and right == .int) {
                            return token.TokenLiteral{ .boolean = left.float > @as(f32, @floatFromInt(right.int)) };
                        }
                        if (left == .int and right == .float) {
                            return token.TokenLiteral{ .boolean = @as(f32, @floatFromInt(left.int)) > right.float };
                        }
                        if (left == .float and right == .float) {
                            return token.TokenLiteral{ .boolean = left.float > right.float };
                        }
                        return error.TypeError;
                    },
                    .GREATER_EQUAL => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) >= 0 };
                        }
                        return error.TypeError;
                    },
                    .LESS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) < 0 };
                        }
                        return error.TypeError;
                    },
                    .LESS_EQUAL => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) <= 0 };
                        }
                        return error.TypeError;
                    },
                    .BANG_EQUAL => {
                        return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) != 0 };
                    },
                    .MODULO => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return token.TokenLiteral{ .int = @mod(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    else => return error.InvalidOperator,
                };
            },
            .Unary => |un| {
                const operand = try self.evaluate(un.right orelse return error.InvalidExpression);
                switch (un.operator.type) {
                    .MINUS => {
                        if (operand == .int) {
                            return token.TokenLiteral{ .int = -operand.int };
                        }
                        return error.TypeError;
                    },
                    .BANG => {
                        if (operand == .boolean) {
                            return token.TokenLiteral{ .boolean = !operand.boolean };
                        }
                        return error.TypeError;
                    },
                    .PLUS => {
                        if (operand == .int) {
                            return token.TokenLiteral{ .int = operand.int };
                        }
                        if (operand == .float) {
                            return token.TokenLiteral{ .float = operand.float };
                        }
                        return error.InvalidOperator;
                    },
                    else => return error.InvalidOperator,
                }
            },
            .Variable => |var_token| {
                if (self.debug_enabled) {
                    std.debug.print("Looking up variable: '{s}' (len: {})\n", .{ var_token.lexeme, var_token.lexeme.len });
                }

                // Check if variable exists
                if (self.environment.get(var_token.lexeme)) |value| {
                    return value;
                }

                // If we get here, variable wasn't found
                if (self.debug_enabled) {
                    std.debug.print("Variable not found: '{s}'\n", .{var_token.lexeme});
                }
                return error.VariableNotFound;
            },
            .Assignment => |assign| {
                const value = try self.evaluate(assign.value orelse return error.InvalidExpression);

                // Get variable's type info
                const var_type = try self.environment.getTypeInfo(assign.name.lexeme);

                // Type checking
                switch (var_type.base) {
                    .Int => if (value != .int) {
                        std.debug.print("Type error: Cannot assign {s} to int variable\n", .{@tagName(value)});
                        return error.TypeError;
                    },
                    .Float => if (value != .float) {
                        std.debug.print("Type error: Cannot assign {s} to float variable\n", .{@tagName(value)});
                        return error.TypeError;
                    },
                    .Dynamic => {},
                    else => {},
                }

                try self.environment.assign(assign.name.lexeme, value);
                return value;
            },
            .Grouping => |group| {
                return try self.evaluate(group orelse return error.InvalidExpression);
            },
            .If => |if_expr| {
                const condition = try self.evaluate(if_expr.condition orelse return error.InvalidExpression);
                if (condition != .boolean) {
                    return error.TypeError;
                }

                const branch = if (condition.boolean)
                    if_expr.then_branch orelse return error.InvalidExpression
                else
                    if_expr.else_branch orelse return error.InvalidExpression;

                // Special handling for block expressions
                switch (branch.*) {
                    .Block => |block| {
                        var block_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                        defer block_env.deinit();

                        // If we have a value expression without statements, evaluate it directly
                        if (block.statements.len == 0 and block.value != null) {
                            return try self.evaluate(block.value.?);
                        }

                        // Execute the block and handle return values
                        const block_result = self.executeBlock(block.statements, &block_env) catch |err| {
                            if (err == error.ReturnValue) {
                                if (block_env.get("return")) |return_value| {
                                    if (self.debug_enabled) {
                                        std.debug.print("Block returned: {any}\n", .{return_value});
                                    }
                                    return return_value;
                                }
                            }
                            return err;
                        };

                        return if (block_result) |result|
                            result
                        else if (block.value) |value|
                            try self.evaluate(value)
                        else
                            .{ .nothing = {} };
                    },
                    else => return try self.evaluate(branch),
                }
            },
            .Block => |block| {
                if (self.debug_enabled) {
                    std.debug.print("Evaluating block with {} statements\n", .{block.statements.len});
                    if (block.value) |value| {
                        std.debug.print("Block has value expression: {any}\n", .{value});
                    }
                }

                var block_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                defer block_env.deinit();

                // If we have a value expression without statements, evaluate it directly
                if (block.statements.len == 0 and block.value != null) {
                    const result = try self.evaluate(block.value.?);
                    if (self.debug_enabled) {
                        std.debug.print("Block evaluated to: {any}\n", .{result});
                    }
                    return result;
                }

                // Otherwise execute statements and handle value
                _ = try self.executeBlock(block.statements, &block_env);
                const result = if (block.value) |value|
                    try self.evaluate(value)
                else
                    token.TokenLiteral{ .nothing = {} };

                if (self.debug_enabled) {
                    std.debug.print("Block evaluated to: {any}\n", .{result});
                }
                return result;
            },
            .Array => |elements| {
                var array_values = std.ArrayList(token.TokenLiteral).init(self.memory.getAllocator());
                errdefer array_values.deinit();

                // Get the array's type info if it exists
                const array_type = if (self.environment.types.get("current_array_type")) |type_info|
                    type_info
                else
                    ast.TypeInfo{ .base = .Dynamic };

                // Check each element against the array type
                for (elements) |element| {
                    const value = try self.evaluate(element);

                    // Type check if we have a specific array type
                    if (array_type.base != .Dynamic) {
                        const matches = switch (array_type.element_type orelse .Dynamic) {
                            .Int => value == .int,
                            .Float => value == .float,
                            .String => value == .string,
                            .Boolean => value == .boolean,
                            .Array => value == .array,
                            .Dynamic => true,
                            else => false,
                        };

                        if (!matches) {
                            array_values.deinit();
                            return error.TypeError;
                        }
                    }

                    try array_values.append(value);
                }

                const owned_slice = try array_values.toOwnedSlice();
                errdefer self.memory.getAllocator().free(owned_slice);

                return token.TokenLiteral{ .array = owned_slice };
            },
            .Struct => |fields| {
                var struct_fields = std.ArrayList(token.StructField).init(self.memory.getAllocator());
                errdefer struct_fields.deinit();

                for (fields) |field| {
                    const value = try self.evaluate(field.value);
                    try struct_fields.append(token.StructField{
                        .name = field.name.lexeme,
                        .value = value,
                    });
                }
                return token.TokenLiteral{ .struct_value = .{
                    .type_name = expr.StructLiteral.name.lexeme,
                    .fields = try struct_fields.toOwnedSlice(),
                } };
            },
            .Index => |idx| {
                const array = try self.evaluate(idx.array);
                const index = try self.evaluate(idx.index);
                if (index.int < 0) {
                    return error.IndexOutOfBounds;
                }
                const usize_index = @as(usize, @intCast(index.int));
                if (usize_index >= array.array.len) {
                    return error.IndexOutOfBounds;
                }
                return array.array[usize_index];
            },
            .IndexAssign => |idx_assign| {
                const array_val = try self.evaluate(idx_assign.array);
                const index = try self.evaluate(idx_assign.index);
                const new_value = try self.evaluate(idx_assign.value);

                if (array_val != .array) {
                    return error.TypeError;
                }
                if (index != .int) {
                    return error.TypeError;
                }

                const usize_index = @as(usize, @intCast(index.int));
                if (usize_index >= array_val.array.len) {
                    return error.IndexOutOfBounds;
                }

                // Modify the array in place
                array_val.array[usize_index] = new_value;
                return new_value;
            },
            .Call => |call| {
                const callee = try self.evaluate(call.callee);
                return self.callFunction(callee, call.arguments);
            },
            .Logical => |logical| {
                const left = try self.evaluate(logical.left);
                const right = try self.evaluate(logical.right);
                return token.TokenLiteral{
                    .boolean = switch (logical.operator.type) {
                        .AND_KEYWORD, .AND_SYMBOL => left.boolean and right.boolean,
                        .OR_KEYWORD, .OR_SYMBOL => left.boolean or right.boolean,
                        .XOR => {
                            // Both operands must be boolean
                            if (left != .boolean) return error.TypeError;
                            if (right != .boolean) return error.TypeError;

                            // XOR is true if operands are different
                            return token.TokenLiteral{ .boolean = left.boolean != right.boolean };
                        },
                        else => return error.InvalidOperator,
                    },
                };
            },
            .Function => |f| token.TokenLiteral{ .function = .{
                .params = f.params,
                .body = f.body,
                .closure = self.environment,
            } },
            .Print => |print| {
                const value = try self.evaluate(print.expr);
                var buffer = std.ArrayList(u8).init(self.memory.getAllocator());
                defer buffer.deinit();

                // Format the output into the buffer
                try buffer.writer().print("[{s}:{d}:{d}] {s} = ", .{ print.location.file, print.location.line, print.location.column, switch (print.expr.*) {
                    .Variable => |v| v.lexeme,
                    .Binary => "expression",
                    .Call => "function call",
                    else => "value",
                } });

                // Format the value into the buffer
                switch (value) {
                    .int => |i| try buffer.writer().print("{d}", .{i}),
                    .float => |f| try buffer.writer().print("{d}", .{f}),
                    .boolean => |b| try buffer.writer().print("{}", .{b}),
                    .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                    .nothing => try buffer.writer().print("nothing", .{}),
                    .array => |arr| {
                        try buffer.writer().print("[", .{});
                        for (arr, 0..) |item, i| {
                            if (i > 0) try buffer.writer().print(", ", .{});
                            try buffer.writer().print("{any}", .{item});
                        }
                        try buffer.writer().print("]", .{});
                    },
                    .function => try buffer.writer().print("<function>", .{}),
                    .struct_value => |sv| try buffer.writer().print("{any}", .{sv}),
                }
                try buffer.writer().print("\n", .{});

                // Write the entire buffer at once
                try std.io.getStdOut().writeAll(buffer.items);

                return value;
            },
            .While => |while_expr| {
                while (true) {
                    const condition_result = try self.evaluate(while_expr.condition);
                    if (condition_result != .boolean) {
                        return error.TypeError;
                    }
                    if (!condition_result.boolean) break;

                    _ = try self.evaluate(while_expr.body);
                }
                return token.TokenLiteral{ .nothing = {} };
            },
            .For => |for_expr| {
                // Execute initializer if present
                if (for_expr.initializer) |init_expr| {
                    _ = try self.executeStatement(init_expr, self.debug_enabled);
                }

                // Main loop
                while (true) {
                    // Check condition if present
                    if (for_expr.condition) |cond| {
                        const condition_result = try self.evaluate(cond);
                        if (condition_result != .boolean) {
                            return error.TypeError;
                        }
                        if (!condition_result.boolean) break;
                    }

                    // Execute body
                    _ = try self.evaluate(for_expr.body);

                    // Execute increment if present
                    if (for_expr.increment) |incr| {
                        _ = try self.evaluate(incr);
                    }
                }
                return token.TokenLiteral{ .nothing = {} };
            },
            .ForEach => |foreach| {
                const array_value = try self.evaluate(foreach.array);
                if (array_value != .array) {
                    return error.TypeError;
                }

                // Create a new environment for the loop
                var iter_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                defer iter_env.deinit();

                // Infer type from array elements or use dynamic if empty
                var item_type = ast.TypeInfo{ .base = .Dynamic };
                if (array_value.array.len > 0) {
                    item_type = switch (array_value.array[0]) {
                        .int => ast.TypeInfo{ .base = .Int },
                        .float => ast.TypeInfo{ .base = .Float },
                        .string => ast.TypeInfo{ .base = .String },
                        .boolean => ast.TypeInfo{ .base = .Boolean },
                        .array => ast.TypeInfo{ .base = .Array },
                        .struct_value => ast.TypeInfo{ .base = .Struct },
                        .nothing => ast.TypeInfo{ .base = .Nothing },
                        else => ast.TypeInfo{ .base = .Dynamic },
                    };
                }

                // Execute the loop body for each item
                for (array_value.array) |item| {
                    try iter_env.define(foreach.item_name.lexeme, item, item_type);

                    // Execute the loop body
                    _ = try self.executeBlock(foreach.body, &iter_env);
                }

                return token.TokenLiteral{ .nothing = {} };
            },
            .FieldAccess => |field| {
                const object = try self.evaluate(field.object);
                if (object != .struct_value) {
                    return error.NotAStruct;
                }

                // Look up field in struct
                for (object.struct_value.fields) |struct_field| {
                    if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                        return struct_field.value;
                    }
                }
                return error.FieldNotFound;
            },
            .StructDecl => |decl| {
                // Create struct type and store it in environment
                const struct_type = ast.TypeInfo{
                    .base = .Struct,
                    .struct_fields = try self.memory.getAllocator().alloc(ast.StructFieldType, decl.fields.len),
                };

                for (decl.fields, 0..) |field, i| {
                    struct_type.struct_fields.?[i] = .{
                        .name = field.name.lexeme,
                        .type_info = try ast.typeInfoFromExpr(self.memory.getAllocator(), field.type_expr),
                    };
                }

                try self.environment.define(decl.name.lexeme, .{ .nothing = {} }, struct_type);
                return .{ .nothing = {} };
            },
            .StructLiteral => |literal| {
                var struct_fields = std.ArrayList(token.StructField).init(self.memory.getAllocator());
                errdefer struct_fields.deinit();

                // Get the struct's type info from the environment
                const struct_type = try self.environment.getTypeInfo(literal.name.lexeme);
                if (struct_type.base != .Struct) {
                    return error.NotAStruct;
                }

                // Evaluate each field
                for (literal.fields) |field| {
                    const value = try self.evaluate(field.value);

                    // Type check the field value against the struct's field type
                    if (struct_type.struct_fields) |type_fields| {
                        for (type_fields) |type_field| {
                            if (std.mem.eql(u8, type_field.name, field.name.lexeme)) {
                                // Verify type matches
                                const matches = switch (type_field.type_info.base) {
                                    .Int => value == .int,
                                    .Float => value == .float,
                                    .String => value == .string,
                                    .Boolean => value == .boolean,
                                    .Array => value == .array,
                                    .Struct => value == .struct_value,
                                    else => true,
                                };
                                if (!matches) {
                                    return error.TypeError;
                                }
                                break;
                            }
                        }
                    }

                    try struct_fields.append(.{
                        .name = field.name.lexeme,
                        .value = value,
                    });
                }

                return token.TokenLiteral{ .struct_value = .{
                    .type_name = literal.name.lexeme,
                    .fields = try struct_fields.toOwnedSlice(),
                } };
            },
            .FieldAssignment => |field_assign| {
                const object = try self.evaluate(field_assign.object);
                const value = try self.evaluate(field_assign.value);

                if (self.debug_enabled) {
                    std.debug.print("Field assignment: {s} = {any}\n", .{
                        field_assign.field.lexeme,
                        value,
                    });
                }

                switch (object) {
                    .struct_value => |*struct_val| {
                        // Find and update the field
                        for (struct_val.fields) |*field| {
                            if (std.mem.eql(u8, field.name, field_assign.field.lexeme)) {
                                field.value = value;
                                return value;
                            }
                        }
                        return error.FieldNotFound;
                    },
                    else => return error.NotAStruct,
                }
            },
            .Exists => |e| {
                if (self.debug_enabled) {
                    std.debug.print("Evaluating exists expression\n", .{});
                    std.debug.print("Variable name: {s}\n", .{e.variable.lexeme});
                }

                // Store the current environment to restore it later
                const prev_env = self.environment;
                defer self.environment = prev_env;

                // Create a new environment for the quantifier scope
                var quantifier_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                self.environment = &quantifier_env;

                // Evaluate the array expression to get the actual array
                if (self.debug_enabled) {
                    std.debug.print("Evaluating array expression: {any}\n", .{e.array});
                }
                const array_value = try self.evaluate(e.array);
                if (self.debug_enabled) {
                    std.debug.print("Array value: {any}\n", .{array_value});
                }
                if (array_value != .array) {
                    if (self.debug_enabled) {
                        std.debug.print("TypeError: Expected array, got {s}\n", .{@tagName(array_value)});
                    }
                    return error.TypeError;
                }

                // Iterate over the actual array elements
                for (array_value.array) |val| {
                    if (self.debug_enabled) {
                        std.debug.print("Testing value: {any}\n", .{val});
                    }
                    try self.environment.define(e.variable.lexeme, val, .{ .base = .Dynamic });
                    const result = try self.evaluate(e.condition);
                    if (self.debug_enabled) {
                        std.debug.print("Condition result: {any}\n", .{result});
                    }
                    if (result == .boolean and result.boolean) {
                        return token.TokenLiteral{ .boolean = true };
                    }
                }

                return token.TokenLiteral{ .boolean = false };
            },
            .ForAll => |f| {
                // Get the variable name
                const var_name = f.variable.lexeme;

                // Store the current environment to restore it later
                const prev_env = self.environment;
                defer self.environment = prev_env;

                // Create a new environment for the quantifier scope
                var quantifier_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                self.environment = &quantifier_env;

                // Evaluate the array expression to get the actual array
                const array_value = try self.evaluate(f.array);
                if (array_value != .array) {
                    return error.TypeError;
                }

                // Iterate over the actual array elements
                for (array_value.array) |val| {
                    try self.environment.define(var_name, val, .{ .base = .Dynamic });
                    const result = try self.evaluate(f.condition);
                    if (!(result == .boolean and result.boolean)) {
                        return token.TokenLiteral{ .boolean = false };
                    }
                }

                return token.TokenLiteral{ .boolean = true };
            },
            .ArrayType => {
                return token.TokenLiteral{ .nothing = {} };
            },
        };
    }

    fn makeNothing(self: *Interpreter) token.TokenLiteral {
        _ = self;
        return .{ .nothing = {} };
    }

    fn executeAssignment(self: *Interpreter, expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        const assignment = expr.Assignment;
        const value = try self.evaluate(assignment.value);

        // Get variable's type info
        const var_type = try self.environment.getTypeInfo(assignment.name.lexeme);

        // Type checking
        switch (var_type.base) {
            .Int => {
                if (value != .int) {
                    std.debug.print("Type error: Cannot assign {s} to int variable\n", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Float => {
                if (value != .float) {
                    std.debug.print("Type error: Cannot assign {s} to float variable\n", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .String => {
                if (value != .string) {
                    std.debug.print("Type error: Cannot assign {s} to string variable\n", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Boolean => {
                if (value != .boolean) {
                    std.debug.print("Type error: Cannot assign {s} to boolean variable\n", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Dynamic => {}, // Allow any assignment for dynamic types
            .Auto => {}, // Type is already fixed from initialization
            else => {},
        }

        try self.environment.assign(assignment.name.lexeme, value);
        return value;
    }

    fn callFunction(self: *Interpreter, callee: token.TokenLiteral, arguments: []const *ast.Expr) ErrorList!token.TokenLiteral {
        switch (callee) {
            .function => |f| {
                var environment = Environment.init(self.memory.getAllocator(), f.closure, self.debug_enabled);
                defer environment.deinit();

                // Type check arguments against parameter types
                if (arguments.len != f.params.len) {
                    return error.InvalidArgumentCount;
                }

                for (arguments, f.params) |arg, param| {
                    const arg_value = try self.evaluate(arg);

                    // Type check argument against parameter type
                    switch (param.type_info.base) {
                        .Int => if (arg_value != .int) {
                            std.debug.print("Type error: Expected int argument, got {s}\n", .{@tagName(arg_value)});
                            return error.TypeError;
                        },
                        .Float => if (arg_value != .float) {
                            std.debug.print("Type error: Expected float argument, got {s}\n", .{@tagName(arg_value)});
                            return error.TypeError;
                        },
                        .Boolean => if (arg_value != .boolean) {
                            std.debug.print("Type error: Expected boolean argument, got {s}\n", .{@tagName(arg_value)});
                            return error.TypeError;
                        },
                        .String => if (arg_value != .string) {
                            std.debug.print("Type error: Expected string argument, got {s}\n", .{@tagName(arg_value)});
                            return error.TypeError;
                        },
                        .Dynamic => {},
                        else => {},
                    }

                    try environment.define(param.name.lexeme, arg_value, param.type_info);
                }

                const old_env = self.environment;
                self.environment = &environment;
                defer self.environment = old_env;

                _ = try self.executeBlock(f.body, &environment);
            },
            else => return error.NotCallable,
        }

        return token.TokenLiteral{ .nothing = {} };
    }

    fn assignField(self: *Interpreter, object: token.TokenLiteral, field: token.Token, value: *ast.Expr) ErrorList!token.TokenLiteral {
        if (object != .struct_value) {
            return error.NotAStruct;
        }

        const struct_type = try self.environment.getTypeInfo(object.struct_value.type_name);
        if (struct_type.base != .Struct) {
            return error.TypeError;
        }

        for (object.struct_value.fields) |*struct_field| {
            if (std.mem.eql(u8, struct_field.name, field.lexeme)) {
                const new_value = try self.evaluate(value);
                struct_field.value = new_value;
                return new_value;
            }
        }
        return error.FieldNotFound;
    }

    fn valuesEqual(self: *Interpreter, a: token.TokenLiteral, b: token.TokenLiteral) bool {
        _ = self;
        return switch (a) {
            .int => |val| b == .int and val == b.int,
            .float => |val| b == .float and val == b.float,
            .string => |val| b == .string and std.mem.eql(u8, val, b.string),
            .boolean => |val| b == .boolean and val == b.boolean,
            .nothing => b == .nothing,
            .array => |arr| b == .array and arr.len == b.array.len,
            .struct_value => |s| b == .struct_value and std.mem.eql(u8, s.type_name, b.struct_value.type_name),
            .function => b == .function,
        };
    }
};
