const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const ReportingModule = @import("reporting.zig");
const ErrorList = ReportingModule.ErrorList;
const MemoryManager = @import("memory.zig").MemoryManager;

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
    enclosing: ?*Environment,
    values: std.StringHashMap(token.TokenLiteral),
    allocator: std.mem.Allocator,
    debug_enabled: bool,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment, debug_enabled: bool) Environment {
        return Environment{
            .enclosing = enclosing,
            .values = std.StringHashMap(token.TokenLiteral).init(allocator),
            .allocator = allocator,
            .debug_enabled = debug_enabled,
        };
    }

    pub fn deinit(self: *Environment) void {
        var it = self.values.iterator();
        while (it.next()) |entry| {
            switch (entry.value_ptr.*) {
                .array => |arr| self.allocator.free(arr),
                .function => |f| {
                    // Clean up each parameter's type expression
                    for (f.params) |*param| {
                        param.deinit(self.allocator);
                    }
                    self.allocator.free(f.params);
                },
                else => {},
            }
        }
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: token.TokenLiteral) !void {
        // Check if variable already exists in current scope
        if (self.values.contains(name)) {
            return error.VariableAlreadyDefined;
        }
        try self.values.put(name, value);
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
};

pub const Interpreter = struct {
    memory: *MemoryManager,
    environment: *Environment,
    globals: *Environment,
    debug_enabled: bool,
    string_interner: StringInterner,

    pub fn init(memory: *MemoryManager) !Interpreter {
        const globals = try memory.getAllocator().create(Environment);
        globals.* = Environment.init(memory.getAllocator(), null, memory.debug_enabled);

        return Interpreter{
            .memory = memory,
            .environment = globals,
            .globals = globals,
            .debug_enabled = memory.debug_enabled,
            .string_interner = StringInterner.init(memory.getAllocator()),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.string_interner.deinit();
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
                const value = if (decl.initializer) |i|
                    try self.evaluate(i)
                else
                    token.TokenLiteral{ .nothing = {} };

                const key = try self.string_interner.intern(decl.name.lexeme);
                try self.environment.define(key, value);

                if (self.debug_enabled) {
                    std.debug.print("Declared variable {s} = {any}\n", .{
                        decl.name.lexeme, value,
                    });
                }
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
            .Function => |func| {
                if (self.debug_enabled) {
                    std.debug.print("Defining function '{s}' with {} parameters\n", .{ func.name.lexeme, func.params.len });
                }

                // Make a copy of the parameters
                var params = try self.memory.getAllocator().alloc(ast.FunctionParam, func.params.len);
                errdefer {
                    for (params) |*param| {
                        param.deinit(self.memory.getAllocator());
                    }
                    self.memory.getAllocator().free(params);
                }

                // Deep copy each parameter
                for (func.params, 0..) |param, i| {
                    params[i] = .{
                        .name = param.name,
                        .type_expr = param.type_expr,
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
                try self.environment.define(func.name.lexeme, function);
                return null;
            },
            .Return => |ret| {
                if (ret.value) |value_expr| {
                    const return_value = try self.evaluate(value_expr);
                    try self.environment.define("return", return_value);
                    return error.ReturnValue;
                }
                return error.ReturnNothing;
            },
        };
    }

    pub fn evaluate(self: *Interpreter, expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        const result = switch (expr.*) {
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
                            if (left.struct_value.len != right.struct_value.len) break :blk token.TokenLiteral{ .boolean = false };
                            for (left.struct_value, right.struct_value) |l, r| {
                                if (!std.mem.eql(u8, l.name, r.name) or !std.meta.eql(l.value, r.value))
                                    break :blk token.TokenLiteral{ .boolean = false };
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
                const key = try self.string_interner.intern(assign.name.lexeme);
                try self.environment.assign(key, value);
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

                for (elements) |element| {
                    const value = try self.evaluate(element);
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

                return token.TokenLiteral{ .struct_value = try struct_fields.toOwnedSlice() };
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
                if (self.debug_enabled) {
                    std.debug.print("\nEvaluating function call...\n", .{});
                }

                const callee = try self.evaluate(call.callee);
                if (callee != .function) {
                    if (self.debug_enabled) {
                        std.debug.print("Error: Not a function\n", .{});
                    }
                    return error.NotCallable;
                }

                if (self.debug_enabled) {
                    std.debug.print("Function call - expected params: {}, provided args: {}\n", .{ callee.function.params.len, call.arguments.len });
                }

                // Validate parameter count
                if (call.arguments.len != callee.function.params.len) {
                    if (self.debug_enabled) {
                        std.debug.print("Error: Parameter count mismatch\n", .{});
                    }
                    return error.InvalidArgumentCount;
                }

                // Evaluate all arguments
                var args = try self.memory.getAllocator().alloc(token.TokenLiteral, call.arguments.len);
                defer self.memory.getAllocator().free(args);

                for (call.arguments, 0..) |arg, i| {
                    args[i] = try self.evaluate(arg);
                }

                // Create a new environment for the function
                var func_env = Environment.init(self.memory.getAllocator(), callee.function.closure, self.debug_enabled);
                defer func_env.deinit();

                // Bind parameters to arguments
                for (callee.function.params, args) |param, arg| {
                    try func_env.define(param.name.lexeme, arg);
                }

                // Execute function body
                const previous = self.environment;
                self.environment = &func_env;
                defer self.environment = previous;

                // Execute the function body and handle return values
                _ = self.executeBlock(callee.function.body, &func_env) catch |err| {
                    if (err == error.ReturnValue) {
                        if (func_env.get("return")) |return_value| {
                            if (self.debug_enabled) {
                                std.debug.print("Function returned: {any}\n", .{return_value});
                            }
                            return return_value;
                        }
                    }
                    return err;
                };

                // If no explicit return, return nothing
                return token.TokenLiteral{ .nothing = {} };
            },
            .Logical => |logical| {
                const left = try self.evaluate(logical.left);
                const right = try self.evaluate(logical.right);
                return token.TokenLiteral{ .boolean = switch (logical.operator.type) {
                    .AND_KEYWORD, .AND_SYMBOL => left.boolean and right.boolean,
                    .OR_KEYWORD, .OR_SYMBOL => left.boolean or right.boolean,
                    else => return error.InvalidOperator,
                } };
            },
            .Function => |f| token.TokenLiteral{ .function = .{
                .params = f.params,
                .body = f.body,
                .closure = self.environment,
            } },
            .Print => |e| {
                if (e) |print_expr| {
                    const value = try self.evaluate(print_expr);
                    switch (value) {
                        .int => |n| std.debug.print("{d}\n", .{n}),
                        .float => |f| std.debug.print("{d}\n", .{f}),
                        .string => |s| std.debug.print("{s}\n", .{s}),
                        .boolean => |b| std.debug.print("{}\n", .{b}),
                        .nothing => std.debug.print("nothing\n", .{}),
                        .array => |arr| {
                            std.debug.print("[", .{});
                            for (arr, 0..) |item, i| {
                                if (i > 0) std.debug.print(", ", .{});
                                switch (item) {
                                    .int => |n| std.debug.print("{d}", .{n}),
                                    .float => |f| std.debug.print("{d}", .{f}),
                                    .string => |s| std.debug.print("\"{s}\"", .{s}),
                                    .boolean => |b| std.debug.print("{}", .{b}),
                                    .nothing => std.debug.print("nothing", .{}),
                                    else => std.debug.print("...", .{}),
                                }
                            }
                            std.debug.print("]\n", .{});
                        },
                        else => std.debug.print("{any}\n", .{value}),
                    }
                    return value;
                }
                return token.TokenLiteral{ .nothing = {} };
            },
        };

        if (self.debug_enabled) {
            std.debug.print("Evaluated result: {any}\n", .{result});
        }
        return result;
    }

    fn makeNothing(self: *Interpreter) token.TokenLiteral {
        _ = self;
        return .{ .nothing = {} };
    }
};
