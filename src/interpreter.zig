const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const ReportingModule = @import("reporting.zig");
const ErrorList = ReportingModule.ErrorList;

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
            // Free any array values
            if (entry.value_ptr.* == .array) {
                self.allocator.free(entry.value_ptr.*.array);
            }
        }
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: token.TokenLiteral) !void {
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
    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    debug_enabled: bool,
    string_interner: StringInterner,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) !Interpreter {
        const globals = try allocator.create(Environment);
        globals.* = Environment.init(allocator, null, debug_enabled);

        return Interpreter{
            .allocator = allocator,
            .environment = globals,
            .globals = globals,
            .debug_enabled = debug_enabled,
            .string_interner = StringInterner.init(allocator),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.allocator.destroy(self.globals);
        self.string_interner.deinit();
    }

    pub fn executeBlock(self: *Interpreter, statements: []ast.Stmt, environment: *Environment) ErrorList!void {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;

        for (statements) |stmt| {
            try self.executeStatement(&stmt);
        }
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

    pub fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt) ErrorList!void {
        switch (stmt.*) {
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
            },
            .Expression => |expr| {
                if (self.debug_enabled) {
                    std.debug.print("Before evaluating expression\n", .{});
                }
                const value = try self.evaluate(expr orelse return error.InvalidExpression);
                if (self.debug_enabled) {
                    std.debug.print("{any}\n", .{value});
                }
            },
            .Block => |statements| {
                var block_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer block_env.deinit();
                try self.executeBlock(statements, &block_env);
            },
            .Function => |func| {
                // Create function value
                const function = token.TokenLiteral{ .function = .{
                    .params = func.params,
                    .body = func.body,
                    .closure = self.environment,
                } };

                // Store in current environment
                try self.environment.define(func.name.lexeme, function);
            },
            .Return => |ret| {
                if (ret.value) |value_expr| {
                    const return_value = try self.evaluate(value_expr);
                    // Store the return value in the environment
                    try self.environment.define("return", return_value);
                    return error.ReturnValue;
                }
                return error.ReturnNothing;
            },
        }
    }

    pub fn evaluate(self: *Interpreter, expr: *const ast.Expr) !token.TokenLiteral {
        switch (expr.*) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |bin| {
                const left = try self.evaluate(bin.left orelse return error.InvalidExpression);
                const right = try self.evaluate(bin.right orelse return error.InvalidExpression);

                switch (bin.operator.type) {
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
                    .EQUALITY => {
                        return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) == 0 };
                    },
                    .BANG_EQUAL => {
                        return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) != 0 };
                    },
                    else => return error.InvalidOperator,
                }
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
                    var it = self.environment.values.iterator();
                    while (it.next()) |entry| {
                        std.debug.print("Stored key: '{s}' (len: {}), value: {any}\n", .{ entry.key_ptr.*, entry.key_ptr.*.len, entry.value_ptr.* });
                    }
                }
                if (self.environment.get(var_token.lexeme)) |value| {
                    return value;
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

                return if (condition.boolean)
                    try self.evaluate(if_expr.then_branch orelse return error.InvalidExpression)
                else
                    try self.evaluate(if_expr.else_branch orelse return error.InvalidExpression);
            },
            .Block => |statements| {
                var last_value = token.TokenLiteral{ .nothing = {} };
                var block_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer block_env.deinit();

                try self.executeBlock(statements, &block_env);

                // Get the value of the last expression in the block
                if (statements.len > 0) {
                    const last_stmt = statements[statements.len - 1];
                    if (last_stmt == .Expression) {
                        if (last_stmt.Expression) |ex| {
                            last_value = try self.evaluate(ex);
                        }
                    }
                }

                return last_value;
            },
            .Array => |elements| {
                var array_values = std.ArrayList(token.TokenLiteral).init(self.allocator);
                errdefer array_values.deinit();

                for (elements) |element| {
                    const value = try self.evaluate(element);
                    try array_values.append(value);
                }

                const owned_slice = try array_values.toOwnedSlice();
                errdefer self.allocator.free(owned_slice);

                return token.TokenLiteral{ .array = owned_slice };
            },
            .Struct => |fields| {
                var struct_fields = std.ArrayList(token.StructField).init(self.allocator);
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
                const callee = try self.evaluate(call.callee);
                if (callee != .function) {
                    return error.NotCallable;
                }

                // Evaluate all arguments
                var args = try self.allocator.alloc(token.TokenLiteral, call.arguments.len);
                defer self.allocator.free(args);

                for (call.arguments, 0..) |arg, i| {
                    args[i] = try self.evaluate(arg);
                }

                // Create a new environment for the function
                var func_env = Environment.init(self.allocator, callee.function.closure, self.debug_enabled);
                defer func_env.deinit();

                // Bind parameters to arguments
                for (callee.function.params, args) |param, arg| {
                    try func_env.define(param.name.lexeme, arg);
                }

                // Execute function body
                const previous = self.environment;
                self.environment = &func_env;
                defer self.environment = previous;

                // Execute the function body and catch any return values
                self.executeBlock(callee.function.body, &func_env) catch |err| switch (err) {
                    error.ReturnValue => {
                        // Return value will be in the environment
                        if (func_env.get("return")) |ret| {
                            return ret;
                        }
                        return token.TokenLiteral{ .nothing = {} };
                    },
                    error.ReturnNothing => return token.TokenLiteral{ .nothing = {} },
                    else => |e| return e,
                };

                return token.TokenLiteral{ .nothing = {} };
            },
        }
    }
};
