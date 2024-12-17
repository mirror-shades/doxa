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

    pub fn define(self: *Environment, key: []const u8, value: token.TokenLiteral, type_info: ast.TypeInfo) !void {
        if (self.debug_enabled) {
            std.debug.print("Defining variable '{s}' = {any}\n", .{ key, value });
        }

        // Check if the variable already exists in this scope
        if (self.values.contains(key)) {
            // If it exists and is mutable, update it
            const type_info_result = try self.getTypeInfo(key);
            if (type_info_result.is_mutable) {
                try self.values.put(key, value);
                try self.types.put(key, type_info);
                return;
            } else {
                return error.ImmutableVariable;
            }
        }

        // If it doesn't exist, add it
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
    entry_point: ?[]const u8 = null,

    pub fn init(memory: *MemoryManager) !Interpreter {
        const globals = try memory.getAllocator().create(Environment);
        globals.* = Environment.init(memory.getAllocator(), null, memory.debug_enabled);

        var interpreter = Interpreter{
            .memory = memory,
            .environment = globals,
            .globals = globals,
            .debug_enabled = memory.debug_enabled,
            .string_interner = StringInterner.init(memory.getAllocator()),
            .stdout_buffer = std.ArrayList(u8).init(memory.getAllocator()),
            .entry_point = null,
        };

        // Explicitly set debug mode
        interpreter.debug_enabled = memory.debug_enabled;

        if (memory.debug_enabled) {
            std.debug.print("Interpreter initialized with debug mode: {}\n", .{interpreter.debug_enabled});
        }

        return interpreter;
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.string_interner.deinit();
        self.stdout_buffer.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []ast.Stmt) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("\n=== Starting interpretation ===\n", .{});
            std.debug.print("Debug mode is: {}\n", .{self.debug_enabled});
            std.debug.print("Number of statements: {d}\n", .{statements.len});
        }

        // First pass - find entry point and execute all statements
        for (statements) |*stmt| {
            if (self.debug_enabled) {
                std.debug.print("\nExecuting statement type: {s}\n", .{@tagName(stmt.*)});
            }

            if (stmt.* == .Function) {
                if (stmt.Function.is_entry) {
                    if (self.debug_enabled) {
                        std.debug.print("Found entry point function: {s}\n", .{stmt.Function.name.lexeme});
                    }
                    self.entry_point = stmt.Function.name.lexeme;
                }
            }

            _ = try self.executeStatement(stmt, self.debug_enabled);
        }

        // Execute entry point immediately after all declarations
        if (self.entry_point) |main_fn| {
            if (self.debug_enabled) {
                std.debug.print("\n=== Executing entry point ===\n", .{});
            }

            const main_value = try self.environment.get(main_fn);
            if (main_value != .function) {
                return error.InvalidEntryPoint;
            }

            var args = [_]token.TokenLiteral{};
            _ = try self.callFunction(main_value.function, &args);
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Interpretation complete ===\n", .{});
        }
    }

    pub fn executeBlock(self: *Interpreter, statements: []ast.Stmt, environment: *Environment) ErrorList!?token.TokenLiteral {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;

        // Handle empty blocks
        if (statements.len == 0) {
            return null;
        }

        var result: ?token.TokenLiteral = null;

        // Execute all statements, allowing errors to propagate
        for (statements) |stmt| {
            result = try self.executeStatement(&stmt, self.debug_enabled);
        }

        return result;
    }

    pub fn compare(a: anytype, b: anytype) i8 {
        var a_float: f64 = undefined;
        var b_float: f64 = undefined;
        if (@TypeOf(a) == i32 or @TypeOf(b) == i32) {
            a_float = @as(f64, @floatFromInt(a));
            b_float = @as(f64, @floatFromInt(b));
        } else if (@TypeOf(a) == f64 or @TypeOf(b) == f64) {
            a_float = a;
            b_float = b;
        } else {
            return error.InvalidType;
        }
        if (a_float < b_float) return -1;
        if (a_float > b_float) return 1;
        return 0;
    }

    pub fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt, debug_enabled: bool) ErrorList!?token.TokenLiteral {
        if (debug_enabled) {
            self.debug_enabled = true;
        }
        if (self.debug_enabled) {
            std.debug.print("Executing statement: {any}\n", .{stmt.*});
        }

        return switch (stmt.*) {
            .Function => |f| {
                if (self.debug_enabled) {
                    std.debug.print("Executing function declaration: {s}\n", .{f.name.lexeme});
                }

                const function = token.TokenLiteral{
                    .function = .{
                        .params = f.params,
                        .body = f.body,
                        .closure = self.environment,
                    },
                };

                // Define the return type first
                try self.environment.define(
                    "return_type",
                    .{ .nothing = {} },
                    f.return_type_info,
                );

                // Then define the function
                try self.environment.define(
                    f.name.lexeme,
                    function,
                    f.return_type_info,
                );

                if (self.debug_enabled) {
                    std.debug.print("Defined function '{s}' with {d} parameters\n", .{ f.name.lexeme, f.params.len });
                }

                // If this is an entry point function, execute it immediately
                if (f.is_entry) {
                    if (self.debug_enabled) {
                        std.debug.print("\n=== Executing entry point function ===\n", .{});
                    }
                    const empty_args: []const *ast.Expr = &[_]*ast.Expr{};
                    return try self.callFunction(function, empty_args);
                }

                return null;
            },
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
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize int variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Float => if (init_value != .float and init_value != .int) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize float variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Boolean => if (init_value != .boolean) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize bool variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Array => {
                            if (init_value != .array) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot initialize array variable with {s}", .{@tagName(init_value)});
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
                        .Map => {
                            if (init_value != .map) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot initialize map variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }
                        },
                        .Dynamic => {},
                        else => {},
                    }
                    break :blk init_value;
                } else switch (decl.type_info.base) {
                    .Int => token.TokenLiteral{ .int = 0 },
                    .Float => token.TokenLiteral{ .float = 0.0 },
                    .String => token.TokenLiteral{ .string = try self.string_interner.intern("") },
                    .Boolean => token.TokenLiteral{ .boolean = false },
                    .Map => token.TokenLiteral{ .map = std.StringHashMap(token.TokenLiteral).init(self.memory.getAllocator()) },
                    else => token.TokenLiteral{ .nothing = {} },
                };

                try self.environment.define(decl.name.lexeme, value, decl.type_info);
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
            .EnumDecl => |decl| {
                // Create an enum type and store it in environment
                const enum_type = TypeInfo{
                    .base = .Enum,
                    .variants = try self.memory.getAllocator().alloc([]const u8, decl.variants.len),
                };

                // Store each variant
                for (decl.variants, 0..) |variant, i| {
                    enum_type.variants.?[i] = variant.lexeme;
                }

                try self.environment.define(decl.name.lexeme, .{ .nothing = {} }, enum_type);
                return .{ .nothing = {} };
            },
            .Map => |entries| {
                var map = std.StringHashMap(token.TokenLiteral).init(self.memory.getAllocator());
                errdefer map.deinit();

                for (entries) |entry| {
                    const key = try self.evaluate(entry.key);
                    const value = try self.evaluate(entry.value);

                    // Only support string keys for now
                    if (key != .string) {
                        return error.InvalidMapKey;
                    }

                    try map.put(key.string, value);
                }

                return token.TokenLiteral{ .map = map };
            },
            .Try => |try_stmt| {
                // Create new environment for try block
                var try_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                defer try_env.deinit();

                // Execute try block
                const try_result = self.executeBlock(try_stmt.try_body, &try_env) catch |err| {
                    // Create new environment for catch block
                    var catch_env = Environment.init(self.memory.getAllocator(), self.environment, self.debug_enabled);
                    defer catch_env.deinit();

                    // If there's an error variable, bind the error to it
                    if (try_stmt.error_var) |error_var| {
                        try catch_env.define(error_var.lexeme, .{ .string = @errorName(err) }, .{ .base = .String });
                    }

                    // Execute catch block
                    return self.executeBlock(try_stmt.catch_body, &catch_env) catch |catch_err| {
                        return catch_err;
                    };
                };

                return try_result;
            },
            .Module => |_| {
                return .{ .nothing = {} };
            },
            .Import => |_| {
                return .{ .nothing = {} };
            },
            .Path => |_| {
                return .{ .nothing = {} };
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
                    .EQUALITY_SYMBOL, .EQUALITY_KEYWORD => switch (left) {
                        .int => token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) == 0 },
                        .float => token.TokenLiteral{ .boolean = left.float == right.float },
                        .boolean => if (right == .boolean) {
                            return token.TokenLiteral{ .boolean = left.boolean == right.boolean };
                        } else if (right == .tetra) {
                            return token.TokenLiteral{ .boolean = switch (right.tetra) {
                                .true => left.boolean == true,
                                .false => left.boolean == false,
                                .both => true,
                                .neither => false,
                            } };
                        } else token.TokenLiteral{ .boolean = false },
                        .tetra => if (right == .boolean) {
                            return token.TokenLiteral{ .boolean = switch (left.tetra) {
                                .true => right.boolean == true,
                                .false => right.boolean == false,
                                .both => true,
                                .neither => false,
                            } };
                        } else if (right == .tetra) {
                            return token.TokenLiteral{ .boolean = left.tetra == right.tetra };
                        } else token.TokenLiteral{ .boolean = false },
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
                        .enum_variant => token.TokenLiteral{ .boolean = std.mem.eql(u8, left.enum_variant, right.enum_variant) },
                        .tuple => |l| switch (right) {
                            .tuple => |r| {
                                if (l.len != r.len) return token.TokenLiteral{ .boolean = false };
                                // Compare each element
                                for (l, 0..) |item, i| {
                                    if (!self.valuesEqual(item, r[i])) {
                                        return token.TokenLiteral{ .boolean = false };
                                    }
                                }
                                return token.TokenLiteral{ .boolean = true };
                            },
                            else => token.TokenLiteral{ .boolean = false },
                        },
                        .map => token.TokenLiteral{ .boolean = false },
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
                            return token.TokenLiteral{ .boolean = left.float > @as(f64, @floatFromInt(right.int)) };
                        }
                        if (left == .int and right == .float) {
                            return token.TokenLiteral{ .boolean = @as(f64, @floatFromInt(left.int)) > right.float };
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
                    .NOT_LOGICAL, .NOT_KEYWORD => {
                        if (operand == .boolean) {
                            return token.TokenLiteral{ .boolean = !operand.boolean };
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

                // Check mutability
                if (!var_type.is_mutable) {
                    return error.ConstAssignment;
                }

                // Type checking
                if (!var_type.is_dynamic) {
                    switch (var_type.base) {
                        .Int => {
                            if (value != .int) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Float => {
                            if (value != .float) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .String => {
                            if (value != .string) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Boolean => {
                            if (value != .boolean) {
                                var reporting = ReportingModule.Reporting.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to boolean variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Dynamic => {}, // Allow any assignment for dynamic types
                        .Auto => {}, // Type is already fixed from initialization
                        else => {},
                    }
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

                const branch = if ((condition.boolean))
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

                // Evaluate first element to establish type
                if (elements.len > 0) {
                    const first = try self.evaluate(elements[0]);
                    try array_values.append(first);
                    const first_type = @as(std.meta.Tag(token.TokenLiteral), first);

                    // Check remaining elements match the first element's type
                    for (elements[1..]) |element| {
                        const value = try self.evaluate(element);
                        const value_type = @as(std.meta.Tag(token.TokenLiteral), value);

                        if (value_type != first_type) {
                            array_values.deinit();
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Heterogeneous array detected: cannot mix {s} and {s}", .{ @tagName(first_type), @tagName(value_type) });
                            return error.HeterogeneousArray;
                        }
                        try array_values.append(value);
                    }
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
            .Index => |index| {
                const array_value = try self.evaluate(index.array);
                const index_value = try self.evaluate(index.index);

                return switch (array_value) {
                    .array => |arr| {
                        if (index_value != .int) {
                            return error.TypeError;
                        }
                        if (index_value.int < 0) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Array index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= arr.len) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("Array index out of bounds: index {d} for array of length {d}", .{ idx, arr.len });
                            return error.IndexOutOfBounds;
                        }
                        return arr[idx];
                    },
                    .map => |m| {
                        if (index_value != .string) {
                            return error.TypeError;
                        }
                        if (m.get(index_value.string)) |value| {
                            return value;
                        }
                        return error.KeyNotFound;
                    },
                    .tuple => |tup| {
                        if (index_value != .int) {
                            return error.TypeError;
                        }
                        if (index_value.int >= tup.len) {
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= tup.len) {
                            return error.IndexOutOfBounds;
                        }
                        return tup[idx];
                    },
                    else => error.TypeError,
                };
            },
            .IndexAssign => |idx_assign| {
                const array_val = try self.evaluate(idx_assign.array);
                const index = try self.evaluate(idx_assign.index);
                const new_value = try self.evaluate(idx_assign.value);

                // Prevent tuple modification
                if (array_val == .tuple) {
                    return error.CannotModifyTuple;
                }

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
                // Convert boolean operands to tetra if needed
                var left_val: token.TokenLiteral = undefined;
                if (left == .boolean)
                    left_val = token.TokenLiteral{ .tetra = if (left.boolean) .true else .false }
                else
                    left_val = left;
                const right_val = if (right == .boolean)
                    token.TokenLiteral{ .tetra = if (right.boolean) .true else .false }
                else
                    right;

                return token.TokenLiteral{
                    .tetra = switch (logical.operator.type) {
                        .AND_KEYWORD, .AND_SYMBOL, .AND_LOGICAL => switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => .false,
                            .both => if (right_val.tetra == .false) .false else .both,
                            .neither => .neither,
                        },
                        .OR_KEYWORD, .OR_SYMBOL, .OR_LOGICAL => switch (left_val.tetra) {
                            .true => .true,
                            .false => right_val.tetra,
                            .both => .both,
                            .neither => if (right_val.tetra == .true) .true else .neither,
                        },
                        .XOR => switch (left_val.tetra) {
                            .true => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .false => right_val.tetra,
                            .both => .both,
                            .neither => .neither,
                        },
                        .IFF => switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .both => .both,
                            .neither => .neither,
                        },
                        .NAND => switch (left_val.tetra) {
                            .true => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .false => .true,
                            .both => .both,
                            .neither => .neither,
                        },
                        .NOR => switch (left_val.tetra) {
                            .true => .false,
                            .false => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .both => .both,
                            .neither => .neither,
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

                // Format the value into the buffer based on its type
                switch (value) {
                    .int => |i| try buffer.writer().print("{d}", .{i}),
                    .float => |f| try buffer.writer().print("{d}", .{f}),
                    .boolean => |b| try buffer.writer().print("{}", .{b}),
                    .tetra => |t| {
                        try buffer.writer().print("{s}", .{@tagName(t)});
                    },
                    .string => |s| {
                        // Only wrap in quotes if it's a literal string value, not a type name or enum variant
                        if (print.expr.* == .TypeOf or print.expr.* == .EnumMember) {
                            try buffer.writer().print("{s}", .{s});
                        } else {
                            try buffer.writer().print("\"{s}\"", .{s});
                        }
                    },
                    .nothing => try buffer.writer().print("nothing", .{}),
                    .array => |arr| {
                        try buffer.writer().print("[", .{});
                        for (arr, 0..) |item, i| {
                            if (i > 0) try buffer.writer().print(", ", .{});
                            switch (item) {
                                .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                else => try buffer.writer().print("{any}", .{item}),
                            }
                        }
                        try buffer.writer().print("]", .{});
                    },
                    .function => try buffer.writer().print("function", .{}),
                    .struct_value => |sv| try buffer.writer().print("{any}", .{sv}),
                    .enum_variant => |variant| try buffer.writer().print("{s}", .{variant}),
                    .tuple => |tup| {
                        try buffer.writer().print("(", .{});
                        for (tup, 0..) |item, i| {
                            if (i > 0) try buffer.writer().print(", ", .{});
                            switch (item) {
                                .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                else => try buffer.writer().print("{any}", .{item}),
                            }
                        }
                        try buffer.writer().print(")", .{});
                    },
                    .map => |m| {
                        try buffer.writer().print("{{", .{}); // Double braces for escaping
                        var iter = m.iterator();
                        var first = true;
                        while (iter.next()) |entry| {
                            if (!first) {
                                try buffer.writer().print(", ", .{});
                            }
                            first = false;
                            try buffer.writer().print("{s}: ", .{entry.key_ptr.*});

                            switch (entry.value_ptr.*) {
                                .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                .int => |i| try buffer.writer().print("{d}", .{i}),
                                .float => |f| try buffer.writer().print("{d}", .{f}),
                                .boolean => |b| try buffer.writer().print("{}", .{b}),
                                .tetra => |t| {
                                    const name = @tagName(t);
                                    var lower_buffer: [16]u8 = undefined; // Adjust size as needed
                                    for (name, 0..) |c, i| {
                                        lower_buffer[i] = std.ascii.toLower(c);
                                    }
                                    try buffer.writer().print("{s}", .{lower_buffer[0..name.len]});
                                },
                                .nothing => try buffer.writer().print("nothing", .{}),
                                .array => |arr| {
                                    try buffer.writer().print("[", .{});
                                    for (arr, 0..) |item, i| {
                                        if (i > 0) try buffer.writer().print(", ", .{});
                                        switch (item) {
                                            .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                            else => try buffer.writer().print("{any}", .{item}),
                                        }
                                    }
                                    try buffer.writer().print("]", .{});
                                },
                                .map => try buffer.writer().print("nested-map", .{}),
                                .function => try buffer.writer().print("function", .{}),
                                .struct_value => |sv| try buffer.writer().print("{any}", .{sv}),
                                .enum_variant => |variant| try buffer.writer().print(".{s}", .{variant}),
                                .tuple => |tup| {
                                    try buffer.writer().print("(", .{});
                                    for (tup, 0..) |item, i| {
                                        if (i > 0) try buffer.writer().print(", ", .{});
                                        switch (item) {
                                            .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                            else => try buffer.writer().print("{any}", .{item}),
                                        }
                                    }
                                    try buffer.writer().print(")", .{});
                                },
                            }
                        }
                        try buffer.writer().print("}}", .{}); // Double braces for escaping
                    },
                }

                try buffer.writer().print("\n", .{});
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
                // First check if this is an enum type access
                if (object == .nothing) {
                    // Try to get type info for the object
                    const type_info = self.environment.getTypeInfo(field.object.Variable.lexeme) catch |err| {
                        if (self.debug_enabled) {
                            std.debug.print("Error getting type info: {}\n", .{err});
                        }
                        return err;
                    };
                    if (type_info.base == .Enum) {
                        // This is an enum member access
                        return token.TokenLiteral{ .enum_variant = field.field.lexeme };
                    }
                }
                // Otherwise handle as struct field access
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
            .Match => |match_expr| {
                const value = try self.evaluate(match_expr.value);

                // For each case in the match expression
                for (match_expr.cases) |case| {
                    // Handle else case
                    if (case.pattern.type == .ELSE) {
                        return try self.evaluate(case.body);
                    }
                    // Check if this case matches the value
                    if (std.mem.eql(u8, case.pattern.lexeme, value.enum_variant)) {
                        return try self.evaluate(case.body);
                    }
                }
                return error.NoMatchCase;
            },
            .EnumDecl => |decl| {
                // Create an enum type and store it in environment
                const enum_type = TypeInfo{
                    .base = .Enum,
                    .variants = try self.memory.getAllocator().alloc([]const u8, decl.variants.len),
                };

                // Store each variant
                for (decl.variants, 0..) |variant, i| {
                    enum_type.variants.?[i] = variant.lexeme;
                }

                try self.environment.define(decl.name.lexeme, .{ .nothing = {} }, enum_type);
                return .{ .nothing = {} };
            },
            .EnumMember => |member| {
                // Return the enum variant as a string
                return token.TokenLiteral{ .enum_variant = member.lexeme };
            },
            .DefaultArgPlaceholder => {
                return token.TokenLiteral{ .nothing = {} };
            },
            .TypeOf => |expr_value| {
                const value = try self.evaluate(expr_value);
                return token.TokenLiteral{
                    .string = switch (value) {
                        .int => "int",
                        .float => "float",
                        .string => "string",
                        .boolean => "boolean",
                        .tetra => "tetra",
                        .nothing => if (expr_value.* == .EnumDecl or expr_value.* == .StructDecl or
                            if (expr_value.* == .Variable)
                            if (self.environment.getTypeInfo(expr_value.Variable.lexeme) catch null) |type_info|
                                type_info.base == .Enum or type_info.base == .Struct
                            else
                                false
                        else
                            false)
                            switch (expr_value.*) {
                                .EnumDecl => "enum",
                                .StructDecl => "struct",
                                .Variable => |var_token| if (self.environment.getTypeInfo(var_token.lexeme) catch null) |type_info|
                                    if (type_info.base == .Enum) "enum" else "struct"
                                else
                                    "nothing",
                                else => "nothing",
                            }
                        else
                            "nothing",
                        .array => "array",
                        .function => "function",
                        .struct_value => |sv| sv.type_name,
                        .enum_variant => |ev| return token.TokenLiteral{ .string = if (value == .enum_variant and std.mem.eql(u8, ev, value.enum_variant))
                            value.enum_variant
                        else
                            "enum_variant" },
                        .tuple => "tuple",
                        .map => "map",
                    },
                };
            },
            .Tuple => |elements| {
                var tuple_values = std.ArrayList(token.TokenLiteral).init(self.memory.getAllocator());
                errdefer tuple_values.deinit();

                // Evaluate each element in the tuple
                for (elements) |element| {
                    const value = try self.evaluate(element);
                    try tuple_values.append(value);
                }

                const owned_slice = try tuple_values.toOwnedSlice();
                errdefer self.memory.getAllocator().free(owned_slice);

                return token.TokenLiteral{ .tuple = owned_slice };
            },
            .Map => |entries| {
                var map = std.StringHashMap(token.TokenLiteral).init(self.memory.getAllocator());
                errdefer map.deinit();

                for (entries) |entry| {
                    const key = try self.evaluate(entry.key);
                    const value = try self.evaluate(entry.value);

                    // Only support string keys for now
                    if (key != .string) {
                        return error.InvalidMapKey;
                    }

                    try map.put(key.string, value);
                }

                return token.TokenLiteral{ .map = map };
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

        // Check mutability
        if (!var_type.is_mutable) {
            return error.ConstAssignment;
        }

        // Type checking
        if (!var_type.is_dynamic) {
            switch (var_type.base) {
                .Int => {
                    if (value != .int) {
                        var reporting = ReportingModule.Reporting.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Float => {
                    if (value != .float) {
                        var reporting = ReportingModule.Reporting.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .String => {
                    if (value != .string) {
                        var reporting = ReportingModule.Reporting.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Boolean => {
                    if (value != .boolean) {
                        var reporting = ReportingModule.Reporting.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to boolean variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Dynamic => {}, // Allow any assignment for dynamic types
                .Auto => {}, // Type is already fixed from initialization
                else => {},
            }
        }

        try self.environment.assign(assignment.name.lexeme, value);
        return value;
    }

    pub fn callFunction(self: *Interpreter, callee: token.TokenLiteral, arguments: []const *ast.Expr) ErrorList!token.TokenLiteral {
        switch (callee) {
            .function => |f| {
                // Create new environment for function call
                var function_env = Environment.init(self.memory.getAllocator(), f.closure, self.debug_enabled);
                errdefer function_env.deinit();

                // Check argument count
                if (arguments.len > f.params.len) {
                    return error.TooManyArguments;
                }

                // Evaluate and bind arguments to parameters
                for (f.params, 0..) |param, i| {
                    var value: token.TokenLiteral = undefined;

                    if (i < arguments.len) {
                        // If an argument is provided and it's not ~, use it
                        const arg = arguments[i];
                        if (arg.* == .DefaultArgPlaceholder) {
                            // Use default value if available, otherwise error
                            if (param.default_value) |default| {
                                if (self.debug_enabled) {
                                    std.debug.print("Using default value for parameter '{s}'\n", .{param.name.lexeme});
                                }
                                value = try self.evaluate(default);
                            } else {
                                return error.NoDefaultValue;
                            }
                        } else {
                            value = try self.evaluate(arg);
                        }
                    } else if (param.default_value) |default| {
                        // No argument provided, use default value
                        if (self.debug_enabled) {
                            std.debug.print("Using default value for parameter '{s}'\n", .{param.name.lexeme});
                        }
                        value = try self.evaluate(default);
                    } else {
                        return error.TooFewArguments;
                    }

                    if (self.debug_enabled) {
                        std.debug.print("Binding parameter '{s}' = {any}\n", .{ param.name.lexeme, value });
                    }

                    try function_env.define(param.name.lexeme, value, .{ .base = .Dynamic, .is_dynamic = true, .is_mutable = true });
                }

                // Execute function body with the new environment
                const result = self.executeBlock(f.body, &function_env) catch |err| {
                    if (err == error.ReturnValue) {
                        if (function_env.get("return")) |return_value| {
                            return return_value;
                        }
                    }
                    return err;
                };

                // If we get here, there was no return statement
                return if (result) |value| value else .{ .nothing = {} };
            },
            else => return error.NotCallable,
        }
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
        return switch (a) {
            .int => |val| b == .int and val == b.int,
            .float => |val| b == .float and val == b.float,
            .string => |val| b == .string and std.mem.eql(u8, val, b.string),
            .boolean => if (b == .boolean or b == .tetra) switch (b) {
                .boolean => a.boolean == b.boolean,
                .tetra => switch (b.tetra) {
                    .true => a.boolean == true,
                    .false => a.boolean == false,
                    .both => true,
                    .neither => false,
                },
                else => unreachable,
            } else false,
            .tetra => if (b == .boolean) switch (a.tetra) {
                .true => b.boolean == true,
                .false => b.boolean == false,
                .both => true,
                .neither => false,
            } else if (b == .tetra) {
                return a.tetra == b.tetra;
            } else false,
            .nothing => b == .nothing,
            .array => |arr| b == .array and arr.len == b.array.len,
            .struct_value => |s| b == .struct_value and std.mem.eql(u8, s.type_name, b.struct_value.type_name),
            .function => b == .function,
            .enum_variant => |ev| if (b == .enum_variant) {
                return std.mem.eql(u8, ev, b.enum_variant);
            } else false,
            .tuple => |arr| b == .tuple and arr.len == b.tuple.len,
            .map => |m| {
                if (b != .map) return false;
                if (m.count() != b.map.count()) return false;

                // Check that all key-value pairs match
                var iter = m.iterator();
                while (iter.next()) |entry| {
                    if (b.map.get(entry.key_ptr.*)) |other_value| {
                        if (!self.valuesEqual(entry.value_ptr.*, other_value)) {
                            return false;
                        }
                    } else {
                        return false; // Key not found in other map
                    }
                }
                return true;
            },
        };
    }
};
