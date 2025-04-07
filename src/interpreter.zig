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

    pub fn get(self: *Environment, name: []const u8) ErrorList!?token.TokenLiteral {
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
    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    debug_enabled: bool,
    string_interner: StringInterner,
    stdout_buffer: std.ArrayList(u8),
    entry_point_name: ?[]const u8 = null,
    last_result: ?token.TokenLiteral = null,
    had_error: bool = false,
    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,
    parser: ?*@import("parser/parser_types.zig").Parser = null,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) !Interpreter {
        const globals = try allocator.create(Environment);
        globals.* = Environment.init(allocator, null, debug_enabled);

        const interpreter = Interpreter{
            .allocator = allocator,
            .environment = globals,
            .globals = globals,
            .debug_enabled = debug_enabled,
            .string_interner = StringInterner.init(allocator),
            .stdout_buffer = std.ArrayList(u8).init(allocator),
            .entry_point_name = null,
            .had_error = false,
        };

        if (debug_enabled) {
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
            std.debug.print("Number of statements: {}\n", .{statements.len});
        }

        // First pass - Process all struct and enum declarations
        for (0..statements.len) |i| {
            const stmt = &statements[i];
            if (self.debug_enabled) {
                std.debug.print("\nScanning for types - statement {d}: {s}\n", .{ i, @tagName(stmt.*) });
            }

            if (stmt.* == .Expression) {
                // Check for struct declarations in expressions
                if (stmt.Expression) |expr| {
                    if (expr.* == .StructDecl) {
                        if (self.debug_enabled) {
                            std.debug.print("Processing struct declaration: {s}\n", .{expr.StructDecl.name.lexeme});
                        }
                        // Define the struct type in the environment
                        var fields = std.ArrayList(ast.StructFieldType).init(self.allocator);
                        defer fields.deinit();

                        for (expr.StructDecl.fields) |field| {
                            const type_info = try ast.typeInfoFromExpr(self.allocator, field.type_expr);
                            try fields.append(.{
                                .name = field.name.lexeme,
                                .type_info = type_info,
                            });
                        }

                        try self.environment.define(expr.StructDecl.name.lexeme, .{ .nothing = {} }, .{
                            .base = .Struct,
                            .is_mutable = true,
                            .is_dynamic = false,
                            .struct_fields = try fields.toOwnedSlice(),
                        });
                    }
                }
            } else if (stmt.* == .EnumDecl) {
                if (self.debug_enabled) {
                    std.debug.print("Processing enum declaration: {s}\n", .{stmt.EnumDecl.name.lexeme});
                }
                _ = try self.executeStatement(stmt, self.debug_enabled);
            }
        }

        // Second pass - Process all variable declarations
        var var_declarations = std.ArrayList(*ast.Stmt).init(self.allocator);
        defer var_declarations.deinit();

        for (0..statements.len) |i| {
            const stmt = &statements[i];
            if (self.debug_enabled) {
                std.debug.print("\nScanning statement {d}: {s}\n", .{ i, @tagName(stmt.*) });
            }

            if (stmt.* == .VarDecl) {
                if (self.debug_enabled) {
                    std.debug.print("Found variable declaration: {s}\n", .{stmt.VarDecl.name.lexeme});
                }
                try var_declarations.append(stmt);
            }
        }

        // Process variable declarations in order of appearance
        if (self.debug_enabled) {
            std.debug.print("\nProcessing {d} variable declarations\n", .{var_declarations.items.len});
        }

        for (var_declarations.items) |stmt| {
            if (self.debug_enabled) {
                std.debug.print("Processing variable declaration: {s}\n", .{stmt.VarDecl.name.lexeme});
            }
            _ = try self.executeStatement(stmt, self.debug_enabled);
        }

        // Third pass - Process all function declarations
        for (statements, 0..) |*stmt, i| {
            if (self.debug_enabled and stmt.* == .Function) {
                std.debug.print("\nChecking statement {d}: {s}\n", .{ i, @tagName(stmt.*) });
                std.debug.print("Raw statement: {any}\n", .{stmt.*});
                const f = stmt.Function;
                std.debug.print("Found function: {s}\n", .{f.name.lexeme});
            }

            if (stmt.* == .Function) {
                const f = stmt.Function;
                if (self.debug_enabled) {
                    std.debug.print("\nCreating forward declaration for: {s}\n", .{f.name.lexeme});
                }

                const function = token.TokenLiteral{
                    .function = .{
                        .params = f.params,
                        .body = f.body,
                        .closure = self.environment,
                    },
                };

                try self.environment.define(
                    f.name.lexeme,
                    function,
                    .{
                        .base = .Function,
                        .is_mutable = false,
                        .is_dynamic = false,
                    },
                );

                if (self.debug_enabled) {
                    std.debug.print("Defined function '{s}' with {d} parameters\n", .{ f.name.lexeme, f.params.len });
                }
            }
        }

        if (self.entry_point_name == null) {
            // Script mode - execute all statements (except functions and variables which were already processed)
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in script mode ===\n", .{});
            }
            for (statements) |*stmt| {
                if (stmt.* == .Function or stmt.* == .VarDecl) {
                    // Skip declarations in third pass since they're already defined
                    continue;
                }
                self.last_result = try self.executeStatement(stmt, self.debug_enabled);
            }
        } else {
            // Program mode with entry point
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in program mode ===\n", .{});
            }

            // In program mode, variable declarations and functions are already processed
            // Only execute the entry point
        }

        // Handle entry point execution
        if (self.entry_point_name) |main_fn| {
            if (self.debug_enabled) {
                std.debug.print("\n=== Executing entry point ===\n", .{});
            }

            const main_value = (try self.environment.get(main_fn)) orelse return error.InvalidEntryPoint;
            if (main_value != .function) {
                return error.InvalidEntryPoint;
            }

            var empty_args = [_]*ast.Expr{}; // Create empty slice of ast.Expr pointers
            self.last_result = try self.callFunction(main_value, &empty_args);
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Interpretation complete ===\n", .{});
        }
    }

    fn createForwardDeclaration(self: *Interpreter, func: ast.FunctionStmt) !void {
        const name = func.name.lexeme;
        const function = token.TokenLiteral{ .function = .{
            .params = func.params,
            .body = func.body,
            .closure = self.environment,
        } };
        try self.environment.define(name, function);
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
        for (statements) |*stmt| {
            result = self.executeStatement(stmt, self.debug_enabled) catch |err| {
                if (err == error.ReturnValue) {
                    // Get the return value from the environment
                    if (try environment.get("return")) |return_value| {
                        // Make a copy of the return value
                        const value = switch (return_value) {
                            .tetra => |t| token.TokenLiteral{ .tetra = t },
                            .boolean => |b| token.TokenLiteral{ .boolean = b },
                            else => return_value,
                        };
                        return value;
                    }
                }
                return err;
            };

            // Check for return value after each statement
            if (try environment.get("return")) |return_value| {
                // Make a copy of the return value
                const value = switch (return_value) {
                    .tetra => |t| token.TokenLiteral{ .tetra = t },
                    .boolean => |b| token.TokenLiteral{ .boolean = b },
                    else => return_value,
                };
                return value;
            }
        }

        if (self.debug_enabled) {
            std.debug.print("Block evaluated to: {any}\n", .{result});
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
                    .Boolean => token.TokenLiteral{ .boolean = token.Boolean.false },
                    .Map => token.TokenLiteral{ .map = std.StringHashMap(token.TokenLiteral).init(self.allocator) },
                    .Array => blk: {
                        // Check if we have a size from the type declaration
                        if (decl.type_info.array_size) |size| {
                            // Create an array of the specified size
                            const array_elements = try self.allocator.alloc(token.TokenLiteral, size);

                            // Initialize with default values (0 for u8)
                            for (array_elements) |*elem| {
                                // Set default value based on element type
                                if (decl.type_info.element_type) |elem_type| {
                                    elem.* = switch (elem_type) {
                                        .Int => token.TokenLiteral{ .int = 0 },
                                        .U8 => token.TokenLiteral{ .u8 = 0 },
                                        .Float => token.TokenLiteral{ .float = 0.0 },
                                        .String => token.TokenLiteral{ .string = try self.string_interner.intern("") },
                                        .Boolean => token.TokenLiteral{ .boolean = .false },
                                        else => token.TokenLiteral{ .nothing = {} },
                                    };
                                } else {
                                    // Default to u8 if no element type specified
                                    elem.* = token.TokenLiteral{ .u8 = 0 };
                                }
                            }

                            break :blk token.TokenLiteral{ .array = array_elements };
                        }

                        // If no size specified, use empty array (current behavior)
                        break :blk token.TokenLiteral{ .array = &[_]token.TokenLiteral{} };
                    },
                    else => token.TokenLiteral{ .nothing = {} },
                };

                try self.environment.define(decl.name.lexeme, value, decl.type_info);
                return null;
            },
            .Block => |statements| {
                var block_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer block_env.deinit();
                const result = try self.executeBlock(statements, &block_env);
                if (self.debug_enabled and result != null) {
                    std.debug.print("{any}\n", .{result.?});
                }
                return result;
            },
            .EnumDecl => |decl| {
                // Create an enum type and store it in environment
                const enum_type = TypeInfo{
                    .base = .Enum,
                    .variants = try self.allocator.alloc([]const u8, decl.variants.len),
                };

                // Store each variant
                for (decl.variants, 0..) |variant, i| {
                    enum_type.variants.?[i] = variant.lexeme;
                }

                try self.environment.define(decl.name.lexeme, .{ .nothing = {} }, enum_type);
                return .{ .nothing = {} };
            },
            .Map => |entries| {
                var map = std.StringHashMap(token.TokenLiteral).init(self.allocator);
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
                var try_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer try_env.deinit();

                // Execute try block
                const try_result = self.executeBlock(try_stmt.try_body, &try_env) catch |err| {
                    // Create new environment for catch block
                    var catch_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
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
            .Return => |ret| {
                if (ret.value) |value| {
                    const return_value = try self.evaluate(value);
                    try self.environment.define("return", return_value, .{ .base = .Dynamic });
                    return error.ReturnValue;
                }
                try self.environment.define("return", .{ .nothing = {} }, .{ .base = .Nothing });
                return error.ReturnValue;
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
                        .int => |i| switch (right) {
                            .int => |j| token.TokenLiteral{ .boolean = if (i == j) .true else .false },
                            .u8 => |j| token.TokenLiteral{ .boolean = if (i == j) .true else .false },
                            else => token.TokenLiteral{ .boolean = .false },
                        },
                        .u8 => |i| switch (right) {
                            .int => |j| token.TokenLiteral{ .boolean = if (i == j) .true else .false },
                            .u8 => |j| token.TokenLiteral{ .boolean = if (i == j) .true else .false },
                            else => token.TokenLiteral{ .boolean = .false },
                        },
                        .float => {
                            if (right != .int and right != .float) return error.TypeError;

                            if (Interpreter.compare(left.int, right.int) == 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        },
                        .boolean, .tetra => {
                            if (right == .boolean or right == .tetra) {
                                return compareLogical(left, right);
                            }
                            return error.TypeError;
                        },
                        .string => {
                            if (std.mem.eql(u8, left.string, right.string)) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        },
                        .nothing => {
                            if (right == .nothing) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        },
                        .array => if (right == .array) blk: {
                            if (left.array.len != right.array.len) break :blk token.TokenLiteral{ .boolean = token.Boolean.false };
                            for (left.array, right.array) |l, r| {
                                if (!std.meta.eql(l, r)) break :blk token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                            break :blk token.TokenLiteral{ .boolean = token.Boolean.true };
                        } else token.TokenLiteral{ .boolean = token.Boolean.false },
                        .struct_value => if (right == .struct_value) blk: {
                            if (left.struct_value.fields.len != right.struct_value.fields.len) break :blk token.TokenLiteral{ .boolean = token.Boolean.false };
                            for (left.struct_value.fields, right.struct_value.fields) |l, r| {
                                if (!std.mem.eql(u8, l.name, r.name)) break :blk token.TokenLiteral{ .boolean = token.Boolean.false };
                                const values_equal = try self.valuesEqual(l.value, r.value);
                                if (!values_equal) break :blk token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                            break :blk token.TokenLiteral{ .boolean = token.Boolean.true };
                        } else token.TokenLiteral{ .boolean = token.Boolean.false },
                        .function => token.TokenLiteral{ .boolean = token.Boolean.false }, // Functions are never equal
                        .enum_variant => {
                            if (std.mem.eql(u8, left.enum_variant, right.enum_variant)) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        },
                        .tuple => |l| switch (right) {
                            .tuple => |r| {
                                if (l.len != r.len) return token.TokenLiteral{ .boolean = token.Boolean.false };
                                // Compare each element
                                for (l, 0..) |item, i| {
                                    const values_equal = try self.valuesEqual(item, r[i]);
                                    if (!values_equal) {
                                        return token.TokenLiteral{ .boolean = token.Boolean.false };
                                    }
                                }
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            },
                            else => token.TokenLiteral{ .boolean = token.Boolean.false },
                        },
                        .map => token.TokenLiteral{ .boolean = token.Boolean.false },
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
                            if (Interpreter.compare(left.int, right.int) > 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        if (left == .float and right == .int) {
                            if (left.float > @as(f64, @floatFromInt(right.int))) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        if (left == .int and right == .float) {
                            if (@as(f64, @floatFromInt(left.int)) > right.float) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        if (left == .float and right == .float) {
                            if (left.float > right.float) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        return error.TypeError;
                    },
                    .GREATER_EQUAL => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) >= 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        return error.TypeError;
                    },
                    .LESS => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) < 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        return error.TypeError;
                    },
                    .LESS_EQUAL => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) <= 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        return error.TypeError;
                    },
                    .BANG_EQUAL => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) != 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        return error.TypeError;
                    },
                    .MODULO => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return token.TokenLiteral{ .int = @mod(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    .OR_KEYWORD, .OR_LOGICAL => {
                        if (left != .boolean and left != .tetra) return error.TypeError;
                        if (right != .boolean and right != .tetra) return error.TypeError;
                        return try orLogical(left, right);
                    },
                    else => return error.InvalidOperator,
                };
            },
            .If => |if_expr| {
                const condition = try self.evaluate(if_expr.condition orelse return error.InvalidExpression);
                if (condition != .boolean) {
                    return error.TypeError;
                }

                const branch = if (condition.boolean == token.Boolean.true)
                    if_expr.then_branch orelse return error.InvalidExpression
                else
                    if_expr.else_branch orelse return error.InvalidExpression;

                // Create a new environment for the if block
                var if_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer if_env.deinit();

                // Allocate the statement array
                var statements = try self.allocator.alloc(ast.Stmt, 1);
                defer self.allocator.free(statements);
                statements[0] = .{ .Expression = branch };

                // Execute the chosen branch in the new environment
                const result = self.executeBlock(statements, &if_env) catch |err| {
                    if (err == error.ReturnValue) {
                        if (try if_env.get("return")) |return_value| {
                            return return_value;
                        }
                    }
                    return err;
                };

                return result orelse .{ .nothing = {} };
            },
            .Block => |block| {
                var block_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                defer block_env.deinit();

                const result = self.executeBlock(block.statements, &block_env) catch |err| {
                    if (err == error.ReturnValue) {
                        if (try block_env.get("return")) |return_value| {
                            return return_value;
                        }
                    }
                    return err;
                };

                return result orelse .{ .nothing = {} };
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
                    .NOT_TRANCENDENTAL => {
                        if (operand == .boolean or operand == .tetra) {
                            return token.TokenLiteral{ .tetra = token.Tetra.neither };
                        }
                        return error.TypeError;
                    },
                    .NOT_LOGICAL, .NOT_KEYWORD => {
                        if (operand == .boolean or operand == .tetra) {
                            return negateLogical(operand);
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
                if (try self.environment.get(var_token.lexeme)) |value| {
                    return value;
                }

                // Check if this is a namespace (imported module)
                // Check for imports registered by the parser
                if (self.parser) |p| {
                    if (p.imported_symbols) |_| {
                        if (self.debug_enabled) {
                            std.debug.print("Checking for imported symbols with name: {s}\n", .{var_token.lexeme});
                        }

                        // Check if this is an import namespace
                        if (p.module_namespaces.contains(var_token.lexeme)) {
                            if (self.debug_enabled) {
                                std.debug.print("Found module namespace: {s}\n", .{var_token.lexeme});
                            }

                            // Return a special value to indicate this is a namespace
                            // The caller should check for namespace when accessing fields
                            return token.TokenLiteral{ .nothing = {} };
                        }
                    }
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
            .CompoundAssign => |compound| {
                // Get the current value of the variable
                const current_value = (try self.environment.get(compound.name.lexeme)) orelse return error.UndefinedVariable;

                // Get the value to add/subtract
                const rhs_value = try self.evaluate(compound.value orelse return error.InvalidExpression);

                // Get variable's type info for mutability check
                const var_type = try self.environment.getTypeInfo(compound.name.lexeme);
                if (!var_type.is_mutable) {
                    return error.ConstAssignment;
                }

                // Perform the compound operation
                const result = switch (compound.operator.type) {
                    .PLUS_EQUAL => switch (current_value) {
                        .int => if (rhs_value == .int)
                            token.TokenLiteral{ .int = current_value.int + rhs_value.int }
                        else
                            return error.TypeError,
                        .u8 => if (rhs_value == .int and rhs_value.int >= 0 and rhs_value.int <= 255) {
                            const sum = current_value.u8 + rhs_value.int;
                            if (sum > 255) return error.Overflow;
                            return token.TokenLiteral{ .u8 = @intCast(sum) };
                        } else return error.TypeError,
                        else => return error.TypeError,
                    },
                    .MINUS_EQUAL => switch (current_value) {
                        .int => if (rhs_value == .int)
                            token.TokenLiteral{ .int = current_value.int - rhs_value.int }
                        else
                            return error.TypeError,
                        .u8 => switch (rhs_value) {
                            .u8 => {
                                const result = @subWithOverflow(current_value.u8, rhs_value.u8);
                                if (result[1] != 0) return error.Overflow;
                                return token.TokenLiteral{ .u8 = result[0] };
                            },
                            .int => {
                                if (rhs_value.int < 0 or rhs_value.int > 255)
                                    return error.Overflow;
                                const val: u8 = @intCast(rhs_value.int);
                                const result = @subWithOverflow(current_value.u8, val);
                                if (result[1] != 0) return error.Overflow;
                                return token.TokenLiteral{ .u8 = result[0] };
                            },
                            else => return error.TypeError,
                        },
                        else => return error.TypeError,
                    },
                    else => return error.InvalidOperator,
                };

                // Assign the result back to the variable
                try self.environment.assign(compound.name.lexeme, result);
                return result;
            },
            .Grouping => |group| {
                return try self.evaluate(group orelse return error.InvalidExpression);
            },
            .Array => |elements| {
                var array_values = std.ArrayList(token.TokenLiteral).init(self.allocator);
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
                return token.TokenLiteral{ .struct_value = .{
                    .type_name = expr.StructLiteral.name.lexeme,
                    .fields = try struct_fields.toOwnedSlice(),
                } };
            },
            .Index => |index| {
                const array_value = try self.evaluate(index.array);
                const index_value = try self.evaluate(index.index);

                return switch (array_value) {
                    .string => |str| {
                        if (index_value != .int) {
                            return error.TypeError;
                        }
                        if (index_value.int < 0) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("String index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= str.len) {
                            var reporting = ReportingModule.Reporting.init();
                            reporting.reportRuntimeError("String index out of bounds: index {d} for string of length {d}", .{ idx, str.len });
                            return error.IndexOutOfBounds;
                        }
                        // Create a new string containing just the character at the index
                        var char_str = try self.allocator.alloc(u8, 1);
                        char_str[0] = str[idx];
                        return token.TokenLiteral{ .string = char_str };
                    },
                    .array => |arr| {
                        if (index_value != .int) {
                            // Special case: check if this is a length access
                            if (index.index.* == .Literal and
                                index.index.Literal == .string and
                                std.mem.eql(u8, "length", index.index.Literal.string))
                            {
                                return token.TokenLiteral{ .int = @intCast(arr.len) };
                            }
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

                // Check if this is a compound assignment
                if (idx_assign.value.* == .CompoundAssign) {
                    // Get the current value at the index
                    const usize_index = @as(usize, @intCast(index.int));
                    if (usize_index >= array_val.array.len) {
                        return error.IndexOutOfBounds;
                    }

                    const current_value = array_val.array[usize_index];

                    // Evaluate the compound assignment
                    const compound = idx_assign.value.CompoundAssign;
                    const rhs_value = try self.evaluate(compound.value orelse return error.InvalidExpression);

                    // Perform the compound operation
                    const result = switch (compound.operator.type) {
                        .PLUS_EQUAL => switch (current_value) {
                            .int => if (rhs_value == .int)
                                token.TokenLiteral{ .int = current_value.int + rhs_value.int }
                            else
                                return error.TypeError,
                            .u8 => switch (rhs_value) {
                                .int => {
                                    if (rhs_value.int < 0 or rhs_value.int > 255) return error.Overflow;
                                    const val: u8 = @intCast(rhs_value.int);
                                    const sum = current_value.u8 + val;
                                    if (sum > 255) return error.Overflow;
                                    return token.TokenLiteral{ .u8 = sum };
                                },
                                .u8 => {
                                    const sum = current_value.u8 + rhs_value.u8;
                                    if (sum > 255) return error.Overflow;
                                    return token.TokenLiteral{ .u8 = sum };
                                },
                                else => return error.TypeError,
                            },
                            else => return error.TypeError,
                        },
                        .MINUS_EQUAL => switch (current_value) {
                            .int => if (rhs_value == .int)
                                token.TokenLiteral{ .int = current_value.int - rhs_value.int }
                            else
                                return error.TypeError,
                            .u8 => switch (rhs_value) {
                                .u8 => {
                                    const result = @subWithOverflow(current_value.u8, rhs_value.u8);
                                    if (result[1] != 0) return error.Overflow;
                                    return token.TokenLiteral{ .u8 = result[0] };
                                },
                                .int => {
                                    if (rhs_value.int < 0 or rhs_value.int > 255)
                                        return error.Overflow;
                                    const val: u8 = @intCast(rhs_value.int);
                                    const result = @subWithOverflow(current_value.u8, val);
                                    if (result[1] != 0) return error.Overflow;
                                    return token.TokenLiteral{ .u8 = result[0] };
                                },
                                else => return error.TypeError,
                            },
                            else => return error.TypeError,
                        },
                        else => return error.InvalidOperator,
                    };

                    // Update the array with the new value
                    array_val.array[usize_index] = result;
                    return result;
                }

                // Regular index assignment
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
                // Check if this is a method call (callee is a field access)
                if (call.callee.* == .FieldAccess) {
                    const field_access = call.callee.FieldAccess;
                    const object = try self.evaluate(field_access.object);

                    // Handle array methods
                    if (object == .array) {
                        if (std.mem.eql(u8, field_access.field.lexeme, "push")) {
                            if (call.arguments.len != 1) {
                                return error.InvalidArgumentCount;
                            }

                            const arg_value = try self.evaluate(call.arguments[0]);

                            // Get the current array
                            const current_array = object.array;

                            // Create new array with one more element
                            var new_array = try self.allocator.alloc(token.TokenLiteral, current_array.len + 1);
                            errdefer self.allocator.free(new_array);

                            // Copy existing elements
                            @memcpy(new_array[0..current_array.len], current_array);

                            // Add new element
                            new_array[current_array.len] = arg_value;

                            // Free old array
                            self.allocator.free(current_array);

                            // Create new token literal with the new array
                            const new_value = token.TokenLiteral{ .array = new_array };

                            // Update the variable in the environment
                            if (field_access.object.* == .Variable) {
                                try self.environment.assign(field_access.object.Variable.lexeme, new_value);
                            }

                            return new_value;
                        }
                        if (std.mem.eql(u8, field_access.field.lexeme, "length")) {
                            return try self.arrayLength(field_access.object);
                        }
                        return error.UnknownMethod;
                    }

                    // If this is a namespace access, try to evaluate the full field access expression
                    // to resolve the function, then call it
                    if (object == .nothing) {
                        if (self.debug_enabled) {
                            std.debug.print("Found namespace access in Call, evaluating field access for function\n", .{});
                        }
                        // Create a temporary copy of the field access expression
                        const field_access_expr = try self.allocator.create(ast.Expr);
                        field_access_expr.* = .{ .FieldAccess = field_access };

                        // Evaluate the field access to get the function
                        const callee_fn = try self.evaluate(field_access_expr);
                        self.allocator.destroy(field_access_expr);

                        if (self.debug_enabled) {
                            std.debug.print("Field access evaluated, calling function\n", .{});
                        }

                        return self.callFunction(callee_fn, call.arguments);
                    }

                    // Handle struct methods or return error for other types
                    if (object != .struct_value) {
                        return error.NotAStruct;
                    }

                    // Handle struct methods here if needed
                    return error.MethodNotImplemented;
                }

                // Handle regular function calls
                const callee = try self.evaluate(call.callee);
                return self.callFunction(callee, call.arguments);
            },
            .Logical => |logical| {
                const left = try self.evaluate(logical.left);
                const right = try self.evaluate(logical.right);
                // Only allow boolean inputs
                if (left != .boolean or right != .boolean) return error.TypeError;

                const left_val = token.TokenLiteral{ .tetra = if (left.boolean == token.Boolean.true) .true else .false };
                const right_val = token.TokenLiteral{ .tetra = if (right.boolean == token.Boolean.true) .true else .false };

                // Store the tetra result in a comptime-known variable
                var result_tetra: token.Tetra = undefined;
                switch (logical.operator.type) {
                    .AND_KEYWORD, .AND_LOGICAL => {
                        result_tetra = switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => .false,
                            .both => if (right_val.tetra == .false) .false else .both,
                            .neither => .neither,
                        };
                    },
                    .OR_KEYWORD, .OR_LOGICAL => {
                        result_tetra = switch (left_val.tetra) {
                            .true => .true,
                            .false => right_val.tetra,
                            .both => .true,
                            .neither => .false,
                        };
                    },
                    .XOR => {
                        result_tetra = switch (left_val.tetra) {
                            .true => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .false => right_val.tetra,
                            .both => .both,
                            .neither => .neither,
                        };
                    },
                    .IFF => {
                        result_tetra = switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .both => .both,
                            .neither => .neither,
                        };
                    },
                    .NAND => {
                        result_tetra = switch (left_val.tetra) {
                            .true => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .false => .true,
                            .both => .both,
                            .neither => .neither,
                        };
                    },
                    .NOR => {
                        result_tetra = switch (left_val.tetra) {
                            .true => .false,
                            .false => switch (right_val.tetra) {
                                .true => .false,
                                .false => .true,
                                .both => .both,
                                .neither => .neither,
                            },
                            .both => .both,
                            .neither => .neither,
                        };
                    },
                    else => return error.InvalidOperator,
                }

                const is_true = result_tetra == .true;
                return token.TokenLiteral{ .boolean = if (is_true) token.Boolean.true else token.Boolean.false };
            },
            .Function => |f| token.TokenLiteral{ .function = .{
                .params = f.params,
                .body = f.body,
                .closure = self.environment,
            } },
            .Print => |print| {
                const value = try self.evaluate(print.expr);
                var buffer = std.ArrayList(u8).init(self.allocator);
                defer buffer.deinit();

                // Format the location information
                try buffer.writer().print("[{s}:{d}:{d}] {s} = ", .{
                    print.location.file,
                    print.location.line,
                    print.location.column,
                    print.variable_name orelse "value",
                });

                // Then format the value
                switch (value) {
                    .tetra => |t| try buffer.writer().print("{s}", .{@tagName(t)}),
                    .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                    .int => |i| try buffer.writer().print("{d}", .{i}),
                    .u8 => |u| try buffer.writer().print("{d}", .{u}),
                    .float => |f| try buffer.writer().print("{d}", .{f}),
                    .boolean => |b| try buffer.writer().print("{s}", .{@tagName(b)}),
                    .nothing => try buffer.writer().print("nothing", .{}),
                    .array => |arr| {
                        // Regular array printing logic
                        try buffer.writer().print("[", .{});
                        for (arr, 0..) |item, i| {
                            if (i > 0) try buffer.writer().print(", ", .{});
                            switch (item) {
                                .int => |n| try buffer.writer().print("{d}", .{n}),
                                .u8 => |u| try buffer.writer().print("{d}", .{u}),
                                .float => |f| try buffer.writer().print("{d}", .{f}),
                                .boolean => |b| try buffer.writer().print("{}", .{b}),
                                .string => |s| try buffer.writer().print("\"{s}\"", .{s}),
                                .array => |nested| {
                                    try buffer.writer().print("[", .{});
                                    for (nested, 0..) |nested_item, j| {
                                        if (j > 0) try buffer.writer().print(", ", .{});
                                        switch (nested_item) {
                                            .int => |n| try buffer.writer().print("{d}", .{n}),
                                            else => try buffer.writer().print("{any}", .{nested_item}),
                                        }
                                    }
                                    try buffer.writer().print("]", .{});
                                },
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
                                .u8 => |u| try buffer.writer().print("{d}", .{u}),
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
                                            .int => |n| try buffer.writer().print("{d}", .{n}),
                                            .float => |f| try buffer.writer().print("{d}", .{f}),
                                            .boolean => |b| try buffer.writer().print("{}", .{b}),
                                            .array => |nested| {
                                                try buffer.writer().print("[", .{});
                                                for (nested, 0..) |nested_item, j| {
                                                    if (j > 0) try buffer.writer().print(", ", .{});
                                                    try buffer.writer().print("{any}", .{nested_item});
                                                }
                                                try buffer.writer().print("]", .{});
                                            },
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
                    if (condition_result.boolean == token.Boolean.false) break;

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
                        if (condition_result.boolean == token.Boolean.false) break;
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
                var iter_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
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

                // Handle string length property
                if (object == .string) {
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        return token.TokenLiteral{ .int = @intCast(object.string.len) };
                    }
                    // Add bytes field access
                    if (std.mem.eql(u8, field.field.lexeme, "bytes")) {
                        var bytes = try self.allocator.alloc(token.TokenLiteral, object.string.len);
                        for (object.string, 0..) |byte, i| {
                            bytes[i] = token.TokenLiteral{ .u8 = byte }; // Use .u8 instead of .int
                        }
                        const array_value = token.TokenLiteral{ .array = bytes };
                        try self.environment.define(field.field.lexeme, array_value, .{ .base = .Array, .element_type = .U8, .is_mutable = true });
                        return array_value;
                    }
                }

                // Handle array properties first
                if (object == .array) {
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        return token.TokenLiteral{ .int = @intCast(object.array.len) };
                    }
                    return error.UnknownMethod;
                }

                // First check if this is an enum type access
                if (object == .nothing) {
                    // Try to get type info for the object
                    if (field.object.* == .Variable) {
                        // Check if this is a module namespace
                        const var_name = field.object.Variable.lexeme;
                        if (self.parser) |p| {
                            if (p.module_namespaces.contains(var_name)) {
                                // This is a module namespace, check for the field in imported symbols
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ var_name, field.field.lexeme });
                                defer self.allocator.free(full_name);

                                if (self.debug_enabled) {
                                    std.debug.print("Looking up imported symbol: {s}\n", .{full_name});

                                    // Debug: List all imported symbols
                                    if (p.imported_symbols) |symbols| {
                                        var it = symbols.iterator();
                                        std.debug.print("Available imported symbols:\n", .{});
                                        while (it.next()) |entry| {
                                            std.debug.print("  {s} (kind: {s})\n", .{ entry.key_ptr.*, @tagName(entry.value_ptr.kind) });
                                        }
                                    }
                                }

                                if (p.imported_symbols) |symbols| {
                                    if (symbols.get(full_name)) |symbol| {
                                        if (self.debug_enabled) {
                                            std.debug.print("Found imported symbol: {s} (kind: {s})\n", .{ full_name, @tagName(symbol.kind) });
                                        }

                                        // Create function value if it's a function
                                        if (symbol.kind == .Function) {
                                            // Load the function from the module
                                            const module_info = p.module_namespaces.get(var_name).?;

                                            // Find the function in the module's statements
                                            if (module_info.ast) |module_ast| {
                                                if (module_ast.* == .Block) {
                                                    for (module_ast.Block.statements) |stmt| {
                                                        if (stmt == .Function and
                                                            std.mem.eql(u8, stmt.Function.name.lexeme, field.field.lexeme))
                                                        {
                                                            if (self.debug_enabled) {
                                                                std.debug.print("Found function: {s} in module {s}\n", .{ field.field.lexeme, var_name });
                                                            }

                                                            // Create a function value
                                                            return token.TokenLiteral{
                                                                .function = .{
                                                                    .params = stmt.Function.params,
                                                                    .body = stmt.Function.body,
                                                                    .closure = self.environment,
                                                                },
                                                            };
                                                        }
                                                    }

                                                    if (self.debug_enabled) {
                                                        std.debug.print("Could not find function {s} in module statements\n", .{field.field.lexeme});
                                                        // Print all available functions in the module
                                                        std.debug.print("Available functions in module:\n", .{});
                                                        for (module_ast.Block.statements) |stmt| {
                                                            if (stmt == .Function) {
                                                                std.debug.print("  {s}\n", .{stmt.Function.name.lexeme});
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        } else if (symbol.kind == .Variable) {
                                            // Handle imported variables
                                            const module_info = p.module_namespaces.get(var_name).?;

                                            if (self.debug_enabled) {
                                                std.debug.print("Found variable: {s} in module {s}\n", .{ field.field.lexeme, var_name });
                                            }

                                            // Find the variable in the module's statements
                                            if (module_info.ast) |module_ast| {
                                                if (module_ast.* == .Block) {
                                                    for (module_ast.Block.statements) |stmt| {
                                                        if (stmt == .VarDecl and
                                                            std.mem.eql(u8, stmt.VarDecl.name.lexeme, field.field.lexeme))
                                                        {
                                                            if (self.debug_enabled) {
                                                                std.debug.print("Found variable declaration: {s}\n", .{field.field.lexeme});
                                                            }

                                                            // Evaluate the variable's initializer expression
                                                            if (stmt.VarDecl.initializer) |init_expr| {
                                                                return self.evaluate(init_expr);
                                                            } else {
                                                                // If no initializer, return a default value based on type
                                                                return token.TokenLiteral{ .nothing = {} };
                                                            }
                                                        }
                                                    }

                                                    if (self.debug_enabled) {
                                                        std.debug.print("Could not find variable {s} in module statements\n", .{field.field.lexeme});
                                                    }
                                                }
                                            }
                                        }

                                        // For other types, we might need different handling
                                        return token.TokenLiteral{ .nothing = {} };
                                    } else if (self.debug_enabled) {
                                        std.debug.print("Symbol {s} not found in imported symbols\n", .{full_name});
                                    }
                                }

                                // If we get here but we're still a namespace access, treat it specially
                                // This happens when the symbol exists but wasn't properly registered
                                if (self.debug_enabled) {
                                    std.debug.print("Trying direct module access for {s}.{s}\n", .{ var_name, field.field.lexeme });
                                }

                                // Try to find the function directly in the module
                                const module_info = p.module_namespaces.get(var_name).?;
                                if (module_info.ast) |module_ast| {
                                    if (module_ast.* == .Block) {
                                        for (module_ast.Block.statements) |stmt| {
                                            if (stmt == .Function and
                                                std.mem.eql(u8, stmt.Function.name.lexeme, field.field.lexeme))
                                            {
                                                if (self.debug_enabled) {
                                                    std.debug.print("Found function by direct lookup: {s}\n", .{field.field.lexeme});
                                                }

                                                // Create a function value
                                                return token.TokenLiteral{
                                                    .function = .{
                                                        .params = stmt.Function.params,
                                                        .body = stmt.Function.body,
                                                        .closure = self.environment,
                                                    },
                                                };
                                            }
                                        }
                                    }
                                }

                                // Still not found, this is a module but the symbol doesn't exist
                                return error.UndefinedProperty;
                            }
                        }

                        // Continue with regular type info check
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

                    // If we reach here, it's a .nothing value but not a namespace or enum
                    // This is not a valid struct, so return an error
                    return error.InvalidFieldAccess;
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
                    .struct_fields = try self.allocator.alloc(ast.StructFieldType, decl.fields.len),
                };

                for (decl.fields, 0..) |field, i| {
                    struct_type.struct_fields.?[i] = .{
                        .name = field.name.lexeme,
                        .type_info = try ast.typeInfoFromExpr(self.allocator, field.type_expr),
                    };
                }

                try self.environment.define(decl.name.lexeme, .{ .nothing = {} }, struct_type);
                return .{ .nothing = {} };
            },
            .StructLiteral => |literal| {
                var struct_fields = std.ArrayList(token.StructField).init(self.allocator);
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
            .FieldAssignment => |assign| {
                // Evaluate the RHS first to get the value
                const value = try self.evaluate(assign.value);

                // Check if the object is a variable representing a namespace
                if (assign.object.* == .Variable) {
                    const namespace_name = assign.object.Variable.lexeme;
                    // Check if the parser context is available and contains the namespace
                    if (self.parser) |p| {
                        if (p.module_namespaces.contains(namespace_name)) {
                            // This is an assignment to a variable within a namespace
                            const field_name = assign.field.lexeme;
                            // Construct the fully qualified name (e.g., "math.ticker")
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace_name, field_name });
                            defer self.allocator.free(full_name);

                            if (self.debug_enabled) {
                                std.debug.print("Attempting assignment to imported variable: {s} = {any}\n", .{ full_name, value });
                            }

                            // Check if the global variable exists and get its type info.
                            // Public symbols are registered globally, so we check self.globals.
                            const global_type_info = self.globals.getTypeInfo(full_name) catch |err| {
                                if (self.debug_enabled) {
                                    std.debug.print("Error looking up TypeInfo for imported var '{s}': {}\n", .{ full_name, err });
                                }
                                // Return specific errors or UndefinedVariable if not found globally
                                return switch (err) {
                                    error.UndefinedVariable => error.UndefinedVariable,
                                    else => err, // Propagate other errors like OutOfMemory
                                };
                            };

                            // Check mutability (was it declared 'var' or 'const' in the module?)
                            if (!global_type_info.is_mutable) {
                                if (self.debug_enabled) {
                                    std.debug.print("Error: Cannot assign to imported constant {s}\n", .{full_name});
                                }
                                return error.ConstAssignment; // Cannot assign to immutable variable
                            }

                            // Optional but recommended: Add type checking
                            if (!global_type_info.is_dynamic) {
                                const value_tag_name = @tagName(value);
                                const expected_tag_name: ?[]const u8 = switch (global_type_info.base) {
                                    .Int => "int",
                                    .Float => "float",
                                    .String => "string",
                                    .Boolean => "boolean",
                                    // Add cases for other types as needed (Array, U8, etc.)
                                    else => null, // Allow assignment if type is complex or not checked
                                };
                                // Check if types match (simple tag check for now)
                                if (expected_tag_name != null and !std.mem.eql(u8, value_tag_name, expected_tag_name)) {
                                    if (self.debug_enabled) {
                                        std.debug.print("Type mismatch assigning to {s}. Expected {s}, got {s}.\n", .{ full_name, expected_tag_name, value_tag_name });
                                    }
                                    return error.TypeError; // Type mismatch
                                }
                            }

                            // Assign the value in the *global* environment using the full name
                            try self.globals.assign(full_name, value);
                            if (self.debug_enabled) {
                                std.debug.print("Assigned imported variable {s} = {any} in global scope\n", .{ full_name, value });
                            }
                            return value; // Return the assigned value successfully
                        }
                    }
                }

                // --- If it wasn't a namespace assignment, proceed with other possibilities (like struct fields) ---
                const object = try self.evaluate(assign.object);
                switch (object) {
                    .struct_value => |*struct_val| {
                        // --- Struct Field Assignment ---
                        // This requires finding the original struct variable in the environment,
                        // modifying its field, and potentially assigning the modified struct back.
                        // This is complex and not fully implemented here.
                        if (self.debug_enabled) {
                            std.debug.print("Mutable struct field assignment is not yet fully supported.\n", .{});
                        }
                        // Placeholder: Return error until proper implementation
                        return error.NotImplemented;

                        // // --- Future Implementation Sketch ---
                        // // Find the field in the evaluated struct copy
                        // for (struct_val.fields) |*field| {
                        //     if (std.mem.eql(u8, field.name, assign.field.lexeme)) {
                        //         // TODO: Check field mutability based on struct definition if possible
                        //         field.value = value; // Modify the temporary copy
                        //
                        //         // Find the original struct variable and update it in the environment
                        //         if (assign.object.* == .Variable) {
                        //             // Need Environment.assignStructField(varName, fieldName, newValue) or similar
                        //             // try self.environment.assign(assign.object.Variable.lexeme, object); // This assigns the modified *copy* back
                        //         } else {
                        //            // Handle assignment through nested field access, etc.
                        //         }
                        //         return value;
                        //     }
                        // }
                        // return error.FieldNotFound; // Field not found in struct
                    },
                    else => {
                        // If it's not a namespace variable or a struct, assignment is invalid
                        if (self.debug_enabled) {
                            std.debug.print("Error: Cannot assign to field of type {s}. Expected namespace or struct.\n", .{@tagName(object)});
                        }
                        return error.InvalidAssignmentTarget;
                    },
                }
            },
            .Exists => |e| {
                if (self.debug_enabled) {
                    std.debug.print("Evaluating exists expression\n", .{});
                    std.debug.print("Variable name: {s}\n", .{e.variable.lexeme});
                }

                // Evaluate the array expression in the current environment
                // BEFORE creating the new quantifier environment
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

                // Store the current environment to restore it later
                const prev_env = self.environment;
                defer self.environment = prev_env;

                // Create a new environment for the quantifier scope
                var quantifier_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                self.environment = &quantifier_env;

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
                    if (result == .boolean and result.boolean == token.Boolean.true) {
                        return token.TokenLiteral{ .boolean = token.Boolean.true };
                    }
                }

                return token.TokenLiteral{ .boolean = token.Boolean.false };
            },
            .ForAll => |f| {
                // Get the variable name
                const var_name = f.variable.lexeme;

                // Evaluate the array expression in the current environment first
                const array_value = try self.evaluate(f.array);
                if (array_value != .array) {
                    return error.TypeError;
                }

                // Store the current environment to restore it later
                const prev_env = self.environment;
                defer self.environment = prev_env;

                // Create a new environment for the quantifier scope
                var quantifier_env = Environment.init(self.allocator, self.environment, self.debug_enabled);
                self.environment = &quantifier_env;

                // Iterate over the actual array elements
                for (array_value.array) |val| {
                    try self.environment.define(var_name, val, .{ .base = .Dynamic });
                    const result = try self.evaluate(f.condition);
                    if (!(result == .boolean and result.boolean == token.Boolean.true)) {
                        return token.TokenLiteral{ .boolean = token.Boolean.false };
                    }
                }

                return token.TokenLiteral{ .boolean = token.Boolean.true };
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
                    .variants = try self.allocator.alloc([]const u8, decl.variants.len),
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
                        .u8 => "u8",
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
                var tuple_values = std.ArrayList(token.TokenLiteral).init(self.allocator);
                errdefer tuple_values.deinit();

                // Evaluate each element in the tuple
                for (elements) |element| {
                    const value = try self.evaluate(element);
                    try tuple_values.append(value);
                }

                const owned_slice = try tuple_values.toOwnedSlice();
                errdefer self.allocator.free(owned_slice);

                return token.TokenLiteral{ .tuple = owned_slice };
            },
            .Map => |entries| {
                var map = std.StringHashMap(token.TokenLiteral).init(self.allocator);
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
            .MethodCall => |method_call| {
                if (std.mem.eql(u8, method_call.method.lexeme, "length")) {
                    const receiver_value = try self.evaluate(method_call.receiver);
                    if (receiver_value != .array) {
                        return error.TypeError;
                    }
                    return token.TokenLiteral{ .int = @intCast(receiver_value.array.len) };
                }
                return try self.callMethod(.{
                    .receiver = method_call.receiver,
                    .method = method_call.method,
                    .arguments = method_call.arguments,
                });
            },
            .ArrayPush => |ap| {
                return try self.arrayPush(ap.array, ap.element);
            },
            .ArrayLength => |al| {
                const array_value = try self.evaluate(al.array);
                if (array_value != .array) {
                    return error.TypeError;
                }
                return token.TokenLiteral{ .int = @intCast(array_value.array.len) };
            },
            .ArrayPop => |ap| {
                return try self.arrayPop(ap.array);
            },
            .ArrayIsEmpty => |ae| {
                return try self.arrayIsEmpty(ae.array);
            },
            .ArrayConcat => |ac| {
                return try self.arrayConcat(ac.array, ac.array2);
            },
            .Input => |input| {
                if (input.prompt.lexeme.len > 0) {
                    const prompt = if (input.prompt.lexeme[0] == '"' and
                        input.prompt.lexeme[input.prompt.lexeme.len - 1] == '"')
                        input.prompt.lexeme[1 .. input.prompt.lexeme.len - 1]
                    else
                        input.prompt.lexeme;
                    try std.io.getStdOut().writer().print("{s}", .{prompt});
                }

                const stdin = std.io.getStdIn().reader();
                var buffer = std.ArrayList(u8).init(self.allocator);
                defer buffer.deinit();

                // Read until newline
                while (true) {
                    const byte = stdin.readByte() catch |err| switch (err) {
                        error.EndOfStream => break,
                        else => return err,
                    };
                    if (byte == '\n') break;
                    if (byte == '\r') {
                        // If we see a \r, check for and consume a following \n
                        const next_byte = stdin.readByte() catch |err| switch (err) {
                            error.EndOfStream => break,
                            else => return err,
                        };
                        if (next_byte != '\n') {
                            try buffer.append(next_byte);
                        }
                        break;
                    }
                    try buffer.append(byte);
                }

                const input_str = try buffer.toOwnedSlice();
                return token.TokenLiteral{ .string = input_str };
            },
        };
    }

    fn makeNothing(self: *Interpreter) token.TokenLiteral {
        _ = self;
        return .{ .nothing = {} };
    }

    fn convertToTetra(value: token.TokenLiteral) !token.Tetra {
        return switch (value) {
            .boolean => {
                if (value.boolean == token.Boolean.true) {
                    return token.Tetra.true;
                } else {
                    return token.Tetra.false;
                }
            },
            .tetra => value.tetra,
            else => error.TypeError,
        };
    }

    fn negateLogical(value: token.TokenLiteral) ErrorList!token.TokenLiteral {
        return switch (value) {
            .boolean => if (value.boolean == token.Boolean.true)
                token.TokenLiteral{ .boolean = token.Boolean.false }
            else
                token.TokenLiteral{ .boolean = token.Boolean.true },
            .tetra => switch (value.tetra) {
                .neither => token.TokenLiteral{ .tetra = .both },
                .both => token.TokenLiteral{ .tetra = .neither },
                .true => token.TokenLiteral{ .tetra = .false },
                .false => token.TokenLiteral{ .tetra = .true },
            },
            else => return error.TypeError,
        };
    }

    fn orLogical(left: token.TokenLiteral, right: token.TokenLiteral) !token.TokenLiteral {
        const left_tetra = try convertToTetra(left);
        const right_tetra = try convertToTetra(right);
        if (left_tetra == .neither or right_tetra == .neither) {
            return token.TokenLiteral{ .boolean = token.Boolean.false };
        }
        if (left_tetra == .both or right_tetra == .both) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        }
        if (left_tetra == .true or right_tetra == .true) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        }
        return token.TokenLiteral{ .boolean = token.Boolean.false };
    }

    fn compareLogical(left: token.TokenLiteral, right: token.TokenLiteral) !token.TokenLiteral {
        // convert left and right to tetra
        const left_tetra = try convertToTetra(left);
        const right_tetra = try convertToTetra(right);

        // neither cancels all other cases
        if (left_tetra == .neither or right_tetra == .neither) {
            return token.TokenLiteral{ .boolean = token.Boolean.false };
        }
        // if both an no neither then true
        if (left_tetra == .both or right_tetra == .both) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        }
        // normal cases
        if (left_tetra == .true and right_tetra == .true) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        } else if (left_tetra == .false and right_tetra == .false) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        }

        return token.TokenLiteral{ .boolean = token.Boolean.false };
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
                var function_env = Environment.init(self.allocator, f.closure, self.debug_enabled);
                defer function_env.deinit();

                // Check argument count
                if (arguments.len > f.params.len) {
                    return error.TooManyArguments;
                }

                // Evaluate and bind arguments to parameters
                for (f.params, 0..) |param, i| {
                    var value: token.TokenLiteral = undefined;

                    if (i < arguments.len) {
                        const arg = arguments[i];
                        if (arg.* == .DefaultArgPlaceholder) {
                            if (param.default_value) |default| {
                                if (self.debug_enabled) {
                                    std.debug.print("Using default value for parameter '{s}'\n", .{param.name.lexeme});
                                }
                                const previous_env = self.environment;
                                self.environment = f.closure;
                                value = try self.evaluate(default);
                                self.environment = previous_env;
                            } else {
                                return error.NoDefaultValue;
                            }
                        } else {
                            value = try self.evaluate(arg);
                        }
                    } else if (param.default_value) |default| {
                        if (self.debug_enabled) {
                            std.debug.print("Using default value for parameter '{s}'\n", .{param.name.lexeme});
                        }
                        const previous_env = self.environment;
                        self.environment = f.closure;
                        value = try self.evaluate(default);
                        self.environment = previous_env;
                    } else {
                        return error.TooFewArguments;
                    }

                    if (self.debug_enabled) {
                        std.debug.print("Binding parameter '{s}' = {any}\n", .{ param.name.lexeme, value });
                    }

                    // Convert TypeExpr to TypeInfo if available, otherwise use dynamic type
                    var type_info: ast.TypeInfo = undefined;
                    if (param.type_expr) |te| {
                        const ptr = try ast.typeInfoFromExpr(self.allocator, te);
                        defer self.allocator.destroy(ptr);
                        type_info = ptr.*;
                        if (self.debug_enabled) {
                            std.debug.print("Parameter '{s}' type: {any}\n", .{ param.name.lexeme, type_info });
                        }
                    } else {
                        type_info = .{ .base = .Dynamic, .is_dynamic = true, .is_mutable = true };
                    }

                    // Define parameter only in function environment
                    try function_env.define(param.name.lexeme, value, type_info);
                    if (self.debug_enabled) {
                        std.debug.print("Defined parameter '{s}' in function environment\n", .{param.name.lexeme});
                    }
                }

                // Store current environment and switch to function environment
                const previous_env = self.environment;
                self.environment = &function_env;

                // Execute function body
                const result = self.executeBlock(f.body, &function_env) catch |err| {
                    self.environment = previous_env;
                    if (err == error.ReturnValue) {
                        if (try function_env.get("return")) |return_value| {
                            // Deep copy the return value to preserve array contents
                            return switch (return_value) {
                                .array => |arr| blk: {
                                    var new_array = try self.allocator.alloc(token.TokenLiteral, arr.len);
                                    for (arr, 0..) |item, i| {
                                        new_array[i] = switch (item) {
                                            .int => |n| token.TokenLiteral{ .int = n },
                                            .boolean => |b| token.TokenLiteral{ .boolean = b },
                                            .array => |a| blk2: {
                                                var inner_array = try self.allocator.alloc(token.TokenLiteral, a.len);
                                                for (a, 0..) |inner_item, j| {
                                                    inner_array[j] = inner_item;
                                                }
                                                break :blk2 token.TokenLiteral{ .array = inner_array };
                                            },
                                            else => item,
                                        };
                                    }
                                    break :blk token.TokenLiteral{ .array = new_array };
                                },
                                else => return_value,
                            };
                        }
                    }
                    return err;
                };

                // Restore previous environment
                self.environment = previous_env;

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

    fn compareLiteralValues(a: anytype, b: @TypeOf(a)) token.TokenLiteral {
        if (a == b) {
            return token.TokenLiteral{ .boolean = token.Boolean.true };
        } else {
            return token.TokenLiteral{ .boolean = token.Boolean.false };
        }
    }

    fn valuesEqual(self: *Interpreter, a: token.TokenLiteral, b: token.TokenLiteral) ErrorList!bool {
        return switch (a) {
            .int => |val| b == .int and val == b.int,
            .u8 => |val| b == .u8 and val == b.u8,
            .float => |val| b == .float and val == b.float,
            .string => |val| b == .string and std.mem.eql(u8, val, b.string),
            .nothing => b == .nothing,
            .boolean, .tetra => {
                const result = try compareLogical(a, b);
                return result.boolean == token.Boolean.true;
            },
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
                        const values_equal = try self.valuesEqual(entry.value_ptr.*, other_value);
                        if (!values_equal) {
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

    const MethodCallExpr = struct {
        receiver: *ast.Expr,
        method: token.Token,
        arguments: []const *ast.Expr,
    };

    fn callMethod(self: *Interpreter, method_call: MethodCallExpr) ErrorList!token.TokenLiteral {
        // First evaluate the receiver to get the actual array
        const receiver_value = try self.evaluate(method_call.receiver);

        // Handle array methods
        if (receiver_value == .array) {
            if (std.mem.eql(u8, method_call.method.lexeme, "push")) {
                // Verify argument count
                if (method_call.arguments.len != 1) {
                    return error.InvalidArgumentCount;
                }

                // Evaluate the argument
                const arg_value = try self.evaluate(method_call.arguments[0]);

                // Get the current array
                const current_array = receiver_value.array;

                // Create new array with one more element
                var new_array = try self.allocator.alloc(token.TokenLiteral, current_array.len + 1);
                errdefer self.allocator.free(new_array);

                // Copy existing elements
                @memcpy(new_array[0..current_array.len], current_array);

                // Add new element
                new_array[current_array.len] = arg_value;

                // Free old array
                self.allocator.free(current_array);

                // Create new token literal with the new array
                const new_value = token.TokenLiteral{ .array = new_array };

                // Update the variable in the environment
                if (method_call.receiver.* == .Variable) {
                    try self.environment.assign(method_call.receiver.Variable.lexeme, new_value);
                }

                return new_value;
            }
            if (std.mem.eql(u8, method_call.method.lexeme, "length")) {
                return try self.arrayLength(method_call.receiver);
            }
            // Add other array methods here (pop, length, etc.)
            return error.UnknownMethod;
        }

        // Handle string methods
        if (receiver_value == .string) {
            if (std.mem.eql(u8, method_call.method.lexeme, "length")) {
                return token.TokenLiteral{ .int = @intCast(receiver_value.string.len) };
            }
        }

        // Handle other types' methods here
        return error.MethodNotFound;
    }

    fn arrayIsEmpty(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        const array_value = try self.evaluate(array_expr);
        return token.TokenLiteral{ .boolean = if (array_value.array.len == 0)
            token.Boolean.true
        else
            token.Boolean.false };
    }

    fn arrayPop(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        const array_value = try self.evaluate(array_expr);
        if (array_value != .array) {
            return error.TypeError;
        }

        const current_array = array_value.array;
        if (current_array.len == 0) {
            return error.EmptyArray;
        }

        // Get the last element
        const popped_value = current_array[current_array.len - 1];

        // Create new array with one less element
        var new_array = try self.allocator.alloc(token.TokenLiteral, current_array.len - 1);
        errdefer self.allocator.free(new_array);

        // Copy all elements except the last one
        @memcpy(new_array[0..(current_array.len - 1)], current_array[0..(current_array.len - 1)]);

        // Update the array in the environment if it's a variable
        if (array_expr.* == .Variable) {
            try self.environment.assign(array_expr.Variable.lexeme, .{ .array = new_array });
        }

        // Free old array
        self.allocator.free(current_array);

        // Return the popped value
        return popped_value;
    }

    fn arrayLength(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!token.TokenLiteral {
        const array_value = try self.evaluate(array_expr);
        if (array_value != .array) {
            return error.TypeError;
        }
        return token.TokenLiteral{ .int = @intCast(array_value.array.len) };
    }

    fn arrayConcat(self: *Interpreter, array: *ast.Expr, array2: *ast.Expr) ErrorList!token.TokenLiteral {
        const array_value = try self.evaluate(array);
        const array2_value = try self.evaluate(array2);

        // Create new array with combined length
        var new_array = try self.allocator.alloc(token.TokenLiteral, array_value.array.len + array2_value.array.len);
        errdefer self.allocator.free(new_array);

        // Copy first array
        @memcpy(new_array[0..array_value.array.len], array_value.array);

        // Copy second array
        @memcpy(new_array[array_value.array.len..], array2_value.array);

        return token.TokenLiteral{ .array = new_array };
    }

    fn arrayPush(self: *Interpreter, array: *ast.Expr, element: *ast.Expr) ErrorList!token.TokenLiteral {
        // Create a method call expression using the PUSH token
        const method_call = MethodCallExpr{
            .receiver = array,
            .method = .{
                .type = .IDENTIFIER,
                .lexeme = "push",
                .literal = .{ .nothing = {} }, // Use .nothing instead of null
                .line = 0, // Since this is synthetic, we use 0
                .column = 0, // Since this is synthetic, we use 0
            },
            .arguments = &[_]*ast.Expr{element},
        };

        // Use the existing callMethod implementation
        return try self.callMethod(method_call);
    }
};
