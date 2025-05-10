const std = @import("std");
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const token = @import("../lexer/token.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const Reporter = Reporting.Reporter;
const Memory = @import("../utils/memory.zig");
const Scope = Memory.Scope;
const MemoryManager = Memory.MemoryManager;
const Parser = @import("../parser/parser_types.zig").Parser;

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
            std.debug.print("\n=== Environment Define ===\n", .{});
            std.debug.print("Environment: {*}\n", .{self});
            std.debug.print("Key: '{s}'\n", .{key});
            std.debug.print("Value type: {s}\n", .{@tagName(value)});
            std.debug.print("Raw value: {any}\n", .{value});
            if (value == .array) {
                std.debug.print("Array contents:\n", .{});
                for (value.array, 0..) |item, i| {
                    std.debug.print("  [{d}] Type={s}, Raw={any}\n", .{ i, @tagName(item), item });
                    if (item == .string) {
                        std.debug.print("    String content: '{s}'\n", .{item.string});
                    }
                }
            }
        }

        try self.values.put(key, value);
        try self.types.put(key, type_info);

        if (self.debug_enabled) {
            std.debug.print("Value stored successfully\n", .{});
            if (try self.get(key)) |stored| {
                std.debug.print("Verification - stored value: {any}\n", .{stored});
                if (stored == .array) {
                    std.debug.print("Stored array contents:\n", .{});
                    for (stored.array, 0..) |item, i| {
                        std.debug.print("  [{d}] Type={s}, Raw={any}\n", .{ i, @tagName(item), item });
                        if (item == .string) {
                            std.debug.print("    String content: '{s}'\n", .{item.string});
                        }
                    }
                }
            }
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
    environment: *Environment,
    global_environment: *Environment,
    parser: ?*Parser,
    allocator: std.mem.Allocator,
    runtime_errors: std.ArrayList(Reporter.RuntimeError),
    debug_enabled: bool,
    string_interner: StringInterner,
    moduleEnvironments: std.StringHashMap(*Environment),
    memory_manager: *MemoryManager,

    return_value: ?token.TokenLiteral = null,
    entry_point_name: ?[]const u8 = null,
    last_result: ?token.TokenLiteral = null,
    had_error: bool = false,
    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,
    has_returned: bool = false,

    pub fn init(allocator: std.mem.Allocator, environment: *Environment, parser: ?*Parser, debug_enabled: bool, memory_manager: *MemoryManager) Interpreter {
        // Initialize scope_manager with a root scope
        if (memory_manager.scope_manager.root_scope == null) {
            memory_manager.scope_manager.root_scope = Scope.init(memory_manager.scope_manager, memory_manager.scope_manager.next_storage_id, null) catch unreachable;
            memory_manager.scope_manager.next_storage_id += 1;
        }

        return .{
            .environment = environment,
            .global_environment = environment,
            .parser = parser,
            .allocator = allocator,
            .runtime_errors = std.ArrayList(Reporter.RuntimeError).init(allocator),
            .debug_enabled = debug_enabled,
            .has_returned = false,
            .entry_point_name = null,
            .string_interner = StringInterner.init(allocator),
            .moduleEnvironments = std.StringHashMap(*Environment).init(allocator),
            .memory_manager = memory_manager,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.runtime_errors.deinit();
        self.string_interner.deinit();

        // Free up all module environments
        var it = self.moduleEnvironments.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.moduleEnvironments.deinit();

        // memory_manager will handle scope cleanup
        // reset our root_scope pointer to guard against double-free
        self.memory_manager.scope_manager.root_scope = null;
    }

    fn u8BoundsCheck(value: token.TokenLiteral) ErrorList!void {
        if (value.int < 0) {
            var reporting = Reporter.init();
            reporting.reportRuntimeError("Underflow: u8 cannot be negative", .{});
            return error.u8Underflow;
        }
        if (value.int > 255) {
            var reporting = Reporter.init();
            reporting.reportRuntimeError("Overflow: u8 cannot be greater than 255", .{});
            return error.u8Overflow;
        }
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

        // Second pass - Process all function declarations
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
                    .{ // Use default TypeInfo for function, actual types checked during call
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

        // Third pass - Process all variable declarations
        var var_declarations = std.ArrayList(*ast.Stmt).init(self.allocator);
        defer var_declarations.deinit();

        for (0..statements.len) |i| {
            const stmt = &statements[i];
            if (self.debug_enabled) {
                std.debug.print("\nScanning for variables - statement {d}: {s}\n", .{ i, @tagName(stmt.*) });
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

        // --- Final Execution Pass ---
        if (self.entry_point_name == null) {
            // Script mode - execute all remaining statements
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in script mode (final execution) ===\n", .{});
            }
            for (statements) |*stmt| {
                // Skip declarations handled in previous passes
                if (stmt.* == .Function or stmt.* == .VarDecl or stmt.* == .EnumDecl or
                    (stmt.* == .Expression and stmt.Expression != null and stmt.Expression.?.* == .StructDecl))
                {
                    continue;
                }
                self.last_result = try self.executeStatement(stmt, self.debug_enabled);
            }
        } else {
            // Program mode with entry point
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in program mode (executing entry point) ===\n", .{});
            }

            // Entry point execution (declarations already handled)
            if (self.entry_point_name) |main_fn| {
                const main_value = (try self.environment.get(main_fn)) orelse return error.InvalidEntryPoint;
                if (main_value != .function) {
                    return error.InvalidEntryPoint;
                }

                var empty_args = [_]*ast.Expr{}; // Create empty slice of ast.Expr pointers
                self.last_result = try self.callFunction(main_value, &empty_args);
            } else {
                // This case should technically not be reachable if entry_point_name is set
                // but included for completeness
                if (self.debug_enabled) {
                    std.debug.print("Warning: Program mode set but no entry point name found.\n", .{});
                }
            }
        }

        // // Handle entry point execution (removed from here, handled in program mode logic)
        // if (self.entry_point_name) |main_fn| { ... }

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

        // Reset return flag at start of block (unless we're in a nested call)
        const previous_return_state = self.has_returned;
        defer self.has_returned = previous_return_state;

        // Execute all statements, respecting return status
        for (statements) |*stmt| {
            // Skip statements if we've already returned
            if (self.has_returned) break;

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

                        // Set the return flag
                        self.has_returned = true;
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

                // Set the return flag
                self.has_returned = true;
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
                    if (self.debug_enabled) {
                        std.debug.print("\nHandling array declaration for {s}\n", .{decl.name.lexeme});
                        std.debug.print("Raw array type info: {any}\n", .{decl.type_info});
                    }

                    // For u8 arrays, create a proper type info with element type
                    var type_info = decl.type_info;
                    if (decl.type_info.array_type != null) {
                        // Check if this is a u8 array
                        if (decl.type_info.array_type.?.base == .U8) {
                            if (self.debug_enabled) {
                                std.debug.print("Detected u8 array\n", .{});
                            }
                            // Create a new type info with U8 element type
                            type_info = ast.TypeInfo{
                                .base = .Array,
                                .is_dynamic = false,
                                .is_mutable = true,
                                .element_type = .U8,
                                .array_type = decl.type_info.array_type,
                                .array_size = decl.type_info.array_size,
                            };
                        }
                    }

                    if (self.debug_enabled) {
                        std.debug.print("Using type info: {any}\n", .{type_info});
                    }
                }

                const value = if (decl.initializer) |i| blk: {
                    const init_value = try self.evaluate(i);

                    // Type checking for initialization
                    switch (decl.type_info.base) {
                        .Int => if (init_value != .int and init_value != .u8) {
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize int variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .U8 => {
                            if (init_value != .u8 and init_value != .int) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot initialize u8 variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }

                            // Convert int to u8 if needed
                            if (init_value == .int) {
                                if (init_value.int < 0 or init_value.int > 255) {
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Type error: Integer value {d} out of range for u8", .{init_value.int});
                                    return error.TypeError;
                                }
                                break :blk token.TokenLiteral{ .u8 = @intCast(init_value.int) };
                            }
                        },
                        .Float => if (init_value != .float and init_value != .int) {
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize float variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Boolean => if (init_value != .boolean) {
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("Type error: Cannot initialize bool variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Array => {
                            if (init_value != .array) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot initialize array variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }

                            // Check array element types if element_type is specified
                            if (decl.type_info.element_type) |expected_type| {
                                // If this is a u8 array, convert int elements to u8
                                if (expected_type == .U8) {
                                    var new_array = try self.allocator.alloc(token.TokenLiteral, init_value.array.len);
                                    for (init_value.array, 0..) |elem, idx| {
                                        if (elem == .int) {
                                            if (elem.int < 0 or elem.int > 255) {
                                                var reporting = Reporter.init();
                                                reporting.reportRuntimeError("Type error: Integer value {d} at index {d} out of range for u8 array", .{ elem.int, idx });
                                                self.allocator.free(new_array);
                                                return error.TypeError;
                                            }
                                            new_array[idx] = token.TokenLiteral{ .u8 = @intCast(elem.int) };
                                        } else if (elem == .u8) {
                                            new_array[idx] = elem;
                                        } else {
                                            var reporting = Reporter.init();
                                            reporting.reportRuntimeError("Type error: Cannot convert {s} at index {d} to u8", .{ @tagName(elem), idx });
                                            self.allocator.free(new_array);
                                            return error.TypeError;
                                        }
                                    }

                                    // Free the original array
                                    self.allocator.free(init_value.array);

                                    // Return the new array with proper u8 elements
                                    break :blk token.TokenLiteral{ .array = new_array };
                                }

                                // For other array types, check that elements match
                                for (init_value.array) |element| {
                                    const matches = switch (expected_type) {
                                        .Int => element == .int,
                                        .U8 => element == .u8,
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
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot initialize map variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }
                        },
                        .Auto => {},
                        else => {},
                    }
                    break :blk init_value;
                } else switch (decl.type_info.base) {
                    .Int => token.TokenLiteral{ .int = 0 },
                    .U8 => token.TokenLiteral{ .u8 = 0 },
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

                // Use the specialized type_info for u8 arrays if available
                var final_type_info = decl.type_info;
                if (decl.type_info.base == .Array and
                    decl.type_info.array_type != null and
                    decl.type_info.array_type.?.base == .U8)
                {
                    if (self.debug_enabled) {
                        std.debug.print("Using specialized u8 array type info\n", .{});
                    }

                    final_type_info = ast.TypeInfo{
                        .base = .Array,
                        .is_dynamic = false,
                        .is_mutable = true,
                        .element_type = .U8,
                        .array_type = decl.type_info.array_type,
                        .array_size = decl.type_info.array_size,
                    };
                }

                try self.environment.define(decl.name.lexeme, value, final_type_info);
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
            .Assert => |assert| {
                const condition = try self.evaluate(assert.condition);
                if (condition != .boolean) {
                    var reporter = Reporter.init();
                    reporter.reportRuntimeError("Assertion failed: {s} is not a boolean", .{@tagName(condition)});
                    return error.TypeError;
                }
                if (condition.boolean == token.Boolean.false) {
                    var reporter = Reporter.init();

                    // Use custom message if provided
                    if (assert.message != null) {
                        const message_value = try self.evaluate(assert.message.?);
                        if (message_value == .string) {
                            reporter.reportCompileError(assert.location, "Assertion failed: {s}", .{message_value.string});
                        } else {
                            reporter.reportCompileError(assert.location, "Assertion failed", .{});
                        }
                    } else {
                        reporter.reportCompileError(assert.location, "Assertion failed", .{});
                    }
                    return error.AssertionFailed;
                }
                return .{ .nothing = {} };
            },
            .Module => return .{ .nothing = {} },
            .Import => return .{ .nothing = {} },
            .Path => return .{ .nothing = {} },
            .Continue => return .{ .nothing = {} },
            .Break => return .{ .nothing = {} },
            .Return => |ret| {
                if (ret.value) |value| {
                    const return_value = try self.evaluate(value);
                    try self.environment.define("return", return_value, .{ .base = .Auto });
                    self.has_returned = true; // Set the flag
                    return error.ReturnValue;
                }
                try self.environment.define("return", .{ .nothing = {} }, .{ .base = .Nothing });
                self.has_returned = true; // Set the flag
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
                        if (left == .string and right == .string) {
                            const result = try std.mem.concat(self.allocator, u8, &.{ left.string, right.string });
                            return token.TokenLiteral{ .string = result };
                        }
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
                        if (left == .string and right == .string) {
                            if (!std.mem.eql(u8, left.string, right.string)) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) != 0) {
                                return token.TokenLiteral{ .boolean = token.Boolean.true };
                            } else {
                                return token.TokenLiteral{ .boolean = token.Boolean.false };
                            }
                        }
                        var reporter = Reporter.init();
                        const location: Reporter.Location = .{ .file = "stdin", .line = binary.operator.line, .column = binary.operator.column };
                        reporter.reportCompileError(location, "Cannot compare {s} and {s}. Both sides must be bools.", .{ @tagName(left), @tagName(right) });
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
                var reporter = Reporter.init();
                const location: Reporter.Location = .{ .file = "stdin", .line = var_token.line, .column = var_token.column };
                reporter.reportCompileError(location, "Variable '{s}' not found", .{var_token.lexeme});
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
                            if (value != .int and value != .u8) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .U8 => {
                            if (value != .u8 and value != .int) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to u8 variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Float => {
                            if (value != .float) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .String => {
                            if (value != .string) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Boolean => {
                            if (value != .boolean) {
                                var reporting = Reporter.init();
                                reporting.reportRuntimeError("Type error: Cannot assign {s} to boolean variable", .{@tagName(value)});
                                return error.TypeError;
                            }
                        },
                        .Auto => {}, // Type is already fixed from initialization
                        else => {
                            var reporting = Reporter.init();
                            const location: Reporter.Location = .{ .file = "stdin", .line = assign.name.line, .column = assign.name.column };
                            reporting.reportCompileError(location, "Type error: Cannot assign {s} to variable", .{@tagName(value)});
                            return error.TypeError;
                        },
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
                        .u8 => switch (rhs_value) {
                            .u8 => {
                                const result = @addWithOverflow(current_value.u8, rhs_value.u8);
                                if (result[1] != 0) { // Check overflow flag
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Overflow during u8 addition", .{});
                                    return error.Overflow;
                                }
                                return token.TokenLiteral{ .u8 = result[0] };
                            },
                            .int => {
                                // Check if the int is representable as u8 for addition operand
                                if (rhs_value.int < 0 or rhs_value.int > 255) {
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Integer value {d} out of range for u8 addition", .{rhs_value.int});
                                    return error.Overflow;
                                }
                                const val: u8 = @intCast(rhs_value.int); // Safe cast due to above check
                                const result = @addWithOverflow(current_value.u8, val);
                                if (result[1] != 0) { // Check overflow flag
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Overflow during u8 addition", .{});
                                    return error.Overflow;
                                }
                                return token.TokenLiteral{ .u8 = result[0] };
                            },
                            else => return error.TypeError,
                        },
                        else => return error.TypeError,
                    },
                    .MINUS_EQUAL => switch (current_value) {
                        .int => if (rhs_value == .int)
                            // TODO: Add i32 overflow check?
                            token.TokenLiteral{ .int = current_value.int - rhs_value.int }
                        else
                            return error.TypeError,
                        .u8 => switch (rhs_value) {
                            .u8 => {
                                const result = @subWithOverflow(current_value.u8, rhs_value.u8);
                                if (result[1] != 0) { // Check underflow flag
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Underflow during u8 subtraction", .{});
                                    return error.Overflow; // Using Overflow for underflow too
                                }
                                return token.TokenLiteral{ .u8 = result[0] };
                            },
                            .int => {
                                // Check if the int is representable as u8 for subtraction operand
                                if (rhs_value.int < 0 or rhs_value.int > 255) {
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Integer value {d} out of range for u8 subtraction", .{rhs_value.int});
                                    return error.Overflow;
                                }
                                const val: u8 = @intCast(rhs_value.int); // Safe cast
                                const result = @subWithOverflow(current_value.u8, val);
                                if (result[1] != 0) { // Check underflow flag
                                    var reporting = Reporter.init();
                                    reporting.reportRuntimeError("Underflow during u8 subtraction", .{});
                                    return error.Overflow; // Using Overflow for underflow too
                                }
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
                            var reporting = Reporter.init();
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
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("String index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= str.len) {
                            var reporting = Reporter.init();
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
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("Array index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= arr.len) {
                            var reporting = Reporter.init();
                            reporting.reportRuntimeError("Array index out of bounds: index {d} for array of length {d}", .{ idx, arr.len });
                            return error.IndexOutOfBounds;
                        }

                        // Try to find array type information to preserve element types
                        if (index.array.* == .Variable) {
                            const var_name = index.array.Variable.lexeme;
                            if (self.environment.getTypeInfo(var_name)) |type_info| {
                                if (type_info.base == .Array and type_info.element_type != null) {
                                    // If element is an int but array type is u8, convert to u8
                                    if (type_info.element_type.? == .U8 and arr[idx] == .int) {
                                        const int_val = arr[idx].int;
                                        if (int_val >= 0 and int_val <= 255) {
                                            return token.TokenLiteral{ .u8 = @intCast(int_val) };
                                        }
                                    }
                                }
                            } else |_| {}
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
                                            .u8 => |u| try buffer.writer().print("{d}", .{u}),
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
                var item_type = ast.TypeInfo{ .base = .Auto };
                if (array_value.array.len > 0) {
                    item_type = switch (array_value.array[0]) {
                        .int => ast.TypeInfo{ .base = .Int },
                        .u8 => ast.TypeInfo{ .base = .U8 },
                        .float => ast.TypeInfo{ .base = .Float },
                        .string => ast.TypeInfo{ .base = .String },
                        .boolean => ast.TypeInfo{ .base = .Boolean },
                        .array => ast.TypeInfo{ .base = .Array },
                        .struct_value => ast.TypeInfo{ .base = .Struct },
                        .nothing => ast.TypeInfo{ .base = .Nothing },
                        else => ast.TypeInfo{ .base = .Auto },
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
                            bytes[i] = token.TokenLiteral{ .u8 = byte };
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
                                    .U8 => value == .u8,
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
                    try self.environment.define(e.variable.lexeme, val, .{ .base = .Auto });
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
                    try self.environment.define(var_name, val, .{ .base = .Auto });
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
            .TypeOf => |expr_to_check| {
                // Special case for array indexing
                if (expr_to_check.* == .Index and expr_to_check.Index.array.* == .Variable) {
                    const array_name = expr_to_check.Index.array.Variable.lexeme;

                    // Debug info
                    if (self.debug_enabled) {
                        std.debug.print("\nTypeOf on array index: array_name={s}\n", .{array_name});
                    }

                    const array_type_info = self.environment.getTypeInfo(array_name) catch |err| {
                        if (self.debug_enabled) {
                            std.debug.print("Error getting type info: {any}\n", .{err});
                        }
                        return error.VariableNotFound;
                    };

                    // Debug info
                    if (self.debug_enabled) {
                        std.debug.print("Array type info: base={any}, element_type={any}\n", .{ array_type_info.base, array_type_info.element_type });
                    }

                    // If this is a u8 array, return "u8" as the type
                    if (array_type_info.base == .Array and array_type_info.element_type != null) {
                        if (array_type_info.element_type.? == .U8) {
                            if (self.debug_enabled) {
                                std.debug.print("Found u8 array, returning u8 type\n", .{});
                            }
                            return token.TokenLiteral{ .string = "u8" };
                        }
                    }
                } else if (self.debug_enabled and expr_to_check.* == .Index) {
                    std.debug.print("\nTypeOf on Index but array is not Variable: {any}\n", .{expr_to_check.Index.array.*});
                }

                // Handle simple variables
                if (expr_to_check.* == .Variable) {
                    const var_token = expr_to_check.Variable;
                    // Get the DECLARED type info from the environment
                    const type_info = self.environment.getTypeInfo(var_token.lexeme) catch {
                        // Handle cases where the variable might not be found (shouldn't happen if code is valid)
                        var reporter = Reporter.init();
                        const location: Reporter.Location = .{ .file = "stdin", .line = var_token.line, .column = var_token.column };
                        reporter.reportCompileError(location, "Variable '{s}' not found during typeof", .{var_token.lexeme});
                        return error.VariableNotFound; // Or return err
                    };

                    // Return the string based on the DECLARED type_info.base
                    return token.TokenLiteral{
                        .string = switch (type_info.base) {
                            .Int => "int",
                            .U8 => "u8",
                            .Float => "float",
                            .String => "string",
                            .Boolean => "boolean",
                            .Tetra => "tetra",
                            .Nothing => "nothing",
                            .Array => "array",
                            .Function => "function",
                            .Struct => if (type_info.custom_type) |name| name else "struct", // Use stored name if available
                            .Enum => if (type_info.custom_type) |name| name else "enum", // Use stored name if available
                            .Tuple => "tuple",
                            .Map => "map",
                            .Custom => "custom",
                            .Reference => "reference",
                            .Auto => "auto",
                        },
                    };
                } else {
                    // If it's not a simple variable, evaluate it and report the runtime type
                    const value = try self.evaluate(expr_to_check);
                    return token.TokenLiteral{
                        .string = switch (value) {
                            .int => "int",
                            .u8 => "u8",
                            .float => "float",
                            .string => "string",
                            .boolean => "boolean",
                            .tetra => "tetra",
                            .nothing => "nothing",
                            .array => "array",
                            .function => "function",
                            .struct_value => |sv| sv.type_name, // Get type name from struct instance
                            .enum_variant => "enum_variant", // Might want more specific enum type later
                            .tuple => "tuple",
                            .map => "map",
                        },
                    };
                }
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
            .Assert => |assert| {
                const condition = try self.evaluate(assert.condition);
                if (condition != .boolean) {
                    var reporter = Reporter.init();
                    reporter.reportRuntimeError("Assertion failed: {s} is not a boolean", .{@tagName(condition)});
                    return error.TypeError;
                }
                if (condition.boolean == token.Boolean.false) {
                    var reporter = Reporter.init();

                    // Use custom message if provided
                    if (assert.message != null) {
                        const message_value = try self.evaluate(assert.message.?);
                        if (message_value == .string) {
                            reporter.reportCompileError(assert.location, "Assertion failed: {s}", .{message_value.string});
                        } else {
                            reporter.reportCompileError(assert.location, "Assertion failed", .{});
                        }
                    } else {
                        reporter.reportCompileError(assert.location, "Assertion failed", .{});
                    }
                    return error.AssertionFailed;
                }
                return .{ .nothing = {} };
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
                    if (value != .int and value != .u8) {
                        var reporting = Reporter.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .U8 => {
                    if (value != .u8 and value != .int) {
                        var reporting = Reporter.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to u8 variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Float => {
                    if (value != .float) {
                        var reporting = Reporter.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .String => {
                    if (value != .string) {
                        var reporting = Reporter.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Boolean => {
                    if (value != .boolean) {
                        var reporting = Reporter.init();
                        reporting.reportRuntimeError("Type error: Cannot assign {s} to boolean variable", .{@tagName(value)});
                        return error.TypeError;
                    }
                },
                .Auto => {}, // Type is already fixed from initialization
                else => {
                    var reporting = Reporter.init();
                    const location: Reporter.Location = .{ .file = "stdin", .line = assignment.name.line, .column = assignment.name.column };
                    reporting.reportCompileError(location, "Type error: Cannot assign {s} to variable", .{@tagName(value)});
                    return error.TypeError;
                },
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
                        type_info = .{ .base = .Auto, .is_dynamic = true, .is_mutable = true };
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

                // Execute function body with saved return state
                const previous_return_state = self.has_returned;
                self.has_returned = false; // Reset for this function call

                const result = self.executeBlock(f.body, &function_env) catch |err| {
                    self.environment = previous_env;
                    self.has_returned = previous_return_state; // Restore return state

                    if (err == error.ReturnValue) {
                        if (try function_env.get("return")) |return_value| {
                            if (self.debug_enabled) {
                                std.debug.print("\n=== Function Return Debug ===\n", .{});
                                std.debug.print("Raw return value: {any}\n", .{return_value});
                                if (return_value == .array) {
                                    std.debug.print("Array contents before processing:\n", .{});
                                    for (return_value.array, 0..) |item, i| {
                                        std.debug.print("  [{d}] Type={s}, Raw={any}\n", .{ i, @tagName(item), item });
                                        if (item == .string) {
                                            std.debug.print("    String content: '{s}'\n", .{item.string});
                                        }
                                    }
                                }
                                std.debug.print("Current environment: {*}\n", .{self.environment});
                                std.debug.print("Function environment: {*}\n", .{&function_env});
                            }

                            // Move the return value to the parent environment before function environment is destroyed
                            const moved_value = try self.moveValueToParentEnv(return_value);

                            if (self.debug_enabled) {
                                std.debug.print("\nAfter moveValueToParentEnv:\n", .{});
                                std.debug.print("Moved value: {any}\n", .{moved_value});
                                if (moved_value == .array) {
                                    std.debug.print("Array contents after move:\n", .{});
                                    for (moved_value.array, 0..) |item, i| {
                                        std.debug.print("  [{d}] Type={s}, Raw={any}\n", .{ i, @tagName(item), item });
                                        if (item == .string) {
                                            std.debug.print("    String content: '{s}'\n", .{item.string});
                                        }
                                    }
                                }
                            }

                            return moved_value;
                        }
                    }
                    return err;
                };

                // Restore previous environment
                self.environment = previous_env;
                self.has_returned = previous_return_state; // Restore return state

                return if (result) |value| value else .{ .nothing = {} };
            },
            else => return error.NotCallable,
        }
    }

    fn moveValueToParentEnv(self: *Interpreter, value: token.TokenLiteral) !token.TokenLiteral {
        if (self.debug_enabled) {
            std.debug.print("\n=== Moving value between environments ===\n", .{});
            std.debug.print("Input value type: {s}\n", .{@tagName(value)});
        }

        return switch (value) {
            .array => |arr| blk: {
                if (self.debug_enabled) {
                    std.debug.print("\nMoving array with {d} elements\n", .{arr.len});
                    std.debug.print("Original array elements:\n", .{});
                    for (arr, 0..) |item, i| {
                        std.debug.print("  [{d}] Type={s}, Value={any}\n", .{ i, @tagName(item), item });
                    }
                }

                var new_array = try self.allocator.alloc(token.TokenLiteral, arr.len);
                errdefer self.allocator.free(new_array);

                for (arr, 0..) |item, i| {
                    if (self.debug_enabled) {
                        std.debug.print("\nProcessing element {d}:\n", .{i});
                        std.debug.print("  Original type: {s}\n", .{@tagName(item)});
                        std.debug.print("  Original value: {any}\n", .{item});
                    }

                    new_array[i] = switch (item) {
                        .string => |s| blk2: {
                            if (self.debug_enabled) {
                                std.debug.print("  Found string: '{s}'\n", .{s});
                                std.debug.print("  String length: {d}\n", .{s.len});
                            }

                            // Get string from interner
                            const interned = try self.string_interner.intern(s);
                            if (self.debug_enabled) {
                                std.debug.print("  Interned string: '{s}'\n", .{interned});
                                std.debug.print("  Interned length: {d}\n", .{interned.len});
                            }

                            const result = token.TokenLiteral{ .string = interned };
                            if (self.debug_enabled) {
                                std.debug.print("  Created token: {any}\n", .{result});
                            }
                            break :blk2 result;
                        },
                        .int => |n| token.TokenLiteral{ .int = n },
                        .u8 => |u| token.TokenLiteral{ .u8 = u },
                        .float => |fl| token.TokenLiteral{ .float = fl },
                        .boolean => |b| token.TokenLiteral{ .boolean = b },
                        .tetra => |t| token.TokenLiteral{ .tetra = t },
                        .nothing => token.TokenLiteral{ .nothing = {} },
                        .array => |nested| try self.moveValueToParentEnv(token.TokenLiteral{ .array = nested }),
                        else => blk2: {
                            if (self.debug_enabled) {
                                std.debug.print("  WARNING: Unhandled type: {s}\n", .{@tagName(item)});
                            }
                            break :blk2 item;
                        },
                    };

                    if (self.debug_enabled) {
                        std.debug.print("  Final element type: {s}\n", .{@tagName(new_array[i])});
                        std.debug.print("  Final element value: {any}\n", .{new_array[i]});
                    }
                }

                if (self.debug_enabled) {
                    std.debug.print("\nFinal array contents:\n", .{});
                    for (new_array, 0..) |item, i| {
                        std.debug.print("  [{d}] Type={s}, Value={any}\n", .{ i, @tagName(item), item });
                    }
                }

                const result = token.TokenLiteral{ .array = new_array };
                if (self.debug_enabled) {
                    std.debug.print("\nReturning array token: {any}\n", .{result});
                }
                break :blk result;
            },
            else => blk: {
                if (self.debug_enabled) {
                    std.debug.print("Non-array value, passing through as-is\n", .{});
                }
                break :blk value;
            },
        };
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
