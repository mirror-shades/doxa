const std = @import("std");
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TokenImport = @import("../lexer/token.zig");
const Token = TokenImport.Token;
const StructField = TypesImport.StructField;
const StructLiteralField = ast.StructLiteralField;
const StructInstanceField = ast.StructInstanceField;
const reporting = @import("../utils/reporting.zig");
const ErrorList = reporting.ErrorList;
const Reporter = reporting.Reporter;
const Memory = @import("../utils/memory.zig");
const Scope = Memory.Scope;
const MemoryManager = Memory.MemoryManager;
const Parser = @import("../parser/parser_types.zig").Parser;
const Environment = @import("../types/types.zig").Environment;
const env_module = @import("environment.zig");
const TypesImport = @import("../types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const Tetra = TypesImport.Tetra;
const ModuleEnvironment = TypesImport.ModuleEnvironment;
const Expr = ast.Expr;
const StringInterner = Memory.StringInterner;

pub const Interpreter = struct {
    environment: *Environment,
    global_environment: *Environment,
    parser: ?*Parser,
    allocator: std.mem.Allocator,
    runtime_errors: std.ArrayList(Reporter.RuntimeError),
    debug_enabled: bool,
    string_interner: StringInterner,
    moduleEnvironments: std.StringHashMap(*Environment),
    module_contexts: std.StringHashMap(*ModuleEnvironment), // NEW: Track module environments
    memory_manager: *MemoryManager,
    reporter: *Reporter,

    return_value: ?TokenLiteral = null,
    entry_point_name: ?[]const u8 = null,
    last_result: ?TokenLiteral = null,
    had_error: bool = false,
    has_entry_point: bool = false,
    entry_point_location: ?Token = null,
    has_returned: bool = false,

    pub fn init(allocator: std.mem.Allocator, environment: *Environment, parser: ?*Parser, debug_enabled: bool, memory_manager: *MemoryManager, reporter: *Reporter) Interpreter {
        // Initialize scope_manager with a root scope
        if (memory_manager.scope_manager.root_scope == null) {
            memory_manager.scope_manager.root_scope = Scope.init(memory_manager.scope_manager, memory_manager.scope_manager.next_storage_id, null, debug_enabled) catch unreachable;
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
            .module_contexts = std.StringHashMap(*ModuleEnvironment).init(allocator),
            .memory_manager = memory_manager,
            .reporter = reporter,
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

        // Free up all module contexts
        var module_it = self.module_contexts.iterator();
        while (module_it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.module_contexts.deinit();

        // Clean up root scope if it exists
        if (self.memory_manager.scope_manager.root_scope) |root_scope| {
            root_scope.deinit();
            self.memory_manager.scope_manager.root_scope = null;
        }
        // Then set to null to guard against double-free
        self.memory_manager.scope_manager.root_scope = null;
    }

    /// Get or create a ModuleEnvironment for the given module
    fn getOrCreateModuleEnvironment(self: *Interpreter, module_name: []const u8) !*ModuleEnvironment {
        // Check if we already have this module environment
        if (self.module_contexts.get(module_name)) |existing| {
            return existing;
        }

        if (self.debug_enabled) {
            std.debug.print("Creating new ModuleEnvironment for: {s}\n", .{module_name});
        }

        // Create new ModuleEnvironment
        const module_env = try ModuleEnvironment.init(self.allocator, module_name, self.memory_manager, self.debug_enabled);

        // Get the module info from the parser
        if (self.parser) |p| {
            if (p.module_namespaces.get(module_name)) |module_info| {
                if (self.debug_enabled) {
                    std.debug.print("Populating ModuleEnvironment with module symbols\n", .{});
                }

                // Populate the module environment with the module's variables and functions
                try self.populateModuleEnvironment(module_env, module_info);

                // Add the module's imports to the module environment
                try self.populateModuleImports(module_env, module_info);
            }
        }

        // Store the module environment
        try self.module_contexts.put(module_name, module_env);

        return module_env;
    }

    /// Populate a ModuleEnvironment with variables and functions from ModuleInfo
    fn populateModuleEnvironment(self: *Interpreter, module_env: *ModuleEnvironment, module_info: ast.ModuleInfo) !void {
        if (module_info.ast) |module_ast| {
            if (module_ast.data == .Block) {
                const statements = module_ast.data.Block.statements;

                if (self.debug_enabled) {
                    std.debug.print("Processing {d} statements in module {s}\n", .{ statements.len, module_info.name });
                }

                for (statements) |stmt| {
                    switch (stmt.data) {
                        .VarDecl => |var_decl| {
                            // Evaluate the variable's initializer and add it to the module environment
                            if (var_decl.initializer) |init_expr| {
                                const value = try self.evaluate(init_expr);
                                try module_env.environment.define(var_decl.name.lexeme, value, var_decl.type_info);

                                if (self.debug_enabled) {
                                    std.debug.print("Added variable to module environment: {s} = {any}\n", .{ var_decl.name.lexeme, value });
                                }
                            } else {
                                // Default value based on type
                                const default_value = switch (var_decl.type_info.base) {
                                    .Int => TokenLiteral{ .int = 0 },
                                    .U8 => TokenLiteral{ .u8 = 0 },
                                    .Float => TokenLiteral{ .float = 0.0 },
                                    .String => TokenLiteral{ .string = try self.string_interner.intern("") },
                                    .Tetra => TokenLiteral{ .tetra = .false },
                                    else => TokenLiteral{ .nothing = {} },
                                };
                                try module_env.environment.define(var_decl.name.lexeme, default_value, var_decl.type_info);
                            }
                        },
                        .FunctionDecl => |func_decl| {
                            // Create function with reference to this module environment
                            const function = TokenLiteral{
                                .function = .{
                                    .params = func_decl.params,
                                    .body = func_decl.body,
                                    .closure = module_env.environment,
                                    .defining_module = module_env,
                                },
                            };
                            try module_env.environment.define(func_decl.name.lexeme, function, .{
                                .base = .Function,
                                .is_mutable = false,
                            });

                            if (self.debug_enabled) {
                                std.debug.print("Added function to module environment: {s}\n", .{func_decl.name.lexeme});
                            }
                        },
                        else => {}, // Skip other statement types for now
                    }
                }
            }
        }
    }

    /// Populate a ModuleEnvironment with its imports
    fn populateModuleImports(self: *Interpreter, module_env: *ModuleEnvironment, module_info: ast.ModuleInfo) !void {
        for (module_info.imports) |import_info| {
            if (import_info.namespace_alias) |alias| {
                try module_env.addImport(alias, import_info.module_path);

                if (self.debug_enabled) {
                    std.debug.print("Added import to module {s}: {s} -> {s}\n", .{ module_env.module_name, alias, import_info.module_path });
                }
            }
        }
    }

    fn u8BoundsCheck(self: *Interpreter, value: TokenLiteral) ErrorList!void {
        if (value.int < 0) {
            self.reporter.reportRuntimeError("Underflow: u8 cannot be negative", .{});
            return error.u8Underflow;
        }
        if (value.int > 255) {
            self.reporter.reportRuntimeError("Overflow: u8 cannot be greater than 255", .{});
            return error.u8Overflow;
        }
    }

    pub fn interpret(self: *Interpreter, statements: []ast.Stmt) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("\n=== Starting interpretation ===\n", .{});
            std.debug.print("Debug mode is: {}\n", .{self.debug_enabled});
            std.debug.print("Number of statements: {}\n", .{statements.len});
        }

        // First pass - Only process structs, enums, and functions (type declarations)
        for (0..statements.len) |i| {
            const stmt = &statements[i];
            if (self.debug_enabled) {
                std.debug.print("\nScanning for types - statement {d}: {s}\n", .{ i, @tagName(stmt.data) });
            }

            if (stmt.data == .Expression) {
                // Check for struct declarations in expressions
                if (stmt.data.Expression) |expr| {
                    if (expr.data == .StructDecl) {
                        if (self.debug_enabled) {
                            std.debug.print("Processing struct declaration: {s}\n", .{expr.data.StructDecl.name.lexeme});
                        }
                        // Define the struct type in the environment
                        var fields = std.ArrayList(ast.StructFieldType).init(self.allocator);
                        defer fields.deinit();

                        for (expr.data.StructDecl.fields) |field| {
                            const type_info = try ast.typeInfoFromExpr(self.allocator, field.type_expr);
                            try fields.append(.{
                                .name = field.name.lexeme,
                                .type_info = type_info,
                            });
                        }

                        try self.environment.define(expr.data.StructDecl.name.lexeme, .{ .nothing = {} }, .{
                            .base = .Struct,
                            .is_mutable = true,
                            .struct_fields = try fields.toOwnedSlice(),
                        });
                    }
                }
            } else if (stmt.data == .EnumDecl) {
                if (self.debug_enabled) {
                    std.debug.print("Processing enum declaration: {s}\n", .{stmt.data.EnumDecl.name.lexeme});
                }
                _ = try self.executeStatement(stmt, self.debug_enabled);
            } else if (stmt.data == .FunctionDecl) {
                const f = stmt.data.FunctionDecl;
                if (self.debug_enabled) {
                    std.debug.print("\nCreating forward declaration for: {s}\n", .{f.name.lexeme});
                }

                const function = TokenLiteral{
                    .function = .{
                        .params = f.params,
                        .body = f.body,
                        .closure = self.environment,
                        .defining_module = null, // TODO: Set this to the actual module when available
                    },
                };

                try self.environment.define(
                    f.name.lexeme,
                    function,
                    .{ // Use default TypeInfo for function, actual types checked during call
                        .base = .Function,
                        .is_mutable = false,
                    },
                );

                if (self.debug_enabled) {
                    std.debug.print("Defined function '{s}' with {d} parameters\n", .{ f.name.lexeme, f.params.len });
                }
            }
        }

        // Second pass - Execute all statements in order of appearance
        if (self.entry_point_name == null) {
            // Script mode - execute all remaining statements in order
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in script mode ===\n", .{});
            }
            for (statements) |*stmt| {
                // Skip declarations that were handled in the first pass
                if (stmt.data == .FunctionDecl or stmt.data == .EnumDecl or
                    (stmt.data == .Expression and stmt.data.Expression != null and stmt.data.Expression.?.data == .StructDecl))
                {
                    continue;
                }

                // Execute all other statements in order (including variable declarations and assignments)
                self.last_result = try self.executeStatement(stmt, self.debug_enabled);
            }
        } else {
            // Program mode with entry point
            if (self.debug_enabled) {
                std.debug.print("\n=== Running in program mode ===\n", .{});
            }

            // Entry point execution
            if (self.entry_point_name) |main_fn| {
                const main_value = (try self.environment.get(main_fn)) orelse return error.InvalidEntryPoint;
                if (main_value != .function) {
                    return error.InvalidEntryPoint;
                }

                var empty_args = [_]*ast.Expr{};
                self.last_result = try self.callFunction(main_value, &empty_args);
            } else {
                if (self.debug_enabled) {
                    std.debug.print("Warning: Program mode set but no entry point name found.\n", .{});
                }
                unreachable;
            }
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Interpretation complete ===\n", .{});
        }
    }

    pub fn executeBlock(self: *Interpreter, statements: []ast.Stmt, environment: *Environment) ErrorList!?TokenLiteral {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;

        // Handle empty blocks
        if (statements.len == 0) {
            return null;
        }

        var result: ?TokenLiteral = null;

        // CRITICAL FIX: Don't reset has_returned flag - it should persist until function completes
        // const previous_return_state = self.has_returned;
        // defer self.has_returned = previous_return_state;

        // Execute all statements, respecting return status
        for (statements) |*stmt| {
            // Skip statements if we've already returned
            if (self.has_returned) break;

            result = self.executeStatement(stmt, self.debug_enabled) catch |err| {
                if (err == error.ReturnValue) {
                    // Set the return flag but propagate the error to caller
                    self.has_returned = true;
                    if (self.debug_enabled) {
                        std.debug.print("EXECUTE BLOCK: Propagating ReturnValue error to caller\n", .{});
                    }
                    return err;
                }
                return err;
            };

            // Check for return value after each statement
            if (try environment.get("return")) |return_value| {
                // Set the return flag
                self.has_returned = true;
                return return_value;
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

    pub fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt, debug_enabled: bool) ErrorList!?TokenLiteral {
        if (debug_enabled) {
            std.debug.print("Executing statement: {any}\n", .{stmt.*});
        }

        return switch (stmt.data) {
            .FunctionDecl => |f| {
                if (self.debug_enabled) {
                    std.debug.print("Executing function declaration: {s}\n", .{f.name.lexeme});
                }

                const function = TokenLiteral{
                    .function = .{
                        .params = f.params,
                        .body = f.body,
                        .closure = self.environment,
                        .defining_module = null, // TODO: Set this to the actual module when available
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
                    // CRITICAL FIX: Don't use try - let ReturnValue errors propagate properly
                    const result = self.evaluate(e) catch |err| {
                        if (err == error.ReturnValue) {
                            if (self.debug_enabled) {
                                std.debug.print("EXPRESSION STATEMENT: Propagating ReturnValue error\n", .{});
                            }
                            return err;
                        }
                        return err;
                    };
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
                            self.reporter.reportRuntimeError("Type error: Cannot initialize int variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .U8 => {
                            if (init_value != .u8 and init_value != .int) {
                                self.reporter.reportRuntimeError("Type error: Cannot initialize u8 variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }

                            // Convert int to u8 if needed
                            if (init_value == .int) {
                                if (init_value.int < 0 or init_value.int > 255) {
                                    self.reporter.reportRuntimeError("Type error: Integer value {d} out of range for u8", .{init_value.int});
                                    return error.TypeError;
                                }
                                break :blk TokenLiteral{ .u8 = @intCast(init_value.int) };
                            }
                        },
                        .Float => if (init_value != .float and init_value != .int) {
                            self.reporter.reportRuntimeError("Type error: Cannot initialize float variable with {s}", .{@tagName(init_value)});
                            return error.TypeError;
                        },
                        .Array => {
                            if (init_value != .array) {
                                self.reporter.reportRuntimeError("Type error: Cannot initialize array variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }

                            // Check array element types if element_type is specified
                            if (decl.type_info.element_type) |expected_type| {
                                // If this is a u8 array, convert int elements to u8
                                if (expected_type == .U8) {
                                    var new_array = try self.allocator.alloc(TokenLiteral, init_value.array.len);
                                    for (init_value.array, 0..) |elem, idx| {
                                        if (elem == .int) {
                                            if (elem.int < 0 or elem.int > 255) {
                                                self.reporter.reportRuntimeError("Type error: Integer value {d} at index {d} out of range for u8 array", .{ elem.int, idx });
                                                self.allocator.free(new_array);
                                                return error.TypeError;
                                            }
                                            new_array[idx] = TokenLiteral{ .u8 = @intCast(elem.int) };
                                        } else if (elem == .u8) {
                                            new_array[idx] = elem;
                                        } else {
                                            self.reporter.reportRuntimeError("Type error: Cannot convert {s} at index {d} to u8", .{ @tagName(elem), idx });
                                            self.allocator.free(new_array);
                                            return error.TypeError;
                                        }
                                    }

                                    // Free the original array
                                    self.allocator.free(init_value.array);

                                    // Return the new array with proper u8 elements
                                    break :blk TokenLiteral{ .array = new_array };
                                }

                                // For other array types, check that elements match
                                for (init_value.array) |element| {
                                    const matches = switch (expected_type) {
                                        .Int => element == .int,
                                        .U8 => element == .u8,
                                        .Float => element == .float,
                                        .String => element == .string,
                                        .Tetra => element == .tetra,
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
                                self.reporter.reportRuntimeError("Type error: Cannot initialize map variable with {s}", .{@tagName(init_value)});
                                return error.TypeError;
                            }
                        },
                        .Auto => {},
                        else => {},
                    }
                    break :blk init_value;
                } else switch (decl.type_info.base) {
                    .Int => TokenLiteral{ .int = 0 },
                    .U8 => TokenLiteral{ .u8 = 0 },
                    .Float => TokenLiteral{ .float = 0.0 },
                    .String => TokenLiteral{ .string = try self.string_interner.intern("") },
                    .Map => TokenLiteral{ .map = std.StringHashMap(TokenLiteral).init(self.allocator) },
                    .Tetra => TokenLiteral{ .tetra = Tetra.false },
                    .Array => blk: {
                        // Check if we have a size from the type declaration
                        if (decl.type_info.array_size) |size| {
                            // Create an array of the specified size
                            const array_elements = try self.allocator.alloc(TokenLiteral, size);

                            // Initialize with default values (0 for u8)
                            for (array_elements) |*elem| {
                                // Set default value based on element type
                                if (decl.type_info.element_type) |elem_type| {
                                    elem.* = switch (elem_type) {
                                        .Int => TokenLiteral{ .int = 0 },
                                        .U8 => TokenLiteral{ .u8 = 0 },
                                        .Float => TokenLiteral{ .float = 0.0 },
                                        .String => TokenLiteral{ .string = try self.string_interner.intern("") },
                                        .Tetra => TokenLiteral{ .tetra = Tetra.false },
                                        else => TokenLiteral{ .nothing = {} },
                                    };
                                } else {
                                    // Default to u8 if no element type specified
                                    elem.* = TokenLiteral{ .u8 = 0 };
                                }
                            }

                            break :blk TokenLiteral{ .array = array_elements };
                        }

                        // If no size specified, use empty array (current behavior)
                        break :blk TokenLiteral{ .array = &[_]TokenLiteral{} };
                    },
                    else => TokenLiteral{ .nothing = {} },
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
                        .is_mutable = true,
                        .element_type = .U8,
                        .array_type = decl.type_info.array_type,
                        .array_size = decl.type_info.array_size,
                    };
                }

                // Capture struct type name from struct instances
                if (value == .struct_value and final_type_info.base == .Struct) {
                    final_type_info.custom_type = value.struct_value.type_name;
                }

                try self.environment.define(decl.name.lexeme, value, final_type_info);
                return null;
            },
            .Block => |statements| {
                var block_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
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
                var map = std.StringHashMap(TokenLiteral).init(self.allocator);
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

                return TokenLiteral{ .map = map };
            },
            .Try => |try_stmt| {
                // Create new environment for try block
                var try_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                defer try_env.deinit();

                // Execute try block
                const try_result = self.executeBlock(try_stmt.try_body, &try_env) catch |err| {
                    // Create new environment for catch block
                    var catch_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
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
                if (condition != .tetra) {
                    self.reporter.reportRuntimeError("Assertion failed: {s} is not a tetra", .{@tagName(condition)});
                    return error.TypeError;
                }
                if (condition.tetra == Tetra.false) {

                    // Use custom message if provided
                    if (assert.message != null) {
                        const message_value = try self.evaluate(assert.message.?);
                        if (message_value == .string) {
                            self.reporter.reportCompileError(assert.location, "Assertion failed: {s}", .{message_value.string});
                        } else {
                            self.reporter.reportCompileError(assert.location, "Assertion failed", .{});
                        }
                    } else {
                        self.reporter.reportCompileError(assert.location, "Assertion failed", .{});
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
                if (self.debug_enabled) {
                    std.debug.print("RETURN STATEMENT: Executing return statement\n", .{});
                }
                if (ret.value) |value| {
                    const return_value = try self.evaluate(value);
                    try self.environment.define("return", return_value, .{ .base = .Auto });
                    self.has_returned = true; // Set the flag
                    if (self.debug_enabled) {
                        std.debug.print("RETURN STATEMENT: Set has_returned=true, returning error.ReturnValue\n", .{});
                    }
                    return error.ReturnValue;
                }
                try self.environment.define("return", .{ .nothing = {} }, .{ .base = .Nothing });
                self.has_returned = true; // Set the flag
                if (self.debug_enabled) {
                    std.debug.print("RETURN STATEMENT: Set has_returned=true, returning error.ReturnValue\n", .{});
                }
                return error.ReturnValue;
            },
        };
    }

    pub fn evaluate(self: *Interpreter, expr: *const ast.Expr) ErrorList!TokenLiteral {
        return switch (expr.data) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |binary| {
                const left = try self.evaluate(binary.left orelse return error.InvalidExpression);
                const right = try self.evaluate(binary.right orelse return error.InvalidExpression);

                if (self.debug_enabled) {
                    std.debug.print("Left operand: {any}\n", .{left});
                    std.debug.print("Right operand: {any}\n", .{right});
                }

                return switch (binary.operator.type) {
                    .EQUALITY => switch (left) {
                        .int => |i| switch (right) {
                            .int => |j| TokenLiteral{ .tetra = if (i == j) .true else .false },
                            .u8 => |j| TokenLiteral{ .tetra = if (i == j) .true else .false },
                            else => TokenLiteral{ .tetra = .false },
                        },
                        .u8 => |i| switch (right) {
                            .int => |j| TokenLiteral{ .tetra = if (i == j) .true else .false },
                            .u8 => |j| TokenLiteral{ .tetra = if (i == j) .true else .false },
                            else => TokenLiteral{ .tetra = .false },
                        },
                        .float => {
                            if (right != .int and right != .float) return error.TypeError;

                            if (Interpreter.compare(left.int, right.int) == 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        },
                        .tetra => {
                            if (right != .tetra) return error.TypeError;
                            return compareLogical(left.tetra, right.tetra);
                        },
                        .string => {
                            if (std.mem.eql(u8, left.string, right.string)) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        },
                        .nothing => {
                            if (right == .nothing) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        },
                        .array => if (right == .array) blk: {
                            if (left.array.len != right.array.len) break :blk TokenLiteral{ .tetra = .false };
                            for (left.array, right.array) |l, r| {
                                if (!std.meta.eql(l, r)) break :blk TokenLiteral{ .tetra = .false };
                            }
                            break :blk TokenLiteral{ .tetra = .true };
                        } else TokenLiteral{ .tetra = .false },
                        .struct_value => if (right == .struct_value) blk: {
                            if (left.struct_value.fields.len != right.struct_value.fields.len) break :blk TokenLiteral{ .tetra = .false };
                            for (left.struct_value.fields, right.struct_value.fields) |l, r| {
                                if (!std.mem.eql(u8, l.name, r.name)) break :blk TokenLiteral{ .tetra = .false };
                                const values_equal = try self.valuesEqual(l.value, r.value);
                                if (!values_equal) break :blk TokenLiteral{ .tetra = .false };
                            }
                            break :blk TokenLiteral{ .tetra = .true };
                        } else TokenLiteral{ .tetra = .false },
                        .function => TokenLiteral{ .tetra = .false }, // Functions are never equal
                        .enum_variant => {
                            if (std.mem.eql(u8, left.enum_variant, right.enum_variant)) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        },
                        .tuple => |l| switch (right) {
                            .tuple => |r| {
                                if (l.len != r.len) return TokenLiteral{ .tetra = .false };
                                // Compare each element
                                for (l, 0..) |item, i| {
                                    const values_equal = try self.valuesEqual(item, r[i]);
                                    if (!values_equal) {
                                        return TokenLiteral{ .tetra = .false };
                                    }
                                }
                                return TokenLiteral{ .tetra = .true };
                            },
                            else => TokenLiteral{ .tetra = .false },
                        },
                        .map => TokenLiteral{ .tetra = .false },
                    },
                    .PLUS => {
                        if (left == .string and right == .string) {
                            const result = try std.mem.concat(self.allocator, u8, &.{ left.string, right.string });
                            return TokenLiteral{ .string = result };
                        }
                        if (left == .int and right == .int) {
                            return TokenLiteral{ .int = left.int + right.int };
                        }
                        return error.TypeError;
                    },
                    .MINUS => {
                        if (left == .int and right == .int) {
                            return TokenLiteral{ .int = left.int - right.int };
                        }
                        return error.TypeError;
                    },
                    .ASTERISK => {
                        if (left == .int and right == .int) {
                            return TokenLiteral{ .int = left.int * right.int };
                        }
                        return error.TypeError;
                    },
                    .SLASH => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return TokenLiteral{ .int = @divTrunc(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    .GREATER => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) > 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        if (left == .float and right == .int) {
                            if (left.float > @as(f64, @floatFromInt(right.int))) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        if (left == .int and right == .float) {
                            if (@as(f64, @floatFromInt(left.int)) > right.float) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        if (left == .float and right == .float) {
                            if (left.float > right.float) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        return error.TypeError;
                    },
                    .GREATER_EQUAL => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) >= 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        return error.TypeError;
                    },
                    .LESS => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) < 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        return error.TypeError;
                    },
                    .LESS_EQUAL => {
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) <= 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        return error.TypeError;
                    },
                    .BANG_EQUAL => {
                        if (left == .string and right == .string) {
                            if (!std.mem.eql(u8, left.string, right.string)) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        if (left == .int and right == .int) {
                            if (Interpreter.compare(left.int, right.int) != 0) {
                                return TokenLiteral{ .tetra = .true };
                            } else {
                                return TokenLiteral{ .tetra = .false };
                            }
                        }
                        const location: Reporter.Location = .{ .file = "stdin", .line = binary.operator.line, .column = binary.operator.column };
                        self.reporter.reportCompileError(location, "Cannot compare {s} and {s}. Both sides must be bools.", .{ @tagName(left), @tagName(right) });
                        return error.TypeError;
                    },
                    .MODULO => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return TokenLiteral{ .int = @mod(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    .OR => {
                        if (left != .tetra or right != .tetra) return error.TypeError;
                        return try orLogical(left.tetra, right.tetra);
                    },
                    else => return error.InvalidOperator,
                };
            },
            .If => |if_expr| {
                const condition = try self.evaluate(if_expr.condition orelse return error.InvalidExpression);
                if (condition != .tetra) {
                    return error.TypeError;
                }

                const branch = if (condition.tetra == .true or condition.tetra == .both)
                    if_expr.then_branch orelse return error.InvalidExpression
                else
                    if_expr.else_branch orelse return error.InvalidExpression;

                // Create a new environment for the if block
                var if_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                defer if_env.deinit();

                // Allocate the statement array
                var statements = try self.allocator.alloc(ast.Stmt, 1);
                defer self.allocator.free(statements);
                statements[0] = .{
                    .base = .{
                        .id = 0, // Temporary ID for dynamic statements
                        .span = .{
                            .start = .{ .file = "", .line = 0, .column = 0 },
                            .end = .{ .file = "", .line = 0, .column = 0 },
                        },
                    },
                    .data = .{ .Expression = branch },
                };

                // Execute the chosen branch in the new environment
                const result = self.executeBlock(statements, &if_env) catch |err| {
                    if (err == error.ReturnValue) {
                        if (self.debug_enabled) {
                            std.debug.print("IF STATEMENT: Caught ReturnValue error from if block\n", .{});
                        }
                        // CRITICAL FIX: Save return value in parent environment and propagate error
                        if (try if_env.get("return")) |return_value| {
                            if (self.debug_enabled) {
                                std.debug.print("IF STATEMENT: Found return value {any}, defining in parent environment\n", .{return_value});
                            }
                            // Try to assign first, if that fails then define
                            self.environment.assign("return", return_value) catch {
                                try self.environment.define("return", return_value, .{ .base = .Auto });
                            };
                        }
                        // Propagate the ReturnValue error instead of consuming it
                        if (self.debug_enabled) {
                            std.debug.print("IF STATEMENT: Propagating ReturnValue error to parent\n", .{});
                        }
                        return err;
                    }
                    return err;
                };

                return result orelse .{ .nothing = {} };
            },
            .Block => |block| {
                var block_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                defer block_env.deinit();

                const result = self.executeBlock(block.statements, &block_env) catch |err| {
                    if (err == error.ReturnValue) {
                        // CRITICAL FIX: Save return value in parent environment and propagate error
                        if (try block_env.get("return")) |return_value| {
                            // Try to assign first, if that fails then define
                            self.environment.assign("return", return_value) catch {
                                try self.environment.define("return", return_value, .{ .base = .Auto });
                            };
                        }
                        // Propagate the ReturnValue error instead of consuming it
                        return err;
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
                            return TokenLiteral{ .int = -operand.int };
                        }
                        return error.TypeError;
                    },
                    .NOT_TRANCENDENTAL => {
                        if (operand == .tetra) {
                            return TokenLiteral{ .tetra = Tetra.neither };
                        }
                        return error.TypeError;
                    },
                    .NOT => {
                        if (operand == .tetra) {
                            return negateLogical(operand.tetra);
                        }
                        return error.TypeError;
                    },
                    .PLUS => {
                        if (operand == .int) {
                            return TokenLiteral{ .int = operand.int };
                        }
                        if (operand == .float) {
                            return TokenLiteral{ .float = operand.float };
                        }
                        return error.InvalidOperator;
                    },
                    else => return error.InvalidOperator,
                }
            },
            .Variable => |var_token| {
                if (self.debug_enabled) {
                    std.debug.print("\n=== Variable Lookup Debug ===\n", .{});
                    std.debug.print("Looking up variable: '{s}' (len: {})\n", .{ var_token.lexeme, var_token.lexeme.len });
                    std.debug.print("Current environment: {*}\n", .{self.environment});
                    std.debug.print("Environment has enclosing: {any}\n", .{self.environment.enclosing != null});
                }

                // Check if variable exists in current environment
                if (try self.environment.get(var_token.lexeme)) |value| {
                    if (self.debug_enabled) {
                        std.debug.print("Found variable in current environment: {any}\n", .{value});
                    }
                    return value;
                }

                if (self.debug_enabled) {
                    std.debug.print("Variable not found in current environment\n", .{});
                }

                // If not found locally, check if we're in a function with a defining module
                // and look for the variable in the module's environment
                if (self.environment.module) |current_module| {
                    if (self.debug_enabled) {
                        std.debug.print("Checking variable in defining module: {s}\n", .{current_module.module_name});
                    }

                    if (try current_module.environment.get(var_token.lexeme)) |module_value| {
                        if (self.debug_enabled) {
                            std.debug.print("Found variable in module environment: {any}\n", .{module_value});
                        }
                        return module_value;
                    }

                    // Also check if this is an import in the module
                    if (current_module.imports.get(var_token.lexeme)) |import_path| {
                        if (self.debug_enabled) {
                            std.debug.print("Found import in module: {s} -> {s}\n", .{ var_token.lexeme, import_path });
                        }

                        // Extract module name from import path
                        var path_iter = std.mem.splitSequence(u8, import_path, "/");
                        var import_module_name: []const u8 = "";
                        while (path_iter.next()) |part| {
                            import_module_name = part;
                        }
                        if (std.mem.endsWith(u8, import_module_name, ".doxa")) {
                            import_module_name = import_module_name[0 .. import_module_name.len - 5];
                        }

                        // Return namespace access marker
                        return TokenLiteral{ .nothing = {} };
                    }
                }

                // Check if this is a namespace (imported module)
                // Check for imports registered by the parser
                if (self.parser) |p| {
                    if (self.debug_enabled) {
                        std.debug.print("Checking parser for imports...\n", .{});
                        std.debug.print("Parser module namespaces count: {d}\n", .{p.module_namespaces.count()});
                    }

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
                            return TokenLiteral{ .nothing = {} };
                        } else if (self.debug_enabled) {
                            std.debug.print("Variable '{s}' not found in module namespaces\n", .{var_token.lexeme});
                            std.debug.print("Available namespaces:\n", .{});
                            var namespace_it = p.module_namespaces.iterator();
                            while (namespace_it.next()) |entry| {
                                std.debug.print("  - {s}\n", .{entry.key_ptr.*});
                            }
                        }
                    } else if (self.debug_enabled) {
                        std.debug.print("Parser has no imported symbols\n", .{});
                    }
                } else if (self.debug_enabled) {
                    std.debug.print("No parser available\n", .{});
                }

                // If we get here, variable wasn't found
                if (self.debug_enabled) {
                    std.debug.print("Variable not found: '{s}'\n", .{var_token.lexeme});
                }
                const location: Reporter.Location = .{ .file = "stdin", .line = var_token.line, .column = var_token.column };
                self.reporter.reportCompileError(location, "Variable '{s}' not found", .{var_token.lexeme});
                return error.VariableNotFound;
            },
            .Assignment => |assign| {
                const value = try self.evaluate(assign.value orelse return error.InvalidExpression);

                // Get variable's type info
                const var_type = try self.environment.getTypeInfo(assign.name.lexeme);

                // Debug: print the type info
                if (self.debug_enabled) {
                    std.debug.print("DEBUG: Assignment to '{s}': type_info.is_mutable={}, type_info.base={s}\n", .{ assign.name.lexeme, var_type.is_mutable, @tagName(var_type.base) });
                }

                // Check mutability
                if (!var_type.is_mutable) {
                    return error.ConstAssignment;
                }

                // Type checking (always enabled in statically typed mode)
                switch (var_type.base) {
                    .Int => {
                        if (value != .int and value != .u8) {
                            self.reporter.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                            return error.TypeError;
                        }
                    },
                    .U8 => {
                        if (value != .u8 and value != .int) {
                            self.reporter.reportRuntimeError("Type error: Cannot assign {s} to u8 variable", .{@tagName(value)});
                            return error.TypeError;
                        }
                    },
                    .Float => {
                        if (value != .float) {
                            self.reporter.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                            return error.TypeError;
                        }
                    },
                    .String => {
                        if (value != .string) {
                            self.reporter.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                            return error.TypeError;
                        }
                    },
                    .Tetra => {
                        if (value != .tetra) {
                            self.reporter.reportRuntimeError("Type error: Cannot assign {s} to tetra variable", .{@tagName(value)});
                            return error.TypeError;
                        }
                    },
                    .Auto => {}, // Type is already fixed from initialization
                    else => {
                        const location: Reporter.Location = .{ .file = "stdin", .line = assign.name.line, .column = assign.name.column };
                        self.reporter.reportCompileError(location, "Type error: Cannot assign {s} to variable", .{@tagName(value)});
                        return error.TypeError;
                    },
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
                            TokenLiteral{ .int = current_value.int + rhs_value.int }
                        else
                            return error.TypeError,
                        .u8 => switch (rhs_value) {
                            .u8 => {
                                const result = @addWithOverflow(current_value.u8, rhs_value.u8);
                                if (result[1] != 0) { // Check overflow flag
                                    self.reporter.reportRuntimeError("Overflow during u8 addition", .{});
                                    return error.Overflow;
                                }
                                return TokenLiteral{ .u8 = result[0] };
                            },
                            .int => {
                                // Check if the int is representable as u8 for addition operand
                                if (rhs_value.int < 0 or rhs_value.int > 255) {
                                    self.reporter.reportRuntimeError("Integer value {d} out of range for u8 addition", .{rhs_value.int});
                                    return error.Overflow;
                                }
                                const val: u8 = @intCast(rhs_value.int); // Safe cast due to above check
                                const result = @addWithOverflow(current_value.u8, val);
                                if (result[1] != 0) { // Check overflow flag
                                    self.reporter.reportRuntimeError("Overflow during u8 addition", .{});
                                    return error.Overflow;
                                }
                                return TokenLiteral{ .u8 = result[0] };
                            },
                            else => return error.TypeError,
                        },
                        else => return error.TypeError,
                    },
                    .MINUS_EQUAL => switch (current_value) {
                        .int => if (rhs_value == .int)
                            // TODO: Add i32 overflow check?
                            TokenLiteral{ .int = current_value.int - rhs_value.int }
                        else
                            return error.TypeError,
                        .u8 => switch (rhs_value) {
                            .u8 => {
                                const result = @subWithOverflow(current_value.u8, rhs_value.u8);
                                if (result[1] != 0) { // Check underflow flag
                                    self.reporter.reportRuntimeError("Underflow during u8 subtraction", .{});
                                    return error.Overflow; // Using Overflow for underflow too
                                }
                                return TokenLiteral{ .u8 = result[0] };
                            },
                            .int => {
                                // Check if the int is representable as u8 for subtraction operand
                                if (rhs_value.int < 0 or rhs_value.int > 255) {
                                    self.reporter.reportRuntimeError("Integer value {d} out of range for u8 subtraction", .{rhs_value.int});
                                    return error.Overflow;
                                }
                                const val: u8 = @intCast(rhs_value.int); // Safe cast
                                const result = @subWithOverflow(current_value.u8, val);
                                if (result[1] != 0) { // Check underflow flag
                                    self.reporter.reportRuntimeError("Underflow during u8 subtraction", .{});
                                    return error.Overflow; // Using Overflow for underflow too
                                }
                                return TokenLiteral{ .u8 = result[0] };
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
                var array_values = std.ArrayList(TokenLiteral).init(self.allocator);
                errdefer array_values.deinit();

                // Evaluate first element to establish type
                if (elements.len > 0) {
                    const first = try self.evaluate(elements[0]);
                    try array_values.append(first);
                    const first_type = @as(std.meta.Tag(TokenLiteral), first);

                    // Check remaining elements match the first element's type
                    for (elements[1..]) |element| {
                        const value = try self.evaluate(element);
                        const value_type = @as(std.meta.Tag(TokenLiteral), value);

                        if (value_type != first_type) {
                            array_values.deinit();
                            self.reporter.reportRuntimeError("Heterogeneous array detected: cannot mix {s} and {s}", .{ @tagName(first_type), @tagName(value_type) });
                            return error.HeterogeneousArray;
                        }
                        try array_values.append(value);
                    }
                }

                const owned_slice = try array_values.toOwnedSlice();
                errdefer self.allocator.free(owned_slice);

                return TokenLiteral{ .array = owned_slice };
            },
            .Struct => |fields| {
                var struct_fields = std.ArrayList(TypesImport.StructField).init(self.allocator);
                errdefer struct_fields.deinit();

                for (fields) |field| {
                    const value = try self.evaluate(field.value);
                    try struct_fields.append(.{
                        .name = field.name.lexeme,
                        .value = value,
                    });
                }

                // For basic struct creation without a specific type name, use a generic name
                return TokenLiteral{ .struct_value = .{
                    .type_name = "struct",
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
                            self.reporter.reportRuntimeError("String index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= str.len) {
                            self.reporter.reportRuntimeError("String index out of bounds: index {d} for string of length {d}", .{ idx, str.len });
                            return error.IndexOutOfBounds;
                        }
                        // Create a new string containing just the character at the index
                        var char_str = try self.allocator.alloc(u8, 1);
                        char_str[0] = str[idx];
                        return TokenLiteral{ .string = char_str };
                    },
                    .array => |arr| {
                        if (index_value != .int) {
                            // Special case: check if this is a length access
                            if (index.index.data == .Literal and
                                index.index.data.Literal == .string and
                                std.mem.eql(u8, "length", index.index.data.Literal.string))
                            {
                                return TokenLiteral{ .int = @intCast(arr.len) };
                            }
                            return error.TypeError;
                        }
                        if (index_value.int < 0) {
                            self.reporter.reportRuntimeError("Array index out of bounds: negative index {d}", .{index_value.int});
                            return error.IndexOutOfBounds;
                        }
                        const idx = @as(usize, @intCast(index_value.int));
                        if (idx >= arr.len) {
                            self.reporter.reportRuntimeError("Array index out of bounds: index {d} for array of length {d}", .{ idx, arr.len });
                            return error.IndexOutOfBounds;
                        }

                        // Try to find array type information to preserve element types
                        if (index.array.data == .Variable) {
                            const var_name = index.array.data.Variable.lexeme;
                            if (self.environment.getTypeInfo(var_name)) |type_info| {
                                if (type_info.base == .Array and type_info.element_type != null) {
                                    // If element is an int but array type is u8, convert to u8
                                    if (type_info.element_type.? == .U8 and arr[idx] == .int) {
                                        const int_val = arr[idx].int;
                                        if (int_val >= 0 and int_val <= 255) {
                                            return TokenLiteral{ .u8 = @intCast(int_val) };
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
                if (idx_assign.value.data == .CompoundAssign) {
                    // Get the current value at the index
                    const usize_index = @as(usize, @intCast(index.int));
                    if (usize_index >= array_val.array.len) {
                        return error.IndexOutOfBounds;
                    }

                    const current_value = array_val.array[usize_index];

                    // Evaluate the compound assignment
                    const compound = idx_assign.value.data.CompoundAssign;
                    const rhs_value = try self.evaluate(compound.value orelse return error.InvalidExpression);

                    // Perform the compound operation
                    const result = switch (compound.operator.type) {
                        .PLUS_EQUAL => switch (current_value) {
                            .int => if (rhs_value == .int)
                                TokenLiteral{ .int = current_value.int + rhs_value.int }
                            else
                                return error.TypeError,
                            .u8 => switch (rhs_value) {
                                .int => {
                                    if (rhs_value.int < 0 or rhs_value.int > 255) return error.Overflow;
                                    const val: u8 = @intCast(rhs_value.int);
                                    const sum = current_value.u8 + val;
                                    if (sum > 255) return error.Overflow;
                                    return TokenLiteral{ .u8 = sum };
                                },
                                .u8 => {
                                    const sum = current_value.u8 + rhs_value.u8;
                                    if (sum > 255) return error.Overflow;
                                    return TokenLiteral{ .u8 = sum };
                                },
                                else => return error.TypeError,
                            },
                            else => return error.TypeError,
                        },
                        .MINUS_EQUAL => switch (current_value) {
                            .int => if (rhs_value == .int)
                                TokenLiteral{ .int = current_value.int - rhs_value.int }
                            else
                                return error.TypeError,
                            .u8 => switch (rhs_value) {
                                .u8 => {
                                    const result = @subWithOverflow(current_value.u8, rhs_value.u8);
                                    if (result[1] != 0) return error.Overflow;
                                    return TokenLiteral{ .u8 = result[0] };
                                },
                                .int => {
                                    if (rhs_value.int < 0 or rhs_value.int > 255)
                                        return error.Overflow;
                                    const val: u8 = @intCast(rhs_value.int);
                                    const result = @subWithOverflow(current_value.u8, val);
                                    if (result[1] != 0) return error.Overflow;
                                    return TokenLiteral{ .u8 = result[0] };
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
                if (call.callee.data == .FieldAccess) {
                    const field_access = call.callee.data.FieldAccess;

                    // FIRST: Check if this is a namespace access before evaluating the object as a variable
                    if (field_access.object.data == .Variable) {
                        const namespace = field_access.object.data.Variable.lexeme;

                        // Check if this is a known namespace in the parser
                        if (self.parser) |p| {
                            if (p.module_namespaces.contains(namespace)) {
                                if (self.debug_enabled) {
                                    std.debug.print("Detected namespace access pattern: {s}.{s}\n", .{ namespace, field_access.field.lexeme });
                                }

                                // Handle namespace access directly
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, field_access.field.lexeme });
                                defer self.allocator.free(full_name);

                                // Check in imported symbols
                                if (p.imported_symbols) |symbols| {
                                    if (symbols.get(full_name)) |symbol| {
                                        if (self.debug_enabled) {
                                            std.debug.print("Found function in namespace: {s}\n", .{full_name});
                                        }

                                        if (symbol.kind == .Function) {
                                            // Get the module that contains this function
                                            const module_info = p.module_namespaces.get(namespace).?;

                                            // Find the function in this module
                                            if (module_info.ast) |module_ast| {
                                                if (module_ast.data == .Block) {
                                                    for (module_ast.data.Block.statements) |module_stmt| {
                                                        if (module_stmt.data == .FunctionDecl and
                                                            std.mem.eql(u8, module_stmt.data.FunctionDecl.name.lexeme, field_access.field.lexeme))
                                                        {
                                                            // Create function value and call it
                                                            const module_env = try self.getOrCreateModuleEnvironment(namespace);
                                                            const function = TokenLiteral{
                                                                .function = .{
                                                                    .params = module_stmt.data.FunctionDecl.params,
                                                                    .body = module_stmt.data.FunctionDecl.body,
                                                                    .closure = module_env.environment,
                                                                    .defining_module = module_env,
                                                                },
                                                            };
                                                            return self.callFunction(function, call.arguments);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // SECOND: If not a namespace access, evaluate as a regular object
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
                            var new_array = try self.allocator.alloc(TokenLiteral, current_array.len + 1);
                            errdefer self.allocator.free(new_array);

                            // Copy existing elements
                            @memcpy(new_array[0..current_array.len], current_array);

                            // Add new element
                            new_array[current_array.len] = arg_value;

                            // Free old array
                            self.allocator.free(current_array);

                            // Create new token literal with the new array
                            const new_value = TokenLiteral{ .array = new_array };

                            // Update the variable in the environment
                            if (field_access.object.data == .Variable) {
                                try self.environment.assign(field_access.object.data.Variable.lexeme, new_value);
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

                        // Check if this is a direct namespace or a nested module reference
                        if (field_access.object.data == .Variable) {
                            const namespace = field_access.object.data.Variable.lexeme;

                            // Look for the function in the imported symbols
                            if (self.parser) |p| {
                                // First try the straightforward lookup - exact namespace.function
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, field_access.field.lexeme });
                                defer self.allocator.free(full_name);

                                // Check in imported symbols first
                                if (p.imported_symbols) |symbols| {
                                    if (symbols.get(full_name)) |symbol| {
                                        if (self.debug_enabled) {
                                            std.debug.print("Found function in direct imported symbol: {s}\n", .{full_name});
                                        }

                                        // Continue with existing code to get the function
                                        if (symbol.kind == .Function) {
                                            // Get the module that contains this function
                                            const module_info = p.module_namespaces.get(namespace).?;

                                            // Find the function in this module
                                            if (module_info.ast) |module_ast| {
                                                if (module_ast.data == .Block) {
                                                    for (module_ast.data.Block.statements) |module_stmt| {
                                                        if (module_stmt.data == .FunctionDecl and
                                                            std.mem.eql(u8, module_stmt.data.FunctionDecl.name.lexeme, field_access.field.lexeme))
                                                        {
                                                            // Create function value and call it
                                                            const module_env = try self.getOrCreateModuleEnvironment(namespace);
                                                            const function = TokenLiteral{
                                                                .function = .{
                                                                    .params = module_stmt.data.FunctionDecl.params,
                                                                    .body = module_stmt.data.FunctionDecl.body,
                                                                    .closure = module_env.environment,
                                                                    .defining_module = module_env,
                                                                },
                                                            };
                                                            return self.callFunction(function, call.arguments);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // If not found in direct imports, look for qualified names (e.g., math.add)
                                    // This handles the case when safeMath imports "math" and tries to use "math.add"
                                    var it = symbols.iterator();
                                    while (it.next()) |entry| {
                                        const symbol_name = entry.key_ptr.*;

                                        // Look for symbols in the format "namespace.field_name"
                                        if (std.mem.startsWith(u8, symbol_name, namespace) and
                                            symbol_name.len > namespace.len + 1 and
                                            symbol_name[namespace.len] == '.' and
                                            std.mem.eql(u8, symbol_name[namespace.len + 1 ..], field_access.field.lexeme))
                                        {
                                            if (self.debug_enabled) {
                                                std.debug.print("Found function in qualified symbol: {s}\n", .{symbol_name});
                                            }

                                            // Get the original module that defines this function
                                            const symbol = entry.value_ptr.*;

                                            // Create function value and call it
                                            if (symbol.kind == .Function) {
                                                // Find the actual function definition
                                                const original_module = p.module_namespaces.get(namespace).?;
                                                if (original_module.ast) |module_ast| {
                                                    if (module_ast.data == .Block) {
                                                        for (module_ast.data.Block.statements) |module_stmt2| {
                                                            if (module_stmt2.data == .FunctionDecl and
                                                                std.mem.eql(u8, module_stmt2.data.FunctionDecl.name.lexeme, field_access.field.lexeme))
                                                            {
                                                                const module_env = try self.getOrCreateModuleEnvironment(namespace);
                                                                const function = TokenLiteral{
                                                                    .function = .{
                                                                        .params = module_stmt2.data.FunctionDecl.params,
                                                                        .body = module_stmt2.data.FunctionDecl.body,
                                                                        .closure = module_env.environment,
                                                                        .defining_module = module_env,
                                                                    },
                                                                };
                                                                return self.callFunction(function, call.arguments);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // If we get here, fall back to the original approach
                        // Create a temporary copy of the field access expression
                        const field_access_expr = try self.allocator.create(ast.Expr);
                        field_access_expr.* = .{
                            .base = .{
                                .id = ast.generateNodeId(),
                                .span = ast.SourceSpan.fromToken(field_access.field),
                            },
                            .data = .{ .FieldAccess = field_access },
                        };

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
                // Only allow tetra inputs
                if (left != .tetra or right != .tetra) return error.TypeError;

                const left_val = TokenLiteral{ .tetra = left.tetra };
                const right_val = TokenLiteral{ .tetra = right.tetra };

                // Store the tetra result in a comptime-known variable
                var result_tetra: Tetra = undefined;
                switch (logical.operator.type) {
                    .AND => {
                        result_tetra = switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => .false,
                            .both => if (right_val.tetra == .false) .false else .both,
                            .neither => .neither,
                        };
                    },
                    .OR => {
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
                    .IMPLIES => {
                        result_tetra = switch (left_val.tetra) {
                            .true => right_val.tetra,
                            .false => .true,
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
                return TokenLiteral{ .tetra = if (is_true) .true else .false };
            },
            .FunctionExpr => |f| TokenLiteral{
                .function = .{
                    .params = f.params,
                    .body = f.body,
                    .closure = self.environment,
                    .defining_module = null, // TODO: Set this to the actual module when available
                },
            },
            .Inspect => |insp| {
                const value = try self.evaluate(insp.expr);
                var buffer = std.ArrayList(u8).init(self.allocator);
                defer buffer.deinit();

                // Format the location information
                try buffer.writer().print("[{s}:{d}:{d}] {s} = ", .{
                    insp.location.file,
                    insp.location.line,
                    insp.location.column,
                    insp.variable_name orelse "value",
                });

                // Create a helper function to format values recursively
                try formatValue(buffer.writer(), value);
                try buffer.writer().print("\n", .{});
                try std.io.getStdOut().writeAll(buffer.items);

                return value;
            },
            .InspectStruct => {
                unreachable;
            },
            .While => |while_expr| {
                while (true) {
                    // CRITICAL FIX: Check has_returned flag before evaluating condition
                    if (self.has_returned) break;

                    const condition_result = try self.evaluate(while_expr.condition);
                    if (condition_result != .tetra) {
                        return error.TypeError;
                    }
                    if (condition_result.tetra == .false) break;

                    // Create a new scope for this iteration
                    const current_scope = self.memory_manager.scope_manager.root_scope;
                    var iteration_scope = try self.memory_manager.scope_manager.createScope(current_scope);
                    // Update the root scope to our new iteration scope
                    self.memory_manager.scope_manager.root_scope = iteration_scope;

                    // CRITICAL FIX: Evaluate body and handle ReturnValue properly
                    _ = self.evaluate(while_expr.body) catch |err| {
                        if (err == error.ReturnValue) {
                            // For ReturnValue, just restore scope - DON'T deinit yet!
                            // Let the has_returned check handle cleanup
                            self.memory_manager.scope_manager.root_scope = current_scope;
                            if (self.debug_enabled) {
                                std.debug.print("WHILE LOOP: Caught ReturnValue error, checking has_returned flag\n", .{});
                            }
                            // Continue execution to check has_returned flag below
                        } else {
                            // For other errors, restore scope and propagate immediately
                            self.memory_manager.scope_manager.root_scope = current_scope;
                            iteration_scope.deinit();
                            return err;
                        }
                    };

                    // CRITICAL FIX: ALWAYS check has_returned flag after body execution, regardless of errors
                    if (self.has_returned) {
                        if (self.debug_enabled) {
                            std.debug.print("WHILE LOOP: Detected has_returned=true, breaking loop to allow function return\n", .{});
                        }

                        // CRITICAL: Before cleaning up iteration scope, preserve return value in parent environment
                        if (iteration_scope.lookupVariable("return")) |return_var| {
                            if (self.memory_manager.scope_manager.value_storage.get(return_var.storage_id)) |storage| {
                                const return_value = storage.value;
                                if (self.debug_enabled) {
                                    std.debug.print("WHILE LOOP: Preserving return value before scope cleanup: {any}\n", .{return_value});
                                }
                                // Store return value in current environment (function environment)
                                try self.environment.define("return", return_value, .{ .base = .Nothing });
                            }
                        }

                        // Restore the original scope before breaking
                        self.memory_manager.scope_manager.root_scope = current_scope;
                        iteration_scope.deinit();
                        // Break the loop and let the function handle the return normally
                        break;
                    }

                    // Restore the original scope
                    self.memory_manager.scope_manager.root_scope = current_scope;
                    iteration_scope.deinit();
                }
                return TokenLiteral{ .nothing = {} };
            },
            .For => |for_expr| {
                // Execute initializer if present
                if (for_expr.initializer) |init_expr| {
                    _ = try self.executeStatement(init_expr, self.debug_enabled);
                }

                // Main loop
                while (true) {
                    // CRITICAL FIX: Check has_returned flag before evaluating condition
                    if (self.has_returned) break;

                    // Check condition if present
                    if (for_expr.condition) |cond| {
                        const condition_result = try self.evaluate(cond);
                        if (condition_result != .tetra) {
                            return error.TypeError;
                        }
                        if (condition_result.tetra == .false) break;
                    }

                    // Create a new scope for this iteration
                    const current_scope = self.memory_manager.scope_manager.root_scope;
                    var iteration_scope = try self.memory_manager.scope_manager.createScope(current_scope);
                    self.memory_manager.scope_manager.root_scope = iteration_scope;

                    // CRITICAL FIX: Evaluate body and handle ReturnValue properly
                    _ = self.evaluate(for_expr.body) catch |err| {
                        if (err == error.ReturnValue) {
                            // For ReturnValue, just restore scope - DON'T deinit yet!
                            // Let the has_returned check handle cleanup
                            self.memory_manager.scope_manager.root_scope = current_scope;
                            if (self.debug_enabled) {
                                std.debug.print("FOR LOOP: Caught ReturnValue error, checking has_returned flag\n", .{});
                            }
                            // Continue execution to check has_returned flag below
                        } else {
                            // For other errors, restore scope and propagate immediately
                            self.memory_manager.scope_manager.root_scope = current_scope;
                            iteration_scope.deinit();
                            return err;
                        }
                    };

                    // CRITICAL FIX: Check has_returned flag - if set, return ReturnValue error
                    if (self.has_returned) {
                        if (self.debug_enabled) {
                            std.debug.print("FOR LOOP: Detected has_returned=true, propagating ReturnValue error\n", .{});
                        }
                        // Restore the original scope before breaking
                        self.memory_manager.scope_manager.root_scope = current_scope;
                        iteration_scope.deinit();
                        // Return the ReturnValue error so function call can handle it
                        return error.ReturnValue;
                    }

                    // Execute increment if present
                    if (for_expr.increment) |incr| {
                        _ = try self.evaluate(incr);
                    }

                    // Restore the original scope
                    self.memory_manager.scope_manager.root_scope = current_scope;
                    iteration_scope.deinit();
                }
                return TokenLiteral{ .nothing = {} };
            },
            .ForEach => |foreach| {
                const array_value = try self.evaluate(foreach.array);
                if (array_value != .array) {
                    return error.TypeError;
                }

                // Create a new environment for the loop
                var iter_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                defer iter_env.deinit();

                // Infer type from array elements or use dynamic if empty
                var item_type = ast.TypeInfo{ .base = .Auto };
                if (array_value.array.len > 0) {
                    item_type = switch (array_value.array[0]) {
                        .int => ast.TypeInfo{ .base = .Int },
                        .u8 => ast.TypeInfo{ .base = .U8 },
                        .float => ast.TypeInfo{ .base = .Float },
                        .string => ast.TypeInfo{ .base = .String },
                        .tetra => ast.TypeInfo{ .base = .Tetra },
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

                return TokenLiteral{ .nothing = {} };
            },
            .FieldAccess => |field| {
                const object = try self.evaluate(field.object);

                // Handle string length property
                if (object == .string) {
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        return TokenLiteral{ .int = @intCast(object.string.len) };
                    }
                    // Add bytes field access
                    if (std.mem.eql(u8, field.field.lexeme, "bytes")) {
                        var bytes = try self.allocator.alloc(TokenLiteral, object.string.len);
                        for (object.string, 0..) |byte, i| {
                            bytes[i] = TokenLiteral{ .u8 = byte };
                        }
                        const array_value = TokenLiteral{ .array = bytes };
                        try self.environment.define(field.field.lexeme, array_value, .{ .base = .Array, .element_type = .U8, .is_mutable = true });
                        return array_value;
                    }
                }

                // Handle array properties first
                if (object == .array) {
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        return TokenLiteral{ .int = @intCast(object.array.len) };
                    }
                    return error.UnknownMethod;
                }

                // First check if this is an enum type access
                if (object == .nothing) {
                    // Try to get type info for the object
                    if (field.object.data == .Variable) {
                        // Check if this is a module namespace
                        const var_name = field.object.data.Variable.lexeme;
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
                                                if (module_ast.data == .Block) {
                                                    for (module_ast.data.Block.statements) |module_stmt3| {
                                                        if (module_stmt3.data == .FunctionDecl and
                                                            std.mem.eql(u8, module_stmt3.data.FunctionDecl.name.lexeme, field.field.lexeme))
                                                        {
                                                            if (self.debug_enabled) {
                                                                std.debug.print("Found function: {s} in module {s}\n", .{ field.field.lexeme, var_name });
                                                            }

                                                            // Create a function value
                                                            const module_env = try self.getOrCreateModuleEnvironment(var_name);
                                                            return TokenLiteral{
                                                                .function = .{
                                                                    .params = module_stmt3.data.FunctionDecl.params,
                                                                    .body = module_stmt3.data.FunctionDecl.body,
                                                                    .closure = module_env.environment,
                                                                    .defining_module = module_env,
                                                                },
                                                            };
                                                        }
                                                    }

                                                    if (self.debug_enabled) {
                                                        std.debug.print("Could not find function {s} in module statements\n", .{field.field.lexeme});
                                                        // Print all available functions in the module
                                                        std.debug.print("Available functions in module:\n", .{});
                                                        for (module_ast.data.Block.statements) |module_stmt4| {
                                                            if (module_stmt4.data == .FunctionDecl) {
                                                                std.debug.print("  {s}\n", .{module_stmt4.data.FunctionDecl.name.lexeme});
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
                                                if (module_ast.data == .Block) {
                                                    for (module_ast.data.Block.statements) |module_stmt5| {
                                                        if (module_stmt5.data == .VarDecl and
                                                            std.mem.eql(u8, module_stmt5.data.VarDecl.name.lexeme, field.field.lexeme))
                                                        {
                                                            if (self.debug_enabled) {
                                                                std.debug.print("Found variable declaration: {s}\n", .{field.field.lexeme});
                                                            }

                                                            // Evaluate the variable's initializer expression
                                                            if (module_stmt5.data.VarDecl.initializer) |init_expr| {
                                                                return self.evaluate(init_expr);
                                                            } else {
                                                                // If no initializer, return a default value based on type
                                                                return TokenLiteral{ .nothing = {} };
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
                                        return TokenLiteral{ .nothing = {} };
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
                                    if (module_ast.data == .Block) {
                                        for (module_ast.data.Block.statements) |module_stmt6| {
                                            if (module_stmt6.data == .FunctionDecl and
                                                std.mem.eql(u8, module_stmt6.data.FunctionDecl.name.lexeme, field.field.lexeme))
                                            {
                                                if (self.debug_enabled) {
                                                    std.debug.print("Found function by direct lookup: {s}\n", .{field.field.lexeme});
                                                }

                                                // Create a function value
                                                const module_env = try self.getOrCreateModuleEnvironment(var_name);
                                                return TokenLiteral{
                                                    .function = .{
                                                        .params = module_stmt6.data.FunctionDecl.params,
                                                        .body = module_stmt6.data.FunctionDecl.body,
                                                        .closure = module_env.environment,
                                                        .defining_module = module_env,
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
                        const type_info = self.environment.getTypeInfo(field.object.data.Variable.lexeme) catch |err| {
                            if (self.debug_enabled) {
                                std.debug.print("Error getting type info: {}\n", .{err});
                            }
                            return err;
                        };
                        if (type_info.base == .Enum) {
                            // This is an enum member access
                            return TokenLiteral{ .enum_variant = field.field.lexeme };
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
                var struct_fields = std.ArrayList(TypesImport.StructField).init(self.allocator);
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
                                    .Tetra => value == .tetra,
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

                return TokenLiteral{ .struct_value = .{
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
                var quantifier_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                self.environment = &quantifier_env;

                // Iterate over the actual array elements
                for (array_value.array) |val| {
                    if (self.debug_enabled) {
                        std.debug.print("Testing value: {any}\n", .{val});
                    }

                    // Create a new scope for this iteration
                    const current_scope = self.memory_manager.scope_manager.root_scope;
                    var iteration_scope = try self.memory_manager.scope_manager.createScope(current_scope);
                    self.memory_manager.scope_manager.root_scope = iteration_scope;

                    try self.environment.define(e.variable.lexeme, val, .{ .base = .Auto });
                    const result = try self.evaluate(e.condition);

                    // Restore the original scope
                    self.memory_manager.scope_manager.root_scope = current_scope;
                    iteration_scope.deinit();

                    if (self.debug_enabled) {
                        std.debug.print("Condition result: {any}\n", .{result});
                    }
                    if (result == .tetra and result.tetra == .true) {
                        return TokenLiteral{ .tetra = .true };
                    }
                }

                return TokenLiteral{ .tetra = .false };
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
                var quantifier_env = env_module.init(self.allocator, self.environment, self.debug_enabled, self.memory_manager);
                self.environment = &quantifier_env;

                // Iterate over the actual array elements
                for (array_value.array) |val| {
                    // Create a new scope for this iteration
                    const current_scope = self.memory_manager.scope_manager.root_scope;
                    var iteration_scope = try self.memory_manager.scope_manager.createScope(current_scope);
                    self.memory_manager.scope_manager.root_scope = iteration_scope;

                    try self.environment.define(var_name, val, .{ .base = .Auto });
                    const result = try self.evaluate(f.condition);

                    // Restore the original scope
                    self.memory_manager.scope_manager.root_scope = current_scope;
                    iteration_scope.deinit();

                    if (!(result == .tetra and result.tetra == .true)) {
                        return TokenLiteral{ .tetra = .false };
                    }
                }

                return TokenLiteral{ .tetra = .true };
            },
            .ArrayType => {
                return TokenLiteral{ .nothing = {} };
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
                return TokenLiteral{ .enum_variant = member.lexeme };
            },
            .DefaultArgPlaceholder => {
                return TokenLiteral{ .nothing = {} };
            },
            .TypeOf => |expr_to_check| {
                // Special case for array indexing
                if (expr_to_check.data == .Index and expr_to_check.data.Index.array.data == .Variable) {
                    const array_name = expr_to_check.data.Index.array.data.Variable.lexeme;

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
                            return TokenLiteral{ .string = "u8" };
                        }
                    }
                } else if (self.debug_enabled and expr_to_check.data == .Index) {
                    std.debug.print("\nTypeOf on Index but array is not Variable: {any}\n", .{expr_to_check.data.Index.array.data});
                }

                // Handle simple variables
                if (expr_to_check.data == .Variable) {
                    const var_token = expr_to_check.data.Variable;
                    // Get the DECLARED type info from the environment
                    const type_info = self.environment.getTypeInfo(var_token.lexeme) catch {
                        // Handle cases where the variable might not be found (shouldn't happen if code is valid)
                        const location: Reporter.Location = .{ .file = "stdin", .line = var_token.line, .column = var_token.column };
                        self.reporter.reportCompileError(location, "Variable '{s}' not found during typeof", .{var_token.lexeme});
                        return error.VariableNotFound; // Or return err
                    };

                    // Return the string based on the DECLARED type_info.base
                    return TokenLiteral{
                        .string = switch (type_info.base) {
                            .Int => "int",
                            .U8 => "u8",
                            .Float => "float",
                            .String => "string",
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
                    return TokenLiteral{
                        .string = switch (value) {
                            .int => "int",
                            .u8 => "u8",
                            .float => "float",
                            .string => "string",
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
                var tuple_values = std.ArrayList(TokenLiteral).init(self.allocator);
                errdefer tuple_values.deinit();

                // Evaluate each element in the tuple
                for (elements) |element| {
                    const value = try self.evaluate(element);
                    try tuple_values.append(value);
                }

                const owned_slice = try tuple_values.toOwnedSlice();
                errdefer self.allocator.free(owned_slice);

                return TokenLiteral{ .tuple = owned_slice };
            },
            .Map => |entries| {
                var map = std.StringHashMap(TokenLiteral).init(self.allocator);
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

                return TokenLiteral{ .map = map };
            },
            .MethodCall => |method_call| {
                if (std.mem.eql(u8, method_call.method.lexeme, "length")) {
                    const receiver_value = try self.evaluate(method_call.receiver);
                    if (receiver_value != .array) {
                        return error.TypeError;
                    }
                    return TokenLiteral{ .int = @intCast(receiver_value.array.len) };
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
                return TokenLiteral{ .int = @intCast(array_value.array.len) };
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
                return TokenLiteral{ .string = input_str };
            },
            .Assert => |assert| {
                const condition = try self.evaluate(assert.condition);
                if (condition != .tetra) {
                    self.reporter.reportRuntimeError("Assertion failed: {s} is not a tetra", .{@tagName(condition)});
                    return error.TypeError;
                }
                if (condition.tetra == .false) {

                    // Use custom message if provided
                    if (assert.message != null) {
                        const message_value = try self.evaluate(assert.message.?);
                        if (message_value == .string) {
                            self.reporter.reportCompileError(assert.location, "Assertion failed: {s}", .{message_value.string});
                        } else {
                            self.reporter.reportCompileError(assert.location, "Assertion failed", .{});
                        }
                    } else {
                        self.reporter.reportCompileError(assert.location, "Assertion failed", .{});
                    }
                    return error.AssertionFailed;
                }
                return .{ .nothing = {} };
            },
            .ReturnExpr => |return_expr| {
                if (self.debug_enabled) {
                    std.debug.print("RETURN EXPRESSION: Executing return expression\n", .{});
                }
                // Evaluate the return value if present
                const return_value = if (return_expr.value) |value|
                    try self.evaluate(value)
                else
                    TokenLiteral{ .nothing = {} };

                // Store the return value in the environment for the caller to retrieve
                try self.environment.define("return", return_value, .{ .base = .Nothing });
                self.has_returned = true;
                if (self.debug_enabled) {
                    std.debug.print("RETURN EXPRESSION: Set has_returned=true, returning error.ReturnValue\n", .{});
                }
                return error.ReturnValue;
            },
        };
    }

    fn makeNothing(self: *Interpreter) TokenLiteral {
        _ = self;
        return .{ .nothing = {} };
    }

    fn negateLogical(value: Tetra) ErrorList!TokenLiteral {
        return switch (value) {
            .neither => TokenLiteral{ .tetra = .neither },
            .both => TokenLiteral{ .tetra = .both },
            .true => TokenLiteral{ .tetra = .false },
            .false => TokenLiteral{ .tetra = .true },
        };
    }

    fn orLogical(left: Tetra, right: Tetra) !TokenLiteral {
        // neither overrides true
        if (left == .neither or right == .neither) {
            return TokenLiteral{ .tetra = .false };
        }
        // both overrides false
        if ((left == .both or left == .true) and (right == .both or right == .true)) {
            return TokenLiteral{ .tetra = .true };
        }
        if (left == .false and right == .false) {
            return TokenLiteral{ .tetra = .false };
        }
        unreachable;
    }

    fn compareLogical(left: Tetra, right: Tetra) !TokenLiteral {
        if (left == .neither or right == .neither) {
            return TokenLiteral{ .tetra = .false };
        }
        if (left == .false or right == .false) {
            return TokenLiteral{ .tetra = .false };
        }
        if ((left == .true or left == .both) and (right == .true or right == .both)) {
            return TokenLiteral{ .tetra = .true };
        }
        unreachable;
    }

    fn executeAssignment(self: *Interpreter, expr: *const ast.Expr) ErrorList!TokenLiteral {
        const assignment = expr.Assignment;
        const value = try self.evaluate(assignment.value);

        // Get variable's type info
        const var_type = try self.environment.getTypeInfo(assignment.name.lexeme);

        // Check mutability
        if (!var_type.is_mutable) {
            return error.ConstAssignment;
        }

        // Type checking (always enabled in statically typed mode)
        switch (var_type.base) {
            .Int => {
                if (value != .int and value != .u8) {
                    self.reporter.reportRuntimeError("Type error: Cannot assign {s} to int variable", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .U8 => {
                if (value != .u8 and value != .int) {
                    self.reporter.reportRuntimeError("Type error: Cannot assign {s} to u8 variable", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Float => {
                if (value != .float) {
                    self.reporter.reportRuntimeError("Type error: Cannot assign {s} to float variable", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .String => {
                if (value != .string) {
                    self.reporter.reportRuntimeError("Type error: Cannot assign {s} to string variable", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Tetra => {
                if (value != .tetra) {
                    self.reporter.reportRuntimeError("Type error: Cannot assign {s} to tetra variable", .{@tagName(value)});
                    return error.TypeError;
                }
            },
            .Auto => {}, // Type is already fixed from initialization
            else => {
                const location: Reporter.Location = .{ .file = "stdin", .line = assignment.name.line, .column = assignment.name.column };
                self.reporter.reportCompileError(location, "Type error: Cannot assign {s} to variable", .{@tagName(value)});
                return error.TypeError;
            },
        }

        try self.environment.assign(assignment.name.lexeme, value);
        return value;
    }

    pub fn callFunction(self: *Interpreter, callee: TokenLiteral, arguments: []const *ast.Expr) ErrorList!TokenLiteral {
        switch (callee) {
            .function => |f| {
                // Determine the base environment for the function call
                var base_environment = f.closure;

                // If this function has a defining module, use the module's environment as the base
                if (f.defining_module) |module_env| {
                    base_environment = module_env.environment;
                    if (self.debug_enabled) {
                        std.debug.print("Using module environment for function: {s}\n", .{module_env.module_name});
                    }
                } else if (self.debug_enabled) {
                    std.debug.print("Function has no defining module, using closure environment\n", .{});
                }

                // Create new environment for function call
                var function_env = env_module.init(self.allocator, base_environment, self.debug_enabled, self.memory_manager);
                defer function_env.deinit();

                // If this function has a defining module, set the module reference in the function environment
                if (f.defining_module) |module_env| {
                    function_env.module = module_env;
                    if (self.debug_enabled) {
                        std.debug.print("Set function environment module reference to: {s}\n", .{module_env.module_name});
                    }
                }

                if (self.debug_enabled) {
                    std.debug.print("\n=== Function Call Environment Debug ===\n", .{});
                    std.debug.print("Function closure environment: {*}\n", .{f.closure});
                    std.debug.print("Current interpreter environment: {*}\n", .{self.environment});
                    std.debug.print("New function environment: {*}\n", .{&function_env});

                    // Check if parser has module namespaces available
                    if (self.parser) |p| {
                        std.debug.print("Parser module namespaces count: {d}\n", .{p.module_namespaces.count()});
                        var namespace_it = p.module_namespaces.iterator();
                        while (namespace_it.next()) |entry| {
                            std.debug.print("  Available namespace: {s}\n", .{entry.key_ptr.*});
                        }

                        if (p.imported_symbols) |symbols| {
                            std.debug.print("Parser imported symbols count: {d}\n", .{symbols.count()});
                            var symbol_it = symbols.iterator();
                            while (symbol_it.next()) |sym_entry| {
                                std.debug.print("  Available imported symbol: {s}\n", .{sym_entry.key_ptr.*});
                            }
                        }
                    }
                    std.debug.print("=====================================\n", .{});
                }

                // Create a new scope for this function call, ensure it's a child of the current scope
                const current_scope = self.memory_manager.scope_manager.root_scope;
                var function_scope = try self.memory_manager.scope_manager.createScope(current_scope);

                // Update the memory manager to use the new scope
                self.memory_manager.scope_manager.root_scope = function_scope;

                // Make sure to restore the original scope when done
                defer {
                    self.memory_manager.scope_manager.root_scope = current_scope;
                    function_scope.deinit();
                }

                // Check argument count
                if (arguments.len > f.params.len) {
                    return error.TooManyArguments;
                }

                // Evaluate and bind arguments to parameters
                for (f.params, 0..) |param, i| {
                    var value: TokenLiteral = undefined;

                    if (i < arguments.len) {
                        const arg = arguments[i];
                        if (arg.data == .DefaultArgPlaceholder) {
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
                        type_info = .{ .base = .Auto, .is_mutable = true };
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

    fn moveValueToParentEnv(self: *Interpreter, value: TokenLiteral) !TokenLiteral {
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

                var new_array = try self.allocator.alloc(TokenLiteral, arr.len);
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

                            const result = TokenLiteral{ .string = interned };
                            if (self.debug_enabled) {
                                std.debug.print("  Created token: {any}\n", .{result});
                            }
                            break :blk2 result;
                        },
                        .int => |n| TokenLiteral{ .int = n },
                        .u8 => |u| TokenLiteral{ .u8 = u },
                        .float => |fl| TokenLiteral{ .float = fl },
                        .tetra => |t| TokenLiteral{ .tetra = t },
                        .nothing => TokenLiteral{ .nothing = {} },
                        .array => |nested| try self.moveValueToParentEnv(TokenLiteral{ .array = nested }),
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

                const result = TokenLiteral{ .array = new_array };
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

    fn assignField(self: *Interpreter, object: TokenLiteral, field: Token, value: *ast.Expr) ErrorList!TokenLiteral {
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

    fn compareLiteralValues(a: anytype, b: @TypeOf(a)) TokenLiteral {
        if (a == b) {
            return TokenLiteral{ .tetra = .true };
        } else {
            return TokenLiteral{ .tetra = .false };
        }
    }

    fn valuesEqual(self: *Interpreter, a: TokenLiteral, b: TokenLiteral) ErrorList!bool {
        return switch (a) {
            .int => |val| b == .int and val == b.int,
            .u8 => |val| b == .u8 and val == b.u8,
            .float => |val| b == .float and val == b.float,
            .string => |val| b == .string and std.mem.eql(u8, val, b.string),
            .nothing => b == .nothing,
            .tetra => {
                if (b != .tetra) return false;
                const result = try compareLogical(a.tetra, b.tetra);
                return result.tetra == .true;
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

                // Check that all key-value pairs pr
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
        method: Token,
        arguments: []const *ast.Expr,
    };

    fn callMethod(self: *Interpreter, method_call: MethodCallExpr) ErrorList!TokenLiteral {
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
                var new_array = try self.allocator.alloc(TokenLiteral, current_array.len + 1);
                errdefer self.allocator.free(new_array);

                // Copy existing elements
                @memcpy(new_array[0..current_array.len], current_array);

                // Add new element
                new_array[current_array.len] = arg_value;

                // Free old array
                self.allocator.free(current_array);

                // Create new token literal with the new array
                const new_value = TokenLiteral{ .array = new_array };

                // Update the variable in the environment
                if (method_call.receiver.data == .Variable) {
                    try self.environment.assign(method_call.receiver.data.Variable.lexeme, new_value);
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
                return TokenLiteral{ .int = @intCast(receiver_value.string.len) };
            }
        }

        // Handle other types' methods here
        return error.MethodNotFound;
    }

    fn arrayIsEmpty(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!TokenLiteral {
        const array_value = try self.evaluate(array_expr);
        return TokenLiteral{ .tetra = if (array_value.array.len == 0)
            .true
        else
            .false };
    }

    fn arrayPop(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!TokenLiteral {
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
        var new_array = try self.allocator.alloc(TokenLiteral, current_array.len - 1);
        errdefer self.allocator.free(new_array);

        // Copy all elements except the last one
        @memcpy(new_array[0..(current_array.len - 1)], current_array[0..(current_array.len - 1)]);

        // Update the array in the environment if it's a variable
        if (array_expr.data == .Variable) {
            try self.environment.assign(array_expr.data.Variable.lexeme, .{ .array = new_array });
        }

        // Free old array
        self.allocator.free(current_array);

        // Return the popped value
        return popped_value;
    }

    fn arrayLength(self: *Interpreter, array_expr: *const ast.Expr) ErrorList!TokenLiteral {
        const array_value = try self.evaluate(array_expr);
        if (array_value != .array) {
            return error.TypeError;
        }
        return TokenLiteral{ .int = @intCast(array_value.array.len) };
    }

    fn arrayConcat(self: *Interpreter, array: *ast.Expr, array2: *ast.Expr) ErrorList!TokenLiteral {
        const array_value = try self.evaluate(array);
        const array2_value = try self.evaluate(array2);

        // Create new array with combined length
        var new_array = try self.allocator.alloc(TokenLiteral, array_value.array.len + array2_value.array.len);
        errdefer self.allocator.free(new_array);

        // Copy first array
        @memcpy(new_array[0..array_value.array.len], array_value.array);

        // Copy second array
        @memcpy(new_array[array_value.array.len..], array2_value.array);

        return TokenLiteral{ .array = new_array };
    }

    fn arrayPush(self: *Interpreter, array: *ast.Expr, element: *ast.Expr) ErrorList!TokenLiteral {
        // Create a method call expression using the PUSH token
        const method_call = MethodCallExpr{
            .receiver = array,
            .method = .{
                .type = .IDENTIFIER,
                .lexeme = "push",
                .literal = .{ .nothing = {} }, // Use .nothing instead of null
                .line = 0, // Since this is synthetic, we use 0
                .column = 0, // Since this is synthetic, we use 0
                .file = "", // Since this is synthetic, we use empty string
            },
            .arguments = &[_]*ast.Expr{element},
        };

        // Use the existing callMethod implementation
        return try self.callMethod(method_call);
    }
};

// Add this helper function outside the switch statement
fn formatValue(writer: anytype, value: TokenLiteral) !void {
    switch (value) {
        .tetra => |t| try writer.print("{s}", .{@tagName(t)}),
        .string => |s| try writer.print("\"{s}\"", .{s}),
        .int => |i| try writer.print("{d}", .{i}),
        .u8 => |u| try writer.print("{d}", .{u}),
        .float => |f| try writer.print("{d}", .{f}),
        .nothing => try writer.print("nothing", .{}),
        .function => try writer.print("function", .{}),
        .array => |arr| {
            try writer.print("[", .{});
            for (arr, 0..) |item, i| {
                if (i > 0) try writer.print(", ", .{});
                try formatValue(writer, item);
            }
            try writer.print("]", .{});
        },
        .struct_value => |sv| {
            try writer.print("{s} {{", .{sv.type_name});
            for (sv.fields, 0..) |field, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("{s}: ", .{field.name});
                try formatValue(writer, field.value);
            }
            try writer.print("}}", .{});
        },
        .enum_variant => |variant| try writer.print(".{s}", .{variant}),
        .tuple => |tup| {
            try writer.print("(", .{});
            for (tup, 0..) |item, i| {
                if (i > 0) try writer.print(", ", .{});
                try formatValue(writer, item);
            }
            try writer.print(")", .{});
        },
        .map => |m| {
            try writer.print("{{", .{});
            var iter = m.iterator();
            var first = true;
            while (iter.next()) |entry| {
                if (!first) try writer.print(", ", .{});
                first = false;
                try writer.print("{s}: ", .{entry.key_ptr.*});
                try formatValue(writer, entry.value_ptr.*);
            }
            try writer.print("}}", .{});
        },
    }
}
