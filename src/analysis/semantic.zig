const std = @import("std");
const ast = @import("../ast/ast.zig");
const Memory = @import("../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorList = Reporting.ErrorList;

const Types = @import("../types/types.zig");
const TokenLiteral = Types.TokenLiteral;

const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;

// Fix the import at the top to make Token public
const Token = @import("../types/token.zig").Token;

//======================================================================

const NodeId = u32; // Or whatever your AST uses

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, memory: *MemoryManager) SemanticAnalyzer {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .fatal_error = false,
            .current_scope = null,
            .type_cache = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        // Just clear the cache - the TypeInfo instances are owned by the AST
        // or will be cleaned up by the memory manager
        self.type_cache.deinit();
    }

    pub fn analyze(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        const root_scope = try self.memory.scope_manager.createScope(null);
        self.memory.scope_manager.root_scope = root_scope;
        self.current_scope = root_scope;

        try self.collectDeclarations(statements, root_scope);

        if (self.fatal_error) {
            return error.SemanticError;
        }

        try self.validateStatements(statements);
    }

    fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // Create TypeInfo
                    const type_info = try self.allocator.create(ast.TypeInfo);
                    errdefer self.allocator.destroy(type_info);

                    // Check if we have an explicit type annotation
                    if (decl.type_info.base != .Nothing) {
                        // Use explicit type, but resolve custom types
                        type_info.* = try self.resolveTypeInfo(decl.type_info);
                        if (self.reporter.is_debug) {
                            std.debug.print("DEBUG: Variable '{s}' has explicit type: {s}\n", .{ decl.name.lexeme, @tagName(type_info.base) });
                        }
                    } else if (decl.initializer) |init_expr| {
                        // Infer from initializer
                        const inferred = try self.inferTypeFromExpr(init_expr);
                        type_info.* = inferred.*;
                        if (self.reporter.is_debug) {
                            std.debug.print("DEBUG: Variable '{s}' inferred type: {s}\n", .{ decl.name.lexeme, @tagName(type_info.base) });
                        }
                    } else {
                        // Check if this is an explicit nothing type (no initializer but type is Nothing)
                        // This is a special case for nothing type variables
                        if (decl.type_info.base == .Nothing) {
                            type_info.* = decl.type_info;
                            if (self.reporter.is_debug) {
                                std.debug.print("DEBUG: Variable '{s}' has explicit nothing type\n", .{decl.name.lexeme});
                            }
                        } else {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Variable declaration requires either type annotation or initializer",
                                .{},
                            );
                            self.fatal_error = true;
                            continue;
                        }
                    }

                    // Convert TypeInfo to TokenType
                    const token_type = self.convertTypeToTokenType(type_info.base);
                    if (self.reporter.is_debug) {
                        std.debug.print("DEBUG: Variable '{s}' token_type: {s}\n", .{ decl.name.lexeme, @tagName(token_type) });
                    }

                    // Get the actual value from initializer or use default for uninitialized variables
                    var value = if (decl.initializer) |init_expr|
                        try self.evaluateExpression(init_expr)
                    else
                        // Only use defaults for uninitialized variables
                        switch (type_info.base) {
                            .Int => TokenLiteral{ .int = 0 },
                            .Float => TokenLiteral{ .float = 0.0 },
                            .String => TokenLiteral{ .string = "" },
                            .Tetra => TokenLiteral{ .tetra = .false },
                            .Byte => TokenLiteral{ .byte = 0 },
                            else => TokenLiteral{ .nothing = {} },
                        };

                    // Convert value to match the declared type
                    value = try self.convertValueToType(value, type_info.base);

                    // Add to scope
                    _ = scope.createValueBinding(
                        decl.name.lexeme,
                        value,
                        token_type,
                        type_info.*,
                        !type_info.is_mutable,
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate variable name '{s}' in current scope",
                                .{decl.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };
                },
                .Block => |block_stmts| {
                    const block_scope = try self.memory.scope_manager.createScope(scope);
                    try self.collectDeclarations(block_stmts, block_scope);
                    block_scope.deinit();
                },
                .FunctionDecl => |func| {
                    // Create function type
                    const func_type = try self.allocator.create(ast.FunctionType);
                    // Set up parameter types and return type
                    // Add function to current scope
                    const func_type_info = try self.allocator.create(ast.TypeInfo);
                    func_type_info.* = .{ .base = .Function, .function_type = func_type };
                    var env = @import("../interpreter/environment.zig").init(
                        self.allocator,
                        null, // no enclosing environment for function declarations
                        false, // debug disabled
                        self.memory,
                    );
                    _ = scope.createValueBinding(
                        func.name.lexeme,
                        TokenLiteral{ .function = .{
                            .params = func.params,
                            .body = func.body,
                            .closure = &env,
                            .defining_module = null,
                        } },
                        .FUNCTION,
                        func_type_info.*,
                        true,
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate function name '{s}' in current scope",
                                .{func.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };
                },
                .EnumDecl => |enum_decl| {
                    // Register the enum type in the current scope
                    const enum_type_info = try self.allocator.create(ast.TypeInfo);
                    enum_type_info.* = .{ .base = .Enum, .custom_type = enum_decl.name.lexeme, .is_mutable = false };

                    // Create a placeholder value for the enum type
                    const enum_value = TokenLiteral{ .string = enum_decl.name.lexeme };

                    _ = scope.createValueBinding(
                        enum_decl.name.lexeme,
                        enum_value,
                        .ENUM,
                        enum_type_info.*,
                        true, // Enum types are constants
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate enum name '{s}' in current scope",
                                .{enum_decl.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };
                },

                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        // Handle struct declarations that are expressions
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.data.StructDecl;
                            // Register the struct type in the current scope
                            const struct_type_info = try self.allocator.create(ast.TypeInfo);
                            struct_type_info.* = .{ .base = .Struct, .custom_type = struct_decl.name.lexeme, .is_mutable = false };

                            // Create a placeholder value for the struct type
                            const struct_value = TokenLiteral{ .string = struct_decl.name.lexeme };

                            _ = scope.createValueBinding(
                                struct_decl.name.lexeme,
                                struct_value,
                                .STRUCT,
                                struct_type_info.*,
                                true, // Struct types are constants
                            ) catch |err| {
                                if (err == error.DuplicateVariableName) {
                                    self.reporter.reportCompileError(
                                        stmt.base.span.start,
                                        "Duplicate struct name '{s}' in current scope",
                                        .{struct_decl.name.lexeme},
                                    );
                                    self.fatal_error = true;
                                    continue;
                                } else {
                                    return err;
                                }
                            };
                        }
                    }
                },
                // TODO: Handle other declarations...
                else => {},
            }
        }
    }

    fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) TokenType {
        _ = self;
        return switch (base_type) {
            .Int => .INT,
            .Float => .FLOAT,
            .String => .STRING,
            .Tetra => .TETRA,
            .Byte => .BYTE,
            .Array => .ARRAY,
            .Function => .FUNCTION,
            .Struct => .STRUCT,
            .Nothing => .NOTHING,
            .Map => .MAP,
            .Custom => .CUSTOM,
            .Alias => .ALIAS,
            .Enum => .ENUM,
            .Union => .UNION,
        };
    }

    fn validateStatements(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    if (decl.initializer) |init_expr| {
                        const init_type = try self.inferTypeFromExpr(init_expr);
                        if (decl.type_info.base != .Nothing) {
                            // Check initializer matches declared type
                            // Resolve the declared type first, then create a mutable copy for unifyTypes
                            const resolved_type = try self.resolveTypeInfo(decl.type_info);
                            var decl_type_copy = resolved_type;
                            try self.unifyTypes(&decl_type_copy, init_type, stmt.base.span);
                        }
                    }
                },
                .Block => |block_stmts| {
                    const prev_scope = self.current_scope;
                    self.current_scope = try self.memory.scope_manager.createScope(self.current_scope);
                    defer {
                        self.current_scope = prev_scope;
                        // Scope cleanup handled by memory manager
                    }
                    try self.validateStatements(block_stmts);
                },

                // TODO: Handle other statements...
                else => {},
            }
        }
    }

    fn unifyTypes(self: *SemanticAnalyzer, expected: *ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
        if (expected.base != actual.base) {
            // Special cases for type conversions
            if (expected.base == .Float and (actual.base == .Int or actual.base == .Byte)) {
                return; // Allow implicit int/byte to float
            }
            if (expected.base == .Byte and actual.base == .Int) {
                // Allow int to byte conversion if the value is in range
                // We'll need to check the actual value during evaluation
                return; // Allow implicit int to byte
            }
            if (expected.base == .Int and actual.base == .Byte) {
                return; // Allow byte to int conversion (always safe)
            }
            // Allow enum variant literals (Enum) to be assigned to variables of enum types (Custom)
            if (expected.base == .Custom and actual.base == .Enum) {
                return; // Allow enum variant literals to be assigned to enum type variables
            }
            // Allow struct literals (Custom) to be assigned to variables of struct types (Custom)
            if (expected.base == .Custom and actual.base == .Custom) {
                return; // Allow struct literals to be assigned to struct type variables
            }

            self.reporter.reportCompileError(
                span.start,
                "Type mismatch: expected {s}, got {s}",
                .{ @tagName(expected.base), @tagName(actual.base) },
            );
            self.fatal_error = true;
        }

        // Handle complex type unification (arrays, structs, etc)
        switch (expected.base) {
            .Array => {
                if (expected.array_type) |exp_elem| {
                    if (actual.array_type) |act_elem| {
                        try self.unifyTypes(exp_elem, act_elem, span);
                    }
                }
            },
            .Struct => {
                if (expected.struct_fields) |exp_fields| {
                    if (actual.struct_fields) |act_fields| {
                        if (exp_fields.len != act_fields.len) {
                            self.reporter.reportCompileError(
                                span.start,
                                "Struct field count mismatch: expected {}, got {}",
                                .{ exp_fields.len, act_fields.len },
                            );
                            self.fatal_error = true;
                            return;
                        }
                        for (exp_fields, act_fields) |exp_field, act_field| {
                            if (!std.mem.eql(u8, exp_field.name, act_field.name)) {
                                self.reporter.reportCompileError(
                                    span.start,
                                    "Struct field name mismatch: expected '{s}', got '{s}'",
                                    .{ exp_field.name, act_field.name },
                                );
                                self.fatal_error = true;
                                return;
                            }
                            try self.unifyTypes(exp_field.type_info, act_field.type_info, span);
                        }
                    }
                }
            },
            // Handle other complex types...
            else => {},
        }
    }

    fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) !*ast.TypeInfo {
        // Check cache first
        if (self.type_cache.get(expr.base.id)) |cached| {
            return cached;
        }

        var type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        switch (expr.data) {
            .Literal => |lit| {
                type_info.inferFrom(lit); // TokenLiteral is already the right type
            },
            .Binary => |bin| {
                const left_type = try self.inferTypeFromExpr(bin.left.?);
                const right_type = try self.inferTypeFromExpr(bin.right.?);

                // Operator-specific type rules
                const op = bin.operator.lexeme;

                if (std.mem.eql(u8, op, "/")) {
                    // Division always returns float
                    if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Division requires numeric operands, got {s}",
                            .{@tagName(left_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Division requires numeric operands, got {s}",
                            .{@tagName(right_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = .{ .base = .Float };
                } else if (std.mem.eql(u8, op, "%")) {
                    // Modulo for integers and bytes
                    if ((left_type.base != .Int and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Modulo requires integer or byte operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    // Return type follows same promotion rules as arithmetic
                    if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else if (std.mem.eql(u8, op, "+") or std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                    // Arithmetic with type promotion
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Arithmetic requires numeric operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // Type promotion rules: Float > Int > Byte
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else if (std.mem.eql(u8, op, "**") or std.mem.eql(u8, op, "^")) {
                    // Exponentiation promotes to float for safety
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Exponentiation requires numeric operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = .{ .base = .Float };
                } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
                    std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!="))
                {
                    // Comparison operators return boolean
                    // Allow mixed numeric types for comparison
                    const left_numeric = (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte);
                    const right_numeric = (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte);

                    if (left_numeric and right_numeric) {
                        // Both are numeric - allow comparison
                        type_info.* = .{ .base = .Tetra };
                    } else if (left_type.base == right_type.base) {
                        // Same type - allow comparison
                        type_info.* = .{ .base = .Tetra };
                    } else {
                        // Incompatible types for comparison
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Cannot compare {s} with {s}",
                            .{ @tagName(left_type.base), @tagName(right_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    // Default case: unify types and use left type
                    try self.unifyTypes(left_type, right_type, expr.base.span);
                    type_info.* = left_type.*;
                }
            },
            .Variable => |var_token| {
                // Look up in current and parent scopes
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        type_info.* = storage.type_info;
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Internal error: Variable storage not found",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Undefined variable",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            },
            .Unary => |unary| {
                const operand_type = try self.inferTypeFromExpr(unary.right.?);
                // Handle unary operators: -, !, ~, etc.
                type_info.* = operand_type.*;
            },
            .Call => |call| {
                const callee_type = try self.inferTypeFromExpr(call.callee);
                if (callee_type.base == .Function) {
                    if (callee_type.function_type) |func_type| {
                        type_info.* = func_type.return_type.*;
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Function type has no return type information",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot call non-function type {s}",
                        .{@tagName(callee_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            },
            .Index => |index| {
                const array_type = try self.inferTypeFromExpr(index.array);
                const index_type = try self.inferTypeFromExpr(index.index);

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot index non-array type {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Array index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    type_info.* = elem_type.*;
                } else {
                    type_info.base = .Nothing;
                }
            },
            .FieldAccess => |field| {
                const object_type = try self.inferTypeFromExpr(field.object);
                if (object_type.base == .Struct) {
                    if (object_type.struct_fields) |fields| {
                        for (fields) |struct_field| {
                            if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                                type_info.* = struct_field.type_info.*;
                                break;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Field '{s}' not found in struct",
                                .{field.field.lexeme},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Struct has no fields defined",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (object_type.base == .Enum or object_type.base == .Custom) {
                    // Allow enum variant access: Color.Red
                    // You may want to check if the variant exists, but for now just return Enum type
                    type_info.* = .{ .base = .Enum };
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot access field on non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            },
            .Array => |elements| {
                if (elements.len == 0) {
                    type_info.* = .{ .base = .Array, .array_type = null };
                } else {
                    const first_type = try self.inferTypeFromExpr(elements[0]);
                    for (elements[1..]) |element| {
                        const element_type = try self.inferTypeFromExpr(element);
                        try self.unifyTypes(first_type, element_type, expr.base.span);
                    }
                    const array_type = try self.allocator.create(ast.TypeInfo);
                    array_type.* = first_type.*;
                    type_info.* = .{ .base = .Array, .array_type = array_type };
                }
            },
            .Struct => |fields| {
                const struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                for (fields, struct_fields) |field, *struct_field| {
                    const field_type = try self.inferTypeFromExpr(field.value);
                    struct_field.* = .{
                        .name = field.name.lexeme,
                        .type_info = field_type,
                    };
                }
                type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
            },
            .If => |if_expr| {
                const condition_type = try self.inferTypeFromExpr(if_expr.condition.?);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                const then_type = try self.inferTypeFromExpr(if_expr.then_branch.?);
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferTypeFromExpr(else_branch);
                    try self.unifyTypes(then_type, else_type, expr.base.span);
                }
                type_info.* = then_type.*;
            },
            .Logical => |logical| {
                const left_type = try self.inferTypeFromExpr(logical.left);
                const right_type = try self.inferTypeFromExpr(logical.right);

                if (left_type.base != .Tetra or right_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Logical operators require boolean operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra };
            },
            .While => |while_expr| {
                const condition_type = try self.inferTypeFromExpr(while_expr.condition);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "While condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                _ = try self.inferTypeFromExpr(while_expr.body);
                type_info.* = .{ .base = .Nothing }; // While expressions have no value
            },
            .For => |for_expr| {
                if (for_expr.condition) |condition| {
                    const condition_type = try self.inferTypeFromExpr(condition);
                    if (condition_type.base != .Tetra) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "For condition must be boolean, got {s}",
                            .{@tagName(condition_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }

                _ = try self.inferTypeFromExpr(for_expr.body);
                type_info.* = .{ .base = .Nothing }; // For expressions have no value
            },
            .ForEach => |foreach_expr| {
                const array_type = try self.inferTypeFromExpr(foreach_expr.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForEach requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Analyze the body statements
                for (foreach_expr.body) |stmt| {
                    try self.validateStatements(&[_]ast.Stmt{stmt});
                }

                type_info.* = .{ .base = .Nothing }; // ForEach expressions have no value
            },
            .Match => |match_expr| {
                _ = try self.inferTypeFromExpr(match_expr.value);

                // Analyze all cases and ensure they have compatible types
                if (match_expr.cases.len > 0) {
                    const first_case_type = try self.inferTypeFromExpr(match_expr.cases[0].body);
                    for (match_expr.cases[1..]) |case| {
                        const case_type = try self.inferTypeFromExpr(case.body);
                        try self.unifyTypes(first_case_type, case_type, expr.base.span);
                    }
                    type_info.* = first_case_type.*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .Grouping => |grouped_expr| {
                if (grouped_expr) |expr_in_parens| {
                    type_info.* = (try self.inferTypeFromExpr(expr_in_parens)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .Assignment => |assign| {
                if (assign.value) |value| {
                    const value_type = try self.inferTypeFromExpr(value);
                    // Look up variable in scope
                    if (self.lookupVariable(assign.name.lexeme)) |variable| {
                        // Check if variable is mutable
                        if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                            if (storage.constant) {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(&storage.type_info, value_type, expr.base.span);
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Undefined variable '{s}'",
                            .{assign.name.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }
                type_info.* = .{ .base = .Nothing }; // Assignment expressions have no value
            },
            .CompoundAssign => |compound_assign| {
                if (compound_assign.value) |value| {
                    const value_type = try self.inferTypeFromExpr(value);
                    // Look up variable in scope
                    if (self.lookupVariable(compound_assign.name.lexeme)) |variable| {
                        // Check if variable is mutable
                        if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                            if (storage.constant) {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{compound_assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(&storage.type_info, value_type, expr.base.span);
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Undefined variable '{s}'",
                            .{compound_assign.name.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }
                type_info.* = .{ .base = .Nothing }; // Compound assignment expressions have no value
            },
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    type_info.* = (try self.inferTypeFromExpr(value)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .TypeOf => |type_of_expr| {
                _ = try self.inferTypeFromExpr(type_of_expr);
                type_info.* = .{ .base = .String }; // typeof returns a string representation
            },
            .LengthOf => |length_of_expr| {
                const array_type = try self.inferTypeFromExpr(length_of_expr);
                if (array_type.base != .Array and array_type.base != .String) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "LengthOf requires array or string type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Int }; // LengthOf returns an integer
            },
            .BytesOf => |bytes_of_expr| {
                const value_type = try self.inferTypeFromExpr(bytes_of_expr);
                if (value_type.base != .String) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "BytesOf requires string type, got {s}",
                        .{@tagName(value_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Int }; // BytesOf returns an integer
            },
            .Map => |map_entries| {
                if (map_entries.len == 0) {
                    type_info.* = .{ .base = .Map };
                } else {
                    // Analyze key and value types from first entry
                    const first_key_type = try self.inferTypeFromExpr(map_entries[0].key);
                    const first_value_type = try self.inferTypeFromExpr(map_entries[0].value);

                    // Ensure all keys and values have consistent types
                    for (map_entries[1..]) |entry| {
                        const key_type = try self.inferTypeFromExpr(entry.key);
                        const value_type = try self.inferTypeFromExpr(entry.value);
                        try self.unifyTypes(first_key_type, key_type, expr.base.span);
                        try self.unifyTypes(first_value_type, value_type, expr.base.span);
                    }

                    type_info.* = .{ .base = .Map };
                }
            },
            .ArrayPush => |array_push| {
                const array_type = try self.inferTypeFromExpr(array_push.array);
                const element_type = try self.inferTypeFromExpr(array_push.element);

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayPush requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |expected_elem_type| {
                    try self.unifyTypes(expected_elem_type, element_type, expr.base.span);
                }

                type_info.* = .{ .base = .Nothing }; // ArrayPush has no return value
            },
            .ArrayLength => |array_length| {
                const array_type = try self.inferTypeFromExpr(array_length.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayLength requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Int }; // ArrayLength returns an integer
            },
            .ArrayPop => |array_pop| {
                const array_type = try self.inferTypeFromExpr(array_pop.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayPop requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    type_info.* = elem_type.*; // ArrayPop returns the element type
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .ArrayIsEmpty => |array_is_empty| {
                const array_type = try self.inferTypeFromExpr(array_is_empty.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayIsEmpty requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Tetra }; // ArrayIsEmpty returns a boolean
            },
            .ArrayConcat => |array_concat| {
                const array1_type = try self.inferTypeFromExpr(array_concat.array);
                const array2_type = try self.inferTypeFromExpr(array_concat.array2);

                if (array1_type.base != .Array or array2_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayConcat requires array types",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Ensure both arrays have compatible element types
                if (array1_type.array_type) |elem1| {
                    if (array2_type.array_type) |elem2| {
                        try self.unifyTypes(elem1, elem2, expr.base.span);
                        type_info.* = array1_type.*; // Return type of first array
                    } else {
                        type_info.* = array1_type.*;
                    }
                } else {
                    type_info.* = array2_type.*;
                }
            },
            .MethodCall => |method_call| {
                const receiver_type = try self.inferTypeFromExpr(method_call.receiver);

                // For now, assume method calls return the same type as the receiver
                // In a more sophisticated implementation, you'd look up method signatures
                type_info.* = receiver_type.*;
            },
            .EnumMember => {
                // Enum members have the enum type
                type_info.* = .{ .base = .Enum };
            },
            .DefaultArgPlaceholder => {
                type_info.* = .{ .base = .Nothing };
            },
            .Input => {
                type_info.* = .{ .base = .String }; // Input returns a string
            },
            .Peek => |_peek| {
                const expr_type = try self.inferTypeFromExpr(_peek.expr);
                type_info.* = expr_type.*; // Peek returns the same type as the expression
            },
            .PeekStruct => |_peek_struct| {
                const expr_type = try self.inferTypeFromExpr(_peek_struct.expr);
                type_info.* = expr_type.*; // PeekStruct returns the same type as the expression
            },
            .IndexAssign => |index_assign| {
                const array_type = try self.inferTypeFromExpr(index_assign.array);
                const index_type = try self.inferTypeFromExpr(index_assign.index);
                const value_type = try self.inferTypeFromExpr(index_assign.value);

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot assign to index of non-array type {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Array index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    try self.unifyTypes(elem_type, value_type, expr.base.span);
                }

                type_info.* = .{ .base = .Nothing }; // IndexAssign has no return value
            },
            .FieldAssignment => |field_assign| {
                const object_type = try self.inferTypeFromExpr(field_assign.object);
                const value_type = try self.inferTypeFromExpr(field_assign.value);

                if (object_type.base != .Struct) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot assign to field of non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (object_type.struct_fields) |fields| {
                    for (fields) |struct_field| {
                        if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                            try self.unifyTypes(struct_field.type_info, value_type, expr.base.span);
                            break;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Field '{s}' not found in struct",
                            .{field_assign.field.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }

                type_info.* = .{ .base = .Nothing }; // FieldAssignment has no return value
            },
            .Exists => |exists| {
                const array_type = try self.inferTypeFromExpr(exists.array);

                // Create a temporary scope for the bound variable
                const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope);

                // Add the bound variable to the quantifier scope
                // Infer the type from the array element type
                const bound_var_type = if (array_type.array_type) |elem_type|
                    elem_type.*
                else
                    ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

                _ = quantifier_scope.createValueBinding(
                    exists.variable.lexeme, // "e"
                    TokenLiteral{ .nothing = {} },
                    self.convertTypeToTokenType(bound_var_type.base),
                    bound_var_type,
                    true, // Bound variables are mutable
                ) catch |err| {
                    if (err == error.DuplicateVariableName) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Duplicate bound variable name '{s}' in exists quantifier",
                            .{exists.variable.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        quantifier_scope.deinit();
                        return type_info;
                    } else {
                        quantifier_scope.deinit();
                        return err;
                    }
                };

                // Temporarily set current scope to quantifier scope for condition analysis
                const prev_scope = self.current_scope;
                self.current_scope = quantifier_scope;

                const condition_type = try self.inferTypeFromExpr(exists.condition);

                // Restore previous scope
                self.current_scope = prev_scope;

                // Clean up the quantifier scope
                quantifier_scope.deinit();

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Exists requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Exists condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra }; // Exists returns a boolean
            },
            .ForAll => |for_all| {
                const array_type = try self.inferTypeFromExpr(for_all.array);

                // Create a temporary scope for the bound variable
                const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope);

                // Add the bound variable to the quantifier scope
                // Infer the type from the array element type
                const bound_var_type = if (array_type.array_type) |elem_type|
                    elem_type.*
                else
                    ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

                _ = quantifier_scope.createValueBinding(
                    for_all.variable.lexeme, // "u"
                    TokenLiteral{ .nothing = {} },
                    self.convertTypeToTokenType(bound_var_type.base),
                    bound_var_type,
                    true, // Bound variables are mutable
                ) catch |err| {
                    if (err == error.DuplicateVariableName) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Duplicate bound variable name '{s}' in forall quantifier",
                            .{for_all.variable.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        quantifier_scope.deinit();
                        return type_info;
                    } else {
                        quantifier_scope.deinit();
                        return err;
                    }
                };

                // Temporarily set current scope to quantifier scope for condition analysis
                const prev_scope = self.current_scope;
                self.current_scope = quantifier_scope;

                const condition_type = try self.inferTypeFromExpr(for_all.condition);

                // Restore previous scope
                self.current_scope = prev_scope;

                // Clean up the quantifier scope
                quantifier_scope.deinit();

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForAll requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForAll condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra }; // ForAll returns a boolean
            },
            .Assert => |assert| {
                const condition_type = try self.inferTypeFromExpr(assert.condition);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Assert condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Nothing }; // Assert has no return value
            },
            .StructDecl => |struct_decl| {
                // Register the struct type in the current scope
                if (self.current_scope) |scope| {
                    const struct_type_info = try self.allocator.create(ast.TypeInfo);
                    struct_type_info.* = .{ .base = .Struct, .custom_type = struct_decl.name.lexeme, .is_mutable = false };

                    // Create a placeholder value for the struct type
                    const struct_value = TokenLiteral{ .string = struct_decl.name.lexeme };

                    _ = scope.createValueBinding(
                        struct_decl.name.lexeme,
                        struct_value,
                        .STRUCT,
                        struct_type_info.*,
                        true, // Struct types are constants
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Duplicate struct name '{s}' in current scope",
                                .{struct_decl.name.lexeme},
                            );
                            self.fatal_error = true;
                        } else {
                            return err;
                        }
                    };
                }

                type_info.* = .{ .base = .Struct };
            },
            .StructLiteral => |struct_lit| {
                // Struct literals should be Custom type with the struct type name
                // This is similar to how enum variant literals are Enum type
                type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
            },
            .EnumDecl => {
                type_info.* = .{ .base = .Enum };
            },
            .ArrayType => {
                type_info.* = .{ .base = .Array };
            },
            .FunctionExpr => {
                type_info.* = .{ .base = .Function };
            },
            .Block => |block| {
                if (block.value) |value| {
                    type_info.* = (try self.inferTypeFromExpr(value)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
        }

        // Cache the result
        try self.type_cache.put(expr.base.id, type_info);
        return type_info;
    }

    fn resolveTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        // If this is a custom type, check if it refers to a declared struct or enum type
        if (type_info.base == .Custom) {
            if (type_info.custom_type) |custom_type_name| {
                if (self.lookupVariable(custom_type_name)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |_| {
                        // If the custom type refers to a struct or enum declaration, keep it as Custom
                        // This is the correct behavior - variables of struct/enum types should be Custom
                        return type_info;
                    }
                }
            }
        }
        // Return the original type info if no resolution needed
        return type_info;
    }

    fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
        if (self.current_scope) |scope| {
            return scope.lookupVariable(name);
        }
        return null;
    }

    // Instead of setting fatal_error = true immediately, collect errors
    // and continue analysis where possible
    pub fn reportTypeError(self: *SemanticAnalyzer, span: ast.SourceSpan, comptime fmt: []const u8, args: anytype) void {
        self.reporter.reportCompileError(span.start, fmt, args);
        // Only set fatal_error for critical errors that prevent further analysis
    }

    fn typeExprToTypeInfo(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr) !*ast.TypeInfo {
        const type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        switch (type_expr.data) {
            .Basic => |basic| {
                type_info.* = .{ .base = switch (basic) {
                    .Integer => .Int,
                    .Byte => .Byte,
                    .Float => .Float,
                    .String => .String,
                    .Tetra => .Tetra,
                    .Nothing => .Nothing,
                } };
            },
            .Custom => |custom| {
                // Variables of enum types should remain as Custom type with the enum type name
                // Only enum declarations themselves should be Enum base type
                type_info.* = .{ .base = .Custom, .custom_type = custom.lexeme };
            },
            .Array => |array| {
                const element_type = try self.typeExprToTypeInfo(array.element_type);
                type_info.* = .{ .base = .Array, .array_type = element_type };
            },
            .Struct => |fields| {
                const struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                for (fields, struct_fields) |field, *struct_field| {
                    const field_type = try self.typeExprToTypeInfo(field.type_expr);
                    struct_field.* = .{
                        .name = field.name.lexeme,
                        .type_info = field_type,
                    };
                }
                type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
            },
            .Enum => |variants| {
                type_info.* = .{ .base = .Enum, .variants = @constCast(variants) };
            },
        }

        return type_info;
    }

    // Add this new function to evaluate expressions and get their values
    fn evaluateExpression(self: *SemanticAnalyzer, expr: *ast.Expr) !TokenLiteral {
        switch (expr.data) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |bin| {
                const left_value = try self.evaluateExpression(bin.left.?);
                const right_value = try self.evaluateExpression(bin.right.?);

                return self.evaluateBinaryOp(left_value, bin.operator, right_value);
            },
            .Unary => |unary| {
                const operand_value = try self.evaluateExpression(unary.right.?);
                return self.evaluateUnaryOp(unary.operator, operand_value);
            },
            .Grouping => |grouped_expr| {
                if (grouped_expr) |expr_in_parens| {
                    return self.evaluateExpression(expr_in_parens);
                } else {
                    return TokenLiteral{ .nothing = {} };
                }
            },
            .Variable => |var_token| {
                // Look up variable in current scope
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        return storage.value;
                    }
                }
                // Return default value if variable not found (shouldn't happen in well-formed code)
                return TokenLiteral{ .nothing = {} };
            },
            else => {
                // For complex expressions that can't be evaluated at compile time,
                // return a default value based on the inferred type
                const inferred_type = try self.inferTypeFromExpr(expr);
                return switch (inferred_type.base) {
                    .Int => TokenLiteral{ .int = 0 },
                    .Float => TokenLiteral{ .float = 0.0 },
                    .String => TokenLiteral{ .string = "" },
                    .Tetra => TokenLiteral{ .tetra = .false },
                    .Byte => TokenLiteral{ .byte = 0 },
                    else => TokenLiteral{ .nothing = {} },
                };
            },
        }
    }

    fn evaluateBinaryOp(self: *SemanticAnalyzer, left: TokenLiteral, operator: Token, right: TokenLiteral) TokenLiteral {
        _ = self; // Unused parameter

        return switch (operator.type) {
            .PLUS => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l + r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                    .byte => |r| TokenLiteral{ .int = l + r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l + r },
                    .byte => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l + r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                    .byte => |r| TokenLiteral{ .byte = l + r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .MINUS => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l - r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                    .byte => |r| TokenLiteral{ .int = l - r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l - r },
                    .byte => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l - r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                    .byte => |r| TokenLiteral{ .byte = l - r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .ASTERISK => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l * r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                    .byte => |r| TokenLiteral{ .int = l * r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l * r },
                    .byte => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l * r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                    .byte => |r| TokenLiteral{ .byte = l * r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .SLASH => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                    .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l / r },
                    .byte => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                    .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .MODULO => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    .byte => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    .byte => |r| TokenLiteral{ .byte = l % r }, // This is fine since bytes are unsigned
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .POWER => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, l, r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        };
    }

    fn evaluateUnaryOp(self: *SemanticAnalyzer, operator: Token, operand: TokenLiteral) TokenLiteral {
        _ = self; // Unused parameter

        return switch (operator.type) {
            .MINUS => switch (operand) {
                .int => |i| TokenLiteral{ .int = -i },
                .float => |f| TokenLiteral{ .float = -f },
                .byte => |b| TokenLiteral{ .int = -@as(i32, b) },
                else => TokenLiteral{ .nothing = {} },
            },
            .BANG => switch (operand) {
                .tetra => |t| TokenLiteral{ .tetra = switch (t) {
                    .true => .false,
                    .false => .true,
                    .both => .neither,
                    .neither => .both,
                } },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        };
    }

    // Add this function to convert values to the expected type
    fn convertValueToType(self: *SemanticAnalyzer, value: TokenLiteral, expected_type: ast.Type) !TokenLiteral {
        _ = self; // Unused parameter

        return switch (expected_type) {
            .Int => switch (value) {
                .int => value,
                .byte => |b| TokenLiteral{ .int = b },
                .float => |f| TokenLiteral{ .int = @intFromFloat(f) },
                else => value, // Keep as is for other types
            },
            .Byte => switch (value) {
                .int => |i| {
                    if (i >= 0 and i <= 255) {
                        return TokenLiteral{ .byte = @intCast(i) };
                    } else {
                        // This should be caught by type checking, but handle gracefully
                        return TokenLiteral{ .byte = 0 };
                    }
                },
                .byte => value,
                .float => |f| {
                    const int_val = @as(i32, @intFromFloat(f)); // Fixed: use @intFromFloat instead of @as
                    if (int_val >= 0 and int_val <= 255) {
                        return TokenLiteral{ .byte = @intCast(int_val) };
                    } else {
                        return TokenLiteral{ .byte = 0 };
                    }
                },
                else => value, // Keep as is for other types
            },
            .Float => switch (value) {
                .int => |i| TokenLiteral{ .float = @floatFromInt(i) },
                .byte => |b| TokenLiteral{ .float = @floatFromInt(b) },
                .float => value,
                else => value, // Keep as is for other types
            },
            else => value, // No conversion needed for other types
        };
    }

    fn handleTypeOf(self: *SemanticAnalyzer, expr: *ast.Expr) !TokenLiteral {
        const type_info = try self.inferTypeFromExpr(expr);
        defer self.allocator.destroy(type_info);

        const type_string = switch (type_info.base) {
            .Int => "int",
            .Float => "float",
            .Byte => "byte",
            .String => "string",
            .Tetra => "tetra",
            .Array => "array",
            .Function => "function",
            .Struct => "struct",
            .Map => "map",
            .Enum => "enum",
            .Union => "union",
            .Nothing => "nothing",
            else => "unknown",
        };

        return TokenLiteral{ .string = type_string };
    }
};
