const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const Scope = Memory.Scope;
const Parser = @import("../../parser/parser_types.zig").Parser;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ErrorList = Errors.ErrorList;

const types = @import("types.zig");
const eval = @import("eval_utils.zig");
const union_handling = @import("union_handling.zig");
const type_analysis = @import("type_analysis.zig");
const scope_management = @import("scope_management.zig");

const getLocationFromBase = @import("helpers.zig").getLocationFromBase;
const isEnumTypeRequiringInitializer = eval.isEnumTypeRequiringInitializer;
const convertTypeToTokenType = eval.convertTypeToTokenType;

/// Context for validation operations
pub const ValidationContext = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *Memory.MemoryManager,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
    custom_types: std.StringHashMap(types.CustomTypeInfo),
    struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
    parser: ?*const Parser,
    fatal_error: *bool,
    current_initializing_var: ?[]const u8,
    current_struct_type: ?[]const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        reporter: *Reporter,
        memory: *Memory.MemoryManager,
        current_scope: ?*Scope,
        type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
        custom_types: std.StringHashMap(types.CustomTypeInfo),
        struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
        parser: ?*const Parser,
        fatal_error: *bool,
        current_initializing_var: ?[]const u8,
        current_struct_type: ?[]const u8,
    ) ValidationContext {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .current_scope = current_scope,
            .type_cache = type_cache,
            .custom_types = custom_types,
            .struct_methods = struct_methods,
            .parser = parser,
            .fatal_error = fatal_error,
            .current_initializing_var = current_initializing_var,
            .current_struct_type = current_struct_type,
        };
    }
};

/// Validate statements
pub fn validateStatements(ctx: *ValidationContext, statements: []const ast.Stmt) ErrorList!void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .VarDecl => |decl| {
                // For function bodies, we need to add local variables to the function scope
                // For global scope, variables are already added during collectDeclarations
                if (ctx.current_scope) |scope| {
                    // Check if this variable is already in the scope (from collectDeclarations)
                    if (scope.lookupVariable(decl.name.lexeme) == null) {
                        // This is a local variable in a function body that wasn't added during collection
                        // Create TypeInfo for the variable
                        const type_info = try ast.TypeInfo.createDefault(ctx.allocator);
                        errdefer ctx.allocator.destroy(type_info);

                        // Check if we have an explicit type annotation
                        if (decl.type_info.base != .Nothing) {
                            // If parser provided an incomplete array type (no element), prefer inferring from initializer if present
                            if (decl.type_info.base == .Array and decl.type_info.array_type == null) {
                                if (decl.initializer) |init_expr| {
                                    var type_ctx = type_analysis.TypeAnalysisContext.init(
                                        ctx.allocator,
                                        ctx.reporter,
                                        ctx.memory,
                                        ctx.current_scope,
                                        ctx.type_cache,
                                        ctx.custom_types,
                                        ctx.struct_methods,
                                        ctx.parser,
                                        ctx.fatal_error,
                                        ctx.current_struct_type,
                                    );
                                    const inferred = try type_analysis.inferTypeFromExpr(&type_ctx, init_expr);
                                    type_info.* = inferred.*;
                                } else {
                                    // Use explicit type, but resolve custom types
                                    var type_ctx = type_analysis.TypeAnalysisContext.init(
                                        ctx.allocator,
                                        ctx.reporter,
                                        ctx.memory,
                                        ctx.current_scope,
                                        ctx.type_cache,
                                        ctx.custom_types,
                                        ctx.struct_methods,
                                        ctx.parser,
                                        ctx.fatal_error,
                                        ctx.current_struct_type,
                                    );
                                    type_info.* = try type_analysis.resolveTypeInfo(&type_ctx, decl.type_info);
                                }
                            } else {
                                // Use explicit type, but resolve custom types
                                var type_ctx = type_analysis.TypeAnalysisContext.init(
                                    ctx.allocator,
                                    ctx.reporter,
                                    ctx.memory,
                                    ctx.current_scope,
                                    ctx.type_cache,
                                    ctx.custom_types,
                                    ctx.struct_methods,
                                    ctx.parser,
                                    ctx.fatal_error,
                                );
                                type_info.* = try type_analysis.resolveTypeInfo(&type_ctx, decl.type_info);
                            }
                            // Preserve mutability from the variable declaration (var vs const)
                            type_info.is_mutable = decl.type_info.is_mutable;
                        } else if (decl.initializer) |init_expr| {
                            // Infer from initializer
                            var type_ctx = type_analysis.TypeAnalysisContext.init(
                                ctx.allocator,
                                ctx.reporter,
                                ctx.memory,
                                ctx.current_scope,
                                ctx.type_cache,
                                ctx.custom_types,
                                ctx.struct_methods,
                                ctx.parser,
                                ctx.fatal_error,
                            );
                            const inferred = try type_analysis.inferTypeFromExpr(&type_ctx, init_expr);
                            type_info.* = inferred.*;
                        } else {
                            // No type annotation and no initializer - this is invalid
                            ctx.reporter.reportCompileError(
                                getLocationFromBase(stmt.base),
                                ErrorCode.VARIABLE_DECLARATION_MISSING_ANNOTATION,
                                "Variable declaration requires either type annotation (::) or initializer",
                                .{},
                            );
                            ctx.fatal_error.* = true;
                            continue;
                        }

                        const token_type = convertTypeToTokenType(type_info.base);

                        var value: TokenLiteral = undefined;

                        if (decl.initializer) |init_expr| {
                            ctx.current_initializing_var = decl.name.lexeme;
                            defer {
                                ctx.current_initializing_var = null;
                            }

                            value = try evaluateExpression(ctx, init_expr);
                        } else {
                            if (isEnumTypeRequiringInitializer(type_info, ctx.custom_types)) {
                                ctx.reporter.reportCompileError(
                                    getLocationFromBase(stmt.base),
                                    ErrorCode.ENUM_REQUIRES_INITIALIZER,
                                    "Enum variables must be initialized",
                                    .{},
                                );
                                ctx.fatal_error.* = true;
                                continue;
                            }

                            // Only use defaults for uninitialized variables
                            value = switch (type_info.base) {
                                .Int => TokenLiteral{ .int = 0 },
                                .Float => TokenLiteral{ .float = 0.0 },
                                .String => TokenLiteral{ .string = "" },
                                .Tetra => TokenLiteral{ .tetra = .false },
                                .Byte => TokenLiteral{ .byte = 0 },
                                .Array => TokenLiteral{ .array = &[_]TokenLiteral{} },
                                .Union => if (type_info.union_type) |ut|
                                    union_handling.getUnionDefaultValue(ut)
                                else
                                    TokenLiteral{ .nothing = {} },
                                else => TokenLiteral{ .nothing = {} },
                            };
                        }

                        // Convert value to match the declared type
                        value = try eval.convertValueToType(value, type_info.base);

                        _ = scope.createValueBinding(
                            decl.name.lexeme,
                            value,
                            token_type,
                            type_info,
                            !type_info.is_mutable,
                        ) catch |err| {
                            if (err == error.DuplicateVariableName) {
                                ctx.reporter.reportCompileError(
                                    getLocationFromBase(stmt.base),
                                    ErrorCode.DUPLICATE_VARIABLE,
                                    "Duplicate variable name '{s}' in current scope",
                                    .{decl.name.lexeme},
                                );
                                ctx.fatal_error.* = true;
                                continue;
                            } else {
                                return err;
                            }
                        };
                    }
                }

                // Validate type compatibility if both type annotation and initializer are present
                if (decl.type_info.base != .Nothing and decl.initializer != null) {
                    var type_ctx = type_analysis.TypeAnalysisContext.init(
                        ctx.allocator,
                        ctx.reporter,
                        ctx.memory,
                        ctx.current_scope,
                        ctx.type_cache,
                        ctx.custom_types,
                        ctx.struct_methods,
                        ctx.parser,
                        ctx.fatal_error,
                        ctx.current_struct_type,
                    );
                    const init_type = try type_analysis.inferTypeFromExpr(&type_ctx, decl.initializer.?);
                    const resolved_type = try type_analysis.resolveTypeInfo(&type_ctx, decl.type_info);
                    var decl_type_copy = resolved_type;
                    try type_analysis.unifyTypes(&type_ctx, &decl_type_copy, init_type, .{ .location = getLocationFromBase(stmt.base) });
                }
            },
            .Block => |block_stmts| {
                const prev_scope = ctx.current_scope;
                ctx.current_scope = try ctx.memory.scope_manager.createScope(ctx.current_scope, ctx.memory);
                defer {
                    ctx.current_scope = prev_scope;
                    // Scope cleanup handled by memory manager
                }
                try validateStatements(ctx, block_stmts);
            },
            .Expression => |expr| {
                if (expr) |expression| {
                    var type_ctx = type_analysis.TypeAnalysisContext.init(
                        ctx.allocator,
                        ctx.reporter,
                        ctx.memory,
                        ctx.current_scope,
                        ctx.type_cache,
                        ctx.custom_types,
                        ctx.struct_methods,
                        ctx.parser,
                        ctx.fatal_error,
                        ctx.current_struct_type,
                    );
                    _ = try type_analysis.inferTypeFromExpr(&type_ctx, expression);
                }
            },
            .Return => |return_stmt| {
                if (return_stmt.value) |value| {
                    // Validate return value type
                    var type_ctx = type_analysis.TypeAnalysisContext.init(
                        ctx.allocator,
                        ctx.reporter,
                        ctx.memory,
                        ctx.current_scope,
                        ctx.type_cache,
                        ctx.custom_types,
                        ctx.struct_methods,
                        ctx.parser,
                        ctx.fatal_error,
                        ctx.current_struct_type,
                    );
                    _ = try type_analysis.inferTypeFromExpr(&type_ctx, value);
                }
            },
            .FunctionDecl => |func| {
                // Validate function body by creating a function scope and processing its statements
                // TODO: validate return statements against func.return_type_info
                const prev_scope = ctx.current_scope;
                ctx.current_scope = try ctx.memory.scope_manager.createScope(ctx.current_scope, ctx.memory);
                defer {
                    ctx.current_scope = prev_scope;
                }

                // Register function parameters in the function scope
                for (func.params) |param| {
                    const param_type_info = try ast.TypeInfo.createDefault(ctx.allocator);
                    errdefer ctx.allocator.destroy(param_type_info);

                    if (param.type_expr) |type_expr| {
                        const resolved = try ast.typeInfoFromExpr(ctx.allocator, type_expr);
                        param_type_info.* = resolved.*;
                    } else {
                        param_type_info.* = .{ .base = .Nothing };
                    }

                    _ = ctx.current_scope.?.createValueBinding(
                        param.name.lexeme,
                        TokenLiteral{ .nothing = {} },
                        convertTypeToTokenType(param_type_info.base),
                        param_type_info,
                        !param_type_info.is_mutable,
                    ) catch |err| {
                        // Parameters may shadow outer variables; this is handled by the scope
                        return err;
                    };
                }

                try validateStatements(ctx, func.body);
            },
            .MapLiteral => |*map_literal| {
                // Validate map literal entries
                var type_ctx = type_analysis.TypeAnalysisContext.init(
                    ctx.allocator,
                    ctx.reporter,
                    ctx.memory,
                    ctx.current_scope,
                    ctx.type_cache,
                    ctx.custom_types,
                    ctx.struct_methods,
                    ctx.parser,
                    ctx.fatal_error,
                    ctx.current_struct_type,
                );
                for (map_literal.entries) |entry| {
                    _ = try type_analysis.inferTypeFromExpr(&type_ctx, entry.key);
                    _ = try type_analysis.inferTypeFromExpr(&type_ctx, entry.value);
                }

                // Validate else_value if present - it should match the map's value type
                if (map_literal.else_value) |else_val| {
                    _ = try type_analysis.inferTypeFromExpr(&type_ctx, else_val);
                    // Type compatibility check would be done during unification if needed
                }
            },
            .ZigDecl, .EnumDecl, .GroupDecl, .Module, .Path, .Import, .Continue, .Break, .Assert, .Cast => {},
        }
    }
}

/// Evaluate expression to get its value
fn evaluateExpression(ctx: *ValidationContext, expr: *ast.Expr) !TokenLiteral {
    switch (expr.data) {
        .Literal => |lit| {
            return lit;
        },
        .Binary => |bin| {
            const left_value = try evaluateExpression(ctx, bin.left.?);
            const right_value = try evaluateExpression(ctx, bin.right.?);
            return eval.evaluateBinaryOp(left_value, bin.operator, right_value);
        },
        .Unary => |unary| {
            const operand_value = try evaluateExpression(ctx, unary.right.?);
            return eval.evaluateUnaryOp(unary.operator, operand_value);
        },
        .Grouping => |grouped_expr| {
            if (grouped_expr) |expr_in_parens| {
                return evaluateExpression(ctx, expr_in_parens);
            } else {
                return TokenLiteral{ .nothing = {} };
            }
        },
        .Variable => |var_token| {
            if (ctx.current_initializing_var) |current_var| {
                if (std.mem.eql(u8, var_token.lexeme, current_var)) {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.SELF_REFERENTIAL_INITIALIZER,
                        "Variable '{s}' cannot reference itself in its own initializer",
                        .{current_var},
                    );
                    ctx.fatal_error.* = true;
                    return TokenLiteral{ .nothing = {} };
                }
            }

            if (scope_management.lookupVariable(ctx.current_scope, ctx.parser, ctx.allocator, ctx.reporter, var_token.lexeme)) |variable| {
                if (ctx.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    return storage.value;
                }
            }

            if (scope_management.suggestVariableName(ctx.current_scope, ctx.parser, var_token.lexeme)) |suggested| {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNDEFINED_VARIABLE,
                    "Undefined variable: '{s}'. Did you mean '{s}'?",
                    .{ var_token.lexeme, suggested },
                );
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNDEFINED_VARIABLE,
                    "Undefined variable: '{s}'",
                    .{var_token.lexeme},
                );
            }
            ctx.fatal_error.* = true;
            return TokenLiteral{ .nothing = {} };
        },
        .Cast => |cast| {
            const value_literal = try evaluateExpression(ctx, cast.value);
            const target_type_info = try ast.typeInfoFromExpr(ctx.allocator, cast.target_type);
            defer ctx.allocator.destroy(target_type_info);

            const matches_target: bool = switch (target_type_info.base) {
                .Int => value_literal == .int,
                .Float => value_literal == .float,
                .Byte => value_literal == .byte,
                .String => value_literal == .string,
                .Tetra => value_literal == .tetra,
                .Nothing => value_literal == .nothing,
                else => false,
            };

            if (cast.then_branch != null or cast.else_branch != null) {
                if (matches_target) {
                    if (cast.then_branch) |then_expr| {
                        return evaluateExpression(ctx, then_expr);
                    }
                } else {
                    if (cast.else_branch) |else_expr| {
                        return evaluateExpression(ctx, else_expr);
                    }
                }
                return TokenLiteral{ .nothing = {} };
            }

            return value_literal;
        },
        else => {
            return TokenLiteral{ .nothing = {} };
        },
    }
}
