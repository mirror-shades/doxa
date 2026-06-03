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

/// Context for declaration collection operations
pub const DeclarationCollectionContext = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *Memory.MemoryManager,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
    custom_types: std.StringHashMap(types.CustomTypeInfo),
    struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
    function_return_types: std.AutoHashMap(u32, *ast.TypeInfo),
    current_function_returns: std.array_list.Managed(*ast.TypeInfo),
    parser: ?*const Parser,
    fatal_error: *bool,
    current_struct_type: ?[]const u8,
    current_initializing_var: ?[]const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        reporter: *Reporter,
        memory: *Memory.MemoryManager,
        current_scope: ?*Scope,
        type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
        custom_types: std.StringHashMap(types.CustomTypeInfo),
        struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
        function_return_types: std.AutoHashMap(u32, *ast.TypeInfo),
        current_function_returns: std.array_list.Managed(*ast.TypeInfo),
        parser: ?*const Parser,
        fatal_error: *bool,
        current_struct_type: ?[]const u8,
    ) DeclarationCollectionContext {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .current_scope = current_scope,
            .type_cache = type_cache,
            .custom_types = custom_types,
            .struct_methods = struct_methods,
            .function_return_types = function_return_types,
            .current_function_returns = current_function_returns,
            .parser = parser,
            .fatal_error = fatal_error,
            .current_struct_type = current_struct_type,
            .current_initializing_var = null,
        };
    }
};

/// Collect declarations from statements
pub fn collectDeclarations(ctx: *DeclarationCollectionContext, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
    // First pass: pre-register all function declarations so calls can see them
    for (statements) |stmt| {
        switch (stmt.data) {
            .FunctionDecl => |func| {
                // Create function type
                const func_type = try ctx.allocator.create(ast.FunctionType);

                // Infer or use explicit return type
                var inferred_return_type = func.return_type_info;
                if (func.return_type_info.base == .Nothing) {
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
                    const inferred = try inferFunctionReturnType(&type_ctx, func);
                    inferred_return_type = inferred.*;
                } else if (func.return_type_info.base == .Union) {
                    if (func.return_type_info.union_type) |union_type| {
                        const flattened = try union_handling.flattenUnionType(ctx.allocator, union_type);
                        inferred_return_type = ast.TypeInfo{
                            .base = .Union,
                            .union_type = flattened,
                            .is_mutable = false,
                        };
                    }
                }

                // Build parameter types and track aliases
                var param_types = try ctx.allocator.alloc(ast.TypeInfo, func.params.len);
                var param_aliases = try ctx.allocator.alloc(bool, func.params.len);
                for (func.params, 0..) |param, i| {
                    param_aliases[i] = param.is_alias;
                    if (param.type_expr) |type_expr| {
                        const param_type_ptr = try ast.typeInfoFromExpr(ctx.allocator, type_expr);
                        defer ctx.allocator.destroy(param_type_ptr);

                        // For explicit parameter types, use the declared type directly
                        // Don't call resolveTypeInfo which might trigger type inference
                        param_types[i] = param_type_ptr.*;
                    } else {
                        param_types[i] = .{ .base = .Nothing };
                    }
                }

                // Set up function type
                func_type.* = .{
                    .params = param_types,
                    .return_type = try ast.TypeInfo.createDefault(ctx.allocator),
                    .param_aliases = param_aliases,
                };
                func_type.return_type.* = inferred_return_type;

                // Add function to current scope if not already present
                if (scope.lookupVariable(func.name.lexeme) == null) {
                    const func_type_info = try ast.TypeInfo.createDefault(ctx.allocator);
                    func_type_info.* = .{ .base = .Function, .function_type = func_type };

                    _ = scope.createValueBinding(
                        func.name.lexeme,
                        TokenLiteral{ .nothing = {} },
                        convertTypeToTokenType(.Function),
                        func_type_info,
                        false,
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            ctx.reporter.reportCompileError(
                                getLocationFromBase(stmt.base),
                                ErrorCode.DUPLICATE_VARIABLE,
                                "Duplicate function name '{s}' in current scope",
                                .{func.name.lexeme},
                            );
                            ctx.fatal_error.* = true;
                            continue;
                        } else {
                            return err;
                        }
                    };

                    // Store the return type for later validation
                    try ctx.function_return_types.put(stmt.base.id, func_type.return_type);
                }
            },
            .VarDecl => |decl| {
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
                            ctx.current_struct_type,
                        );
                        type_info.* = try type_analysis.resolveTypeInfo(&type_ctx, decl.type_info);
                    }
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
                        ctx.current_struct_type,
                    );
                    const inferred = try type_analysis.inferTypeFromExpr(&type_ctx, init_expr);
                    // Deep copy the inferred type to avoid dangling internal pointers
                    type_info.* = try type_analysis.deepCopyTypeInfo(&type_ctx, inferred.*);
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
                    defer ctx.current_initializing_var = null;

                    value = evaluateExpression(ctx, init_expr);

                    // Fall back to default if evaluation produced nothing
                    if (value == .nothing) {
                        value = switch (type_info.base) {
                            .Int => TokenLiteral{ .int = 0 },
                            .Float => TokenLiteral{ .float = 0.0 },
                            .String => TokenLiteral{ .string = "" },
                            .Tetra => TokenLiteral{ .tetra = .false },
                            .Byte => TokenLiteral{ .byte = 0 },
                            .Union => if (type_info.union_type) |ut|
                                union_handling.getUnionDefaultValue(ut)
                            else
                                TokenLiteral{ .nothing = {} },
                            else => TokenLiteral{ .nothing = {} },
                        };
                    }
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
            },
            .Import => |import_info| {
                // Handle import statements
                try validateImport(ctx, import_info, .{ .location = getLocationFromBase(stmt.base) });
            },
            else => {
                // Other statement types don't need declaration collection
            },
        }
    }
}

/// Infer function return type from function body.
/// Walks top-level statements and nested Block statements to find returns.
/// Returns .Nothing if no return-with-value is found.
fn inferFunctionReturnType(ctx: *type_analysis.TypeAnalysisContext, func: anytype) !*ast.TypeInfo {
    const return_type = try ast.TypeInfo.createDefault(ctx.allocator);
    return_type.* = .{ .base = .Nothing };

    if (findFirstReturnType(ctx, func.body)) |rtt| {
        defer ctx.allocator.destroy(rtt);
        return_type.* = try type_analysis.deepCopyTypeInfo(ctx, rtt.*);
    }
    return return_type;
}

fn findFirstReturnType(ctx: *type_analysis.TypeAnalysisContext, stmts: []const ast.Stmt) ErrorList!?*ast.TypeInfo {
    for (stmts) |stmt| {
        switch (stmt.data) {
            .Return => |ret| {
                if (ret.value) |value_expr| {
                    return try type_analysis.inferTypeFromExpr(ctx, value_expr);
                }
            },
            .Block => |block_stmts| {
                if (try findFirstReturnType(ctx, block_stmts)) |t| return t;
            },
            else => {
                // TODO: walk Expression statements (if/for/while/match) to find nested returns
            },
        }
    }
    return null;
}

/// Validate import statement
fn validateImport(ctx: *DeclarationCollectionContext, import_info: ast.ImportInfo, span: ast.SourceSpan) !void {
    if (import_info.module_path.len == 0) {
        ctx.reporter.reportCompileError(
            span.location,
            ErrorCode.INVALID_IMPORT,
            "Import path cannot be empty",
            .{},
        );
        ctx.fatal_error.* = true;
        return;
    }

    // Validate specific symbol names are non-empty identifiers
    // TODO: deeper validation — verify module exists in cache and requested symbols are exported
    if (import_info.specific_symbols) |symbols| {
        for (symbols) |sym| {
            if (sym.len == 0) {
                ctx.reporter.reportCompileError(
                    span.location,
                    ErrorCode.INVALID_IMPORT,
                    "Imported symbol name cannot be empty",
                    .{},
                );
                ctx.fatal_error.* = true;
                return;
            }
        }
    }

    if (import_info.specific_symbol) |sym| {
        if (sym.len == 0) {
            ctx.reporter.reportCompileError(
                span.location,
                ErrorCode.INVALID_IMPORT,
                "Imported symbol name cannot be empty",
                .{},
            );
            ctx.fatal_error.* = true;
            return;
        }
    }
}

/// Recursively evaluate constant expressions during declaration collection.
/// Returns .nothing for expressions that cannot be statically resolved
/// (forward references, runtime values, unsupported expression types).
fn evaluateExpression(ctx: *DeclarationCollectionContext, expr: *ast.Expr) TokenLiteral {
    return switch (expr.data) {
        .Literal => |lit| lit,
        .Binary => |bin| {
            const left = evaluateExpression(ctx, bin.left.?);
            if (left == .nothing) return TokenLiteral{ .nothing = {} };
            const right = evaluateExpression(ctx, bin.right.?);
            if (right == .nothing) return TokenLiteral{ .nothing = {} };
            return eval.evaluateBinaryOp(left, bin.operator, right);
        },
        .Unary => |unary| {
            const operand = evaluateExpression(ctx, unary.right.?);
            if (operand == .nothing) return TokenLiteral{ .nothing = {} };
            return eval.evaluateUnaryOp(unary.operator, operand);
        },
        .Grouping => |grouped_expr| {
            if (grouped_expr) |inner| return evaluateExpression(ctx, inner);
            return TokenLiteral{ .nothing = {} };
        },
        .Variable => |var_token| {
            if (ctx.current_initializing_var) |current_var| {
                if (std.mem.eql(u8, var_token.lexeme, current_var)) {
                    return TokenLiteral{ .nothing = {} };
                }
            }
            if (scope_management.lookupVariable(ctx.current_scope, ctx.parser, ctx.allocator, ctx.reporter, var_token.lexeme)) |variable| {
                if (ctx.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    return storage.value;
                }
            }
            return TokenLiteral{ .nothing = {} };
        },
        else => TokenLiteral{ .nothing = {} },
    };
}
