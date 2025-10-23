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
const Environment = @import("../../interpreter/environment.zig");

const types = @import("types.zig");
const union_handling = @import("union_handling.zig");
const type_analysis = @import("type_analysis.zig");

/// Helper function to get location from AST Base, handling optional spans
fn getLocationFromBase(base: ast.Base) Location {
    if (base.span) |span| {
        return span.location;
    } else {
        // Synthetic node - return default location
        return .{
            .file = "",
            .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
        };
    }
}

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
                    .return_type = try ctx.allocator.create(ast.TypeInfo),
                    .param_aliases = param_aliases,
                };
                func_type.return_type.* = inferred_return_type;

                // Add function to current scope if not already present
                if (scope.lookupVariable(func.name.lexeme) == null) {
                    const func_type_info = try ctx.allocator.create(ast.TypeInfo);
                    func_type_info.* = .{ .base = .Function, .function_type = func_type };

                    _ = Environment.init(
                        ctx.allocator,
                        null,
                        false,
                        ctx.memory,
                    );

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
                const type_info = try ctx.allocator.create(ast.TypeInfo);
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
                    _ = init_expr; // May be used for evaluation in the future
                    // For now, just use a placeholder value
                    // In a full implementation, this would evaluate the expression
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
                } else {
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
                value = try convertValueToType(value, type_info.base);

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

/// Infer function return type from function body
fn inferFunctionReturnType(ctx: *type_analysis.TypeAnalysisContext, _func: anytype) !*ast.TypeInfo {
    _ = _func;
    // This is a simplified implementation
    // In a full implementation, this would analyze the function body to determine return type
    const return_type = try ctx.allocator.create(ast.TypeInfo);
    return_type.* = .{ .base = .Nothing };
    return return_type;
}

/// Validate import statement
fn validateImport(ctx: *DeclarationCollectionContext, import_info: ast.ImportInfo, span: ast.SourceSpan) !void {
    _ = ctx;
    _ = import_info;
    _ = span;
    // Import validation would be implemented here
}

/// Convert TypeInfo base type to TokenType
fn convertTypeToTokenType(base_type: ast.Type) TokenType {
    return switch (base_type) {
        .Int => .INT,
        .Float => .FLOAT,
        .String => .STRING,
        .Tetra => .TETRA,
        .Byte => .BYTE,
        .Nothing => .NOTHING,
        .Array => .ARRAY,
        .Struct => .STRUCT,
        .Map => .MAP,
        .Enum => .ENUM,
        .Function => .FUNCTION,
        .Union => .UNION,
        .Custom => .CUSTOM,
    };
}

/// Convert value to match the declared type
fn convertValueToType(value: TokenLiteral, target_type: ast.Type) !TokenLiteral {
    _ = target_type; // May be used for type conversion in the future
    return value;
}
