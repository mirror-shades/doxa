const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const Variable = Memory.Variable;
const Parser = @import("../../parser/parser_types.zig").Parser;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const TypeSystem = @import("../../codegen/hir/type_system.zig").TypeSystem;

const types = @import("types.zig");
const union_handling = @import("union_handling.zig");
const scope_management = @import("scope_management.zig");
const helpers = @import("helpers.zig");

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

/// Context for type analysis operations
pub const TypeAnalysisContext = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *Memory.MemoryManager,
    current_scope: ?*Memory.Scope,
    type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
    custom_types: std.StringHashMap(types.CustomTypeInfo),
    struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
    parser: ?*const Parser,
    fatal_error: *bool,

    pub fn init(
        allocator: std.mem.Allocator,
        reporter: *Reporter,
        memory: *Memory.MemoryManager,
        current_scope: ?*Memory.Scope,
        type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
        custom_types: std.StringHashMap(types.CustomTypeInfo),
        struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
        parser: ?*const Parser,
        fatal_error: *bool,
    ) TypeAnalysisContext {
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
        };
    }
};

/// Infer type from expression - this is the main type inference function
pub fn inferTypeFromExpr(ctx: *TypeAnalysisContext, expr: *ast.Expr) !*ast.TypeInfo {
    // Check cache first
    if (ctx.type_cache.get(expr.base.id)) |cached| {
        return cached;
    }

    var type_info = try ctx.allocator.create(ast.TypeInfo);
    errdefer ctx.allocator.destroy(type_info);

    switch (expr.data) {
        .Map => |entries| {
            // Minimal inference for map literals: infer key/value from first entry if present.
            type_info.* = .{ .base = .Map };
            if (entries.len > 0) {
                const first_key_type = try inferTypeFromExpr(ctx, entries[0].key);
                const first_val_type = try inferTypeFromExpr(ctx, entries[0].value);
                // Reuse inferred pointers to avoid unnecessary deep copies and allocations.
                type_info.map_key_type = first_key_type;
                type_info.map_value_type = first_val_type;
            }
        },
        .Literal => |lit| {
            type_info.inferFrom(lit); // TokenLiteral is already the right type
        },
        .Break => {
            type_info.base = .Nothing;
        },
        .Increment => {
            type_info.base = .Int;
        },
        .Decrement => {
            type_info.base = .Int;
        },
        .Binary => |bin| {
            const left_type = try inferTypeFromExpr(ctx, bin.left.?);
            const right_type = try inferTypeFromExpr(ctx, bin.right.?);

            // Operator-specific type rules
            const op = bin.operator.lexeme;

            if (std.mem.eql(u8, op, "/")) {
                // Division always returns float
                if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                        "Division requires numeric operands, got {s}",
                        .{@tagName(left_type.base)},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                        "Division requires numeric operands, got {s}",
                        .{@tagName(right_type.base)},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Float };
            } else if (std.mem.eql(u8, op, "%")) {
                // Modulo for integers and bytes
                if ((left_type.base != .Int and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Byte))
                {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                        "Modulo requires integer or byte operands",
                        .{},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                // Return type follows same promotion rules as arithmetic
                if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "+")) {
                // Handle string and array concatenation first
                if (left_type.base == .String and right_type.base == .String) {
                    type_info.* = .{ .base = .String }; // String concatenation
                } else if (left_type.base == .Array and right_type.base == .Array) {
                    type_info.* = .{ .base = .Array }; // Array concatenation
                } else if (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte or
                    right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte)
                {
                    // Numeric addition with type promotion
                    // Type promotion rules: Float > Int > Byte
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Invalid operands for '+' operator: {s} and {s}",
                        .{ @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                // Numeric operations with type promotion
                if (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte or
                    right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte)
                {
                    // Type promotion rules: Float > Int > Byte
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Invalid operands for '{s}' operator: {s} and {s}",
                        .{ op, @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!=") or
                std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">="))
            {
                // Comparison operators return tetra
                type_info.* = .{ .base = .Tetra };
            } else if (std.mem.eql(u8, op, "and") or std.mem.eql(u8, op, "or")) {
                // Logical operators require tetra operands and return tetra
                if (left_type.base != .Tetra) {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Left operand of '{s}' must be tetra, got {s}",
                        .{ op, @tagName(left_type.base) },
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                if (right_type.base != .Tetra) {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Right operand of '{s}' must be tetra, got {s}",
                        .{ op, @tagName(right_type.base) },
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Tetra };
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNSUPPORTED_OPERATOR,
                    "Unknown operator: {s}",
                    .{op},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .Unary => |unary| {
            const operand_type = try inferTypeFromExpr(ctx, unary.right.?);
            const op = unary.operator.lexeme;

            if (std.mem.eql(u8, op, "-")) {
                // Unary minus for numeric types
                if (operand_type.base == .Int or operand_type.base == .Float or operand_type.base == .Byte) {
                    type_info.* = operand_type.*;
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Unary '-' requires numeric operand, got {s}",
                        .{@tagName(operand_type.base)},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (std.mem.eql(u8, op, "not")) {
                // Logical not for tetra
                if (operand_type.base == .Tetra) {
                    type_info.* = .{ .base = .Tetra };
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Unary 'not' requires tetra operand, got {s}",
                        .{@tagName(operand_type.base)},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNSUPPORTED_OPERATOR,
                    "Unknown unary operator: {s}",
                    .{op},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .Variable => |var_name| {
            // Look up variable in current scope
            const variable = scope_management.lookupVariable(
                ctx.current_scope,
                ctx.parser,
                ctx.allocator,
                ctx.reporter,
                var_name.lexeme,
            );

            if (variable) |variable_ptr| {
                // Get the storage to access type_info
                if (ctx.memory.scope_manager.value_storage.get(variable_ptr.storage_id)) |storage| {
                    type_info.* = storage.type_info.*;
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.UNDEFINED_VARIABLE,
                        "Variable storage not found: {s}",
                        .{var_name.lexeme},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNDEFINED_VARIABLE,
                    "Undefined variable: {s}",
                    .{var_name.lexeme},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .FunctionCall => |call| {
            // Method call support: receiver.method(args)
            if (call.callee.data == .FieldAccess) {
                const fa = call.callee.data.FieldAccess;
                const object_type = try inferTypeFromExpr(ctx, fa.object);
                const method_name = fa.field.lexeme;

                // Static method on a struct type: Type.Method(...)
                if (fa.object.data == .Variable) {
                    const object_name = fa.object.data.Variable.lexeme;
                    if (helpers.isModuleNamespace(@constCast(ctx), object_name)) {
                        // Module function call - return a generic type for now
                        type_info.* = .{ .base = .Int };
                        return type_info;
                    }
                    if (ctx.custom_types.get(object_name)) |ct| {
                        if (ct.kind == .Struct) {
                            if (ctx.struct_methods.get(object_name)) |mt| {
                                if (mt.get(method_name)) |mi| {
                                    if (mi.is_static) {
                                        type_info.* = mi.return_type.*;
                                        return type_info;
                                    }
                                }
                            }
                        }
                    }
                } else if (fa.object.data == .FieldAccess) {
                    // Nested namespace: graphics.raylib.Func
                    if (object_type.base == .Custom and object_type.custom_type) |ctn| {
                        if (std.mem.indexOfScalar(u8, ctn, '.')) |dot_idx| {
                            const root = ctn[0..dot_idx];
                            if (helpers.isModuleNamespace(@constCast(ctx), root)) {
                                type_info.* = .{ .base = .Int };
                                return type_info;
                            }
                        }
                    }
                }

                // Instance method based on receiver type
                var struct_name: ?[]const u8 = null;
                if (object_type.base == .Custom and object_type.custom_type) |ctn| {
                    // If this is a nested graphics namespace (graphics.raylib / graphics.doxa), treat as module function
                    if (std.mem.startsWith(u8, ctn, "graphics.")) {
                        type_info.* = .{ .base = .Int };
                        return type_info;
                    }
                    struct_name = ctn;
                } else if (object_type.base == .Struct and object_type.custom_type) |ctn2| {
                    struct_name = ctn2;
                }

                if (struct_name) |sn| {
                    if (ctx.struct_methods.get(sn)) |tbl| {
                        if (tbl.get(method_name)) |mi2| {
                            type_info.* = mi2.return_type.*;
                            return type_info;
                        }
                    }
                }
                // Fallthrough to error below
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNKNOWN_METHOD,
                    "Unknown method '{s}'",
                    .{method_name},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // Regular function call: f(args)
            const callee_type = try inferTypeFromExpr(ctx, call.callee);
            if (callee_type.base == .Function) {
                if (callee_type.function_type) |func_type| {
                    type_info.* = func_type.return_type.*;
                } else {
                    type_info.base = .Nothing;
                }
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_CALL_METHOD_ON_TYPE,
                    "Cannot call non-function type: {s}",
                    .{@tagName(callee_type.base)},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .Grouping => |grouped| {
            // Grouping doesn't change type
            const grouped_type = try inferTypeFromExpr(ctx, grouped.?);
            type_info.* = grouped_type.*;
        },
        .Array => |elements| {
            if (elements.len == 0) {
                type_info.* = .{ .base = .Array, .array_type = null };
            } else {
                const first_type = try inferTypeFromExpr(ctx, elements[0]);
                for (elements[1..]) |element| {
                    const element_type = try inferTypeFromExpr(ctx, element);
                    try unifyTypes(ctx, first_type, element_type, .{ .location = getLocationFromBase(expr.base) });
                }
                const array_type = try ctx.allocator.create(ast.TypeInfo);
                array_type.* = first_type.*;
                type_info.* = .{ .base = .Array, .array_type = array_type };
            }
        },
        .Struct => |fields| {
            const struct_fields = try ctx.allocator.alloc(ast.StructFieldType, fields.len);
            for (fields, struct_fields) |field, *struct_field| {
                const field_type = try inferTypeFromExpr(ctx, field.value);
                struct_field.* = .{
                    .name = field.name.lexeme,
                    .type_info = field_type,
                };
            }
            type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
        },
        .If => |if_expr| {
            const condition_type = try inferTypeFromExpr(ctx, if_expr.condition.?);
            if (condition_type.base != .Tetra) {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }

            const then_type = try inferTypeFromExpr(ctx, if_expr.then_branch.?);
            if (if_expr.else_branch) |else_branch| {
                const else_type = try inferTypeFromExpr(ctx, else_branch);

                // If the parser produced an implicit else that is a Nothing literal,
                // treat this as if there is no else-branch to avoid forcing unification
                // of a concrete then-type with Nothing in statement contexts.
                const else_is_implicit_nothing = (else_branch.data == .Literal and else_type.base == .Nothing);
                if (else_is_implicit_nothing) {
                    type_info.* = then_type.*;
                    return type_info;
                }

                // Special handling for peek expressions - allow different types
                // since peek is used for output/printing and the actual return value
                // should be the same as the expression being peeked
                const then_is_peek = if_expr.then_branch.?.data == .Peek;
                const else_is_peek = else_branch.data == .Peek;

                // New: if one branch is a peek and the other is implicit nothing, allow the peek branch type
                if ((then_is_peek and else_type.base == .Nothing) or (else_is_peek and then_type.base == .Nothing)) {
                    // Prefer the branch that actually peeks a value
                    type_info.* = if (then_is_peek) then_type.* else else_type.*;
                } else if (then_is_peek and else_is_peek) {
                    // Both are peek expressions - use the then type as the result type
                    // This allows different types to be peeked in different branches
                    type_info.* = then_type.*;
                } else {
                    // If one branch is effectively "no value" (Nothing) and the other produces a value,
                    // use the value-producing branch's type
                    if (then_type.base == .Nothing and else_type.base != .Nothing) {
                        type_info.* = else_type.*;
                    } else if (else_type.base == .Nothing and then_type.base != .Nothing) {
                        type_info.* = then_type.*;
                    } else {
                        // Both branches produce values - they must be compatible
                        try unifyTypes(ctx, then_type, else_type, .{ .location = getLocationFromBase(expr.base) });
                        type_info.* = then_type.*;
                    }
                }
            } else {
                // No else branch - return type is the then branch type
                type_info.* = then_type.*;
            }
        },
        .Match => |match_expr| {
            // Infer type from the matched expression
            const matched_type = try inferTypeFromExpr(ctx, match_expr.value);

            // For now, assume match expressions return the same type as the matched expression
            // In a more sophisticated implementation, this would depend on the case types
            type_info.* = matched_type.*;
        },
        .Peek => |peek_expr| {
            // Peek expressions return the type of the peeked expression
            const peek_type = try inferTypeFromExpr(ctx, peek_expr.expr);
            type_info.* = peek_type.*;
        },
        .FieldAccess => |field_access| {
            const object_type = try inferTypeFromExpr(ctx, field_access.object);

            if (object_type.base == .Struct) {
                if (object_type.struct_fields) |fields| {
                    // Look for the field in the struct
                    for (fields) |field| {
                        if (std.mem.eql(u8, field.name, field_access.field.lexeme)) {
                            type_info.* = field.type_info.*;
                            return type_info;
                        }
                    }
                    // Field not found
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.FIELD_NOT_FOUND,
                        "Struct has no field '{s}'",
                        .{field_access.field.lexeme},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                } else {
                    ctx.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.STRUCT_HAS_NO_FIELDS,
                        "Struct has no fields defined",
                        .{},
                    );
                    ctx.fatal_error.* = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (object_type.base == .Custom) {
                // Handle custom type field access
                if (object_type.custom_type) |custom_type_name| {
                    if (ctx.custom_types.get(custom_type_name)) |custom_type| {
                        if (custom_type.kind == .Struct and custom_type.struct_fields != null) {
                            const fields = custom_type.struct_fields.?;
                            for (fields) |field| {
                                if (std.mem.eql(u8, field.name, field_access.field.lexeme)) {
                                    type_info.* = field.field_type_info.*;
                                    return type_info;
                                }
                            }
                        }
                    }
                }
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.FIELD_NOT_FOUND,
                    "Custom type has no field '{s}'",
                    .{field_access.field.lexeme},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            } else if (object_type.base == .Enum) {
                // Allow enum variant access: Color.Red
                // You may want to check if the variant exists, but for now just return Enum type
                type_info.* = .{ .base = .Enum };
            } else {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                    "Cannot access field on non-struct type {s}",
                    .{@tagName(object_type.base)},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .Range => |range| {
            // Analyze the start and end expressions
            const start_type = try inferTypeFromExpr(ctx, range.start);
            const end_type = try inferTypeFromExpr(ctx, range.end);

            // Both start and end should be numeric types
            if (start_type.base != .Int and start_type.base != .Float and start_type.base != .Byte) {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range start value must be numeric, got {s}",
                    .{@tagName(start_type.base)},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (end_type.base != .Int and end_type.base != .Float and end_type.base != .Byte) {
                ctx.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range end value must be numeric, got {s}",
                    .{@tagName(end_type.base)},
                );
                ctx.fatal_error.* = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // Range expressions always produce arrays of integers
            const element_type = try ctx.allocator.create(ast.TypeInfo);
            element_type.* = .{ .base = .Int };
            type_info.* = .{ .base = .Array, .array_type = element_type };
        },
        else => {
            ctx.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.UNSUPPORTED_OPERATOR,
                "Unsupported expression type: {s}",
                .{@tagName(expr.data)},
            );
            ctx.fatal_error.* = true;
            type_info.base = .Nothing;
            return type_info;
        },
    }

    // Cache the result
    try ctx.type_cache.put(expr.base.id, type_info);
    return type_info;
}

/// Unify two types, checking compatibility
pub fn unifyTypes(ctx: *TypeAnalysisContext, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
    // Handle union types first - check if actual type is compatible with the expected union
    if (expected.base == .Union) {
        if (expected.union_type) |exp_union| {
            const is_compatible = try union_handling.isTypeCompatibleWithUnion(
                actual,
                exp_union,
                ctx.allocator,
                ctx.reporter,
                span,
            );
            if (!is_compatible) {
                ctx.fatal_error.* = true;
                return;
            }
        }
    } else {
        // Non-union expected type - use regular type unification
        try unifyTypes(ctx, expected, actual, span);
    }
}

/// Resolve type information, handling custom types
pub fn resolveTypeInfo(ctx: *TypeAnalysisContext, type_info: ast.TypeInfo) !ast.TypeInfo {
    if (type_info.base == .Custom) {
        if (type_info.custom_type) |custom_type_name| {
            if (ctx.custom_types.get(custom_type_name)) |custom_type| {
                switch (custom_type.kind) {
                    .Struct => return ast.TypeInfo{ .base = .Struct, .is_mutable = false },
                    .Enum => return ast.TypeInfo{ .base = .Enum, .is_mutable = false },
                }
            }
        }
    }
    return type_info;
}

/// Deep copy type information
pub fn deepCopyTypeInfo(ctx: *TypeAnalysisContext, type_info: ast.TypeInfo) std.mem.Allocator.Error!ast.TypeInfo {
    var copy = type_info;

    if (type_info.union_type) |union_type| {
        const union_copy = try ctx.allocator.create(ast.UnionType);
        union_copy.* = .{
            .types = try ctx.allocator.dupe(*ast.TypeInfo, union_type.types),
            .current_type_index = union_type.current_type_index,
        };
        copy.union_type = union_copy;
    }

    if (type_info.function_type) |func_type| {
        const fn_copy = try ctx.allocator.create(ast.FunctionType);
        fn_copy.* = .{
            .params = try ctx.allocator.dupe(ast.TypeInfo, func_type.params),
            .return_type = try ctx.allocator.create(ast.TypeInfo),
            .param_aliases = if (func_type.param_aliases) |aliases|
                try ctx.allocator.dupe(bool, aliases)
            else
                null,
        };
        fn_copy.return_type.* = try deepCopyTypeInfo(ctx, func_type.return_type.*);
        copy.function_type = fn_copy;
    }

    if (type_info.struct_fields) |fields| {
        const fields_copy = try ctx.allocator.alloc(ast.StructFieldType, fields.len);
        for (fields, fields_copy) |field, *field_copy| {
            field_copy.* = .{
                .name = try ctx.allocator.dupe(u8, field.name),
                .type_info = try deepCopyTypeInfoPtr(ctx, field.type_info),
            };
        }
        copy.struct_fields = fields_copy;
    }

    if (type_info.array_type) |array_type| {
        const array_copy = try ctx.allocator.create(ast.TypeInfo);
        array_copy.* = try deepCopyTypeInfo(ctx, array_type.*);
        copy.array_type = array_copy;
    }

    if (type_info.custom_type) |custom_type| {
        copy.custom_type = try ctx.allocator.dupe(u8, custom_type);
    }

    return copy;
}

/// Deep copy type information pointer
pub fn deepCopyTypeInfoPtr(ctx: *TypeAnalysisContext, src: *ast.TypeInfo) !*ast.TypeInfo {
    const copy = try ctx.allocator.create(ast.TypeInfo);
    copy.* = try deepCopyTypeInfo(ctx, src.*);
    return copy;
}
