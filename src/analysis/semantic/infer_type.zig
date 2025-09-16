const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const helpers = @import("./helpers.zig");
const unifyTypes = helpers.unifyTypes;
const getLocationFromBase = helpers.getLocationFromBase;
const lookupVariable = helpers.lookupVariable;
const infer_type = @import("./infer_type.zig");

pub fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) !*ast.TypeInfo {

    // Check cache first
    if (self.type_cache.get(expr.base.id)) |cached| {
        return cached;
    }

    var type_info = try self.allocator.create(ast.TypeInfo);
    errdefer self.allocator.destroy(type_info);

    switch (expr.data) {
        .Map => |entries| {
            // Minimal inference for map literals: infer key/value from first entry if present.
            type_info.* = .{ .base = .Map };
            if (entries.len > 0) {
                const first_key_type = try inferTypeFromExpr(self, entries[0].key);
                const first_val_type = try inferTypeFromExpr(self, entries[0].value);
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
            const left_type = try inferTypeFromExpr(self, bin.left.?);
            const right_type = try inferTypeFromExpr(self, bin.right.?);

            // Operator-specific type rules
            const op = bin.operator.lexeme;

            if (std.mem.eql(u8, op, "/")) {
                // Division always returns float
                if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                        "Division requires numeric operands, got {s}",
                        .{@tagName(left_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
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
                        getLocationFromBase(expr.base),
                        ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
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
                    // Type mismatch - report compile error
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.TYPE_MISMATCH,
                        "Cannot use + operator between {s} and {s}. Both operands must be the same type.",
                        .{ @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                // Arithmetic with type promotion
                if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
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
            } else if (std.mem.eql(u8, op, "**")) {
                // Power operator with type promotion
                if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                        "Power operator requires numeric operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Power operator type promotion: Float > Int > Byte
                // If either operand is Float, result is Float
                if (left_type.base == .Float or right_type.base == .Float) {
                    type_info.* = .{ .base = .Float };
                } else if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
                std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "equals") or std.mem.eql(u8, op, "!="))
            {
                const left_numeric = (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte);
                const right_numeric = (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte);

                if (left_numeric and right_numeric) {
                    // Both are numeric - allow comparison
                    type_info.* = .{ .base = .Tetra };
                } else if (left_type.base == right_type.base) {
                    // Same type - allow comparison
                    type_info.* = .{ .base = .Tetra };
                } else if ((left_type.base == .Custom and right_type.base == .Enum) or
                    (left_type.base == .Enum and right_type.base == .Custom))
                {
                    // Allow enum variable (Custom) to be compared with enum literal (Enum)
                    type_info.* = .{ .base = .Tetra };
                } else {
                    // Incompatible types for comparison
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INCOMPATIBLE_TYPES_FOR_COMPARISON,
                        "Cannot compare {s} with {s}",
                        .{ @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else {
                try unifyTypes(self, left_type, right_type, .{ .location = getLocationFromBase(expr.base) });
                type_info.* = left_type.*;
            }
        },
        .Variable => |var_token| {
            if (lookupVariable(self, var_token.lexeme)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    type_info.* = storage.type_info.*;
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INTERNAL_ERROR,
                        "Internal error: Variable storage not found",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.VARIABLE_NOT_FOUND,
                    "Undefined variable",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
            }
        },
        .Unary => |unary| {
            const operand_type = try infer_type.inferTypeFromExpr(self, unary.right.?);
            // Handle unary operators: -, !, ~, etc.
            type_info.* = operand_type.*;
        },
        .FunctionCall => |function_call| {
            // Check if this is a method call (callee is FieldAccess)
            if (function_call.callee.data == .FieldAccess) {
                const field_access = function_call.callee.data.FieldAccess;
                const object_type = try infer_type.inferTypeFromExpr(self, field_access.object);
                const method_name = field_access.field.lexeme;

                // Check if this is a module function call (e.g., safeMath.safeAdd)
                if (field_access.object.data == .Variable) {
                    const object_name = field_access.object.data.Variable.lexeme;
                    if (helpers.isModuleNamespace(self, object_name)) {
                        // This is a module function call, not a method call
                        // Return a generic type for now - the actual type will be resolved during code generation
                        type_info.* = .{ .base = .Int }; // Assume module functions return int for now
                        return type_info;
                    }

                    // Check if this is a static method call on a struct type (e.g., Point.New)
                    if (self.custom_types.get(object_name)) |custom_type| {
                        if (custom_type.kind == .Struct) {
                            if (self.struct_methods.get(object_name)) |method_table| {
                                if (method_table.get(method_name)) |method_info| {
                                    if (method_info.is_static) {
                                        type_info.* = method_info.return_type.*;
                                        return type_info;
                                    }
                                }
                            }
                        }
                    }
                }

                // Handle method calls based on object type
                switch (object_type.base) {
                    .Array => {
                        // Handle array methods
                        if (std.mem.eql(u8, method_name, "push")) {
                            type_info.* = .{ .base = .Nothing }; // push returns nothing
                        } else if (std.mem.eql(u8, method_name, "pop")) {
                            if (object_type.array_type) |elem_type| {
                                type_info.* = elem_type.*;
                            } else {
                                type_info.* = .{ .base = .Nothing };
                            }
                        } else if (std.mem.eql(u8, method_name, "length")) {
                            type_info.* = .{ .base = .Int };
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.UNKNOWN_METHOD,
                                "Unknown array method '{s}'",
                                .{method_name},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        }
                    },
                    .String => {
                        // Handle string methods
                        if (std.mem.eql(u8, method_name, "length")) {
                            type_info.* = .{ .base = .Int };
                        } else if (std.mem.eql(u8, method_name, "bytes")) {
                            type_info.* = .{ .base = .Array };
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.UNKNOWN_METHOD,
                                "Unknown string method '{s}'",
                                .{method_name},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        }
                    },
                    .Map => {
                        // Disallow dot-methods on maps; use bracket indexing instead
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.UNKNOWN_METHOD,
                            "Maps do not have methods; use indexing (map[key]) or assignment (map[key] is value)",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    },
                    .Struct, .Custom => {
                        // Handle both Struct and Custom types for method resolution
                        var struct_name: ?[]const u8 = null;
                        if (object_type.base == .Custom) {
                            if (object_type.custom_type) |ct_name| {
                                struct_name = ct_name;
                            }
                        } else if (object_type.base == .Struct) {
                            if (object_type.custom_type) |ct_name| {
                                struct_name = ct_name;
                            }
                        }

                        // Prefer registered methods over fields when resolving a call
                        if (struct_name) |name| {
                            if (self.struct_methods.get(name)) |tbl| {
                                if (tbl.get(method_name)) |mi| {
                                    type_info.* = mi.return_type.*;
                                    return type_info;
                                }
                            }
                        }

                        // Fallback: allow calling function-typed fields as methods (if any)
                        if (object_type.struct_fields) |fields| {
                            for (fields) |field| {
                                if (std.mem.eql(u8, field.name, method_name) and field.type_info.base == .Function) {
                                    type_info.* = field.type_info.*;
                                    return type_info;
                                }
                            }
                        }

                        // If we reach here, no method matched
                        const display_name = struct_name orelse @as([]const u8, "<struct>");
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.UNKNOWN_METHOD,
                            "Unknown method '{s}' on struct '{s}'",
                            .{ method_name, display_name },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    },
                    else => {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.CANNOT_CALL_METHOD_ON_TYPE,
                            "Cannot call method '{s}' on type {s}",
                            .{ method_name, @tagName(object_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    },
                }
            } else {
                // Regular function call
                const callee_type = try infer_type.inferTypeFromExpr(self, function_call.callee);
                if (callee_type.base == .Function) {
                    if (callee_type.function_type) |func_type| {
                        // Validate argument count allowing default placeholders (~)
                        const expected_arg_count: usize = func_type.params.len;
                        var provided_arg_count: usize = 0;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data != .DefaultArgPlaceholder) provided_arg_count += 1;
                        }
                        if (provided_arg_count > expected_arg_count) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.ARGUMENT_COUNT_MISMATCH,
                                "Argument count mismatch: expected at most {}, got {}",
                                .{ expected_arg_count, provided_arg_count },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }

                        // Validate each non-placeholder argument type against parameter type
                        var param_index: usize = 0;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data == .DefaultArgPlaceholder) {
                                if (param_index < expected_arg_count) param_index += 1;
                                continue;
                            }
                            if (param_index >= expected_arg_count) break;

                            // Check if parameter requires alias but argument is not marked as alias
                            if (func_type.param_aliases != null and func_type.param_aliases.?[param_index] and !arg_expr_it.is_alias) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(arg_expr_it.expr.base),
                                    ErrorCode.ALIAS_PARAMETER_REQUIRED,
                                    "Function parameter requires an alias argument (use ^ before the argument)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }

                            // Check if parameter is not alias but argument is marked as alias
                            if (func_type.param_aliases != null and !func_type.param_aliases.?[param_index] and arg_expr_it.is_alias) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(arg_expr_it.expr.base),
                                    ErrorCode.ALIAS_ARGUMENT_NOT_NEEDED,
                                    "Function parameter does not require an alias argument (remove ^ before the argument)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }

                            const arg_type = try infer_type.inferTypeFromExpr(self, arg_expr_it.expr);
                            if (func_type.params[param_index].base != .Nothing) {
                                try helpers.unifyTypes(self, &func_type.params[param_index], arg_type, .{ .location = getLocationFromBase(expr.base) });
                            }
                            param_index += 1;
                        }

                        // Propagate return type
                        type_info.* = func_type.return_type.*;
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_FUNCTION_TYPE,
                            "Function type has no return type information",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_FUNCTION_CALL,
                        "Cannot call non-function type {s}",
                        .{@tagName(callee_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            }
        },
        .Index => |index| {
            const array_type = try infer_type.inferTypeFromExpr(self, index.array);
            const index_type = try infer_type.inferTypeFromExpr(self, index.index);

            if (array_type.base == .Array) {
                // Array indexing
                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ARRAY_INDEX_TYPE,
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
                    // FIX: If no array_type is set, try to infer from the variable declaration
                    // This handles cases like "var tape :: byte[] is [...]" where the type annotation
                    // should be used to determine the return type
                    if (index.array.data == .Variable) {
                        const var_name = index.array.data.Variable.lexeme;
                        if (self.current_scope) |scope| {
                            if (scope.lookupVariable(var_name)) |variable| {
                                // Access the type info through the scope manager's value storage
                                if (scope.manager.value_storage.get(variable.storage_id)) |storage| {
                                    if (storage.type_info.base == .Array) {
                                        if (storage.type_info.array_type) |declared_elem_type| {
                                            type_info.* = declared_elem_type.*;
                                            return type_info;
                                        } else {}
                                    }
                                } else {}
                            } else {}
                        } else {}
                    } else {}
                    type_info.base = .Nothing;
                }
            } else if (array_type.base == .Map) {
                // Map indexing - allow string, int, or enum keys, but not unions
                if (index_type.base != .String and index_type.base != .Int and index_type.base != .Enum) {
                    if (index_type.base == .Union) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                            .{},
                        );
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map key must be string, int, or enum, got {s}",
                            .{@tagName(index_type.base)},
                        );
                    }
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Use the stored map value type; this must be present if inference worked
                if (array_type.map_value_type) |value_type| {
                    type_info.* = value_type.*;
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE, // reuse for now; dedicated code can be added later
                        "Map value type not inferred; declare explicit 'returns <Type>' or add a first entry to infer",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (array_type.base == .String) {
                // String indexing
                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_STRING_INDEX_TYPE,
                        "String index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .String }; // String indexing returns a character (string)
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_INDEX_TYPE,
                    "Cannot index non-array/map/string type {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .FieldAccess => |field| {
            const object_type = try infer_type.inferTypeFromExpr(self, field.object);

            // First check if the object type is Custom and needs to be resolved to a struct
            var resolved_object_type = object_type;
            if (object_type.base == .Custom) {
                if (object_type.custom_type) |custom_type_name| {
                    // First check if this is a module namespace
                    if (helpers.isModuleNamespace(self, custom_type_name)) {
                        // Handle module namespace access
                        return helpers.handleModuleFieldAccess(self, custom_type_name, field.field.lexeme, .{ .location = getLocationFromBase(expr.base) });
                    }

                    if (self.custom_types.get(custom_type_name)) |custom_type| {
                        if (custom_type.kind == .Struct) {
                            // Convert the Custom type to a Struct type for consistent handling
                            const struct_fields = try self.allocator.alloc(ast.StructFieldType, custom_type.struct_fields.?.len);
                            for (custom_type.struct_fields.?, 0..) |custom_field, i| {
                                struct_fields[i] = .{
                                    .name = custom_field.name,
                                    .type_info = custom_field.field_type_info,
                                };
                            }
                            const resolved_type_info = try self.allocator.create(ast.TypeInfo);
                            resolved_type_info.* = ast.TypeInfo{ .base = .Struct, .custom_type = custom_type_name, .struct_fields = struct_fields, .is_mutable = object_type.is_mutable };
                            resolved_object_type = resolved_type_info;
                        } else {
                            // Not a struct, treat as enum variant access
                            type_info.* = .{ .base = .Enum };
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.TYPE_MISMATCH,
                            "Undefined type '{s}'",
                            .{custom_type_name},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    // No custom type name, treat as enum variant access
                    type_info.* = .{ .base = .Enum };
                    return type_info;
                }
            }

            // Now handle field access on the resolved object type
            if (resolved_object_type.base == .Struct) {
                if (resolved_object_type.struct_fields) |fields| {
                    for (fields) |struct_field| {
                        if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {

                            // Copy the field type info
                            type_info.* = struct_field.type_info.*;

                            // If this field is a struct type, look up its fields from custom_types
                            if (type_info.base == .Custom and type_info.custom_type != null) {
                                if (self.custom_types.get(type_info.custom_type.?)) |custom_type| {
                                    if (custom_type.kind == .Struct) {
                                        // Convert CustomTypeInfo.StructField to ast.StructFieldType
                                        const struct_fields_inner = try self.allocator.alloc(ast.StructFieldType, custom_type.struct_fields.?.len);
                                        for (custom_type.struct_fields.?, 0..) |custom_field, i| {
                                            struct_fields_inner[i] = .{
                                                .name = custom_field.name,
                                                .type_info = custom_field.field_type_info,
                                            };
                                        }
                                        type_info.* = .{ .base = .Struct, .custom_type = type_info.custom_type, .struct_fields = struct_fields_inner, .is_mutable = false };
                                    }
                                }
                            }

                            // Enforce private field access: if this struct originates from a named custom type
                            // and the field is marked private, disallow access from outside that type context.
                            if (resolved_object_type.custom_type) |owner_name| {
                                if (self.custom_types.get(owner_name)) |owner_ct| {
                                    if (owner_ct.kind == .Struct and owner_ct.struct_fields != null) {
                                        const owner_fields = owner_ct.struct_fields.?;
                                        // Find visibility for this field by name
                                        var is_public_field = false; // private by default unless marked public
                                        for (owner_fields) |ofld| {
                                            if (std.mem.eql(u8, ofld.name, struct_field.name)) {
                                                is_public_field = ofld.is_public;
                                                break;
                                            }
                                        }
                                        if (!is_public_field) {
                                            // Basic rule: only allow private access via 'this' receiver in methods of the same struct
                                            const accessed_via_this = (field.object.data == .This);
                                            if (!accessed_via_this) {
                                                self.reporter.reportCompileError(
                                                    getLocationFromBase(expr.base),
                                                    ErrorCode.PRIVATE_FIELD_ACCESS,
                                                    "Cannot access private field '{s}' of struct '{s}'",
                                                    .{ struct_field.name, owner_name },
                                                );
                                                self.fatal_error = true;
                                                type_info.base = .Nothing;
                                                return type_info;
                                            }
                                        }
                                    }
                                }
                            }
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.FIELD_NOT_FOUND,
                            "Field '{s}' not found in struct{s}",
                            .{ field.field.lexeme, if (resolved_object_type.custom_type) |name| std.fmt.allocPrint(self.allocator, " '{s}'", .{name}) catch "" else "" },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.STRUCT_HAS_NO_FIELDS,
                        "Struct{s} has no fields defined",
                        .{if (resolved_object_type.custom_type) |name| std.fmt.allocPrint(self.allocator, " '{s}'", .{name}) catch "" else ""},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (resolved_object_type.base == .Enum) {
                // Allow enum variant access: Color.Red
                // You may want to check if the variant exists, but for now just return Enum type
                type_info.* = .{ .base = .Enum };
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                    "Cannot access field on non-struct type {s}",
                    .{@tagName(resolved_object_type.base)},
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
                const first_type = try infer_type.inferTypeFromExpr(self, elements[0]);
                for (elements[1..]) |element| {
                    const element_type = try infer_type.inferTypeFromExpr(self, element);
                    try helpers.unifyTypes(self, first_type, element_type, .{ .location = getLocationFromBase(expr.base) });
                }
                const array_type = try self.allocator.create(ast.TypeInfo);
                array_type.* = first_type.*;
                type_info.* = .{ .base = .Array, .array_type = array_type };
            }
        },
        .Struct => |fields| {
            const struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
            for (fields, struct_fields) |field, *struct_field| {
                const field_type = try infer_type.inferTypeFromExpr(self, field.value);
                struct_field.* = .{
                    .name = field.name.lexeme,
                    .type_info = field_type,
                };
            }
            type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
        },
        .If => |if_expr| {
            const condition_type = try infer_type.inferTypeFromExpr(self, if_expr.condition.?);
            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            const then_type = try infer_type.inferTypeFromExpr(self, if_expr.then_branch.?);
            if (if_expr.else_branch) |else_branch| {
                const else_type = try infer_type.inferTypeFromExpr(self, else_branch);

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
                    // prefer the non-Nothing type without forcing unification. This covers cases where one
                    // branch contains control-flow like return/print and the other is a statement.
                    if (then_type.base == .Nothing and else_type.base != .Nothing) {
                        type_info.* = else_type.*;
                    } else if (else_type.base == .Nothing and then_type.base != .Nothing) {
                        type_info.* = then_type.*;
                    } else if (then_type.base != else_type.base) {
                        // As a fallback when branches differ, allow a union of both branch types
                        var members = [_]*ast.TypeInfo{ then_type, else_type };
                        const u = try helpers.createUnionType(self, members[0..]);
                        type_info.* = u.*;
                    } else {
                        // Types match - propagate
                        type_info.* = then_type.*;
                    }
                }
            } else {
                type_info.* = then_type.*;
            }
        },
        .Logical => |logical| {
            const left_type = try infer_type.inferTypeFromExpr(self, logical.left);
            const right_type = try infer_type.inferTypeFromExpr(self, logical.right);

            if (left_type.base != .Tetra or right_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_OPERAND_TYPE,
                    "Logical operators require tetra operands",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        // (Loop case handled earlier in this switch)
        .Match => |match_expr| {
            _ = try infer_type.inferTypeFromExpr(self, match_expr.value);

            // Analyze all cases and create a union type if they have different types
            if (match_expr.cases.len > 0) {
                // If the match value is a simple variable, we can narrow it inside cases
                var matched_var_name: ?[]const u8 = null;
                if (match_expr.value.data == .Variable) {
                    matched_var_name = match_expr.value.data.Variable.lexeme;
                }

                // For match expressions used as statements, analyze case bodies without type narrowing
                // This allows assignments in match cases to work properly
                var union_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
                defer union_types.deinit();

                // Infer each case body type with proper type narrowing
                for (match_expr.cases) |case| {
                    const case_type = try self.inferMatchCaseTypeWithNarrow(case, matched_var_name);
                    try union_types.append(case_type);
                }

                // If all cases return the same type, use that type
                if (union_types.items.len > 0) {
                    var all_same_type = true;
                    const first_type = union_types.items[0];

                    for (union_types.items[1..]) |case_type| {
                        if (case_type.base != first_type.base) {
                            all_same_type = false;
                            break;
                        }
                    }

                    if (all_same_type) {
                        type_info.* = first_type.*;
                    } else {
                        // Create a union type from all case types
                        const union_type_array = try self.allocator.alloc(*ast.TypeInfo, union_types.items.len);
                        for (union_types.items, union_type_array) |item, *dest| {
                            dest.* = item;
                        }

                        const union_type = try self.allocator.create(ast.UnionType);
                        union_type.* = .{ .types = union_type_array };

                        type_info.* = .{ .base = .Union, .union_type = union_type };
                    }
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .Grouping => |grouped_expr| {
            if (grouped_expr) |expr_in_parens| {
                type_info.* = (try infer_type.inferTypeFromExpr(self, expr_in_parens)).*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .Assignment => |assign| {
            if (assign.value) |value| {
                const value_type = try infer_type.inferTypeFromExpr(self, value);
                // Look up variable in scope
                if (lookupVariable(self, assign.name.lexeme)) |variable| {
                    // Check if variable is mutable
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        if (storage.constant) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_ASSIGNMENT_TARGET,
                                "Cannot assign to immutable variable '{s}'",
                                .{assign.name.lexeme},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                        // Check type compatibility
                        try helpers.unifyTypes(self, storage.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.VARIABLE_NOT_FOUND,
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
                const value_type = try infer_type.inferTypeFromExpr(self, value);
                // Look up variable in scope
                if (lookupVariable(self, compound_assign.name.lexeme)) |variable| {
                    // Check if variable is mutable
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        if (storage.constant) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_ASSIGNMENT_TARGET,
                                "Cannot assign to immutable variable '{s}'",
                                .{compound_assign.name.lexeme},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                        // Check type compatibility
                        try helpers.unifyTypes(self, storage.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.VARIABLE_NOT_FOUND,
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
                type_info.* = (try infer_type.inferTypeFromExpr(self, value)).*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .BuiltinCall => |bc| {
            const fname = bc.function.lexeme;

            // Default result on error
            type_info.* = .{ .base = .Nothing };

            // Helper to assert arity
            const requireArity = struct {
                fn check(sem: *SemanticAnalyzer, e: *ast.Expr, got: usize, expect: usize, name: []const u8) void {
                    if (got != expect) {
                        sem.reporter.reportCompileError(
                            getLocationFromBase(e.base),
                            ErrorCode.INVALID_ARGUMENT_COUNT,
                            "@{s} requires exactly {d} argument(s)",
                            .{ name, expect },
                        );
                        sem.fatal_error = true;
                    }
                }
            };

            if (std.mem.eql(u8, fname, "length")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const t0 = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (t0.base != .Array and t0.base != .String) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(bc.arguments[0].base),
                        ErrorCode.INVALID_ARRAY_TYPE,
                        "@length requires array or string, got {s}",
                        .{@tagName(t0.base)},
                    );
                    self.fatal_error = true;
                    return type_info;
                }
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "push")) {
                requireArity.check(self, expr, bc.arguments.len, 2, fname);
                if (bc.arguments.len != 2) return type_info;
                const coll_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                const val_t = try infer_type.inferTypeFromExpr(self, bc.arguments[1]);
                if (coll_t.base == .Array) {
                    if (coll_t.array_type) |elem| try helpers.unifyTypes(self, elem, val_t, .{ .location = getLocationFromBase(bc.arguments[1].base) });
                } else if (coll_t.base != .String) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@push requires array or string, got {s}", .{@tagName(coll_t.base)});
                    self.fatal_error = true;
                }
                return type_info;
            } else if (std.mem.eql(u8, fname, "pop")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const coll_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (coll_t.base == .Array) {
                    if (coll_t.array_type) |elem| type_info.* = elem.*;
                    return type_info;
                } else if (coll_t.base == .String) {
                    type_info.* = .{ .base = .String };
                    return type_info;
                } else {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@pop requires array or string, got {s}", .{@tagName(coll_t.base)});
                    self.fatal_error = true;
                    return type_info;
                }
            } else if (std.mem.eql(u8, fname, "insert")) {
                requireArity.check(self, expr, bc.arguments.len, 3, fname);
                if (bc.arguments.len != 3) return type_info;
                const coll_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                const idx_t = try infer_type.inferTypeFromExpr(self, bc.arguments[1]);
                if (idx_t.base != .Int) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@insert index must be int, got {s}", .{@tagName(idx_t.base)});
                    self.fatal_error = true;
                    return type_info;
                }
                if (coll_t.base == .Array) {
                    const val_t = try infer_type.inferTypeFromExpr(self, bc.arguments[2]);
                    if (coll_t.array_type) |elem| try helpers.unifyTypes(self, elem, val_t, .{ .location = getLocationFromBase(bc.arguments[2].base) });
                } else if (coll_t.base != .String) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@insert requires array or string, got {s}", .{@tagName(coll_t.base)});
                    self.fatal_error = true;
                    return type_info;
                }
                return type_info;
            } else if (std.mem.eql(u8, fname, "remove")) {
                requireArity.check(self, expr, bc.arguments.len, 2, fname);
                if (bc.arguments.len != 2) return type_info;
                const coll_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                const idx_t = try infer_type.inferTypeFromExpr(self, bc.arguments[1]);
                if (idx_t.base != .Int) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@remove index must be int, got {s}", .{@tagName(idx_t.base)});
                    self.fatal_error = true;
                    return type_info;
                }
                if (coll_t.base == .Array) {
                    if (coll_t.array_type) |elem| {
                        type_info.* = elem.*;
                    } else {
                        type_info.* = .{ .base = .Nothing };
                    }
                    return type_info;
                } else if (coll_t.base == .String) {
                    type_info.* = .{ .base = .Byte };
                    return type_info;
                }
                self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@remove requires array or string, got {s}", .{@tagName(coll_t.base)});
                self.fatal_error = true;
                return type_info;
            } else if (std.mem.eql(u8, fname, "slice")) {
                requireArity.check(self, expr, bc.arguments.len, 3, fname);
                if (bc.arguments.len != 3) return type_info;
                const coll_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                const start_t = try infer_type.inferTypeFromExpr(self, bc.arguments[1]);
                const len_t = try infer_type.inferTypeFromExpr(self, bc.arguments[2]);
                if (start_t.base != .Int or len_t.base != .Int) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice start/length must be ints", .{});
                    self.fatal_error = true;
                    return type_info;
                }
                if (coll_t.base == .String) {
                    type_info.* = .{ .base = .String };
                } else if (coll_t.base == .Array) {
                    if (coll_t.array_type) |elem| {
                        const new_elem = try self.allocator.create(ast.TypeInfo);
                        new_elem.* = elem.*;
                        type_info.* = .{ .base = .Array, .array_type = new_elem };
                    } else {
                        type_info.* = .{ .base = .Array };
                    }
                } else {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice requires array or string, got {s}", .{@tagName(coll_t.base)});
                    self.fatal_error = true;
                }
                return type_info;
            } else if (std.mem.eql(u8, fname, "string")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                type_info.* = .{ .base = .String };
                return type_info;
            } else if (std.mem.eql(u8, fname, "int")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                // Always return union: int | ValueError
                const int_t = try self.allocator.create(ast.TypeInfo);
                int_t.* = .{ .base = .Int };
                const err_t = try self.allocator.create(ast.TypeInfo);
                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                var union_members = [_]*ast.TypeInfo{ int_t, err_t };
                const u = try helpers.createUnionType(self, union_members[0..]);
                type_info.* = u.*;
                return type_info;
            } else if (std.mem.eql(u8, fname, "float")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                // Always return union: float | ValueError
                const float_t = try self.allocator.create(ast.TypeInfo);
                float_t.* = .{ .base = .Float };
                const err_t = try self.allocator.create(ast.TypeInfo);
                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                var union_members = [_]*ast.TypeInfo{ float_t, err_t };
                const u = try helpers.createUnionType(self, union_members[0..]);
                type_info.* = u.*;
                return type_info;
            } else if (std.mem.eql(u8, fname, "byte")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const a0_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                // String or numeric -> byte | ValueError
                _ = a0_t; // All supported inputs converge to same union type
                const byte_t = try self.allocator.create(ast.TypeInfo);
                byte_t.* = .{ .base = .Byte };
                const err_t = try self.allocator.create(ast.TypeInfo);
                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                var union_members = [_]*ast.TypeInfo{ byte_t, err_t };
                const u = try helpers.createUnionType(self, union_members[0..]);
                type_info.* = u.*;
                return type_info;
            } else if (std.mem.eql(u8, fname, "type")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                type_info.* = .{ .base = .String };
                return type_info;
            } else if (std.mem.eql(u8, fname, "input")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .String };
                return type_info;
            } else if (std.mem.eql(u8, fname, "os")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .String };
                return type_info;
            } else if (std.mem.eql(u8, fname, "arch")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .String };
                return type_info;
            } else if (std.mem.eql(u8, fname, "time")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "exit")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                // Validate that the argument is an integer
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Int and arg_type.base != .Byte) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.TYPE_MISMATCH, "@exit: argument must be an integer", .{});
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Nothing };
                return type_info;
            } else if (std.mem.eql(u8, fname, "sleep")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                // Validate that the argument is an integer
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Int and arg_type.base != .Byte) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.TYPE_MISMATCH, "@sleep: argument must be an integer", .{});
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Nothing };
                return type_info;
            } else if (std.mem.eql(u8, fname, "random")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .Float };
                return type_info;
            }

            self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.NOT_IMPLEMENTED, "Unknown builtin '@{s}'", .{fname});
            self.fatal_error = true;
            return type_info;
        },
        .InternalCall => |method_call| {
            var receiver_type = try infer_type.inferTypeFromExpr(self, method_call.receiver);
            const method_name = method_call.method.lexeme;

            // Validate receiver type based on method
            switch (method_call.method.type) {
                // Array methods
                .PUSH,
                .POP,
                .INSERT,
                .REMOVE,
                .SLICE,
                => {
                    // If inference yielded Nothing but the receiver is a known variable,
                    // fall back to its declared type. This helps for statement-level method calls
                    // where the receiver was initialized to a default (e.g., empty array) and
                    // type info exists in storage.
                    if (receiver_type.base == .Nothing and method_call.receiver.data == .Variable) {
                        const var_name = method_call.receiver.data.Variable.lexeme;
                        if (lookupVariable(self, var_name)) |variable| {
                            if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                receiver_type = storage.type_info;
                            }
                        }
                    }
                    const allow_string_for_method = receiver_type.base == .String and (method_call.method.type == .PUSH or method_call.method.type == .POP or method_call.method.type == .INSERT or method_call.method.type == .REMOVE or method_call.method.type == .SLICE);
                    if (receiver_type.base != .Array and !allow_string_for_method) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(method_call.receiver.base),
                            ErrorCode.INVALID_ARRAY_TYPE,
                            "Cannot call array method '{s}' on non-array type {s}",
                            .{ method_name, @tagName(receiver_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.* = .{ .base = .Nothing };
                        return type_info;
                    }

                    // Transform into appropriate array operation
                    switch (method_call.method.type) {
                        .PUSH => {
                            if (method_call.arguments.len != 1) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.INVALID_ARRAY_TYPE,
                                    "@push requires exactly one argument",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            const value_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            if (receiver_type.base == .Array) {
                                // Check element type matches array
                                if (receiver_type.array_type) |elem_type| {
                                    try helpers.unifyTypes(self, elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[0].base) });
                                }
                            } else if (receiver_type.base == .String) {
                                // String concatenation: require value to be String
                                if (value_type.base != .String) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.TYPE_MISMATCH,
                                        "@push on string requires string value, got {s}",
                                        .{@tagName(value_type.base)},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                            }

                            // Lower to BuiltinCall: @push(array, element)
                            var args = try self.allocator.alloc(*ast.Expr, 2);
                            args[0] = method_call.receiver;
                            args[1] = method_call.arguments[0];
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },
                        .POP => {
                            if (method_call.arguments.len != 0) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_COUNT,
                                    "@pop requires no arguments",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Lower to BuiltinCall: @pop(array)
                            var args = try self.allocator.alloc(*ast.Expr, 1);
                            args[0] = method_call.receiver;
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },
                        .INSERT => {
                            if (method_call.arguments.len != 2) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_COUNT,
                                    "@insert requires exactly two arguments (index, element)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Check index is integer
                            const index_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            if (index_type.base != .Int) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                                    "@insert index must be integer, got {s}",
                                    .{@tagName(index_type.base)},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Check element type matches array
                            const value_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[1]);
                            if (receiver_type.array_type) |elem_type| {
                                try helpers.unifyTypes(self, elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[1].base) });
                            }

                            // Lower to BuiltinCall: @insert(array, index, element)
                            var args = try self.allocator.alloc(*ast.Expr, 3);
                            args[0] = method_call.receiver;
                            args[1] = method_call.arguments[0];
                            args[2] = method_call.arguments[1];
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },
                        .REMOVE => {
                            if (method_call.arguments.len != 1) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_COUNT,
                                    "@remove requires exactly one argument (index)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Check index is integer
                            const index_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            if (index_type.base != .Int) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                                    "@remove index must be integer, got {s}",
                                    .{@tagName(index_type.base)},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Lower to BuiltinCall: @remove(array, index)
                            var args = try self.allocator.alloc(*ast.Expr, 2);
                            args[0] = method_call.receiver;
                            args[1] = method_call.arguments[0];
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },
                        .SLICE => {
                            if (method_call.arguments.len != 2) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_COUNT,
                                    "@slice requires exactly two arguments (start, length)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Check start and length are integers
                            const start_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            const length_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[1]);
                            if (start_type.base != .Int) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.TYPE_MISMATCH,
                                    "@slice start index must be integer, got {s}",
                                    .{@tagName(start_type.base)},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }
                            if (length_type.base != .Int) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[1].base),
                                    ErrorCode.TYPE_MISMATCH,
                                    "@slice length must be integer, got {s}",
                                    .{@tagName(length_type.base)},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }

                            // Return type is union: T | ValueError.OutOfBounds
                            if (receiver_type.base == .Array) {
                                // Create union: ArrayType | ValueError (preserve element type)
                                const array_t = try self.allocator.create(ast.TypeInfo);
                                array_t.* = receiver_type.*;
                                const err_t = try self.allocator.create(ast.TypeInfo);
                                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                var union_members = [_]*ast.TypeInfo{ array_t, err_t };
                                const u = try helpers.createUnionType(self, union_members[0..]);
                                type_info.* = u.*;
                            } else if (receiver_type.base == .String) {
                                // Create union: String | ValueError
                                const string_t = try self.allocator.create(ast.TypeInfo);
                                string_t.* = .{ .base = .String };
                                const err_t = try self.allocator.create(ast.TypeInfo);
                                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                var union_members = [_]*ast.TypeInfo{ string_t, err_t };
                                const u = try helpers.createUnionType(self, union_members[0..]);
                                type_info.* = u.*;
                            }

                            // Lower to BuiltinCall: @slice(container, start, length)
                            var args = try self.allocator.alloc(*ast.Expr, 3);
                            args[0] = method_call.receiver;
                            args[1] = method_call.arguments[0];
                            args[2] = method_call.arguments[1];
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },

                        else => {
                            // For now, other array methods not implemented
                            self.reporter.reportCompileError(
                                getLocationFromBase(method_call.receiver.base),
                                ErrorCode.NOT_IMPLEMENTED,
                                "Array method '{s}' not yet implemented",
                                .{method_name},
                            );
                            self.fatal_error = true;
                            type_info.* = .{ .base = .Nothing };
                        },
                    }
                },

                // Type method (legacy) — convert to BuiltinCall("type", receiver)
                .TYPE => {
                    var args = try self.allocator.alloc(*ast.Expr, 1);
                    args[0] = method_call.receiver;
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

                // Built-in simple methods (legacy) — map to BuiltinCall
                .LENGTH => {
                    var args = try self.allocator.alloc(*ast.Expr, 1);
                    args[0] = method_call.receiver;
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

                // String methods
                .TOSTRING, .TOINT, .TOFLOAT, .TOBYTE => {
                    switch (method_call.method.type) {
                        .TOSTRING => {
                            // Any type can be converted to string
                            type_info.* = .{ .base = .String };
                        },
                        .TOINT, .TOFLOAT, .TOBYTE => {
                            // Check receiver is string
                            if (receiver_type.base != .String) {
                                // Non-string receivers are allowed for numeric conversions; set return type directly
                                // Numeric conversions can overflow/underflow: always produce union types
                                switch (method_call.method.type) {
                                    .TOINT => {
                                        const a = try self.allocator.create(ast.TypeInfo);
                                        a.* = .{ .base = .Int };
                                        const b = try self.allocator.create(ast.TypeInfo);
                                        b.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                        var members = [_]*ast.TypeInfo{ a, b };
                                        const u = try helpers.createUnionType(self, members[0..]);
                                        type_info.* = u.*;
                                    },
                                    .TOFLOAT => {
                                        const a = try self.allocator.create(ast.TypeInfo);
                                        a.* = .{ .base = .Float };
                                        const b = try self.allocator.create(ast.TypeInfo);
                                        b.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                        var members = [_]*ast.TypeInfo{ a, b };
                                        const u = try helpers.createUnionType(self, members[0..]);
                                        type_info.* = u.*;
                                    },
                                    .TOBYTE => {
                                        // If receiver is string, result is byte[] with possible parse/out-of-bounds error is not represented; keep array type
                                        if (receiver_type.base == .String) {
                                            const elem = try self.allocator.create(ast.TypeInfo);
                                            elem.* = .{ .base = .Byte };
                                            type_info.* = .{ .base = .Array, .array_type = elem };
                                        } else {
                                            const a = try self.allocator.create(ast.TypeInfo);
                                            a.* = .{ .base = .Byte };
                                            const b = try self.allocator.create(ast.TypeInfo);
                                            b.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                            var members = [_]*ast.TypeInfo{ a, b };
                                            const u = try helpers.createUnionType(self, members[0..]);
                                            type_info.* = u.*;
                                        }
                                    },
                                    else => unreachable,
                                }
                                return type_info;
                            }

                            // For string receivers, handle parse semantics and unions where applicable
                            // For now, do not fold literals; keep as builtin for uniformity
                            if (method_call.method.type == .TOINT and method_call.receiver.data == .Literal) {
                                var args = try self.allocator.alloc(*ast.Expr, 1);
                                args[0] = method_call.receiver;
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                const int_t = try self.allocator.create(ast.TypeInfo);
                                int_t.* = .{ .base = .Int };
                                const err_t = try self.allocator.create(ast.TypeInfo);
                                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                var union_members = [_]*ast.TypeInfo{ int_t, err_t };
                                const u = try helpers.createUnionType(self, union_members[0..]);
                                type_info.* = u.*;
                                return type_info;
                            }

                            // Lower to builtin for runtime string->int conversion when receiver is not literal
                            if (method_call.method.type == .TOINT) {
                                // Return type: int | ValueError
                                const int_t = try self.allocator.create(ast.TypeInfo);
                                int_t.* = .{ .base = .Int };
                                const err_t = try self.allocator.create(ast.TypeInfo);
                                err_t.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                var union_members = [_]*ast.TypeInfo{ int_t, err_t };
                                const u = try helpers.createUnionType(self, union_members[0..]);
                                var args = try self.allocator.alloc(*ast.Expr, 1);
                                args[0] = method_call.receiver;
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                type_info.* = u.*;
                                return type_info;
                            }
                            if (method_call.method.type == .TOFLOAT) {
                                const a = try self.allocator.create(ast.TypeInfo);
                                a.* = .{ .base = .Float };
                                const b = try self.allocator.create(ast.TypeInfo);
                                b.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                var members = [_]*ast.TypeInfo{ a, b };
                                const u = try helpers.createUnionType(self, members[0..]);
                                type_info.* = u.*;
                                return type_info;
                            }
                            if (method_call.method.type == .TOBYTE) {
                                if (receiver_type.base == .String) {
                                    const elem = try self.allocator.create(ast.TypeInfo);
                                    elem.* = .{ .base = .Byte };
                                    type_info.* = .{ .base = .Array, .array_type = elem };
                                } else {
                                    const a = try self.allocator.create(ast.TypeInfo);
                                    a.* = .{ .base = .Byte };
                                    const b = try self.allocator.create(ast.TypeInfo);
                                    b.* = .{ .base = .Custom, .custom_type = "ValueError" };
                                    var members = [_]*ast.TypeInfo{ a, b };
                                    const u = try helpers.createUnionType(self, members[0..]);
                                    type_info.* = u.*;
                                }
                                return type_info;
                            }
                        },
                        else => unreachable,
                    }
                },

                // I/O methods
                .READ,
                .WRITE,
                => {
                    switch (method_call.method.type) {
                        .READ => {
                            // Check path argument is string
                            if (method_call.arguments.len != 1) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_TYPE,
                                    "@read requires exactly one string argument (path)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }
                            const path_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            if (path_type.base != .String) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.INVALID_FILE_PATH_TYPE,
                                    "@read path must be string, got {s}",
                                    .{@tagName(path_type.base)},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }
                            type_info.* = .{ .base = .String };
                        },
                        .WRITE => {
                            // Check path and content arguments are strings
                            if (method_call.arguments.len != 2) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.INVALID_ARGUMENT_TYPE,
                                    "@write requires two arguments (path, content)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }
                            const path_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[0]);
                            const content_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[1]);
                            if (path_type.base != .String or content_type.base != .String) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.arguments[0].base),
                                    ErrorCode.INVALID_FILE_PATH_TYPE,
                                    "@write arguments must be strings, got {s} and {s}",
                                    .{ @tagName(path_type.base), @tagName(content_type.base) },
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                                return type_info;
                            }
                            type_info.* = .{ .base = .Tetra }; // Returns success/failure
                        },

                        else => unreachable,
                    }
                },

                // System information methods (no receiver required)
                .OS, .ARCH, .TIME => {
                    // These methods don't require a receiver, they're called as @os(), @arch(), @time()
                    // Transform into BuiltinCall with no arguments
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = &[_]*ast.Expr{} } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

                // Other methods
                else => {
                    self.reporter.reportCompileError(
                        getLocationFromBase(method_call.receiver.base),
                        ErrorCode.NOT_IMPLEMENTED,
                        "Unknown method '{s}'",
                        .{method_name},
                    );
                    self.fatal_error = true;
                    type_info.* = .{ .base = .Nothing };
                },
            }
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
            const expr_type = try infer_type.inferTypeFromExpr(self, _peek.expr);
            type_info.* = expr_type.*; // Peek returns the same type as the expression
        },
        .PeekStruct => |_peek_struct| {
            const expr_type = try infer_type.inferTypeFromExpr(self, _peek_struct.expr);
            type_info.* = expr_type.*; // PeekStruct returns the same type as the expression
        },
        .Print => {},
        .IndexAssign => |index_assign| {
            const array_type = try infer_type.inferTypeFromExpr(self, index_assign.array);
            const index_type = try infer_type.inferTypeFromExpr(self, index_assign.index);
            const value_type = try infer_type.inferTypeFromExpr(self, index_assign.value);

            if (array_type.base != .Array and array_type.base != .Map) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_INDEX_TYPE,
                    "Cannot assign to index of non-array/non-map type {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // For arrays, index must be Int; for maps, index can be Int or String
            if (array_type.base == .Array and index_type.base != .Int) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                    "Array index must be integer, got {s}",
                    .{@tagName(index_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            } else if (array_type.base == .Map and index_type.base != .Int and index_type.base != .String and index_type.base != .Enum) {
                if (index_type.base == .Union) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE,
                        "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                        .{},
                    );
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE,
                        "Map key must be integer, string, or enum, got {s}",
                        .{@tagName(index_type.base)},
                    );
                }
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (array_type.array_type) |elem_type| {
                try helpers.unifyTypes(self, elem_type, value_type, .{ .location = getLocationFromBase(expr.base) });
            }

            type_info.* = .{ .base = .Nothing }; // IndexAssign has no return value
        },
        .FieldAssignment => |field_assign| {
            const object_type = try infer_type.inferTypeFromExpr(self, field_assign.object);
            const value_type = try infer_type.inferTypeFromExpr(self, field_assign.value);

            if (object_type.base != .Struct and object_type.base != .Custom) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                    "Cannot assign to field of non-struct type {s}",
                    .{@tagName(object_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (object_type.base == .Struct and object_type.struct_fields != null) {
                const fields = object_type.struct_fields.?;
                for (fields) |struct_field| {
                    if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                        try helpers.unifyTypes(self, struct_field.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                        break;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.FIELD_NOT_FOUND,
                        "Field '{s}' not found in struct",
                        .{field_assign.field.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (object_type.base == .Custom and object_type.custom_type != null) {
                const custom_type_name = object_type.custom_type.?;
                if (self.custom_types.get(custom_type_name)) |custom_type| {
                    if (custom_type.kind == .Struct and custom_type.struct_fields != null) {
                        const fields = custom_type.struct_fields.?;
                        for (fields) |struct_field| {
                            if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                                // For field assignment, we just need to validate that the value type is compatible
                                // We can't do full type unification since we don't have the full type info
                                break;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.FIELD_NOT_FOUND,
                                "Field '{s}' not found in struct '{s}'",
                                .{ field_assign.field.lexeme, custom_type_name },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                            "Cannot assign to field of non-struct type {s}",
                            .{@tagName(object_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.FIELD_NOT_FOUND,
                        "Undefined type '{s}'",
                        .{custom_type_name},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            }

            type_info.* = .{ .base = .Nothing }; // FieldAssignment has no return value
        },
        .Exists => |exists| {
            const array_type = try infer_type.inferTypeFromExpr(self, exists.array);

            // Create a temporary scope for the bound variable
            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            // Add the bound variable to the quantifier scope
            // Infer the type from the array element type
            var bound_var_type = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

            _ = quantifier_scope.createValueBinding(
                exists.variable.lexeme, // "e"
                TokenLiteral{ .nothing = {} },
                helpers.convertTypeToTokenType(self, bound_var_type.base),
                &bound_var_type,
                true, // Bound variables are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
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

            const condition_type = try infer_type.inferTypeFromExpr(self, exists.condition);

            // Restore previous scope
            self.current_scope = prev_scope;

            // Clean up the quantifier scope
            quantifier_scope.deinit();

            if (array_type.base != .Array) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_TYPE,
                    "Exists requires array type, got {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Exists condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        .ForAll => |for_all| {
            const array_type = try infer_type.inferTypeFromExpr(self, for_all.array);

            // Create a temporary scope for the bound variable
            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            // Add the bound variable to the quantifier scope
            // Infer the type from the array element type
            var bound_var_type = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

            _ = quantifier_scope.createValueBinding(
                for_all.variable.lexeme, // "u"
                TokenLiteral{ .nothing = {} },
                helpers.convertTypeToTokenType(self, bound_var_type.base),
                &bound_var_type,
                true, // Bound variables are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
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

            const condition_type = try infer_type.inferTypeFromExpr(self, for_all.condition);

            // Restore previous scope
            self.current_scope = prev_scope;

            // Clean up the quantifier scope
            quantifier_scope.deinit();

            if (array_type.base != .Array) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_TYPE,
                    "ForAll requires array type, got {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "ForAll condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        .Assert => |assert| {
            const condition_type = try infer_type.inferTypeFromExpr(self, assert.condition);
            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Assert condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Nothing }; // Assert has no return value
        },
        .StructDecl => {
            type_info.* = .{ .base = .Struct };
        },
        .StructLiteral => |struct_lit| {
            // Look up the struct declaration to get canonical TypeInfo
            if (lookupVariable(self, struct_lit.name.lexeme)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    if (storage.type_info.base == .Struct) {
                        // Just point to the canonical TypeInfo
                        type_info.* = storage.type_info.*;

                        // Validate struct literal fields against declared struct fields
                        if (type_info.struct_fields) |decl_fields| {
                            // 1) Check field count matches
                            if (decl_fields.len != struct_lit.fields.len) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(expr.base),
                                    ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                                    "Struct field count mismatch: expected {}, got {}",
                                    .{ decl_fields.len, struct_lit.fields.len },
                                );
                                self.fatal_error = true;
                            }

                            // 2) For each provided field, ensure it exists and types unify
                            for (struct_lit.fields) |lit_field| {
                                var found = false;
                                for (decl_fields) |decl_field| {
                                    if (std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) {
                                        found = true;
                                        const lit_type = try infer_type.inferTypeFromExpr(self, lit_field.value);
                                        try helpers.unifyTypes(
                                            self,
                                            decl_field.type_info,
                                            lit_type,
                                            ast.SourceSpan.fromToken(lit_field.name),
                                        );
                                        break;
                                    }
                                }
                                if (!found) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(lit_field.value.base),
                                        ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                                        "Field '{s}' not found in struct '{s}'",
                                        .{ lit_field.name.lexeme, struct_lit.name.lexeme },
                                    );
                                    self.fatal_error = true;
                                }
                            }
                        }
                    } else {
                        // Fallback to basic struct type
                        type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
                    }
                } else {
                    type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
                }
            } else {
                type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
            }
        },
        .EnumDecl => {
            type_info.* = .{ .base = .Enum };
        },
        .ArrayType => {
            type_info.* = .{ .base = .Array };
        },
        .Block => |block| {
            // Ensure statements inside a block expression are validated so that
            // statement-level transformations (e.g., @push lowering) occur within blocks
            const prev_scope = self.current_scope;
            const block_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            self.current_scope = block_scope;
            defer {
                self.current_scope = prev_scope;
                block_scope.deinit();
            }

            try self.validateStatements(block.statements);

            // Always analyze the block's final value expression (if any) so that
            // expression-level transformations (like lowering compiler methods) occur
            // even inside loop scopes. When in a loop, we still report the type as Nothing.
            var value_type_ptr: ?*ast.TypeInfo = null;
            if (block.value) |value_expr| {
                value_type_ptr = try infer_type.inferTypeFromExpr(self, value_expr);
            }

            if (self.in_loop_scope) {
                // If we are in a loop scope, the block usually has no value in terms of control flow.
                // However, if the block contains an expression that returns a value (e.g., an error),
                // we should propagate that type.
                if (value_type_ptr) |vt| {
                    type_info.* = vt.*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            } else if (value_type_ptr) |vt| {
                type_info.* = vt.*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .TypeExpr => |type_expr| {
            // Convert TypeExpr to TypeInfo
            const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
            type_info.* = type_info_ptr.*;
            self.allocator.destroy(type_info_ptr);
        },
        .Cast => |cast| {
            // Enforce: 'as' requires an else; then is optional
            if (cast.else_branch == null) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.EXPECTED_CLOSING_BRACE,
                    "'as' requires an 'else' block",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
            // For cast expressions (value as Type), the result type is the target type
            // Parse the target type from the cast expression
            const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
            type_info.* = target_type_info.*;

            // Also validate that the value being cast is a union type
            const value_type = try infer_type.inferTypeFromExpr(self, cast.value);
            if (value_type.base != .Union) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_OPERAND_TYPE,
                    "Type casting 'as' can only be used with union types, got {s}",
                    .{@tagName(value_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // Handle the then branch in a new scope with narrowed type
            var then_type: ?*ast.TypeInfo = null;
            if (cast.then_branch) |then_expr| {
                // Create a scope for the then branch where we know the type
                const then_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                defer then_scope.deinit();

                const prev_scope = self.current_scope;
                self.current_scope = then_scope;
                defer self.current_scope = prev_scope;

                // If the cast value is a variable, shadow it with the narrowed type
                if (cast.value.data == .Variable) {
                    const var_name = cast.value.data.Variable.lexeme;
                    const token_type = helpers.convertTypeToTokenType(self, target_type_info.base);
                    _ = then_scope.createValueBinding(
                        var_name,
                        TokenLiteral{ .nothing = {} },
                        token_type,
                        target_type_info,
                        false,
                    ) catch {};
                } else if (cast.value.data == .FieldAccess) {
                    const field_access = cast.value.data.FieldAccess;
                    // Only support narrowing when object is a simple variable (e.g., token.value)
                    if (field_access.object.data == .Variable) {
                        const obj_name = field_access.object.data.Variable.lexeme;
                        const fld_name = field_access.field.lexeme;
                        // Lookup original variable type
                        if (lookupVariable(self, obj_name)) |variable| {
                            if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                const original_type = storage.type_info.*;
                                // Build a struct type view of the object
                                var struct_fields: ?[]ast.StructFieldType = null;
                                if (original_type.base == .Struct and original_type.struct_fields != null) {
                                    struct_fields = original_type.struct_fields.?;
                                } else if (original_type.base == .Custom and original_type.custom_type != null) {
                                    // Expand custom struct fields from registry
                                    if (self.custom_types.get(original_type.custom_type.?)) |custom_type| {
                                        if (custom_type.kind == .Struct and custom_type.struct_fields != null) {
                                            const ct_fields = custom_type.struct_fields.?;
                                            const new_sf = try self.allocator.alloc(ast.StructFieldType, ct_fields.len);
                                            for (ct_fields, 0..) |ct_field, i| {
                                                new_sf[i] = .{ .name = ct_field.name, .type_info = ct_field.field_type_info };
                                            }
                                            struct_fields = new_sf;
                                        }
                                    }
                                }
                                if (struct_fields) |fields_arr| {
                                    // Duplicate and narrow the specific field
                                    const dup_fields = try self.allocator.alloc(ast.StructFieldType, fields_arr.len);
                                    for (fields_arr, 0..) |sf, i| {
                                        if (std.mem.eql(u8, sf.name, fld_name)) {
                                            dup_fields[i] = .{ .name = sf.name, .type_info = target_type_info };
                                        } else {
                                            dup_fields[i] = sf;
                                        }
                                    }
                                    const narrowed_struct = try self.allocator.create(ast.TypeInfo);
                                    narrowed_struct.* = .{ .base = .Struct, .struct_fields = dup_fields };
                                    _ = then_scope.createValueBinding(
                                        obj_name,
                                        TokenLiteral{ .nothing = {} },
                                        .STRUCT,
                                        narrowed_struct,
                                        false,
                                    ) catch {};
                                }
                            }
                        }
                    }
                }
                // Analyze the then branch with the narrowed type
                then_type = try infer_type.inferTypeFromExpr(self, then_expr);
            }

            // Handle the else branch (no type narrowing)
            var else_type: ?*ast.TypeInfo = null;
            if (cast.else_branch) |else_expr| {
                else_type = try infer_type.inferTypeFromExpr(self, else_expr);
                // Treat control-flow exits (e.g., return ...) as Nothing for type inference
                if (else_expr.data == .ReturnExpr) {
                    const nothing_type = try self.allocator.create(ast.TypeInfo);
                    nothing_type.* = .{ .base = .Nothing };
                    else_type = nothing_type;
                }
            }

            // For an as expression with then/else, first set the type to the target type
            // This ensures proper type inference in the branches
            type_info.* = target_type_info.*;

            // For an as expression with then/else, the type depends on the branch types
            if (then_type) |tt| {
                if (else_type) |et| {
                    // Both then and else exist: result can be either branch
                    if (tt.base == et.base) {
                        type_info.* = tt.*;
                    } else {
                        var types = [_]*ast.TypeInfo{ tt, et };
                        const union_type = try helpers.createUnionType(self, &types);
                        type_info.* = union_type.*;
                    }
                } else {
                    // Only then branch exists: result type is the then-type
                    type_info.* = tt.*;
                }
            } else if (else_type) |et| {
                // Else-only: if else yields Nothing (control-flow exit), result is target type
                if (et.base == .Nothing) {
                    type_info.* = target_type_info.*;
                } else if (target_type_info.base == et.base) {
                    type_info.* = target_type_info.*;
                } else {
                    var types = [_]*ast.TypeInfo{ target_type_info, et };
                    const union_type = try helpers.createUnionType(self, &types);
                    type_info.* = union_type.*;
                }
            } else {
                // No branches: type remains the target type
                type_info.* = target_type_info.*;
            }
        },
        .Loop => |loop| {
            // Create outer loop scope (for loop-level variables)
            const outer_loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            const prev_scope = self.current_scope;
            self.current_scope = outer_loop_scope;
            defer {
                self.current_scope = prev_scope;
                outer_loop_scope.deinit();
            }

            const prev_in_loop_scope = self.in_loop_scope;
            self.in_loop_scope = true;
            defer self.in_loop_scope = prev_in_loop_scope;

            // Handle optional initializer statement
            if (loop.var_decl) |vd| {
                var single: [1]ast.Stmt = .{vd.*};
                try self.validateStatements(single[0..]);
            }

            // Analyze optional condition
            if (loop.condition) |cond| {
                _ = try infer_type.inferTypeFromExpr(self, cond);
            }

            // Create a new scope for EACH ITERATION
            const iteration_scope = try self.memory.scope_manager.createScope(outer_loop_scope, self.memory);
            defer iteration_scope.deinit();

            // Rebind loop variable for each iteration
            if (loop.var_decl) |vd| {
                // Save current scope
                const saved_scope = self.current_scope;
                self.current_scope = iteration_scope;

                // Re-analyze variable declaration in iteration scope
                var single: [1]ast.Stmt = .{vd.*};
                try self.validateStatements(single[0..]);

                // Restore scope
                self.current_scope = saved_scope;
            }

            // Analyze loop body in iteration scope
            {
                const saved_scope = self.current_scope;
                self.current_scope = iteration_scope;
                defer self.current_scope = saved_scope;

                _ = try infer_type.inferTypeFromExpr(self, loop.body);
            }

            // Analyze optional step
            if (loop.step) |stp| {
                _ = try infer_type.inferTypeFromExpr(self, stp);
            }

            // Loops do not yield a value
            type_info.* = .{ .base = .Nothing };
        },
        .This => {
            // 'this' refers to the current struct instance in method context
            // For now, return a generic struct type - this will be resolved during code generation
            // TODO: Implement proper 'this' type resolution based on the current method context
            type_info.* = .{ .base = .Struct };
        },
        .Range => |range| {
            // Analyze the start and end expressions
            const start_type = try inferTypeFromExpr(self, range.start);
            const end_type = try inferTypeFromExpr(self, range.end);

            // Both start and end should be numeric types
            if (start_type.base != .Int and start_type.base != .Float and start_type.base != .Byte) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range start value must be numeric, got {s}",
                    .{@tagName(start_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (end_type.base != .Int and end_type.base != .Float and end_type.base != .Byte) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range end value must be numeric, got {s}",
                    .{@tagName(end_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // Range expressions always produce arrays of integers
            const element_type = try self.allocator.create(ast.TypeInfo);
            element_type.* = .{ .base = .Int };
            type_info.* = .{ .base = .Array, .array_type = element_type };
        },
        // No ForEach expression variant; 'each' is desugared to a Loop statement
    }

    // Cache the result
    try self.type_cache.put(expr.base.id, type_info);
    return type_info;
}
