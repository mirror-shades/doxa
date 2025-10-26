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
    if (self.type_cache.get(expr.base.id)) |cached| {
        return cached;
    }

    var type_info = try self.allocator.create(ast.TypeInfo);
    errdefer self.allocator.destroy(type_info);

    switch (expr.data) {
        .Map => |entries| {
            type_info.* = .{ .base = .Map };
            if (entries.len > 0) {
                const first_key_type = try inferTypeFromExpr(self, entries[0].key);
                const first_val_type = try inferTypeFromExpr(self, entries[0].value);
                type_info.map_key_type = first_key_type;
                type_info.map_value_type = first_val_type;
            }
        },
        .Literal => |lit| {
            type_info.inferFrom(lit);
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

            const op = bin.operator.lexeme;

            if (std.mem.eql(u8, op, "/")) {
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
                if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "+")) {
                if (left_type.base == .String and right_type.base == .String) {
                    type_info.* = .{ .base = .String };
                } else if (left_type.base == .Array and right_type.base == .Array) {
                    type_info.* = .{ .base = .Array };
                } else if (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte or
                    right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte)
                {
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else {
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

                if (left_type.base == .Float or right_type.base == .Float) {
                    type_info.* = .{ .base = .Float };
                } else if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "**")) {
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
                    type_info.* = .{ .base = .Tetra };
                } else if (left_type.base == right_type.base) {
                    type_info.* = .{ .base = .Tetra };
                } else if ((left_type.base == .Custom and right_type.base == .Enum) or
                    (left_type.base == .Enum and right_type.base == .Custom))
                {
                    type_info.* = .{ .base = .Tetra };
                } else {
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
            type_info.* = operand_type.*;
        },
        .FunctionCall => |function_call| {
            if (function_call.callee.data == .FieldAccess) {
                const fa = function_call.callee.data.FieldAccess;
                if (fa.object.data == .FieldAccess) {
                    const inner = fa.object.data.FieldAccess;
                    if (inner.object.data == .Variable) {
                        const alias = inner.object.data.Variable.lexeme;
                        if (helpers.isModuleNamespace(self, alias)) {
                            if (self.parser) |p| {
                                if (p.module_namespaces.get(alias)) |mi| {
                                    if (std.mem.eql(u8, mi.name, "graphics") or std.mem.eql(u8, mi.file_path, "graphics")) {
                                        if (fa.field.lexeme.len >= 2) {
                                            const submodule = fa.field.lexeme;
                                            if (std.mem.eql(u8, submodule, "doxa")) {
                                                if (function_call.arguments.len >= 4) {
                                                    type_info.* = .{ .base = .Struct, .custom_type = "graphics.App" };
                                                    return type_info;
                                                }
                                            }
                                        }
                                        type_info.* = .{ .base = .Int };
                                        return type_info;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (function_call.callee.data == .FieldAccess) {
                const field_access = function_call.callee.data.FieldAccess;
                const object_type = try infer_type.inferTypeFromExpr(self, field_access.object);
                const method_name = field_access.field.lexeme;

                if (field_access.object.data == .Variable) {
                    const object_name = field_access.object.data.Variable.lexeme;
                    if (helpers.isModuleNamespace(self, object_name)) {
                        if (std.mem.eql(u8, object_name, "g") or std.mem.eql(u8, object_name, "graphics")) {
                            if (field_access.object.data == .FieldAccess) {
                                const inner_fa = field_access.object.data.FieldAccess;
                                if (std.mem.eql(u8, inner_fa.field.lexeme, "doxa")) {
                                    if (std.mem.eql(u8, method_name, "Init")) {
                                        type_info.* = .{ .base = .Struct, .custom_type = "graphics.App" };
                                        return type_info;
                                    }
                                }
                            }
                        }
                        type_info.* = .{ .base = .Int };
                        return type_info;
                    }

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
                } else if (field_access.object.data == .FieldAccess) {
                    if (object_type.base == .Custom) {
                        if (object_type.custom_type) |ct_name| {
                            if (std.mem.indexOfScalar(u8, ct_name, '.')) |dot_idx| {
                                const root = ct_name[0..dot_idx];
                                if (helpers.isModuleNamespace(self, root)) {
                                    type_info.* = .{ .base = .Int };
                                    return type_info;
                                }
                            }
                        }
                    }
                }

                switch (object_type.base) {
                    .Array => {
                        if (std.mem.eql(u8, method_name, "push")) {
                            type_info.* = .{ .base = .Nothing };
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
                        var struct_name: ?[]const u8 = null;
                        if (object_type.base == .Custom) {
                            if (object_type.custom_type) |ct_name| {
                                if (std.mem.startsWith(u8, ct_name, "graphics.")) {
                                    if (std.mem.eql(u8, ct_name, "graphics.doxa")) {
                                        if (std.mem.eql(u8, method_name, "Init")) {
                                            type_info.* = .{ .base = .Struct, .custom_type = "graphics.App" };
                                            return type_info;
                                        }
                                    }
                                    type_info.* = .{ .base = .Int };
                                    return type_info;
                                }
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
                const callee_type = try infer_type.inferTypeFromExpr(self, function_call.callee);
                if (callee_type.base == .Function) {
                    if (callee_type.function_type) |func_type| {
                        const expected_arg_count: usize = func_type.params.len;
                        var provided_arg_count: usize = 0;
                        var has_placeholders: bool = false;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data != .DefaultArgPlaceholder) {
                                provided_arg_count += 1;
                            } else {
                                has_placeholders = true;
                            }
                        }

                        if (function_call.arguments.len > expected_arg_count) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_MANY_ARGUMENTS,
                                "Too many arguments: expected at most {}, got {}",
                                .{ expected_arg_count, function_call.arguments.len },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }

                        if (provided_arg_count < expected_arg_count and !has_placeholders) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_FEW_ARGUMENTS,
                                "Too few arguments: expected {}, got {} (use ~ to explicitly skip parameters)",
                                .{ expected_arg_count, provided_arg_count },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }

                        var param_index: usize = 0;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data == .DefaultArgPlaceholder) {
                                if (param_index < expected_arg_count) param_index += 1;
                                continue;
                            }
                            if (param_index >= expected_arg_count) break;

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
                    if (index.array.data == .Variable) {
                        const var_name = index.array.data.Variable.lexeme;
                        if (self.current_scope) |scope| {
                            if (scope.lookupVariable(var_name)) |variable| {
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
                if (index_type.base != .String and index_type.base != .Int and index_type.base != .Enum and index_type.base != .Custom) {
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

                type_info.* = .{ .base = .String };
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

            var resolved_object_type = object_type;
            if (object_type.base == .Custom) {
                if (object_type.custom_type) |custom_type_name| {
                    if (helpers.isModuleNamespace(self, custom_type_name)) {
                        return helpers.handleModuleFieldAccess(self, custom_type_name, field.field.lexeme, .{ .location = getLocationFromBase(expr.base) });
                    }

                    if (self.custom_types.get(custom_type_name)) |custom_type| {
                        if (custom_type.kind == .Struct) {
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
                            type_info.* = .{ .base = .Custom, .custom_type = custom_type_name };
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
                    type_info.* = .{ .base = .Enum, .custom_type = null };
                    return type_info;
                }
            }

            if (resolved_object_type.base == .Struct) {
                if (resolved_object_type.struct_fields) |fields| {
                    for (fields) |struct_field| {
                        if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                            type_info.* = struct_field.type_info.*;

                            if (type_info.base == .Custom and type_info.custom_type != null) {
                                if (self.custom_types.get(type_info.custom_type.?)) |custom_type| {
                                    if (custom_type.kind == .Struct) {
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

                            if (resolved_object_type.custom_type) |owner_name| {
                                if (self.custom_types.get(owner_name)) |owner_ct| {
                                    if (owner_ct.kind == .Struct and owner_ct.struct_fields != null) {
                                        const owner_fields = owner_ct.struct_fields.?;
                                        var is_public_field = false;
                                        for (owner_fields) |ofld| {
                                            if (std.mem.eql(u8, ofld.name, struct_field.name)) {
                                                is_public_field = ofld.is_public;
                                                break;
                                            }
                                        }
                                        if (!is_public_field) {
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

                if ((then_is_peek and else_type.base == .Nothing) or (else_is_peek and then_type.base == .Nothing)) {
                    type_info.* = if (then_is_peek) then_type.* else else_type.*;
                } else if (then_is_peek and else_is_peek) {
                    type_info.* = then_type.*;
                } else {
                    if (then_type.base == .Nothing and else_type.base != .Nothing) {
                        type_info.* = else_type.*;
                    } else if (else_type.base == .Nothing and then_type.base != .Nothing) {
                        type_info.* = then_type.*;
                    } else if (then_type.base != else_type.base) {
                        var members = [_]*ast.TypeInfo{ then_type, else_type };
                        const u = try helpers.createUnionType(self, members[0..]);
                        type_info.* = u.*;
                    } else {
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
        .Match => |match_expr| {
            _ = try infer_type.inferTypeFromExpr(self, match_expr.value);

            if (match_expr.cases.len > 0) {
                var matched_var_name: ?[]const u8 = null;
                if (match_expr.value.data == .Variable) {
                    matched_var_name = match_expr.value.data.Variable.lexeme;
                }

                var union_types = std.array_list.Managed(*ast.TypeInfo).init(self.allocator);
                defer union_types.deinit();

                for (match_expr.cases) |case| {
                    const case_type = try self.inferMatchCaseTypeWithNarrow(case, matched_var_name);
                    try union_types.append(case_type);
                }

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
                if (lookupVariable(self, assign.name.lexeme)) |variable| {
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
            type_info.* = .{ .base = .Nothing };
        },
        .CompoundAssign => |compound_assign| {
            if (compound_assign.value) |value| {
                const value_type = try infer_type.inferTypeFromExpr(self, value);
                if (lookupVariable(self, compound_assign.name.lexeme)) |variable| {
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
            type_info.* = .{ .base = .Nothing };
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

            type_info.* = .{ .base = .Nothing };

            const requireArity = struct {
                fn check(sem: *SemanticAnalyzer, e: *ast.Expr, got: usize, expect: usize, name: []const u8) void {
                    if (got == expect) return;
                    if (got < expect) {
                        sem.reporter.reportCompileError(
                            getLocationFromBase(e.base),
                            ErrorCode.TOO_FEW_ARGUMENTS,
                            "Too few arguments to @{s}: expected {d}, got {d}",
                            .{ name, expect, got },
                        );
                    } else {
                        sem.reporter.reportCompileError(
                            getLocationFromBase(e.base),
                            ErrorCode.TOO_MANY_ARGUMENTS,
                            "Too many arguments to @{s}: expected {d}, got {d}",
                            .{ name, expect, got },
                        );
                    }
                    sem.fatal_error = true;
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
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "float")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                type_info.* = .{ .base = .Float };
                return type_info;
            } else if (std.mem.eql(u8, fname, "byte")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                type_info.* = .{ .base = .Byte };
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
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Int and arg_type.base != .Byte) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.TYPE_MISMATCH, "@sleep: argument must be an integer", .{});
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Nothing };
                return type_info;
            } else if (std.mem.eql(u8, fname, "spawn")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Array) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(bc.arguments[0].base),
                        ErrorCode.TYPE_MISMATCH,
                        "@spawn: argument must be array of strings",
                        .{},
                    );
                    self.fatal_error = true;
                } else if (arg_type.array_type) |elem_type| {
                    if (elem_type.base != .String and elem_type.base != .Nothing) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(bc.arguments[0].base),
                            ErrorCode.TYPE_MISMATCH,
                            "@spawn: array elements must be strings, got {s}",
                            .{@tagName(elem_type.base)},
                        );
                        self.fatal_error = true;
                    }
                }
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "kill")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(bc.arguments[0].base),
                        ErrorCode.TYPE_MISMATCH,
                        "@kill: argument must be process id (int)",
                        .{},
                    );
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Nothing };
                return type_info;
            } else if (std.mem.eql(u8, fname, "wait")) {
                requireArity.check(self, expr, bc.arguments.len, 1, fname);
                if (bc.arguments.len != 1) return type_info;
                const arg_type = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                if (arg_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(bc.arguments[0].base),
                        ErrorCode.TYPE_MISMATCH,
                        "@wait: argument must be process id (int)",
                        .{},
                    );
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "random")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .Float };
                return type_info;
            } else if (std.mem.eql(u8, fname, "dice_roll")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "tick")) {
                requireArity.check(self, expr, bc.arguments.len, 0, fname);
                if (bc.arguments.len != 0) return type_info;
                type_info.* = .{ .base = .Int };
                return type_info;
            } else if (std.mem.eql(u8, fname, "build")) {
                // @build(source_path: string, output_path: string, arch: string, os: string, debug: tetra) -> int
                requireArity.check(self, expr, bc.arguments.len, 5, fname);
                if (bc.arguments.len != 5) return type_info;
                const src_t = try infer_type.inferTypeFromExpr(self, bc.arguments[0]);
                const out_t = try infer_type.inferTypeFromExpr(self, bc.arguments[1]);
                const arch_t = try infer_type.inferTypeFromExpr(self, bc.arguments[2]);
                const os_t = try infer_type.inferTypeFromExpr(self, bc.arguments[3]);
                const dbg_t = try infer_type.inferTypeFromExpr(self, bc.arguments[4]);

                if (src_t.base != .String or out_t.base != .String) {
                    self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.TYPE_MISMATCH, "@build expects first two arguments to be strings (source, output)", .{});
                    self.fatal_error = true;
                }
                if (arch_t.base != .String) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[2].base), ErrorCode.TYPE_MISMATCH, "@build arch must be string", .{});
                    self.fatal_error = true;
                }
                if (os_t.base != .String) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[3].base), ErrorCode.TYPE_MISMATCH, "@build os must be string", .{});
                    self.fatal_error = true;
                }
                if (dbg_t.base != .Tetra) {
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[4].base), ErrorCode.TYPE_MISMATCH, "@build debug must be tetra", .{});
                    self.fatal_error = true;
                }
                type_info.* = .{ .base = .Int };
                return type_info;
            }

            self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.NOT_IMPLEMENTED, "Unknown builtin '@{s}'", .{fname});
            self.fatal_error = true;
            return type_info;
        },
        .InternalCall => |method_call| {
            var receiver_type = try infer_type.inferTypeFromExpr(self, method_call.receiver);
            const method_name = method_call.method.lexeme;

            switch (method_call.method.type) {
                .BUILD => {
                    const argc = method_call.arguments.len;
                    if (argc != 5) {
                        if (argc < 5) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_FEW_ARGUMENTS,
                                "Too few arguments to @build: expected 5, got {d}",
                                .{argc},
                            );
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_MANY_ARGUMENTS,
                                "Too many arguments to @build: expected 5, got {d}",
                                .{argc},
                            );
                        }
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // Basic type validation: src/out/arch/os strings; debug tetra
                    const arch_expr = method_call.arguments[2];
                    const os_expr = method_call.arguments[3];
                    const debug_expr = method_call.arguments[4];

                    const arch_t = try infer_type.inferTypeFromExpr(self, arch_expr);
                    const os_t = try infer_type.inferTypeFromExpr(self, os_expr);
                    const dbg_t = try infer_type.inferTypeFromExpr(self, debug_expr);

                    if (arch_t.base != .String) {
                        self.reporter.reportCompileError(getLocationFromBase(arch_expr.base), ErrorCode.TYPE_MISMATCH, "@build arch must be string", .{});
                    }
                    if (os_t.base != .String) {
                        self.reporter.reportCompileError(getLocationFromBase(os_expr.base), ErrorCode.TYPE_MISMATCH, "@build os must be string", .{});
                    }
                    if (dbg_t.base != .Tetra) {
                        self.reporter.reportCompileError(getLocationFromBase(debug_expr.base), ErrorCode.TYPE_MISMATCH, "@build debug must be tetra", .{});
                    }

                    // Return int status code
                    type_info.base = .Int;
                    return type_info;
                },
                .PUSH,
                .POP,
                .INSERT,
                .REMOVE,
                .SLICE,
                => {
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
                                if (receiver_type.array_type) |elem_type| {
                                    try helpers.unifyTypes(self, elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[0].base) });
                                }
                            } else if (receiver_type.base == .String) {
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

                            const value_type = try infer_type.inferTypeFromExpr(self, method_call.arguments[1]);
                            if (receiver_type.array_type) |elem_type| {
                                try helpers.unifyTypes(self, elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[1].base) });
                            }

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

                            if (receiver_type.base == .Array) {
                                type_info.* = receiver_type.*;
                            } else if (receiver_type.base == .String) {
                                type_info.* = .{ .base = .String };
                            }

                            var args = try self.allocator.alloc(*ast.Expr, 3);
                            args[0] = method_call.receiver;
                            args[1] = method_call.arguments[0];
                            args[2] = method_call.arguments[1];
                            expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                            return try infer_type.inferTypeFromExpr(self, expr);
                        },

                        else => {
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

                .TYPE => {
                    var args = try self.allocator.alloc(*ast.Expr, 1);
                    args[0] = method_call.receiver;
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

                .LENGTH => {
                    var args = try self.allocator.alloc(*ast.Expr, 1);
                    args[0] = method_call.receiver;
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

                .TOSTRING, .TOINT, .TOFLOAT, .TOBYTE => {
                    switch (method_call.method.type) {
                        .TOSTRING => {
                            type_info.* = .{ .base = .String };
                        },
                        .TOINT, .TOFLOAT, .TOBYTE => {
                            // Intrinsics are non-union: infer concrete return types
                            switch (method_call.method.type) {
                                .TOINT => type_info.* = .{ .base = .Int },
                                .TOFLOAT => type_info.* = .{ .base = .Float },
                                .TOBYTE => type_info.* = .{ .base = .Byte },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }
                },

                .READ,
                .WRITE,
                => {
                    switch (method_call.method.type) {
                        .READ => {
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
                            type_info.* = .{ .base = .Nothing };
                        },

                        else => unreachable,
                    }
                },

                .OS, .ARCH, .TIME, .TICK => {
                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = &[_]*ast.Expr{} } };
                    return try infer_type.inferTypeFromExpr(self, expr);
                },

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
            type_info.* = .{ .base = .Custom, .custom_type = null };
        },
        .DefaultArgPlaceholder => {
            type_info.* = .{ .base = .Nothing };
        },
        .Input => {
            type_info.* = .{ .base = .String };
        },
        .Peek => |_peek| {
            const expr_type = try infer_type.inferTypeFromExpr(self, _peek.expr);
            type_info.* = expr_type.*;
        },
        .PeekStruct => |_peek_struct| {
            const expr_type = try infer_type.inferTypeFromExpr(self, _peek_struct.expr);
            type_info.* = expr_type.*;
        },
        .Print => {
            type_info.* = .{ .base = .Nothing };
        },
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
            } else if (array_type.base == .Map and index_type.base != .Int and index_type.base != .String and index_type.base != .Enum and index_type.base != .Custom) {
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

            type_info.* = .{ .base = .Nothing };
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

            type_info.* = .{ .base = .Nothing };
        },
        .Exists => |exists| {
            const array_type = try infer_type.inferTypeFromExpr(self, exists.array);

            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            var bound_var_type = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int };

            _ = quantifier_scope.createValueBinding(
                exists.variable.lexeme,
                TokenLiteral{ .nothing = {} },
                helpers.convertTypeToTokenType(self, bound_var_type.base),
                &bound_var_type,
                true,
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

            const prev_scope = self.current_scope;
            self.current_scope = quantifier_scope;

            const condition_type = try infer_type.inferTypeFromExpr(self, exists.condition);

            self.current_scope = prev_scope;

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

            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            var bound_var_type = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int };

            _ = quantifier_scope.createValueBinding(
                for_all.variable.lexeme,
                TokenLiteral{ .nothing = {} },
                helpers.convertTypeToTokenType(self, bound_var_type.base),
                &bound_var_type,
                true,
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

            const prev_scope = self.current_scope;
            self.current_scope = quantifier_scope;

            const condition_type = try infer_type.inferTypeFromExpr(self, for_all.condition);

            self.current_scope = prev_scope;

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

            type_info.* = .{ .base = .Nothing };
        },
        .StructDecl => {
            type_info.* = .{ .base = .Struct };
        },
        .StructLiteral => |struct_lit| {
            if (lookupVariable(self, struct_lit.name.lexeme)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    if (storage.type_info.base == .Custom) {
                        type_info.* = storage.type_info.*;

                        if (type_info.struct_fields) |decl_fields| {
                            if (decl_fields.len != struct_lit.fields.len) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(expr.base),
                                    ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                                    "Struct field count mismatch: expected {}, got {}",
                                    .{ decl_fields.len, struct_lit.fields.len },
                                );
                                self.fatal_error = true;
                            }

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

            var value_type_ptr: ?*ast.TypeInfo = null;
            if (block.value) |value_expr| {
                value_type_ptr = try infer_type.inferTypeFromExpr(self, value_expr);
            }

            if (self.in_loop_scope) {
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
            const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
            type_info.* = type_info_ptr.*;
            self.allocator.destroy(type_info_ptr);
        },
        .Cast => |cast| {
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
            const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
            type_info.* = target_type_info.*;

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

            var then_type: ?*ast.TypeInfo = null;
            if (cast.then_branch) |then_expr| {
                const then_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                defer then_scope.deinit();

                const prev_scope = self.current_scope;
                self.current_scope = then_scope;
                defer self.current_scope = prev_scope;

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
                    if (field_access.object.data == .Variable) {
                        const obj_name = field_access.object.data.Variable.lexeme;
                        const fld_name = field_access.field.lexeme;
                        if (lookupVariable(self, obj_name)) |variable| {
                            if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                const original_type = storage.type_info.*;
                                var struct_fields: ?[]ast.StructFieldType = null;
                                if (original_type.base == .Struct and original_type.struct_fields != null) {
                                    struct_fields = original_type.struct_fields.?;
                                } else if (original_type.base == .Custom and original_type.custom_type != null) {
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
                then_type = try infer_type.inferTypeFromExpr(self, then_expr);
            }

            var else_type: ?*ast.TypeInfo = null;
            if (cast.else_branch) |else_expr| {
                else_type = try infer_type.inferTypeFromExpr(self, else_expr);
                if (else_expr.data == .ReturnExpr) {
                    const nothing_type = try self.allocator.create(ast.TypeInfo);
                    nothing_type.* = .{ .base = .Nothing };
                    else_type = nothing_type;
                }
            }

            type_info.* = target_type_info.*;

            if (then_type) |tt| {
                if (else_type) |et| {
                    if (tt.base == et.base) {
                        type_info.* = tt.*;
                    } else {
                        var types = [_]*ast.TypeInfo{ tt, et };
                        const union_type = try helpers.createUnionType(self, &types);
                        type_info.* = union_type.*;
                    }
                } else {
                    type_info.* = tt.*;
                }
            } else if (else_type) |et| {
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
                type_info.* = target_type_info.*;
            }
        },
        .Loop => |loop| {
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

            if (loop.var_decl) |vd| {
                var single: [1]ast.Stmt = .{vd.*};
                try self.validateStatements(single[0..]);
            }

            if (loop.condition) |cond| {
                _ = try infer_type.inferTypeFromExpr(self, cond);
            }

            // Use current_scope as parent to ensure function declarations are accessible
            const iteration_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            defer iteration_scope.deinit();

            if (loop.var_decl) |vd| {
                const saved_scope = self.current_scope;
                self.current_scope = iteration_scope;

                var single: [1]ast.Stmt = .{vd.*};
                try self.validateStatements(single[0..]);

                self.current_scope = saved_scope;
            }

            {
                const saved_scope = self.current_scope;
                self.current_scope = iteration_scope;
                defer self.current_scope = saved_scope;

                _ = try infer_type.inferTypeFromExpr(self, loop.body);
            }

            if (loop.step) |stp| {
                _ = try infer_type.inferTypeFromExpr(self, stp);
            }

            type_info.* = .{ .base = .Nothing };
        },
        .This => {
            // TODO: Implement proper 'this' type resolution based on the current method context
            type_info.* = .{ .base = .Struct };
        },
        .Range => |range| {
            const start_type = try inferTypeFromExpr(self, range.start);
            const end_type = try inferTypeFromExpr(self, range.end);

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

            const element_type = try self.allocator.create(ast.TypeInfo);
            element_type.* = .{ .base = .Int };
            type_info.* = .{ .base = .Array, .array_type = element_type };
        },
    }

    try self.type_cache.put(expr.base.id, type_info);
    return type_info;
}
