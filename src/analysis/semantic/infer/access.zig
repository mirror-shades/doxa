//! Type rules for names, indexing, and stores into a name or an element.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const borrows = @import("../borrows.zig");
const effects = @import("../effects.zig");
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;

pub fn variableExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const var_token = expr.data.Variable;
    if (try names.resolveVariableExpr(self, expr)) |variable| {
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
    } else if (try names.namespaceOf(self, expr)) |_| {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.MODULE_NAMESPACE_NOT_A_VALUE,
            "Module namespace '{s}' is not a value",
            .{var_token.lexeme},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
    } else if (self.current_initializing_var != null and std.mem.eql(u8, self.current_initializing_var.?, var_token.lexeme)) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.SELF_REFERENTIAL_INITIALIZER,
            "Variable '{s}' cannot reference itself in its own initializer",
            .{var_token.lexeme},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
    } else if (names.suggestName(self, var_token.lexeme)) |suggested| {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.UNDEFINED_VARIABLE,
            "Undefined variable: '{s}'. Did you mean '{s}'?",
            .{ var_token.lexeme, suggested },
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
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
}

pub fn indexExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const index = expr.data.Index;
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
            return;
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
                                    return;
                                } else {}
                            }
                        } else {}
                    } else {}
                } else {}
            } else if (index.array.data == .FieldAccess) {
                // Handle field access like this.FileToContext[file].line_breaks
                const field_access = index.array.data.FieldAccess;
                const object_type = try infer_type.inferTypeFromExpr(self, field_access.object);

                if (object_type.base == .Struct or object_type.base == .Custom) {
                    if (object_type.struct_fields) |fields| {
                        for (fields) |field| {
                            if (std.mem.eql(u8, field.name, field_access.field.lexeme)) {
                                if (field.type_info.base == .Array) {
                                    if (field.type_info.array_type) |elem_type| {
                                        type_info.* = elem_type.*;
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // If we can't determine the element type, default to Nothing
            type_info.base = .Nothing;
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
            return;
        }

        type_info.* = .{ .base = .String };
    } else {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.CANNOT_INDEX_TYPE,
            "Cannot index non-array/string type {s}",
            .{@tagName(array_type.base)},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }
}

pub fn assignment(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const assign = &expr.data.Assignment;
    if (assign.value) |value| {
        const prev_bve = self.block_value_expected;
        self.block_value_expected = true;
        defer self.block_value_expected = prev_bve;
        // The target is resolved first: its storage's type is the
        // context the value is typed in.
        try borrows.checkPath(self, &.{assign.name.lexeme}, expr, .write);
        const assigned = try names.resolveAssignmentTarget(self, expr, &assign.name);
        const target_storage = if (assigned) |variable| self.memory.scope_manager.value_storage.get(variable.storage_id) else null;
        const value_type = if (target_storage) |storage|
            try infer_type.inferTypeIn(self, value, storage.type_info)
        else
            try infer_type.inferTypeFromExpr(self, value);
        if (assigned) |variable| {
            // A store into a union `^` parameter keeps its member only
            // through a narrowing of it to one member: un-narrowed, or
            // narrowed to a smaller union, it may store another.
            if (self.getStoreTarget(expr.base.id)) |target| {
                if (self.union_alias_params.get(target.storage)) |param| {
                    if (!variable.is_view or target.read.base == .Union) try self.member_mutators.put(param, {});
                }
            }
            if (target_storage) |storage| {
                if (storage.constant) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ASSIGNMENT_TARGET,
                        "Cannot assign to immutable variable '{s}'",
                        .{assign.name.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return;
                }
                try helpers.unifyTypesExpr(self, storage.type_info, value_type, value, .{ .location = getLocationFromBase(expr.base) });
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
            return;
        }
    }
    type_info.* = .{ .base = .Nothing };
}

pub fn indexAssign(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const index_assign = expr.data.IndexAssign;
    try borrows.check(self, index_assign.array, expr, .write);
    const array_type = try infer_type.inferTypeFromExpr(self, index_assign.array);
    try effects.write(self, index_assign.array);
    const index_type = try infer_type.inferTypeFromExpr(self, index_assign.index);
    const value_type = if (array_type.base == .Array and array_type.array_type != null)
        try infer_type.inferTypeIn(self, index_assign.value, array_type.array_type.?)
    else
        try infer_type.inferTypeFromExpr(self, index_assign.value);

    if (array_type.base != .Array) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.CANNOT_INDEX_TYPE,
            "Cannot assign to index of non-array type {s}",
            .{@tagName(array_type.base)},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    if (index_type.base != .Int) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.INVALID_ARRAY_INDEX_TYPE,
            "Array index must be integer, got {s}",
            .{@tagName(index_type.base)},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    if (array_type.array_type) |elem_type| {
        try helpers.unifyElement(self, elem_type, value_type, index_assign.value, .{ .location = getLocationFromBase(expr.base) });
    }

    type_info.* = .{ .base = .Nothing };
}
