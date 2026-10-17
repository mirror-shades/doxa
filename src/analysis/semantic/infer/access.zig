//! Type rules for names, indexing, and stores into a name or an element.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const Memory = @import("../../../utils/memory.zig");
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
    if (array_type.base == .Error or index_type.base == .Error) return;

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
            // An array nothing gave an element type has no element to read.
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.CANNOT_INFER_ARRAY_ELEMENT_TYPE,
                "cannot infer element type of empty array literal; add an element type annotation (e.g. `int[]`)",
                .{},
            );
            self.fatal_error = true;
            type_info.* = .{ .base = .Error };
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
    type_info.* = .{ .base = .Nothing };
    const value = assign.value orelse return;
    const prev_bve = self.block_value_expected;
    self.block_value_expected = true;
    defer self.block_value_expected = prev_bve;
    // The target is resolved first: its storage's type is the
    // context the value is typed in.
    try borrows.checkPath(self, &.{assign.name.lexeme}, expr, .write);
    const variable = try names.resolveAssignmentTarget(self, expr, &assign.name) orelse {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.VARIABLE_NOT_FOUND,
            "Undefined variable '{s}'",
            .{assign.name.lexeme},
        );
        self.fatal_error = true;
        return;
    };
    const storage = self.memory.scope_manager.value_storage.get(variable.storage_id);
    const value_type = if (storage) |held_storage|
        try infer_type.inferTypeIn(self, value, held_storage.type_info)
    else
        try infer_type.inferTypeFromExpr(self, value);
    try checkNameStore(self, expr, assign.name, variable, value_type, value);
}

/// Check a store of `value` (of `value_type`) into the name `variable`
/// through `store`, the node its store target is keyed by.
fn checkNameStore(self: *SemanticAnalyzer, store: *const ast.Expr, name: ast.Token, variable: *const Memory.Variable, value_type: *ast.TypeInfo, value: *ast.Expr) SemanticError!void {
    // A store into a union `^` parameter keeps its member only
    // through a narrowing of it to one member: un-narrowed, or
    // narrowed to a smaller union, it may store another.
    if (self.getStoreTarget(store.base.id)) |target| {
        if (self.union_alias_params.get(target.storage)) |param| {
            if (!variable.is_view or target.read.base == .Union) try self.member_mutators.put(param, {});
        }
    }
    const storage = self.memory.scope_manager.value_storage.get(variable.storage_id) orelse return;
    if (storage.constant) {
        self.reporter.reportCompileError(
            getLocationFromBase(store.base),
            ErrorCode.INVALID_ASSIGNMENT_TARGET,
            "Cannot assign to immutable variable '{s}'",
            .{name.lexeme},
        );
        self.fatal_error = true;
        return;
    }
    try helpers.unifyTypesExpr(self, storage.type_info, value_type, value, .{ .location = getLocationFromBase(store.base) });
}

/// `target op= value`: the target is analyzed once, as the read its
/// operation's `Held` stands for. The operation is typed in the target's
/// type and checked as a store into the target.
pub fn compoundAssign(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const compound = expr.data.CompoundAssign;
    const target = compound.target;
    type_info.* = .{ .base = .Nothing };
    const prev_bve = self.block_value_expected;
    self.block_value_expected = true;
    defer self.block_value_expected = prev_bve;
    // The write is checked before the read, as a plain store checks it.
    switch (target.data) {
        .Variable => |name| try borrows.checkPath(self, &.{name.lexeme}, expr, .write),
        .Index => |idx| try borrows.check(self, idx.array, expr, .write),
        .FieldAccess => |fa| try borrows.check(self, fa.object, expr, .write),
        else => unreachable, // the parser admits only these targets
    }
    const current = try infer_type.inferTypeFromExpr(self, target);
    try self.held.put(expr.base.id, current);
    const value_type = try infer_type.inferTypeIn(self, compound.operation, current);
    const location: ast.SourceSpan = .{ .location = getLocationFromBase(expr.base) };
    switch (target.data) {
        .Variable => |name| {
            // An unresolved name is already reported by its read.
            const variable = (try names.resolveName(self, name.lexeme)).variable orelse return;
            try checkNameStore(self, target, name, variable, value_type, compound.operation);
        },
        .Index => |idx| {
            try effects.write(self, idx.array);
            const array_type = try infer_type.inferTypeFromExpr(self, idx.array);
            if (array_type.base != .Array) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_INDEX_TYPE,
                    "Cannot assign to index of non-array type {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                return;
            }
            if (array_type.array_type) |element| try helpers.unifyElement(self, element, value_type, compound.operation, location);
        },
        .FieldAccess => |fa| {
            try effects.write(self, fa.object);
            try helpers.unifyTypesExpr(self, current, value_type, compound.operation, location);
        },
        else => unreachable,
    }
}

/// A `Held` read: the value its owner holds, of the type the owner
/// recorded when it analyzed it.
pub fn heldExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    // An owner records its value before it analyzes anything that reads it.
    type_info.* = self.held.get(expr.data.Held).?.*;
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
