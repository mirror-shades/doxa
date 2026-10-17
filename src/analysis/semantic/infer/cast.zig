//! The `as` narrowing rule and the view bindings it creates.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("../eval_utils.zig");
const names = @import("../names.zig");
const Scope = @import("../../../utils/memory.zig").Scope;
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;
const fields = @import("fields.zig");

const GroupCast = enum { not_a_group, valid, not_a_member };

pub fn castExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const cast = expr.data.Cast;
    if (cast.else_branch == null) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.EXPECTED_CLOSING_BRACE,
            "'as' requires an 'else' block",
            .{},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }
    const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
    type_info.* = target_type_info.*;
    expr.data.Cast.target = target_type_info;

    const value_type = try infer_type.inferTypeFromExpr(self, cast.value);
    const group_cast = classifyGroupCast(self, value_type, target_type_info);
    if (value_type.base != .Union and group_cast != .valid) {
        if (group_cast == .not_a_member) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.INVALID_OPERAND_TYPE,
                "'{s}' is not a member of group '{s}'",
                try helpers.typeLabels(self, target_type_info, value_type),
            );
        } else {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.INVALID_OPERAND_TYPE,
                "Type casting 'as' can only be used with union types, got {s}",
                .{@tagName(value_type.base)},
            );
        }
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    // When this cast is the initializer of a declaration, the declared
    // name (carried on the cast node) is narrowed to the target type in
    // the then branch and to the remainder in the else branch.
    const cast_decl_name = cast.decl_name;

    // TODO: warn on discarded non-Nothing values in if/as blocks without lift
    // TODO: warn on unused variable when both branches diverge via return
    var then_type: ?*ast.TypeInfo = null;
    if (cast.then_branch) |then_expr| {
        const then_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer then_scope.deinit();

        const prev_scope = self.current_scope;
        self.current_scope = then_scope;
        defer self.current_scope = prev_scope;

        try bindNarrowedCastType(self, then_scope, cast.value, target_type_info);
        if (cast_decl_name) |name| expr.data.Cast.decl_then = try bindCastDeclName(then_scope, name, target_type_info);

        if (then_expr.data == .Block and then_expr.data.Block.value != null) {
            self.reporter.reportWarning(
                getLocationFromBase(expr.base),
                ErrorCode.CONDITION_ALWAYS_TRUE,
                "lift in 'as' then block has no effect; value comes from the narrowed original",
                .{},
            );
        }

        const inferred_then = try infer_type.inferTypeFromExpr(self, then_expr);
        if (then_expr.data != .Block) {
            then_type = inferred_then;
        }

        if (cast_decl_name) |name| then_scope.propagateUsedToParent(name);
        if (castValueBindingName(cast.value)) |name| then_scope.propagateUsedToParent(name);
    }

    var else_diverges = false;
    var else_type: ?*ast.TypeInfo = null;
    if (cast.else_branch) |else_expr| {
        const else_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer else_scope.deinit();

        const prev_else_scope = self.current_scope;
        self.current_scope = else_scope;
        defer self.current_scope = prev_else_scope;

        const remainder_type = try helpers.subtractTypeFromUnion(self, value_type, target_type_info);
        try bindNarrowedCastType(self, else_scope, cast.value, remainder_type);
        if (cast_decl_name) |name| expr.data.Cast.decl_else = try bindCastDeclName(else_scope, name, remainder_type);

        else_type = try infer_type.inferTypeFromExpr(self, else_expr);
        if (expressionDiverges(else_expr)) {
            else_diverges = true;
            const nothing_type = try ast.TypeInfo.createDefault(self.allocator);
            nothing_type.* = .{ .base = .Nothing };
            else_type = nothing_type;
        }

        if (cast_decl_name) |name| else_scope.propagateUsedToParent(name);
        if (castValueBindingName(cast.value)) |name| else_scope.propagateUsedToParent(name);
    }

    if (else_type) |et| {
        const label = struct {
            fn run(t: *ast.TypeInfo) []const u8 {
                if (t.custom_type) |ct| return ct.displayName();
                return switch (t.base) {
                    .Int => "int",
                    .Byte => "byte",
                    .Float => "float",
                    .String => "string",
                    .Tetra => "tetra",
                    .Nothing => "nothing",
                    else => @tagName(t.base),
                };
            }
        }.run;

        const fallback_matches_target = !self.block_value_expected or (et.base == .Nothing and else_diverges) or helpers.typesEqual(self, target_type_info, et);
        if (!fallback_matches_target) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.TYPE_MISMATCH,
                "Fallback for 'as' must produce type {s}, got {s}",
                .{ label(target_type_info), label(et) },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
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
    }
}

pub fn classifyGroupCast(self: *SemanticAnalyzer, value_type: *ast.TypeInfo, target_type: *ast.TypeInfo) GroupCast {
    const group = helpers.matchSubjectGroup(self, value_type) orelse return .not_a_group;
    if (target_type.base != .Custom) return .not_a_member;
    const target = (target_type.custom_type orelse return .not_a_member).resolved();
    const group_id = self.group_table.idOf(group) orelse return .not_a_member;
    for (self.group_table.members(group_id) orelse &.{}) |member| {
        if (member.ref.eql(target)) return .valid;
    }
    return .not_a_member;
}

/// Returns the variable name that `bindNarrowedCastType` would shadow for the
/// given cast value, so usage of that narrowed binding can be propagated to the
/// original variable when the narrowing scope is torn down.
pub fn castValueBindingName(cast_value: *ast.Expr) ?[]const u8 {
    if (cast_value.data == .Variable) return cast_value.data.Variable.lexeme;
    if (cast_value.data == .FieldAccess) {
        const field_access = cast_value.data.FieldAccess;
        if (field_access.object.data == .Variable) return field_access.object.data.Variable.lexeme;
    }
    return null;
}

pub fn bindNarrowedName(scope: *Scope, name: []const u8, narrowed_type: *ast.TypeInfo) !void {
    const token_type = eval.convertTypeToTokenType(narrowed_type.base);
    const view = scope.createValueBinding(
        name,
        token_type,
        narrowed_type,
        false,
    ) catch return;
    view.is_view = true;
}

/// Bind the name a cast declares inside one of its branches. The declaration
/// has no value until the cast resolves, so inside a branch the name is its own
/// binding, holding the subject narrowed to that branch's type.
pub fn bindCastDeclName(scope: *Scope, name: []const u8, narrowed_type: *ast.TypeInfo) !?ast.CastBinding {
    const binding = scope.createValueBinding(
        name,
        eval.convertTypeToTokenType(narrowed_type.base),
        narrowed_type,
        false,
    ) catch |err| switch (err) {
        error.DuplicateVariableName => return null,
        else => |e| return e,
    };
    return .{ .storage = binding.storage_id, .type_info = narrowed_type };
}

pub fn bindNarrowedCastType(self: *SemanticAnalyzer, scope: *Scope, cast_value: *ast.Expr, narrowed_type: *ast.TypeInfo) !void {
    if (cast_value.data == .Variable) {
        try bindNarrowedName(scope, cast_value.data.Variable.lexeme, narrowed_type);
    } else if (cast_value.data == .FieldAccess) {
        const field_access = cast_value.data.FieldAccess;
        if (field_access.object.data == .Variable) {
            const obj_name = field_access.object.data.Variable.lexeme;
            const fld_name = field_access.field.lexeme;
            if (try names.lookupVariable(self, obj_name)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    const original_type = storage.type_info.*;
                    var struct_fields: ?[]const ast.StructFieldType = null;
                    if (original_type.base == .Struct and original_type.struct_fields != null) {
                        struct_fields = original_type.struct_fields.?;
                    } else if (original_type.base == .Custom and original_type.custom_type != null) {
                        if (try self.customType(original_type.custom_type.?.resolved())) |custom_type| {
                            if (custom_type.kind == .Struct) struct_fields = try fields.structFieldTypes(self, custom_type.struct_fields orelse &.{});
                        }
                    }
                    if (struct_fields) |fields_arr| {
                        const dup_fields = try self.allocator.alloc(ast.StructFieldType, fields_arr.len);
                        for (fields_arr, 0..) |sf, i| {
                            if (std.mem.eql(u8, sf.name, fld_name)) {
                                dup_fields[i] = .{ .name = sf.name, .type_info = narrowed_type };
                            } else {
                                dup_fields[i] = sf;
                            }
                        }
                        const narrowed_struct = try ast.TypeInfo.createDefault(self.allocator);
                        narrowed_struct.* = .{ .base = .Struct, .struct_fields = dup_fields };
                        const view = scope.createValueBinding(
                            obj_name,
                            .STRUCT,
                            narrowed_struct,
                            false,
                        ) catch return;
                        view.is_view = true;
                    }
                }
            }
        }
    }
}

/// Whether an expression unconditionally transfers control away and therefore
/// never produces a value (an explicit return, `@panic`, `@exit`, or
/// unreachable). Used to accept diverging `as`/`if` fallback branches.
pub fn expressionDiverges(expr: *ast.Expr) bool {
    return switch (expr.data) {
        .ReturnExpr, .Unreachable => true,
        .InternalCall => |ic| ic.method.type == .PANIC or ic.method.type == .EXIT,
        .Block => |block| blockDiverges(block.statements),
        else => false,
    };
}

pub fn blockDiverges(statements: []ast.Stmt) bool {
    if (statements.len == 0) return false;
    return statementDiverges(statements[statements.len - 1]);
}

pub fn statementDiverges(stmt: ast.Stmt) bool {
    return switch (stmt.data) {
        .Return, .Break, .Continue => true,
        .Expression => |maybe_expr| if (maybe_expr) |e| expressionDiverges(e) else false,
        else => false,
    };
}
