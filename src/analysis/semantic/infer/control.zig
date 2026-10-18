//! Type rules for control-flow expressions: if, match, loops, quantifiers, blocks, and returns.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("../eval_utils.zig");
const borrows = @import("../borrows.zig");
const effects = @import("../effects.zig");
const TypeRef = ast.TypeRef;
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;
const calls = @import("calls.zig");
const patterns = @import("../patterns.zig");

pub fn ifExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo, discarded: bool) SemanticError!void {
    const if_expr = expr.data.If;
    var condition_type = try infer_type.inferTypeFromExpr(self, if_expr.condition.?);

    // If condition is a function, automatically infer its return type as if it were called
    if (condition_type.base == .Function) {
        if (condition_type.function_type) |func_type| {
            condition_type = func_type.return_type;
        } else {
            condition_type = try ast.TypeInfo.createDefault(self.allocator);
            condition_type.* = .{ .base = .Nothing, .is_mutable = false };
        }
    }

    if (condition_type.base != .Tetra) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.INVALID_CONDITION_TYPE,
            "Condition must be tetra, got {s}",
            .{@tagName(condition_type.base)},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    if (if_expr.condition.?.data == .Literal) {
        const literal = if_expr.condition.?.data.Literal;
        const tag = @tagName(literal.tetra);
        self.reporter.reportWarning(
            getLocationFromBase(if_expr.condition.?.base),
            ErrorCode.CONDITION_ALWAYS_TRUE,
            "condition is always {s}",
            .{tag},
        );
    }

    // A discarded `if` discards its branches' values too.
    self.discarded_grant = discarded;
    const then_type = try infer_type.inferTypeFromExpr(self, if_expr.then_branch.?);
    if (if_expr.implicit_else) {
        // Without an `else` there is no value on the false path: legal only
        // where nothing reads the `if`'s value.
        if (!discarded) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.IF_VALUE_REQUIRES_ELSE,
                "an `if` used as a value requires an `else` branch",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        _ = try infer_type.inferTypeFromExpr(self, if_expr.else_branch.?);
        type_info.* = then_type.*;
        return;
    }
    if (if_expr.else_branch) |else_branch| {
        self.discarded_grant = discarded;
        const else_type = try infer_type.inferTypeFromExpr(self, else_branch);

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
            } else if (!helpers.typesEqual(self, then_type, else_type)) {
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
}

pub fn matchExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo, discarded: bool) SemanticError!void {
    const match_expr = expr.data.Match;
    const subject_type = try infer_type.inferTypeFromExpr(self, match_expr.value);
    try helpers.checkGroupMatchExhaustive(self, match_expr, subject_type, getLocationFromBase(expr.base));
    try helpers.checkEnumMatchExhaustive(self, match_expr, subject_type, getLocationFromBase(expr.base));

    if (match_expr.cases.len > 0) {
        const group = helpers.matchSubjectGroup(self, subject_type);
        const subject_enum: ?ast.TypeRef = blk: {
            const custom = subject_type.custom_type orelse break :blk null;
            const ct = (try self.customType(custom.resolved())) orelse break :blk null;
            break :blk if (ct.kind == .Enum) custom.resolved() else null;
        };

        var union_types = std.array_list.Managed(*ast.TypeInfo).init(self.allocator);
        defer union_types.deinit();

        for (match_expr.cases) |*case| {
            try self.resolveMatchPatterns(case, group, subject_enum);
            const case_type = try self.inferMatchCaseTypeWithNarrow(case.*, subject_type, match_expr.value, discarded);
            try union_types.append(case_type);
        }
        if (!discarded) try patterns.checkUnionMatchExhaustive(self, match_expr, subject_type, getLocationFromBase(expr.base));

        if (union_types.items.len > 0) {
            // The match is one of its arms' values: their common type,
            // or the union of them. `Common.X` and `IO.Y` are two types,
            // though both are enums.
            type_info.* = (try helpers.createUnionType(self, union_types.items)).*;
        } else {
            type_info.* = .{ .base = .Nothing };
        }
    } else {
        type_info.* = .{ .base = .Nothing };
    }
}

pub fn grouping(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo, discarded: bool) SemanticError!void {
    const grouped_expr = expr.data.Grouping;
    if (grouped_expr) |expr_in_parens| {
        self.discarded_grant = discarded;
        type_info.* = (try infer_type.inferTypeFromExpr(self, expr_in_parens)).*;
    } else {
        type_info.* = .{ .base = .Nothing };
    }
}

pub fn returnExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const return_expr = expr.data.ReturnExpr;
    if (return_expr.value) |value| {
        const prev_bve = self.block_value_expected;
        self.block_value_expected = true;
        defer self.block_value_expected = prev_bve;
        type_info.* = (try self.checkReturnValue(value, getLocationFromBase(expr.base))).*;
    } else {
        self.checkBareReturn(getLocationFromBase(expr.base));
        type_info.* = .{ .base = .Nothing };
    }
}

const QuantifierKind = enum { exists, forall };

fn quantifierName(kind: QuantifierKind) []const u8 {
    return switch (kind) {
        .exists => "exists",
        .forall => "forall",
    };
}

fn quantifierTitle(kind: QuantifierKind) []const u8 {
    return switch (kind) {
        .exists => "Exists",
        .forall => "ForAll",
    };
}

pub fn existsExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const q = &expr.data.Exists;
    return checkQuantifier(self, expr, type_info, .exists, q.variable, q.array, q.condition, &q.storage);
}

pub fn forAll(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const q = &expr.data.ForAll;
    return checkQuantifier(self, expr, type_info, .forall, q.variable, q.array, q.condition, &q.storage);
}

fn checkQuantifier(
    self: *SemanticAnalyzer,
    expr: *ast.Expr,
    type_info: *ast.TypeInfo,
    kind: QuantifierKind,
    variable: ast.Token,
    array: *ast.Expr,
    condition: *ast.Expr,
    storage: *?u32,
) SemanticError!void {
    const array_type = try infer_type.inferTypeFromExpr(self, array);

    const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

    // The binding's type outlives this call: every read of the bound
    // name records it as a store target, so it cannot live on this
    // frame.
    const bound_var_type = try ast.TypeInfo.createDefault(self.allocator);
    bound_var_type.* = if (array_type.array_type) |elem_type|
        elem_type.*
    else
        ast.TypeInfo{ .base = .Int };

    self.checkFreshName(quantifier_scope, variable);
    const bound = quantifier_scope.createValueBinding(
        variable.lexeme,
        bound_var_type,
        true,
    ) catch |err| {
        if (err == error.DuplicateVariableName) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
                "Duplicate bound variable name '{s}' in {s} quantifier",
                .{ variable.lexeme, quantifierName(kind) },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            quantifier_scope.deinit();
            return;
        } else {
            quantifier_scope.deinit();
            return err;
        }
    };

    storage.* = bound.storage_id;

    const prev_scope = self.current_scope;
    self.current_scope = quantifier_scope;

    const condition_type = try infer_type.inferTypeFromExpr(self, condition);

    self.current_scope = prev_scope;

    quantifier_scope.deinit();

    if (array_type.base != .Array) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.INVALID_ARRAY_TYPE,
            "{s} requires array type, got {s}",
            .{ quantifierTitle(kind), @tagName(array_type.base) },
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    if (condition_type.base != .Tetra) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.INVALID_CONDITION_TYPE,
            "{s} condition must be tetra, got {s}",
            .{ quantifierTitle(kind), @tagName(condition_type.base) },
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    type_info.* = .{ .base = .Tetra };
}

pub fn assertExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const assert = expr.data.Assert;
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
        return;
    }

    type_info.* = .{ .base = .Nothing };
}

pub fn blockExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo, discarded: bool) SemanticError!void {
    const block = expr.data.Block;
    // Ensure statements inside a block expression are validated so that
    // statement-level transformations (e.g., @push lowering) occur within blocks
    const prev_scope = self.current_scope;
    const block_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
    self.current_scope = block_scope;
    defer {
        self.current_scope = prev_scope;
        self.closeBlockScope(block_scope);
    }

    try self.validateStatements(block.statements);

    var value_type_ptr: ?*ast.TypeInfo = null;
    if (block.value) |value_expr| {
        // A block's value is its tail's: discarded when the block's is.
        self.discarded_grant = discarded;
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
}

pub fn loopExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const loop = expr.data.Loop;
    // `each n in ^path`: the body borrows the path. Its source is
    // checked first, before the loop's own reads of it.
    const borrowed = if (loop.lent) try calls.borrowSource(self, loop.over.?) else false;
    defer if (borrowed) {
        const live = self.borrows.pop().?;
        self.allocator.free(live.path);
    };
    // What the loop iterates belongs to the scope around it; the header and
    // the body read it as a `Held` of the loop.
    if (loop.over) |over| try self.held.put(expr.base.id, try infer_type.inferTypeFromExpr(self, over));
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
        if (cond.data == .Literal) {
            const literal = cond.data.Literal;
            const tag = @tagName(literal.tetra);
            self.reporter.reportWarning(
                getLocationFromBase(cond.base),
                ErrorCode.CONDITION_ALWAYS_TRUE,
                "condition is always {s}",
                .{tag},
            );
        }
    }

    // The body writes the borrowed path through its element binding.
    if (borrowed) try effects.write(self, loop.over.?);

    // Create iteration scope for loop body
    const iteration_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
    defer iteration_scope.deinit();

    // Process loop body in iteration scope
    {
        const saved_scope = self.current_scope;
        self.current_scope = iteration_scope;
        defer self.current_scope = saved_scope;

        // A loop has no value: every iteration discards its body's.
        self.discarded_grant = true;
        _ = try infer_type.inferTypeFromExpr(self, loop.body);
    }

    if (loop.step) |stp| {
        self.discarded_grant = true;
        _ = try infer_type.inferTypeFromExpr(self, stp);
    }

    type_info.* = .{ .base = .Nothing };
}

pub fn rangeExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const range = expr.data.Range;
    const start_type = try infer_type.inferTypeFromExpr(self, range.start);
    const end_type = try infer_type.inferTypeFromExpr(self, range.end);

    if (start_type.base != .Int and start_type.base != .Float and start_type.base != .Byte) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
            "Range start value must be numeric, got {s}",
            .{@tagName(start_type.base)},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
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
        return;
    }

    const element_type = try ast.TypeInfo.createDefault(self.allocator);
    element_type.* = .{ .base = .Int };
    type_info.* = .{ .base = .Array, .array_type = element_type };
}
