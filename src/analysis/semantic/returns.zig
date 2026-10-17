//! Function body validation: parameter binding, return checking, and return-path analysis.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const helpers = @import("helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("eval_utils.zig");
const infer_type = @import("infer_type.zig");
const effects = @import("effects.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const Enclosing = SemanticAnalyzer.Enclosing;
const ParamRef = @import("semantic.zig").ParamRef;
const program = @import("program.zig");

pub fn validateFunctionBodyWithStruct(self: *SemanticAnalyzer, func: anytype, func_span: ast.SourceSpan, expected_return_type: ast.TypeInfo, enclosing: ?Enclosing) !void {
    // Create function scope with parameters
    const func_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

    // Add parameters to function scope
    for (func.params) |*param| {
        const param_type_info = if (param.type_expr) |type_expr|
            try self.typeExprToTypeInfo(type_expr)
        else
            try ast.TypeInfo.createDefault(self.allocator);

        // A default stands in for an argument the caller left out, so it
        // is typed against the parameter as an argument is.
        if (param.default_value) |default_value| {
            const default_type = try infer_type.inferTypeFromExpr(self, default_value);
            try helpers.unifyTypesExpr(self, param_type_info, default_type, default_value, .{ .location = getLocationFromBase(default_value.base) });
        }

        self.checkFreshName(func_scope, param.name);
        const param_var = func_scope.createValueBinding(
            param.name.lexeme,
            param_type_info,
            false, // Parameters are mutable
        ) catch |err| {
            if (err == error.DuplicateVariableName) {
                self.reporter.reportCompileError(
                    func_span.location,
                    ErrorCode.DUPLICATE_VARIABLE,
                    "Duplicate parameter name '{s}' in function '{s}'",
                    .{ param.name.lexeme, func.name.lexeme },
                );
                self.fatal_error = true;
                return;
            } else {
                return err;
            }
        };
        param_var.is_param = true;
        param_var.recordDeclLocation(param.name);
        param.storage = param_var.storage_id;
    }

    // The body's union `^` parameters, whose stores decide whether each
    // preserves its member.
    const outer_union_alias_params = self.union_alias_params;
    self.union_alias_params = std.AutoHashMap(u32, ParamRef).init(self.allocator);
    defer {
        self.union_alias_params.deinit();
        self.union_alias_params = outer_union_alias_params;
    }
    // The body's parameters and receiver, whose writes decide which
    // by-value parameters a call must copy, and which arguments lent to
    // a `^` parameter are written (`effects.zig`).
    const outer_params = self.effects.params;
    const outer_receiver = self.effects.receiver;
    self.effects.params = .empty;
    self.effects.receiver = null;
    defer {
        self.effects.params.deinit(self.allocator);
        self.effects.params = outer_params;
        self.effects.receiver = outer_receiver;
    }
    if (program.registeredSignature(self, func.name.lexeme, enclosing)) |signature| {
        try self.checked_signatures.put(signature, {});
        for (func.params, 0..) |param, i| {
            try self.effects.params.put(self.allocator, param.storage.?, .{ .signature = signature, .index = @intCast(i) });
            if (!param.is_alias or signature.params[i].base != .Union) continue;
            try self.union_alias_params.put(param.storage.?, .{ .signature = signature, .index = @intCast(i) });
        }
        if (enclosing) |e| if (e.has_this) {
            self.effects.receiver = .{ .signature = signature, .index = null };
        };
    }

    // Temporarily set current scope to function scope
    const prev_scope = self.current_scope;
    const prev_enclosing = self.enclosing;
    const prev_returning = self.returning;
    self.current_scope = func_scope;
    self.enclosing = enclosing;
    self.returning = .{ .function = func.name, .type = expected_return_type };
    defer self.returning = prev_returning;

    // Every `return` is checked against `expected_return_type` as the body
    // is validated; what is left is whether every path reaches one.
    try self.validateStatements(func.body);
    try validateReturnPaths(self, func.body, expected_return_type, func_span);

    // Restore previous scope and struct type
    self.current_scope = prev_scope;
    self.enclosing = prev_enclosing;
}

/// Report a function that declares a return type but has no path to a
/// `return`. Return values are checked where each `return` is analyzed
/// (`checkReturnValue`), not here.
pub fn validateReturnPaths(self: *SemanticAnalyzer, body: []ast.Stmt, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!void {
    if (expected_return_type.base == .Nothing) return;
    if (try checkStatementsHaveReturns(self, body)) return;
    self.reporter.reportCompileError(
        func_span.location,
        ErrorCode.MISSING_RETURN_VALUE,
        "Function expects return value of type {s}; add an explicit return",
        .{@tagName(expected_return_type.base)},
    );
    self.fatal_error = true;
}

/// Infer a returned value and check it against the enclosing function's
/// declared return type. A function without `returns` returns `nothing`,
/// so a value returned from it is an error.
pub fn checkReturnValue(self: *SemanticAnalyzer, value: *ast.Expr, location: Location) ErrorList!*ast.TypeInfo {
    const returning = self.returning orelse return infer_type.inferTypeFromExpr(self, value);
    const value_type = try infer_type.inferTypeIn(self, value, &returning.type);
    if (returning.type.base == .Nothing) {
        self.reporter.reportCompileError(
            location,
            ErrorCode.TYPE_MISMATCH,
            "'{s}' declares no `returns`, so it cannot return a value",
            .{returning.function.lexeme},
        );
        self.fatal_error = true;
        return value_type;
    }
    try validateReturnTypeCompatibility(self, &returning.type, value_type, value, .{ .location = location });
    return value_type;
}

/// A bare `return` returns `nothing`, so the function has to declare a
/// return type that admits it: none at all, or a union with a `nothing`
/// member. A function returns what it declares; nothing widens it.
pub fn checkBareReturn(self: *SemanticAnalyzer, location: Location) void {
    const returning = self.returning orelse return;
    const declared = returning.type;
    if (admitsNothing(&declared)) return;
    self.reporter.reportCompileError(
        location,
        ErrorCode.TYPE_MISMATCH,
        "'{s}' returns {s}, so a bare `return` has no value to give; declare `returns nothing | {s}` to return nothing",
        .{ returning.function.lexeme, helpers.typeLabel(&declared), helpers.typeLabel(&declared) },
    );
    self.fatal_error = true;
}

pub fn checkExpressionHasReturns(self: *SemanticAnalyzer, expr: *ast.Expr) ErrorList!bool {
    switch (expr.data) {
        .Block => |block| {
            return checkStatementsHaveReturns(self, block.statements);
        },
        .If => |if_data| {
            // Check both branches - if either has returns, the expression has returns
            var has_returns = false;
            if (if_data.then_branch) |then_branch| {
                if (try checkExpressionHasReturns(self, then_branch)) has_returns = true;
            }
            if (if_data.else_branch) |else_branch| {
                if (try checkExpressionHasReturns(self, else_branch)) has_returns = true;
            }
            return has_returns;
        },
        .Match => |match_expr| {
            // A `match` carries returns when any arm does; the arms are
            // where every path out of a `match`-shaped function returns.
            for (match_expr.cases) |case| {
                if (try checkExpressionHasReturns(self, case.body)) return true;
            }
            return false;
        },
        .Cast => |cast| {
            // `x as T then .. else ..` is the narrowing form of a
            // conditional: its branches are where those paths return.
            if (cast.then_branch) |then_branch| {
                if (try checkExpressionHasReturns(self, then_branch)) return true;
            }
            if (cast.else_branch) |else_branch| {
                if (try checkExpressionHasReturns(self, else_branch)) return true;
            }
            return false;
        },
        .ReturnExpr => return true,
        .Unreachable => return true,
        .Loop => |loop_data| {
            // For loops, check if the body contains return statements
            if (loop_data.body.data == .Block) {
                return checkStatementsHaveReturns(self, loop_data.body.data.Block.statements);
            } else {
                return checkExpressionHasReturns(self, loop_data.body);
            }
        },
        else => return false,
    }
}

pub fn checkStatementsHaveReturns(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!bool {
    for (statements) |stmt| {
        switch (stmt.data) {
            .Return => return true,
            .Expression => |expr| {
                if (expr) |expression| {
                    if (try checkExpressionHasReturns(self, expression)) return true;
                }
            },
            else => {},
        }
    }
    return false;
}

/// Whether a value of `t` may be `nothing`: `nothing` itself, or a union
/// with a `nothing` member.
pub fn admitsNothing(t: *const ast.TypeInfo) bool {
    return t.base == .Nothing or (t.base == .Union and unionHasNothing(t));
}

pub fn unionHasNothing(t: *const ast.TypeInfo) bool {
    const union_type = t.union_type orelse return false;
    for (union_type.types) |member| if (member.base == .Nothing) return true;
    return false;
}

/// Whether `expected` accepts `actual` as the same named type: an enum
/// value matches a named enum, and either matches a group that contains it.
/// This is what widens `nothing | error.IO` (an inline-Zig fallible return)
/// into `nothing | error.StdError`, where the group's members are the enums.
pub fn namedMemberAccepts(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *const ast.TypeInfo) bool {
    const expected_ref = switch (expected.custom_type orelse return false) {
        .ref => |r| r,
        .written => return false,
    };
    if (self.custom_types.get(expected_ref)) |custom| {
        if (custom.kind == .Enum and actual.base == .Enum) return true;
    }
    const actual_ref = switch (actual.custom_type orelse return false) {
        .ref => |r| r,
        .written => return false,
    };
    const group_id = self.group_table.idOf(expected_ref) orelse return false;
    const group_members = self.group_table.members(group_id) orelse return false;
    for (group_members) |member| {
        if (member.ref.eql(actual_ref)) return true;
    }
    return false;
}

pub fn validateReturnTypeCompatibility(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, actual_expr: ?*ast.Expr, span: ast.SourceSpan) !void {
    // A failed expression already has its error.
    if (actual.base == .Error) return;
    // `nothing` is returned only where the declared type admits it.
    if (actual.base == .Nothing or (actual.base == .Union and unionHasNothing(actual))) {
        if (!admitsNothing(expected)) {
            self.reporter.reportCompileError(
                span.location,
                ErrorCode.TYPE_MISMATCH,
                "this return can be `nothing`, which {s} does not admit; declare `returns nothing | {s}`",
                .{ helpers.typeLabel(expected), helpers.typeLabel(expected) },
            );
            self.fatal_error = true;
            return;
        }
        if (actual.base == .Nothing) return;
    }
    // Handle union types for return type checking
    if (expected.base == .Union) {
        if (expected.union_type) |union_type| {

            // Check if actual type is a union
            if (actual.base == .Union) {
                if (actual.union_type) |actual_union| {
                    // Check that every member of the actual union is compatible with the expected union
                    for (actual_union.types) |actual_member| {
                        // `nothing` was checked against the declared type above.
                        if (actual_member.base == .Nothing) continue;
                        var found_match = false;
                        for (union_type.types) |expected_member| {
                            if (actual_member.base == expected_member.base) {
                                found_match = true;
                                break;
                            }
                            // Allow enum literal (Enum) to match a Custom enum member in the expected union
                            if (expected_member.base == .Custom and actual_member.base == .Enum) {
                                if (expected_member.custom_type) |custom| {
                                    if (self.custom_types.get(custom.resolved())) |ct| {
                                        if (ct.kind == .Enum) {
                                            found_match = true;
                                            break;
                                        }
                                    }
                                }
                            }
                            if (namedMemberAccepts(self, expected_member, actual_member)) {
                                found_match = true;
                                break;
                            }
                        }
                        if (!found_match) {
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
                                "Return type {s} is not compatible with function's union return type",
                                .{@tagName(actual_member.base)},
                            );
                            self.fatal_error = true;
                            return;
                        }
                    }
                    return; // All members are compatible
                }
            } else {
                // Single type - early accept for Nothing handled above
                // Single type - check if it's a member of the expected union
                var found_match = false;
                for (union_type.types) |member_type| {
                    if (member_type.base == actual.base) {
                        found_match = true;
                        break;
                    }
                    // Allow enum literal (Enum) to match a Custom enum member in the expected union
                    if (member_type.base == .Custom and actual.base == .Enum) {
                        if (member_type.custom_type) |custom| {
                            if (self.custom_types.get(custom.resolved())) |ct| {
                                if (ct.kind == .Enum) {
                                    found_match = true;
                                    break;
                                }
                            }
                        }
                    }
                    if (namedMemberAccepts(self, member_type, actual)) {
                        found_match = true;
                        break;
                    }
                }

                if (!found_match) {
                    // Build a list of allowed types for the error message
                    var type_list = std.array_list.Managed(u8).init(self.allocator);
                    defer type_list.deinit();

                    for (union_type.types, 0..) |member_type, i| {
                        if (i > 0) try type_list.appendSlice(" | ");
                        try type_list.appendSlice(@tagName(member_type.base));
                    }

                    self.reporter.reportCompileError(
                        span.location,
                        ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
                        "Return type {s} is not compatible with function's union return type ({s})",
                        .{ @tagName(actual.base), type_list.items },
                    );
                    self.fatal_error = true;
                    return;
                }
            }
        }
    } else {
        // Non-union expected type - use regular type unification
        try helpers.unifyTypesExpr(self, expected, actual, actual_expr, span);
    }
}
