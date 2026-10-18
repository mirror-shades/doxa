//! Type rules for calls: functions, methods, compiler methods, and `^` arguments.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const TokenType = @import("../../../types/token.zig").TokenType;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const borrows = @import("../borrows.zig");
const effects = @import("../effects.zig");
const ParamRef = @import("../semantic.zig").ParamRef;
const builtin_methods = @import("../../../runtime/builtin_methods.zig");
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;
const member_mod = @import("member.zig");
const builtins = @import("builtins.zig");

pub fn functionCall(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const function_call = expr.data.FunctionCall;
    if (function_call.callee.data == .FieldAccess) {
        _ = try inferMemberCall(self, expr, function_call.callee, function_call.arguments, type_info);
        return;
    }

    const callee_type = try infer_type.inferTypeFromExpr(self, function_call.callee);
    if (callee_type.base == .Function) {
        if (callee_type.function_type) |func_type| {
            if (!try validateFunctionCallArguments(self, expr, function_call.arguments, func_type)) return;
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

pub fn internalCall(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const method_call = expr.data.InternalCall;
    const method_name = method_call.method.lexeme;
    if (resizes(method_call.method.type)) try borrows.check(self, method_call.receiver, expr, .resize);
    // `@std` takes no arguments; its parser-supplied receiver is a
    // placeholder, not an argument.
    if (method_call.method.type == .STD) {
        type_info.* = (try builtins.inferBuiltinCall(self, expr, method_name, null, &.{})).*;
        return;
    }
    const result = try builtins.inferBuiltinCall(self, expr, method_name, method_call.receiver, method_call.arguments);
    // An in-place intrinsic writes its subject; one the registry does
    // not know is taken to.
    const in_place = if (builtin_methods.getMethodInfoByName(method_name)) |info| info.mutates_arg else true;
    if (in_place) try effects.write(self, method_call.receiver);
    type_info.* = result.*;
}

/// A call through a field access: a module function (`ns.fn(...)`), a static
/// method or constructor (`T.m(...)`), or a method on a value (`x.m(...)`).
/// The callee is recorded as a resolution, so codegen dispatches without
/// re-deriving any of this.
pub fn inferMemberCall(self: *SemanticAnalyzer, expr: *ast.Expr, callee: *ast.Expr, arguments: []const ast.CallArgument, type_info: *ast.TypeInfo) SemanticError!*ast.TypeInfo {
    const field_access = callee.data.FieldAccess;
    const method_name = field_access.field.lexeme;

    if (try names.namespaceOf(self, field_access.object)) |namespace| {
        const function_type = try member_mod.memberOfNamespace(self, callee, namespace, field_access.field, try ast.TypeInfo.createDefault(self.allocator));
        try self.type_cache.put(callee.base.id, function_type);
        if (function_type.base != .Function or function_type.function_type == null) {
            if (function_type.base != .Nothing) {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_FUNCTION_CALL, "'{s}' is not a function", .{method_name});
                self.fatal_error = true;
            }
            type_info.base = .Nothing;
            return type_info;
        }
        if (!try validateFunctionCallArguments(self, expr, arguments, function_type.function_type.?)) {
            type_info.base = .Nothing;
            return type_info;
        }
        type_info.* = function_type.function_type.?.return_type.*;
        return type_info;
    }

    if (try member_mod.typeNamed(self, field_access.object)) |owner| {
        const method = (try member_mod.methodOf(self, expr, owner, method_name, null)) orelse {
            if (!self.fatal_error) {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "Unknown method '{s}' on struct '{s}'", .{ method_name, owner.name });
                self.fatal_error = true;
            }
            type_info.base = .Nothing;
            return type_info;
        };
        if (!method.is_static) {
            self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "Method '{s}' of '{s}' needs a receiver; call it on a value", .{ method_name, owner.name });
            self.fatal_error = true;
            type_info.base = .Nothing;
            return type_info;
        }
        try names.annotate(self, callee, .{ .static_method = .{ .owner = owner, .name = method.name } });
        if (!try validateFunctionCallArguments(self, expr, arguments, method.signature)) {
            type_info.base = .Nothing;
            return type_info;
        }
        type_info.* = method.signature.return_type.*;
        return type_info;
    }

    const object_type = try infer_type.inferTypeFromExpr(self, field_access.object);
    switch (object_type.base) {
        // A built-in type has no methods of its own: a compiler method is
        // `@`-prefixed (`xs.@push(1)`, `@push(xs, 1)`), so `xs.push(1)` is
        // never one (docs/methods.md).
        .Array, .String => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.UNKNOWN_METHOD,
                "{s} has no method '{s}'; compiler methods are `@`-prefixed: `.@{s}(...)`",
                .{ if (object_type.base == .Array) "An array" else "A string", method_name, method_name },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        .Struct, .Custom => {
            if (object_type.custom_type) |custom| {
                const owner = custom.resolved();
                const start = self.reporter.diagnostics.items.len;
                if (try member_mod.methodOf(self, expr, owner, method_name, field_access.object)) |method| {
                    // A struct's function called through `this` takes no
                    // receiver; any other value cannot stand in for the type.
                    if (method.is_static and field_access.object.data != .This) {
                        self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "'{s}' is a function of '{s}'; call it as {s}.{s}()", .{ method_name, owner.name, owner.name, method_name });
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    try names.annotate(self, callee, if (method.is_static)
                        .{ .static_method = .{ .owner = owner, .name = method.name } }
                    else
                        .{ .method = .{ .owner = owner, .name = method.name } });
                    if (!method.is_static) try effects.receiverCall(self, field_access.object, method.signature);
                    if (!try validateFunctionCallArguments(self, expr, arguments, method.signature)) {
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = method.signature.return_type.*;
                    return type_info;
                }
                if (self.reporter.firstErrorSince(start) != null) {
                    type_info.base = .Nothing;
                    return type_info;
                }
            }

            // A function-typed field called like a method.
            if (object_type.struct_fields) |fields| {
                for (fields) |field| {
                    if (std.mem.eql(u8, field.name, method_name) and field.type_info.base == .Function) {
                        if (!try validateFunctionCallArguments(self, expr, arguments, field.type_info.function_type.?)) {
                            type_info.base = .Nothing;
                            return type_info;
                        }
                        type_info.* = field.type_info.function_type.?.return_type.*;
                        return type_info;
                    }
                }
            }

            const display_name = if (object_type.custom_type) |custom| custom.displayName() else "<struct>";
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
    return type_info;
}

pub fn validateFunctionCallArguments(self: *SemanticAnalyzer, expr: *ast.Expr, arguments: []const ast.CallArgument, func_type: *const ast.FunctionType) SemanticError!bool {
    const expected_arg_count: usize = func_type.params.len;
    var provided_arg_count: usize = 0;
    var has_placeholders: bool = false;
    for (arguments) |arg_expr_it| {
        if (arg_expr_it.expr.data != .DefaultArgPlaceholder) {
            provided_arg_count += 1;
        } else {
            has_placeholders = true;
        }
    }

    if (arguments.len > expected_arg_count) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.TOO_MANY_ARGUMENTS,
            "Too many arguments: expected at most {}, got {}",
            .{ expected_arg_count, arguments.len },
        );
        self.fatal_error = true;
        return false;
    }

    if (provided_arg_count < expected_arg_count and !has_placeholders) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.TOO_FEW_ARGUMENTS,
            "Too few arguments: expected {}, got {} (use ~ to explicitly skip parameters)",
            .{ expected_arg_count, provided_arg_count },
        );
        self.fatal_error = true;
        return false;
    }

    var param_index: usize = 0;
    for (arguments) |arg_expr_it| {
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
            return false;
        }

        if (func_type.param_aliases != null and !func_type.param_aliases.?[param_index] and arg_expr_it.is_alias) {
            self.reporter.reportCompileError(
                getLocationFromBase(arg_expr_it.expr.base),
                ErrorCode.ALIAS_ARGUMENT_NOT_NEEDED,
                "Function parameter does not require an alias argument (remove ^ before the argument)",
                .{},
            );
            self.fatal_error = true;
            return false;
        }

        const arg_type = try infer_type.inferTypeIn(self, arg_expr_it.expr, &func_type.params[param_index]);
        if (arg_expr_it.is_alias) {
            if (!try checkAliasArgument(self, arg_expr_it.expr, func_type, param_index)) return false;
        } else if (func_type.params[param_index].base != .Nothing) {
            var expected_type = func_type.params[param_index];
            try helpers.unifyTypesExpr(self, &expected_type, arg_type, arg_expr_it.expr, .{ .location = getLocationFromBase(expr.base) });
        }
        param_index += 1;
    }

    // A `^` loan is exclusive for the call (`docs/alias.md`): one storage lent
    // to two parameters would reach the callee under two names.
    for (arguments, 0..) |arg, i| {
        if (!arg.is_alias) continue;
        const target = self.getStoreTarget(arg.expr.base.id) orelse continue;
        for (arguments[0..i]) |earlier| {
            if (!earlier.is_alias) continue;
            const earlier_target = self.getStoreTarget(earlier.expr.base.id) orelse continue;
            if (earlier_target.storage != target.storage) continue;
            // The parser admits only a name after `^`.
            const name = if (arg.expr.data == .Variable) arg.expr.data.Variable.lexeme else "this value";
            self.reporter.reportCompileError(
                getLocationFromBase(arg.expr.base),
                ErrorCode.BORROW_CONFLICT,
                "this call already lends '{s}' to a ^ parameter; a value is lent to one ^ parameter at a time",
                .{name},
            );
            self.fatal_error = true;
            return false;
        }
    }

    return true;
}

/// A `^` argument lends its storage, so the storage's type — the binding
/// beneath any narrowing view — must be exactly the parameter's: a callee may
/// store any value of its parameter type, which a narrower or wider caller slot
/// cannot hold. The one exception is a union parameter that preserves its
/// member, which may be lent storage of any one member type; that is settled
/// once every body is analyzed (`settleMemberLoans`). An unannotated parameter
/// accepts any storage.
/// Whether the intrinsic changes its subject's length.
pub fn resizes(method: TokenType) bool {
    return switch (method) {
        .PUSH, .POP, .INSERT, .REMOVE, .CLEAR => true,
        else => false,
    };
}

/// Begin the borrow `each n in ^source` takes, once `source` is known to be
/// a path (rule 1) overlapping no live borrow. False when it is not taken.
pub fn borrowSource(self: *SemanticAnalyzer, source: *ast.Expr) !bool {
    const path = try borrows.sourcePath(self.allocator, source) orelse {
        self.reporter.reportCompileError(
            getLocationFromBase(source.base),
            ErrorCode.INVALID_BORROW_SOURCE,
            "'each ... in ^' borrows a variable or a field of one, such as '^items' or '^game.players': bind this value to a variable first",
            .{},
        );
        self.fatal_error = true;
        return false;
    };
    try borrows.checkPath(self, path, source, .borrow);
    try self.borrows.append(self.allocator, .{ .path = path, .source = source });
    return true;
}

pub fn checkAliasArgument(self: *SemanticAnalyzer, arg: *ast.Expr, signature: *const ast.FunctionType, index: usize) !bool {
    const param = &signature.params[index];
    try borrows.check(self, arg, arg, .lend);
    try effects.lend(self, arg, signature, @intCast(index));
    if (param.base == .Nothing) return true;
    // The parser admits only a name after `^`; an unresolved one is already
    // `Undefined variable`.
    const target = self.getStoreTarget(arg.base.id) orelse return true;
    const lent_to: ParamRef = .{ .signature = signature, .index = @intCast(index) };
    if (helpers.typesEqual(self, target.slot, param)) {
        // Lending a union `^` parameter on: it preserves its member only if
        // the parameter it is lent to does.
        if (self.union_alias_params.get(target.storage)) |from| {
            try self.member_relays.append(self.allocator, .{ .from = from, .to = lent_to });
        }
        return true;
    }
    if (param.base == .Union and target.slot.base != .Union) {
        for (param.union_type.?.types) |member| {
            if (!helpers.typesEqual(self, target.slot, member)) continue;
            try self.member_loans.append(self.allocator, .{ .param = lent_to, .location = getLocationFromBase(arg.base) });
            return true;
        }
    }
    self.reporter.reportCompileError(
        getLocationFromBase(arg.base),
        ErrorCode.INVALID_ALIAS_ARGUMENT,
        "An alias argument lends its storage, so its type must be exactly the parameter's: {s} is not {s}",
        try helpers.typeLabels(self, target.slot, param),
    );
    self.fatal_error = true;
    return false;
}
