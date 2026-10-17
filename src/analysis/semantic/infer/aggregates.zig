//! Type rules for literals, arrays, enum members, and type expressions.

const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;

pub fn literal(_: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const lit = expr.data.Literal;
    type_info.inferFrom(lit);
    switch (lit) {
        .int => |i| type_info.comptime_int = i,
        .byte => |b| type_info.comptime_int = @as(i64, b),
        else => {},
    }
}

pub fn breakExpr(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.base = .Nothing;
}

pub fn array(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const elements = expr.data.Array;
    if (elements.len == 0) {
        type_info.* = .{ .base = .Array, .array_type = null };
    } else {
        // The element type is the elements' promoted type: a float
        // among int or byte elements makes the array float[], wherever
        // it sits (docs/math.md). Every element is then checked against
        // that type, so a literal widens and a runtime value does not.
        const element_types = try self.allocator.alloc(*ast.TypeInfo, elements.len);
        defer self.allocator.free(element_types);
        var joined = try infer_type.inferTypeFromExpr(self, elements[0]);
        element_types[0] = joined;
        for (elements[1..], element_types[1..]) |element, *element_type| {
            element_type.* = try infer_type.inferTypeFromExpr(self, element);
            if (element_type.*.base == .Float and (joined.base == .Int or joined.base == .Byte)) joined = element_type.*;
            // An empty element takes its element type from a sibling
            // that has one: `[[], [1]]` is an `int[][]`.
            if (helpers.hasUninferredElement(joined) and !helpers.hasUninferredElement(element_type.*)) joined = element_type.*;
        }
        for (elements, element_types) |element, element_type| {
            if (element_type == joined) continue;
            try helpers.unifyTypesExpr(self, joined, element_type, element, .{ .location = getLocationFromBase(expr.base) });
        }
        const array_type = try ast.TypeInfo.createDefault(self.allocator);
        array_type.* = joined.*;
        // An element type is a type, not a literal: `[65]` is an
        // `int[]` whose elements a byte context may not narrow.
        array_type.comptime_int = null;
        type_info.* = .{ .base = .Array, .array_type = array_type };
    }
}

pub fn unreachableExpr(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Nothing };
}

pub fn enumMember(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    // Untyped until its context names the enum; checked when the
    // record's analysis ends (`reportUntypedVariants`).
    try self.bare_variants.append(self.allocator, expr);
    type_info.* = .{ .base = .Enum };
}

pub fn defaultArgPlaceholder(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Nothing };
}

pub fn peek(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const _peek = expr.data.Peek;
    const expr_type = try infer_type.inferTypeFromExpr(self, _peek.expr);
    type_info.* = expr_type.*;
}

pub fn interpolatedString(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const template = expr.data.InterpolatedString;
    for (template.parts) |part| {
        switch (part) {
            .Expression => |part_expr| {
                _ = try infer_type.inferTypeFromExpr(self, part_expr);
            },
            .String => {},
        }
    }
    type_info.* = .{ .base = .String };
}

pub fn structDecl(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Struct };
}

pub fn enumDecl(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Enum };
}

pub fn groupDecl(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Enum, .custom_type = .{ .ref = .{ .module = self.current_module, .name = expr.data.GroupDecl.name.lexeme } } };
}

pub fn arrayType(_: *SemanticAnalyzer, _: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    type_info.* = .{ .base = .Array };
}

pub fn typeExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const type_expr = expr.data.TypeExpr;
    type_info.* = (try self.typeExprToTypeInfo(type_expr)).*;
}

pub fn thisExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    // `this` is the receiver of the method whose body is being checked.
    const enclosing = self.enclosing orelse {
        self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_THIS, "`this` is only available inside a method", .{});
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    };
    if (!enclosing.has_this) {
        self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_THIS, "`this` is only available inside a method; '{s}' functions have no receiver", .{enclosing.ref.name});
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }
    type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = enclosing.ref }, .is_mutable = false };
}
