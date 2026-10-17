//! Type inference for expressions. This file owns the entry points and the
//! tag dispatch; each expression family's rule lives in `infer/`.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const helpers = @import("./helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;

const access = @import("infer/access.zig");
const aggregates = @import("infer/aggregates.zig");
const calls = @import("infer/calls.zig");
const cast = @import("infer/cast.zig");
const control = @import("infer/control.zig");
const fields = @import("infer/fields.zig");
const operators = @import("infer/operators.zig");

pub const SemanticError = std.mem.Allocator.Error || ErrorList;

/// The type of `expr`. Every answer is recorded in `type_cache` under the
/// expression's node id, whichever arm of the inference produced it and
/// whether or not that arm reported an error: later stages read an
/// expression's type from the cache instead of deriving it again
/// (plan/type-authority.md), so an arm that returned without recording would
/// leave them nothing to read.
/// The type of `expr` in a position that expects `expected`. An array literal
/// there is typed by it, element by element: each element is checked against
/// the expected element type, so the elements need not agree with one another
/// (`[IOError.NotFound, ParseError.Eof]` is an `Error[]`). Anything else is
/// inferred on its own, and the caller unifies it with `expected` as before.
pub fn inferTypeIn(self: *SemanticAnalyzer, expr: *ast.Expr, expected: *const ast.TypeInfo) SemanticError!*ast.TypeInfo {
    // A concatenation is the array its context expects, and so is each side.
    if (expected.base == .Array and expr.data == .Binary and expr.data.Binary.operator.type == .PLUS) {
        _ = try inferTypeIn(self, expr.data.Binary.left.?, expected);
        _ = try inferTypeIn(self, expr.data.Binary.right.?, expected);
        return inferTypeFromExpr(self, expr);
    }
    if (expected.base == .Array and expr.data == .Grouping and expr.data.Grouping != null) {
        _ = try inferTypeIn(self, expr.data.Grouping.?, expected);
        return inferTypeFromExpr(self, expr);
    }
    if (expr.data != .Array or expected.base != .Array) {
        const inferred = try inferTypeFromExpr(self, expr);
        // An int literal a byte context expects is that byte (an
        // out-of-range one is the context's check to report).
        if (inferred.comptime_int) |v| if (v >= 0 and v <= 255) {
            _ = helpers.adoptByteLiteral(self, expr, inferred, expected);
        };
        return inferred;
    }
    const element_type = expected.array_type orelse return inferTypeFromExpr(self, expr);
    if (self.type_cache.get(expr.base.id)) |cached| return cached;
    const start = self.reporter.diagnostics.items.len;
    for (expr.data.Array) |element| {
        const actual = try inferTypeIn(self, element, element_type);
        if (!helpers.adoptByteLiteral(self, element, actual, element_type)) break;
        try helpers.unifyTypesExpr(self, element_type, actual, element, .{ .location = getLocationFromBase(element.base) });
    }
    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    type_info.* = if (self.reporter.firstErrorSince(start) != null) .{ .base = .Error } else .{
        .base = .Array,
        .array_type = element_type,
        .array_storage = expected.array_storage,
        .array_size = expected.array_size,
    };
    try self.type_cache.put(expr.base.id, type_info);
    return type_info;
}

pub fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) SemanticError!*ast.TypeInfo {
    // Whether this expression's value is discarded is decided here, once:
    // the grant covers only the inference it was given for.
    const discarded = self.discarded_grant;
    self.discarded_grant = false;
    if (self.type_cache.get(expr.base.id)) |cached| {
        return cached;
    }
    const start = self.reporter.diagnostics.items.len;
    const inferred = try inferTypeFromExprUncached(self, expr, discarded);
    // A failed expression is not a value: whatever reported an error while
    // it was analyzed, its type is poison, which every check accepts.
    if (self.reporter.firstErrorSince(start) != null) inferred.* = .{ .base = .Error };
    try self.type_cache.put(expr.base.id, inferred);
    // A declaration's own expression has no value type to complete.
    const declares = switch (expr.data) {
        .StructDecl, .EnumDecl, .GroupDecl => true,
        else => false,
    };
    if (!declares and helpers.incompleteness(inferred) != .complete) try self.incomplete.append(self.allocator, expr);
    return inferred;
}

fn inferTypeFromExprUncached(self: *SemanticAnalyzer, expr: *ast.Expr, discarded: bool) SemanticError!*ast.TypeInfo {
    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    errdefer self.allocator.destroy(type_info);

    switch (expr.data) {
        .Literal => try aggregates.literal(self, expr, type_info),
        .Break => try aggregates.breakExpr(self, expr, type_info),
        .Binary => try operators.binary(self, expr, type_info),
        .Variable => try access.variableExpr(self, expr, type_info),
        .Unary => try operators.unaryExpr(self, expr, type_info),
        .FunctionCall => try calls.functionCall(self, expr, type_info),
        .Index => try access.indexExpr(self, expr, type_info),
        .FieldAccess => try fields.fieldAccess(self, expr, type_info),
        .Array => try aggregates.array(self, expr, type_info),
        .If => try control.ifExpr(self, expr, type_info, discarded),
        .Logical => try operators.logicalExpr(self, expr, type_info),
        .Match => try control.matchExpr(self, expr, type_info, discarded),
        .Grouping => try control.grouping(self, expr, type_info, discarded),
        .Assignment => try access.assignment(self, expr, type_info),
        .ReturnExpr => try control.returnExpr(self, expr, type_info),
        .Unreachable => try aggregates.unreachableExpr(self, expr, type_info),
        .InternalCall => try calls.internalCall(self, expr, type_info),
        .EnumMember => try aggregates.enumMember(self, expr, type_info),
        .DefaultArgPlaceholder => try aggregates.defaultArgPlaceholder(self, expr, type_info),
        .Peek => try aggregates.peek(self, expr, type_info),
        .InterpolatedString => try aggregates.interpolatedString(self, expr, type_info),
        .IndexAssign => try access.indexAssign(self, expr, type_info),
        .FieldAssignment => try fields.fieldAssignment(self, expr, type_info),
        .Exists => try control.existsExpr(self, expr, type_info),
        .ForAll => try control.forAll(self, expr, type_info),
        .Assert => try control.assertExpr(self, expr, type_info),
        .StructDecl => try aggregates.structDecl(self, expr, type_info),
        .StructLiteral => try fields.structLiteral(self, expr, type_info),
        .EnumDecl => try aggregates.enumDecl(self, expr, type_info),
        .GroupDecl => try aggregates.groupDecl(self, expr, type_info),
        .ArrayType => try aggregates.arrayType(self, expr, type_info),
        .Block => try control.blockExpr(self, expr, type_info, discarded),
        .TypeExpr => try aggregates.typeExpr(self, expr, type_info),
        .Cast => try cast.castExpr(self, expr, type_info, discarded),
        .Loop => try control.loopExpr(self, expr, type_info),
        .This => try aggregates.thisExpr(self, expr, type_info),
        .Range => try control.rangeExpr(self, expr, type_info),
        .CompoundAssign => try access.compoundAssign(self, expr, type_info),
        .Held => try access.heldExpr(self, expr, type_info),
    }

    return type_info;
}
