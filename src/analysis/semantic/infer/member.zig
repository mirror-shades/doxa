//! Resolution of members: namespace members, type names, and methods.

const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const TypeRef = ast.TypeRef;
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;

/// The type of `namespace`'s member `name` in value position: a function, a
/// global, or a type. The access is recorded as a resolution. A namespace is
/// not a value.
pub fn memberOfNamespace(self: *SemanticAnalyzer, expr: *ast.Expr, namespace: @import("../../../module/ids.zig").ModuleId, name: ast.Token, type_info: *ast.TypeInfo) SemanticError!*ast.TypeInfo {
    const bound = try names.member(self, namespace, name);
    const symbol = switch (bound.binding) {
        .symbol => |symbol| symbol,
        .namespace => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULE_NAMESPACE_NOT_A_VALUE,
                "Module namespace '{s}' is not a value",
                .{name.lexeme},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return type_info;
        },
    };
    const resolved = names.symbolResolution(self, symbol).?;
    try names.annotate(self, expr, resolved);
    if (resolved == .global) {
        try self.recordGlobalSite(.{ .symbol = symbol, .place = .{ .member = expr } });
    }
    const variable = (try names.declaredVariable(self, symbol)) orelse {
        // The defining record bound the name but registered nothing under it:
        // its registration is still active (a declaration cycle).
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.CIRCULAR_IMPORT,
            "'{s}' is used before its module finished declaring it",
            .{name.lexeme},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return type_info;
    };
    const storage = self.memory.scope_manager.value_storage.get(variable.storage_id) orelse return error.StorageNotFound;
    // A qualified global is rewritten in place into its link name, which
    // codegen reads as any other name.
    if (resolved == .global) try names.recordStoreTarget(self, expr.base.id, variable, variable, true);
    type_info.* = storage.type_info.*;
    return type_info;
}

/// The type an expression names in value position — a bare type name or a
/// namespace-qualified one — recorded as a type resolution; null when the
/// expression is not a type name.
pub fn typeNamed(self: *SemanticAnalyzer, expr: *ast.Expr) SemanticError!?TypeRef {
    switch (expr.data) {
        .Variable => |tok| {
            const result = try names.resolveName(self, tok.lexeme);
            const symbol = result.symbol orelse return null;
            if (symbol.kind != .Type) return null;
            try names.annotate(self, expr, .{ .type = symbol.typeRef() });
            return symbol.typeRef();
        },
        .FieldAccess => |field| {
            const namespace = (try names.namespaceOf(self, field.object)) orelse return null;
            const bound = try names.member(self, namespace, field.field);
            const symbol = switch (bound.binding) {
                .symbol => |symbol| symbol,
                .namespace => return null,
            };
            if (symbol.kind != .Type) return null;
            try names.annotate(self, expr, .{ .type = symbol.typeRef() });
            return symbol.typeRef();
        },
        else => return null,
    }
}

/// A method or function of `owner` called through `receiver` (null when it is
/// called through the type, `T.f()`). A private one is reachable only from
/// inside the struct: through `this`, or through the type from the struct's own
/// functions and methods.
pub fn methodOf(self: *SemanticAnalyzer, expr: *ast.Expr, owner: TypeRef, name: []const u8, receiver: ?*const ast.Expr) SemanticError!?@import("../semantic.zig").StructMethodInfo {
    const methods = (try self.methodsOf(owner)) orelse return null;
    const method = methods.get(name) orelse return null;
    const reachable = if (receiver) |r|
        r.data == .This
    else if (self.enclosing) |enclosing|
        enclosing.ref.eql(owner)
    else
        false;
    if (!method.is_public and !reachable) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.PRIVATE_FIELD_ACCESS,
            "Cannot call private {s} '{s}' of struct '{s}' outside the struct",
            .{ if (method.is_static) "function" else "method", name, owner.name },
        );
        self.fatal_error = true;
        return null;
    }
    return method;
}
