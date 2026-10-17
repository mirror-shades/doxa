//! Type rules for field access, field stores, and struct literals.

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
const member_mod = @import("member.zig");

pub fn fieldAccess(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const field = expr.data.FieldAccess;
    // A member of a namespace: `ns.fn`, `ns.global`, `ns.Type`.
    if (try names.namespaceOf(self, field.object)) |namespace| {
        _ = try member_mod.memberOfNamespace(self, expr, namespace, field.field, type_info);
        return;
    }

    const object_type = try infer_type.inferTypeFromExpr(self, field.object);
    if (object_type.base == .Error) return;
    switch (object_type.base) {
        .Union => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                "Cannot access field '{s}' on union type; narrow it with 'as' or match first",
                .{field.field.lexeme},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        .Custom => {
            const ref = if (object_type.custom_type) |custom| custom.resolved() else {
                // An anonymous enum value carries no named type.
                type_info.* = .{ .base = .Enum };
                return;
            };
            const custom = (try self.customType(ref)) orelse {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.TYPE_MISMATCH, "Undefined type '{s}'", .{ref.name});
                self.fatal_error = true;
                type_info.base = .Nothing;
                return;
            };
            switch (custom.kind) {
                .Struct => {
                    const struct_field = findField(custom.struct_fields orelse &.{}, field.field.lexeme) orelse {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.FIELD_NOT_FOUND,
                            "Field '{s}' not found in struct '{s}'",
                            .{ field.field.lexeme, ref.name },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return;
                    };
                    // A private field is read only through `this`.
                    if (!struct_field.is_public and field.object.data != .This) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.PRIVATE_FIELD_ACCESS,
                            "Cannot access private field '{s}' of struct '{s}'; a private field is reached only through `this`",
                            .{ struct_field.name, ref.name },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return;
                    }
                    type_info.* = struct_field.field_type_info.*;
                },
                // `Color.Red`, `Group.Member`: the qualifier names the
                // value's type.
                .Enum, .Group => type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref } },
            }
        },
        .Struct => {
            const fields = object_type.struct_fields orelse &.{};
            for (fields) |struct_field| {
                if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                    type_info.* = struct_field.type_info.*;
                    return;
                }
            }
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.FIELD_NOT_FOUND,
                "Field '{s}' not found in struct",
                .{field.field.lexeme},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        .Enum => type_info.* = .{ .base = .Custom, .custom_type = object_type.custom_type },
        else => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                "Cannot access field on non-struct type {s}",
                .{@tagName(object_type.base)},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
    }
}

pub fn fieldAssignment(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const field_assign = expr.data.FieldAssignment;
    try borrows.check(self, field_assign.object, expr, .write);
    const object_type = try infer_type.inferTypeFromExpr(self, field_assign.object);
    try effects.write(self, field_assign.object);
    const value_type = try infer_type.inferTypeFromExpr(self, field_assign.value);

    const fields: []const ast.StructFieldType = switch (object_type.base) {
        .Struct => object_type.struct_fields orelse &.{},
        .Custom => blk: {
            const ref = (object_type.custom_type orelse break :blk &.{}).resolved();
            const custom = (try self.customType(ref)) orelse break :blk &.{};
            if (custom.kind != .Struct) break :blk &.{};
            break :blk try structFieldTypes(self, custom.struct_fields orelse &.{});
        },
        else => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                "Cannot assign to field of non-struct type {s}",
                .{@tagName(object_type.base)},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        },
    };

    for (fields) |struct_field| {
        if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
            // A private field is written only through `this`.
            if (!struct_field.is_public and field_assign.object.data != .This) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.PRIVATE_FIELD_ACCESS,
                    "Cannot assign private field '{s}'; a private field is reached only through `this`",
                    .{struct_field.name},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return;
            }
            try helpers.unifyTypesExpr(self, struct_field.type_info, value_type, field_assign.value, .{ .location = getLocationFromBase(expr.base) });
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
        return;
    }

    type_info.* = .{ .base = .Nothing };
}

pub fn structLiteral(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const struct_lit = expr.data.StructLiteral;
    const ref = (try names.resolveTypeName(self, struct_lit.name.lexeme, self.current_module)) orelse {
        self.reporter.reportCompileError(
            ast.SourceSpan.fromToken(struct_lit.name).location,
            ErrorCode.UNKNOWN_TYPE,
            "Unknown type '{s}'",
            .{struct_lit.name.lexeme},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    };
    try names.annotate(self, expr, .{ .type = ref });
    const custom = (try self.customType(ref)) orelse return error.UndefinedType;
    if (custom.kind != .Struct) {
        self.reporter.reportCompileError(
            ast.SourceSpan.fromToken(struct_lit.name).location,
            ErrorCode.TYPE_MISMATCH,
            "'{s}' is not a struct",
            .{ref.name},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }
    const decl_fields = try structFieldTypes(self, custom.struct_fields orelse &.{});
    type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .struct_fields = decl_fields, .is_mutable = false };

    var unknown: usize = 0;
    for (struct_lit.fields, 0..) |lit_field, index| {
        // A literal sets each field once.
        for (struct_lit.fields[0..index]) |earlier| {
            if (!std.mem.eql(u8, earlier.name.lexeme, lit_field.name.lexeme)) continue;
            self.reporter.reportCompileError(
                ast.SourceSpan.fromToken(lit_field.name).location,
                ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                "struct '{s}' literal sets field '{s}' twice",
                .{ ref.name, lit_field.name.lexeme },
            );
            self.fatal_error = true;
            unknown += 1;
            break;
        } else for (decl_fields) |decl_field| {
            if (!std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) continue;
            // A private field is set only by the struct itself.
            const inside = if (self.enclosing) |enclosing| enclosing.ref.eql(ref) else false;
            if (!decl_field.is_public and !inside) {
                self.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(lit_field.name).location,
                    ErrorCode.PRIVATE_FIELD_ACCESS,
                    "Cannot set private field '{s}' of struct '{s}' outside the struct",
                    .{ decl_field.name, ref.name },
                );
                self.fatal_error = true;
            }
            const lit_type = try infer_type.inferTypeFromExpr(self, lit_field.value);
            try helpers.unifyTypesExpr(self, decl_field.type_info, lit_type, lit_field.value, ast.SourceSpan.fromToken(lit_field.name));
            break;
        } else {
            const declared_list = try declaredFieldList(self, decl_fields);
            defer self.allocator.free(declared_list);
            self.reporter.reportCompileError(
                ast.SourceSpan.fromToken(lit_field.name).location,
                ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                "struct '{s}' has no field '{s}'; declared fields: {s}",
                .{ ref.name, lit_field.name.lexeme, declared_list },
            );
            self.fatal_error = true;
            unknown += 1;
        }
    }
    // A field the literal misnames already explains a wrong count.
    if (unknown == 0 and decl_fields.len != struct_lit.fields.len) {
        self.reporter.reportCompileError(
            ast.SourceSpan.fromToken(struct_lit.name).location,
            ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
            "struct '{s}' expects {d} field{s}, but this literal provides {d}",
            .{ ref.name, decl_fields.len, if (decl_fields.len == 1) "" else "s", struct_lit.fields.len },
        );
        self.fatal_error = true;
    }
}

/// Join declared struct field names into a human-readable list for error
/// messages, e.g. `name, entry_point, output`.
pub fn declaredFieldList(self: *SemanticAnalyzer, fields: []const ast.StructFieldType) ![]u8 {
    var list = std.array_list.Managed(u8).init(self.allocator);
    errdefer list.deinit();
    for (fields, 0..) |field, i| {
        if (i > 0) try list.appendSlice(", ");
        try list.appendSlice(field.name);
    }
    if (fields.len == 0) try list.appendSlice("(none)");
    return list.toOwnedSlice();
}

pub fn findField(fields: []const @import("../../../types/types.zig").StructField, name: []const u8) ?@import("../../../types/types.zig").StructField {
    for (fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field;
    }
    return null;
}

/// A registered struct's fields in the shape a `TypeInfo` carries them.
pub fn structFieldTypes(self: *SemanticAnalyzer, fields: []const @import("../../../types/types.zig").StructField) SemanticError![]ast.StructFieldType {
    const out = try self.allocator.alloc(ast.StructFieldType, fields.len);
    for (fields, out) |field, *dest| {
        dest.* = .{ .name = field.name, .type_info = field.field_type_info, .is_public = field.is_public };
    }
    return out;
}
