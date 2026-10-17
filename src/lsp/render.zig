//! Rendering Doxa types, signatures, and snippets as editor text.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const model = @import("model.zig");
const snippet = @import("../common/snippet.zig");

pub fn packLspTypeString(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |ct| {
        return ct.displayName();
    }
    return @tagName(type_info.base);
}

pub fn typeBaseLabel(base: ast.Type) []const u8 {
    return switch (base) {
        .Int => "int",
        .Byte => "byte",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Array => "array",
        .Function => "function",
        .Struct => "struct",
        .Enum => "enum",
        .Custom => "custom",
        .Nothing => "nothing",
        .Union => "union",
        .Error => "error",
    };
}

fn basicTypeLabel(basic: ast.BasicType) []const u8 {
    return switch (basic) {
        .Integer => "int",
        .Byte => "byte",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Nothing => "nothing",
    };
}

/// Renders a syntactic parameter type (`^name :: string | error.Foo`) the way
/// it is written in source. Falls back to `any` for an untyped parameter.
fn writeTypeExpr(writer: *std.Io.Writer, type_expr: ?*const ast.TypeExpr) !void {
    const expr = type_expr orelse {
        try writer.writeAll("any");
        return;
    };
    switch (expr.data) {
        .Basic => |basic| try writer.writeAll(basicTypeLabel(basic)),
        .Custom => |custom| try writer.writeAll(custom.name.lexeme),
        .Array => |array| {
            try writeTypeExpr(writer, array.element_type);
            try writer.writeAll("[]");
        },
        .Struct => try writer.writeAll("struct"),
        .Union => |types| {
            for (types, 0..) |t, i| {
                if (i > 0) try writer.writeAll(" | ");
                try writeTypeExpr(writer, t);
            }
        },
    }
}

pub fn renderTypeExpr(allocator: std.mem.Allocator, type_expr: ?*const ast.TypeExpr) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    try writeTypeExpr(&out.writer, type_expr);
    return out.toOwnedSlice();
}

pub fn renderTypeInfo(allocator: std.mem.Allocator, type_info: ast.TypeInfo) ![]u8 {
    if (type_info.custom_type) |ct| return allocator.dupe(u8, ct.displayName());
    return allocator.dupe(u8, typeBaseLabel(type_info.base));
}

/// A one-line signature for a user callable: `foo(a: int, ^b: string) -> bool`.
pub fn callableSignature(allocator: std.mem.Allocator, callable: *const model.CachedCallable) ![]u8 {
    return signatureFor(allocator, callable.name, callable.params, callable.return_type);
}

fn signatureFor(
    allocator: std.mem.Allocator,
    name: []const u8,
    params: []const model.CachedParam,
    return_type: []const u8,
) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{name});
    for (params, 0..) |param, i| {
        if (i > 0) try w.writeAll(", ");
        try writeCachedParam(w, param);
    }
    try w.writeAll(")");
    if (return_type.len > 0) {
        try w.print(" -> {s}", .{return_type});
    }
    return out.toOwnedSlice();
}

fn writeCachedParam(writer: *std.Io.Writer, param: model.CachedParam) !void {
    if (param.alias) try writer.writeAll("^");
    if (param.name.len > 0) {
        try writer.print("{s}: ", .{param.name});
    }
    try writer.writeAll(param.type_text);
}

pub fn cachedParamLabel(allocator: std.mem.Allocator, param: model.CachedParam) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    try writeCachedParam(&out.writer, param);
    return out.toOwnedSlice();
}

pub fn callableSnippet(allocator: std.mem.Allocator, callable: *const model.CachedCallable) ![]u8 {
    return snippetFor(allocator, callable.name, callable.params.len);
}

fn snippetFor(allocator: std.mem.Allocator, name: []const u8, param_count: usize) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    try snippet.write(&out.writer, name, param_count);
    return out.toOwnedSlice();
}

pub fn methodSignature(allocator: std.mem.Allocator, method: model.CachedMethod) ![]u8 {
    return signatureFor(allocator, method.name, method.params, method.return_type);
}

pub fn methodSnippet(allocator: std.mem.Allocator, method: model.CachedMethod) ![]u8 {
    return snippetFor(allocator, method.name, method.params.len);
}

pub fn inlayTypeText(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |ct| return ct.displayName();
    return typeBaseLabel(type_info.base);
}
