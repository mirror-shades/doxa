//! Completion payloads and item rendering.

const std = @import("std");
const Types = @import("../types/types.zig");
const stdlib = @import("../stdlib/catalog.zig");
const InternalMethods = @import("internal_methods.zig");
const model = @import("model.zig");
const render = @import("render.zig");
const transport = @import("transport.zig");

pub fn buildCompletionPayload(env: model.FeatureContext, id: model.JsonValue, ctx: model.CompletionContext) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(env.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try transport.writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");

    switch (ctx.kind) {
        .Intrinsic => try writeIntrinsicCompletions(env.allocator, writer, ctx.prefix, ctx.edit()),
        .Dot => try writeMemberCompletions(env, writer, ctx, ctx.edit()),
        .Word => try writeWordCompletions(env, writer, ctx.prefix, ctx.edit()),
        .None => {},
    }

    try writer.writeAll("]}}");
    return buffer.toOwnedSlice();
}

/// Emits the explicit replacement range and text for an item. Relying on the
/// client's word boundaries would re-insert the `@` of an intrinsic that the
/// user already typed.
fn writeTextEdit(writer: *std.Io.Writer, edit: model.CompletionEdit, new_text: []const u8) !void {
    try writer.writeAll(",\"textEdit\":{\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{edit.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{edit.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.end.character});
    try writer.writeAll("}},\"newText\":");
    try transport.writeJsonValue(writer, new_text);
    try writer.writeAll("}");
}

pub fn writeIntrinsicCompletions(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    var first = true;
    for (InternalMethods.all()) |*info| {
        const label = try std.fmt.allocPrint(allocator, "@{s}", .{info.name});
        defer allocator.free(label);
        if (!isMemberMatch(label, prefix)) continue;

        const detail = try InternalMethods.signature(allocator, info);
        defer allocator.free(detail);
        const snippet = try InternalMethods.snippet(allocator, info);
        defer allocator.free(snippet);

        if (!first) try writer.writeAll(",");
        first = false;
        try writer.writeAll("{\"label\":");
        try transport.writeJsonValue(writer, label);
        try writer.writeAll(",\"kind\":3,\"detail\":");
        try transport.writeJsonValue(writer, detail);
        const doc = InternalMethods.documentation(info);
        if (doc.len > 0) {
            try writer.writeAll(",\"documentation\":");
            try transport.writeJsonValue(writer, doc);
        }
        try writer.writeAll(",\"insertTextFormat\":2");
        try writeTextEdit(writer, edit, snippet);
        try writer.writeAll("}");
    }
}

pub fn writeMemberCompletions(
    env: model.FeatureContext,
    writer: *std.Io.Writer,
    ctx: model.CompletionContext,
    edit: model.CompletionEdit,
) !void {
    const obj_name = ctx.object_name orelse return;
    if (obj_name.len == 0) return;

    // The standard-library catalog takes precedence: it knows the whole public
    // surface, not just what the open document happens to import.
    if (env.stdlib) |cat| {
        if (try writeStdlibCompletions(cat, env.allocator, writer, obj_name, ctx.prefix, edit)) return;
    }

    const index = env.index;
    if (index.module_members.get(obj_name)) |members| {
        var first = true;
        for (members.items) |member| {
            if (!isMemberMatch(member, ctx.prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeModuleMemberCompletionItem(writer, member, edit);
        }
        return;
    }

    const type_name = model.resolveObjectType(index, obj_name) orelse return;
    if (index.types.contains(type_name)) {
        try writeTypeMembers(env.allocator, index, writer, type_name, ctx.prefix, edit);
        return;
    }

    // A value whose declared type is a standard-library struct/enum. The
    // inferred name can be a union (`Response | error.StdError`), so try the
    // whole string and then each alternative.
    if (env.stdlib) |cat| {
        if (model.findCatalogType(cat, type_name)) |owner| {
            try writeCatalogTypeMembers(env.allocator, writer, owner, ctx.prefix, edit);
            return;
        }
        var alternatives = std.mem.splitSequence(u8, type_name, " | ");
        while (alternatives.next()) |alternative| {
            const trimmed = std.mem.trim(u8, alternative, " \t");
            if (model.findCatalogType(cat, trimmed)) |owner| {
                try writeCatalogTypeMembers(env.allocator, writer, owner, ctx.prefix, edit);
                return;
            }
        }
    }
}

/// Completes the symbols in scope for a bare identifier: the standard library
/// root, imported modules, declarations in the document, in-scope variables,
/// and custom types. Duplicate labels are collapsed.
pub fn writeWordCompletions(
    env: model.FeatureContext,
    writer: *std.Io.Writer,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    var seen = std.StringHashMap(void).init(env.allocator);
    defer seen.deinit();

    var first = true;

    try writeWordItem(writer, &seen, &first, "std", 9, prefix, edit);

    var module_it = env.index.modules.iterator();
    while (module_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, 9, prefix, edit);
    }

    // Top-level functions get a signature and an argument snippet; methods
    // (qualified `Type.method`) are offered through member completion instead.
    var callable_it = env.index.callables.iterator();
    while (callable_it.next()) |entry| {
        const callable = entry.value_ptr.*;
        if (std.mem.indexOfScalar(u8, callable.qualified, '.') != null) continue;
        try writeCallableWordItem(env.allocator, writer, &seen, &first, callable, prefix, edit);
    }

    for (env.index.symbols.items) |sym| {
        try writeWordItem(writer, &seen, &first, sym.name, completionKindForSymbol(sym.kind), prefix, edit);
    }

    var var_it = env.index.variables.iterator();
    while (var_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, 6, prefix, edit);
    }

    var type_it = env.index.types.iterator();
    while (type_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, completionKindForCustomType(entry.value_ptr.kind), prefix, edit);
    }
}

fn writeWordItem(
    writer: *std.Io.Writer,
    seen: *std.StringHashMap(void),
    first: *bool,
    name: []const u8,
    kind: u32,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    if (name.len == 0) return;
    if (!isMemberMatch(name, prefix)) return;

    const gop = try seen.getOrPut(name);
    if (gop.found_existing) return;

    if (!first.*) try writer.writeAll(",");
    first.* = false;
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, name);
    try writer.print(",\"kind\":{d}", .{kind});
    try writeTextEdit(writer, edit, name);
    try writer.writeAll("}");
}

fn writeCallableWordItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    seen: *std.StringHashMap(void),
    first: *bool,
    callable: *const model.CachedCallable,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    if (!isMemberMatch(callable.name, prefix)) return;

    const gop = try seen.getOrPut(callable.name);
    if (gop.found_existing) return;

    const detail = try render.callableSignature(allocator, callable);
    defer allocator.free(detail);
    const snippet = try render.callableSnippet(allocator, callable);
    defer allocator.free(snippet);

    if (!first.*) try writer.writeAll(",");
    first.* = false;
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, callable.name);
    try writer.writeAll(",\"kind\":3,\"detail\":");
    try transport.writeJsonValue(writer, detail);
    try writer.writeAll(",\"insertTextFormat\":2");
    try writeTextEdit(writer, edit, snippet);
    try writer.writeAll("}");
}

fn completionKindForSymbol(symbol_kind: u32) u32 {
    return switch (symbol_kind) {
        12 => 3, // Function
        13 => 6, // Variable
        10 => 13, // Enum
        23 => 22, // Struct / group
        2 => 9, // Module
        else => 6,
    };
}

fn completionKindForCustomType(kind: Types.CustomTypeKind) u32 {
    return switch (kind) {
        .Struct => 22,
        .Enum => 13,
        .Group => 22,
    };
}

/// Emits completions for a standard-library object path (`std`, `std.io`,
/// `std.io.print`, `std.json.Node`, ...). Returns true when the path resolved
/// to a catalog entity, even if the prefix filtered every item — the caller
/// must not then fall through to unrelated imported symbols.
pub fn writeStdlibCompletions(
    cat: *const stdlib.Catalog,
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    obj_name: []const u8,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !bool {
    if (std.mem.eql(u8, obj_name, "std")) {
        var first = true;
        for (cat.modules) |*module| {
            if (!isMemberMatch(module.name, prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeStdlibModuleItem(writer, module, edit);
        }
        return true;
    }

    var segments: [8][]const u8 = undefined;
    var count: usize = 0;
    var it = std.mem.splitScalar(u8, obj_name, '.');
    while (it.next()) |segment| {
        if (count >= segments.len) return false;
        segments[count] = segment;
        count += 1;
    }

    var start: usize = 0;
    if (count > 0 and std.mem.eql(u8, segments[0], "std")) start = 1;
    if (start >= count) return false;

    const module = cat.findModule(segments[start]) orelse return false;
    const remaining = count - start;

    if (remaining == 1) {
        var first = true;
        for (module.decls) |*decl| {
            if (!isMemberMatch(decl.short_name, prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeStdlibDeclItem(allocator, writer, decl, edit);
        }
        return true;
    }

    // `std.<module>.<Type>` — complete the type's members.
    const owner = module.find(segments[start + 1]) orelse return false;
    if (remaining == 2) {
        try writeCatalogTypeMembers(allocator, writer, owner, prefix, edit);
        return true;
    }

    return true;
}

/// Emits a catalog type's methods, associated functions, and enum variants.
fn writeCatalogTypeMembers(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    owner: *const stdlib.Decl,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    var first = true;
    for (owner.members) |*member| {
        if (!isMemberMatch(member.short_name, prefix)) continue;
        if (!first) try writer.writeAll(",");
        first = false;
        try writeStdlibDeclItem(allocator, writer, member, edit);
    }
    for (owner.variants) |variant| {
        if (!isMemberMatch(variant, prefix)) continue;
        if (!first) try writer.writeAll(",");
        first = false;
        try writeStdlibVariantItem(writer, owner.short_name, variant, edit);
    }
}

fn writeStdlibModuleItem(writer: *std.Io.Writer, module: *const stdlib.Module, edit: model.CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, module.name);
    try writer.writeAll(",\"kind\":9,\"detail\":");
    var buf: [128]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "module std.{s}", .{module.name}) catch "module";
    try transport.writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, module.name);
    try writer.writeAll("}");
}

fn writeStdlibDeclItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    decl: *const stdlib.Decl,
    edit: model.CompletionEdit,
) !void {
    const detail = try stdlib.signatureDetail(allocator, decl);
    defer allocator.free(detail);

    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, decl.short_name);
    try writer.print(",\"kind\":{d},\"detail\":", .{completionKindFor(decl.kind)});
    try transport.writeJsonValue(writer, detail);
    if (decl.doc) |doc| {
        if (doc.len > 0) {
            try writer.writeAll(",\"documentation\":");
            try transport.writeJsonValue(writer, doc);
        }
    }
    if (decl.kind == .function or decl.kind == .method) {
        const snippet = try stdlib.callSnippet(allocator, decl);
        defer allocator.free(snippet);
        try writer.writeAll(",\"insertTextFormat\":2");
        try writeTextEdit(writer, edit, snippet);
    } else {
        try writeTextEdit(writer, edit, decl.short_name);
    }
    try writer.writeAll("}");
}

fn writeStdlibVariantItem(
    writer: *std.Io.Writer,
    owner: []const u8,
    variant: []const u8,
    edit: model.CompletionEdit,
) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, variant);
    try writer.writeAll(",\"kind\":20,\"detail\":");
    var buf: [256]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{s}.{s}", .{ owner, variant }) catch "enum member";
    try transport.writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, variant);
    try writer.writeAll("}");
}

fn completionKindFor(kind: stdlib.Kind) u32 {
    return switch (kind) {
        .function => 3,
        .method => 2,
        .structure => 22,
        .enumeration => 13,
        .group => 22,
        .constant => 21,
        .variable => 6,
        .module => 9,
    };
}

fn writeTypeMembers(
    allocator: std.mem.Allocator,
    index: *const model.SymbolIndex,
    writer: *std.Io.Writer,
    type_name: []const u8,
    prefix: []const u8,
    edit: model.CompletionEdit,
) !void {
    var first = true;

    if (index.types.get(type_name)) |ct| {
        switch (ct.kind) {
            .Struct => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field, edit);
                }
                for (ct.methods) |method| {
                    if (!isMemberMatch(method.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeMethodCompletionItem(allocator, writer, method, edit);
                }
            },
            .Enum => {
                for (ct.enum_variants) |variant| {
                    if (!isMemberMatch(variant, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeEnumCompletionItem(writer, variant, edit);
                }
            },
            .Group => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field, edit);
                }
            },
        }
    }
}

fn isMemberMatch(name: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return true;
    return std.mem.startsWith(u8, name, prefix);
}

fn writeFieldCompletionItem(writer: *std.Io.Writer, field: model.CachedField, edit: model.CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, field.name);
    try writer.writeAll(",\"kind\":5");
    try writer.writeAll(",\"detail\":");
    var buf: [256]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{s}", .{field.type_name}) catch field.type_name;
    try transport.writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, field.name);
    try writer.writeAll("}");
}

fn writeMethodCompletionItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    method: model.CachedMethod,
    edit: model.CompletionEdit,
) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, method.name);
    try writer.writeAll(",\"kind\":2");
    try writer.writeAll(",\"detail\":");

    const has_signature = method.params.len > 0 or method.return_type.len > 0;
    if (has_signature) {
        const sig = try render.methodSignature(allocator, method);
        defer allocator.free(sig);
        try transport.writeJsonValue(writer, sig);
    } else {
        try transport.writeJsonValue(writer, if (method.is_static) "static method" else "method");
    }

    const snippet = try render.methodSnippet(allocator, method);
    defer allocator.free(snippet);
    try writer.writeAll(",\"insertTextFormat\":2");
    try writeTextEdit(writer, edit, snippet);
    try writer.writeAll("}");
}

fn writeEnumCompletionItem(writer: *std.Io.Writer, variant: []const u8, edit: model.CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, variant);
    try writer.writeAll(",\"kind\":13");
    try writer.writeAll(",\"detail\":");
    try transport.writeJsonValue(writer, "enum member");
    try writeTextEdit(writer, edit, variant);
    try writer.writeAll("}");
}

fn writeModuleMemberCompletionItem(writer: *std.Io.Writer, name: []const u8, edit: model.CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try transport.writeJsonValue(writer, name);
    try writer.writeAll(",\"kind\":9");
    try writer.writeAll(",\"detail\":");
    try transport.writeJsonValue(writer, "module export");
    try writeTextEdit(writer, edit, name);
    try writer.writeAll("}");
}
