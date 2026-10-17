//! Hover payloads for intrinsics, user callables, and the standard library.

const std = @import("std");
const stdlib = @import("../stdlib/catalog.zig");
const InternalMethods = @import("internal_methods.zig");
const model = @import("model.zig");
const render = @import("render.zig");
const text = @import("text.zig");
const transport = @import("transport.zig");

pub fn buildHoverPayload(env: model.FeatureContext, id: model.JsonValue, params: ?model.JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(env.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try transport.writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":");

    if (text.extractDocumentContext(env, params)) |ctx| {
        if (text.findMethodRange(ctx.text, ctx.offset)) |method_range| {
            const name = ctx.text[method_range.start..method_range.end];
            if (InternalMethods.find(name)) |info| {
                const start_pos = text.offsetToPosition(ctx.text, method_range.start);
                const end_pos = text.offsetToPosition(ctx.text, method_range.end);

                var markdown = std.Io.Writer.Allocating.init(env.allocator);
                defer markdown.deinit();
                const md = &markdown.writer;
                const sig = try InternalMethods.signature(env.allocator, info);
                defer env.allocator.free(sig);
                try md.writeAll("```doxa\n");
                try md.writeAll(sig);
                try md.writeAll("\n```");
                const prose = InternalMethods.documentation(info);
                if (prose.len > 0) {
                    try md.writeAll("\n\n");
                    try md.writeAll(prose);
                }

                try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
                try transport.writeJsonValue(writer, markdown.written());
                try writer.writeAll("},\"range\":{\"start\":{\"line\":");
                try writer.print("{d}", .{start_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{start_pos.character});
                try writer.writeAll("},\"end\":{\"line\":");
                try writer.print("{d}", .{end_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{end_pos.character});
                try writer.writeAll("}}}}");

                return buffer.toOwnedSlice();
            }
        }

        if (text.findDottedPathAtOffset(ctx.text, ctx.offset)) |path| {
            if (try buildStdlibHover(env, writer, ctx, path)) {
                return buffer.toOwnedSlice();
            }
            if (try buildUserHover(env, writer, ctx, path)) {
                return buffer.toOwnedSlice();
            }
        }

        const dot_ctx = text.computeCompletionAtOffset(ctx.text, ctx.offset);
        if (dot_ctx.kind == .Dot and dot_ctx.object_name != null and dot_ctx.object_name.?.len > 0) {
            if (try buildDotHover(env, writer, dot_ctx)) {
                try writer.writeAll("}");
                return buffer.toOwnedSlice();
            }
        }
    }

    try writer.writeAll("null");
    try writer.writeAll("}");
    return buffer.toOwnedSlice();
}

fn buildDotHover(
    env: model.FeatureContext,
    writer: *std.Io.Writer,
    comp_ctx: model.CompletionContext,
) !bool {
    const obj_name = comp_ctx.object_name.?;
    const type_name = model.resolveObjectType(env.index, obj_name) orelse return false;
    const ct = env.index.types.get(type_name) orelse return false;

    const member_text = comp_ctx.prefix;
    if (member_text.len == 0) return false;

    for (ct.fields) |field| {
        if (std.mem.eql(u8, field.name, member_text)) {
            var markdown = std.Io.Writer.Allocating.init(env.allocator);
            defer markdown.deinit();
            const md = &markdown.writer;
            try md.writeAll("`");
            try md.writeAll(field.name);
            try md.writeAll(" : ");
            try md.writeAll(field.type_name);
            try md.writeAll("`");

            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
            try transport.writeJsonValue(writer, markdown.written());
            try writer.writeAll("}}");
            return true;
        }
    }
    for (ct.methods) |method| {
        if (std.mem.eql(u8, method.name, member_text)) {
            var markdown = std.Io.Writer.Allocating.init(env.allocator);
            defer markdown.deinit();
            const md = &markdown.writer;
            try md.writeAll("```doxa\n");
            if (method.is_static) try md.writeAll("static ");
            const sig = try render.methodSignature(env.allocator, method);
            defer env.allocator.free(sig);
            try md.writeAll(sig);
            try md.writeAll("\n```");

            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
            try transport.writeJsonValue(writer, markdown.written());
            try writer.writeAll("}}");
            return true;
        }
    }
    for (ct.enum_variants) |variant| {
        if (std.mem.eql(u8, variant, member_text)) {
            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":\"`enum `");
            try transport.writeJsonValue(writer, type_name);
            try writer.writeAll(".");
            try transport.writeJsonValue(writer, variant);
            try writer.writeAll("`\"}}");
            return true;
        }
    }
    return false;
}

/// Hover for a user-defined function or struct method, rendering its signature.
fn buildUserHover(env: model.FeatureContext, writer: *std.Io.Writer, ctx: model.DocumentContext, path: text.DottedPath) !bool {
    var signature: ?[]u8 = null;
    defer {
        if (signature) |sig| env.allocator.free(sig);
    }

    if (env.index.findCallable(path.text)) |callable| {
        signature = try render.callableSignature(env.allocator, callable);
    } else if (std.mem.lastIndexOfScalar(u8, path.text, '.')) |dot| {
        const object = path.text[0..dot];
        const method_name = path.text[dot + 1 ..];
        if (model.resolveObjectType(env.index, object)) |type_name| {
            if (env.index.findMethod(type_name, method_name)) |method| {
                signature = try render.methodSignature(env.allocator, method.*);
            }
        }
    }

    const sig = signature orelse return false;
    const start_pos = text.offsetToPosition(ctx.text, path.start);
    const end_pos = text.offsetToPosition(ctx.text, path.end);

    var markdown = std.Io.Writer.Allocating.init(env.allocator);
    defer markdown.deinit();
    try markdown.writer.writeAll("```doxa\n");
    try markdown.writer.writeAll(sig);
    try markdown.writer.writeAll("\n```");

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try transport.writeJsonValue(writer, markdown.written());
    try writer.writeAll("},\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start_pos.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end_pos.character});
    try writer.writeAll("}}}}");
    return true;
}

fn writeDocBlock(writer: *std.Io.Writer, decl: *const stdlib.Decl) !void {
    try writer.writeAll("```doxa\n");
    try writer.writeAll(decl.signature);
    try writer.writeAll("\n```");
    if (decl.doc) |doc| {
        if (doc.len > 0) {
            try writer.writeAll("\n\n");
            try writer.writeAll(doc);
        }
    }
}

fn buildStdlibHover(env: model.FeatureContext, writer: *std.Io.Writer, ctx: model.DocumentContext, path: text.DottedPath) !bool {
    const cat = env.stdlib orelse return false;

    var markdown = std.Io.Writer.Allocating.init(env.allocator);
    defer markdown.deinit();
    const md = &markdown.writer;

    if (std.mem.eql(u8, path.text, "std")) {
        try md.writeAll("The Doxa standard library. Complete a module, e.g. `std.io`.");
    } else {
        var segments: [8][]const u8 = undefined;
        var count: usize = 0;
        var it = std.mem.splitScalar(u8, path.text, '.');
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
            try md.print("`module std.{s}`", .{module.name});
        } else {
            const owner = module.find(segments[start + 1]) orelse return false;
            if (remaining == 2) {
                try writeDocBlock(md, owner);
            } else {
                const member_name = segments[start + 2];
                if (owner.findMember(member_name)) |member| {
                    try writeDocBlock(md, member);
                } else {
                    var found = false;
                    for (owner.variants) |variant| {
                        if (std.mem.eql(u8, variant, member_name)) {
                            try md.print("`{s}.{s}` — member of enum `{s}`", .{ module.name, owner.short_name, member_name });
                            found = true;
                            break;
                        }
                    }
                    if (!found) return false;
                }
            }
        }
    }

    const start_pos = text.offsetToPosition(ctx.text, path.start);
    const end_pos = text.offsetToPosition(ctx.text, path.end);

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try transport.writeJsonValue(writer, markdown.written());
    try writer.writeAll("},\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start_pos.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end_pos.character});
    try writer.writeAll("}}}}");
    return true;
}
