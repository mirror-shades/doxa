//! Signature help: resolving a call site to its declaration and serializing
//! the active parameter.

const std = @import("std");
const stdlib = @import("../stdlib/catalog.zig");
const InternalMethods = @import("internal_methods.zig");
const model = @import("model.zig");
const render = @import("render.zig");
const text = @import("text.zig");
const transport = @import("transport.zig");

const ResolvedSignature = struct {
    label: []u8,
    params: [][]u8,
    variadic: bool,

    fn deinit(self: ResolvedSignature, allocator: std.mem.Allocator) void {
        allocator.free(self.label);
        for (self.params) |p| allocator.free(p);
        allocator.free(self.params);
    }
};

fn catalogResolved(allocator: std.mem.Allocator, decl: *const stdlib.Decl) !ResolvedSignature {
    var params = std.array_list.Managed([]u8).init(allocator);
    errdefer {
        for (params.items) |p| allocator.free(p);
        params.deinit();
    }
    for (decl.params) |*param| {
        try params.append(try stdlib.parameterLabel(allocator, param));
    }
    return .{
        .label = try stdlib.signatureDetail(allocator, decl),
        .params = try params.toOwnedSlice(),
        .variadic = false,
    };
}

fn resolvedSignature(
    allocator: std.mem.Allocator,
    params: []const model.CachedParam,
    label: []u8,
) !ResolvedSignature {
    var out = std.array_list.Managed([]u8).init(allocator);
    errdefer {
        for (out.items) |p| allocator.free(p);
        out.deinit();
        allocator.free(label);
    }
    for (params) |param| {
        try out.append(try render.cachedParamLabel(allocator, param));
    }
    return .{
        .label = label,
        .params = try out.toOwnedSlice(),
        .variadic = false,
    };
}

fn callableResolved(allocator: std.mem.Allocator, callable: *const model.CachedCallable) !ResolvedSignature {
    return resolvedSignature(allocator, callable.params, try render.callableSignature(allocator, callable));
}

fn methodResolved(allocator: std.mem.Allocator, method: model.CachedMethod) !ResolvedSignature {
    return resolvedSignature(allocator, method.params, try render.methodSignature(allocator, method));
}

/// Resolves a caller path (`std.io.print`, `io.print`, `json.Node.parse`) to
/// its catalog declaration.
fn resolveCatalogCall(cat: *const stdlib.Catalog, path: []const u8) ?*const stdlib.Decl {
    var segments: [8][]const u8 = undefined;
    var count: usize = 0;
    var it = std.mem.splitScalar(u8, path, '.');
    while (it.next()) |segment| {
        if (count >= segments.len) return null;
        segments[count] = segment;
        count += 1;
    }

    var start: usize = 0;
    if (count > 0 and std.mem.eql(u8, segments[0], "std")) start = 1;
    if (start >= count) return null;

    const module = cat.findModule(segments[start]) orelse return null;
    const remaining = count - start;
    if (remaining == 2) return module.find(segments[start + 1]);
    if (remaining == 3) {
        const owner = module.find(segments[start + 1]) orelse return null;
        return owner.findMember(segments[start + 2]);
    }
    return null;
}

pub fn buildSignatureHelpPayload(env: model.FeatureContext, id: model.JsonValue, params: ?model.JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(env.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try transport.writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":");

    var wrote = false;
    if (text.extractDocumentContext(env, params)) |ctx| {
        if (text.computeCallAtOffset(ctx.text, ctx.offset)) |call| {
            wrote = try writeSignatureHelp(env, writer, call);
        }
    }
    if (!wrote) try writer.writeAll("null");
    try writer.writeAll("}");
    return buffer.toOwnedSlice();
}

/// Resolves the callable behind a call site: an `@` intrinsic, a standard
/// library path, a user function, or a method on a value whose type is known.
fn resolveSignature(env: model.FeatureContext, callee: []const u8) !?ResolvedSignature {
    if (callee.len == 0) return null;

    if (callee[0] == '@') {
        const info = InternalMethods.find(callee) orelse return null;
        var params = std.array_list.Managed([]u8).init(env.allocator);
        errdefer {
            for (params.items) |p| env.allocator.free(p);
            params.deinit();
        }
        for (0..info.input_types.len) |idx| {
            try params.append(try InternalMethods.parameterLabel(env.allocator, info, idx));
        }
        return .{
            .label = try InternalMethods.signature(env.allocator, info),
            .params = try params.toOwnedSlice(),
            .variadic = info.arg_count_max == null,
        };
    }

    if (std.mem.lastIndexOfScalar(u8, callee, '.')) |dot| {
        const object = callee[0..dot];
        const method_name = callee[dot + 1 ..];

        if (env.stdlib) |cat| {
            if (resolveCatalogCall(cat, callee)) |decl| {
                if (decl.kind == .function or decl.kind == .method) {
                    return try catalogResolved(env.allocator, decl);
                }
            }
        }

        const type_name = model.resolveObjectType(env.index, object) orelse return null;
        if (env.index.findMethod(type_name, method_name)) |method| {
            return try methodResolved(env.allocator, method.*);
        }
        if (env.stdlib) |cat| {
            if (model.findCatalogType(cat, type_name)) |owner| {
                if (owner.findMember(method_name)) |member| {
                    if (member.kind == .method or member.kind == .function) {
                        return try catalogResolved(env.allocator, member);
                    }
                }
            }
        }
        return null;
    }

    if (env.index.findCallable(callee)) |callable| {
        return try callableResolved(env.allocator, callable);
    }
    return null;
}

pub fn writeSignatureHelp(env: model.FeatureContext, writer: *std.Io.Writer, call: text.CallContext) !bool {
    const resolved = (try resolveSignature(env, call.callee)) orelse return false;
    defer resolved.deinit(env.allocator);

    var active = call.active_parameter;
    const declared = resolved.params.len;
    if (!resolved.variadic and declared > 0 and active >= declared) active = declared - 1;

    try writer.writeAll("{\"signatures\":[{\"label\":");
    try transport.writeJsonValue(writer, resolved.label);
    try writer.writeAll(",\"parameters\":[");
    for (resolved.params, 0..) |p, idx| {
        if (idx > 0) try writer.writeAll(",");
        try writer.writeAll("{\"label\":");
        try transport.writeJsonValue(writer, p);
        try writer.writeAll("}");
    }
    try writer.writeAll("]}],\"activeSignature\":0,\"activeParameter\":");
    try writer.print("{d}", .{active});
    try writer.writeAll("}");
    return true;
}
