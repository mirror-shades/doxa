const std = @import("std");
const testing = std.testing;
const catalog = @import("../src/stdlib/catalog.zig");

const SAMPLE =
    \\module error from "../error.doxa"
    \\
    \\# Fetches a URL and returns the response.
    \\public function get(url :: string, timeout :: int) returns Response | error.StdError {
    \\    return performGet(url, timeout)
    \\}
    \\
    \\public function request(verb :: string, url :: string, ^req :: Request) returns Response | error.StdError {
    \\    return perform(verb, url, req)
    \\}
    \\
    \\public const version is "1.0"
    \\
    \\public struct Node {
    \\    handle :: int,
    \\
    \\    /* The node's kind. */
    \\    public method kind() returns Kind {
    \\        return Kind.Invalid
    \\    }
    \\}
    \\
    \\public enum Kind {
    \\    Invalid,
    \\    Null,
    \\}
    \\
    \\zig Helper {
    \\    // A brace-looking string must not confuse the scanner: "{"
    \\    pub fn check() void {
    \\        _ = "{";
    \\    }
    \\}
    \\
    \\public function afterZig() returns nothing {
    \\}
    \\
;

fn findDecl(decls: []const catalog.Decl, short_name: []const u8) ?*const catalog.Decl {
    for (decls) |*decl| {
        if (std.mem.eql(u8, decl.short_name, short_name)) return decl;
    }
    return null;
}

test "catalog parses functions, params, aliases, and returns" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const decls = try catalog.parseSource(arena.allocator(), SAMPLE);

    const get = findDecl(decls, "get") orelse return error.MissingDecl;
    try testing.expectEqual(catalog.Kind.function, get.kind);
    try testing.expectEqual(@as(usize, 2), get.params.len);
    try testing.expectEqualStrings("url", get.params[0].name);
    try testing.expectEqualStrings("string", get.params[0].type_text);
    try testing.expect(!get.params[0].alias);
    try testing.expectEqualStrings("Response | error.StdError", get.return_type.?);
    try testing.expectEqualStrings("Fetches a URL and returns the response.", get.doc.?);

    const request = findDecl(decls, "request") orelse return error.MissingDecl;
    try testing.expectEqual(@as(usize, 3), request.params.len);
    try testing.expect(request.params[2].alias);
    try testing.expectEqualStrings("req", request.params[2].name);
    try testing.expectEqualStrings("Request", request.params[2].type_text);

    const version = findDecl(decls, "version") orelse return error.MissingDecl;
    try testing.expectEqual(catalog.Kind.constant, version.kind);

    // Inline Zig blocks are skipped, so declarations after one are still found.
    try testing.expect(findDecl(decls, "afterZig") != null);
}

test "catalog captures struct members and enum variants" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const decls = try catalog.parseSource(arena.allocator(), SAMPLE);

    const node = findDecl(decls, "Node") orelse return error.MissingDecl;
    try testing.expectEqual(catalog.Kind.structure, node.kind);
    try testing.expectEqual(@as(usize, 1), node.members.len);
    try testing.expectEqualStrings("Node.kind", node.members[0].name);
    try testing.expectEqualStrings("kind", node.members[0].short_name);
    try testing.expectEqualStrings("The node's kind.", node.members[0].doc.?);

    const kind = findDecl(decls, "Kind") orelse return error.MissingDecl;
    try testing.expectEqual(catalog.Kind.enumeration, kind.kind);
    try testing.expectEqual(@as(usize, 2), kind.variants.len);
    try testing.expectEqualStrings("Invalid", kind.variants[0]);
    try testing.expectEqualStrings("Null", kind.variants[1]);
}

test "signatureDetail renders a compact signature" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const decls = try catalog.parseSource(allocator, SAMPLE);
    const get = findDecl(decls, "get") orelse return error.MissingDecl;
    const detail = try catalog.signatureDetail(allocator, get);
    try testing.expectEqualStrings("get(url: string, timeout: int) -> Response | error.StdError", detail);

    const request = findDecl(decls, "request") orelse return error.MissingDecl;
    const request_detail = try catalog.signatureDetail(allocator, request);
    try testing.expectEqualStrings("request(verb: string, url: string, ^req: Request) -> Response | error.StdError", request_detail);
}

test "parameterLabel and callSnippet render editor metadata" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const decls = try catalog.parseSource(allocator, SAMPLE);
    const get = findDecl(decls, "get") orelse return error.MissingDecl;

    const first = try catalog.parameterLabel(allocator, &get.params[0]);
    try testing.expectEqualStrings("url: string", first);

    const snippet = try catalog.callSnippet(allocator, get);
    try testing.expectEqualStrings("get($1, $2)", snippet);

    const request = findDecl(decls, "request") orelse return error.MissingDecl;
    const alias = try catalog.parameterLabel(allocator, &request.params[2]);
    try testing.expectEqualStrings("^req: Request", alias);

    const request_snippet = try catalog.callSnippet(allocator, request);
    try testing.expectEqualStrings("request($1, $2, $3)", request_snippet);

    const kind = findDecl(decls, "Kind") orelse return error.MissingDecl;
    const no_args = try catalog.callSnippet(allocator, kind);
    try testing.expectEqualStrings("Kind()", no_args);
}
