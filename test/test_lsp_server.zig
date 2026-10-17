const std = @import("std");
const reporting = @import("../src/utils/reporting.zig");
const stdlib = @import("../src/stdlib/catalog.zig");
const server_mod = @import("../src/lsp/server.zig");
const transport = @import("../src/lsp/transport.zig");
const model = @import("../src/lsp/model.zig");
const lsp_text = @import("../src/lsp/text.zig");
const lsp_completion = @import("../src/lsp/completion.zig");
const lsp_hover = @import("../src/lsp/hover.zig");
const lsp_signature = @import("../src/lsp/signature.zig");
const lsp_symbols = @import("../src/lsp/symbols.zig");

const Reporter = reporting.Reporter;
const CaptureSink = transport.CaptureSink;
const Document = model.Document;
const CachedField = model.CachedField;
const CompletionEdit = model.CompletionEdit;
const CompletionKind = model.CompletionKind;
const JsonValue = model.JsonValue;

/// The repository's standard library; tests run from the repository root.
const test_std_dir = "std";

test "completion context captures a dotted object path" {
    const ctx = lsp_text.computeCompletionAtOffset("std.http.ge", "std.http.ge".len);
    try std.testing.expectEqualStrings("std.http", ctx.object_name.?);
    try std.testing.expectEqualStrings("ge", ctx.prefix);

    const plain = lsp_text.computeCompletionAtOffset("node.field", "node.field".len);
    try std.testing.expectEqualStrings("node", plain.object_name.?);
}

test "findDottedPathAtOffset expands both directions" {
    const source = "const r is std.http.get(\"x\")";
    const mid = std.mem.indexOf(u8, source, "http").? + 1;
    const path = lsp_text.findDottedPathAtOffset(source, mid).?;
    try std.testing.expectEqualStrings("std.http.get", path.text);

    const end = std.mem.indexOf(u8, source, "get").? + 3;
    const at_end = lsp_text.findDottedPathAtOffset(source, end).?;
    try std.testing.expectEqualStrings("std.http.get", at_end.text);
}

fn dummyEdit() CompletionEdit {
    return .{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };
}

test "stdlib completions resolve modules, members, and methods" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\public function get(url :: string) returns Response {
        \\}
        \\public struct Response {
        \\    public method statusCode() returns int {
        \\    }
        \\}
        \\public enum Kind {
        \\    Ok,
        \\    Err,
        \\}
    ;
    const decls = try stdlib.parseSource(allocator, src);
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    const cat = stdlib.Catalog{ .modules = &modules };

    var members = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try lsp_completion.writeStdlibCompletions(&cat, allocator, &members.writer, "std.http", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"label\":\"get\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "get(url: string) -> Response") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"newText\":\"get($1)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"label\":\"Response\"") != null);

    var roots = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try lsp_completion.writeStdlibCompletions(&cat, allocator, &roots.writer, "std", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, roots.written(), "\"label\":\"http\"") != null);

    var methods = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try lsp_completion.writeStdlibCompletions(&cat, allocator, &methods.writer, "std.http.Response", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, methods.written(), "\"label\":\"statusCode\"") != null);

    var variants = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try lsp_completion.writeStdlibCompletions(&cat, allocator, &variants.writer, "std.http.Kind", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, variants.written(), "\"label\":\"Ok\"") != null);
}

test "word completion context captures the identifier prefix and replacement range" {
    const source = "const foo is ba";
    const ctx = lsp_text.computeCompletionAtOffset(source, source.len);
    try std.testing.expectEqual(CompletionKind.Word, ctx.kind);
    try std.testing.expectEqualStrings("ba", ctx.prefix);
    try std.testing.expectEqual(@as(usize, "const foo is ".len), ctx.replace_start);
    try std.testing.expectEqual(@as(usize, source.len), ctx.replace_end);

    const intrinsic = lsp_text.computeCompletionAtOffset("@pu", 3);
    try std.testing.expectEqual(CompletionKind.Intrinsic, intrinsic.kind);
    try std.testing.expectEqualStrings("@pu", intrinsic.prefix);
    try std.testing.expectEqual(@as(usize, 0), intrinsic.replace_start);

    const dot = lsp_text.computeCompletionAtOffset("foo.", 4);
    try std.testing.expectEqual(CompletionKind.Dot, dot.kind);
    try std.testing.expectEqualStrings("foo", dot.object_name.?);
}

test "call context finds the enclosing call and active parameter" {
    const source = "@insert(arr, ";
    const call = lsp_text.computeCallAtOffset(source, source.len).?;
    try std.testing.expectEqualStrings("@insert", call.callee);
    try std.testing.expectEqual(@as(usize, 1), call.active_parameter);

    const nested = "f(g(1, 2), ";
    const outer = lsp_text.computeCallAtOffset(nested, nested.len).?;
    try std.testing.expectEqualStrings("f", outer.callee);
    try std.testing.expectEqual(@as(usize, 1), outer.active_parameter);

    const with_string = "f(\"a,b\", ";
    const str_call = lsp_text.computeCallAtOffset(with_string, with_string.len).?;
    try std.testing.expectEqualStrings("f", str_call.callee);
    try std.testing.expectEqual(@as(usize, 1), str_call.active_parameter);

    try std.testing.expect(lsp_text.computeCallAtOffset("no call here", "no call here".len) == null);
}

test "signature help describes an intrinsic in progress" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    const source = "@insert(arr, ";
    const call = lsp_text.computeCallAtOffset(source, source.len).?;

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try lsp_signature.writeSignatureHelp(server.features(), &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"@insert(collection: array | string, index: int, value: any) -> nothing\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"collection: array | string\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

test "signature help resolves a stdlib call through the catalog" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), "public function get(url :: string, timeout :: int) returns int {\n}\n");
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };

    const source = "std.http.get(\"x\", ";
    const call = lsp_text.computeCallAtOffset(source, source.len).?;
    try std.testing.expectEqualStrings("std.http.get", call.callee);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try lsp_signature.writeSignatureHelp(server.features(), &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "get(url: string, timeout: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"timeout: int\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

test "intrinsic completion carries a typed signature and snippet" {
    var buf = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer buf.deinit();
    try lsp_completion.writeIntrinsicCompletions(std.testing.allocator, &buf.writer, "@pu", dummyEdit());

    const rendered = buf.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"@push\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "@push(collection: array | string, value: any) -> nothing") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"@push($1, $2)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"insertTextFormat\":2") != null);
}

test "word completion offers in-scope symbols and collapses duplicates" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    const alloc = server.symbol_index.arena.allocator();
    try server.symbol_index.addVariable("count", "int");
    try server.symbol_index.addModule("mymod");
    try server.symbol_index.types.put(try alloc.dupe(u8, "Node"), .{
        .name = try alloc.dupe(u8, "Node"),
        .kind = .Struct,
        .fields = &.{},
        .methods = &.{},
        .enum_variants = &.{},
    });
    // A declaration entry with the same name as the variable must not duplicate.
    try server.symbol_index.symbols.append(.{
        .name = try alloc.dupe(u8, "count"),
        .kind = 13,
        .start_line = 0,
        .start_character = 0,
        .end_line = 0,
        .end_character = 5,
    });

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try lsp_completion.writeWordCompletions(server.features(), &out.writer, "", dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"std\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"count\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"mymod\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"Node\"") != null);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, rendered, "\"label\":\"count\""));
}

const USER_SOURCE =
    \\public function add(a :: int, ^b :: string) returns int {
    \\    return a
    \\}
    \\
    \\public struct Node {
    \\    public method kind(level :: int) returns string {
    \\        return "x"
    \\    }
    \\}
;

/// Runs the real lexer/parser/analyzer over `src` so the symbol index is
/// populated exactly as it would be for an open document.
fn analyzeInto(server: *server_mod.Server, src: []const u8) !void {
    const copied = try std.testing.allocator.dupe(u8, src);
    defer std.testing.allocator.free(copied);
    var doc = Document{ .path = "test/misc/lazy/direct.doxa", .text = copied };
    try server.performAnalysis(std.testing.io, &doc, "file:///test.doxa");
}

test "a missing standard library is reported on the document" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, "no-such-std", sink.asResponseSink(), false);
    defer server.deinit();

    try std.testing.expectError(error.ModuleRootUnknown, analyzeInto(&server, USER_SOURCE));
    try std.testing.expectEqual(@as(usize, 1), reporter.diagnostics.items.len);
    const diagnostic = reporter.diagnostics.items[0];
    try std.testing.expectEqualStrings("E7012", diagnostic.code.?);
    try std.testing.expect(std.mem.indexOf(u8, diagnostic.message, "'no-such-std'") != null);
    try std.testing.expectEqualStrings("file:///test.doxa", diagnostic.loc.?.file_uri.?);
}

test "AST collection captures user function and method signatures" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const add = server.symbol_index.findCallable("add") orelse return error.MissingCallable;
    try std.testing.expectEqual(@as(usize, 2), add.params.len);
    try std.testing.expectEqualStrings("a", add.params[0].name);
    try std.testing.expectEqualStrings("int", add.params[0].type_text);
    try std.testing.expect(!add.params[0].alias);
    try std.testing.expect(add.params[1].alias);
    try std.testing.expectEqualStrings("string", add.params[1].type_text);
    try std.testing.expectEqualStrings("int", add.return_type);

    const kind = server.symbol_index.findMethod("Node", "kind") orelse return error.MissingMethod;
    try std.testing.expectEqual(@as(usize, 1), kind.params.len);
    try std.testing.expectEqualStrings("level", kind.params[0].name);
    try std.testing.expectEqualStrings("int", kind.params[0].type_text);
    try std.testing.expectEqualStrings("string", kind.return_type);
}

test "word completion snippets user functions" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try lsp_completion.writeWordCompletions(server.features(), &out.writer, "", dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"add\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"add($1, $2)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "add(a: int, ^b: string) -> int") != null);
}

test "member completion offers user method signatures and snippets" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const ctx = lsp_text.computeCompletionAtOffset("Node.", "Node.".len);
    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try lsp_completion.writeMemberCompletions(server.features(), &out.writer, ctx, dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"kind\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "kind(level: int) -> string") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"kind($1)\"") != null);
}

test "signature help covers user functions" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const source = "add(1, ";
    const call = lsp_text.computeCallAtOffset(source, source.len).?;
    try std.testing.expectEqualStrings("add", call.callee);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try lsp_signature.writeSignatureHelp(server.features(), &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "add(a: int, ^b: string) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"^b: string\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

const STDLIB_TYPE_SOURCE =
    \\public struct Response {
    \\    public method statusCode(code :: int) returns int {
    \\        return code
    \\    }
    \\}
;

test "std catalog-typed values get member completion and signature help" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), STDLIB_TYPE_SOURCE);
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };
    try server.symbol_index.addVariable("resp", "Response");

    const ctx = lsp_text.computeCompletionAtOffset("resp.", "resp.".len);
    var completion = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer completion.deinit();
    try lsp_completion.writeMemberCompletions(server.features(), &completion.writer, ctx, dummyEdit());

    const rendered = completion.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"statusCode\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "statusCode(code: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"statusCode($1)\"") != null);

    const source = "resp.statusCode(";
    const call = lsp_text.computeCallAtOffset(source, source.len).?;
    try std.testing.expectEqualStrings("resp.statusCode", call.callee);

    var help = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer help.deinit();
    try std.testing.expect(try lsp_signature.writeSignatureHelp(server.features(), &help.writer, call));
    try std.testing.expect(std.mem.indexOf(u8, help.written(), "statusCode(code: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, help.written(), "\"label\":\"code: int\"") != null);
}

fn expectValidHover(server: *server_mod.Server, uri: []const u8, character: usize) ![]u8 {
    const params_json = try std.fmt.allocPrint(
        std.testing.allocator,
        "{{\"textDocument\":{{\"uri\":\"{s}\"}},\"position\":{{\"line\":0,\"character\":{d}}}}}",
        .{ uri, character },
    );
    defer std.testing.allocator.free(params_json);
    var parsed_params = try std.json.parseFromSlice(JsonValue, std.testing.allocator, params_json, .{});
    defer parsed_params.deinit();

    const payload = try lsp_hover.buildHoverPayload(server.features(), .{ .integer = 1 }, parsed_params.value);
    errdefer std.testing.allocator.free(payload);

    // A malformed payload must fail here rather than silently reaching a client.
    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();
    return payload;
}

test "hover payload stays valid JSON for intrinsics and user callables" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    try server.storeDocument("file:///hover.doxa", "@push(arr, 1)");
    const intrinsic = try expectValidHover(&server, "file:///hover.doxa", 2);
    defer std.testing.allocator.free(intrinsic);
    try std.testing.expect(std.mem.indexOf(u8, intrinsic, "@push(collection: array | string, value: any) -> nothing") != null);

    // `add` is a user function: hover must render its collected signature.
    try server.storeDocument("file:///hover.doxa", "const x is add(1, \"y\")");
    const user = try expectValidHover(&server, "file:///hover.doxa", 12);
    defer std.testing.allocator.free(user);
    try std.testing.expect(std.mem.indexOf(u8, user, "add(a: int, ^b: string) -> int") != null);

    // Standard-library hover shares the same payload shape.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), "public function get(url :: string) returns int {\n    return 1\n}\n");
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };
    try server.storeDocument("file:///hover.doxa", "std.http.get(\"x\")");
    const std_hover = try expectValidHover(&server, "file:///hover.doxa", 9);
    defer std.testing.allocator.free(std_hover);
    try std.testing.expect(std.mem.indexOf(u8, std_hover, "get(url :: string)") != null);
}

test "diagnostic payload is valid JSON for long union messages" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    const uri = "file:///c:/dev/doxa-projects/oord/src/main.doxa";
    const loc = reporting.Location{
        .file = "c:\\dev\\doxa-projects\\oord\\src\\main.doxa",
        .file_uri = uri,
        .range = .{ .start_line = 96, .start_col = 1, .end_line = 96, .end_col = 1 },
    };
    reporter.reportCompileError(loc, "E1003", "Type mismatch: expected union (String | Array | Array | Array | Array | Array), got Function", .{});
    reporter.reportCompileError(loc, "E1003", "Type mismatch: expected union (String | Int | Float | Byte | Tetra), got member of kind error.Method", .{});

    const payload = try reporter.buildPublishDiagnosticsPayload(std.testing.allocator, uri);
    defer std.testing.allocator.free(payload);

    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();
    try std.testing.expect(std.mem.indexOf(u8, payload, "expected union") != null);
}

test "dot-field hover payload is valid JSON" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    const alloc = server.symbol_index.arena.allocator();
    try server.symbol_index.addVariable("node", "Node");
    const fields = try alloc.alloc(CachedField, 1);
    fields[0] = .{ .name = try alloc.dupe(u8, "x"), .type_name = "int", .is_public = true };
    try server.symbol_index.types.put(try alloc.dupe(u8, "Node"), .{
        .name = try alloc.dupe(u8, "Node"),
        .kind = .Struct,
        .fields = fields,
        .methods = &.{},
        .enum_variants = &.{},
    });

    try server.storeDocument("file:///dot.doxa", "node.x");
    const params = "{\"textDocument\":{\"uri\":\"file:///dot.doxa\"},\"position\":{\"line\":0,\"character\":6}}";
    var parsed_params = try std.json.parseFromSlice(JsonValue, std.testing.allocator, params, .{});
    defer parsed_params.deinit();

    const payload = try lsp_hover.buildHoverPayload(server.features(), .{ .integer = 1 }, parsed_params.value);
    defer std.testing.allocator.free(payload);

    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value.object.get("result").?.object.get("contents") != null);
}

test "document symbol ranges are well-formed and selection is contained" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const payload = try lsp_symbols.buildDocumentSymbolsPayload(server.features(), .{ .integer = 1 });
    defer std.testing.allocator.free(payload);

    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?;
    try std.testing.expect(result.array.items.len > 0);

    for (result.array.items) |item| {
        const full = item.object.get("range").?.object;
        const sel = item.object.get("selectionRange").?.object;
        const fsl = full.get("start").?.object.get("line").?.integer;
        const fsc = full.get("start").?.object.get("character").?.integer;
        const fel = full.get("end").?.object.get("line").?.integer;
        const fec = full.get("end").?.object.get("character").?.integer;
        const ssl = sel.get("start").?.object.get("line").?.integer;
        const ssc = sel.get("start").?.object.get("character").?.integer;
        const sel_line = sel.get("end").?.object.get("line").?.integer;
        const sec = sel.get("end").?.object.get("character").?.integer;

        try std.testing.expect(fsl < fel or (fsl == fel and fsc <= fec));
        try std.testing.expect(fsl < ssl or (fsl == ssl and fsc <= ssc));
        try std.testing.expect(sel_line < fel or (sel_line == fel and sec <= fec));
    }

    // The first statement is `add`, so its zero-based start line is 0.
    try std.testing.expectEqual(@as(i64, 0), result.array.items[0].object.get("range").?.object.get("start").?.object.get("line").?.integer);
}

const INLAY_SOURCE =
    \\const inferred is 1 + 2
    \\var explicit :: int is 5
    \\public function f() returns int {
    \\    const local is 3
    \\    return local
    \\}
;

test "inlay hints describe inferred variable types only" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, INLAY_SOURCE);

    const hints = server.symbol_index.inlay_hints.items;
    try std.testing.expectEqual(@as(usize, 2), hints.len);

    try std.testing.expectEqual(@as(usize, 0), hints[0].line);
    try std.testing.expectEqual(@as(usize, "const ".len + "inferred".len), hints[0].character);
    try std.testing.expectEqualStrings(":: int", hints[0].label);

    try std.testing.expectEqual(@as(usize, 3), hints[1].line);
    try std.testing.expectEqualStrings(":: int", hints[1].label);
}

test "inlay hint payload is valid JSON and honors the requested range" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = server_mod.Server.init(std.testing.allocator, &reporter, test_std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, INLAY_SOURCE);

    const params_json =
        "{\"textDocument\":{\"uri\":\"file:///inlay.doxa\"},\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":100}}}";
    var parsed_params = try std.json.parseFromSlice(JsonValue, std.testing.allocator, params_json, .{});
    defer parsed_params.deinit();

    const payload = try lsp_symbols.buildInlayHintPayload(server.features(), .{ .integer = 1 }, parsed_params.value);
    defer std.testing.allocator.free(payload);

    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?;
    try std.testing.expectEqual(@as(usize, 1), result.array.items.len);
    const hint = result.array.items[0].object;
    try std.testing.expectEqual(@as(i64, 0), hint.get("position").?.object.get("line").?.integer);
    try std.testing.expectEqualStrings(":: int", hint.get("label").?.string);
    try std.testing.expectEqual(@as(i64, 1), hint.get("kind").?.integer);
}
