const std = @import("std");
const testing = std.testing;

const inline_zig = @import("../src/parser/inline_zig.zig");
const ast = @import("../src/ast/ast.zig");
const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../src/parser/parser_types.zig").Parser;
const Reporting = @import("../src/utils/reporting.zig");
const inline_zig_compiler = @import("../src/inline_zig/compiler.zig");

test "inline zig: accepts import consts and function bodies" {
    const src =
        \\const std = @import("std");
        \\
        \\pub fn add(a: i64, b: i64) i64 {
        \\    return a + b;
        \\}
    ;

    const sigs = try inline_zig.sanitizeAndExtract(testing.allocator, src);
    defer {
        for (sigs) |sig| {
            testing.allocator.free(sig.name);
            testing.allocator.free(sig.param_types);
        }
        testing.allocator.free(sigs);
    }

    try testing.expectEqual(@as(usize, 1), sigs.len);
    try testing.expectEqualStrings("add", sigs[0].name);
    try testing.expectEqual(ast.Type.Int, sigs[0].return_type.base);
    try testing.expectEqual(@as(usize, 2), sigs[0].param_types.len);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[0].base);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[1].base);
}

test "inline zig: accepts multiline signatures" {
    const src =
        \\pub fn foo(
        \\    a: i64,
        \\    b: f64,
        \\) f64 {
        \\    _ = a;
        \\    return b;
        \\}
    ;

    const sigs = try inline_zig.sanitizeAndExtract(testing.allocator, src);
    defer {
        for (sigs) |sig| {
            testing.allocator.free(sig.name);
            testing.allocator.free(sig.param_types);
        }
        testing.allocator.free(sigs);
    }

    try testing.expectEqual(@as(usize, 1), sigs.len);
    try testing.expectEqualStrings("foo", sigs[0].name);
    try testing.expectEqual(ast.Type.Float, sigs[0].return_type.base);
    try testing.expectEqual(@as(usize, 2), sigs[0].param_types.len);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[0].base);
    try testing.expectEqual(ast.Type.Float, sigs[0].param_types[1].base);
}

test "inline zig: rejects non-import const at top level" {
    const src = "const x = 123;";
    try testing.expectError(error.InlineZigNotValid, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects invalid parameter type on pub fn" {
    const src = "pub fn bad(x: i32) i64 { _ = x; return 0; }";
    try testing.expectError(error.InvalidParamType, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects invalid return type on pub fn" {
    const src = "pub fn bad(x: i64) i32 { _ = x; return 0; }";
    try testing.expectError(error.InvalidReturnType, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects pub const struct at top level" {
    const src =
        \\pub const S = struct { x: i64 };
        \\pub fn f() i64 { return 1; }
    ;
    try testing.expectError(error.InlineZigNotValid, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects extern declarations without bodies" {
    const src = "extern fn f(x: i64) i64;";
    try testing.expectError(error.InlineZigNotValid, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: collectInlineZigDecls only sees reachable modules" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexer = try LexicalAnalyzer.init(allocator, "module std from \"std/std.doxa\"\n", "test/inline_zig_collect.doxa", &reporter);
    defer lexer.deinit();
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    defer tokens.deinit();

    const uri = try reporter.ensureFileUri("test/inline_zig_collect.doxa");
    var parser = Parser.init(allocator, tokens.items, "test/inline_zig_collect.doxa", uri, &reporter);
    defer parser.deinit();
    _ = try parser.execute();

    _ = try parser.ensureModuleNamespace("std");

    // Only load std.process, not std.http
    _ = try parser.ensureNestedModuleNamespace("std", "process");
    try testing.expect(parser.module_cache.contains("process/process.doxa"));
    try testing.expect(!parser.module_cache.contains("http/http.doxa"));

    // collectInlineZigDecls iterates module_namespaces — should only see Process's zig block
    const parsed_at_root: [0]ast.Stmt = .{};
    const zig_decls = try inline_zig_compiler.collectInlineZigDecls(
        allocator,
        &parsed_at_root,
        &parser,
    );
    defer allocator.free(zig_decls);

    // Should contain exactly 1 decl: "Process"
    try testing.expect(zig_decls.len >= 1);

    var found_process = false;
    var found_http = false;
    for (zig_decls) |decl| {
        if (std.mem.eql(u8, decl.module_name, "Process")) found_process = true;
        if (std.mem.eql(u8, decl.module_name, "HTTP")) found_http = true;
    }
    try testing.expect(found_process);
    try testing.expect(!found_http);

    // Now load std.http and verify it shows up
    _ = try parser.ensureNestedModuleNamespace("std", "http");
    try testing.expect(parser.module_cache.contains("http/http.doxa"));

    const zig_decls2 = try inline_zig_compiler.collectInlineZigDecls(
        allocator,
        &parsed_at_root,
        &parser,
    );
    defer allocator.free(zig_decls2);

    found_process = false;
    found_http = false;
    for (zig_decls2) |decl| {
        if (std.mem.eql(u8, decl.module_name, "Process")) found_process = true;
        if (std.mem.eql(u8, decl.module_name, "HTTP")) found_http = true;
    }
    try testing.expect(found_process);
    try testing.expect(found_http);
}
