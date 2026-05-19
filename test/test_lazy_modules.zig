const std = @import("std");
const testing = std.testing;

const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../src/parser/parser_types.zig").Parser;
const Reporting = @import("../src/utils/reporting.zig");

const ParseResult = struct {
    tokens: std.array_list.Managed(@import("../src/types/token.zig").Token),
    parser: Parser,

    fn deinit(self: *ParseResult) void {
        self.parser.deinit();
        self.tokens.deinit();
    }
};

fn parseSource(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, source: []const u8, path: []const u8) !ParseResult {
    var lexer = try LexicalAnalyzer.init(allocator, source, path, reporter);
    defer lexer.deinit();
    try lexer.initKeywords();

    const tokens = try lexer.lexTokens();
    const uri = try reporter.ensureFileUri(path);
    var parser = Parser.init(allocator, tokens.items, path, uri, reporter);
    _ = try parser.execute();

    return .{ .tokens = tokens, .parser = parser };
}

test "lazy modules: aggregator children load only when referenced" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module bundle from \"./bundle.doxa\"\n", "test/misc/lazy/main.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expectEqual(@as(usize, 0), parser.module_cache.count());
    try testing.expect(parser.module_namespaces.contains("bundle"));

    _ = try parser.ensureModuleNamespace("bundle");
    try testing.expectEqual(@as(usize, 1), parser.module_cache.count());
    try testing.expect(parser.module_cache.contains("bundle.doxa"));
    try testing.expect(!parser.module_cache.contains("a.doxa"));
    try testing.expect(!parser.module_cache.contains("b.doxa"));

    _ = try parser.ensureNestedModuleNamespace("bundle", "a");
    try testing.expect(parser.module_cache.contains("a.doxa"));
    try testing.expect(!parser.module_cache.contains("b.doxa"));
}

test "lazy modules: standard-library aggregator stays shallow until child use" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module std from \"std/std.doxa\"\n", "test/misc/lazy/std_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("std");
    try testing.expect(parser.module_cache.contains("std/std.doxa"));
    try testing.expect(!parser.module_cache.contains("process/process.doxa"));
    try testing.expect(!parser.module_cache.contains("http/http.doxa"));

    _ = try parser.ensureNestedModuleNamespace("std", "process");
    try testing.expect(parser.module_cache.contains("process/process.doxa"));
    try testing.expect(!parser.module_cache.contains("http/http.doxa"));

    _ = try parser.ensureNestedModuleNamespace("std", "http");
    try testing.expect(parser.module_cache.contains("http/http.doxa"));
}

test "lazy modules: direct imports materialize without transitive siblings" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module direct from \"./direct.doxa\"\n", "test/misc/lazy/direct_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expectEqual(@as(usize, 0), parser.module_cache.count());
    _ = try parser.ensureModuleNamespace("direct");
    try testing.expect(parser.module_cache.contains("direct.doxa"));
}

test "lazy modules: reachable dependencies follow body references" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module parent from \"./uses_a_only.doxa\"\n", "test/misc/lazy/uses_a_only_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("parent");
    try testing.expect(parser.module_cache.contains("uses_a_only.doxa"));
    try testing.expect(!parser.module_cache.contains("a.doxa"));
    try testing.expect(!parser.module_cache.contains("b.doxa"));

    try parser.ensureReachableModuleDependencies();
    try testing.expect(parser.module_cache.contains("a.doxa"));
    try testing.expect(!parser.module_cache.contains("b.doxa"));
}

test "lazy modules: duplicate specific symbol names keep distinct import entries" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "import Same from \"./same_a.doxa\"\nimport Same from \"./same_b.doxa\"\n",
        "test/misc/lazy/same_user.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expectEqual(@as(usize, 2), parser.specific_imports.items.len);
    try testing.expect(std.mem.eql(u8, parser.specific_imports.items[0].module_path, "./same_a.doxa"));
    try testing.expect(std.mem.eql(u8, parser.specific_imports.items[1].module_path, "./same_b.doxa"));
}

test "lazy modules: circular imports are detected when reached" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(allocator, .{ .log_to_stderr = false });
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module cycle from \"./cycle_a.doxa\"\n", "test/misc/lazy/cycle_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("cycle");
    _ = try parser.ensureNestedModuleNamespace("cycle", "b");
    try testing.expectError(error.CircularImport, parser.ensureNestedModuleNamespace("cycle.b", "a"));
}
