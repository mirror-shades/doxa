const std = @import("std");
const testing = std.testing;

const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../src/parser/parser_types.zig").Parser;
const Reporting = @import("../src/utils/reporting.zig");
const module_graph = @import("../src/module/graph.zig");

const ParseResult = struct {
    tokens: std.array_list.Managed(@import("../src/types/token.zig").Token),
    parser: Parser,
    graph: *module_graph.ModuleGraph,

    fn deinit(self: *ParseResult) void {
        self.parser.deinit();
        self.tokens.deinit();
        self.graph.deinit();
    }
};

fn parseSource(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, source: []const u8, path: []const u8) !ParseResult {
    var lexer = try LexicalAnalyzer.init(testing.io, allocator, source, path, reporter);
    defer lexer.deinit();
    try lexer.initKeywords();

    const tokens = try lexer.lexTokens();
    const uri = try reporter.ensureFileUri(testing.io, path);

    // The repo root (cwd under `zig build test`) is the declared root, so
    // fixtures and `std/std.doxa` resolve under the module graph.
    const graph_store = try allocator.create(module_graph.ModuleGraph);
    graph_store.* = try module_graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = "." },
    });

    // Mirror the pipeline: the entry file is record 0, registered before any
    // import resolves and completed from the root parse. A path that is on disk
    // is keyed by its real path (as resolution keys it); a synthetic path that
    // is not is keyed lexically, so it still gets a deterministic record.
    const entry_physical = module_graph.physicalPath(testing.io, allocator, path) catch try allocator.dupe(u8, path);
    const entry_stable = (try graph_store.stableKeyFor(allocator, entry_physical)) orelse
        try module_graph.stableKey(allocator, "pkg", entry_physical);
    const entry_record = try graph_store.addRecord(entry_physical, entry_stable);

    var parser = Parser.init(testing.io, allocator, tokens.items, path, uri, reporter, graph_store);
    parser.owner_record = entry_record;
    const statements = try parser.execute();
    try parser.completeEntryRecord(entry_record, source, statements);

    return .{ .tokens = tokens, .parser = parser, .graph = graph_store };
}

test "lazy modules: aggregator children load only when referenced" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
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

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
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

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module direct from \"./direct.doxa\"\n", "test/misc/lazy/direct_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expectEqual(@as(usize, 0), parser.module_cache.count());
    _ = try parser.ensureModuleNamespace("direct");
    try testing.expect(parser.module_cache.contains("direct.doxa"));
}

test "lazy modules: two spellings of one file are one record" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module one from \"./bundle.doxa\"\nmodule two from \"././bundle.doxa\"\n",
        "test/misc/lazy/spell_user.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("one");
    _ = try parser.ensureModuleNamespace("two");

    // Both spellings bind a namespace, but the underlying file is one record.
    try testing.expect(parser.module_namespaces.contains("one"));
    try testing.expect(parser.module_namespaces.contains("two"));
    try testing.expect(parser.graph.findStable("pkg//test/misc/lazy/bundle.doxa") != null);
    // The entry file is record 0; the two spellings add exactly one more.
    try testing.expectEqualStrings("pkg//test/misc/lazy/spell_user.doxa", parser.graph.record(0).stable_module_key);
    try testing.expectEqual(@as(usize, 2), parser.graph.count());
}

test "lazy modules: the entry file is record 0 and an import of it dedups" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module self from \"./bundle.doxa\"\n",
        "test/misc/lazy/bundle.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    // The pipeline registered the entry before imports resolved.
    const entry = parser.graph.record(0);
    try testing.expectEqualStrings("pkg//test/misc/lazy/bundle.doxa", entry.stable_module_key);
    try testing.expect(entry.status == module_graph.ModuleStatus.Parsed);
    try testing.expect(entry.ast != null);

    // A module importing the entry file resolves to that same record rather
    // than parsing the root again.
    _ = try parser.ensureModuleNamespace("self");
    const resolved = parser.graph.findStable("pkg//test/misc/lazy/bundle.doxa").?;
    try testing.expect(resolved == entry);
    try testing.expectEqual(@as(usize, 1), parser.graph.count());
}

test "lazy modules: an inline zig block is a synthetic record owned by its file" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "zig Math {\n    pub fn double(n: i64) i64 {\n        return n * 2;\n    }\n}\n",
        "test/misc/lazy/inline_owner.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expect(parser.module_namespaces.contains("Math"));

    // The block is a synthetic record whose stable key derives from the owner
    // file, not a bare `gen:Math`.
    const generated = parser.graph.findStable("pkg//test/misc/lazy/inline_owner.doxa//zig/Math").?;
    try testing.expect(generated.physical_key == null);
    try testing.expect(generated.id != parser.graph.record(0).id);

    // Entry plus the one synthetic record.
    try testing.expectEqual(@as(usize, 2), parser.graph.count());
}

test "lazy modules: two files may each declare the same inline zig block" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module a from \"./inline_a.doxa\"\nmodule b from \"./inline_b.doxa\"\n",
        "test/misc/lazy/inline_users.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("a");
    _ = try parser.ensureModuleNamespace("b");

    // The same block name in two files is two distinct identities, because the
    // key carries the owner.
    const a_common = parser.graph.findStable("pkg//test/misc/lazy/inline_a.doxa//zig/Common").?;
    const b_common = parser.graph.findStable("pkg//test/misc/lazy/inline_b.doxa//zig/Common").?;
    try testing.expect(a_common != b_common);
    try testing.expect(a_common.physical_key == null);
    try testing.expect(b_common.physical_key == null);
}

test "lazy modules: a .zig file import is a record with the file's own identity" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module z from \"./../zig_module.zig\"\n",
        "test/misc/lazy/zig_user.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("z");

    // The alias only names the binding; identity is the imported file's own
    // physical path and stable key.
    const record = parser.graph.findStable("pkg//test/misc/zig_module.zig").?;
    try testing.expect(record.physical_key != null);
    try testing.expectEqualStrings("pkg//test/misc/zig_module.zig", record.stable_module_key);
}

test "lazy modules: reachable dependencies follow body references" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
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

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
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

test "specific import: binds only the named symbol and exposes no namespace" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "import alpha from \"./two.doxa\"\n", "test/misc/lazy/two_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    try testing.expect(try parser.ensureImportedSymbol("alpha"));

    // Only the named symbol is bound; the module's other public declarations
    // are not injected into the importer.
    try testing.expect(parser.imported_symbols.?.contains("alpha"));
    try testing.expect(!parser.imported_symbols.?.contains("beta"));
    try testing.expect(!(try parser.ensureImportedSymbol("beta")));

    // A specific import exposes no user-visible namespace for its module; the
    // module is registered under a path-hash key so calls within it still
    // resolve, but the file stem never becomes a namespace the importer sees.
    try testing.expect(!parser.module_namespaces.contains("two"));
    var found_hashed = false;
    var it = parser.module_namespaces.iterator();
    while (it.next()) |entry| {
        if (std.mem.startsWith(u8, entry.key_ptr.*, "$import_")) found_hashed = true;
    }
    try testing.expect(found_hashed);
}

test "lazy modules: circular imports are detected when reached" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module cycle from \"./cycle_a.doxa\"\n", "test/misc/lazy/cycle_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("cycle");
    _ = try parser.ensureNestedModuleNamespace("cycle", "b");
    try testing.expectError(error.CircularImport, parser.ensureNestedModuleNamespace("cycle.b", "a"));
}

test "lazy modules: std.process usage does not expose std.http in module_namespaces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module std from \"std/std.doxa\"\n", "test/misc/lazy/std_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    // Materializing the std aggregator must not register any child namespace.
    _ = try parser.ensureModuleNamespace("std");
    try testing.expect(parser.module_namespaces.contains("std"));
    try testing.expect(!parser.module_namespaces.contains("std.process"));
    try testing.expect(!parser.module_namespaces.contains("std.http"));

    // Using std.process registers only std.process — std.http stays absent.
    _ = try parser.ensureNestedModuleNamespace("std", "process");
    try testing.expect(parser.module_namespaces.contains("std.process"));
    try testing.expect(!parser.module_namespaces.contains("std.http"));
}

test "submodule import: `import a from ./bundle.doxa` binds a without exposing the parent" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "import a from \"./bundle.doxa\"\n", "test/misc/lazy/import_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureImportedSymbol("a");
    try testing.expect(parser.module_namespaces.contains("a"));
    try testing.expect(!parser.module_namespaces.contains("bundle"));
    try testing.expect(!parser.module_namespaces.contains("b"));
}

test "submodule import: multiple submodules each bind their own namespace" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "import a, b from \"./bundle.doxa\"\n", "test/misc/lazy/import_multi_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureImportedSymbol("a");
    _ = try parser.ensureImportedSymbol("b");
    try testing.expect(parser.module_namespaces.contains("a"));
    try testing.expect(parser.module_namespaces.contains("b"));

    try testing.expect(!parser.module_namespaces.contains("bundle"));
}

test "submodule import: `module bundle` does not leak submodules as bare namespaces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "module bundle from \"./bundle.doxa\"\n", "test/misc/lazy/module_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("bundle");
    _ = try parser.ensureNestedModuleNamespace("bundle", "a");

    // The submodule is reachable only through its qualified name.
    try testing.expect(parser.module_namespaces.contains("bundle.a"));
    try testing.expect(!parser.module_namespaces.contains("a"));
}

