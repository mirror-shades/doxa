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

/// Whether a module file has a graph record, keyed by its stable module key.
/// A record's existence is the load fact; there is no spelling-keyed cache.
fn moduleLoaded(parser: *Parser, stable_path: []const u8) bool {
    return parser.graph.findStable(stable_path) != null;
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

    // Entry record only; `bundle` is interned but materializes nothing yet.
    try testing.expectEqual(@as(usize, 1), parser.graph.count());
    try testing.expect(parser.module_namespaces.contains("bundle"));

    _ = try parser.ensureModuleNamespace("bundle");
    try testing.expectEqual(@as(usize, 2), parser.graph.count());
    try testing.expect(moduleLoaded(parser, "pkg//test/misc/lazy/bundle.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/b.doxa"));

    _ = try parser.ensureNestedModuleNamespace("bundle", "a");
    try testing.expect(moduleLoaded(parser, "pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/b.doxa"));
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
    try testing.expect(moduleLoaded(parser, "pkg//std/std.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//std/process/process.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//std/http/http.doxa"));

    _ = try parser.ensureNestedModuleNamespace("std", "process");
    try testing.expect(moduleLoaded(parser, "pkg//std/process/process.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//std/http/http.doxa"));

    _ = try parser.ensureNestedModuleNamespace("std", "http");
    try testing.expect(moduleLoaded(parser, "pkg//std/http/http.doxa"));
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

    try testing.expectEqual(@as(usize, 1), parser.graph.count());
    _ = try parser.ensureModuleNamespace("direct");
    try testing.expect(moduleLoaded(parser, "pkg//test/misc/lazy/direct.doxa"));
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
    try testing.expect(moduleLoaded(parser, "pkg//test/misc/lazy/uses_a_only.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/b.doxa"));

    try parser.ensureReachableModuleDependencies();
    try testing.expect(moduleLoaded(parser, "pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!moduleLoaded(parser, "pkg//test/misc/lazy/b.doxa"));
}

test "lazy modules: two imports of one name in a file are a duplicate binding" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    const result = parseSource(
        allocator,
        &reporter,
        "import Same from \"./same_a.doxa\"\nimport Same from \"./same_b.doxa\"\n",
        "test/misc/lazy/same_user.doxa",
    );

    try testing.expectError(error.DuplicateVariableName, result);
    try testing.expect(reporter.hasCompileErrors());

    // The diagnostic points at both sites: the offending import and the
    // previous declaration of the same name.
    try testing.expect(reporter.diagnostics.items.len >= 1);
    const diag = reporter.diagnostics.items[reporter.diagnostics.items.len - 1];
    try testing.expect(diag.related_info != null);
    try testing.expectEqual(@as(usize, 1), diag.related_info.?.len);
    try testing.expectEqualStrings("previous declaration here", diag.related_info.?[0].message);
    try testing.expectEqualStrings("test/misc/lazy/same_user.doxa", diag.related_info.?[0].location.file);
}

test "lazy modules: a module alias may not collide with a declaration" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    const result = parseSource(
        allocator,
        &reporter,
        "var alpha is 1\nmodule alpha from \"./same_a.doxa\"\n",
        "test/misc/lazy/alias_decl_user.doxa",
    );

    try testing.expectError(error.DuplicateVariableName, result);
    try testing.expect(reporter.hasCompileErrors());
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

    // The name is bound owner-scoped on the importing file's record, naming the
    // *defining* module (two.doxa), with the declaration's kind preserved.
    const entry = parsed.graph.record(0);
    const alpha = entry.bindings.get("alpha").?;
    try testing.expect(std.meta.activeTag(alpha.binding) == .symbol);
    try testing.expectEqual(module_graph.SymbolKind.Function, alpha.binding.symbol.kind);
    const two = parsed.graph.findStable("pkg//test/misc/lazy/two.doxa").?;
    try testing.expectEqual(two.id, alpha.binding.symbol.module);
    try testing.expect(entry.bindings.get("beta") == null);

    // A specific import exposes no user-visible namespace for its module; the
    // module is registered under a path-hash key so calls within it still
    // resolve, but the file stem never becomes a namespace the importer sees.
    try testing.expect(!parser.module_namespaces.contains("two"));
    var found_hashed = false;
    var it = parser.module_namespaces.iterator();
    while (it.next()) |entry_ns| {
        if (std.mem.startsWith(u8, entry_ns.key_ptr.*, "$import_")) found_hashed = true;
    }
    try testing.expect(found_hashed);
}

test "bindings: `import n from S` binds the submodule namespace on the importer" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(allocator, &reporter, "import a from \"./bundle.doxa\"\n", "test/misc/lazy/import_bind_user.doxa");
    defer parsed.deinit();
    const parser = &parsed.parser;

    // A submodule import binds a namespace, not an `imported_symbols` entry,
    // so the return value is false but the binding is present.
    _ = try parser.ensureImportedSymbol("a");

    // `a` is a re-exported submodule, so it is a namespace binding on the
    // importing file's record, pointing at a.doxa's record.
    const entry = parsed.graph.record(0);
    const a_binding = entry.bindings.get("a").?;
    try testing.expect(std.meta.activeTag(a_binding.binding) == .namespace);
    const a_record = parsed.graph.findStable("pkg//test/misc/lazy/a.doxa").?;
    try testing.expectEqual(a_record.id, a_binding.binding.namespace);

    // The parent module and its other submodule are not injected.
    try testing.expect(entry.bindings.get("bundle") == null);
    try testing.expect(entry.bindings.get("b") == null);
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

test "bindings: top-level declarations are owner-scoped with visibility" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "public function foo() returns int {\n    return 1\n}\nfunction bar() returns int {\n    return 2\n}\n",
        "test/misc/lazy/bind_decls.doxa",
    );
    defer parsed.deinit();
    const entry = parsed.graph.record(0);

    const foo = entry.bindings.get("foo").?;
    try testing.expect(foo.visibility == .Public);
    try testing.expect(std.meta.activeTag(foo.binding) == .symbol);
    try testing.expectEqual(module_graph.SymbolKind.Function, foo.binding.symbol.kind);
    try testing.expectEqual(entry.id, foo.binding.symbol.module);
    try testing.expect(entry.public_bindings.contains("foo"));

    // A private declaration is in the file's namespace but not its public
    // surface.
    const bar = entry.bindings.get("bar").?;
    try testing.expect(bar.visibility == .Private);
    try testing.expect(!entry.public_bindings.contains("bar"));
}

test "bindings: a resolved module alias is bound on its owning file's record" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module util from \"./bind_a.doxa\"\n",
        "test/misc/lazy/bind_user.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    const entry = parser.graph.record(0);
    try testing.expect(entry.bindings.get("util") == null);

    _ = try parser.ensureModuleNamespace("util");

    // Resolution binds `util` on the file that declared it, not globally.
    const util = entry.bindings.get("util").?;
    try testing.expect(std.meta.activeTag(util.binding) == .namespace);
    const target = parser.graph.findStable("pkg//test/misc/lazy/bind_a.doxa").?;
    try testing.expectEqual(target.id, util.binding.namespace);
}

test "bindings: two files may use the same alias for different targets" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "module left from \"./lazy/alias_left.doxa\"\nmodule right from \"./lazy/alias_right.doxa\"\n",
        "test/misc/bind_root.doxa",
    );
    defer parsed.deinit();
    const parser = &parsed.parser;

    _ = try parser.ensureModuleNamespace("left");
    _ = try parser.ensureModuleNamespace("right");
    try parser.ensureReachableModuleDependencies();

    const left = parser.graph.findStable("pkg//test/misc/lazy/alias_left.doxa").?;
    const right = parser.graph.findStable("pkg//test/misc/lazy/alias_right.doxa").?;
    const a = parser.graph.findStable("pkg//test/misc/lazy/a.doxa").?;
    const b = parser.graph.findStable("pkg//test/misc/lazy/b.doxa").?;

    // Each file's `dep` alias is bound on that file's own record to its own
    // target: the flat alias map cannot represent this, the records can.
    const left_dep = left.bindings.get("dep").?;
    try testing.expect(std.meta.activeTag(left_dep.binding) == .namespace);
    try testing.expectEqual(a.id, left_dep.binding.namespace);

    const right_dep = right.bindings.get("dep").?;
    try testing.expect(std.meta.activeTag(right_dep.binding) == .namespace);
    try testing.expectEqual(b.id, right_dep.binding.namespace);
}

test "bindings: an inline zig block is a namespace binding owned by its file" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var parsed = try parseSource(
        allocator,
        &reporter,
        "zig Math {\n    pub fn double(n: i64) i64 {\n        return n * 2;\n    }\n}\n",
        "test/misc/lazy/bind_zig.doxa",
    );
    defer parsed.deinit();
    const entry = parsed.graph.record(0);

    const math = entry.bindings.get("Math").?;
    try testing.expect(std.meta.activeTag(math.binding) == .namespace);

    const generated = parsed.graph.findStable("pkg//test/misc/lazy/bind_zig.doxa//zig/Math").?;
    try testing.expectEqual(generated.id, math.binding.namespace);
    // The block's own namespace is its function surface.
    try testing.expect(generated.bindings.contains("double"));
    try testing.expectEqual(module_graph.SymbolKind.Function, generated.bindings.get("double").?.binding.symbol.kind);
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

