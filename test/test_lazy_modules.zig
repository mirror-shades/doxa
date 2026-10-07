const std = @import("std");
const testing = std.testing;

const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../src/parser/parser_types.zig").Parser;
const Reporting = @import("../src/utils/reporting.zig");
const MemoryManager = @import("../src/utils/memory.zig").MemoryManager;
const SemanticAnalyzer = @import("../src/analysis/semantic/semantic.zig").SemanticAnalyzer;
const module_graph = @import("../src/module/graph.zig");
const ModuleLoader = @import("../src/module/loader.zig").ModuleLoader;
const ModuleRecord = module_graph.ModuleRecord;

// The loader's laziness, observed through the graph. A record is *interned*
// when a specifier names it and *loaded* once it is parsed: a `module` alias
// interns its target, and only a name resolved through it loads it. The repo
// root (cwd under `zig build test`) is the declared root, so specifiers are
// written from it.

const Fixture = struct {
    graph: *module_graph.ModuleGraph,
    loader: ModuleLoader,
    entry: *ModuleRecord,

    fn record(self: *Fixture, stable_key: []const u8) ?*ModuleRecord {
        return self.graph.findStable(stable_key);
    }

    fn loaded(self: *Fixture, stable_key: []const u8) bool {
        const found = self.record(stable_key) orelse return false;
        return found.status.atLeast(.Parsed);
    }

    /// The namespace `name` is bound to in `owner`.
    fn namespace(owner: *ModuleRecord, name: []const u8) module_graph.ModuleId {
        return owner.bindings.get(name).?.binding.namespace;
    }

    /// Resolve `name` through namespace `id`, as a qualified reference does.
    fn member(self: *Fixture, id: module_graph.ModuleId, name: []const u8) !module_graph.BoundName {
        return (try self.loader.member(id, name)).?;
    }
};

/// Parse `source` as the entry file at `path` and collect its declarations.
/// `path` names the file's identity only; it must exist, but its contents are
/// `source`.
fn loadEntry(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, source: []const u8, path: []const u8) !Fixture {
    var lexer = try LexicalAnalyzer.init(testing.io, allocator, source, path, reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    const uri = try reporter.ensureFileUri(testing.io, path);

    var parser = Parser.init(testing.io, allocator, tokens.items, path, uri, reporter);
    const statements = try parser.execute();

    const graph_store = try allocator.create(module_graph.ModuleGraph);
    graph_store.* = try module_graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = "." },
    });
    var fixture = Fixture{
        .graph = graph_store,
        .loader = ModuleLoader.init(testing.io, allocator, reporter, graph_store),
        .entry = undefined,
    };
    fixture.entry = try fixture.loader.registerEntry(path, source, statements);
    _ = try fixture.loader.ensureDeclarations(fixture.entry);
    return fixture;
}

/// Each test's allocations live in one arena; its graph is torn down with it.
const Harness = struct {
    arena: std.heap.ArenaAllocator,
    reporter: Reporting.Reporter,

    fn init(self: *Harness) void {
        self.arena = std.heap.ArenaAllocator.init(testing.allocator);
        self.reporter = Reporting.Reporter.init(testing.io, self.arena.allocator(), .{ .log_to_stderr = false }, null);
    }

    fn deinit(self: *Harness) void {
        self.reporter.deinit();
        self.arena.deinit();
    }

    fn load(self: *Harness, source: []const u8, path: []const u8) !Fixture {
        return loadEntry(self.arena.allocator(), &self.reporter, source, path);
    }
};

const lazy_dir = "test/misc/lazy/";
/// A fixture no test imports, so an entry parsed under its identity never
/// collides with a record the test reaches.
const entry_path = lazy_dir ++ "direct.doxa";

test "lazy modules: aggregator children load only when referenced" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module bundle from \"pkg//test/misc/lazy/bundle.doxa\"\n", entry_path);

    // The alias interns its target and loads nothing.
    try testing.expect(f.record("pkg//test/misc/lazy/bundle.doxa") != null);
    try testing.expect(!f.loaded("pkg//test/misc/lazy/bundle.doxa"));

    // A name through `bundle` loads it, interning (not loading) its children.
    const a = try f.member(Fixture.namespace(f.entry, "bundle"), "a");
    try testing.expect(f.loaded("pkg//test/misc/lazy/bundle.doxa"));
    try testing.expect(!f.loaded("pkg//test/misc/lazy/b.doxa"));

    // Only the child a name reaches through is loaded.
    _ = try f.member(a.binding.namespace, "value");
    try testing.expect(f.loaded("pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!f.loaded("pkg//test/misc/lazy/b.doxa"));
}

test "lazy modules: the standard-library aggregator stays shallow until child use" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module std from \"pkg//std/std.doxa\"\n", entry_path);

    const process = try f.member(Fixture.namespace(f.entry, "std"), "process");
    try testing.expect(f.loaded("pkg//std/std.doxa"));
    try testing.expect(!f.loaded("pkg//std/process/process.doxa"));
    try testing.expect(!f.loaded("pkg//std/http/http.doxa"));

    _ = try f.loader.ensureDeclarations(f.graph.record(process.binding.namespace));
    try testing.expect(f.loaded("pkg//std/process/process.doxa"));
    try testing.expect(!f.loaded("pkg//std/http/http.doxa"));
}

test "lazy modules: two spellings of one file are one record" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load(
        "module one from \"pkg//test/misc/lazy/bundle.doxa\"\nmodule two from \"./../lazy/bundle.doxa\"\n",
        entry_path,
    );

    try testing.expectEqual(Fixture.namespace(f.entry, "one"), Fixture.namespace(f.entry, "two"));
    // The entry file is record 0; the two spellings add exactly one more.
    try testing.expectEqual(@as(module_graph.ModuleId, 0), f.entry.id);
    try testing.expectEqual(@as(usize, 2), f.graph.count());
}

test "lazy modules: the entry file is record 0 and an import of it dedups" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module self from \"./bundle.doxa\"\n", lazy_dir ++ "bundle.doxa");

    try testing.expectEqualStrings("pkg//test/misc/lazy/bundle.doxa", f.entry.stable_module_key);
    // A module importing the entry file reaches that same record rather than
    // parsing the root again.
    try testing.expectEqual(f.entry.id, Fixture.namespace(f.entry, "self"));
    try testing.expectEqual(@as(usize, 1), f.graph.count());
}

test "lazy modules: an inline zig block is a synthetic record bound as a namespace" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("zig Math {\n    pub fn double(n: i64) i64 {\n        return n * 2;\n    }\n}\n", entry_path);

    // Its stable key derives from the owner file, and its own namespace is its
    // function surface.
    const generated = f.record("pkg//test/misc/lazy/direct.doxa//zig/Math").?;
    try testing.expect(generated.physical_key == null);
    try testing.expectEqual(f.entry.id, generated.owner.?);
    try testing.expectEqual(generated.id, Fixture.namespace(f.entry, "Math"));
    try testing.expectEqual(module_graph.SymbolKind.Function, generated.bindings.get("double").?.binding.symbol.kind);
    try testing.expectEqual(@as(usize, 2), f.graph.count());
}

test "lazy modules: two files may each declare the same inline zig block" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load(
        "module a from \"pkg//test/misc/lazy/inline_a.doxa\"\nmodule b from \"pkg//test/misc/lazy/inline_b.doxa\"\n",
        entry_path,
    );
    _ = try f.loader.ensureDeclarations(f.graph.record(Fixture.namespace(f.entry, "a")));
    _ = try f.loader.ensureDeclarations(f.graph.record(Fixture.namespace(f.entry, "b")));

    // The key carries the owner, so one block name in two files is two records.
    const a_common = f.record("pkg//test/misc/lazy/inline_a.doxa//zig/Common").?;
    const b_common = f.record("pkg//test/misc/lazy/inline_b.doxa//zig/Common").?;
    try testing.expect(a_common != b_common);
}

test "lazy modules: a .zig file import is a record with the file's own identity" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module z from \"pkg//test/misc/zig_module.zig\"\n", entry_path);

    // The alias only names the binding; identity is the file's own.
    const record = f.record("pkg//test/misc/zig_module.zig").?;
    try testing.expect(record.physical_key != null);
    try testing.expectEqual(module_graph.RecordKind.zig_file, record.kind);
    try testing.expectEqual(record.id, Fixture.namespace(f.entry, "z"));
}

test "lazy modules: analysis loads what bodies reference, and nothing else" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module parent from \"pkg//test/misc/lazy/uses_a_only.doxa\"\nparent.value()?\n", entry_path);

    var memory = try MemoryManager.init(h.arena.allocator());
    defer memory.deinit();
    var semantic = SemanticAnalyzer.init(h.arena.allocator(), &h.reporter, &memory, &f.loader, f.entry.id, null);
    defer semantic.deinit();
    try semantic.analyzeProgram();

    // `parent.value` reaches uses_a_only, whose body reaches `a`; `b` is only
    // ever named by an alias nothing reads through.
    try testing.expect(f.loaded("pkg//test/misc/lazy/uses_a_only.doxa"));
    try testing.expect(f.loaded("pkg//test/misc/lazy/a.doxa"));
    try testing.expect(!f.loaded("pkg//test/misc/lazy/b.doxa"));
}

test "lazy modules: two bindings of one name in a file point at both sites" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    const result = h.load(
        "import Same from \"pkg//test/misc/lazy/same_a.doxa\"\nimport Same from \"pkg//test/misc/lazy/same_b.doxa\"\n",
        entry_path,
    );
    try testing.expectError(error.ModuleParseError, result);

    const diag = h.reporter.diagnostics.items[h.reporter.diagnostics.items.len - 1];
    try testing.expectEqualStrings("Duplicate binding name 'Same'", diag.message);
    try testing.expectEqual(@as(usize, 1), diag.related_info.?.len);
    try testing.expectEqualStrings("previous declaration here", diag.related_info.?[0].message);
}

test "lazy modules: a module alias may not collide with a declaration" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    const result = h.load("var alpha is 1\nmodule alpha from \"pkg//test/misc/lazy/same_a.doxa\"\n", entry_path);
    try testing.expectError(error.ModuleParseError, result);
    try testing.expect(h.reporter.hasCompileErrors());
}

test "lazy modules: mutual module aliases are legal" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("module cycle from \"pkg//test/misc/lazy/cycle_a.doxa\"\n", entry_path);

    // A namespace binding needs nothing from its target, so `cycle.b.a` walks
    // the loop and lands back on cycle_a.
    const cycle_a = Fixture.namespace(f.entry, "cycle");
    const b = try f.member(cycle_a, "b");
    const a = try f.member(b.binding.namespace, "a");
    try testing.expectEqual(cycle_a, a.binding.namespace);
    try testing.expect(!h.reporter.hasCompileErrors());
}

test "lazy modules: an import cycle is reported where it is reached" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    // Classifying `x` needs icycle_a's surface, which needs icycle_b's, which
    // needs icycle_a's again.
    const result = h.load("import x from \"pkg//test/misc/lazy/icycle_a.doxa\"\n", entry_path);
    try testing.expectError(error.ModuleParseError, result);

    var reported = false;
    for (h.reporter.diagnostics.items) |diag| {
        if (diag.code) |code| reported = reported or std.mem.eql(u8, code, "E7002");
    }
    try testing.expect(reported);
}

test "specific import: binds only the named symbol, naming its defining module" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("import alpha from \"pkg//test/misc/lazy/two.doxa\"\n", entry_path);

    const alpha = f.entry.bindings.get("alpha").?;
    try testing.expectEqual(module_graph.SymbolKind.Function, alpha.binding.symbol.kind);
    try testing.expectEqual(f.record("pkg//test/misc/lazy/two.doxa").?.id, alpha.binding.symbol.module);
    // Neither the module's other declarations nor a namespace for it appear.
    try testing.expect(f.entry.bindings.get("beta") == null);
    try testing.expect(f.entry.bindings.get("two") == null);
}

test "specific import: a public submodule binds as a namespace, without its parent" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load("import a, b from \"pkg//test/misc/lazy/bundle.doxa\"\n", lazy_dir ++ "two.doxa");

    try testing.expectEqual(f.record("pkg//test/misc/lazy/a.doxa").?.id, Fixture.namespace(f.entry, "a"));
    try testing.expectEqual(f.record("pkg//test/misc/lazy/b.doxa").?.id, Fixture.namespace(f.entry, "b"));
    try testing.expect(f.entry.bindings.get("bundle") == null);
}

test "specific import: a name the module does not publish is a located error" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    const result = h.load("import gamma from \"pkg//test/misc/lazy/two.doxa\"\n", entry_path);
    try testing.expectError(error.ModuleParseError, result);
    try testing.expect(h.reporter.hasCompileErrors());
}

test "bindings: top-level declarations are owner-scoped with visibility" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load(
        "public function foo() returns int {\n    return 1\n}\nfunction bar() returns int {\n    return 2\n}\n",
        entry_path,
    );

    const foo = f.entry.bindings.get("foo").?;
    try testing.expect(foo.visibility == .Public);
    try testing.expectEqual(module_graph.SymbolKind.Function, foo.binding.symbol.kind);
    try testing.expectEqual(f.entry.id, foo.binding.symbol.module);
    try testing.expect(f.entry.public_bindings.contains("foo"));

    // A private declaration is in the file's namespace, not its public surface.
    try testing.expect(f.entry.bindings.get("bar").?.visibility == .Private);
    try testing.expect(!f.entry.public_bindings.contains("bar"));
}

test "bindings: two files may use the same alias for different targets" {
    var h: Harness = undefined;
    h.init();
    defer h.deinit();
    var f = try h.load(
        "module left from \"pkg//test/misc/lazy/alias_left.doxa\"\nmodule right from \"pkg//test/misc/lazy/alias_right.doxa\"\n",
        entry_path,
    );
    const left = try f.loader.ensureDeclarations(f.graph.record(Fixture.namespace(f.entry, "left")));
    const right = try f.loader.ensureDeclarations(f.graph.record(Fixture.namespace(f.entry, "right")));

    // Each file's `dep` is bound on that file's own record to its own target.
    try testing.expectEqual(f.record("pkg//test/misc/lazy/a.doxa").?.id, Fixture.namespace(left, "dep"));
    try testing.expectEqual(f.record("pkg//test/misc/lazy/b.doxa").?.id, Fixture.namespace(right, "dep"));
}
