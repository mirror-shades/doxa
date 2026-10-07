const std = @import("std");
const testing = std.testing;

const graph = @import("../src/module/graph.zig");
const Reporter = @import("../src/utils/reporting.zig").Reporter;

test "module graph: containment is component-wise, not textual" {
    try testing.expectEqualStrings("bar.doxa", graph.relativePart("/pkg/foo", "/pkg/foo/bar.doxa").?);
    try testing.expectEqualStrings("", graph.relativePart("/pkg/foo", "/pkg/foo").?);
    try testing.expectEqualStrings("pkg/foo", graph.relativePart("/", "/pkg/foo").?);

    // `/pkg/foo` must not contain `/pkg/foobar`.
    try testing.expect(graph.relativePart("/pkg/foo", "/pkg/foobar") == null);
    try testing.expect(graph.relativePart("/pkg/foo", "/pkg/foobar/a.doxa") == null);

    // A candidate that runs out of components is not contained.
    try testing.expect(graph.relativePart("/pkg/foo/bar", "/pkg/foo") == null);
}

test "module graph: containment handles Windows separators" {
    try testing.expectEqualStrings("a.doxa", graph.relativePart("C:\\pkg\\foo", "C:\\pkg\\foo\\a.doxa").?);
    try testing.expectEqualStrings("a.doxa", graph.relativePart("C:\\pkg\\foo", "C:/pkg/foo/a.doxa").?);
    try testing.expect(graph.relativePart("C:\\pkg\\foo", "C:\\pkg\\foobar") == null);
}

test "module graph: stable key normalizes separators and is path-independent" {
    const allocator = testing.allocator;

    const key = try graph.stableKey(allocator, "pkg", "src\\util.doxa");
    defer allocator.free(key);
    try testing.expectEqualStrings("pkg//src/util.doxa", key);

    const same = try graph.stableKey(allocator, "pkg", "src/util.doxa");
    defer allocator.free(same);
    try testing.expectEqualStrings(key, same);
}

test "module graph: generated keys derive from their owner" {
    const allocator = testing.allocator;

    const key = try graph.generatedKey(allocator, "pkg//src/main.doxa", "IO");
    defer allocator.free(key);
    try testing.expectEqualStrings("pkg//src/main.doxa//zig/IO", key);

    const nested = try graph.generatedKey(allocator, "pkg//src/main.doxa", "IO.Inner");
    defer allocator.free(nested);
    try testing.expect(!std.mem.eql(u8, key, nested));
}

fn tmpRealPath(tmp: *std.testing.TmpDir, allocator: std.mem.Allocator) ![]const u8 {
    var buffer: [std.fs.max_path_bytes]u8 = undefined;
    const real = buffer[0..try tmp.dir.realPath(testing.io, &buffer)];
    return allocator.dupe(u8, real);
}

test "module graph: root registry matches by components and derives stable keys" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg/foo");
    try tmp.dir.createDirPath(testing.io, "pkg/foobar");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/foo/a.doxa", .data = "one" });
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/foobar/a.doxa", .data = "one" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);

    const foo_root = try std.fs.path.join(allocator, &.{ base, "pkg", "foo" });
    defer allocator.free(foo_root);

    var registry = try graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = foo_root },
    });
    defer registry.deinit();

    const foo_file = try std.fs.path.join(allocator, &.{ base, "pkg", "foo", "a.doxa" });
    defer allocator.free(foo_file);

    const match = registry.classify(foo_file).?;
    try testing.expectEqualStrings("pkg", match.tag);
    try testing.expectEqualStrings("a.doxa", match.relative);

    const key = (try registry.stableKeyFor(allocator, foo_file)).?;
    defer allocator.free(key);
    try testing.expectEqualStrings("pkg//a.doxa", key);

    // The sibling `pkg/foobar` is not under the `pkg/foo` root. A file outside
    // every root is a hard error, so classification returns null.
    const sibling_file = try std.fs.path.join(allocator, &.{ base, "pkg", "foobar", "a.doxa" });
    defer allocator.free(sibling_file);
    try testing.expect(registry.classify(sibling_file) == null);
    try testing.expect((try registry.stableKeyFor(allocator, sibling_file)) == null);
}

test "module graph: content is not identity — same bytes, distinct keys" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/a.doxa", .data = "same" });
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/b.doxa", .data = "same" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const root = try std.fs.path.join(allocator, &.{ base, "pkg" });
    defer allocator.free(root);

    var registry = try graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer registry.deinit();

    const a = try std.fs.path.join(allocator, &.{ base, "pkg", "a.doxa" });
    defer allocator.free(a);
    const b = try std.fs.path.join(allocator, &.{ base, "pkg", "b.doxa" });
    defer allocator.free(b);

    const a_key = (try registry.stableKeyFor(allocator, a)).?;
    defer allocator.free(a_key);
    const b_key = (try registry.stableKeyFor(allocator, b)).?;
    defer allocator.free(b_key);
    try testing.expect(!std.mem.eql(u8, a_key, b_key));
}

test "module graph: first matching root wins" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "outer/inner");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "outer/inner/a.doxa", .data = "x" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const outer = try std.fs.path.join(allocator, &.{ base, "outer" });
    defer allocator.free(outer);
    const inner = try std.fs.path.join(allocator, &.{ base, "outer", "inner" });
    defer allocator.free(inner);

    var registry = try graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "inner", .path = inner },
        .{ .tag = "outer", .path = outer },
    });
    defer registry.deinit();

    const file = try std.fs.path.join(allocator, &.{ base, "outer", "inner", "a.doxa" });
    defer allocator.free(file);

    try testing.expectEqualStrings("inner", registry.classify(file).?.tag);
}

test "module graph: duplicate root tags are rejected" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "one");
    try tmp.dir.createDirPath(testing.io, "two");

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const one = try std.fs.path.join(allocator, &.{ base, "one" });
    defer allocator.free(one);
    const two = try std.fs.path.join(allocator, &.{ base, "two" });
    defer allocator.free(two);

    try testing.expectError(error.DuplicateRootTag, graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "dup", .path = one },
        .{ .tag = "dup", .path = two },
    }));
}

test "module graph: a nonexistent root is an error" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const missing = try std.fs.path.join(allocator, &.{ base, "does-not-exist" });
    defer allocator.free(missing);

    try testing.expectError(error.InvalidRootPath, graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = missing },
    }));
}

test "module graph: records get immutable ids and are indexed by both keys" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const first = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    const second = try graph_store.addRecord("/pkg/b.doxa", "pkg//b.doxa", .doxa);

    try testing.expectEqual(@as(graph.ModuleId, 0), first.id);
    try testing.expectEqual(@as(graph.ModuleId, 1), second.id);

    try testing.expect(graph_store.findPhysical("/pkg/a.doxa").? == first);
    try testing.expect(graph_store.findStable("pkg//b.doxa").? == second);
    try testing.expect(graph_store.record(second.id) == second);
    try testing.expectEqual(@as(usize, 2), graph_store.count());
}

test "module graph: a stable key names exactly one record" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    _ = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);

    // A second physical file claiming the same stable key is a compiler error,
    // never a silent merge.
    try testing.expectError(error.DuplicateStableKey, graph_store.addRecord("/pkg/other.doxa", "pkg//a.doxa", .doxa));
}

test "module graph: generated records have no physical key" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const generated = try graph_store.addRecord(null, "pkg//main.doxa//zig/IO", .inline_zig);
    try testing.expect(generated.physical_key == null);
    try testing.expect(graph_store.findStable("pkg//main.doxa//zig/IO").? == generated);
    try testing.expect(graph_store.findPhysical("pkg//main.doxa//zig/IO") == null);
}

test "module graph: a generated record derives its key from the owner" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const owner = try graph_store.addRecord("/pkg/main.doxa", "pkg//main.doxa", .doxa);
    const zig_io = try graph_store.addGeneratedRecord(testing.allocator, owner, "IO");

    try testing.expect(zig_io.physical_key == null);
    try testing.expectEqualStrings("pkg//main.doxa//zig/IO", zig_io.stable_module_key);
    try testing.expect(graph_store.findStable("pkg//main.doxa//zig/IO").? == zig_io);

    // The same (owner, name) twice is a compiler bug, never a silent merge.
    try testing.expectError(error.DuplicateStableKey, graph_store.addGeneratedRecord(testing.allocator, owner, "IO"));
}

test "module graph: bindings are owner-scoped and mirror the public surface" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const a = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    const b = try graph_store.addRecord("/pkg/b.doxa", "pkg//b.doxa", .doxa);

    try graph_store.bindName(a, "Node", .{ .symbol = .{ .module = a.id, .name = "Node", .kind = .Type } }, .Public, null);
    try graph_store.bindName(a, "util", .{ .namespace = b.id }, .Private, null);
    // The same name in two modules is two bindings, each naming its own
    // defining module.
    try graph_store.bindName(b, "Node", .{ .symbol = .{ .module = b.id, .name = "Node", .kind = .Type } }, .Private, null);

    try testing.expectEqual(a.id, a.bindings.get("Node").?.binding.symbol.module);
    try testing.expectEqual(b.id, b.bindings.get("Node").?.binding.symbol.module);
    try testing.expectEqual(graph.SymbolKind.Type, a.bindings.get("Node").?.binding.symbol.kind);

    // Only public bindings appear in the public surface.
    try testing.expect(a.public_bindings.contains("Node"));
    try testing.expect(!b.public_bindings.contains("Node"));
    try testing.expect(!a.public_bindings.contains("util"));
    try testing.expectEqual(b.id, a.bindings.get("util").?.binding.namespace);
}

test "module graph: symbol keys are module-qualified" {
    const ctx = graph.SymbolKeyContext{};
    const a = graph.SymbolKey{ .module = 1, .name = "Node" };
    const b = graph.SymbolKey{ .module = 2, .name = "Node" };
    const a2 = graph.SymbolKey{ .module = 1, .name = "Node" };

    try testing.expect(ctx.eql(a, a2));
    try testing.expect(!ctx.eql(a, b));
    try testing.expectEqual(ctx.hash(a), ctx.hash(a2));
    try testing.expect(ctx.hash(a) != ctx.hash(b));
}

test "module graph: mangled symbols are deterministic, distinct, and decodable" {
    var g1 = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer g1.deinit();
    const a1 = try g1.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    const b1 = try g1.addRecord("/pkg/b.doxa", "pkg//b.doxa", .doxa);
    try g1.finalizeMangling();

    const a_node = try g1.mangle(testing.allocator, a1.id, .type, &.{"Node"});
    defer testing.allocator.free(a_node);
    const b_node = try g1.mangle(testing.allocator, b1.id, .type, &.{"Node"});
    defer testing.allocator.free(b_node);
    // The same name in two modules is two symbols.
    try testing.expect(!std.mem.eql(u8, a_node, b_node));

    // A kind tag separates a function `S__m` from a method `S.m`.
    const function = try g1.mangle(testing.allocator, a1.id, .function, &.{"S__m"});
    defer testing.allocator.free(function);
    const method = try g1.mangle(testing.allocator, a1.id, .method, &.{ "S", "m" });
    defer testing.allocator.free(method);
    try testing.expect(!std.mem.eql(u8, function, method));

    // Display shows the declared name; identity never does.
    try testing.expectEqualStrings("Node", graph.displayName(a_node));
    try testing.expectEqualStrings("m", graph.displayName(method));
    try testing.expectEqualStrings("plain", graph.displayName("plain"));

    // The tag derives from the stable key alone: independent of the checkout
    // path and of discovery order.
    var g2 = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer g2.deinit();
    _ = try g2.addRecord("/elsewhere/z.doxa", "pkg//z.doxa", .doxa);
    const b2 = try g2.addRecord("/elsewhere/b.doxa", "pkg//b.doxa", .doxa);
    try g2.finalizeMangling();
    const b2_node = try g2.mangle(testing.allocator, b2.id, .type, &.{"Node"});
    defer testing.allocator.free(b2_node);
    try testing.expectEqualStrings(b_node, b2_node);
}

test "module graph: components are framed by byte count and cannot forge a delimiter" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    const a = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    try graph_store.finalizeMangling();

    // A quoted Zig identifier may carry any bytes. `héllo` is six bytes, and
    // its length prefix counts bytes, so it decodes intact.
    const unicode = try graph_store.mangle(testing.allocator, a.id, .function, &.{"h\xc3\xa9llo"});
    defer testing.allocator.free(unicode);
    try testing.expect(std.mem.endsWith(u8, unicode, "__f6$h\xc3\xa9llo"));
    try testing.expectEqualStrings("h\xc3\xa9llo", graph.displayName(unicode));

    // `$`, `_`, and `:` inside a component are content, not structure: the
    // same bytes split differently are different symbols.
    const left = try graph_store.mangle(testing.allocator, a.id, .method, &.{ "S$1", "m" });
    defer testing.allocator.free(left);
    const right = try graph_store.mangle(testing.allocator, a.id, .method, &.{ "S", "1$m" });
    defer testing.allocator.free(right);
    try testing.expect(!std.mem.eql(u8, left, right));
    try testing.expectEqualStrings("1$m", graph.displayName(right));

    const underscored = try graph_store.mangle(testing.allocator, a.id, .method, &.{ "S_", "_m" });
    defer testing.allocator.free(underscored);
    const coloned = try graph_store.mangle(testing.allocator, a.id, .method, &.{ "S:", ":m" });
    defer testing.allocator.free(coloned);
    try testing.expectEqualStrings("_m", graph.displayName(underscored));
    try testing.expectEqualStrings(":m", graph.displayName(coloned));
}

test "module graph: a module-tag collision is extended, per record" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    const a = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    const b = try graph_store.addRecord("/pkg/b.doxa", "pkg//b.doxa", .doxa);
    const c = try graph_store.addRecord("/pkg/c.doxa", "pkg//c.doxa", .doxa);

    // A 64-bit collision cannot be found by search; force one.
    const forced = "0123456789abcdef";
    a.mangle_tag = forced;
    b.mangle_tag = forced;
    c.mangle_tag = "fedcba9876543210";
    try graph_store.disperseMangleTags();

    // Both colliding records are extended, each by its own stable key, and
    // keep the shared base; the record that did not collide is untouched.
    try testing.expect(std.mem.startsWith(u8, a.mangle_tag.?, forced ++ "_"));
    try testing.expect(std.mem.startsWith(u8, b.mangle_tag.?, forced ++ "_"));
    try testing.expectEqual(forced.len * 2 + 1, a.mangle_tag.?.len);
    try testing.expect(!std.mem.eql(u8, a.mangle_tag.?, b.mangle_tag.?));
    try testing.expectEqualStrings("fedcba9876543210", c.mangle_tag.?);
}

test "module graph: a root-qualified specifier is a tag, two slashes, and a path" {
    const std_entry = graph.rootSpecifier(graph.std_specifier).?;
    try testing.expectEqualStrings("std", std_entry.tag);
    try testing.expectEqualStrings("std.doxa", std_entry.relative);

    const nested = graph.rootSpecifier("inc0//lib/util.doxa").?;
    try testing.expectEqualStrings("inc0", nested.tag);
    try testing.expectEqualStrings("lib/util.doxa", nested.relative);

    // Anything else is an ordinary path.
    try testing.expect(graph.rootSpecifier("./util.doxa") == null);
    try testing.expect(graph.rootSpecifier("//server/share/a.doxa") == null);
    try testing.expect(graph.rootSpecifier("../pkg//a.doxa") == null);
    try testing.expect(graph.rootSpecifier("C:\\dev\\a.doxa") == null);
}

test "module graph: record pointers survive growth" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const retained = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);

    var i: usize = 0;
    while (i < 256) : (i += 1) {
        const key = try std.fmt.allocPrint(testing.allocator, "/pkg/m{d}.doxa", .{i});
        defer testing.allocator.free(key);
        const stable = try std.fmt.allocPrint(testing.allocator, "pkg//m{d}.doxa", .{i});
        defer testing.allocator.free(stable);
        _ = try graph_store.addRecord(key, stable, .doxa);
    }

    // A recursive resolution retains *ModuleRecord across ensure* calls; growth
    // must not relocate it, and its id must not change.
    try testing.expect(graph_store.findPhysical("/pkg/a.doxa").? == retained);
    try testing.expectEqual(@as(graph.ModuleId, 0), retained.id);
}

test "module graph: identity pairs a real path with its stable key" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/a.doxa", .data = "x" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const root = try std.fs.path.join(allocator, &.{ base, "pkg" });
    defer allocator.free(root);
    const file = try std.fs.path.join(allocator, &.{ base, "pkg", "a.doxa" });
    defer allocator.free(file);

    var graph_store = try graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer graph_store.deinit();

    const record = try graph_store.ensureRecord(file, .doxa);
    try testing.expectEqualStrings("pkg//a.doxa", record.stable_module_key);
    try testing.expectEqualStrings(file, record.physical_key.?);
}

test "module graph: two spellings of one file dedup to one record" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg/sub");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/a.doxa", .data = "x" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const root = try std.fs.path.join(allocator, &.{ base, "pkg" });
    defer allocator.free(root);

    var graph_store = try graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer graph_store.deinit();

    // The same file reached directly and through `sub/../`.
    const direct = try std.fs.path.join(allocator, &.{ base, "pkg", "a.doxa" });
    defer allocator.free(direct);
    const roundabout = try std.fs.path.join(allocator, &.{ base, "pkg", "sub", "..", "a.doxa" });
    defer allocator.free(roundabout);

    const direct_physical = try graph.physicalPath(testing.io, allocator, direct);
    defer allocator.free(direct_physical);
    const roundabout_physical = try graph.physicalPath(testing.io, allocator, roundabout);
    defer allocator.free(roundabout_physical);

    const first = try graph_store.ensureRecord(direct_physical, .doxa);
    const second = try graph_store.ensureRecord(roundabout_physical, .doxa);

    try testing.expect(first == second);
    try testing.expectEqual(@as(usize, 1), graph_store.count());
    try testing.expectEqualStrings("pkg//a.doxa", first.stable_module_key);
}

test "module graph: a symlink and its target are one record" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "pkg/a.doxa", .data = "x" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const root = try std.fs.path.join(allocator, &.{ base, "pkg" });
    defer allocator.free(root);

    var graph_store = try graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer graph_store.deinit();

    // Creating a symlink needs a privilege (Windows) or a supporting
    // filesystem; where it is unavailable, the dedup property is untestable.
    tmp.dir.symLink(testing.io, "a.doxa", "pkg/link.doxa", .{}) catch return error.SkipZigTest;

    const direct = try std.fs.path.join(allocator, &.{ base, "pkg", "a.doxa" });
    defer allocator.free(direct);
    const via_link = try std.fs.path.join(allocator, &.{ base, "pkg", "link.doxa" });
    defer allocator.free(via_link);

    // Real-path identity collapses the link and its target.
    const direct_physical = try graph.physicalPath(testing.io, allocator, direct);
    defer allocator.free(direct_physical);
    const link_physical = try graph.physicalPath(testing.io, allocator, via_link);
    defer allocator.free(link_physical);
    try testing.expectEqualStrings(direct_physical, link_physical);

    const first = try graph_store.ensureRecord(direct_physical, .doxa);
    const second = try graph_store.ensureRecord(link_physical, .doxa);
    try testing.expect(first == second);
    try testing.expectEqual(@as(usize, 1), graph_store.count());
    try testing.expectEqualStrings("pkg//a.doxa", first.stable_module_key);
}

const StageCtx = struct {
    runs: usize = 0,
    saw_parsing: bool = false,
    inner_in_progress: bool = false,
    graph: ?*graph.ModuleGraph = null,
    reporter: *Reporter,
};

fn testReporter() Reporter {
    return Reporter.init(testing.io, testing.allocator, .{ .log_to_stderr = false }, null);
}

fn succeedStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) anyerror!void {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    ctx.saw_parsing = record.status == .Parsing;
    record.source = "module body";
}

fn reenterStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) anyerror!void {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    const inner = ctx.graph.?.runStage(record, .Parse, ctx.reporter, ctx_ptr, reenterStage);
    ctx.inner_in_progress = std.meta.activeTag(inner) == .in_progress;
}

/// Reports a warning, then the error it fails with.
fn failStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) anyerror!void {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    _ = record;
    ctx.reporter.reportWarning(null, "W0", "noise before the failure", .{});
    ctx.reporter.reportCompileError(null, "E0", "the stage's error", .{});
    return error.StageFailed;
}

/// Reports an error but returns normally.
fn reportOnlyStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) anyerror!void {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    _ = record;
    ctx.reporter.reportCompileError(null, "E0", "reported, not returned", .{});
}

/// Returns an error without reporting one.
fn silentStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) anyerror!void {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    _ = record;
    return error.Silent;
}

test "module graph: a stage drives NotLoaded to Parsed exactly once" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    var reporter = testReporter();
    defer reporter.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    var ctx = StageCtx{ .reporter = &reporter };

    const first = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), succeedStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(first));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expect(ctx.saw_parsing);
    try testing.expectEqual(graph.ModuleStatus.Parsed, entry.status);
    try testing.expectEqualStrings("module body", entry.source.?);

    // A second ensure* is a no-op: the active stage is never re-entered once
    // complete, and the body runs at most once.
    const again = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), succeedStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(again));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
}

test "module graph: a parse re-entry reports in_progress, not ready" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    var reporter = testReporter();
    defer reporter.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    var ctx = StageCtx{ .graph = &graph_store, .reporter = &reporter };

    const result = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), reenterStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(result));
    try testing.expect(ctx.inner_in_progress);
    try testing.expectEqual(@as(usize, 1), ctx.runs);
}

test "module graph: a failed stage is sticky and points at its first error" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    var reporter = testReporter();
    defer reporter.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    var ctx = StageCtx{ .reporter = &reporter };

    const first = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), failStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(first));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expectEqual(graph.ModuleStatus.Failed, entry.status);
    try testing.expectEqual(graph.ModuleStage.Parse, entry.failed_stage.?);
    // The warning reported first is not the failure; the error is.
    try testing.expectEqual(@as(?graph.DiagnosticIndex, 1), entry.failure);

    // The second ensure* returns the same failure without emitting again.
    const second = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), failStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(second));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expectEqual(@as(usize, 2), reporter.diagnostics.items.len);
}

test "module graph: a stage fails on an error it reported, returned or not" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    var reporter = testReporter();
    defer reporter.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    var ctx = StageCtx{ .reporter = &reporter };

    const result = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), reportOnlyStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(result));
    try testing.expectEqual(@as(?graph.DiagnosticIndex, 0), entry.failure);
}

test "module graph: a stage that fails silently is reported as a compiler bug" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();
    var reporter = testReporter();
    defer reporter.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa", .doxa);
    var ctx = StageCtx{ .reporter = &reporter };

    const result = graph_store.runStage(entry, .Parse, &reporter, @ptrCast(&ctx), silentStage);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(result));
    try testing.expectEqual(@as(usize, 1), reporter.diagnostics.items.len);
    const diagnostic = reporter.diagnostics.items[0];
    try testing.expect(std.mem.indexOf(u8, diagnostic.message, "pkg//a.doxa") != null);
    try testing.expect(std.mem.indexOf(u8, diagnostic.message, "compiler bug") != null);
}

test "module graph: a file outside every root is a hard error" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(testing.io, "pkg");
    try tmp.dir.createDirPath(testing.io, "elsewhere");
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "elsewhere/loose.doxa", .data = "x" });

    const base = try tmpRealPath(&tmp, allocator);
    defer allocator.free(base);
    const root = try std.fs.path.join(allocator, &.{ base, "pkg" });
    defer allocator.free(root);
    const outside = try std.fs.path.join(allocator, &.{ base, "elsewhere", "loose.doxa" });
    defer allocator.free(outside);
    const outside_physical = try graph.physicalPath(testing.io, allocator, outside);
    defer allocator.free(outside_physical);

    var graph_store = try graph.ModuleGraph.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer graph_store.deinit();

    try testing.expectError(error.ModuleRootUnknown, graph_store.ensureRecord(outside_physical, .doxa));
    try testing.expectEqual(@as(usize, 0), graph_store.count());
}
