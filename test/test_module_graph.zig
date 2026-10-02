const std = @import("std");
const testing = std.testing;

const graph = @import("../src/module/graph.zig");
const harness = @import("harness.zig");

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

    const first = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");
    const second = try graph_store.addRecord("/pkg/b.doxa", "pkg//b.doxa");

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

    _ = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");

    // A second physical file claiming the same stable key is a compiler error,
    // never a silent merge.
    try testing.expectError(error.DuplicateStableKey, graph_store.addRecord("/pkg/other.doxa", "pkg//a.doxa"));
}

test "module graph: generated records have no physical key" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const generated = try graph_store.addRecord(null, "pkg//main.doxa//zig/IO");
    try testing.expect(generated.physical_key == null);
    try testing.expect(graph_store.findStable("pkg//main.doxa//zig/IO").? == generated);
    try testing.expect(graph_store.findPhysical("pkg//main.doxa//zig/IO") == null);
}

test "module graph: a generated record derives its key from the owner" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const owner = try graph_store.addRecord("/pkg/main.doxa", "pkg//main.doxa");
    const zig_io = try graph_store.addGeneratedRecord(testing.allocator, owner, "IO");

    try testing.expect(zig_io.physical_key == null);
    try testing.expectEqualStrings("pkg//main.doxa//zig/IO", zig_io.stable_module_key);
    try testing.expect(graph_store.findStable("pkg//main.doxa//zig/IO").? == zig_io);

    // The same (owner, name) twice is a compiler bug, never a silent merge.
    try testing.expectError(error.DuplicateStableKey, graph_store.addGeneratedRecord(testing.allocator, owner, "IO"));
}

test "module graph: record pointers survive growth" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const retained = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");

    var i: usize = 0;
    while (i < 256) : (i += 1) {
        const key = try std.fmt.allocPrint(testing.allocator, "/pkg/m{d}.doxa", .{i});
        defer testing.allocator.free(key);
        const stable = try std.fmt.allocPrint(testing.allocator, "pkg//m{d}.doxa", .{i});
        defer testing.allocator.free(stable);
        _ = try graph_store.addRecord(key, stable);
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

    var registry = try graph.RootRegistry.init(testing.io, allocator, &.{
        .{ .tag = "pkg", .path = root },
    });
    defer registry.deinit();

    const identity = (try registry.identityFor(allocator, file)).?;
    defer allocator.free(identity.physical_key);
    defer allocator.free(identity.stable_module_key);
    try testing.expectEqualStrings("pkg//a.doxa", identity.stable_module_key);
    try testing.expect(std.mem.endsWith(u8, identity.physical_key, "a.doxa"));
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

    const first = try graph_store.ensureRecord(direct_physical);
    const second = try graph_store.ensureRecord(roundabout_physical);

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

    const first = try graph_store.ensureRecord(direct_physical);
    const second = try graph_store.ensureRecord(link_physical);
    try testing.expect(first == second);
    try testing.expectEqual(@as(usize, 1), graph_store.count());
    try testing.expectEqualStrings("pkg//a.doxa", first.stable_module_key);
}

const StageCtx = struct {
    runs: usize = 0,
    saw_parsing: bool = false,
    inner_in_progress: bool = false,
    graph: ?*graph.ModuleGraph = null,
};

fn succeedStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) graph.StageOutcome {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    ctx.saw_parsing = record.status == .Parsing;
    record.source = "module body";
    return .ok;
}

fn reenterStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) graph.StageOutcome {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    const inner = ctx.graph.?.ensureParsed(ctx_ptr, reenterStage, record);
    ctx.inner_in_progress = std.meta.activeTag(inner) == .in_progress;
    return .ok;
}

fn failStage(ctx_ptr: *anyopaque, record: *graph.ModuleRecord) graph.StageOutcome {
    const ctx: *StageCtx = @ptrCast(@alignCast(ctx_ptr));
    ctx.runs += 1;
    _ = record;
    return .{ .failed = 7 };
}

test "module graph: ensureParsed drives NotLoaded to Parsed exactly once" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");
    var ctx = StageCtx{};

    const first = graph_store.ensureParsed(@ptrCast(&ctx), succeedStage, entry);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(first));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expect(ctx.saw_parsing);
    try testing.expectEqual(graph.ModuleStatus.Parsed, entry.status);
    try testing.expectEqualStrings("module body", entry.source.?);

    // A second ensure* is a no-op: the active stage is never re-entered once
    // complete, and the body runs at most once.
    const again = graph_store.ensureParsed(@ptrCast(&ctx), succeedStage, entry);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(again));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
}

test "module graph: a parse re-entry reports in_progress, not ready" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");
    var ctx = StageCtx{ .graph = &graph_store };

    const result = graph_store.ensureParsed(@ptrCast(&ctx), reenterStage, entry);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).ready, std.meta.activeTag(result));
    try testing.expect(ctx.inner_in_progress);
    try testing.expectEqual(@as(usize, 1), ctx.runs);
}

test "module graph: a failed stage is sticky and is not re-run" {
    var graph_store = try graph.ModuleGraph.init(testing.io, testing.allocator, &.{});
    defer graph_store.deinit();

    const entry = try graph_store.addRecord("/pkg/a.doxa", "pkg//a.doxa");
    var ctx = StageCtx{};

    const first = graph_store.ensureParsed(@ptrCast(&ctx), failStage, entry);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(first));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expectEqual(graph.ModuleStatus.Failed, entry.status);
    try testing.expectEqual(graph.ModuleStage.Parse, entry.failed_stage.?);
    try testing.expectEqual(@as(?graph.DiagnosticIndex, 7), entry.failure);

    // The second ensure* returns the same failure without emitting again.
    const second = graph_store.ensureParsed(@ptrCast(&ctx), failStage, entry);
    try testing.expectEqual(std.meta.Tag(graph.EnsureResult).failed, std.meta.activeTag(second));
    try testing.expectEqual(@as(usize, 1), ctx.runs);
    try testing.expectEqual(@as(?graph.DiagnosticIndex, 7), entry.failure);
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

    try testing.expectError(error.ModuleRootUnknown, graph_store.ensureRecord(outside_physical));
    try testing.expectEqual(@as(usize, 0), graph_store.count());
}

const checkout_program =
    \\module u from "./util.doxa"
    \\const v is u.helper()
    \\@print("{v}\n")
    \\
;

const checkout_util =
    \\public function helper() returns int {
    \\    return 42
    \\}
    \\
;

/// Compile the same two-file program inside `tmp` and return the optimized IR
/// from the cache. The program imports a sibling module so the emitter has a
/// real module reference to name.
fn emittedIr(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir) ![]u8 {
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "probe.doxa", .data = checkout_program });
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "util.doxa", .data = checkout_util });
    try tmp.dir.createDirPath(testing.io, "cache");

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(testing.io, &cwd_buffer)];

    const doxa = try harness.doxaExePath(allocator);
    defer allocator.free(doxa);

    const argv = [_][]const u8{ doxa, "run", "probe.doxa", "--emit-opt-ir", "--cache-dir=cache" };
    const result = try harness.runCommandCapture(allocator, &argv, cwd, null);
    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }
    if (result.exit_code != 0) {
        std.debug.print("checkout-independence compile failed ({d}):\n{s}\n", .{ result.exit_code, result.stderr });
        return error.CommandFailed;
    }

    return tmp.dir.readFileAlloc(testing.io, "cache/probe.opt.ll", allocator, .unlimited);
}

test "module graph: emitted IR is independent of the checkout directory" {
    const allocator = testing.allocator;

    var tmp_a = testing.tmpDir(.{});
    defer tmp_a.cleanup();
    var tmp_b = testing.tmpDir(.{});
    defer tmp_b.cleanup();

    // Two temp checkouts at different absolute paths emit byte-identical IR.
    const ir_a = try emittedIr(allocator, &tmp_a);
    defer allocator.free(ir_a);
    const ir_b = try emittedIr(allocator, &tmp_b);
    defer allocator.free(ir_b);

    try testing.expectEqualStrings(ir_a, ir_b);
}
