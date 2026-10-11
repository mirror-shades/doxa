const std = @import("std");
const testing = std.testing;

const profiler_mod = @import("../src/utils/profiler.zig");
const Profiler = profiler_mod.Profiler;
const CountingAllocator = profiler_mod.CountingAllocator;

test "profiler: inactive records nothing" {
    var p = Profiler.init(testing.io, testing.allocator, false);
    defer p.deinit();

    p.begin("lex");
    p.end();

    try testing.expectEqual(@as(usize, 0), p.root.children.items.len);
}

test "profiler: repeated siblings aggregate into one node" {
    var p = Profiler.init(testing.io, testing.allocator, true);
    defer p.deinit();

    p.begin("build-obj");
    p.end();
    p.begin("build-obj");
    p.end();

    try testing.expectEqual(@as(usize, 1), p.root.children.items.len);
    const node = p.root.children.items[0];
    try testing.expectEqualStrings("build-obj", node.name);
    try testing.expectEqual(@as(u64, 2), node.count);
}

test "profiler: distinct siblings stay distinct" {
    var p = Profiler.init(testing.io, testing.allocator, true);
    defer p.deinit();

    p.begin("emit-ir");
    p.end();
    p.begin("link");
    p.end();

    try testing.expectEqual(@as(usize, 2), p.root.children.items.len);
}

test "profiler: nesting and self time exclude children" {
    var p = Profiler.init(testing.io, testing.allocator, true);
    defer p.deinit();

    p.begin("inline-zig");
    p.begin("build-obj");
    p.end();
    p.begin("build-obj");
    p.end();
    p.end();

    try testing.expectEqual(@as(usize, 1), p.root.children.items.len);
    const parent = p.root.children.items[0];
    try testing.expectEqual(@as(usize, 1), parent.children.items.len);
    const child = parent.children.items[0];

    try testing.expectEqual(@as(u64, 2), child.count);
    try testing.expect(parent.total_ns >= child.total_ns);
    try testing.expectEqual(parent.total_ns - child.total_ns, parent.selfNs());
}

test "profiler: CountingAllocator tracks allocated, freed and peak" {
    var counter = CountingAllocator.init(testing.allocator);
    const a = counter.allocator();
    const x = try a.alloc(u8, 100);
    const y = try a.alloc(u8, 50);
    a.free(x);
    const z = try a.alloc(u8, 10);
    a.free(y);
    a.free(z);
    try testing.expectEqual(@as(u64, 160), counter.allocated);
    try testing.expectEqual(@as(u64, 160), counter.freed);
    try testing.expectEqual(@as(u64, 0), counter.live);
    try testing.expectEqual(@as(u64, 150), counter.peak);
}

test "profiler: spans record bytes, self bytes and peak" {
    var counter = CountingAllocator.init(testing.allocator);
    const a = counter.allocator();
    var profiler = Profiler.init(testing.io, testing.allocator, true);
    defer profiler.deinit();
    profiler.attachCounter(&counter);

    profiler.begin("outer");
    const x = try a.alloc(u8, 64);
    profiler.begin("inner");
    const y = try a.alloc(u8, 32);
    a.free(y);
    profiler.end();
    a.free(x);
    const z = try a.alloc(u8, 8);
    a.free(z);
    profiler.end();

    const outer = profiler.root.children.items[0];
    const inner = outer.children.items[0];
    try testing.expectEqual(@as(u64, 104), outer.bytes);
    try testing.expectEqual(@as(u64, 72), outer.selfBytes());
    try testing.expectEqual(@as(u64, 96), outer.peak);
    try testing.expectEqual(@as(u64, 32), inner.bytes);
    try testing.expectEqual(@as(u64, 96), inner.peak);
}
