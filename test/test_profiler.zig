const std = @import("std");
const testing = std.testing;

const Profiler = @import("../src/utils/profiler.zig").Profiler;

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
