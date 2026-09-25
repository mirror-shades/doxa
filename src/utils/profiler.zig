const std = @import("std");

/// Opt-in hierarchical profiler.
///
/// Spans nest using `begin`/`end` and are timed with the monotonic clock.
/// Repeated sibling spans that share a name aggregate into one node (total
/// time + call count), which collapses fan-out steps such as the per-module
/// inline-Zig `zig build-obj` spawns. Each node's exclusive (self) time is
/// derived by subtracting its children, so the summary surfaces where wall
/// time is actually spent rather than only the outermost bracket.
///
/// When `active` is false every entry point is a no-op, so instrumented code
/// pays a single branch.
pub const Profiler = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    active: bool,
    root: Node,
    stack: std.array_list.Managed(*Node),
    starts: std.array_list.Managed(u64),

    pub const Node = struct {
        /// Owned display name, except for the synthetic root which is static.
        name: []const u8,
        total_ns: u64 = 0,
        count: u64 = 0,
        children: std.array_list.Managed(*Node),

        /// Time attributed to this span that was not spent in any child span.
        pub fn selfNs(self: *const Node) u64 {
            var children_ns: u64 = 0;
            for (self.children.items) |child| children_ns +|= child.total_ns;
            return self.total_ns -| children_ns;
        }
    };

    pub fn init(io: std.Io, allocator: std.mem.Allocator, active: bool) Profiler {
        return .{
            .io = io,
            .allocator = allocator,
            .active = active,
            .root = .{
                .name = "total",
                .children = std.array_list.Managed(*Node).init(allocator),
            },
            .stack = std.array_list.Managed(*Node).init(allocator),
            .starts = std.array_list.Managed(u64).init(allocator),
        };
    }

    pub fn deinit(self: *Profiler) void {
        self.freeNode(&self.root);
        self.stack.deinit();
        self.starts.deinit();
    }

    fn freeNode(self: *Profiler, node: *Node) void {
        for (node.children.items) |child| self.freeNode(child);
        node.children.deinit();
        if (node != &self.root) {
            self.allocator.free(node.name);
            self.allocator.destroy(node);
        }
    }

    fn now(self: *Profiler) u64 {
        const ns: i96 = std.Io.Timestamp.now(self.io, .awake).nanoseconds;
        return @intCast(ns);
    }

    fn current(self: *Profiler) *Node {
        return if (self.stack.items.len == 0)
            &self.root
        else
            self.stack.items[self.stack.items.len - 1];
    }

    fn findOrAddChild(self: *Profiler, parent: *Node, name: []const u8) ?*Node {
        for (parent.children.items) |child| {
            if (std.mem.eql(u8, child.name, name)) return child;
        }
        const node = self.allocator.create(Node) catch return null;
        node.* = .{
            .name = self.allocator.dupe(u8, name) catch {
                self.allocator.destroy(node);
                return null;
            },
            .children = std.array_list.Managed(*Node).init(self.allocator),
        };
        parent.children.append(node) catch {
            self.allocator.free(node.name);
            self.allocator.destroy(node);
            return null;
        };
        return node;
    }

    /// Open a span nested under the currently open one. Must be paired with
    /// `end`. No-op unless profiling is active.
    pub fn begin(self: *Profiler, name: []const u8) void {
        if (!self.active) return;
        const parent = self.current();
        const node = self.findOrAddChild(parent, name) orelse {
            self.active = false;
            return;
        };
        const start = self.now();
        self.stack.append(node) catch {
            self.active = false;
            return;
        };
        self.starts.append(start) catch {
            _ = self.stack.pop();
            self.active = false;
            return;
        };
    }

    /// Close the most recently opened span.
    pub fn end(self: *Profiler) void {
        if (!self.active) return;
        if (self.stack.items.len == 0) return;
        const node = self.stack.pop().?;
        const start = self.starts.pop().?;
        node.total_ns +|= self.now() -| start;
        node.count += 1;
    }

    fn rootTotal(self: *Profiler) u64 {
        var total: u64 = 0;
        for (self.root.children.items) |child| total +|= child.total_ns;
        return total;
    }

    /// Print the span tree followed by a self-time summary to stderr.
    pub fn dump(self: *Profiler) !void {
        if (!self.active) return;

        const total = self.rootTotal();

        std.debug.print("\n", .{});
        std.debug.print("==========================================\n", .{});
        std.debug.print("Profile\n", .{});
        std.debug.print("==========================================\n", .{});
        std.debug.print("\n", .{});

        for (self.root.children.items) |child| self.printNode(child, 0, total);

        std.debug.print("\nSelf time (exclusive):\n", .{});

        var nodes = std.array_list.Managed(*Node).init(self.allocator);
        defer nodes.deinit();
        for (self.root.children.items) |child| self.collect(child, &nodes);
        std.mem.sort(*Node, nodes.items, {}, lessBySelf);

        for (nodes.items) |node| {
            var buf: [32]u8 = undefined;
            std.debug.print("  {s}\t{s}\t{d:.1}%\t{d} call{s}\n", .{
                node.name,
                durationText(&buf, node.selfNs()),
                percentOf(node.selfNs(), total),
                node.count,
                if (node.count == 1) "" else "s",
            });
        }

        std.debug.print("\n", .{});
    }

    /// Write the span tree as JSON to `path`.
    pub fn writeJsonReport(self: *Profiler, path: []const u8) !void {
        if (!self.active) return;

        var out = std.Io.Writer.Allocating.init(self.allocator);
        defer out.deinit();
        var js: std.json.Stringify = .{
            .writer = &out.writer,
            .options = .{ .whitespace = .indent_2 },
        };

        try js.beginObject();
        try js.objectField("total_ns");
        try js.write(self.rootTotal());
        try js.objectField("spans");
        try js.beginArray();
        for (self.root.children.items) |child| try stringifyNode(&js, child);
        try js.endArray();
        try js.endObject();

        try std.Io.Dir.cwd().writeFile(self.io, .{ .sub_path = path, .data = out.written() });
    }

    fn printNode(self: *Profiler, node: *Node, depth: usize, total: u64) void {
        var indent_buf: [64]u8 = undefined;
        const capped = @min(depth, indent_buf.len / 2);
        var i: usize = 0;
        while (i < capped) : (i += 1) {
            indent_buf[i * 2] = ' ';
            indent_buf[i * 2 + 1] = ' ';
        }
        const indent = indent_buf[0 .. capped * 2];

        var buf: [32]u8 = undefined;
        std.debug.print("{s}{s}\t{s}\t{d:.1}%", .{
            indent,
            node.name,
            durationText(&buf, node.total_ns),
            percentOf(node.total_ns, total),
        });
        if (node.count > 1) std.debug.print("\t({d} calls)", .{node.count});
        std.debug.print("\n", .{});

        for (node.children.items) |child| self.printNode(child, depth + 1, total);
    }

    fn collect(self: *Profiler, node: *Node, out: *std.array_list.Managed(*Node)) void {
        out.append(node) catch return;
        for (node.children.items) |child| self.collect(child, out);
    }
};

fn stringifyNode(js: *std.json.Stringify, node: *Profiler.Node) !void {
    try js.beginObject();
    try js.objectField("name");
    try js.write(node.name);
    try js.objectField("total_ns");
    try js.write(node.total_ns);
    try js.objectField("self_ns");
    try js.write(node.selfNs());
    try js.objectField("count");
    try js.write(node.count);
    if (node.children.items.len > 0) {
        try js.objectField("children");
        try js.beginArray();
        for (node.children.items) |child| try stringifyNode(js, child);
        try js.endArray();
    }
    try js.endObject();
}

fn lessBySelf(_: void, a: *Profiler.Node, b: *Profiler.Node) bool {
    return a.selfNs() > b.selfNs();
}

fn percentOf(part: u64, whole: u64) f64 {
    if (whole == 0) return 0;
    return @as(f64, @floatFromInt(part)) / @as(f64, @floatFromInt(whole)) * 100.0;
}

fn durationText(buf: *[32]u8, ns: u64) []const u8 {
    if (ns >= std.time.ns_per_s) {
        return std.fmt.bufPrint(buf, "{d:.3}s", .{@as(f64, @floatFromInt(ns)) / std.time.ns_per_s}) catch "?";
    }
    if (ns >= std.time.ns_per_ms) {
        return std.fmt.bufPrint(buf, "{d:.3}ms", .{@as(f64, @floatFromInt(ns)) / std.time.ns_per_ms}) catch "?";
    }
    if (ns >= std.time.ns_per_us) {
        return std.fmt.bufPrint(buf, "{d:.2}µs", .{@as(f64, @floatFromInt(ns)) / std.time.ns_per_us}) catch "?";
    }
    return std.fmt.bufPrint(buf, "{d}ns", .{ns}) catch "?";
}
