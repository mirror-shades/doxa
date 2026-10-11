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
/// With a `CountingAllocator` attached (`--profile` installs one beneath the
/// whole compiler), each span also records the bytes allocated inside it and
/// the high-water mark reached inside it. The profiler's own bookkeeping goes
/// through `allocator`, which is never the counted one, so it does not perturb
/// the numbers it reports.
///
/// When `active` is false every entry point is a no-op, so instrumented code
/// pays a single branch.
pub const Profiler = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    active: bool,
    counter: ?*CountingAllocator = null,
    root: Node,
    stack: std.array_list.Managed(Frame),

    pub const Node = struct {
        /// Owned display name, except for the synthetic root which is static.
        name: []const u8,
        total_ns: u64 = 0,
        count: u64 = 0,
        /// Bytes allocated inside the span, children included.
        bytes: u64 = 0,
        /// The most bytes live at once inside the span, over every call.
        peak: u64 = 0,
        children: std.array_list.Managed(*Node),

        /// Time attributed to this span that was not spent in any child span.
        pub fn selfNs(self: *const Node) u64 {
            var children_ns: u64 = 0;
            for (self.children.items) |child| children_ns +|= child.total_ns;
            return self.total_ns -| children_ns;
        }

        /// Bytes allocated inside this span that no child span accounts for.
        pub fn selfBytes(self: *const Node) u64 {
            var children_bytes: u64 = 0;
            for (self.children.items) |child| children_bytes +|= child.bytes;
            return self.bytes -| children_bytes;
        }
    };

    /// An open span: where it started, in time and in the byte counter.
    const Frame = struct {
        node: *Node,
        start_ns: u64,
        start_bytes: u64,
        /// The enclosing span's peak so far, restored (and raised) at `end`.
        outer_peak: u64,
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
            .stack = std.array_list.Managed(Frame).init(allocator),
        };
    }

    pub fn deinit(self: *Profiler) void {
        self.freeNode(&self.root);
        self.stack.deinit();
    }

    /// Attribute the bytes `counter` sees to the spans opened from now on.
    pub fn attachCounter(self: *Profiler, counter: *CountingAllocator) void {
        self.counter = counter;
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
            self.stack.items[self.stack.items.len - 1].node;
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
        var frame: Frame = .{ .node = node, .start_ns = 0, .start_bytes = 0, .outer_peak = 0 };
        if (self.counter) |counter| {
            frame.start_bytes = counter.allocated;
            frame.outer_peak = counter.span_peak;
            counter.span_peak = counter.live;
        }
        self.stack.append(frame) catch {
            if (self.counter) |counter| counter.span_peak = frame.outer_peak;
            self.active = false;
            return;
        };
        self.stack.items[self.stack.items.len - 1].start_ns = self.now();
    }

    /// Close the most recently opened span.
    pub fn end(self: *Profiler) void {
        if (!self.active) return;
        if (self.stack.items.len == 0) return;
        const frame = self.stack.pop().?;
        const node = frame.node;
        node.total_ns +|= self.now() -| frame.start_ns;
        node.count += 1;
        if (self.counter) |counter| {
            node.bytes +|= counter.allocated -| frame.start_bytes;
            node.peak = @max(node.peak, counter.span_peak);
            // The enclosing span reached at least this span's peak.
            counter.span_peak = @max(frame.outer_peak, counter.span_peak);
        }
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
        if (self.counter) |counter| {
            var buf: [32]u8 = undefined;
            std.debug.print("Bytes are what the compiler drew from its backing allocator. The\n", .{});
            std.debug.print("analysis arenas grow in whole chunks, so a span shows arena growth,\n", .{});
            std.debug.print("not its logical allocations. Peak: {s}.\n", .{byteText(&buf, counter.peak)});
        }
        std.debug.print("\n", .{});

        for (self.root.children.items) |child| self.printNode(child, 0, total);

        std.debug.print("\nSelf time (exclusive):\n", .{});

        var nodes = std.array_list.Managed(*Node).init(self.allocator);
        defer nodes.deinit();
        for (self.root.children.items) |child| self.collect(child, &nodes);
        std.mem.sort(*Node, nodes.items, {}, lessBySelf);

        for (nodes.items) |node| {
            var buf: [32]u8 = undefined;
            std.debug.print("  {s}\t{s}\t{d:.1}%", .{
                node.name,
                durationText(&buf, node.selfNs()),
                percentOf(node.selfNs(), total),
            });
            if (self.counter != null) std.debug.print("\t{s}", .{byteText(&buf, node.selfBytes())});
            std.debug.print("\t{d} call{s}\n", .{ node.count, if (node.count == 1) "" else "s" });
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
        if (self.counter) |counter| {
            try js.objectField("bytes");
            try js.write(counter.allocated);
            try js.objectField("peak");
            try js.write(counter.peak);
        }
        try js.objectField("spans");
        try js.beginArray();
        for (self.root.children.items) |child| try stringifyNode(&js, child, self.counter != null);
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
        if (self.counter != null) {
            var peak_buf: [32]u8 = undefined;
            std.debug.print("\t{s}\tpeak {s}", .{ byteText(&buf, node.bytes), byteText(&peak_buf, node.peak) });
        }
        if (node.count > 1) std.debug.print("\t({d} calls)", .{node.count});
        std.debug.print("\n", .{});

        for (node.children.items) |child| self.printNode(child, depth + 1, total);
    }

    fn collect(self: *Profiler, node: *Node, out: *std.array_list.Managed(*Node)) void {
        out.append(node) catch return;
        for (node.children.items) |child| self.collect(child, out);
    }
};

/// Counts the bytes a child allocator hands out and takes back. `--profile`
/// installs one beneath the whole compiler; without it none exists, so the
/// compiler pays nothing. The counters are plain integers: the compiler
/// allocates from one thread.
pub const CountingAllocator = struct {
    child: std.mem.Allocator,
    /// Bytes ever allocated; a growing resize adds its growth.
    allocated: u64 = 0,
    /// Bytes ever freed; a shrinking resize adds its shrink.
    freed: u64 = 0,
    /// Bytes live now.
    live: u64 = 0,
    /// The most bytes ever live at once.
    peak: u64 = 0,
    /// The most bytes live at once since the innermost open span began.
    span_peak: u64 = 0,

    pub fn init(child: std.mem.Allocator) CountingAllocator {
        return .{ .child = child };
    }

    pub fn allocator(self: *CountingAllocator) std.mem.Allocator {
        return .{ .ptr = self, .vtable = &vtable };
    }

    const vtable: std.mem.Allocator.VTable = .{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn grow(self: *CountingAllocator, n: usize) void {
        self.allocated += n;
        self.live += n;
        self.peak = @max(self.peak, self.live);
        self.span_peak = @max(self.span_peak, self.live);
    }

    fn shrink(self: *CountingAllocator, n: usize) void {
        self.freed += n;
        self.live -= n;
    }

    fn resized(self: *CountingAllocator, old_len: usize, new_len: usize) void {
        if (new_len > old_len) self.grow(new_len - old_len) else self.shrink(old_len - new_len);
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        const ptr = self.child.rawAlloc(len, alignment, ret_addr) orelse return null;
        self.grow(len);
        return ptr;
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        if (!self.child.rawResize(memory, alignment, new_len, ret_addr)) return false;
        self.resized(memory.len, new_len);
        return true;
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        const ptr = self.child.rawRemap(memory, alignment, new_len, ret_addr) orelse return null;
        self.resized(memory.len, new_len);
        return ptr;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        self.child.rawFree(memory, alignment, ret_addr);
        self.shrink(memory.len);
    }
};

fn stringifyNode(js: *std.json.Stringify, node: *Profiler.Node, counted: bool) !void {
    try js.beginObject();
    try js.objectField("name");
    try js.write(node.name);
    try js.objectField("total_ns");
    try js.write(node.total_ns);
    try js.objectField("self_ns");
    try js.write(node.selfNs());
    try js.objectField("count");
    try js.write(node.count);
    if (counted) {
        try js.objectField("bytes");
        try js.write(node.bytes);
        try js.objectField("self_bytes");
        try js.write(node.selfBytes());
        try js.objectField("peak");
        try js.write(node.peak);
    }
    if (node.children.items.len > 0) {
        try js.objectField("children");
        try js.beginArray();
        for (node.children.items) |child| try stringifyNode(js, child, counted);
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

fn byteText(buf: *[32]u8, bytes: u64) []const u8 {
    const b: f64 = @floatFromInt(bytes);
    if (bytes >= 1 << 30) return std.fmt.bufPrint(buf, "{d:.2}GiB", .{b / (1 << 30)}) catch "?";
    if (bytes >= 1 << 20) return std.fmt.bufPrint(buf, "{d:.2}MiB", .{b / (1 << 20)}) catch "?";
    if (bytes >= 1 << 10) return std.fmt.bufPrint(buf, "{d:.1}KiB", .{b / (1 << 10)}) catch "?";
    return std.fmt.bufPrint(buf, "{d}B", .{bytes}) catch "?";
}
