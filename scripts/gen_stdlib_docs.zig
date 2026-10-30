const std = @import("std");
const Allocator = std.mem.Allocator;
const catalog = @import("catalog").catalog;

const usage_text =
    \\Usage: gen_stdlib_docs [std-dir] [output-file] [--check]
    \\  std-dir       Directory containing the standard library (default: std)
    \\  output-file   Markdown page to write (default: docs/stdlib-api.md)
    \\  --check       Write nothing; fail if the page differs from what std/ generates
    \\
;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();
    const args = try init.minimal.args.toSlice(allocator);

    const check = args.len > 1 and std.mem.eql(u8, args[args.len - 1], "--check");
    const positional = args[1 .. args.len - @intFromBool(check)];
    var std_dir: []const u8 = "std";
    var out_path: []const u8 = "docs/stdlib-api.md";
    if (positional.len > 0) std_dir = positional[0];
    if (positional.len > 1) out_path = positional[1];
    if (positional.len > 2) {
        std.debug.print("{s}", .{usage_text});
        std.process.exit(1);
    }

    const cat = catalog.load(allocator, init.io, std_dir) catch |err| {
        std.debug.print("gen_stdlib_docs: {s}\n", .{@errorName(err)});
        std.process.exit(1);
    };
    const page = try render(allocator, cat.modules);
    if (check) {
        const committed = std.Io.Dir.cwd().readFileAlloc(init.io, out_path, allocator, .unlimited) catch |err| {
            std.debug.print("gen_stdlib_docs: cannot read {s}: {s}\n", .{ out_path, @errorName(err) });
            std.process.exit(1);
        };
        // A Windows checkout may have converted the page, or the std sources
        // it quotes, to CRLF: compare content, not line endings.
        if (!std.mem.eql(u8, try withoutCarriageReturns(allocator, committed), try withoutCarriageReturns(allocator, page))) {
            std.debug.print("gen_stdlib_docs: {s} is stale; run `zig build docs`\n", .{out_path});
            std.process.exit(1);
        }
        std.debug.print("gen_stdlib_docs: {s} is current\n", .{out_path});
        return;
    }
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = out_path, .data = page });
    std.debug.print("gen_stdlib_docs: wrote {s} ({d} bytes)\n", .{ out_path, page.len });
}

fn withoutCarriageReturns(allocator: Allocator, text: []const u8) ![]u8 {
    const out = try allocator.alloc(u8, text.len);
    var len: usize = 0;
    for (text) |c| {
        if (c == '\r') continue;
        out[len] = c;
        len += 1;
    }
    return out[0..len];
}

fn render(allocator: Allocator, modules: []const catalog.Module) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.writeAll("# Standard Library API\n\n");
    try w.writeAll("> **Generated file.** Built from the Doxa standard library sources by\n");
    try w.writeAll("> `scripts/gen_stdlib_docs.zig`. Do not edit by hand; run `zig build docs`\n");
    try w.writeAll("> to regenerate it after changing anything under `std/`.\n\n");
    try w.writeAll("Each section is one standard-library module. Every entry shows its declared\n");
    try w.writeAll("signature, the description comment that precedes it in the source (when one\n");
    try w.writeAll("exists), and a collapsible copy of the full source declaration.\n\n");

    if (modules.len == 0) {
        try w.writeAll("_No public declarations found._\n");
        return out.toOwnedSlice();
    }

    for (modules) |m| {
        try w.print("## `std.{s}`\n\n", .{m.name});
        if (m.decls.len == 0) {
            try w.writeAll("_No public declarations._\n\n");
            continue;
        }
        for (m.decls) |d| try renderDecl(w, d, 3);
    }

    return out.toOwnedSlice();
}

fn renderDecl(w: *std.Io.Writer, d: catalog.Decl, level: usize) !void {
    var i: usize = 0;
    while (i < level) : (i += 1) try w.writeByte('#');
    try w.writeAll(" `");
    try w.writeAll(d.name);
    try w.writeAll("`\n\n```doxa\n");
    try w.writeAll(d.signature);
    try w.writeAll("\n```\n\n");

    if (d.doc) |doc| {
        if (doc.len > 0) {
            try w.writeAll(doc);
            try w.writeAll("\n\n");
        }
    }

    try w.writeAll("<details>\n<summary>Source</summary>\n\n```doxa\n");
    try w.writeAll(d.source);
    try w.writeAll("\n```\n\n</details>\n\n");

    for (d.members) |m| try renderDecl(w, m, level + 1);
}
