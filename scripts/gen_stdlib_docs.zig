const std = @import("std");
const Allocator = std.mem.Allocator;
const catalog = @import("catalog");

const usage_text =
    \\Usage: gen_stdlib_docs [std-dir] [output-file]
    \\  std-dir       Directory containing the standard library (default: std)
    \\  output-file   Markdown page to write (default: docs/stdlib-api.md)
    \\
;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();
    const args = try init.minimal.args.toSlice(allocator);

    var std_dir: []const u8 = "std";
    var out_path: []const u8 = "docs/stdlib-api.md";
    if (args.len > 1) std_dir = args[1];
    if (args.len > 2) out_path = args[2];
    if (args.len > 3) {
        std.debug.print("{s}", .{usage_text});
        std.process.exit(1);
    }

    const cat = catalog.load(allocator, init.io, std_dir) catch |err| {
        std.debug.print("gen_stdlib_docs: {s}\n", .{@errorName(err)});
        std.process.exit(1);
    };
    const page = try render(allocator, cat.modules);
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = out_path, .data = page });
    std.debug.print("gen_stdlib_docs: wrote {s} ({d} bytes)\n", .{ out_path, page.len });
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
