//! LSP snippet bodies: a callable's name plus one tab stop per parameter.

const std = @import("std");

pub fn write(w: *std.Io.Writer, name: []const u8, param_count: usize) !void {
    try w.print("{s}(", .{name});
    for (0..param_count) |i| {
        if (i > 0) try w.writeAll(", ");
        try w.print("${d}", .{i + 1});
    }
    try w.writeAll(")");
}
