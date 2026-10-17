//! LLVM string escaping, shared by the emitter and the debug metadata writer.

const std = @import("std");

/// A metadata string's contents: LLVM spells a byte outside printable ASCII,
/// or a quote or backslash, as `\XX`.
pub fn write(bytes: []const u8, w: *std.Io.Writer) std.Io.Writer.Error!void {
    for (bytes) |c| {
        if (c >= 0x20 and c < 0x7f and c != '"' and c != '\\') {
            try w.writeByte(c);
        } else {
            try w.print("\\{X:0>2}", .{c});
        }
    }
}

/// The same escape, in `std.fmt`'s argument form (`"{f}"`).
pub fn escaped(bytes: []const u8) std.fmt.Alt([]const u8, write) {
    return .{ .data = bytes };
}
