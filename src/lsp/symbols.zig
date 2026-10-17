//! Document symbols and inlay hints.

const std = @import("std");
const model = @import("model.zig");
const transport = @import("transport.zig");

fn lspZeroBased(value: usize) usize {
    return if (value > 0) value - 1 else 0;
}

pub fn buildDocumentSymbolsPayload(env: model.FeatureContext, id: model.JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(env.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try transport.writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":[");

    var first = true;
    for (env.index.symbols.items) |sym| {
        if (!first) try writer.writeAll(",");
        first = false;

        // Compiler positions are 1-based; LSP is 0-based. An unset end (0)
        // means "same as start", mirroring `writeLspRange`.
        const start_line = lspZeroBased(sym.start_line);
        const start_char = lspZeroBased(sym.start_character);
        var end_line = if (sym.end_line == 0) start_line else lspZeroBased(sym.end_line);
        var end_char = if (sym.end_line == 0) start_char else lspZeroBased(sym.end_character);
        if (end_line < start_line or (end_line == start_line and end_char < start_char)) {
            end_line = start_line;
            end_char = start_char;
        }

        // The selection is the declaration name on the start line, clamped so
        // `selectionRange` is always contained in `range`.
        const sel_line = start_line;
        var sel_char = start_char + sym.name.len;
        if (end_line == start_line and sel_char > end_char) sel_char = end_char;
        if (sel_char < start_char) sel_char = start_char;

        try writer.writeAll("{\"name\":");
        try transport.writeJsonValue(writer, sym.name);
        try writer.writeAll(",\"kind\":");
        try writer.print("{d}", .{sym.kind});
        try writer.writeAll(",\"range\":{\"start\":{\"line\":");
        try writer.print("{d}", .{start_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{start_char});
        try writer.writeAll("},\"end\":{\"line\":");
        try writer.print("{d}", .{end_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{end_char});
        try writer.writeAll("}}");
        try writer.writeAll(",\"selectionRange\":{\"start\":{\"line\":");
        try writer.print("{d}", .{start_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{start_char});
        try writer.writeAll("},\"end\":{\"line\":");
        try writer.print("{d}", .{sel_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{sel_char});
        try writer.writeAll("}}}");
    }

    try writer.writeAll("]}");
    return buffer.toOwnedSlice();
}

fn jsonInteger(value: model.JsonValue) ?usize {
    return switch (value) {
        .integer => |i| if (i < 0) null else @intCast(i),
        .float => |f| if (f < 0) null else @intFromFloat(f),
        else => null,
    };
}

/// The requested line window from a `textDocument/inlayHint` (or any
/// range-bearing) request, or null when absent/invalid (meaning "no filter").
fn requestedLineRange(params: ?model.JsonValue) ?[2]usize {
    const p = params orelse return null;
    if (p != .object) return null;
    const range_value = p.object.get("range") orelse return null;
    if (range_value != .object) return null;
    const start = range_value.object.get("start") orelse return null;
    const end = range_value.object.get("end") orelse return null;
    if (start != .object or end != .object) return null;
    const start_line = jsonInteger(start.object.get("line") orelse return null) orelse return null;
    const end_line = jsonInteger(end.object.get("line") orelse return null) orelse return null;
    return .{ start_line, end_line };
}

/// Emits a type hint (`:: int`) for every inferred declaration in the
/// requested window. The hints are collected during analysis, so this is a
/// pure filter/serialize over the symbol index.
pub fn buildInlayHintPayload(env: model.FeatureContext, id: model.JsonValue, params: ?model.JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(env.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try transport.writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":[");

    const window = requestedLineRange(params);
    var first = true;
    for (env.index.inlay_hints.items) |hint| {
        if (window) |bounds| {
            if (hint.line < bounds[0] or hint.line > bounds[1]) continue;
        }
        if (!first) try writer.writeAll(",");
        first = false;
        try writer.writeAll("{\"position\":{\"line\":");
        try writer.print("{d}", .{hint.line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{hint.character});
        try writer.writeAll("},\"label\":");
        try transport.writeJsonValue(writer, hint.label);
        try writer.writeAll(",\"kind\":1,\"paddingLeft\":true}");
    }

    try writer.writeAll("]}");
    return buffer.toOwnedSlice();
}
