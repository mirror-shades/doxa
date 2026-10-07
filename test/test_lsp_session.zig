//! The language server end to end: a scripted `doxa --lsp` session against the
//! installed binary. It drives the binary, so it runs with the program suites
//! (`suites.zig`), never in a unit root whose result is cached on its own
//! executable.

const std = @import("std");
const testing = std.testing;
const reporting = @import("reporting");

const harness = @import("harness.zig");

/// One `Content-Length`-framed LSP message.
fn frame(writer: *std.Io.Writer, payload: []const u8) !void {
    try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ payload.len, payload });
}

// The language server declares the same roots as a compilation, so a document
// that imports from `@std()` is analyzed against the installed std instead of
// failing with E7012 (outside every root). The probe's only diagnostic is a
// type error that needs `time.monotonic`'s declared return type to find.
test "lsp session: the server resolves @std()" {
    const allocator = testing.allocator;
    const io = testing.io;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const source =
        \\import time from @std()
        \\
        \\const t :: string is time.monotonic()
        \\
    ;
    try tmp.dir.writeFile(io, .{ .sub_path = "main.doxa", .data = source });

    var dir_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const dir = dir_buffer[0..try tmp.dir.realPath(io, &dir_buffer)];
    const path = try std.fs.path.join(allocator, &.{ dir, "main.doxa" });
    defer allocator.free(path);
    const uri = try reporting.convertPathToUri(allocator, path);
    defer allocator.free(uri);

    var did_open = std.Io.Writer.Allocating.init(allocator);
    defer did_open.deinit();
    try std.json.Stringify.value(.{
        .jsonrpc = "2.0",
        .method = "textDocument/didOpen",
        .params = .{ .textDocument = .{ .uri = uri, .languageId = "doxa", .version = 1, .text = source } },
    }, .{}, &did_open.writer);

    var input = std.Io.Writer.Allocating.init(allocator);
    defer input.deinit();
    try frame(&input.writer, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{}}}");
    try frame(&input.writer, "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}");
    try frame(&input.writer, did_open.written());
    try frame(&input.writer, "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}");
    try frame(&input.writer, "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);
    const capture = try doxa.run(allocator, io, .{ .args = &.{"--lsp"}, .cwd = dir, .stdin = input.written() });
    defer capture.deinit(allocator);
    if (!capture.succeeded()) {
        std.debug.print("doxa --lsp ended {any}:\n{s}\n", .{ capture.end, capture.stderr });
        return error.ServerFailed;
    }

    const Diagnostic = struct { code: []const u8 };
    const Publish = struct { method: []const u8, params: struct { diagnostics: []const Diagnostic } };

    // The last publish for the document is its settled state.
    var published: ?std.json.Parsed(Publish) = null;
    defer if (published) |parsed| parsed.deinit();
    var messages = std.mem.splitSequence(u8, capture.stdout, "Content-Length: ");
    while (messages.next()) |message| {
        const body_start = std.mem.indexOf(u8, message, "\r\n\r\n") orelse continue;
        const body = message[body_start + 4 ..];
        if (std.mem.indexOf(u8, body, "\"textDocument/publishDiagnostics\"") == null) continue;
        if (published) |parsed| parsed.deinit();
        published = null;
        published = try std.json.parseFromSlice(Publish, allocator, body, .{ .ignore_unknown_fields = true });
    }

    const diagnostics = (published orelse return error.NoDiagnosticsPublished).value.params.diagnostics;
    try testing.expectEqual(@as(usize, 1), diagnostics.len);
    try testing.expectEqualStrings("E1003", diagnostics[0].code);
}

// A client that closes the stream without `shutdown` is gone: the server
// stops at once with the protocol's failure status instead of waiting on a
// stream that will never deliver another message.
test "lsp session: a closed stream ends the server with status 1" {
    const allocator = testing.allocator;
    const io = testing.io;

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);
    const capture = try doxa.run(allocator, io, .{ .args = &.{"--lsp"}, .stdin = "" });
    defer capture.deinit(allocator);
    try testing.expectEqual(@as(u8, 1), switch (capture.end) {
        .exited => |status| status,
        else => return error.ServerDidNotExit,
    });
}
