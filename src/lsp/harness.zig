//! The LSP debug harness: drive one document through initialize/didOpen and
//! pretty-print the captured responses.

const std = @import("std");
const source_cache = @import("../utils/source_cache.zig");
const reporting = @import("../utils/reporting.zig");
const Reporter = reporting.Reporter;
const ReporterOptions = reporting.ReporterOptions;
const install = @import("../install.zig");
const constants = @import("../common/constants.zig");
const server_mod = @import("server.zig");
const transport = @import("transport.zig");

const JsonValue = std.json.Value;
const HARNESS_MAX_FILE_BYTES: usize = constants.MAX_LSP_FILE_BYTES;

pub const DebugHarnessOptions = struct {
    reporter_options: ReporterOptions,
    script_path: []const u8,
};

pub fn run(io: std.Io, allocator: std.mem.Allocator, options: DebugHarnessOptions) !void {
    std.debug.print("=== Doxa LSP Debug Harness ===\n", .{});
    std.debug.print("Target file: {s}\n", .{options.script_path});

    var src_cache = source_cache.SourceCache.init(allocator);
    defer src_cache.deinit();
    var reporter = Reporter.init(io, allocator, options.reporter_options, &src_cache);
    defer reporter.deinit();

    const file_uri = try reporter.ensureFileUri(io, options.script_path);
    const document_text = try readFileAlloc(io, allocator, options.script_path, HARNESS_MAX_FILE_BYTES);
    defer allocator.free(document_text);

    var sink = transport.CaptureSink.init(allocator);
    defer sink.deinit();

    const std_dir = try install.stdDir(io, allocator);
    defer allocator.free(std_dir);

    var server = server_mod.Server.init(allocator, &reporter, std_dir, sink.asResponseSink(), false);
    defer server.deinit();

    const initialize_request = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootUri\":null,\"capabilities\":{}}}";
    const initialized_notification = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}";
    const did_open_request = try buildDidOpenRequest(allocator, file_uri, document_text);
    defer allocator.free(did_open_request);

    try runHarnessMessage(io, allocator, &server, &sink, "initialize", initialize_request);
    try runHarnessMessage(io, allocator, &server, &sink, "initialized", initialized_notification);
    try runHarnessMessage(io, allocator, &server, &sink, "textDocument/didOpen", did_open_request);
}

fn runHarnessMessage(
    io: std.Io,
    allocator: std.mem.Allocator,
    server: *server_mod.Server,
    sink: *transport.CaptureSink,
    label: []const u8,
    payload: []const u8,
) !void {
    std.debug.print("\n[harness] --> {s}\n", .{label});
    try prettyPrintJson(allocator, payload);
    try server.handlePayload(io, payload);
    try drainCapturedResponses(allocator, sink);
}

fn drainCapturedResponses(allocator: std.mem.Allocator, sink: *transport.CaptureSink) !void {
    if (sink.responses.items.len == 0) {
        std.debug.print("[harness] (no responses)\n", .{});
        return;
    }

    for (sink.responses.items) |response| {
        std.debug.print("[harness] <-- response\n", .{});
        try prettyPrintJson(allocator, response);
        sink.allocator.free(response);
    }
    sink.responses.clearRetainingCapacity();
}

fn prettyPrintJson(allocator: std.mem.Allocator, payload: []const u8) !void {
    const parsed = std.json.parseFromSlice(JsonValue, allocator, payload, .{}) catch {
        std.debug.print("{s}\n", .{payload});
        return;
    };
    defer parsed.deinit();

    const pretty = transport.jsonStringifyAlloc(allocator, parsed.value, .{ .whitespace = .indent_2 }) catch {
        std.debug.print("{s}\n", .{payload});
        return;
    };
    defer allocator.free(pretty);
    std.debug.print("{s}\n", .{pretty});
}

fn readFileAlloc(io: std.Io, allocator: std.mem.Allocator, path: []const u8, max_bytes: usize) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(
        io,
        path,
        allocator,
        .limited(max_bytes),
    );
}

fn buildDidOpenRequest(allocator: std.mem.Allocator, uri: []const u8, text_value: []const u8) ![]u8 {
    const encoded_uri = try transport.jsonStringifyAlloc(allocator, uri, .{});
    defer allocator.free(encoded_uri);

    const encoded_text = try transport.jsonStringifyAlloc(allocator, text_value, .{});
    defer allocator.free(encoded_text);

    return try std.fmt.allocPrint(
        allocator,
        "{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{{\"textDocument\":{{\"uri\":{s},\"languageId\":\"doxa\",\"version\":1,\"text\":{s}}}}}}}",
        .{ encoded_uri, encoded_text },
    );
}
