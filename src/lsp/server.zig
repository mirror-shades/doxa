const std = @import("std");
const builtin = @import("builtin");
const reporting = @import("../utils/reporting.zig");
const Reporter = reporting.Reporter;
const ReporterOptions = reporting.ReporterOptions;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../parser/parser_types.zig").Parser;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const Errors = @import("../utils/errors.zig");

const JsonValue = std.json.Value;

pub const RunOptions = struct {
    reporter_options: ReporterOptions,
    trace_io: bool = false,
};

pub const DebugHarnessOptions = struct {
    reporter_options: ReporterOptions,
    script_path: []const u8,
};

const HARNESS_MAX_FILE_BYTES: usize = 4 * 1024 * 1024;

const ResponseSink = struct {
    context: *anyopaque,
    sendFn: *const fn (context: *anyopaque, payload: []const u8) anyerror!void,
};

const StdIoSink = struct {
    allocator: std.mem.Allocator,
    trace_io: bool,

    fn init(allocator: std.mem.Allocator, trace_io: bool) StdIoSink {
        return .{
            .allocator = allocator,
            .trace_io = trace_io,
        };
    }

    fn asResponseSink(self: *StdIoSink) ResponseSink {
        return .{
            .context = @ptrCast(self),
            .sendFn = StdIoSink.send,
        };
    }

    fn send(context: *anyopaque, payload: []const u8) !void {
        const self: *StdIoSink = @ptrCast(@alignCast(context));
        var stdout_buffer: [4096]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const writer = &stdout_writer.interface;
        const framed = try std.fmt.allocPrint(self.allocator, "Content-Length: {d}\r\n\r\n{s}", .{ payload.len, payload });
        defer self.allocator.free(framed);
        if (self.trace_io) {
            std.debug.print("[lsp-io] writing Content-Length: {d}\n", .{payload.len});
            std.debug.print("[lsp-io] >> {s}\n", .{payload});
        }
        try writer.writeAll(framed);
        try writer.flush();
    }
};

const CaptureSink = struct {
    allocator: std.mem.Allocator,
    responses: std.array_list.Managed([]u8),

    fn init(allocator: std.mem.Allocator) CaptureSink {
        return .{
            .allocator = allocator,
            .responses = std.array_list.Managed([]u8).init(allocator),
        };
    }

    fn deinit(self: *CaptureSink) void {
        for (self.responses.items) |payload| {
            self.allocator.free(payload);
        }
        self.responses.deinit();
    }

    fn asResponseSink(self: *CaptureSink) ResponseSink {
        return .{
            .context = @ptrCast(self),
            .sendFn = CaptureSink.send,
        };
    }

    fn send(context: *anyopaque, payload: []const u8) !void {
        const self: *CaptureSink = @ptrCast(@alignCast(context));
        const copy = try self.allocator.dupe(u8, payload);
        try self.responses.append(copy);
    }
};

pub fn run(allocator: std.mem.Allocator, options: RunOptions) !void {
    var reporter = Reporter.init(allocator, options.reporter_options);
    defer reporter.deinit();

    var sink = StdIoSink.init(allocator, options.trace_io);
    var server = Server.init(allocator, &reporter, sink.asResponseSink(), options.trace_io);
    defer server.deinit();

    try server.loop();
}

pub fn runDebugHarness(allocator: std.mem.Allocator, options: DebugHarnessOptions) !void {
    std.debug.print("=== Doxa LSP Debug Harness ===\n", .{});
    std.debug.print("Target file: {s}\n", .{options.script_path});

    var reporter = Reporter.init(allocator, options.reporter_options);
    defer reporter.deinit();

    const file_uri = try reporter.ensureFileUri(options.script_path);
    const document_text = try readFileAlloc(allocator, options.script_path, HARNESS_MAX_FILE_BYTES);
    defer allocator.free(document_text);

    var sink = CaptureSink.init(allocator);
    defer sink.deinit();

    var server = Server.init(allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    const initialize_request = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootUri\":null,\"capabilities\":{}}}";
    const initialized_notification = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}";
    const did_open_request = try buildDidOpenRequest(allocator, file_uri, document_text);
    defer allocator.free(did_open_request);

    try runHarnessMessage(allocator, &server, &sink, "initialize", initialize_request);
    try runHarnessMessage(allocator, &server, &sink, "initialized", initialized_notification);
    try runHarnessMessage(allocator, &server, &sink, "textDocument/didOpen", did_open_request);
}

const Document = struct {
    path: []const u8,
    text: []u8,
};

const Server = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    documents: std.StringHashMap(Document),
    shutdown_requested: bool,
    should_exit: bool,
    sink: ResponseSink,
    trace_io: bool,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, sink: ResponseSink, trace_io: bool) Server {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .documents = std.StringHashMap(Document).init(allocator),
            .shutdown_requested = false,
            .should_exit = false,
            .sink = sink,
            .trace_io = trace_io,
        };
    }

    pub fn deinit(self: *Server) void {
        var it = self.documents.iterator();
        while (it.next()) |entry| {
            self.freeDocument(entry.key_ptr.*, entry.value_ptr.*);
        }
        self.documents.deinit();
    }

    fn loop(self: *Server) !void {
        var stdin_buffer: [4096]u8 = undefined;
        const stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
        const reader = &stdin_reader.interface;

        while (!self.should_exit) {
            const payload = self.readMessage(reader) catch |err| switch (err) {
                error.EndOfStream => {
                    if (self.shutdown_requested) {
                        return;
                    } else {
                        std.Thread.sleep(10 * std.time.ns_per_ms);
                        continue;
                    }
                },
                error.ReadFailed => {
                    std.Thread.sleep(10 * std.time.ns_per_ms);
                    continue;
                },
                else => return err,
            };

            defer self.allocator.free(payload);
            try self.handlePayload(payload);
        }
    }

    fn readLineAlloc(self: *Server, reader: anytype) ![]u8 {
        var buffer: [4096]u8 = undefined;
        var len: usize = 0;

        while (true) {
            if (len >= buffer.len) return error.StreamTooLong;

            const byte = std.io.Reader.takeByte(@constCast(reader)) catch |err| switch (err) {
                error.EndOfStream => break,
                error.ReadFailed => {
                    // Handle pipe communication issues - retry after brief delay
                    std.Thread.sleep(1 * std.time.ns_per_ms);
                    continue;
                },
                else => return err,
            };

            if (byte == '\n') break;
            buffer[len] = byte;
            len += 1;
        }

        return self.allocator.dupe(u8, buffer[0..len]);
    }

    fn readMessage(self: *Server, reader: anytype) ![]u8 {
        var content_length: ?usize = null;

        while (true) {
            const line = try self.readLineAlloc(reader);
            defer self.allocator.free(line);

            if (line.len == 0) {
                if (self.trace_io) {
                    std.debug.print("[lsp-io] reached EOF while reading headers\n", .{});
                }
                return error.EndOfStream;
            }
            const trimmed = trimLine(line);
            if (self.trace_io) {
                if (trimmed.len == 0) {
                    std.debug.print("[lsp-io] header terminator detected\n", .{});
                } else {
                    std.debug.print("[lsp-io] header line: '{s}'\n", .{trimmed});
                }
            }
            if (trimmed.len == 0) {
                break;
            }

            if (std.ascii.startsWithIgnoreCase(trimmed, "content-length:")) {
                const parts = std.mem.trim(u8, trimmed["content-length:".len..], " \t");
                content_length = std.fmt.parseInt(usize, parts, 10) catch {
                    return error.InvalidMessage;
                };
                if (self.trace_io) {
                    std.debug.print("[lsp-io] parsed Content-Length: {d}\n", .{content_length.?});
                }
            }
        }

        const length = content_length orelse return error.InvalidMessage;
        if (self.trace_io) {
            std.debug.print("[lsp-io] reading payload ({d} bytes)\n", .{length});
        }
        const payload = try self.allocator.alloc(u8, length);
        _ = try std.io.Reader.readSliceShort(@constCast(reader), payload);
        if (self.trace_io) {
            std.debug.print("[lsp-io] << {s}\n", .{payload});
        }
        return payload;
    }

    fn handlePayload(self: *Server, payload: []const u8) !void {
        var parsed = std.json.parseFromSlice(JsonValue, self.allocator, payload, .{
            .duplicate_field_behavior = .use_last,
        }) catch {
            try self.sendErrorResponse(null, -32700, "Parse error");
            return;
        };
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) {
            try self.sendErrorResponse(null, -32600, "Invalid request");
            return;
        }

        const obj = root.object;
        const maybe_method = obj.get("method");
        if (maybe_method) |method_value| {
            if (method_value != .string) {
                try self.sendErrorResponse(obj.get("id"), -32600, "Invalid request");
                return;
            }

            const method = method_value.string;
            const params = obj.get("params");
            const id = obj.get("id");

            if (std.mem.eql(u8, method, "initialize")) {
                std.debug.print("INIT: Received initialize request\n", .{});
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleInitialize(id.?, params);
            } else if (std.mem.eql(u8, method, "shutdown")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleShutdown(id.?);
            } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
                try self.handleDidOpen(params);
            } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
                try self.handleDidChange(params);
            } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
                try self.handleDidClose(params);
            } else if (std.mem.eql(u8, method, "initialized")) {
                // No-op
            } else if (std.mem.eql(u8, method, "exit")) {
                self.should_exit = true;
            } else {
                if (id) |req_id| {
                    try self.sendErrorResponse(req_id, -32601, "Method not found");
                }
            }
        }
    }

    fn handleInitialize(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        _ = params;
        var buffer = std.array_list.Managed(u8).init(self.allocator);
        defer buffer.deinit();
        var writer = buffer.writer();

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try writeJsonValue(&writer, id);
        try writer.writeAll(",\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":1}},\"serverInfo\":{\"name\":\"Doxa\"}}}");

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
        std.debug.print("INIT: Sent initialize response\n", .{});
    }

    fn handleShutdown(self: *Server, id: JsonValue) !void {
        self.shutdown_requested = true;

        var buffer = std.array_list.Managed(u8).init(self.allocator);
        defer buffer.deinit();
        var writer = buffer.writer();

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try writeJsonValue(&writer, id);
        try writer.writeAll(",\"result\":null}");

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleDidOpen(self: *Server, params: ?JsonValue) !void {
        const params_value = params orelse return;
        if (params_value != .object) return;

        const params_obj = params_value.object;
        const doc_value = params_obj.get("textDocument") orelse return;
        if (doc_value != .object) return;

        const doc_obj = doc_value.object;
        const uri_value = doc_obj.get("uri") orelse return;
        const text_value = doc_obj.get("text") orelse return;
        if (uri_value != .string or text_value != .string) return;

        try self.storeDocument(uri_value.string, text_value.string);
        try self.analyzeAndPublish(uri_value.string);
    }

    fn handleDidChange(self: *Server, params: ?JsonValue) !void {
        const params_value = params orelse return;
        if (params_value != .object) return;
        const params_obj = params_value.object;

        const doc_value = params_obj.get("textDocument") orelse return;
        if (doc_value != .object) return;
        const doc_obj = doc_value.object;
        const uri_value = doc_obj.get("uri") orelse return;
        if (uri_value != .string) return;

        const changes_value = params_obj.get("contentChanges") orelse return;
        if (changes_value != .array or changes_value.array.items.len == 0) return;
        const first_change = changes_value.array.items[0];
        if (first_change != .object) return;

        const change_obj = first_change.object;
        const text_value = change_obj.get("text") orelse return;
        if (text_value != .string) return;

        try self.storeDocument(uri_value.string, text_value.string);
        try self.analyzeAndPublish(uri_value.string);
    }

    fn handleDidClose(self: *Server, params: ?JsonValue) !void {
        const params_value = params orelse return;
        if (params_value != .object) return;

        const doc_value = params_value.object.get("textDocument") orelse return;
        if (doc_value != .object) return;

        const uri_value = doc_value.object.get("uri") orelse return;
        if (uri_value != .string) return;

        self.removeDocument(uri_value.string);
        self.reporter.clearByFile(uri_value.string);
        self.reporter.dropPublishedDiagnostics(uri_value.string);
        try self.publishDiagnostics(uri_value.string);
    }

    fn storeDocument(self: *Server, uri: []const u8, text: []const u8) !void {
        const text_copy = try self.allocator.alloc(u8, text.len);
        @memcpy(text_copy, text);

        if (self.documents.getPtr(uri)) |existing| {
            self.allocator.free(existing.text);
            existing.text = text_copy;
            return;
        }

        const path = try reporting.convertUriToPath(self.allocator, uri);
        const key = try self.allocator.dupe(u8, uri);
        errdefer {
            self.allocator.free(key);
            self.allocator.free(path);
            self.allocator.free(text_copy);
        }

        const gop = try self.documents.getOrPut(key);
        if (gop.found_existing) unreachable;

        gop.value_ptr.* = .{
            .path = path,
            .text = text_copy,
        };
    }

    fn removeDocument(self: *Server, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |entry| {
            self.freeDocument(entry.key, entry.value);
        }
    }

    fn freeDocument(self: *Server, key: []const u8, doc: Document) void {
        self.allocator.free(key);
        self.allocator.free(doc.path);
        self.allocator.free(doc.text);
    }

    fn analyzeAndPublish(self: *Server, uri: []const u8) !void {
        const doc = self.documents.getPtr(uri) orelse return;
        self.reporter.clearByFile(uri);
        self.reporter.clearByFile(doc.path);

        self.performAnalysis(doc, uri) catch {};

        try self.publishDiagnostics(uri);
    }

    fn performAnalysis(self: *Server, doc: *Document, uri: []const u8) Errors.ErrorList!void {
        var memory_manager = try MemoryManager.init(self.allocator);
        defer memory_manager.deinit();

        var lexer = try LexicalAnalyzer.init(memory_manager.getAnalysisAllocator(), doc.text, doc.path, self.reporter);
        defer lexer.deinit();
        try lexer.initKeywords();

        var tokens = try lexer.lexTokens();
        defer tokens.deinit();

        var parser = Parser.init(memory_manager.getAnalysisAllocator(), tokens.items, doc.path, uri, self.reporter);
        defer parser.deinit();
        const statements = try parser.execute();

        var semantic = SemanticAnalyzer.init(memory_manager.getAnalysisAllocator(), self.reporter, &memory_manager, &parser);
        defer semantic.deinit();
        try semantic.analyze(statements);
    }

    fn publishDiagnostics(self: *Server, uri: []const u8) !void {
        const payload = try self.reporter.buildPublishDiagnosticsPayload(self.allocator, uri);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
        try self.reporter.markDiagnosticsPublished(uri, std.time.nanoTimestamp());
    }

    fn sendMessage(self: *Server, payload: []const u8) !void {
        try self.sink.sendFn(self.sink.context, payload);
    }

    fn sendErrorResponse(self: *Server, id: ?JsonValue, code: i64, message: []const u8) !void {
        var buffer = std.array_list.Managed(u8).init(self.allocator);
        defer buffer.deinit();
        var writer = buffer.writer();

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |value| {
            try writeJsonValue(&writer, value);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\"error\":{\"code\":");
        try writer.print("{d}", .{code});
        try writer.writeAll(",\"message\":");
        try writeJsonValue(&writer, message);
        try writer.writeAll("}}");

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }
};

fn trimLine(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') {
        return line[0 .. line.len - 1];
    }
    return line;
}

fn writeJsonValue(writer: anytype, value: anytype) !void {
    try writer.print("{f}", .{std.json.fmt(value, .{})});
}

fn runHarnessMessage(
    allocator: std.mem.Allocator,
    server: *Server,
    sink: *CaptureSink,
    label: []const u8,
    payload: []const u8,
) !void {
    std.debug.print("\n[harness] --> {s}\n", .{label});
    try prettyPrintJson(allocator, payload);
    try server.handlePayload(payload);
    try drainCapturedResponses(allocator, sink);
}

fn drainCapturedResponses(allocator: std.mem.Allocator, sink: *CaptureSink) !void {
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

    const pretty = jsonStringifyAlloc(allocator, parsed.value, .{ .whitespace = .indent_2 }) catch {
        std.debug.print("{s}\n", .{payload});
        return;
    };
    defer allocator.free(pretty);
    std.debug.print("{s}\n", .{pretty});
}

fn readFileAlloc(allocator: std.mem.Allocator, path: []const u8, max_bytes: usize) ![]u8 {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, max_bytes);
}

fn buildDidOpenRequest(allocator: std.mem.Allocator, uri: []const u8, text: []const u8) ![]u8 {
    const encoded_uri = try jsonStringifyAlloc(allocator, uri, .{});
    defer allocator.free(encoded_uri);

    const encoded_text = try jsonStringifyAlloc(allocator, text, .{});
    defer allocator.free(encoded_text);

    return try std.fmt.allocPrint(
        allocator,
        "{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{{\"textDocument\":{{\"uri\":{s},\"languageId\":\"doxa\",\"version\":1,\"text\":{s}}}}}}}",
        .{ encoded_uri, encoded_text },
    );
}

fn jsonStringifyAlloc(allocator: std.mem.Allocator, value: anytype, options: std.json.Stringify.Options) ![]u8 {
    var aw: std.io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try std.json.Stringify.value(value, options, &aw.writer);
    return aw.toOwnedSlice();
}
