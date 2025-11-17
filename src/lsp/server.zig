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

pub fn run(allocator: std.mem.Allocator, options: ReporterOptions) !void {
    var reporter = Reporter.init(allocator, options);
    defer reporter.deinit();

    var server = Server.init(allocator, &reporter);
    defer server.deinit();

    try server.loop();
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

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) Server {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .documents = std.StringHashMap(Document).init(allocator),
            .shutdown_requested = false,
            .should_exit = false,
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
        var reader = stdin_reader.interface;

        while (!self.should_exit) {
            const payload = self.readMessage(&reader) catch |err| switch (err) {
                error.EndOfStream => break,
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

            const byte = std.io.Reader.takeByte(reader) catch |err| switch (err) {
                error.EndOfStream => break,
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

            if (line.len == 0) return error.EndOfStream;
            const trimmed = trimLine(line);
            if (trimmed.len == 0) break;

            if (std.ascii.startsWithIgnoreCase(trimmed, "content-length:")) {
                const parts = std.mem.trim(u8, trimmed["content-length:".len..], " \t");
                content_length = std.fmt.parseInt(usize, parts, 10) catch return error.InvalidMessage;
            }
        }

        const length = content_length orelse return error.InvalidMessage;
        const payload = try self.allocator.alloc(u8, length);
        _ = try std.io.Reader.readSliceShort(reader, payload);
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

        _ = self.performAnalysis(doc, uri) catch {};

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
        var stdout_buffer: [4096]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const writer = &stdout_writer.interface;
        const concatenated = try std.fmt.allocPrint(self.allocator, "Content-Length: {d}\r\n\r\n{s}", .{ payload.len, payload });
        defer self.allocator.free(concatenated);
        try writer.writeAll(concatenated);
        try writer.flush();
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
