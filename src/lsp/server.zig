//! The language server session: protocol loop, document store, analysis, and
//! request dispatch. Feature-specific payloads live in sibling modules.

const std = @import("std");
const source_cache = @import("../utils/source_cache.zig");
const reporting = @import("../utils/reporting.zig");
const Reporter = reporting.Reporter;
const ReporterOptions = reporting.ReporterOptions;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../parser/parser_types.zig").Parser;
const module_graph = @import("../module/graph.zig");
const install = @import("../install.zig");
const ModuleLoader = @import("../module/loader.zig").ModuleLoader;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const Errors = @import("../utils/errors.zig");
const stdlib = @import("../stdlib/catalog.zig");
const completion = @import("completion.zig");
const hover = @import("hover.zig");
const index = @import("index.zig");
const model = @import("model.zig");
const signature = @import("signature.zig");
const symbols = @import("symbols.zig");
const text_util = @import("text.zig");
const transport = @import("transport.zig");

const JsonValue = model.JsonValue;
const Document = model.Document;
const ResponseSink = transport.ResponseSink;

pub const RunOptions = struct {
    reporter_options: ReporterOptions,
    trace_io: bool = false,
};

/// Serve the protocol over stdio until the client sends `exit` or closes the
/// stream. Returns the exit status the protocol prescribes: 0 when the client
/// sent `shutdown` first, 1 otherwise (an `exit` without `shutdown`, or a
/// client that went away).
pub fn run(io: std.Io, allocator: std.mem.Allocator, options: RunOptions) !u8 {
    var cache = source_cache.SourceCache.init(allocator);
    defer cache.deinit();
    var reporter = Reporter.init(io, allocator, options.reporter_options, &cache);
    defer reporter.deinit();

    const std_dir = try install.stdDir(io, allocator);
    defer allocator.free(std_dir);

    var sink = transport.StdIoSink.init(io, options.trace_io);
    var server = Server.init(allocator, &reporter, std_dir, sink.asResponseSink(), options.trace_io);
    defer server.deinit();

    try server.loop(io);
    return if (server.shutdown_requested) 0 else 1;
}

pub const Server = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    /// The standard library: the `std` root of every document's module graph,
    /// and the directory the completion catalog is read from. Borrowed.
    std_dir: []const u8,
    documents: std.StringHashMap(Document),
    symbol_index: model.SymbolIndex,
    stdlib_arena: std.heap.ArenaAllocator,
    /// The loaded standard-library catalog; `null` until the first completion
    /// or hover. On load failure it is set to an empty catalog so the (failed)
    /// filesystem lookup happens at most once per session.
    stdlib: ?stdlib.Catalog,
    shutdown_requested: bool,
    should_exit: bool,
    sink: ResponseSink,
    trace_io: bool,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, std_dir: []const u8, sink: ResponseSink, trace_io: bool) Server {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .std_dir = std_dir,
            .documents = std.StringHashMap(Document).init(allocator),
            .symbol_index = model.SymbolIndex.init(allocator),
            .stdlib_arena = std.heap.ArenaAllocator.init(allocator),
            .stdlib = null,
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
        self.symbol_index.deinit();
        self.stdlib_arena.deinit();
    }

    pub fn features(self: *Server) model.FeatureContext {
        return .{
            .allocator = self.allocator,
            .documents = &self.documents,
            .index = &self.symbol_index,
            .stdlib = if (self.stdlib) |*cat| cat else null,
        };
    }

    /// Loads the standard-library catalog from `std_dir` once. Failure is
    /// non-fatal: completion degrades to imported symbols.
    fn ensureStdlib(self: *Server, io: std.Io) void {
        if (self.stdlib != null) return;

        self.stdlib = stdlib.load(self.stdlib_arena.allocator(), io, self.std_dir) catch blk: {
            std.debug.print("doxa-lsp: standard library catalog not found at '{s}'; std completion disabled\n", .{self.std_dir});
            break :blk .{ .modules = &.{} };
        };
    }

    fn loop(self: *Server, io: std.Io) !void {
        var stdin_buffer: [4096]u8 = undefined;
        var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
        const reader = &stdin_reader.interface;

        while (!self.should_exit) {
            const payload = transport.readMessage(self.allocator, self.trace_io, reader) catch |err| switch (err) {
                // The client closed its end: there is nothing left to serve.
                error.EndOfStream => return,
                else => return err,
            };

            defer self.allocator.free(payload);
            try self.handlePayload(io, payload);
        }
    }

    pub fn handlePayload(self: *Server, io: std.Io, payload: []const u8) !void {
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
                try self.handleDidOpen(io, params);
            } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
                try self.handleDidChange(io, params);
            } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
                try self.handleDidClose(io, params);
            } else if (std.mem.eql(u8, method, "textDocument/completion")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleCompletion(io, id.?, params);
            } else if (std.mem.eql(u8, method, "textDocument/signatureHelp")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleSignatureHelp(io, id.?, params);
            } else if (std.mem.eql(u8, method, "textDocument/inlayHint")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleInlayHint(id.?, params);
            } else if (std.mem.eql(u8, method, "textDocument/hover")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleHover(io, id.?, params);
            } else if (std.mem.eql(u8, method, "textDocument/documentSymbol")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleDocumentSymbol(id.?, params);
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

    fn handleInitialize(
        self: *Server,
        id: JsonValue,
        params: ?JsonValue,
    ) !void {
        _ = params;

        var buffer: std.Io.Writer.Allocating = .init(self.allocator);
        errdefer buffer.deinit();

        const writer = &buffer.writer;

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try transport.writeJsonValue(writer, id);
        try writer.writeAll(
            ",\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":1},\"completionProvider\":{\"triggerCharacters\":[\"@\",\".\"]},\"signatureHelpProvider\":{\"triggerCharacters\":[\"(\",\",\"]},\"inlayHintProvider\":true,\"hoverProvider\":true,\"documentSymbolProvider\":true},\"serverInfo\":{\"name\":\"Doxa\"}}}",
        );

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);

        try self.sendMessage(payload);

        std.debug.print("INIT: Sent initialize response\n", .{});
    }

    fn handleShutdown(self: *Server, id: JsonValue) !void {
        self.shutdown_requested = true;

        var buffer: std.Io.Writer.Allocating = .init(self.allocator);
        errdefer buffer.deinit();

        const writer = &buffer.writer;

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try transport.writeJsonValue(writer, id);
        try writer.writeAll(",\"result\":null}");

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);

        try self.sendMessage(payload);
    }

    fn handleDidOpen(self: *Server, io: std.Io, params: ?JsonValue) !void {
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
        try self.analyzeAndPublish(io, uri_value.string);
    }

    fn handleDidChange(self: *Server, io: std.Io, params: ?JsonValue) !void {
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
        try self.analyzeAndPublish(io, uri_value.string);
    }

    fn handleDidClose(self: *Server, io: std.Io, params: ?JsonValue) !void {
        const params_value = params orelse return;
        if (params_value != .object) return;

        const doc_value = params_value.object.get("textDocument") orelse return;
        if (doc_value != .object) return;

        const uri_value = doc_value.object.get("uri") orelse return;
        if (uri_value != .string) return;

        self.removeDocument(uri_value.string);
        self.reporter.clearByFile(uri_value.string);
        self.reporter.dropPublishedDiagnostics(uri_value.string);
        try self.publishDiagnostics(io, uri_value.string);
    }

    fn handleCompletion(self: *Server, io: std.Io, id: JsonValue, params: ?JsonValue) !void {
        self.ensureStdlib(io);
        const env = self.features();
        const ctx = text_util.computeCompletionContext(env, params);
        const payload = try completion.buildCompletionPayload(env, id, ctx);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleHover(self: *Server, io: std.Io, id: JsonValue, params: ?JsonValue) !void {
        self.ensureStdlib(io);
        const payload = try hover.buildHoverPayload(self.features(), id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleSignatureHelp(self: *Server, io: std.Io, id: JsonValue, params: ?JsonValue) !void {
        self.ensureStdlib(io);
        const payload = try signature.buildSignatureHelpPayload(self.features(), id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleInlayHint(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        const payload = try symbols.buildInlayHintPayload(self.features(), id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleDocumentSymbol(self: *Server, id: JsonValue, _: ?JsonValue) !void {
        const payload = try symbols.buildDocumentSymbolsPayload(self.features(), id);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    pub fn storeDocument(self: *Server, uri: []const u8, text: []const u8) !void {
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

    fn analyzeAndPublish(self: *Server, io: std.Io, uri: []const u8) !void {
        const doc = self.documents.getPtr(uri) orelse return;
        self.reporter.clearByFile(uri);
        self.reporter.clearByFile(doc.path);

        self.performAnalysis(io, doc, uri) catch {};

        try self.publishDiagnostics(io, uri);
    }

    pub fn performAnalysis(self: *Server, io: std.Io, doc: *Document, uri: []const u8) Errors.ErrorList!void {
        var memory_manager = try MemoryManager.init(self.allocator);
        defer memory_manager.deinit();

        var lexer = try LexicalAnalyzer.init(io, memory_manager.getAnalysisAllocator(), doc.text, doc.path, self.reporter);
        defer lexer.deinit();
        try lexer.initKeywords();

        var tokens = try lexer.lexTokens();
        defer tokens.deinit();

        var graph_store = module_graph.ModuleGraph.initForEntry(io, self.allocator, doc.path, self.std_dir, &.{}) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.reportUnresolvableRoot(io, doc, uri);
                return error.ModuleRootUnknown;
            },
        };
        defer graph_store.deinit();

        var parser = Parser.init(io, memory_manager.getAnalysisAllocator(), tokens.items, doc.path, uri, self.reporter);
        const statements = try parser.execute();

        var loader = ModuleLoader.init(io, memory_manager.getAnalysisAllocator(), self.reporter, &graph_store);
        const entry = try loader.registerEntry(doc.path, doc.text, statements);

        var semantic = SemanticAnalyzer.init(memory_manager.getAnalysisAllocator(), self.reporter, &memory_manager, &loader, entry.id, parser.entry_point_name);
        defer semantic.deinit();
        try semantic.analyzeProgram();

        index.populate(&self.symbol_index, &semantic, &memory_manager, statements);
    }

    /// A document's roots are its own directory and the standard library; one
    /// that does not exist leaves nothing to analyze. Say which, at the top of
    /// the document, so the client shows why it has no other diagnostics.
    fn reportUnresolvableRoot(self: *Server, io: std.Io, doc: *Document, uri: []const u8) void {
        const location = reporting.Location{
            .file = doc.path,
            .file_uri = uri,
            .range = .{ .start_line = 1, .start_col = 1, .end_line = 1, .end_col = 1 },
        };
        const std_exists = if (module_graph.physicalPath(io, self.allocator, self.std_dir)) |real| blk: {
            self.allocator.free(real);
            break :blk true;
        } else |_| false;
        if (std_exists) {
            self.reporter.reportCompileError(location, Errors.ErrorCode.MODULE_ROOT_UNKNOWN, "This document's directory could not be resolved as a module root", .{});
        } else {
            self.reporter.reportCompileError(location, Errors.ErrorCode.MODULE_ROOT_UNKNOWN, "The standard library was not found at '{s}'; the language server cannot resolve this document's modules", .{self.std_dir});
        }
    }

    fn publishDiagnostics(self: *Server, io: std.Io, uri: []const u8) !void {
        const payload = try self.reporter.buildPublishDiagnosticsPayload(self.allocator, uri);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
        try self.reporter.markDiagnosticsPublished(uri, std.Io.Timestamp.now(io, .real).toNanoseconds());
    }

    fn sendMessage(self: *Server, payload: []const u8) !void {
        try self.sink.sendFn(self.sink.context, payload);
    }

    fn sendErrorResponse(
        self: *Server,
        id: ?JsonValue,
        code: i64,
        message: []const u8,
    ) !void {
        var buffer: std.Io.Writer.Allocating = .init(self.allocator);
        errdefer buffer.deinit();

        const writer = &buffer.writer;

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");

        if (id) |value| {
            try transport.writeJsonValue(writer, value);
        } else {
            try writer.writeAll("null");
        }

        try writer.writeAll(",\"error\":{\"code\":");
        try writer.print("{d}", .{code});
        try writer.writeAll(",\"message\":");
        try transport.writeJsonValue(writer, message);
        try writer.writeAll("}}");

        const payload = try buffer.toOwnedSlice();
        defer self.allocator.free(payload);

        try self.sendMessage(payload);
    }
};
