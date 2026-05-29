const std = @import("std");
const reporting = @import("../utils/reporting.zig");
const Reporter = reporting.Reporter;
const ReporterOptions = reporting.ReporterOptions;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../parser/parser_types.zig").Parser;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const StructMethodInfo = SemanticAnalyzer.StructMethodInfo;
const Errors = @import("../utils/errors.zig");
const InternalMethods = @import("internal_methods.zig");
const Types = @import("../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;
const ast = @import("../ast/ast.zig");
const HARNESS_MAX_FILE_BYTES: usize = @import("../common/constants.zig").MAX_LSP_FILE_BYTES;

const JsonValue = std.json.Value;

pub const RunOptions = struct {
    reporter_options: ReporterOptions,
    trace_io: bool = false,
};

pub const DebugHarnessOptions = struct {
    reporter_options: ReporterOptions,
    script_path: []const u8,
};

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

const CachedField = struct {
    name: []const u8,
    type_name: []const u8,
    is_public: bool,
};

const CachedMethod = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
};

const CachedType = struct {
    name: []const u8,
    kind: CustomTypeInfo.CustomTypeKind,
    fields: []CachedField,
    methods: []CachedMethod,
    enum_variants: [][]const u8,
};

const SymbolEntry = struct {
    name: []const u8,
    kind: u32,
    start_line: usize,
    start_character: usize,
    end_line: usize,
    end_character: usize,
};

const SymbolIndex = struct {
    types: std.StringHashMap(CachedType),
    variables: std.StringHashMap([]const u8),
    modules: std.StringHashMap(void),
    symbols: std.array_list.Managed(SymbolEntry),
    module_members: std.StringHashMap(std.array_list.Managed([]const u8)),
    arena: std.heap.ArenaAllocator,

    fn init(allocator: std.mem.Allocator) SymbolIndex {
        return .{
            .types = std.StringHashMap(CachedType).init(allocator),
            .variables = std.StringHashMap([]const u8).init(allocator),
            .modules = std.StringHashMap(void).init(allocator),
            .symbols = std.array_list.Managed(SymbolEntry).init(allocator),
            .module_members = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    fn deinit(self: *SymbolIndex) void {
        self.types.deinit();
        self.variables.deinit();
        self.modules.deinit();
        {
            var it = self.module_members.iterator();
            while (it.next()) |entry| entry.value_ptr.deinit();
        }
        self.module_members.deinit();
        self.symbols.deinit();
        self.arena.deinit();
    }

    fn clear(self: *SymbolIndex) void {
        self.types.clearRetainingCapacity();
        self.variables.clearRetainingCapacity();
        self.modules.clearRetainingCapacity();
        {
            var it = self.module_members.iterator();
            while (it.next()) |entry| entry.value_ptr.clearRetainingCapacity();
        }
        self.module_members.clearRetainingCapacity();
        self.symbols.clearRetainingCapacity();
        _ = self.arena.reset(.retain_capacity);
    }

    fn addType(self: *SymbolIndex, cti: CustomTypeInfo, methods: ?std.StringHashMap(StructMethodInfo)) !void {
        const alloc = self.arena.allocator();
        const name = try alloc.dupe(u8, cti.name);

        var cached_fields = std.array_list.Managed(CachedField).init(alloc);
        if (cti.struct_fields) |fields| {
            for (fields) |field| {
                const type_str = packLspTypeString(field.field_type_info);
                try cached_fields.append(.{
                    .name = try alloc.dupe(u8, field.name),
                    .type_name = try alloc.dupe(u8, type_str),
                    .is_public = field.is_public,
                });
            }
        }

        var cached_enum_variants = std.array_list.Managed([]const u8).init(alloc);
        if (cti.enum_variants) |variants| {
            for (variants) |v| {
                try cached_enum_variants.append(try alloc.dupe(u8, v.name));
            }
        }

        var cached_methods = std.array_list.Managed(CachedMethod).init(alloc);
        if (methods) |methods_map| {
            var method_it = methods_map.iterator();
            while (method_it.next()) |entry| {
                try cached_methods.append(.{
                    .name = try alloc.dupe(u8, entry.value_ptr.name),
                    .is_public = entry.value_ptr.is_public,
                    .is_static = entry.value_ptr.is_static,
                });
            }
        }

        try self.types.put(name, .{
            .name = name,
            .kind = cti.kind,
            .fields = try cached_fields.toOwnedSlice(),
            .methods = try cached_methods.toOwnedSlice(),
            .enum_variants = try cached_enum_variants.toOwnedSlice(),
        });
    }

    fn addVariable(self: *SymbolIndex, name: []const u8, type_str: []const u8) !void {
        const alloc = self.arena.allocator();
        try self.variables.put(try alloc.dupe(u8, name), try alloc.dupe(u8, type_str));
    }

    fn addModule(self: *SymbolIndex, name: []const u8) !void {
        const alloc = self.arena.allocator();
        try self.modules.put(try alloc.dupe(u8, name), {});
    }
};

const CompletionContext = struct {
    prefix: []const u8,
    kind: enum { Intrinsic, Dot, None },
    object_name: ?[]const u8,
};

const Server = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    documents: std.StringHashMap(Document),
    symbol_index: SymbolIndex,
    shutdown_requested: bool,
    should_exit: bool,
    sink: ResponseSink,
    trace_io: bool,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, sink: ResponseSink, trace_io: bool) Server {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .documents = std.StringHashMap(Document).init(allocator),
            .symbol_index = SymbolIndex.init(allocator),
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
    }

    fn loop(self: *Server) !void {
        var stdin_buffer: [4096]u8 = undefined;
        var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
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
            } else if (std.mem.eql(u8, method, "textDocument/completion")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleCompletion(id.?, params);
            } else if (std.mem.eql(u8, method, "textDocument/hover")) {
                if (id == null) {
                    try self.sendErrorResponse(null, -32600, "Invalid request");
                    return;
                }
                try self.handleHover(id.?, params);
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

    fn handleInitialize(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        _ = params;
        var buffer = std.array_list.Managed(u8).init(self.allocator);
        defer buffer.deinit();
        var writer = buffer.writer();

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try writeJsonValue(&writer, id);
        try writer.writeAll(",\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":1},\"completionProvider\":{\"triggerCharacters\":[\"@\",\".\"]},\"hoverProvider\":true,\"documentSymbolProvider\":true},\"serverInfo\":{\"name\":\"Doxa\"}}}");

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

    fn handleCompletion(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        const ctx = computeCompletionContext(self, params);
        const payload = try buildCompletionPayload(self, id, ctx);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleHover(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        const payload = try buildHoverPayload(self, id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleDocumentSymbol(self: *Server, id: JsonValue, _: ?JsonValue) !void {
        const payload = try buildDocumentSymbolsPayload(self, id);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
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

        self.populateSymbolIndex(&semantic, &parser, &memory_manager, statements);
    }

    fn populateSymbolIndex(self: *Server, semantic: *SemanticAnalyzer, parser: *Parser, memory_manager: *MemoryManager, statements: []ast.Stmt) void {
        self.symbol_index.clear();

        var type_it = semantic.custom_types.iterator();
        while (type_it.next()) |entry| {
            const type_name = entry.key_ptr.*;
            const cti = entry.value_ptr.*;
            const methods = semantic.struct_methods.get(type_name);
            self.symbol_index.addType(cti, methods) catch continue;
        }

        if (semantic.current_scope) |_| {
            populateSymbolIndexFromScope(&self.symbol_index, memory_manager) catch {};
        }

        var mod_it = parser.module_namespaces.iterator();
        while (mod_it.next()) |entry| {
            self.symbol_index.addModule(entry.key_ptr.*) catch continue;
        }

        for (statements) |stmt| {
            populateSymbolEntry(&self.symbol_index, stmt) catch {};
        }

        if (parser.imported_symbols) |symbols| {
            var sym_it = symbols.iterator();
            while (sym_it.next()) |entry| {
                const full_name = entry.key_ptr.*;
                if (std.mem.indexOfScalar(u8, full_name, '.')) |dot_idx| {
                    const module_name = full_name[0..dot_idx];
                    const member_name = full_name[dot_idx + 1 ..];
                    addModuleMember(&self.symbol_index, module_name, member_name) catch continue;
                }
            }
        }
    }

    fn populateSymbolEntry(index: *SymbolIndex, stmt: ast.Stmt) !void {
        const alloc = index.arena.allocator();
        const base = &stmt.base;
        const loc = base.location();
        const entry = SymbolEntry{
            .name = undefined,
            .kind = 0,
            .start_line = loc.range.start_line,
            .start_character = loc.range.start_col,
            .end_line = loc.range.end_line,
            .end_character = loc.range.end_col,
        };

        switch (stmt.data) {
            .FunctionDecl => |f| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, f.name.lexeme),
                    .kind = 12,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .VarDecl => |v| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, v.name.lexeme),
                    .kind = 13,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .EnumDecl => |e| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, e.name.lexeme),
                    .kind = 10,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .GroupDecl => |g| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, g.name.lexeme),
                    .kind = 23,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .MapDecl => |m| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, m.name.lexeme),
                    .kind = 23,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .Module => |mod| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, mod.name.lexeme),
                    .kind = 2,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .ZigDecl => |z| {
                try index.symbols.append(.{
                    .name = try alloc.dupe(u8, z.name.lexeme),
                    .kind = 2,
                    .start_line = entry.start_line,
                    .start_character = entry.start_character,
                    .end_line = entry.end_line,
                    .end_character = entry.end_character,
                });
            },
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    switch (expr.data) {
                        .StructDecl => |s| {
                            try index.symbols.append(.{
                                .name = try alloc.dupe(u8, s.name.lexeme),
                                .kind = 23,
                                .start_line = entry.start_line,
                                .start_character = entry.start_character,
                                .end_line = entry.end_line,
                                .end_character = entry.end_character,
                            });
                        },
                        .EnumDecl => |e| {
                            try index.symbols.append(.{
                                .name = try alloc.dupe(u8, e.name.lexeme),
                                .kind = 10,
                                .start_line = entry.start_line,
                                .start_character = entry.start_character,
                                .end_line = entry.end_line,
                                .end_character = entry.end_character,
                            });
                        },
                        .GroupDecl => |g| {
                            try index.symbols.append(.{
                                .name = try alloc.dupe(u8, g.name.lexeme),
                                .kind = 23,
                                .start_line = entry.start_line,
                                .start_character = entry.start_character,
                                .end_line = entry.end_line,
                                .end_character = entry.end_character,
                            });
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }

    fn addModuleMember(index: *SymbolIndex, module_name: []const u8, member_name: []const u8) !void {
        const alloc = index.arena.allocator();
        const gop = try index.module_members.getOrPut(try alloc.dupe(u8, module_name));
        if (!gop.found_existing) {
            gop.value_ptr.* = std.array_list.Managed([]const u8).init(alloc);
        }
        try gop.value_ptr.append(try alloc.dupe(u8, member_name));
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

const DocumentContext = struct {
    text: []const u8,
    offset: usize,
};

const DocumentPosition = struct {
    line: usize,
    character: usize,
};

const MethodRange = struct {
    start: usize,
    end: usize,
};

fn computeCompletionContext(self: *Server, params: ?JsonValue) CompletionContext {
    if (params == null) return CompletionContext{ .prefix = "", .kind = .None, .object_name = null };
    if (extractDocumentContext(self, params)) |ctx| {
        if (ctx.offset <= ctx.text.len) {
            return computeCompletionAtOffset(ctx.text, ctx.offset);
        }
    }
    return CompletionContext{ .prefix = "", .kind = .None, .object_name = null };
}

fn extractUri(params: ?JsonValue) ?[]const u8 {
    const params_value = params orelse return null;
    if (params_value != .object) return null;
    const doc_value = params_value.object.get("textDocument") orelse return null;
    if (doc_value != .object) return null;
    const uri_value = doc_value.object.get("uri") orelse return null;
    if (uri_value != .string) return null;
    return uri_value.string;
}

fn computeCompletionAtOffset(text: []const u8, offset: usize) CompletionContext {
    if (offset == 0) return CompletionContext{ .prefix = "", .kind = .None, .object_name = null };

    var pos: usize = offset;
    if (pos > text.len) pos = text.len;

    while (pos > 0) : (pos -= 1) {
        const c = text[pos - 1];
        if (c == '@') {
            const end = if (offset > text.len) text.len else offset;
            return CompletionContext{
                .prefix = text[pos - 1 .. end],
                .kind = .Intrinsic,
                .object_name = null,
            };
        }
        if (!isIdentChar(c)) break;
    }

    if (pos > 0 and text[pos - 1] == '.') {
        const member_end = if (offset > text.len) text.len else offset;
        const member_prefix = text[pos..member_end];

        const obj_end: usize = pos - 1;
        if (obj_end == 0) return CompletionContext{ .prefix = member_prefix, .kind = .Dot, .object_name = "" };

        var obj_start = obj_end;
        while (obj_start > 0 and isIdentChar(text[obj_start - 1])) : (obj_start -= 1) {}

        return CompletionContext{
            .prefix = member_prefix,
            .kind = .Dot,
            .object_name = text[obj_start..obj_end],
        };
    }

    return CompletionContext{ .prefix = "", .kind = .None, .object_name = null };
}

fn buildCompletionPayload(self: *Server, id: JsonValue, ctx: CompletionContext) ![]u8 {
    var buffer = std.array_list.Managed(u8).init(self.allocator);
    defer buffer.deinit();
    var writer = buffer.writer();

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(&writer, id);
    try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");

    switch (ctx.kind) {
        .Intrinsic => try writeIntrinsicCompletions(&writer, ctx.prefix),
        .Dot => try writeMemberCompletions(&self.symbol_index, &writer, ctx),
        .None => {},
    }

    try writer.writeAll("]}}");
    return buffer.toOwnedSlice();
}

fn writeIntrinsicCompletions(writer: anytype, prefix: []const u8) !void {
    var first = true;
    for (InternalMethods.all()) |method| {
        if (!std.mem.startsWith(u8, method.label, prefix)) continue;
        if (!first) try writer.writeAll(",");
        first = false;
        try writer.writeAll("{\"label\":");
        try writeJsonValue(writer, method.label);
        try writer.writeAll(",\"kind\":3");
        try writer.writeAll(",\"detail\":");
        try writeJsonValue(writer, method.detail);
        try writer.writeAll(",\"documentation\":");
        try writeJsonValue(writer, method.documentation);
        try writer.writeAll("}");
    }
}

fn writeMemberCompletions(index: *const SymbolIndex, writer: anytype, ctx: CompletionContext) !void {
    const obj_name = ctx.object_name orelse return;
    if (obj_name.len == 0) return;

    if (index.module_members.get(obj_name)) |members| {
        var first = true;
        for (members.items) |member| {
            if (!isMemberMatch(member, ctx.prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeModuleMemberCompletionItem(writer, member);
        }
        return;
    }

    const type_name = resolveObjectType(index, obj_name);
    if (type_name) |tn| {
        try writeTypeMembers(index, writer, tn, ctx.prefix);
    }
}

fn resolveObjectType(index: *const SymbolIndex, name: []const u8) ?[]const u8 {
    if (index.variables.get(name)) |type_str| {
        return type_str;
    }
    if (index.types.contains(name)) {
        return name;
    }
    if (index.modules.contains(name)) {
        return name;
    }
    return null;
}

fn writeTypeMembers(index: *const SymbolIndex, writer: anytype, type_name: []const u8, prefix: []const u8) !void {
    var first = true;

    if (index.types.get(type_name)) |ct| {
        switch (ct.kind) {
            .Struct => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field);
                }
                for (ct.methods) |method| {
                    if (!isMemberMatch(method.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeMethodCompletionItem(writer, method);
                }
            },
            .Enum => {
                for (ct.enum_variants) |variant| {
                    if (!isMemberMatch(variant, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeEnumCompletionItem(writer, variant);
                }
            },
            .Group => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field);
                }
            },
        }
    }
}

fn isMemberMatch(name: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return true;
    return std.mem.startsWith(u8, name, prefix);
}

fn writeFieldCompletionItem(writer: anytype, field: CachedField) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, field.name);
    try writer.writeAll(",\"kind\":5");
    try writer.writeAll(",\"detail\":");
    var buf: [256]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{s}", .{field.type_name}) catch field.type_name;
    try writeJsonValue(writer, detail);
    try writer.writeAll("}");
}

fn writeMethodCompletionItem(writer: anytype, method: CachedMethod) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, method.name);
    try writer.writeAll(",\"kind\":2");
    try writer.writeAll(",\"detail\":");
    const sig = if (method.is_static) "fn" else "fn(self)";
    try writeJsonValue(writer, sig);
    try writer.writeAll("}");
}

fn writeEnumCompletionItem(writer: anytype, variant: []const u8) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, variant);
    try writer.writeAll(",\"kind\":13");
    try writer.writeAll(",\"detail\":");
    try writeJsonValue(writer, "enum member");
    try writer.writeAll("}");
}

fn writeModuleMemberCompletionItem(writer: anytype, name: []const u8) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, name);
    try writer.writeAll(",\"kind\":9");
    try writer.writeAll(",\"detail\":");
    try writeJsonValue(writer, "module export");
    try writer.writeAll("}");
}

fn buildDocumentSymbolsPayload(self: *Server, id: JsonValue) ![]u8 {
    var buffer = std.array_list.Managed(u8).init(self.allocator);
    defer buffer.deinit();
    var writer = buffer.writer();

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(&writer, id);
    try writer.writeAll(",\"result\":[");

    var first = true;
    for (self.symbol_index.symbols.items) |sym| {
            if (!first) try writer.writeAll(",");
            first = false;

            try writer.writeAll("{\"name\":");
            try writeJsonValue(&writer, sym.name);
            try writer.writeAll(",\"kind\":");
            try writer.print("{d}", .{sym.kind});
            try writer.writeAll(",\"range\":{\"start\":{\"line\":");
            try writer.print("{d}", .{sym.start_line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{sym.start_character});
            try writer.writeAll("},\"end\":{\"line\":");
            try writer.print("{d}", .{sym.end_line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{sym.end_character});
            try writer.writeAll("}}");
            try writer.writeAll(",\"selectionRange\":{\"start\":{\"line\":");
            try writer.print("{d}", .{sym.start_line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{sym.start_character});
            try writer.writeAll("},\"end\":{\"line\":");
            try writer.print("{d}", .{sym.start_line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{sym.start_character + sym.name.len});
            try writer.writeAll("}}}");
        }

    try writer.writeAll("]}");
    return buffer.toOwnedSlice();
}

fn buildHoverPayload(self: *Server, id: JsonValue, params: ?JsonValue) ![]u8 {
    var buffer = std.array_list.Managed(u8).init(self.allocator);
    defer buffer.deinit();
    var writer = buffer.writer();

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(&writer, id);
    try writer.writeAll(",\"result\":");

    if (extractDocumentContext(self, params)) |ctx| {
        if (findMethodRange(ctx.text, ctx.offset)) |method_range| {
            const name = ctx.text[method_range.start..method_range.end];
            if (InternalMethods.find(name)) |info| {
                const start_pos = offsetToPosition(ctx.text, method_range.start);
                const end_pos = offsetToPosition(ctx.text, method_range.end);

                try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
                try writeJsonValue(&writer, info.documentation);
                try writer.writeAll("},\"range\":{\"start\":{\"line\":");
                try writer.print("{d}", .{start_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{start_pos.character});
                try writer.writeAll("},\"end\":{\"line\":");
                try writer.print("{d}", .{end_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{end_pos.character});
                try writer.writeAll("}}}");

                const payload = try buffer.toOwnedSlice();
                return payload;
            }
        }

        const dot_ctx = computeCompletionAtOffset(ctx.text, ctx.offset);
        if (dot_ctx.kind == .Dot and dot_ctx.object_name != null and dot_ctx.object_name.?.len > 0) {
            if (try buildDotHover(&self.symbol_index, &writer, dot_ctx)) {
                const payload = try buffer.toOwnedSlice();
                return payload;
            }
        }
    }

    try writer.writeAll("null");
    try writer.writeAll("}");
    return buffer.toOwnedSlice();
}

fn buildDotHover(index: *const SymbolIndex, writer: anytype, comp_ctx: CompletionContext) !bool {
    const obj_name = comp_ctx.object_name.?;
    const type_name = resolveObjectType(index, obj_name) orelse return false;
    const ct = index.types.get(type_name) orelse return false;

    const member_text = comp_ctx.prefix;
    if (member_text.len == 0) return false;

    for (ct.fields) |field| {
        if (std.mem.eql(u8, field.name, member_text)) {
            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":\"`");
            try writer.writeAll(field.name);
            try writer.writeAll(" : ");
            try writer.writeAll(field.type_name);
            try writer.writeAll("`\"}}");
            return true;
        }
    }
    for (ct.methods) |method| {
        if (std.mem.eql(u8, method.name, member_text)) {
            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":\"`");
            if (method.is_static) try writer.writeAll("static ");
            try writer.writeAll("fn ");
            try writer.writeAll(method.name);
            try writer.writeAll("(...)`\"}}");
            return true;
        }
    }
    for (ct.enum_variants) |variant| {
        if (std.mem.eql(u8, variant, member_text)) {
            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":\"`enum `");
            try writeJsonValue(writer, type_name);
            try writer.writeAll(".");
            try writeJsonValue(writer, variant);
            try writer.writeAll("`\"}}");
            return true;
        }
    }
    return false;
}


fn extractDocumentContext(self: *Server, params: ?JsonValue) ?DocumentContext {
    const params_value = params orelse return null;
    if (params_value != .object) return null;
    const params_obj = params_value.object;

    const doc_value = params_obj.get("textDocument") orelse return null;
    if (doc_value != .object) return null;
    const doc_obj = doc_value.object;
    const uri_value = doc_obj.get("uri") orelse return null;
    if (uri_value != .string) return null;
    const document = self.documents.getPtr(uri_value.string) orelse return null;

    const position_value = params_obj.get("position") orelse return null;
    if (position_value != .object) return null;
    const position_obj = position_value.object;

    const line_value = position_obj.get("line") orelse return null;
    const char_value = position_obj.get("character") orelse return null;
    const line = parsePositionComponent(self, line_value) orelse return null;
    const character = parsePositionComponent(self, char_value) orelse return null;

    const offset = computeOffsetFromPosition(document.text, line, character);
    return DocumentContext{
        .text = document.text,
        .offset = offset,
    };
}

fn parsePositionComponent(self: *Server, value: JsonValue) ?usize {
    const repr = jsonStringifyAlloc(self.allocator, value, .{}) catch return null;
    defer self.allocator.free(repr);
    const parsed = std.fmt.parseInt(usize, repr, 10) catch return null;
    return parsed;
}

fn computeOffsetFromPosition(text: []const u8, line: usize, character: usize) usize {
    var offset: usize = 0;
    var current_line: usize = 0;
    while (offset < text.len and current_line < line) {
        if (text[offset] == '\n') {
            current_line += 1;
        }
        offset += 1;
    }
    var current_character: usize = 0;
    while (offset < text.len and current_character < character) {
        const c = text[offset];
        if (c == '\n') break;
        if (c == '\r') {
            offset += 1;
            if (offset < text.len and text[offset] == '\n') {
                // Treat CRLF as a single newline.
            }
            break;
        }
        offset += 1;
        current_character += 1;
    }
    if (offset > text.len) offset = text.len;
    return offset;
}

fn findMethodRange(text: []const u8, offset: usize) ?MethodRange {
    if (findMethodStart(text, offset)) |start| {
        var end = start;
        while (end < text.len and isMethodChar(text[end])) {
            end += 1;
        }
        if (end == start) return null;
        return MethodRange{ .start = start, .end = end };
    }
    return null;
}

fn offsetToPosition(text: []const u8, offset: usize) DocumentPosition {
    var line: usize = 0;
    var character: usize = 0;
    var idx: usize = 0;
    while (idx < offset and idx < text.len) {
        const c = text[idx];
        if (c == '\n') {
            line += 1;
            character = 0;
        } else if (c == '\r') {
            line += 1;
            character = 0;
            if (idx + 1 < text.len and text[idx + 1] == '\n') {
                idx += 1;
            }
        } else {
            character += 1;
        }
        idx += 1;
    }
    return DocumentPosition{ .line = line, .character = character };
}

fn isMethodContinuationChar(c: u8) bool {
    return std.ascii.isAlphabetic(c) or std.ascii.isDigit(c) or c == '_';
}

fn isMethodChar(c: u8) bool {
    return c == '@' or isMethodContinuationChar(c);
}

fn findMethodStart(text: []const u8, offset: usize) ?usize {
    if (text.len == 0) return null;
    var current = if (offset > text.len) text.len else offset;
    while (current > 0) {
        const prev = text[current - 1];
        if (prev == '@') return current - 1;
        if (!isMethodContinuationChar(prev)) break;
        current -= 1;
    }
    if (current < text.len and text[current] == '@') return current;
    return null;
}
fn isIdentChar(c: u8) bool {
    return std.ascii.isAlphabetic(c) or std.ascii.isDigit(c) or c == '_';
}

fn packLspTypeString(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |ct| {
        return ct;
    }
    return @tagName(type_info.base);
}

fn populateSymbolIndexFromScope(index: *SymbolIndex, memory_manager: *MemoryManager) !void {
    for (memory_manager.scope_pool.items) |scope| {
        if (scope.is_deinited) continue;
        var it = scope.name_map.iterator();
        while (it.next()) |entry| {
            const var_name = entry.key_ptr.*;
            const var_ptr = entry.value_ptr.*;
            if (scope.manager.value_storage.get(var_ptr.storage_id)) |storage| {
                const type_str = packLspTypeString(storage.type_info);
                try index.addVariable(var_name, type_str);
            }
        }
    }
}

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
