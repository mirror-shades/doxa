const std = @import("std");
const source_cache = @import("../utils/source_cache.zig");
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
const stdlib = @import("../stdlib/catalog.zig");
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
    io: std.Io,
    trace_io: bool,

    fn init(io: std.Io, trace_io: bool) StdIoSink {
        return .{
            .io = io,
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
        var stdout_writer = std.Io.File.stdout().writer(
            self.io,
            &stdout_buffer,
        );
        const writer = &stdout_writer.interface;

        if (self.trace_io) {
            std.debug.print(
                "[lsp-io] writing Content-Length: {d}\n",
                .{payload.len},
            );
            std.debug.print(
                "[lsp-io] >> {s}\n",
                .{payload},
            );
        }

        try writer.print(
            "Content-Length: {d}\r\n\r\n",
            .{payload.len},
        );
        try writer.writeAll(payload);
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

pub fn run(io: std.Io, allocator: std.mem.Allocator, options: RunOptions) !void {
    var cache = source_cache.SourceCache.init(allocator);
    defer cache.deinit();
    var reporter = Reporter.init(io, allocator, options.reporter_options, &cache);
    defer reporter.deinit();

    var sink = StdIoSink.init(io, options.trace_io);
    var server = Server.init(allocator, &reporter, sink.asResponseSink(), options.trace_io);
    defer server.deinit();

    try server.loop(io);
}

pub fn runDebugHarness(io: std.Io, allocator: std.mem.Allocator, options: DebugHarnessOptions) !void {
    std.debug.print("=== Doxa LSP Debug Harness ===\n", .{});
    std.debug.print("Target file: {s}\n", .{options.script_path});

    var src_cache = source_cache.SourceCache.init(allocator);
    defer src_cache.deinit();
    var reporter = Reporter.init(io, allocator, options.reporter_options, &src_cache);
    defer reporter.deinit();

    const file_uri = try reporter.ensureFileUri(io, options.script_path);
    const document_text = try readFileAlloc(io, allocator, options.script_path, HARNESS_MAX_FILE_BYTES);
    defer allocator.free(document_text);

    var sink = CaptureSink.init(allocator);
    defer sink.deinit();

    var server = Server.init(allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    const initialize_request = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootUri\":null,\"capabilities\":{}}}";
    const initialized_notification = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}";
    const did_open_request = try buildDidOpenRequest(allocator, file_uri, document_text);
    defer allocator.free(did_open_request);

    try runHarnessMessage(io, allocator, &server, &sink, "initialize", initialize_request);
    try runHarnessMessage(io, allocator, &server, &sink, "initialized", initialized_notification);
    try runHarnessMessage(io, allocator, &server, &sink, "textDocument/didOpen", did_open_request);
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

const CachedParam = struct {
    name: []const u8,
    type_text: []const u8,
    alias: bool,
};

const CachedMethod = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
    params: []CachedParam = &.{},
    return_type: []const u8 = "",
};

/// A user-defined callable (top-level function or struct method) with the
/// signature data needed for hover, signature help, and snippets.
const CachedCallable = struct {
    name: []const u8,
    /// `foo` for a function, `Type.method` for a method.
    qualified: []const u8,
    params: []CachedParam,
    return_type: []const u8,
};

const CachedType = struct {
    name: []const u8,
    kind: Types.CustomTypeKind,
    fields: []CachedField,
    methods: []CachedMethod,
    enum_variants: [][]const u8,
};

/// A rendered type hint (`:: int`) anchored after an inferred declaration.
const CachedInlayHint = struct {
    line: usize,
    character: usize,
    label: []const u8,
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
    /// User-defined callables, keyed by qualified name: `foo` for a top-level
    /// function, `Type.method` for a struct method.
    callables: std.StringHashMap(*CachedCallable),
    inlay_hints: std.array_list.Managed(CachedInlayHint),
    arena: std.heap.ArenaAllocator,

    fn init(allocator: std.mem.Allocator) SymbolIndex {
        return .{
            .types = std.StringHashMap(CachedType).init(allocator),
            .variables = std.StringHashMap([]const u8).init(allocator),
            .modules = std.StringHashMap(void).init(allocator),
            .symbols = std.array_list.Managed(SymbolEntry).init(allocator),
            .module_members = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
            .callables = std.StringHashMap(*CachedCallable).init(allocator),
            .inlay_hints = std.array_list.Managed(CachedInlayHint).init(allocator),
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
        self.callables.deinit();
        self.inlay_hints.deinit();
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
        self.callables.clearRetainingCapacity();
        self.inlay_hints.clearRetainingCapacity();
        _ = self.arena.reset(.retain_capacity);
    }

    fn addCallable(
        self: *SymbolIndex,
        qualified: []const u8,
        params: []ast.FunctionParam,
        return_type_info: ast.TypeInfo,
    ) !void {
        const alloc = self.arena.allocator();

        var cached_params = std.array_list.Managed(CachedParam).init(alloc);
        for (params) |param| {
            try cached_params.append(.{
                .name = try alloc.dupe(u8, param.name.lexeme),
                .type_text = try renderTypeExpr(alloc, param.type_expr),
                .alias = param.is_alias,
            });
        }

        const callable = try alloc.create(CachedCallable);
        callable.* = .{
            .name = try alloc.dupe(u8, shortName(qualified)),
            .qualified = try alloc.dupe(u8, qualified),
            .params = try cached_params.toOwnedSlice(),
            .return_type = try renderTypeInfo(alloc, return_type_info),
        };
        try self.callables.put(callable.qualified, callable);
    }

    fn findCallable(self: *const SymbolIndex, qualified: []const u8) ?*const CachedCallable {
        return self.callables.get(qualified);
    }

    /// The struct method table for a type, keyed by the type's cached name.
    fn findMethod(self: *const SymbolIndex, type_name: []const u8, method_name: []const u8) ?*const CachedMethod {
        const ct = self.types.get(type_name) orelse return null;
        for (ct.methods) |*method| {
            if (std.mem.eql(u8, method.name, method_name)) return method;
        }
        return null;
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
                const method_name = entry.value_ptr.name;
                const qualified = try std.fmt.allocPrint(alloc, "{s}.{s}", .{ cti.name, method_name });
                var params: []CachedParam = &.{};
                var return_type: []const u8 = "";
                if (self.callables.get(qualified)) |callable| {
                    params = callable.params;
                    return_type = callable.return_type;
                }
                try cached_methods.append(.{
                    .name = try alloc.dupe(u8, method_name),
                    .is_public = entry.value_ptr.is_public,
                    .is_static = entry.value_ptr.is_static,
                    .params = params,
                    .return_type = return_type,
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

fn shortName(qualified: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, qualified, '.')) |dot| return qualified[dot + 1 ..];
    return qualified;
}

const CompletionKind = enum { Intrinsic, Dot, Word, None };

const CompletionContext = struct {
    prefix: []const u8,
    kind: CompletionKind,
    object_name: ?[]const u8,
    /// Byte offsets of the text a completion item replaces. Equal to the
    /// cursor for an empty prefix.
    replace_start: usize = 0,
    replace_end: usize = 0,
    start_pos: DocumentPosition = .{ .line = 0, .character = 0 },
    end_pos: DocumentPosition = .{ .line = 0, .character = 0 },

    fn edit(self: CompletionContext) CompletionEdit {
        return .{ .start = self.start_pos, .end = self.end_pos };
    }
};

/// The document range a completion item replaces. Supplying this explicitly
/// (rather than relying on the client's word-boundary heuristics) keeps `@`
/// prefixed items from being re-inserted with a doubled sigil.
const CompletionEdit = struct {
    start: DocumentPosition,
    end: DocumentPosition,
};

const Server = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    documents: std.StringHashMap(Document),
    symbol_index: SymbolIndex,
    stdlib_arena: std.heap.ArenaAllocator,
    /// The loaded standard-library catalog; `null` until the first completion
    /// or hover. On load failure it is set to an empty catalog so the (failed)
    /// filesystem lookup happens at most once per session.
    stdlib: ?stdlib.Catalog,
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

    /// Loads `std/` once. Candidates mirror the runtime search in `main.zig`:
    /// the installed `<exe_dir>/../lib/std`, then the dev tree
    /// `<exe_dir>/../../std` (binary under `<repo>/doxa/bin`), then `./std`.
    /// Failure is non-fatal — completion degrades to imported symbols.
    fn ensureStdlib(self: *Server, io: std.Io) void {
        if (self.stdlib != null) return;

        const allocator = self.stdlib_arena.allocator();
        var candidates = std.array_list.Managed([]const u8).init(allocator);
        if (std.process.executableDirPathAlloc(io, allocator)) |exe_dir| {
            if (std.fs.path.join(allocator, &.{ exe_dir, "..", "lib", "std" })) |p| {
                candidates.append(p) catch {};
            } else |_| {}
            if (std.fs.path.join(allocator, &.{ exe_dir, "..", "..", "std" })) |p| {
                candidates.append(p) catch {};
            } else |_| {}
        } else |_| {}
        candidates.append("std") catch {};

        for (candidates.items) |std_dir| {
            if (stdlib.load(allocator, io, std_dir)) |loaded| {
                self.stdlib = loaded;
                return;
            } else |_| {}
        }

        std.debug.print("doxa-lsp: standard library catalog not found; std completion disabled\n", .{});
        self.stdlib = .{ .modules = &.{} };
    }

    fn loop(self: *Server, io: std.Io) !void {
        var stdin_buffer: [4096]u8 = undefined;
        var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
        const reader = &stdin_reader.interface;

        while (!self.should_exit) {
            const payload = self.readMessage(io, reader) catch |err| switch (err) {
                error.EndOfStream => {
                    if (self.shutdown_requested) {
                        return;
                    } else {
                        try io.sleep(.fromMilliseconds(1), .awake);
                        continue;
                    }
                },
                error.ReadFailed => {
                    try io.sleep(.fromMilliseconds(1), .awake);
                    continue;
                },
                else => return err,
            };

            defer self.allocator.free(payload);
            try self.handlePayload(io, payload);
        }
    }

    fn readLineAlloc(self: *Server, io: std.Io, reader: anytype) ![]u8 {
        var buffer: [4096]u8 = undefined;
        var len: usize = 0;

        while (true) {
            if (len >= buffer.len) return error.StreamTooLong;

            const byte = std.Io.Reader.takeByte(@constCast(reader)) catch |err| switch (err) {
                error.EndOfStream => break,
                error.ReadFailed => {
                    // Handle pipe communication issues - retry after brief delay
                    try io.sleep(.fromMilliseconds(1), .awake);
                    continue;
                },
            };

            if (byte == '\n') break;
            buffer[len] = byte;
            len += 1;
        }

        return self.allocator.dupe(u8, buffer[0..len]);
    }

    fn readMessage(self: *Server, io: std.Io, reader: anytype) ![]u8 {
        var content_length: ?usize = null;

        while (true) {
            const line = try self.readLineAlloc(io, reader);
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
        _ = try std.Io.Reader.readSliceShort(@constCast(reader), payload);
        if (self.trace_io) {
            std.debug.print("[lsp-io] << {s}\n", .{payload});
        }
        return payload;
    }

    fn handlePayload(self: *Server, io: std.Io, payload: []const u8) !void {
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
        try writeJsonValue(writer, id);
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
        try writeJsonValue(writer, id);
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
        const ctx = computeCompletionContext(self, params);
        const payload = try buildCompletionPayload(self, id, ctx);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleHover(self: *Server, io: std.Io, id: JsonValue, params: ?JsonValue) !void {
        self.ensureStdlib(io);
        const payload = try buildHoverPayload(self, id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleSignatureHelp(self: *Server, io: std.Io, id: JsonValue, params: ?JsonValue) !void {
        self.ensureStdlib(io);
        const payload = try buildSignatureHelpPayload(self, id, params);
        defer self.allocator.free(payload);
        try self.sendMessage(payload);
    }

    fn handleInlayHint(self: *Server, id: JsonValue, params: ?JsonValue) !void {
        const payload = try buildInlayHintPayload(self, id, params);
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

    fn analyzeAndPublish(self: *Server, io: std.Io, uri: []const u8) !void {
        const doc = self.documents.getPtr(uri) orelse return;
        self.reporter.clearByFile(uri);
        self.reporter.clearByFile(doc.path);

        self.performAnalysis(io, doc, uri) catch {};

        try self.publishDiagnostics(io, uri);
    }

    fn performAnalysis(self: *Server, io: std.Io, doc: *Document, uri: []const u8) Errors.ErrorList!void {
        var memory_manager = try MemoryManager.init(self.allocator);
        defer memory_manager.deinit();

        var lexer = try LexicalAnalyzer.init(io, memory_manager.getAnalysisAllocator(), doc.text, doc.path, self.reporter);
        defer lexer.deinit();
        try lexer.initKeywords();

        var tokens = try lexer.lexTokens();
        defer tokens.deinit();

        var parser = Parser.init(io, memory_manager.getAnalysisAllocator(), tokens.items, doc.path, uri, self.reporter);
        defer parser.deinit();
        const statements = try parser.execute();

        var semantic = SemanticAnalyzer.init(memory_manager.getAnalysisAllocator(), self.reporter, &memory_manager, &parser);
        defer semantic.deinit();
        try semantic.analyze(statements);

        self.populateSymbolIndex(&semantic, &parser, &memory_manager, statements);
    }

    fn populateSymbolIndex(self: *Server, semantic: *SemanticAnalyzer, parser: *Parser, memory_manager: *MemoryManager, statements: []ast.Stmt) void {
        self.symbol_index.clear();

        // AST signatures must land first: `addType` enriches each method with
        // the parameter list collected here.
        self.collectAstSignatures(statements);

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

    /// Walks the document AST once and records the parameter lists and return
    /// types of every top-level function and struct method. The semantic
    /// analyzer only tracks method names, so this is the sole source of user
    /// signature data for hover, signature help, and snippets.
    fn collectAstSignatures(self: *Server, statements: []ast.Stmt) void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl => |f| {
                    self.symbol_index.addCallable(f.name.lexeme, f.params, f.return_type_info) catch {};
                },
                .Expression => |maybe_expr| {
                    const expr = maybe_expr orelse continue;
                    switch (expr.data) {
                        .StructDecl => |struct_decl| {
                            for (struct_decl.methods) |method| {
                                const alloc = self.symbol_index.arena.allocator();
                                const qualified = std.fmt.allocPrint(
                                    alloc,
                                    "{s}.{s}",
                                    .{ struct_decl.name.lexeme, method.name.lexeme },
                                ) catch continue;
                                self.symbol_index.addCallable(qualified, method.params, method.return_type_info) catch {};
                            }
                        },
                        else => {},
                    }
                },
                else => {},
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
            try writeJsonValue(writer, value);
        } else {
            try writer.writeAll("null");
        }

        try writer.writeAll(",\"error\":{\"code\":");
        try writer.print("{d}", .{code});
        try writer.writeAll(",\"message\":");
        try writeJsonValue(writer, message);
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

fn makeCompletionContext(
    text: []const u8,
    kind: CompletionKind,
    prefix: []const u8,
    object_name: ?[]const u8,
    start: usize,
    end: usize,
) CompletionContext {
    return .{
        .prefix = prefix,
        .kind = kind,
        .object_name = object_name,
        .replace_start = start,
        .replace_end = end,
        .start_pos = offsetToPosition(text, start),
        .end_pos = offsetToPosition(text, end),
    };
}

fn computeCompletionAtOffset(text: []const u8, offset: usize) CompletionContext {
    const end = if (offset > text.len) text.len else offset;
    if (end == 0) return makeCompletionContext(text, .None, "", null, 0, 0);

    var pos: usize = end;
    while (pos > 0) : (pos -= 1) {
        const c = text[pos - 1];
        if (c == '@') {
            return makeCompletionContext(text, .Intrinsic, text[pos - 1 .. end], null, pos - 1, end);
        }
        if (!isIdentChar(c)) break;
    }

    if (pos > 0 and text[pos - 1] == '.') {
        const member_prefix = text[pos..end];

        const obj_end: usize = pos - 1;
        if (obj_end == 0) return makeCompletionContext(text, .Dot, member_prefix, "", pos, end);

        // Walk back over a dotted path so `std.io.` yields `std.io` rather than
        // just `io`, which is what standard-library completion keys on.
        var obj_start = obj_end;
        while (obj_start > 0) {
            const prev = text[obj_start - 1];
            if (isIdentChar(prev)) {
                obj_start -= 1;
            } else if (prev == '.' and obj_start >= 2 and isIdentChar(text[obj_start - 2])) {
                obj_start -= 1;
            } else break;
        }

        return makeCompletionContext(text, .Dot, member_prefix, text[obj_start..obj_end], pos, end);
    }

    // A bare, partial identifier: complete it against the symbols in scope.
    return makeCompletionContext(text, .Word, text[pos..end], null, pos, end);
}

fn buildCompletionPayload(self: *Server, id: JsonValue, ctx: CompletionContext) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(self.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");

    switch (ctx.kind) {
        .Intrinsic => try writeIntrinsicCompletions(self.allocator, writer, ctx.prefix, ctx.edit()),
        .Dot => try writeMemberCompletions(self, writer, ctx, ctx.edit()),
        .Word => try writeWordCompletions(self, writer, ctx.prefix, ctx.edit()),
        .None => {},
    }

    try writer.writeAll("]}}");
    return buffer.toOwnedSlice();
}

/// Emits the explicit replacement range and text for an item. Relying on the
/// client's word boundaries would re-insert the `@` of an intrinsic that the
/// user already typed.
fn writeTextEdit(writer: *std.Io.Writer, edit: CompletionEdit, new_text: []const u8) !void {
    try writer.writeAll(",\"textEdit\":{\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{edit.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{edit.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.end.character});
    try writer.writeAll("}},\"newText\":");
    try writeJsonValue(writer, new_text);
    try writer.writeAll("}");
}

fn writeIntrinsicCompletions(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    var first = true;
    for (InternalMethods.all()) |*info| {
        const label = try std.fmt.allocPrint(allocator, "@{s}", .{info.name});
        defer allocator.free(label);
        if (!isMemberMatch(label, prefix)) continue;

        const detail = try InternalMethods.signature(allocator, info);
        defer allocator.free(detail);
        const snippet = try InternalMethods.snippet(allocator, info);
        defer allocator.free(snippet);

        if (!first) try writer.writeAll(",");
        first = false;
        try writer.writeAll("{\"label\":");
        try writeJsonValue(writer, label);
        try writer.writeAll(",\"kind\":3,\"detail\":");
        try writeJsonValue(writer, detail);
        const doc = InternalMethods.documentation(info);
        if (doc.len > 0) {
            try writer.writeAll(",\"documentation\":");
            try writeJsonValue(writer, doc);
        }
        try writer.writeAll(",\"insertTextFormat\":2");
        try writeTextEdit(writer, edit, snippet);
        try writer.writeAll("}");
    }
}

fn writeMemberCompletions(
    self: *Server,
    writer: *std.Io.Writer,
    ctx: CompletionContext,
    edit: CompletionEdit,
) !void {
    const obj_name = ctx.object_name orelse return;
    if (obj_name.len == 0) return;

    // The standard-library catalog takes precedence: it knows the whole public
    // surface, not just what the open document happens to import.
    if (self.stdlib) |*cat| {
        if (try writeStdlibCompletions(cat, self.allocator, writer, obj_name, ctx.prefix, edit)) return;
    }

    const index = &self.symbol_index;
    if (index.module_members.get(obj_name)) |members| {
        var first = true;
        for (members.items) |member| {
            if (!isMemberMatch(member, ctx.prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeModuleMemberCompletionItem(writer, member, edit);
        }
        return;
    }

    const type_name = resolveObjectType(index, obj_name) orelse return;
    if (index.types.contains(type_name)) {
        try writeTypeMembers(self.allocator, index, writer, type_name, ctx.prefix, edit);
        return;
    }

    // A value whose declared type is a standard-library struct/enum. The
    // inferred name can be a union (`Response | error.StdError`), so try the
    // whole string and then each alternative.
    if (self.stdlib) |*cat| {
        if (findCatalogType(cat, type_name)) |owner| {
            try writeCatalogTypeMembers(self.allocator, writer, owner, ctx.prefix, edit);
            return;
        }
        var alternatives = std.mem.splitSequence(u8, type_name, " | ");
        while (alternatives.next()) |alternative| {
            const trimmed = std.mem.trim(u8, alternative, " \t");
            if (findCatalogType(cat, trimmed)) |owner| {
                try writeCatalogTypeMembers(self.allocator, writer, owner, ctx.prefix, edit);
                return;
            }
        }
    }
}

/// Finds a standard-library type (struct, enum, or group) by the name as
/// written: `Response`, `http.Response`, or `std.http.Response`.
fn findCatalogType(cat: *const stdlib.Catalog, name: []const u8) ?*const stdlib.Decl {
    if (std.mem.lastIndexOfScalar(u8, name, '.')) |dot| {
        const module = cat.findModule(name[0..dot]) orelse return null;
        const decl = module.find(name[dot + 1 ..]) orelse return null;
        return if (isCatalogTypeKind(decl.kind)) decl else null;
    }
    for (cat.modules) |*module| {
        if (module.find(name)) |decl| {
            if (isCatalogTypeKind(decl.kind)) return decl;
        }
    }
    return null;
}

fn isCatalogTypeKind(kind: stdlib.Kind) bool {
    return kind == .structure or kind == .enumeration or kind == .group;
}

/// Completes the symbols in scope for a bare identifier: the standard library
/// root, imported modules, declarations in the document, in-scope variables,
/// and custom types. Duplicate labels are collapsed.
fn writeWordCompletions(
    self: *Server,
    writer: *std.Io.Writer,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    var seen = std.StringHashMap(void).init(self.allocator);
    defer seen.deinit();

    var first = true;

    try writeWordItem(writer, &seen, &first, "std", 9, prefix, edit);

    var module_it = self.symbol_index.modules.iterator();
    while (module_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, 9, prefix, edit);
    }

    // Top-level functions get a signature and an argument snippet; methods
    // (qualified `Type.method`) are offered through member completion instead.
    var callable_it = self.symbol_index.callables.iterator();
    while (callable_it.next()) |entry| {
        const callable = entry.value_ptr.*;
        if (std.mem.indexOfScalar(u8, callable.qualified, '.') != null) continue;
        try writeCallableWordItem(self.allocator, writer, &seen, &first, callable, prefix, edit);
    }

    for (self.symbol_index.symbols.items) |sym| {
        try writeWordItem(writer, &seen, &first, sym.name, completionKindForSymbol(sym.kind), prefix, edit);
    }

    var var_it = self.symbol_index.variables.iterator();
    while (var_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, 6, prefix, edit);
    }

    var type_it = self.symbol_index.types.iterator();
    while (type_it.next()) |entry| {
        try writeWordItem(writer, &seen, &first, entry.key_ptr.*, completionKindForCustomType(entry.value_ptr.kind), prefix, edit);
    }
}

fn writeWordItem(
    writer: *std.Io.Writer,
    seen: *std.StringHashMap(void),
    first: *bool,
    name: []const u8,
    kind: u32,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    if (name.len == 0) return;
    if (!isMemberMatch(name, prefix)) return;

    const gop = try seen.getOrPut(name);
    if (gop.found_existing) return;

    if (!first.*) try writer.writeAll(",");
    first.* = false;
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, name);
    try writer.print(",\"kind\":{d}", .{kind});
    try writeTextEdit(writer, edit, name);
    try writer.writeAll("}");
}

fn writeCallableWordItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    seen: *std.StringHashMap(void),
    first: *bool,
    callable: *const CachedCallable,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    if (!isMemberMatch(callable.name, prefix)) return;

    const gop = try seen.getOrPut(callable.name);
    if (gop.found_existing) return;

    const detail = try callableSignature(allocator, callable);
    defer allocator.free(detail);
    const snippet = try callableSnippet(allocator, callable);
    defer allocator.free(snippet);

    if (!first.*) try writer.writeAll(",");
    first.* = false;
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, callable.name);
    try writer.writeAll(",\"kind\":3,\"detail\":");
    try writeJsonValue(writer, detail);
    try writer.writeAll(",\"insertTextFormat\":2");
    try writeTextEdit(writer, edit, snippet);
    try writer.writeAll("}");
}

fn completionKindForSymbol(symbol_kind: u32) u32 {
    return switch (symbol_kind) {
        12 => 3, // Function
        13 => 6, // Variable
        10 => 13, // Enum
        23 => 22, // Struct / group
        2 => 9, // Module
        else => 6,
    };
}

fn completionKindForCustomType(kind: Types.CustomTypeKind) u32 {
    return switch (kind) {
        .Struct => 22,
        .Enum => 13,
        .Group => 22,
    };
}

/// Emits completions for a standard-library object path (`std`, `std.io`,
/// `std.io.print`, `std.json.Node`, ...). Returns true when the path resolved
/// to a catalog entity, even if the prefix filtered every item — the caller
/// must not then fall through to unrelated imported symbols.
fn writeStdlibCompletions(
    cat: *const stdlib.Catalog,
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    obj_name: []const u8,
    prefix: []const u8,
    edit: CompletionEdit,
) !bool {
    if (std.mem.eql(u8, obj_name, "std")) {
        var first = true;
        for (cat.modules) |*module| {
            if (!isMemberMatch(module.name, prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeStdlibModuleItem(writer, module, edit);
        }
        return true;
    }

    var segments: [8][]const u8 = undefined;
    var count: usize = 0;
    var it = std.mem.splitScalar(u8, obj_name, '.');
    while (it.next()) |segment| {
        if (count >= segments.len) return false;
        segments[count] = segment;
        count += 1;
    }

    var start: usize = 0;
    if (count > 0 and std.mem.eql(u8, segments[0], "std")) start = 1;
    if (start >= count) return false;

    const module = cat.findModule(segments[start]) orelse return false;
    const remaining = count - start;

    if (remaining == 1) {
        var first = true;
        for (module.decls) |*decl| {
            if (!isMemberMatch(decl.short_name, prefix)) continue;
            if (!first) try writer.writeAll(",");
            first = false;
            try writeStdlibDeclItem(allocator, writer, decl, edit);
        }
        return true;
    }

    // `std.<module>.<Type>` — complete the type's members.
    const owner = module.find(segments[start + 1]) orelse return false;
    if (remaining == 2) {
        try writeCatalogTypeMembers(allocator, writer, owner, prefix, edit);
        return true;
    }

    return true;
}

/// Emits a catalog type's methods, associated functions, and enum variants.
fn writeCatalogTypeMembers(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    owner: *const stdlib.Decl,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    var first = true;
    for (owner.members) |*member| {
        if (!isMemberMatch(member.short_name, prefix)) continue;
        if (!first) try writer.writeAll(",");
        first = false;
        try writeStdlibDeclItem(allocator, writer, member, edit);
    }
    for (owner.variants) |variant| {
        if (!isMemberMatch(variant, prefix)) continue;
        if (!first) try writer.writeAll(",");
        first = false;
        try writeStdlibVariantItem(writer, owner.short_name, variant, edit);
    }
}

fn writeStdlibModuleItem(writer: *std.Io.Writer, module: *const stdlib.Module, edit: CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, module.name);
    try writer.writeAll(",\"kind\":9,\"detail\":");
    var buf: [128]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "module std.{s}", .{module.name}) catch "module";
    try writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, module.name);
    try writer.writeAll("}");
}

fn writeStdlibDeclItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    decl: *const stdlib.Decl,
    edit: CompletionEdit,
) !void {
    const detail = try stdlib.signatureDetail(allocator, decl);
    defer allocator.free(detail);

    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, decl.short_name);
    try writer.print(",\"kind\":{d},\"detail\":", .{completionKindFor(decl.kind)});
    try writeJsonValue(writer, detail);
    if (decl.doc) |doc| {
        if (doc.len > 0) {
            try writer.writeAll(",\"documentation\":");
            try writeJsonValue(writer, doc);
        }
    }
    if (decl.kind == .function or decl.kind == .method) {
        const snippet = try stdlib.callSnippet(allocator, decl);
        defer allocator.free(snippet);
        try writer.writeAll(",\"insertTextFormat\":2");
        try writeTextEdit(writer, edit, snippet);
    } else {
        try writeTextEdit(writer, edit, decl.short_name);
    }
    try writer.writeAll("}");
}

fn writeStdlibVariantItem(
    writer: *std.Io.Writer,
    owner: []const u8,
    variant: []const u8,
    edit: CompletionEdit,
) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, variant);
    try writer.writeAll(",\"kind\":20,\"detail\":");
    var buf: [256]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{s}.{s}", .{ owner, variant }) catch "enum member";
    try writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, variant);
    try writer.writeAll("}");
}

fn completionKindFor(kind: stdlib.Kind) u32 {
    return switch (kind) {
        .function => 3,
        .method => 2,
        .structure => 22,
        .enumeration => 13,
        .group => 22,
        .map => 6,
        .constant => 21,
        .variable => 6,
        .module => 9,
    };
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

fn writeTypeMembers(
    allocator: std.mem.Allocator,
    index: *const SymbolIndex,
    writer: *std.Io.Writer,
    type_name: []const u8,
    prefix: []const u8,
    edit: CompletionEdit,
) !void {
    var first = true;

    if (index.types.get(type_name)) |ct| {
        switch (ct.kind) {
            .Struct => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field, edit);
                }
                for (ct.methods) |method| {
                    if (!isMemberMatch(method.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeMethodCompletionItem(allocator, writer, method, edit);
                }
            },
            .Enum => {
                for (ct.enum_variants) |variant| {
                    if (!isMemberMatch(variant, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeEnumCompletionItem(writer, variant, edit);
                }
            },
            .Group => {
                for (ct.fields) |field| {
                    if (!isMemberMatch(field.name, prefix)) continue;
                    if (!first) try writer.writeAll(",");
                    first = false;
                    try writeFieldCompletionItem(writer, field, edit);
                }
            },
        }
    }
}

fn isMemberMatch(name: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return true;
    return std.mem.startsWith(u8, name, prefix);
}

fn writeFieldCompletionItem(writer: *std.Io.Writer, field: CachedField, edit: CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, field.name);
    try writer.writeAll(",\"kind\":5");
    try writer.writeAll(",\"detail\":");
    var buf: [256]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{s}", .{field.type_name}) catch field.type_name;
    try writeJsonValue(writer, detail);
    try writeTextEdit(writer, edit, field.name);
    try writer.writeAll("}");
}

fn writeMethodCompletionItem(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    method: CachedMethod,
    edit: CompletionEdit,
) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, method.name);
    try writer.writeAll(",\"kind\":2");
    try writer.writeAll(",\"detail\":");

    const has_signature = method.params.len > 0 or method.return_type.len > 0;
    if (has_signature) {
        const sig = try methodSignature(allocator, method);
        defer allocator.free(sig);
        try writeJsonValue(writer, sig);
    } else {
        try writeJsonValue(writer, if (method.is_static) "static method" else "method");
    }

    const snippet = try methodSnippet(allocator, method);
    defer allocator.free(snippet);
    try writer.writeAll(",\"insertTextFormat\":2");
    try writeTextEdit(writer, edit, snippet);
    try writer.writeAll("}");
}

fn writeEnumCompletionItem(writer: *std.Io.Writer, variant: []const u8, edit: CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, variant);
    try writer.writeAll(",\"kind\":13");
    try writer.writeAll(",\"detail\":");
    try writeJsonValue(writer, "enum member");
    try writeTextEdit(writer, edit, variant);
    try writer.writeAll("}");
}

fn writeModuleMemberCompletionItem(writer: *std.Io.Writer, name: []const u8, edit: CompletionEdit) !void {
    try writer.writeAll("{\"label\":");
    try writeJsonValue(writer, name);
    try writer.writeAll(",\"kind\":9");
    try writer.writeAll(",\"detail\":");
    try writeJsonValue(writer, "module export");
    try writeTextEdit(writer, edit, name);
    try writer.writeAll("}");
}

fn buildDocumentSymbolsPayload(self: *Server, id: JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(self.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":[");

    var first = true;
    for (self.symbol_index.symbols.items) |sym| {
        if (!first) try writer.writeAll(",");
        first = false;

        try writer.writeAll("{\"name\":");
        try writeJsonValue(writer, sym.name);
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
    var buffer = std.Io.Writer.Allocating.init(self.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":");

    if (extractDocumentContext(self, params)) |ctx| {
        if (findMethodRange(ctx.text, ctx.offset)) |method_range| {
            const name = ctx.text[method_range.start..method_range.end];
            if (InternalMethods.find(name)) |info| {
                const start_pos = offsetToPosition(ctx.text, method_range.start);
                const end_pos = offsetToPosition(ctx.text, method_range.end);

                var markdown = std.Io.Writer.Allocating.init(self.allocator);
                defer markdown.deinit();
                const md = &markdown.writer;
                const sig = try InternalMethods.signature(self.allocator, info);
                defer self.allocator.free(sig);
                try md.writeAll("```doxa\n");
                try md.writeAll(sig);
                try md.writeAll("\n```");
                const prose = InternalMethods.documentation(info);
                if (prose.len > 0) {
                    try md.writeAll("\n\n");
                    try md.writeAll(prose);
                }

                try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
                try writeJsonValue(writer, markdown.written());
                try writer.writeAll("},\"range\":{\"start\":{\"line\":");
                try writer.print("{d}", .{start_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{start_pos.character});
                try writer.writeAll("},\"end\":{\"line\":");
                try writer.print("{d}", .{end_pos.line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{end_pos.character});
                try writer.writeAll("}}}}");

                const payload = try buffer.toOwnedSlice();
                return payload;
            }
        }

        if (findDottedPathAtOffset(ctx.text, ctx.offset)) |path| {
            if (try buildStdlibHover(self, writer, ctx, path)) {
                const payload = try buffer.toOwnedSlice();
                return payload;
            }
            if (try buildUserHover(self, writer, ctx, path)) {
                const payload = try buffer.toOwnedSlice();
                return payload;
            }
        }

        const dot_ctx = computeCompletionAtOffset(ctx.text, ctx.offset);
        if (dot_ctx.kind == .Dot and dot_ctx.object_name != null and dot_ctx.object_name.?.len > 0) {
            if (try buildDotHover(self.allocator, &self.symbol_index, writer, dot_ctx)) {
                try writer.writeAll("}");
                const payload = try buffer.toOwnedSlice();
                return payload;
            }
        }
    }

    try writer.writeAll("null");
    try writer.writeAll("}");
    return buffer.toOwnedSlice();
}

fn buildDotHover(
    allocator: std.mem.Allocator,
    index: *const SymbolIndex,
    writer: *std.Io.Writer,
    comp_ctx: CompletionContext,
) !bool {
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
            var markdown = std.Io.Writer.Allocating.init(allocator);
            defer markdown.deinit();
            const md = &markdown.writer;
            try md.writeAll("```doxa\n");
            if (method.is_static) try md.writeAll("static ");
            const sig = try methodSignature(allocator, method);
            defer allocator.free(sig);
            try md.writeAll(sig);
            try md.writeAll("\n```");

            try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
            try writeJsonValue(writer, markdown.written());
            try writer.writeAll("}}");
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

/// Hover for a user-defined function or struct method, rendering its signature.
fn buildUserHover(self: *Server, writer: *std.Io.Writer, ctx: DocumentContext, path: DottedPath) !bool {
    var signature: ?[]u8 = null;
    defer {
        if (signature) |sig| self.allocator.free(sig);
    }

    if (self.symbol_index.findCallable(path.text)) |callable| {
        signature = try callableSignature(self.allocator, callable);
    } else if (std.mem.lastIndexOfScalar(u8, path.text, '.')) |dot| {
        const object = path.text[0..dot];
        const method_name = path.text[dot + 1 ..];
        if (resolveObjectType(&self.symbol_index, object)) |type_name| {
            if (self.symbol_index.findMethod(type_name, method_name)) |method| {
                signature = try methodSignature(self.allocator, method.*);
            }
        }
    }

    const sig = signature orelse return false;
    const start_pos = offsetToPosition(ctx.text, path.start);
    const end_pos = offsetToPosition(ctx.text, path.end);

    var markdown = std.Io.Writer.Allocating.init(self.allocator);
    defer markdown.deinit();
    try markdown.writer.writeAll("```doxa\n");
    try markdown.writer.writeAll(sig);
    try markdown.writer.writeAll("\n```");

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try writeJsonValue(writer, markdown.written());
    try writer.writeAll("},\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start_pos.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end_pos.character});
    try writer.writeAll("}}}}");
    return true;
}

const DottedPath = struct {
    text: []const u8,
    start: usize,
    end: usize,
};

/// The dotted identifier (`std.http.get`) surrounding `offset`, expanded in
/// both directions so hover works mid-symbol as well as at its end.
fn findDottedPathAtOffset(text: []const u8, offset: usize) ?DottedPath {
    if (text.len == 0) return null;
    const clamped = @min(offset, text.len);

    var start = clamped;
    while (start > 0) {
        const c = text[start - 1];
        if (isIdentChar(c)) {
            start -= 1;
        } else if (c == '.' and start >= 2 and isIdentChar(text[start - 2])) {
            start -= 1;
        } else break;
    }

    var end = clamped;
    while (end < text.len) {
        const c = text[end];
        if (isIdentChar(c)) {
            end += 1;
        } else if (c == '.' and end + 1 < text.len and isIdentChar(text[end + 1])) {
            end += 1;
        } else break;
    }

    while (start < end and text[start] == '.') start += 1;
    while (end > start and text[end - 1] == '.') end -= 1;
    if (start >= end) return null;

    return .{ .text = text[start..end], .start = start, .end = end };
}

fn writeDocBlock(writer: *std.Io.Writer, decl: *const stdlib.Decl) !void {
    try writer.writeAll("```doxa\n");
    try writer.writeAll(decl.signature);
    try writer.writeAll("\n```");
    if (decl.doc) |doc| {
        if (doc.len > 0) {
            try writer.writeAll("\n\n");
            try writer.writeAll(doc);
        }
    }
}

fn buildStdlibHover(self: *Server, writer: *std.Io.Writer, ctx: DocumentContext, path: DottedPath) !bool {
    const cat = if (self.stdlib) |*c| c else return false;

    var markdown = std.Io.Writer.Allocating.init(self.allocator);
    defer markdown.deinit();
    const md = &markdown.writer;

    if (std.mem.eql(u8, path.text, "std")) {
        try md.writeAll("The Doxa standard library. Complete a module, e.g. `std.io`.");
    } else {
        var segments: [8][]const u8 = undefined;
        var count: usize = 0;
        var it = std.mem.splitScalar(u8, path.text, '.');
        while (it.next()) |segment| {
            if (count >= segments.len) return false;
            segments[count] = segment;
            count += 1;
        }

        var start: usize = 0;
        if (count > 0 and std.mem.eql(u8, segments[0], "std")) start = 1;
        if (start >= count) return false;

        const module = cat.findModule(segments[start]) orelse return false;
        const remaining = count - start;

        if (remaining == 1) {
            try md.print("`module std.{s}`", .{module.name});
        } else {
            const owner = module.find(segments[start + 1]) orelse return false;
            if (remaining == 2) {
                try writeDocBlock(md, owner);
            } else {
                const member_name = segments[start + 2];
                if (owner.findMember(member_name)) |member| {
                    try writeDocBlock(md, member);
                } else {
                    var found = false;
                    for (owner.variants) |variant| {
                        if (std.mem.eql(u8, variant, member_name)) {
                            try md.print("`{s}.{s}` — member of enum `{s}`", .{ module.name, owner.short_name, member_name });
                            found = true;
                            break;
                        }
                    }
                    if (!found) return false;
                }
            }
        }
    }

    const start_pos = offsetToPosition(ctx.text, path.start);
    const end_pos = offsetToPosition(ctx.text, path.end);

    try writer.writeAll("{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try writeJsonValue(writer, markdown.written());
    try writer.writeAll("},\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start_pos.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end_pos.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end_pos.character});
    try writer.writeAll("}}}}");
    return true;
}

const CallContext = struct {
    callee: []const u8,
    active_parameter: usize,
};

/// Finds the innermost call whose argument list encloses `offset`, together
/// with the zero-based index of the argument the cursor sits in. Returns null
/// when the cursor is not inside a call. A single level of string skipping
/// keeps commas and parens inside literals from being counted.
fn computeCallAtOffset(text: []const u8, offset: usize) ?CallContext {
    if (text.len == 0) return null;
    const end = if (offset > text.len) text.len else offset;

    var i = end;
    var depth: usize = 0;
    var active: usize = 0;
    while (i > 0) {
        i -= 1;
        const c = text[i];
        if (c == '"') {
            i = skipStringBackward(text, i);
            continue;
        }
        switch (c) {
            ')' => depth += 1,
            '(' => {
                if (depth == 0) {
                    const callee = calleeBefore(text, i) orelse return null;
                    return .{ .callee = callee, .active_parameter = active };
                }
                depth -= 1;
            },
            ',' => {
                if (depth == 0) active += 1;
            },
            else => {},
        }
    }
    return null;
}

fn skipStringBackward(text: []const u8, quote_index: usize) usize {
    var j = quote_index;
    while (j > 0) {
        j -= 1;
        if (text[j] == '"' and !isEscapedAt(text, j)) return j;
    }
    return 0;
}

fn isEscapedAt(text: []const u8, index: usize) bool {
    var backslashes: usize = 0;
    var k = index;
    while (k > 0 and text[k - 1] == '\\') : (k -= 1) backslashes += 1;
    return backslashes % 2 == 1;
}

fn calleeBefore(text: []const u8, open_paren: usize) ?[]const u8 {
    var end = open_paren;
    while (end > 0 and (text[end - 1] == ' ' or text[end - 1] == '\t')) end -= 1;

    var start = end;
    while (start > 0) {
        const c = text[start - 1];
        if (isIdentChar(c) or c == '.' or c == '@') {
            start -= 1;
        } else break;
    }

    while (start < end and text[start] == '.') start += 1;
    if (start >= end) return null;
    return text[start..end];
}

/// Resolves a caller path (`std.io.print`, `io.print`, `json.Node.parse`) to
/// its catalog declaration.
fn resolveCatalogCall(cat: *const stdlib.Catalog, path: []const u8) ?*const stdlib.Decl {
    var segments: [8][]const u8 = undefined;
    var count: usize = 0;
    var it = std.mem.splitScalar(u8, path, '.');
    while (it.next()) |segment| {
        if (count >= segments.len) return null;
        segments[count] = segment;
        count += 1;
    }

    var start: usize = 0;
    if (count > 0 and std.mem.eql(u8, segments[0], "std")) start = 1;
    if (start >= count) return null;

    const module = cat.findModule(segments[start]) orelse return null;
    const remaining = count - start;
    if (remaining == 2) return module.find(segments[start + 1]);
    if (remaining == 3) {
        const owner = module.find(segments[start + 1]) orelse return null;
        return owner.findMember(segments[start + 2]);
    }
    return null;
}

fn buildSignatureHelpPayload(self: *Server, id: JsonValue, params: ?JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(self.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":");

    var wrote = false;
    if (extractDocumentContext(self, params)) |ctx| {
        if (computeCallAtOffset(ctx.text, ctx.offset)) |call| {
            wrote = try writeSignatureHelp(self, writer, call);
        }
    }
    if (!wrote) try writer.writeAll("null");
    try writer.writeAll("}");
    return buffer.toOwnedSlice();
}

fn jsonInteger(value: JsonValue) ?usize {
    return switch (value) {
        .integer => |i| if (i < 0) null else @intCast(i),
        .float => |f| if (f < 0) null else @intFromFloat(f),
        else => null,
    };
}

/// The requested line window from a `textDocument/inlayHint` (or any
/// range-bearing) request, or null when absent/invalid (meaning "no filter").
fn requestedLineRange(params: ?JsonValue) ?[2]usize {
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
fn buildInlayHintPayload(self: *Server, id: JsonValue, params: ?JsonValue) ![]u8 {
    var buffer = std.Io.Writer.Allocating.init(self.allocator);
    defer buffer.deinit();
    var writer = &buffer.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try writeJsonValue(writer, id);
    try writer.writeAll(",\"result\":[");

    const window = requestedLineRange(params);
    var first = true;
    for (self.symbol_index.inlay_hints.items) |hint| {
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
        try writeJsonValue(writer, hint.label);
        try writer.writeAll(",\"kind\":1,\"paddingLeft\":true}");
    }

    try writer.writeAll("]}");
    return buffer.toOwnedSlice();
}

const ResolvedSignature = struct {
    label: []u8,
    params: [][]u8,
    variadic: bool,

    fn deinit(self: ResolvedSignature, allocator: std.mem.Allocator) void {
        allocator.free(self.label);
        for (self.params) |p| allocator.free(p);
        allocator.free(self.params);
    }
};

fn catalogResolved(allocator: std.mem.Allocator, decl: *const stdlib.Decl) !ResolvedSignature {
    var params = std.array_list.Managed([]u8).init(allocator);
    errdefer {
        for (params.items) |p| allocator.free(p);
        params.deinit();
    }
    for (decl.params) |*param| {
        try params.append(try stdlib.parameterLabel(allocator, param));
    }
    return .{
        .label = try stdlib.signatureDetail(allocator, decl),
        .params = try params.toOwnedSlice(),
        .variadic = false,
    };
}

fn callableResolved(allocator: std.mem.Allocator, callable: *const CachedCallable) !ResolvedSignature {
    var params = std.array_list.Managed([]u8).init(allocator);
    errdefer {
        for (params.items) |p| allocator.free(p);
        params.deinit();
    }
    for (callable.params) |param| {
        try params.append(try cachedParamLabel(allocator, param));
    }
    return .{
        .label = try callableSignature(allocator, callable),
        .params = try params.toOwnedSlice(),
        .variadic = false,
    };
}

fn methodResolved(allocator: std.mem.Allocator, method: CachedMethod) !ResolvedSignature {
    var params = std.array_list.Managed([]u8).init(allocator);
    errdefer {
        for (params.items) |p| allocator.free(p);
        params.deinit();
    }
    for (method.params) |param| {
        try params.append(try cachedParamLabel(allocator, param));
    }
    return .{
        .label = try methodSignature(allocator, method),
        .params = try params.toOwnedSlice(),
        .variadic = false,
    };
}

/// Resolves the callable behind a call site: an `@` intrinsic, a standard
/// library path, a user function, or a method on a value whose type is known.
fn resolveSignature(self: *Server, callee: []const u8) !?ResolvedSignature {
    if (callee.len == 0) return null;

    if (callee[0] == '@') {
        const info = InternalMethods.find(callee) orelse return null;
        var params = std.array_list.Managed([]u8).init(self.allocator);
        errdefer {
            for (params.items) |p| self.allocator.free(p);
            params.deinit();
        }
        for (0..info.input_types.len) |idx| {
            try params.append(try InternalMethods.parameterLabel(self.allocator, info, idx));
        }
        return .{
            .label = try InternalMethods.signature(self.allocator, info),
            .params = try params.toOwnedSlice(),
            .variadic = info.arg_count_max == null,
        };
    }

    if (std.mem.lastIndexOfScalar(u8, callee, '.')) |dot| {
        const object = callee[0..dot];
        const method_name = callee[dot + 1 ..];

        if (self.stdlib) |*cat| {
            if (resolveCatalogCall(cat, callee)) |decl| {
                if (decl.kind == .function or decl.kind == .method) {
                    return try catalogResolved(self.allocator, decl);
                }
            }
        }

        const type_name = resolveObjectType(&self.symbol_index, object) orelse return null;
        if (self.symbol_index.findMethod(type_name, method_name)) |method| {
            return try methodResolved(self.allocator, method.*);
        }
        if (self.stdlib) |*cat| {
            if (findCatalogType(cat, type_name)) |owner| {
                if (owner.findMember(method_name)) |member| {
                    if (member.kind == .method or member.kind == .function) {
                        return try catalogResolved(self.allocator, member);
                    }
                }
            }
        }
        return null;
    }

    if (self.symbol_index.findCallable(callee)) |callable| {
        return try callableResolved(self.allocator, callable);
    }
    return null;
}

fn writeSignatureHelp(self: *Server, writer: *std.Io.Writer, call: CallContext) !bool {
    const resolved = (try resolveSignature(self, call.callee)) orelse return false;
    defer resolved.deinit(self.allocator);

    var active = call.active_parameter;
    const declared = resolved.params.len;
    if (!resolved.variadic and declared > 0 and active >= declared) active = declared - 1;

    try writer.writeAll("{\"signatures\":[{\"label\":");
    try writeJsonValue(writer, resolved.label);
    try writer.writeAll(",\"parameters\":[");
    for (resolved.params, 0..) |p, idx| {
        if (idx > 0) try writer.writeAll(",");
        try writer.writeAll("{\"label\":");
        try writeJsonValue(writer, p);
        try writer.writeAll("}");
    }
    try writer.writeAll("]}],\"activeSignature\":0,\"activeParameter\":");
    try writer.print("{d}", .{active});
    try writer.writeAll("}");
    return true;
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

fn typeBaseLabel(base: ast.Type) []const u8 {
    return switch (base) {
        .Int => "int",
        .Byte => "byte",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Array => "array",
        .Function => "function",
        .Struct => "struct",
        .Enum => "enum",
        .Custom => "custom",
        .Map => "map",
        .Nothing => "nothing",
        .Union => "union",
    };
}

fn basicTypeLabel(basic: ast.BasicType) []const u8 {
    return switch (basic) {
        .Integer => "int",
        .Byte => "byte",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Nothing => "nothing",
    };
}

/// Renders a syntactic parameter type (`^name :: string | error.Foo`) the way
/// it is written in source. Falls back to `any` for an untyped parameter.
fn writeTypeExpr(writer: *std.Io.Writer, type_expr: ?*const ast.TypeExpr) !void {
    const expr = type_expr orelse {
        try writer.writeAll("any");
        return;
    };
    switch (expr.data) {
        .Basic => |basic| try writer.writeAll(basicTypeLabel(basic)),
        .Custom => |token| try writer.writeAll(token.lexeme),
        .Array => |array| {
            try writeTypeExpr(writer, array.element_type);
            try writer.writeAll("[]");
        },
        .Struct => try writer.writeAll("struct"),
        .Enum => try writer.writeAll("enum"),
        .Union => |types| {
            for (types, 0..) |t, i| {
                if (i > 0) try writer.writeAll(" | ");
                try writeTypeExpr(writer, t);
            }
        },
        .Map => try writer.writeAll("map"),
    }
}

fn renderTypeExpr(allocator: std.mem.Allocator, type_expr: ?*const ast.TypeExpr) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    try writeTypeExpr(&out.writer, type_expr);
    return out.toOwnedSlice();
}

fn renderTypeInfo(allocator: std.mem.Allocator, type_info: ast.TypeInfo) ![]u8 {
    if (type_info.custom_type) |ct| return allocator.dupe(u8, ct);
    return allocator.dupe(u8, typeBaseLabel(type_info.base));
}

/// A one-line signature for a user callable: `foo(a: int, ^b: string) -> bool`.
fn callableSignature(allocator: std.mem.Allocator, callable: *const CachedCallable) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{callable.name});
    for (callable.params, 0..) |param, i| {
        if (i > 0) try w.writeAll(", ");
        try writeCachedParam(w, param);
    }
    try w.writeAll(")");
    if (callable.return_type.len > 0) {
        try w.print(" -> {s}", .{callable.return_type});
    }
    return out.toOwnedSlice();
}

fn writeCachedParam(writer: *std.Io.Writer, param: CachedParam) !void {
    if (param.alias) try writer.writeAll("^");
    if (param.name.len > 0) {
        try writer.print("{s}: ", .{param.name});
    }
    try writer.writeAll(param.type_text);
}

fn cachedParamLabel(allocator: std.mem.Allocator, param: CachedParam) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    try writeCachedParam(&out.writer, param);
    return out.toOwnedSlice();
}

fn callableSnippet(allocator: std.mem.Allocator, callable: *const CachedCallable) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{callable.name});
    for (callable.params, 0..) |_, i| {
        if (i > 0) try w.writeAll(", ");
        try w.print("${d}", .{i + 1});
    }
    try w.writeAll(")");
    return out.toOwnedSlice();
}

fn methodSignature(allocator: std.mem.Allocator, method: CachedMethod) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{method.name});
    for (method.params, 0..) |param, i| {
        if (i > 0) try w.writeAll(", ");
        try writeCachedParam(w, param);
    }
    try w.writeAll(")");
    if (method.return_type.len > 0) {
        try w.print(" -> {s}", .{method.return_type});
    }
    return out.toOwnedSlice();
}

fn methodSnippet(allocator: std.mem.Allocator, method: CachedMethod) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{method.name});
    for (method.params, 0..) |_, i| {
        if (i > 0) try w.writeAll(", ");
        try w.print("${d}", .{i + 1});
    }
    try w.writeAll(")");
    return out.toOwnedSlice();
}

fn populateSymbolIndexFromScope(index: *SymbolIndex, memory_manager: *MemoryManager) !void {
    const alloc = index.arena.allocator();
    for (memory_manager.scope_pool.items) |scope| {
        if (scope.is_deinited) continue;
        var it = scope.name_map.iterator();
        while (it.next()) |entry| {
            const var_name = entry.key_ptr.*;
            const var_ptr = entry.value_ptr.*;
            if (scope.manager.value_storage.get(var_ptr.storage_id)) |storage| {
                const type_str = packLspTypeString(storage.type_info);
                try index.addVariable(var_name, type_str);

                // Inferred declarations carry their position; render a type
                // inlay hint just past the name.
                if (var_ptr.decl_line > 0) {
                    const label = try std.fmt.allocPrint(alloc, ":: {s}", .{inlayTypeText(storage.type_info)});
                    try index.inlay_hints.append(.{
                        .line = var_ptr.decl_line - 1,
                        .character = var_ptr.decl_column - 1 + var_name.len,
                        .label = label,
                    });
                }
            }
        }
    }
}

fn inlayTypeText(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |ct| return ct;
    return typeBaseLabel(type_info.base);
}

fn trimLine(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') {
        return line[0 .. line.len - 1];
    }
    return line;
}

fn writeJsonValue(writer: *std.Io.Writer, value: anytype) !void {
    try writer.print("{f}", .{std.json.fmt(value, .{})});
}

fn runHarnessMessage(
    io: std.Io,
    allocator: std.mem.Allocator,
    server: *Server,
    sink: *CaptureSink,
    label: []const u8,
    payload: []const u8,
) !void {
    std.debug.print("\n[harness] --> {s}\n", .{label});
    try prettyPrintJson(allocator, payload);
    try server.handlePayload(io, payload);
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

fn readFileAlloc(io: std.Io, allocator: std.mem.Allocator, path: []const u8, max_bytes: usize) ![]u8 {
    return std.Io.Dir.cwd().readFileAlloc(
        io,
        path,
        allocator,
        .limited(max_bytes),
    );
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

fn jsonStringifyAlloc(
    allocator: std.mem.Allocator,
    value: anytype,
    options: std.json.Stringify.Options,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();

    try std.json.Stringify.value(
        value,
        options,
        &aw.writer,
    );

    return try aw.toOwnedSlice();
}

test "completion context captures a dotted object path" {
    const ctx = computeCompletionAtOffset("std.http.ge", "std.http.ge".len);
    try std.testing.expectEqualStrings("std.http", ctx.object_name.?);
    try std.testing.expectEqualStrings("ge", ctx.prefix);

    const plain = computeCompletionAtOffset("node.field", "node.field".len);
    try std.testing.expectEqualStrings("node", plain.object_name.?);
}

test "findDottedPathAtOffset expands both directions" {
    const text = "const r is std.http.get(\"x\")";
    const mid = std.mem.indexOf(u8, text, "http").? + 1;
    const path = findDottedPathAtOffset(text, mid).?;
    try std.testing.expectEqualStrings("std.http.get", path.text);

    const end = std.mem.indexOf(u8, text, "get").? + 3;
    const at_end = findDottedPathAtOffset(text, end).?;
    try std.testing.expectEqualStrings("std.http.get", at_end.text);
}

fn dummyEdit() CompletionEdit {
    return .{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };
}

test "stdlib completions resolve modules, members, and methods" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src =
        \\public function get(url :: string) returns Response {
        \\}
        \\public struct Response {
        \\    public method statusCode() returns int {
        \\    }
        \\}
        \\public enum Kind {
        \\    Ok,
        \\    Err,
        \\}
    ;
    const decls = try stdlib.parseSource(allocator, src);
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    const cat = stdlib.Catalog{ .modules = &modules };

    var members = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try writeStdlibCompletions(&cat, allocator, &members.writer, "std.http", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"label\":\"get\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "get(url: string) -> Response") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"newText\":\"get($1)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, members.written(), "\"label\":\"Response\"") != null);

    var roots = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try writeStdlibCompletions(&cat, allocator, &roots.writer, "std", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, roots.written(), "\"label\":\"http\"") != null);

    var methods = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try writeStdlibCompletions(&cat, allocator, &methods.writer, "std.http.Response", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, methods.written(), "\"label\":\"statusCode\"") != null);

    var variants = std.Io.Writer.Allocating.init(allocator);
    try std.testing.expect(try writeStdlibCompletions(&cat, allocator, &variants.writer, "std.http.Kind", "", dummyEdit()));
    try std.testing.expect(std.mem.indexOf(u8, variants.written(), "\"label\":\"Ok\"") != null);
}

test "word completion context captures the identifier prefix and replacement range" {
    const text = "const foo is ba";
    const ctx = computeCompletionAtOffset(text, text.len);
    try std.testing.expectEqual(CompletionKind.Word, ctx.kind);
    try std.testing.expectEqualStrings("ba", ctx.prefix);
    try std.testing.expectEqual(@as(usize, "const foo is ".len), ctx.replace_start);
    try std.testing.expectEqual(@as(usize, text.len), ctx.replace_end);

    const intrinsic = computeCompletionAtOffset("@pu", 3);
    try std.testing.expectEqual(CompletionKind.Intrinsic, intrinsic.kind);
    try std.testing.expectEqualStrings("@pu", intrinsic.prefix);
    try std.testing.expectEqual(@as(usize, 0), intrinsic.replace_start);

    const dot = computeCompletionAtOffset("foo.", 4);
    try std.testing.expectEqual(CompletionKind.Dot, dot.kind);
    try std.testing.expectEqualStrings("foo", dot.object_name.?);
}

test "call context finds the enclosing call and active parameter" {
    const text = "@insert(arr, ";
    const call = computeCallAtOffset(text, text.len).?;
    try std.testing.expectEqualStrings("@insert", call.callee);
    try std.testing.expectEqual(@as(usize, 1), call.active_parameter);

    const nested = "f(g(1, 2), ";
    const outer = computeCallAtOffset(nested, nested.len).?;
    try std.testing.expectEqualStrings("f", outer.callee);
    try std.testing.expectEqual(@as(usize, 1), outer.active_parameter);

    const with_string = "f(\"a,b\", ";
    const str_call = computeCallAtOffset(with_string, with_string.len).?;
    try std.testing.expectEqualStrings("f", str_call.callee);
    try std.testing.expectEqual(@as(usize, 1), str_call.active_parameter);

    try std.testing.expect(computeCallAtOffset("no call here", "no call here".len) == null);
}

test "signature help describes an intrinsic in progress" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    const text = "@insert(arr, ";
    const call = computeCallAtOffset(text, text.len).?;

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try writeSignatureHelp(&server, &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"@insert(collection: array | string, index: int, value: any) -> nothing\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"collection: array | string\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

test "signature help resolves a stdlib call through the catalog" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), "public function get(url :: string, timeout :: int) returns int {\n}\n");
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };

    const text = "std.http.get(\"x\", ";
    const call = computeCallAtOffset(text, text.len).?;
    try std.testing.expectEqualStrings("std.http.get", call.callee);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try writeSignatureHelp(&server, &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "get(url: string, timeout: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"timeout: int\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

test "intrinsic completion carries a typed signature and snippet" {
    var buf = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer buf.deinit();
    try writeIntrinsicCompletions(std.testing.allocator, &buf.writer, "@pu", dummyEdit());

    const rendered = buf.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"@push\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "@push(collection: array | string, value: any) -> nothing") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"@push($1, $2)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"insertTextFormat\":2") != null);
}

test "word completion offers in-scope symbols and collapses duplicates" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    const alloc = server.symbol_index.arena.allocator();
    try server.symbol_index.addVariable("count", "int");
    try server.symbol_index.addModule("mymod");
    try server.symbol_index.types.put(try alloc.dupe(u8, "Node"), .{
        .name = try alloc.dupe(u8, "Node"),
        .kind = .Struct,
        .fields = &.{},
        .methods = &.{},
        .enum_variants = &.{},
    });
    // A declaration entry with the same name as the variable must not duplicate.
    try server.symbol_index.symbols.append(.{
        .name = try alloc.dupe(u8, "count"),
        .kind = 13,
        .start_line = 0,
        .start_character = 0,
        .end_line = 0,
        .end_character = 5,
    });

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try writeWordCompletions(&server, &out.writer, "", dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"std\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"count\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"mymod\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"Node\"") != null);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, rendered, "\"label\":\"count\""));
}

const USER_SOURCE =
    \\public function add(a :: int, ^b :: string) returns int {
    \\    return a
    \\}
    \\
    \\public struct Node {
    \\    public method kind(level :: int) returns string {
    \\        return "x"
    \\    }
    \\}
;

/// Runs the real lexer/parser/analyzer over `src` so the symbol index is
/// populated exactly as it would be for an open document.
fn analyzeInto(server: *Server, src: []const u8) !void {
    const text = try std.testing.allocator.dupe(u8, src);
    defer std.testing.allocator.free(text);
    var doc = Document{ .path = "test.doxa", .text = text };
    try server.performAnalysis(std.testing.io, &doc, "file:///test.doxa");
}

test "AST collection captures user function and method signatures" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const add = server.symbol_index.findCallable("add") orelse return error.MissingCallable;
    try std.testing.expectEqual(@as(usize, 2), add.params.len);
    try std.testing.expectEqualStrings("a", add.params[0].name);
    try std.testing.expectEqualStrings("int", add.params[0].type_text);
    try std.testing.expect(!add.params[0].alias);
    try std.testing.expect(add.params[1].alias);
    try std.testing.expectEqualStrings("string", add.params[1].type_text);
    try std.testing.expectEqualStrings("int", add.return_type);

    const kind = server.symbol_index.findMethod("Node", "kind") orelse return error.MissingMethod;
    try std.testing.expectEqual(@as(usize, 1), kind.params.len);
    try std.testing.expectEqualStrings("level", kind.params[0].name);
    try std.testing.expectEqualStrings("int", kind.params[0].type_text);
    try std.testing.expectEqualStrings("string", kind.return_type);
}

test "word completion snippets user functions" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try writeWordCompletions(&server, &out.writer, "", dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"add\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"add($1, $2)\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "add(a: int, ^b: string) -> int") != null);
}

test "member completion offers user method signatures and snippets" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const ctx = computeCompletionAtOffset("Node.", "Node.".len);
    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try writeMemberCompletions(&server, &out.writer, ctx, dummyEdit());

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"kind\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "kind(level: int) -> string") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"kind($1)\"") != null);
}

test "signature help covers user functions" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    const text = "add(1, ";
    const call = computeCallAtOffset(text, text.len).?;
    try std.testing.expectEqualStrings("add", call.callee);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try std.testing.expect(try writeSignatureHelp(&server, &out.writer, call));

    const rendered = out.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "add(a: int, ^b: string) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"^b: string\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"activeParameter\":1") != null);
}

const STDLIB_TYPE_SOURCE =
    \\public struct Response {
    \\    public method statusCode(code :: int) returns int {
    \\        return code
    \\    }
    \\}
;

test "std catalog-typed values get member completion and signature help" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), STDLIB_TYPE_SOURCE);
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };
    try server.symbol_index.addVariable("resp", "Response");

    const ctx = computeCompletionAtOffset("resp.", "resp.".len);
    var completion = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer completion.deinit();
    try writeMemberCompletions(&server, &completion.writer, ctx, dummyEdit());

    const rendered = completion.written();
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"label\":\"statusCode\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "statusCode(code: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "\"newText\":\"statusCode($1)\"") != null);

    const text = "resp.statusCode(";
    const call = computeCallAtOffset(text, text.len).?;
    try std.testing.expectEqualStrings("resp.statusCode", call.callee);

    var help = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer help.deinit();
    try std.testing.expect(try writeSignatureHelp(&server, &help.writer, call));
    try std.testing.expect(std.mem.indexOf(u8, help.written(), "statusCode(code: int) -> int") != null);
    try std.testing.expect(std.mem.indexOf(u8, help.written(), "\"label\":\"code: int\"") != null);
}

fn expectValidHover(server: *Server, uri: []const u8, character: usize) ![]u8 {
    const params_json = try std.fmt.allocPrint(
        std.testing.allocator,
        "{{\"textDocument\":{{\"uri\":\"{s}\"}},\"position\":{{\"line\":0,\"character\":{d}}}}}",
        .{ uri, character },
    );
    defer std.testing.allocator.free(params_json);
    var parsed_params = try std.json.parseFromSlice(JsonValue, std.testing.allocator, params_json, .{});
    defer parsed_params.deinit();

    const payload = try buildHoverPayload(server, .{ .integer = 1 }, parsed_params.value);
    errdefer std.testing.allocator.free(payload);

    // A malformed payload must fail here rather than silently reaching a client.
    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();
    return payload;
}

test "hover payload stays valid JSON for intrinsics and user callables" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, USER_SOURCE);

    try server.storeDocument("file:///hover.doxa", "@push(arr, 1)");
    const intrinsic = try expectValidHover(&server, "file:///hover.doxa", 2);
    defer std.testing.allocator.free(intrinsic);
    try std.testing.expect(std.mem.indexOf(u8, intrinsic, "@push(collection: array | string, value: any) -> nothing") != null);

    // `add` is a user function: hover must render its collected signature.
    try server.storeDocument("file:///hover.doxa", "const x is add(1, \"y\")");
    const user = try expectValidHover(&server, "file:///hover.doxa", 12);
    defer std.testing.allocator.free(user);
    try std.testing.expect(std.mem.indexOf(u8, user, "add(a: int, ^b: string) -> int") != null);

    // Standard-library hover shares the same payload shape.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const decls = try stdlib.parseSource(arena.allocator(), "public function get(url :: string) returns int {\n    return 1\n}\n");
    const modules = [_]stdlib.Module{.{ .name = "http", .decls = decls }};
    server.stdlib = .{ .modules = &modules };
    try server.storeDocument("file:///hover.doxa", "std.http.get(\"x\")");
    const std_hover = try expectValidHover(&server, "file:///hover.doxa", 9);
    defer std.testing.allocator.free(std_hover);
    try std.testing.expect(std.mem.indexOf(u8, std_hover, "get(url :: string)") != null);
}

const INLAY_SOURCE =
    \\const inferred is 1 + 2
    \\var explicit :: int is 5
    \\public function f() returns int {
    \\    const local is 3
    \\    return local
    \\}
;

test "inlay hints describe inferred variable types only" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, INLAY_SOURCE);

    const hints = server.symbol_index.inlay_hints.items;
    try std.testing.expectEqual(@as(usize, 2), hints.len);

    try std.testing.expectEqual(@as(usize, 0), hints[0].line);
    try std.testing.expectEqual(@as(usize, "const ".len + "inferred".len), hints[0].character);
    try std.testing.expectEqualStrings(":: int", hints[0].label);

    try std.testing.expectEqual(@as(usize, 3), hints[1].line);
    try std.testing.expectEqualStrings(":: int", hints[1].label);
}

test "inlay hint payload is valid JSON and honors the requested range" {
    var reporter = Reporter.init(std.testing.io, std.testing.allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var sink = CaptureSink.init(std.testing.allocator);
    defer sink.deinit();
    var server = Server.init(std.testing.allocator, &reporter, sink.asResponseSink(), false);
    defer server.deinit();

    try analyzeInto(&server, INLAY_SOURCE);

    const params_json =
        "{\"textDocument\":{\"uri\":\"file:///inlay.doxa\"},\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":100}}}";
    var parsed_params = try std.json.parseFromSlice(JsonValue, std.testing.allocator, params_json, .{});
    defer parsed_params.deinit();

    const payload = try buildInlayHintPayload(&server, .{ .integer = 1 }, parsed_params.value);
    defer std.testing.allocator.free(payload);

    var parsed = try std.json.parseFromSlice(JsonValue, std.testing.allocator, payload, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?;
    try std.testing.expectEqual(@as(usize, 1), result.array.items.len);
    const hint = result.array.items[0].object;
    try std.testing.expectEqual(@as(i64, 0), hint.get("position").?.object.get("line").?.integer);
    try std.testing.expectEqualStrings(":: int", hint.get("label").?.string);
    try std.testing.expectEqual(@as(i64, 1), hint.get("kind").?.integer);
}
