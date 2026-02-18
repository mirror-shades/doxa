const std = @import("std");
const builtin = @import("builtin");
const Errors = @import("errors.zig");
const ErrorList = Errors.ErrorList;

pub const MAX_DIAGNOSTIC_LOG_BYTES: usize = 2 * (1024 * 1024);

pub const Severity = enum {
    Error,
    Warning,
    Info,
    Hint,
    Internal,
};

pub const DiagnosticPhase = enum {
    CompileTime,
    Runtime,
    Internal,
    Debug,
};

pub const ReporterOptions = struct {
    max_diagnostics: i32 = 1000,
    warn_as_error: bool = false,
    debug_verbose: bool = false,
    debug_lexer: bool = false,
    debug_parser: bool = false,
    debug_semantic: bool = false,
    debug_hir: bool = false,
    debug_bytecode: bool = false,
    debug_execution: bool = false,
    debug_memory: bool = false,
    log_to_stderr: bool = true,
    publish_debounce_ns: u64 = 30 * std.time.ns_per_ms,
};

pub const Range = struct {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
};

pub const Location = struct {
    file: []const u8,
    file_uri: ?[]const u8 = null,
    range: Range,
};

pub const RelatedInformation = struct {
    message: []const u8,
    location: Location,
};

pub const Diagnostic = struct {
    phase: DiagnosticPhase,
    severity: Severity,
    message: []const u8,
    loc: ?Location,
    code: ?[]const u8 = null,
    related_info: ?[]RelatedInformation = null,
    source: ?[]const u8 = "DoxVM",
};

pub const DiagnosticView = struct {
    allocator: std.mem.Allocator,
    items: []const *const Diagnostic,

    pub fn empty(allocator: std.mem.Allocator) DiagnosticView {
        return .{ .allocator = allocator, .items = &[_]*const Diagnostic{} };
    }

    pub fn deinit(self: *DiagnosticView) void {
        if (self.items.len > 0) {
            self.allocator.free(self.items);
        }
        self.* = DiagnosticView.empty(self.allocator);
    }
};

pub const Reporter = struct {
    diagnostics: std.array_list.Managed(Diagnostic),
    options: ReporterOptions,
    allocator: std.mem.Allocator,
    debug_verbose: bool,
    debug_lexer: bool,
    debug_parser: bool,
    debug_semantic: bool,
    debug_hir: bool,
    debug_bytecode: bool,
    debug_execution: bool,
    debug_memory: bool,
    file_uri_cache: std.StringHashMap([]const u8),
    published_state: std.StringHashMap(PublishedState),

    const FileNeedleContext = struct {
        treat_as_uri: bool,
        normalized_uri: ?[]const u8 = null,
        normalized_path: ?[]const u8 = null,
        path_storage: ?[]u8 = null,

        pub fn init(reporter: *Reporter, file_or_uri: []const u8) FileNeedleContext {
            var ctx = FileNeedleContext{
                .treat_as_uri = std.mem.startsWith(u8, file_or_uri, "file://"),
            };

            if (ctx.treat_as_uri) {
                ctx.normalized_uri = file_or_uri;
                const path = convertUriToPath(reporter.allocator, file_or_uri) catch null;
                if (path) |p| {
                    ctx.path_storage = p;
                    ctx.normalized_path = p;
                    ctx.normalized_uri = reporter.ensureFileUri(p) catch file_or_uri;
                }
            } else {
                ctx.normalized_path = file_or_uri;
                ctx.normalized_uri = reporter.ensureFileUri(file_or_uri) catch file_or_uri;
            }

            return ctx;
        }

        pub fn deinit(self: *FileNeedleContext, reporter: *Reporter) void {
            if (self.path_storage) |storage| {
                reporter.allocator.free(storage);
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, options: ReporterOptions) Reporter {
        return .{
            .diagnostics = std.array_list.Managed(Diagnostic).init(allocator),
            .options = options,
            .allocator = allocator,
            .debug_verbose = options.debug_verbose,
            .debug_lexer = options.debug_lexer,
            .debug_parser = options.debug_parser,
            .debug_semantic = options.debug_semantic,
            .debug_hir = options.debug_hir,
            .debug_bytecode = options.debug_bytecode,
            .debug_execution = options.debug_execution,
            .debug_memory = options.debug_memory,
            .file_uri_cache = std.StringHashMap([]const u8).init(allocator),
            .published_state = std.StringHashMap(PublishedState).init(allocator),
        };
    }

    pub fn deinit(self: *Reporter) void {
        self.clear();
        self.diagnostics.deinit();

        var published_it = self.published_state.iterator();
        while (published_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.published_state.deinit();

        var it = self.file_uri_cache.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.file_uri_cache.deinit();
    }

    pub fn report(
        self: *Reporter,
        phase: DiagnosticPhase,
        severity: Severity,
        loc: ?Location,
        code: ?[]const u8,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.reportWithRelatedInternal(phase, severity, loc, code, null, fmt, args);
    }

    pub fn reportWithRelated(
        self: *Reporter,
        phase: DiagnosticPhase,
        severity: Severity,
        loc: ?Location,
        code: ?[]const u8,
        related: []const RelatedInformation,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.reportWithRelatedInternal(phase, severity, loc, code, related, fmt, args);
    }

    fn reportWithRelatedInternal(
        self: *Reporter,
        phase: DiagnosticPhase,
        severity: Severity,
        loc: ?Location,
        code: ?[]const u8,
        related: ?[]const RelatedInformation,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        if (self.diagnostics.items.len >= self.options.max_diagnostics) return;

        var final_severity = severity;
        if (severity == .Warning and self.options.warn_as_error) {
            final_severity = .Error;
        }

        const msg_len = std.fmt.count(fmt, args);
        const msg_buf = self.allocator.alloc(u8, msg_len) catch {
            const fallback_msg = "out of memory while formatting diagnostic";
            const fallback_copy = self.allocator.dupe(u8, fallback_msg) catch return;

            const diag = Diagnostic{
                .phase = phase,
                .severity = final_severity,
                .message = fallback_copy,
                .loc = loc,
                .code = code,
                .related_info = null,
                .source = "DoxVM",
            };

            self.appendDiagnostic(diag);
            std.debug.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), fallback_copy });
            return;
        };

        const msg_copy = std.fmt.bufPrint(msg_buf, fmt, args) catch |err| {
            self.allocator.free(msg_buf);
            const fallback_msg = switch (err) {
                error.NoSpaceLeft => "message formatting failed",
            };
            const fallback_copy = self.allocator.dupe(u8, fallback_msg) catch return;

            const diag = Diagnostic{
                .phase = phase,
                .severity = final_severity,
                .message = fallback_copy,
                .loc = loc,
                .code = code,
                .related_info = null,
                .source = "DoxVM",
            };

            self.appendDiagnostic(diag);
            std.debug.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), fallback_copy });
            return;
        };

        const related_clone = self.cloneRelatedInformation(related) catch null;

        const diag = Diagnostic{
            .phase = phase,
            .severity = final_severity,
            .message = msg_copy,
            .loc = loc,
            .code = code,
            .related_info = related_clone,
            .source = "DoxVM",
        };

        self.appendDiagnostic(diag);
    }

    fn debugLexer(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_lexer and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    fn debugParser(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_parser and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    fn debugSemantic(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_semantic and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    fn debugHir(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_hir and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    fn debugBytecode(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_bytecode and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    fn debugExecution(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_execution and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    pub fn debugMemory(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug_memory and !self.debug_verbose) return;
        self.report(.Debug, .Hint, loc, code, fmt, args);
    }

    pub fn reportCompileError(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        self.report(.CompileTime, .Error, loc, code, fmt, args);
    }

    pub fn reportRuntimeError(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        self.report(.Runtime, .Error, loc, code, fmt, args);
    }

    pub fn reportWarning(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        self.report(.CompileTime, .Warning, loc, code, fmt, args);
    }

    pub fn reportInfo(self: *Reporter, loc: ?Location, code: ?[]const u8, comptime fmt: []const u8, args: anytype) void {
        self.report(.CompileTime, .Info, loc, code, fmt, args);
    }

    pub fn reportInternalError(
        self: *Reporter,
        comptime fmt: []const u8,
        args: anytype,
        comptime src: std.builtin.SourceLocation,
    ) void {
        self.reportInternal(fmt, args, src);
    }

    pub fn reportInternal(self: *Reporter, comptime fmt: []const u8, args: anytype, comptime src: std.builtin.SourceLocation) void {
        const loc = Location{
            .file = src.file,
            .file_uri = self.ensureFileUri(src.file) catch null,
            .range = .{
                .start_line = src.line,
                .start_col = 0,
                .end_line = src.line,
                .end_col = 0,
            },
        };

        self.report(.Debug, .Hint, loc, null, fmt, args);
    }

    pub fn hasErrors(self: *Reporter) bool {
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .Error or diag.severity == .Internal) return true;
        }
        return false;
    }

    pub fn hasWarnings(self: *Reporter) bool {
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .Warning) return true;
        }
        return false;
    }

    pub fn countBySeverity(self: *Reporter, severity: Severity) i32 {
        var count: i32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == severity) count += 1;
        }
        return count;
    }

    pub fn totalDiagnostics(self: *Reporter) i32 {
        return self.diagnostics.items.len;
    }

    pub fn hasCompileErrors(self: *Reporter) bool {
        for (self.diagnostics.items) |diag| {
            if (diag.phase == .CompileTime and (diag.severity == .Error or diag.severity == .Internal)) return true;
        }
        return false;
    }

    pub fn clear(self: *Reporter) void {
        for (self.diagnostics.items) |diag| {
            self.freeDiagnostic(diag);
        }
        self.diagnostics.clearRetainingCapacity();
    }

    pub fn clearByFile(self: *Reporter, file_or_uri: []const u8) void {
        if (self.diagnostics.items.len == 0) return;

        var needle = FileNeedleContext.init(self, file_or_uri);
        defer needle.deinit(self);

        var write_index: usize = 0;
        for (self.diagnostics.items) |diag| {
            var should_remove = false;
            if (diag.loc) |location| {
                should_remove = self.locationMatchesNeedle(location, &needle);
            }

            if (should_remove) {
                self.freeDiagnostic(diag);
                continue;
            }

            self.diagnostics.items[write_index] = diag;
            write_index += 1;
        }

        self.diagnostics.shrinkRetainingCapacity(write_index);
    }

    pub fn reportBatch(self: *Reporter, diags: []const Diagnostic) void {
        for (diags) |diag| {
            if (self.diagnostics.items.len >= self.options.max_diagnostics) break;
            self.appendDiagnosticClone(&diag);
        }
    }

    pub fn filterBySeverity(self: *Reporter, allocator: std.mem.Allocator, severity: Severity) !DiagnosticView {
        var matches = std.ArrayList(*const Diagnostic).init(allocator);
        errdefer matches.deinit();

        for (self.diagnostics.items, 0..) |diag, idx| {
            if (diag.severity == severity) {
                try matches.append(&self.diagnostics.items[idx]);
            }
        }

        const owned = matches.toOwnedSlice() catch {
            matches.deinit();
            return error.OutOfMemory;
        };
        return .{ .allocator = allocator, .items = owned };
    }

    pub fn filterByFile(self: *Reporter, allocator: std.mem.Allocator, file_or_uri: []const u8) !DiagnosticView {
        var matches = std.ArrayList(*const Diagnostic).init(allocator);
        errdefer matches.deinit();

        var needle = FileNeedleContext.init(self, file_or_uri);
        defer needle.deinit(self);

        for (self.diagnostics.items, 0..) |diag, idx| {
            if (diag.loc) |location| {
                if (self.locationMatchesNeedle(location, &needle)) {
                    try matches.append(&self.diagnostics.items[idx]);
                }
            }
        }

        const owned = matches.toOwnedSlice() catch {
            matches.deinit();
            return error.OutOfMemory;
        };
        return .{ .allocator = allocator, .items = owned };
    }

    pub fn toLspDiagnostics(self: *Reporter, allocator: std.mem.Allocator, file_or_uri: []const u8) ![]u8 {
        var buffer = std.array_list.Managed(u8).init(allocator);
        errdefer buffer.deinit();

        var writer = buffer.writer();
        try writer.writeByte('[');

        var needle = FileNeedleContext.init(self, file_or_uri);
        defer needle.deinit(self);

        var emitted = false;
        for (self.diagnostics.items) |*diag_ptr| {
            const diag = diag_ptr.*;
            const loc = diag.loc orelse continue;
            if (!self.locationMatchesNeedle(loc, &needle)) continue;

            if (emitted) try writer.writeByte(',');
            emitted = true;
            try self.writeLspDiagnostic(writer, diag_ptr, loc);
        }

        try writer.writeByte(']');
        return try buffer.toOwnedSlice();
    }

    pub fn buildPublishDiagnosticsPayload(self: *Reporter, allocator: std.mem.Allocator, file_or_uri: []const u8) ![]u8 {
        const diagnostics_json = try self.toLspDiagnostics(allocator, file_or_uri);
        defer allocator.free(diagnostics_json);

        var buffer = std.array_list.Managed(u8).init(allocator);
        errdefer buffer.deinit();
        var writer = buffer.writer();

        const uri = self.normalizeFileOrUri(file_or_uri);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
        try writeJsonString(writer, uri);
        try writer.writeAll(",\"diagnostics\":");
        try writer.writeAll(diagnostics_json);
        try writer.writeAll("}}");

        return try buffer.toOwnedSlice();
    }

    pub fn diagnosticsChanged(self: *Reporter, file_or_uri: []const u8) bool {
        const target = self.normalizeFileOrUri(file_or_uri);
        const new_hash = computeFileFingerprint(self, target);
        if (self.published_state.get(target)) |state| {
            return state.hash != new_hash;
        }
        return true;
    }

    pub fn shouldThrottlePublish(self: *Reporter, file_or_uri: []const u8, timestamp_ns: i128) bool {
        const interval = @as(i128, @intCast(self.options.publish_debounce_ns));
        if (interval == 0) return false;

        const target = self.normalizeFileOrUri(file_or_uri);
        const state = self.published_state.get(target) orelse return false;
        if (state.last_publish_ns == std.math.minInt(i128)) return false;

        const delta = timestamp_ns - state.last_publish_ns;
        if (delta < 0) return true;
        return delta < interval;
    }

    pub fn markDiagnosticsPublished(self: *Reporter, file_or_uri: []const u8, timestamp_ns: i128) !void {
        const target = self.normalizeFileOrUri(file_or_uri);
        const fingerprint = computeFileFingerprint(self, target);
        const gop = try self.published_state.getOrPut(target);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.allocator.dupe(u8, target);
        }
        gop.value_ptr.* = .{
            .hash = fingerprint,
            .last_publish_ns = timestamp_ns,
        };
    }

    pub fn dropPublishedDiagnostics(self: *Reporter, file_or_uri: []const u8) void {
        const target = self.normalizeFileOrUri(file_or_uri);
        if (self.published_state.fetchRemove(target)) |kv| {
            self.allocator.free(kv.key);
        }
    }

    fn writeLspDiagnostic(self: *Reporter, writer: anytype, diag: *const Diagnostic, loc: Location) !void {
        try writer.writeByte('{');
        try writer.writeAll("\"range\":");
        try writeLspRange(writer, loc.range);

        try writer.writeAll(",\"severity\":");
        try std.fmt.format(writer, "{d}", .{severityToLspValue(diag.severity)});

        if (diag.source) |source| {
            try writer.writeAll(",\"source\":");
            try writeJsonString(writer, source);
        }

        if (diag.code) |code| {
            try writer.writeAll(",\"code\":");
            try writeJsonString(writer, code);
        }

        try writer.writeAll(",\"message\":");
        try writeJsonString(writer, diag.message);

        if (diag.related_info) |related| {
            var emitted = false;
            for (related) |entry| {
                if (!emitted) {
                    try writer.writeAll(",\"relatedInformation\":[");
                    emitted = true;
                } else {
                    try writer.writeByte(',');
                }

                try writer.writeByte('{');
                try writer.writeAll("\"location\":{");
                try writer.writeAll("\"uri\":");
                const uri = self.uriForLocation(entry.location);
                try writeJsonString(writer, uri);
                try writer.writeAll(",\"range\":");
                try writeLspRange(writer, entry.location.range);
                try writer.writeAll("},\"message\":");
                try writeJsonString(writer, entry.message);
                try writer.writeByte('}');
            }

            if (emitted) {
                try writer.writeByte(']');
            }
        }

        try writer.writeByte('}');
    }

    fn uriForLocation(self: *Reporter, loc: Location) []const u8 {
        if (loc.file_uri) |uri| return uri;
        if (loc.file.len == 0) return "";
        return self.ensureFileUri(loc.file) catch loc.file;
    }

    fn normalizeFileOrUri(self: *Reporter, file_or_uri: []const u8) []const u8 {
        if (file_or_uri.len == 0) return file_or_uri;
        if (std.mem.startsWith(u8, file_or_uri, "file://")) return file_or_uri;
        return self.ensureFileUri(file_or_uri) catch file_or_uri;
    }

    fn appendDiagnostic(self: *Reporter, diag: Diagnostic) void {
        self.diagnostics.append(diag) catch {
            self.freeDiagnostic(diag);
            return;
        };
        const stored = &self.diagnostics.items[self.diagnostics.items.len - 1];
        self.logDiagnostic(stored);
    }

    fn appendDiagnosticClone(self: *Reporter, source: *const Diagnostic) void {
        const msg_copy = self.allocator.dupe(u8, source.message) catch return;
        errdefer self.allocator.free(msg_copy);

        const related_source: ?[]const RelatedInformation = source.related_info;
        const related_clone = self.cloneRelatedInformation(related_source) catch null;

        const diag = Diagnostic{
            .phase = source.phase,
            .severity = source.severity,
            .message = msg_copy,
            .loc = source.loc,
            .code = source.code,
            .related_info = related_clone,
            .source = source.source,
        };

        self.appendDiagnostic(diag);
    }

    fn cloneRelatedInformation(
        self: *Reporter,
        maybe_related: ?[]const RelatedInformation,
    ) !?[]RelatedInformation {
        const related = maybe_related orelse return null;
        if (related.len == 0) return null;

        const storage = try self.allocator.alloc(RelatedInformation, related.len);
        var copied: usize = 0;
        errdefer {
            var i: usize = 0;
            while (i < copied) : (i += 1) {
                self.allocator.free(storage[i].message);
            }
            self.allocator.free(storage);
        }

        while (copied < related.len) : (copied += 1) {
            const msg_copy = try self.allocator.dupe(u8, related[copied].message);
            storage[copied] = .{
                .message = msg_copy,
                .location = related[copied].location,
            };
        }

        return storage;
    }

    fn freeRelatedInformation(self: *Reporter, maybe_related: ?[]RelatedInformation) void {
        if (maybe_related) |related| {
            for (related) |entry| {
                self.allocator.free(entry.message);
            }
            self.allocator.free(related);
        }
    }

    fn freeDiagnostic(self: *Reporter, diag: Diagnostic) void {
        self.allocator.free(diag.message);
        self.freeRelatedInformation(diag.related_info);
    }

    fn logDiagnostic(self: *Reporter, diag: *const Diagnostic) void {
        var line_buf = std.array_list.Managed(u8).init(self.allocator);
        defer line_buf.deinit();

        if (diag.loc) |l| {
            const file = l.file;
            const sl = l.range.start_line;
            const sc = l.range.start_col;
            if (diag.code) |c| {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}][{s}] {s}:{d}:{d}: {s}", .{
                    @tagName(diag.phase),
                    @tagName(diag.severity),
                    c,
                    file,
                    sl,
                    sc,
                    diag.message,
                }) catch {};
            } else {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}] {s}:{d}:{d}: {s}", .{
                    @tagName(diag.phase),
                    @tagName(diag.severity),
                    file,
                    sl,
                    sc,
                    diag.message,
                }) catch {};
            }
        } else {
            if (diag.code) |c| {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}][{s}] {s}", .{
                    @tagName(diag.phase),
                    @tagName(diag.severity),
                    c,
                    diag.message,
                }) catch {};
            } else {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}] {s}", .{
                    @tagName(diag.phase),
                    @tagName(diag.severity),
                    diag.message,
                }) catch {};
            }
        }

        const line = line_buf.items;
        if (self.options.log_to_stderr) {
            std.debug.print("DoxVM: {s}\n", .{line});
        }
    }

    fn locationMatchesNeedle(self: *Reporter, loc: Location, ctx: *const FileNeedleContext) bool {
        _ = self;

        if (ctx.treat_as_uri) {
            if (ctx.normalized_uri) |needle_uri| {
                if (loc.file_uri) |uri| {
                    if (std.mem.eql(u8, uri, needle_uri)) return true;
                }
            }

            if (ctx.normalized_path) |needle_path| {
                if (loc.file.len > 0 and pathsEqual(loc.file, needle_path)) return true;
            }

            return false;
        }

        if (ctx.normalized_path) |needle_path| {
            if (loc.file.len > 0 and pathsEqual(loc.file, needle_path)) return true;
        }

        if (ctx.normalized_uri) |needle_uri| {
            if (loc.file_uri) |uri| {
                if (std.mem.eql(u8, uri, needle_uri)) return true;
            }
        }

        return false;
    }

    fn pathsEqual(a: []const u8, b: []const u8) bool {
        if (builtin.os.tag == .windows) {
            return std.ascii.eqlIgnoreCase(a, b);
        }
        return std.mem.eql(u8, a, b);
    }

    pub fn ensureFileUri(self: *Reporter, path: []const u8) ![]const u8 {
        const canonical = canonicalizePath(self.allocator, path) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.AccessDenied => return error.AccessDenied,
            error.NotDir => return error.NotDir,
            error.NotSupported => return error.NotSupported,
            error.FileSystem => return error.FileSystem,
            error.UnrecognizedVolume => return error.UnrecognizedVolume,
            else => return error.BadPathName,
        };
        errdefer self.allocator.free(canonical);

        if (self.file_uri_cache.get(canonical)) |existing| {
            self.allocator.free(canonical);
            return existing;
        }

        const uri = convertPathToUri(self.allocator, canonical) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.BadPathName,
        };
        errdefer self.allocator.free(uri);

        self.file_uri_cache.put(canonical, uri) catch |err| return err;
        return uri;
    }
};

fn writeLspRange(writer: anytype, range: Range) !void {
    const end_line_raw = if (range.end_line == 0) range.start_line else range.end_line;
    const end_col_raw = if (range.end_col == 0) range.start_col else range.end_col;

    const start_line = zeroBased(range.start_line);
    const start_col = zeroBased(range.start_col);
    const end_line = zeroBased(end_line_raw);
    const end_col = zeroBased(end_col_raw);

    try std.fmt.format(
        writer,
        "{{\"start\":{{\"line\":{d},\"character\":{d}}},\"end\":{{\"line\":{d},\"character\":{d}}}}}",
        .{ start_line, start_col, end_line, end_col },
    );
}

fn zeroBased(value: usize) usize {
    return if (value > 0) value - 1 else 0;
}

fn severityToLspValue(severity: Severity) u8 {
    return switch (severity) {
        .Error, .Internal => 1,
        .Warning => 2,
        .Info => 3,
        .Hint => 4,
    };
}

fn writeJsonString(writer: anytype, text: []const u8) !void {
    try writer.writeByte('"');
    for (text) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x08 => try writer.writeAll("\\b"),
            0x0C => try writer.writeAll("\\f"),
            else => {
                if (c < 0x20) {
                    try writeUnicodeEscape(writer, @as(u16, c));
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

const PublishedState = struct {
    hash: u64,
    last_publish_ns: i128,
};

fn fileOptionalHashUpdate(hasher: *std.hash.Wyhash, maybe_text: ?[]const u8) void {
    if (maybe_text) |text| {
        hasher.update(&[_]u8{1});
        fileHashSlice(hasher, text);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn fileHashSlice(hasher: *std.hash.Wyhash, text: []const u8) void {
    const len_bytes = std.mem.asBytes(&text.len);
    hasher.update(len_bytes);
    hasher.update(text);
}

fn hashRange(hasher: *std.hash.Wyhash, range: Range) void {
    hasher.update(std.mem.asBytes(&range.start_line));
    hasher.update(std.mem.asBytes(&range.start_col));
    hasher.update(std.mem.asBytes(&range.end_line));
    hasher.update(std.mem.asBytes(&range.end_col));
}

fn hashLocation(hasher: *std.hash.Wyhash, loc: Location) void {
    fileHashSlice(hasher, loc.file);
    fileOptionalHashUpdate(hasher, loc.file_uri);
    hashRange(hasher, loc.range);
}

fn hashRelated(hasher: *std.hash.Wyhash, related_info: []RelatedInformation) void {
    hasher.update(std.mem.asBytes(&related_info.len));
    for (related_info) |entry| {
        fileHashSlice(hasher, entry.message);
        hashLocation(hasher, entry.location);
    }
}

fn hashDiagnostic(hasher: *std.hash.Wyhash, diag: Diagnostic) void {
    const phase_byte: u8 = @intFromEnum(diag.phase);
    hasher.update(&[_]u8{phase_byte});

    const severity_byte: u8 = @intFromEnum(diag.severity);
    hasher.update(&[_]u8{severity_byte});

    fileOptionalHashUpdate(hasher, diag.code);
    fileOptionalHashUpdate(hasher, diag.source);
    fileHashSlice(hasher, diag.message);

    if (diag.loc) |loc| {
        hasher.update(&[_]u8{1});
        hashLocation(hasher, loc);
    } else {
        hasher.update(&[_]u8{0});
    }

    if (diag.related_info) |related| {
        hasher.update(&[_]u8{1});
        hashRelated(hasher, related);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn computeFileFingerprint(self: *Reporter, file_or_uri: []const u8) u64 {
    var hasher = std.hash.Wyhash.init(0);
    var needle = Reporter.FileNeedleContext.init(self, file_or_uri);
    defer needle.deinit(self);

    var matched = false;
    for (self.diagnostics.items) |diag| {
        const loc = diag.loc orelse continue;
        if (!self.locationMatchesNeedle(loc, &needle)) continue;
        matched = true;
        hashDiagnostic(&hasher, diag);
    }
    return if (matched) hasher.final() else 0;
}

fn writeUnicodeEscape(writer: anytype, value: u16) !void {
    try writer.writeAll("\\u");
    var buffer: [4]u8 = undefined;
    var remaining = value;
    var index: usize = buffer.len;
    while (index > 0) {
        index -= 1;
        buffer[index] = hexDigit(@intCast(remaining & 0xF));
        remaining >>= 4;
    }
    try writer.writeAll(&buffer);
}

fn hexDigit(value: u8) u8 {
    const lut = "0123456789ABCDEF";
    return lut[value & 0xF];
}

fn canonicalizePath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return std.fs.cwd().realpathAlloc(allocator, path) catch |err| switch (err) {
        error.FileNotFound, error.AccessDenied, error.NotDir => blk: {
            if (std.fs.path.isAbsolute(path)) {
                break :blk try allocator.dupe(u8, path);
            }
            const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
            defer allocator.free(cwd);
            break :blk try std.fs.path.join(allocator, &.{ cwd, path });
        },
        else => return err,
    };
}

pub fn convertPathToUri(allocator: std.mem.Allocator, canonical_path: []const u8) ![]u8 {
    var owned_path = try allocator.dupe(u8, canonical_path);
    defer allocator.free(owned_path);

    var normalized = owned_path;
    var host_storage: ?[]u8 = null;
    defer if (host_storage) |host| allocator.free(host);

    if (builtin.os.tag == .windows) {
        for (normalized) |*byte| {
            if (byte.* == '\\') byte.* = '/';
        }

        if (std.mem.startsWith(u8, normalized, "//?/")) {
            normalized = normalized[4..];
        }

        var unc_slice: ?[]u8 = null;
        if (std.mem.startsWith(u8, normalized, "UNC/")) {
            normalized = normalized[4..];
            unc_slice = normalized;
        } else if (normalized.len >= 2 and normalized[0] == '/' and normalized[1] == '/') {
            normalized = normalized[2..];
            unc_slice = normalized;
        }

        if (unc_slice) |slice| {
            if (slice.len == 0) return error.BadPathName;
            const slash_index = std.mem.indexOfScalar(u8, slice, '/');
            const host_end = slash_index orelse slice.len;
            host_storage = try allocator.dupe(u8, slice[0..host_end]);
            normalized = if (slash_index) |idx| slice[idx..] else slice[slice.len..];
        }
    }

    if (normalized.len == 0 or normalized[0] != '/') {
        const expanded = try allocator.alloc(u8, normalized.len + 1);
        expanded[0] = '/';
        @memcpy(expanded[1..], normalized);

        allocator.free(owned_path);
        owned_path = expanded;
        normalized = owned_path;
    }

    var uri = std.Uri{
        .scheme = "file",
        .user = null,
        .password = null,
        .host = if (host_storage) |host| std.Uri.Component{ .raw = host } else .{ .raw = "" },
        .port = null,
        .path = .{ .raw = normalized },
        .query = null,
        .fragment = null,
    };

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try uri.format(&writer.writer);

    const rendered = writer.written();
    return try allocator.dupe(u8, rendered);
}

pub fn convertUriToPath(allocator: std.mem.Allocator, uri_text: []const u8) ![]u8 {
    const parsed = std.Uri.parse(uri_text) catch return error.InvalidUri;
    if (!std.mem.eql(u8, parsed.scheme, "file")) return error.UnsupportedUriScheme;

    var buffer = std.array_list.Managed(u8).init(allocator);
    errdefer buffer.deinit();
    var buf_writer = buffer.writer();

    if (parsed.host) |host_component| {
        try buf_writer.writeAll("\\\\");
        var host_buf: [std.Uri.host_name_max]u8 = undefined;
        const host_raw = host_component.toRaw(&host_buf) catch return error.UriHostTooLong;
        try buf_writer.writeAll(host_raw);
        try appendUriPath(allocator, &buf_writer, parsed.path, .unc);
    } else {
        try appendUriPath(allocator, &buf_writer, parsed.path, .local);
    }

    const path = try buffer.toOwnedSlice();
    if (builtin.os.tag == .windows) {
        for (path) |*byte| {
            if (byte.* == '/') byte.* = '\\';
        }
    }
    return path;
}

const UriPathMode = enum {
    local,
    unc,
};

fn appendUriPath(
    allocator: std.mem.Allocator,
    writer: anytype,
    component: std.Uri.Component,
    mode: UriPathMode,
) !void {
    var tmp = std.array_list.Managed(u8).init(allocator);
    defer tmp.deinit();

    var buffer: [1024]u8 = undefined;
    const component_str = component.toRaw(&buffer) catch blk: {
        break :blk switch (component) {
            .raw => |s| s,
            .percent_encoded => |s| s,
        };
    };
    try tmp.writer().writeAll(component_str);

    var raw = tmp.items;
    if (raw.len == 0) return;

    var start_index: usize = 0;
    switch (mode) {
        .unc => {
            if (raw[0] != '/') {
                try writer.writeByte('/');
            }
        },
        .local => {
            if (builtin.os.tag == .windows and raw.len >= 3 and raw[0] == '/' and raw[2] == ':' and std.ascii.isAlphabetic(raw[1])) {
                start_index = 1;
            }
        },
    }

    try writer.writeAll(raw[start_index..]);
}
