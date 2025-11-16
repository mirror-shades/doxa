const std = @import("std");
const builtin = @import("builtin");

const Errors = @import("errors.zig");
const ErrorList = Errors.ErrorList;

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
    log_to_file: bool = true,
    log_file_path: []const u8 = "last_diagnostics.log",
    max_log_bytes: usize = 2 * 1024 * 1024,
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
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    debug_verbose: bool,
    debug_lexer: bool,
    debug_parser: bool,
    debug_semantic: bool,
    debug_hir: bool,
    debug_bytecode: bool,
    debug_execution: bool,
    stderr_buffer: [1024]u8,
    stderr_writer: std.fs.File.Writer,
    file_uri_cache: std.StringHashMap([]const u8),

    pub fn init(allocator: std.mem.Allocator, options: ReporterOptions) Reporter {
        var stderr_buffer: [1024]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
        return .{
            .diagnostics = std.array_list.Managed(Diagnostic).init(allocator),
            .options = options,
            .writer = &stderr_writer.interface,
            .allocator = allocator,
            .debug_verbose = options.debug_verbose,
            .debug_lexer = options.debug_lexer,
            .debug_parser = options.debug_parser,
            .debug_semantic = options.debug_semantic,
            .debug_hir = options.debug_hir,
            .debug_bytecode = options.debug_bytecode,
            .debug_execution = options.debug_execution,
            .stderr_buffer = stderr_buffer,
            .stderr_writer = stderr_writer,
            .file_uri_cache = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Reporter) void {
        self.clear();
        self.diagnostics.deinit();
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

        var buf = std.array_list.Managed(u8).init(self.allocator);
        defer buf.deinit();

        std.fmt.format(buf.writer(), fmt, args) catch |err| {
            const fallback_msg = switch (err) {
                else => "message formatting failed",
            };
            const msg_copy = self.allocator.dupe(u8, fallback_msg) catch {
                return;
            };

            const diag = Diagnostic{
                .phase = phase,
                .severity = final_severity,
                .message = msg_copy,
                .loc = loc,
                .code = code,
                .related_info = null,
                .source = "DoxVM",
            };

            self.appendDiagnostic(diag);
            std.debug.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), msg_copy });
            return;
        };

        const msg_copy = buf.toOwnedSlice() catch {
            const fallback_msg = "out of memory: diagnostic message lost";
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
        const treat_as_uri = std.mem.startsWith(u8, file_or_uri, "file://");

        var write_index: usize = 0;
        for (self.diagnostics.items) |diag| {
            var should_remove = false;
            if (diag.loc) |location| {
                should_remove = self.locationMatchesNeedle(location, file_or_uri, treat_as_uri);
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

        const treat_as_uri = std.mem.startsWith(u8, file_or_uri, "file://");
        for (self.diagnostics.items, 0..) |diag, idx| {
            if (diag.loc) |location| {
                if (self.locationMatchesNeedle(location, file_or_uri, treat_as_uri)) {
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

    pub fn toLspDiagnostics(self: *Reporter, allocator: std.mem.Allocator) ![]u8 {
        // TODO: serialize diagnostics to LSP JSON
        _ = self;
        _ = allocator;
        return "[]";
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
        std.debug.print("DoxVM: {s}\n", .{line});

        if (self.options.log_to_file) {
            self.writeToLog(line) catch {};
        }
    }

    fn locationMatchesNeedle(self: *Reporter, loc: Location, needle: []const u8, treat_as_uri: bool) bool {
        _ = self;
        if (treat_as_uri) {
            if (loc.file_uri) |uri| {
                return std.mem.eql(u8, uri, needle);
            }
            return false;
        }

        if (std.mem.eql(u8, loc.file, needle)) return true;
        if (loc.file_uri) |uri| {
            return std.mem.eql(u8, uri, needle);
        }
        return false;
    }

    fn writeToLog(self: *Reporter, line: []const u8) !void {
        const opts = self.options;
        const cwd = std.fs.cwd();
        var file = cwd.createFile(opts.log_file_path, .{ .read = true, .truncate = false, .exclusive = false }) catch |e| switch (e) {
            error.PathAlreadyExists => try cwd.openFile(opts.log_file_path, .{ .mode = .read_write }),
            else => return e,
        };
        defer file.close();

        const current_size = file.getEndPos() catch 0;

        if (current_size >= opts.max_log_bytes) {
            try file.setEndPos(0);
        }

        try file.seekFromEnd(0);
        try file.writeAll(line);
        try file.writeAll("\n");
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

fn convertPathToUri(allocator: std.mem.Allocator, canonical_path: []const u8) ![]u8 {
    var path_buf = try allocator.dupe(u8, canonical_path);
    errdefer allocator.free(path_buf);

    if (builtin.os.tag == .windows) {
        for (path_buf) |*c| {
            if (c.* == '\\') c.* = '/';
        }
        if (path_buf.len == 0 or path_buf[0] != '/') {
            const with_slash = try allocator.alloc(u8, path_buf.len + 1);
            with_slash[0] = '/';
            @memcpy(with_slash[1..], path_buf);
            allocator.free(path_buf);
            path_buf = with_slash;
        }
    } else if (path_buf.len == 0 or path_buf[0] != '/') {
        const with_slash = try allocator.alloc(u8, path_buf.len + 1);
        with_slash[0] = '/';
        @memcpy(with_slash[1..], path_buf);
        allocator.free(path_buf);
        path_buf = with_slash;
    }

    var uri = std.Uri{
        .scheme = "file",
        .user = null,
        .password = null,
        .host = .{ .raw = "" },
        .port = null,
        .path = .{ .raw = path_buf },
        .query = null,
        .fragment = null,
    };

    var buffer = std.array_list.Managed(u8).init(allocator);
    defer buffer.deinit();

    var buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;
    try uri.format(stdout);

    allocator.free(path_buf);
    return buffer.toOwnedSlice();
}
