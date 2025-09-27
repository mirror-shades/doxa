const std = @import("std");

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
    max_log_bytes: usize = 2 * 1024 * 1024, // 2MB cap, truncates when exceeded
};

pub const Range = struct {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
};

pub const Location = struct {
    file: []const u8,
    file_uri: ?[]const u8 = null, // TODO: LSP URIs
    range: Range,
};

pub const Diagnostic = struct {
    phase: DiagnosticPhase,
    severity: Severity,
    message: []const u8, // heap-allocated
    loc: ?Location,
    code: ?[]const u8 = null,
    related_info: ?[]const u8 = null, // TODO: related diagnostics
    source: ?[]const u8 = "DoxVM",
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
        };
    }

    pub fn deinit(self: *Reporter) void {
        for (self.diagnostics.items) |diag| {
            self.allocator.free(diag.message);
        }
        self.diagnostics.deinit();
    }

    ////////
    /// CORE REPORT FUNCTION
    ////////

    pub fn report(
        self: *Reporter,
        phase: DiagnosticPhase,
        severity: Severity,
        loc: ?Location,
        code: ?[]const u8,
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

        // Handle formatting errors properly
        std.fmt.format(buf.writer(), fmt, args) catch |err| {
            // Log the formatting error and provide a fallback message
            const fallback_msg = switch (err) {
                else => "message formatting failed",
            };
            const msg_copy = self.allocator.dupe(u8, fallback_msg) catch {
                // If even the fallback allocation fails, we have to skip the diagnostic
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

            self.diagnostics.append(diag) catch {
                self.allocator.free(msg_copy);
                return;
            };

            std.debug.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), msg_copy });

            return;
        };

        const msg_copy = buf.toOwnedSlice() catch {
            // Provide a fallback message for out-of-memory situations
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

            self.diagnostics.append(diag) catch {
                self.allocator.free(fallback_copy);
                return;
            };

            std.debug.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), fallback_copy });
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

        self.diagnostics.append(diag) catch {
            self.allocator.free(msg_copy);
            return;
        };

        // Build a rich, single-line diagnostic with location and code
        var line_buf = std.array_list.Managed(u8).init(self.allocator);
        defer line_buf.deinit();

        if (loc) |l| {
            const file = l.file;
            const sl = l.range.start_line;
            const sc = l.range.start_col;
            if (code) |c| {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}][{s}] {s}:{d}:{d}: {s}", .{
                    @tagName(phase),
                    @tagName(final_severity),
                    c,
                    file,
                    sl,
                    sc,
                    msg_copy,
                }) catch {};
            } else {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}] {s}:{d}:{d}: {s}", .{
                    @tagName(phase),
                    @tagName(final_severity),
                    file,
                    sl,
                    sc,
                    msg_copy,
                }) catch {};
            }
        } else {
            if (code) |c| {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}][{s}] {s}", .{ @tagName(phase), @tagName(final_severity), c, msg_copy }) catch {};
            } else {
                _ = std.fmt.format(line_buf.writer(), "[{s}][{s}] {s}", .{ @tagName(phase), @tagName(final_severity), msg_copy }) catch {};
            }
        }

        const line = line_buf.items;

        std.debug.print("DoxVM: {s}\n", .{line});

        if (self.options.log_to_file) {
            self.writeToLog(line) catch {};
        }
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

    ////////
    /// PRINTING FUNCTIONS
    ////////

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
        // Use the main reporting system for consistency
        const loc = Location{
            .file = src.file,
            .range = .{
                .start_line = src.line,
                .start_col = 0,
                .end_line = src.line,
                .end_col = 0,
            },
        };

        self.report(.Debug, .Hint, loc, null, fmt, args);
    }

    ////////
    /// HELPERS
    ////////

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

    ////////
    /// BATCHING (TODO:for eventual LSP support)
    ///////

    pub fn reportBatch(self: *Reporter, diags: []const Diagnostic) void {
        _ = self;
        _ = diags;
    }

    pub fn filterBySeverity(self: *Reporter, severity: Severity) []const Diagnostic {
        // TODO: return filtered view
        _ = self;
        _ = severity;
        return &[_]Diagnostic{};
    }

    pub fn filterByFile(self: *Reporter, file: []const u8) []const Diagnostic {
        // TODO: return filtered view
        _ = self;
        _ = file;
        return &[_]Diagnostic{};
    }

    pub fn toLspDiagnostics(self: *Reporter, allocator: std.mem.Allocator) ![]u8 {
        // TODO: serialize diagnostics to LSP JSON
        _ = self;
        _ = allocator;
        return "[]";
    }

    // Append a single line to the log file; truncate when exceeding max_log_bytes
    fn writeToLog(self: *Reporter, line: []const u8) !void {
        const opts = self.options;
        const cwd = std.fs.cwd();
        // Try open existing to check size
        var file = cwd.createFile(opts.log_file_path, .{ .read = true, .truncate = false, .exclusive = false }) catch |e| switch (e) {
            error.PathAlreadyExists => try cwd.openFile(opts.log_file_path, .{ .mode = .read_write }),
            else => return e,
        };
        defer file.close();

        // Determine current size; default to 0 on error
        const current_size = file.getEndPos() catch 0;

        // Truncate if too large
        if (current_size >= opts.max_log_bytes) {
            try file.setEndPos(0);
        }

        // Seek to end and append
        try file.seekFromEnd(0);
        try file.writeAll(line);
        try file.writeAll("\n");
    }
};
