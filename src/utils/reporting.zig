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
    debug_mode: bool = false,
    print_immediately: bool = true, // CLI printing control
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
    diagnostics: std.ArrayList(Diagnostic),
    options: ReporterOptions,
    writer: std.io.AnyWriter,
    allocator: std.mem.Allocator,
    debug_mode: bool,

    pub fn init(allocator: std.mem.Allocator, options: ReporterOptions) Reporter {
        const writer = std.io.getStdErr().writer().any();
        return .{
            .diagnostics = std.ArrayList(Diagnostic).init(allocator),
            .options = options,
            .writer = writer,
            .allocator = allocator,
            .debug_mode = options.debug_mode,
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

        var buf = std.ArrayList(u8).init(self.allocator);
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

            if (self.options.print_immediately) {
                self.writer.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), msg_copy }) catch {};
            }
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

            if (self.options.print_immediately) {
                self.writer.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), fallback_copy }) catch {};
            }
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

        if (self.options.print_immediately) {
            self.writer.print("DoxVM[{s}]: {s}\n", .{ @tagName(final_severity), msg_copy }) catch {};
        }
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

    pub fn debug(
        self: *Reporter,
        comptime fmt: []const u8,
        args: anytype,
        comptime src: std.builtin.SourceLocation,
    ) void {
        if (!self.debug_mode) return;
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
};
