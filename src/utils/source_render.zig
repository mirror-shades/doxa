const std = @import("std");
const reporting = @import("reporting.zig");
const source_cache = @import("source_cache.zig");

const Diagnostic = reporting.Diagnostic;
const Location = reporting.Location;
const Severity = reporting.Severity;
const SourceCache = source_cache.SourceCache;
const SourceFile = source_cache.SourceFile;

pub const DiagnosticRenderer = struct {
    source_cache: *const SourceCache,
    allocator: std.mem.Allocator,
    use_ansi: bool,
    context_lines: usize = 1,
    max_line_width: usize = 120,
    tab_width: usize = 4,

    pub fn render(
        self: *const DiagnosticRenderer,
        writer: anytype,
        diag: *const Diagnostic,
    ) !void {
        const loc = diag.loc orelse {
            try self.renderHeaderLine(writer, diag, null);
            return;
        };

        try self.renderPrimary(writer, diag, loc);

        if (diag.related_info) |related| {
            for (related) |ri| {
                try self.renderRelated(writer, diag, ri);
            }
        }
    }

    fn renderPrimary(
        self: *const DiagnosticRenderer,
        writer: anytype,
        diag: *const Diagnostic,
        loc: Location,
    ) !void {
        try self.renderHeaderLine(writer, diag, loc);

        const source_file = self.source_cache.get(loc.file) orelse return;
        const severity = diag.severity;
        try self.renderSnippet(writer, source_file, loc, severity, diag.message, @tagName(severity));
    }

    fn renderRelated(
        self: *const DiagnosticRenderer,
        writer: anytype,
        diag: *const Diagnostic,
        related: reporting.RelatedInformation,
    ) !void {
        const source_file = self.source_cache.get(related.location.file) orelse {
            try self.renderHeaderLine(writer, diag, related.location);
            return;
        };
        try self.renderSnippet(
            writer,
            source_file,
            related.location,
            diag.severity,
            related.message,
            "note",
        );
    }

    fn renderHeaderLine(
        self: *const DiagnosticRenderer,
        writer: anytype,
        diag: *const Diagnostic,
        loc: ?Location,
    ) !void {
        if (self.use_ansi) {
            try writer.writeAll(ansiReset());
        }

        try writer.writeAll("DoxVM: ");

        if (diag.code) |code| {
            if (loc) |l| {
                try writer.print("[{s}][{s}][{s}] {s}:{d}:{d}: {s}\n", .{
                    @tagName(diag.phase), @tagName(diag.severity),
                    code, l.file, l.range.start_line, l.range.start_col, diag.message,
                });
            } else {
                try writer.print("[{s}][{s}][{s}] {s}\n", .{
                    @tagName(diag.phase), @tagName(diag.severity), code, diag.message,
                });
            }
        } else {
            if (loc) |l| {
                try writer.print("[{s}][{s}] {s}:{d}:{d}: {s}\n", .{
                    @tagName(diag.phase), @tagName(diag.severity),
                    l.file, l.range.start_line, l.range.start_col, diag.message,
                });
            } else {
                try writer.print("[{s}][{s}] {s}\n", .{
                    @tagName(diag.phase), @tagName(diag.severity), diag.message,
                });
            }
        }

        if (self.use_ansi) {
            try writer.writeAll(ansiReset());
        }
    }

    fn renderSnippet(
        self: *const DiagnosticRenderer,
        writer: anytype,
        source_file: *const SourceFile,
        loc: Location,
        severity: Severity,
        message: []const u8,
        label: []const u8,
    ) !void {
        const r = loc.range;

        if (r.start_line == 0 or r.start_line > source_file.line_starts.len) return;

        const line_idx = r.start_line - 1;
        const ctx_start = if (line_idx > self.context_lines) line_idx - self.context_lines else 0;
        const ctx_end = @min(line_idx + self.context_lines + 1, source_file.line_starts.len);

        if (ctx_end <= ctx_start) return;

        const primary_col_start_byte: usize = if (r.start_col > 0) r.start_col - 1 else 0;
        const primary_col_end_byte = blk: {
            if (r.end_line == r.start_line and r.end_col > 0) {
                break :blk r.end_col - 1;
            }
            const last_line = self.getLine(source_file, line_idx);
            break :blk last_line.len;
        };

        const color = ansiSeverityColor(severity);
        const reset = ansiReset();
        const dim_col = if (self.use_ansi) "\x1b[2m" else "";
        const dim_reset = if (self.use_ansi) "\x1b[0m" else "";

        for (ctx_start..ctx_end) |li| {
            const line = self.getLine(source_file, li);

            // Line number, right-aligned in 4 chars
            try writer.writeAll(dim_col);
            try writer.print("{d: >4} | ", .{li + 1});
            try writer.writeAll(dim_reset);

            // Write the source line with tab expansion
            try self.writeLineExpanded(writer, line);
            try writer.writeAll("\n");

            // Underline span on the primary error line
            if (li == line_idx) {
                try writer.writeAll(dim_col);
                try writer.writeAll("     | ");
                try writer.writeAll(dim_reset);

                const line_for_col = line;
                const display_col_start = byteColToDisplayCol(line_for_col, self.tab_width, @min(primary_col_start_byte, line_for_col.len));
                const display_col_end = byteColToDisplayCol(line_for_col, self.tab_width, @min(primary_col_end_byte, line_for_col.len));

                // Spaces before underline
                var d: usize = 0;
                while (d < display_col_start) : (d += 1) {
                    try writer.writeAll(" ");
                }

                const span_width: usize = if (display_col_end > display_col_start)
                    display_col_end - display_col_start
                else
                    1;

                if (self.use_ansi) {
                    try writer.writeAll(color);
                }

                // Tildes for span width, caret at the end
                var s: usize = 0;
                while (s < span_width - 1) : (s += 1) {
                    try writer.writeAll("~");
                }
                try writer.writeAll("^");

                if (self.use_ansi) {
                    try writer.writeAll(reset);
                }

                try writer.print(" {s}: {s}\n", .{ label, message });
            }
        }
    }

    fn getLine(self: *const DiagnosticRenderer, source_file: *const SourceFile, line_idx: usize) []const u8 {
        _ = self;
        const start = source_file.line_starts[line_idx];
        const end = if (line_idx + 1 < source_file.line_starts.len)
            source_file.line_starts[line_idx + 1] - 1
        else
            source_file.content.len;

        if (end > start and source_file.content[end - 1] == '\r') {
            return source_file.content[start .. end - 1];
        }
        return source_file.content[start..end];
    }

    fn writeLineExpanded(
        self: *const DiagnosticRenderer,
        writer: anytype,
        line: []const u8,
    ) !void {
        var col: usize = 0;
        for (line) |byte| {
            if (byte == '\t') {
                const spaces = self.tab_width - (col % self.tab_width);
                var s: usize = 0;
                while (s < spaces) : (s += 1) {
                    try writer.writeAll(" ");
                }
                col += spaces;
            } else {
                try writer.writeByte(byte);
                col += 1;
            }
        }
    }
};

fn byteColToDisplayCol(line: []const u8, tab_width: usize, byte_col: usize) usize {
    var display_col: usize = 0;
    var i: usize = 0;
    const limit = @min(byte_col, line.len);
    while (i < limit) : (i += 1) {
        if (line[i] == '\t') {
            display_col += tab_width - (display_col % tab_width);
        } else {
            display_col += 1;
        }
    }
    return display_col;
}

fn ansiSeverityColor(severity: Severity) []const u8 {
    return switch (severity) {
        .Error => "\x1b[31;1m",
        .Warning => "\x1b[33;1m",
        .Info, .Hint => "\x1b[36m",
        .Internal => "\x1b[35;1m",
    };
}

fn ansiReset() []const u8 {
    return "\x1b[0m";
}
