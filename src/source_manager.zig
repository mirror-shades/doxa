const std = @import("std");
const Allocator = std.mem.Allocator;
const Reporting = @import("reporting.zig").Reporting;
const ErrorList = @import("reporting.zig").ErrorList;

pub const SourceLocation = struct {
    file: []const u8,
    line: usize,
    column: usize,
};

pub const SourceMapping = struct {
    original_file: []const u8,
    original_line: usize,
    unified_line: usize,
};

pub const SourceManager = struct {
    allocator: Allocator,
    unified_source: []const u8,
    mappings: std.ArrayList(SourceMapping),
    debug_enabled: bool,
    processed_files: std.StringHashMap(bool),
    current_line: usize,
    reporter: *Reporting,

    pub fn init(allocator: Allocator, debug_enabled: bool, reporter: *Reporting) SourceManager {
        return .{
            .allocator = allocator,
            .unified_source = &[_]u8{},
            .mappings = std.ArrayList(SourceMapping).init(allocator),
            .processed_files = std.StringHashMap(bool).init(allocator),
            .debug_enabled = debug_enabled,
            .current_line = 0,
            .reporter = reporter,
        };
    }

    pub fn deinit(self: *SourceManager) void {
        self.allocator.free(self.unified_source);
        self.mappings.deinit();
    }

    pub fn processFile(self: *SourceManager, file_path: []const u8) ErrorList!void {
        if (self.processed_files.get(file_path)) |_| {
            return;
        }

        try self.processed_files.put(file_path, true);

        var file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(content);

        try self.processImports(content);
        try self.appendSource(content, file_path);
    }

    fn processImports(self: *SourceManager, content: []const u8) ErrorList!void {
        var lines = std.mem.split(u8, content, "\n");
        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, "import")) |_| {
                if (std.mem.indexOf(u8, line, "\"")) |start| {
                    if (std.mem.indexOfPos(u8, line, start + 1, "\"")) |end| {
                        const import_path = line[start + 1 .. end];
                        try self.processFile(import_path);
                    }
                }
            }
        }
    }

    fn appendSource(self: *SourceManager, content: []const u8, file_path: []const u8) ErrorList!void {
        var lines = std.mem.split(u8, content, "\n");
        var line_number: usize = 0;

        var new_content = std.ArrayList(u8).init(self.allocator);
        defer new_content.deinit();

        while (lines.next()) |line| {
            if (std.mem.indexOf(u8, line, "import")) |_| {
                line_number += 1;
                continue;
            }

            try self.mappings.append(.{
                .original_file = file_path,
                .original_line = line_number,
                .unified_line = self.current_line,
            });

            try new_content.appendSlice(line);
            try new_content.append('\n');

            line_number += 1;
            self.current_line += 1;
        }

        const old_source = self.unified_source;
        const new_unified = try self.allocator.alloc(u8, old_source.len + new_content.items.len);
        @memcpy(new_unified[0..old_source.len], old_source);
        @memcpy(new_unified[old_source.len..], new_content.items);

        if (old_source.len > 0) {
            self.allocator.free(old_source);
        }
        self.unified_source = new_unified;
    }

    pub fn getOriginalLocation(self: *SourceManager, unified_line: usize) ?SourceLocation {
        for (self.mappings.items) |mapping| {
            if (mapping.unified_line == unified_line) {
                return SourceLocation{
                    .file = mapping.original_file,
                    .line = mapping.original_line,
                    .column = 0,
                };
            }
        }
        return null;
    }
};
