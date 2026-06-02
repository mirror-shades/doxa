const std = @import("std");

pub const SourceFile = struct {
    path: []const u8,
    content: []const u8,
    line_starts: []usize,
};

pub const SourceCache = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(SourceFile),

    pub fn init(allocator: std.mem.Allocator) SourceCache {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(SourceFile).init(allocator),
        };
    }

    pub fn deinit(self: *SourceCache) void {
        var it = self.files.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.path);
            self.allocator.free(entry.value_ptr.content);
            self.allocator.free(entry.value_ptr.line_starts);
            self.allocator.free(entry.key_ptr.*);
        }
        self.files.deinit();
    }

    pub fn load(self: *SourceCache, path: []const u8, content: []const u8) !void {
        if (self.files.contains(path)) return;

        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);

        const owned_content = try self.allocator.dupe(u8, content);
        errdefer self.allocator.free(owned_content);

        const line_starts = try computeLineStarts(self.allocator, owned_content);
        errdefer self.allocator.free(line_starts);

        const key = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(key);

        try self.files.put(key, .{
            .path = owned_path,
            .content = owned_content,
            .line_starts = line_starts,
        });
    }

    pub fn get(self: *const SourceCache, path: []const u8) ?*const SourceFile {
        return self.files.getPtr(path);
    }
};

fn computeLineStarts(allocator: std.mem.Allocator, content: []const u8) ![]usize {
    var list = std.array_list.Managed(usize).init(allocator);
    errdefer list.deinit();
    try list.append(0);
    for (content, 0..) |byte, i| {
        if (byte == '\n') try list.append(i + 1);
    }
    return try list.toOwnedSlice();
}
