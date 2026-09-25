const std = @import("std");
const hashing = @import("hashing.zig");

/// Content-addressed store for compiled artifacts (object files, archives).
///
/// An artifact is named by the full lowercase hex of its key digest plus an
/// extension (`<dir>/<64 hex>.o`), so the name *is* the key. Producers write to
/// a sibling `<name>.tmp` and call `publish`, which renames it into place; a
/// partial or crashed write can therefore never be observed under the published
/// name. `contains` treats any non-empty file at the published name as valid,
/// which is sound because only atomic renames publish it.
pub const ArtifactCache = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    dir: std.Io.Dir,
    /// Path of `dir` relative to the cwd, used to hand externally-visible
    /// paths to the toolchain (which runs with cwd-relative arguments).
    dir_path: []const u8,
    owns_dir: bool,

    /// Create (if missing) and bind to `<dir_path>` relative to the cwd.
    pub fn init(io: std.Io, allocator: std.mem.Allocator, dir_path: []const u8) !ArtifactCache {
        const cwd = std.Io.Dir.cwd();
        try cwd.createDirPath(io, dir_path);
        const dir = try cwd.openDir(io, dir_path, .{});
        errdefer dir.close(io);
        return .{
            .io = io,
            .allocator = allocator,
            .dir = dir,
            .dir_path = try allocator.dupe(u8, dir_path),
            .owns_dir = true,
        };
    }

    /// Bind to an already-open directory without creating it. The directory
    /// handle must outlive the cache.
    pub fn initInDir(io: std.Io, allocator: std.mem.Allocator, dir: std.Io.Dir, dir_path: []const u8) !ArtifactCache {
        return .{
            .io = io,
            .allocator = allocator,
            .dir = dir,
            .dir_path = try allocator.dupe(u8, dir_path),
            .owns_dir = false,
        };
    }

    pub fn deinit(self: *ArtifactCache) void {
        if (self.owns_dir) self.dir.close(self.io);
        self.allocator.free(self.dir_path);
    }

    /// The published basename for `key`, `<64 hex>.<ext>`.
    pub fn nameAlloc(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) ![]u8 {
        const hex = hashing.hexOf(key);
        return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ hex[0..], ext });
    }

    /// The cwd-relative path of the published artifact.
    pub fn pathAlloc(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) ![]u8 {
        const name = try self.nameAlloc(key, ext);
        defer self.allocator.free(name);
        return std.fs.path.join(self.allocator, &.{ self.dir_path, name });
    }

    /// The basename a producer writes before publishing.
    pub fn stagingNameAlloc(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) ![]u8 {
        const name = try self.nameAlloc(key, ext);
        defer self.allocator.free(name);
        return std.fmt.allocPrint(self.allocator, "{s}.tmp", .{name});
    }

    /// The cwd-relative path a producer writes before publishing.
    pub fn stagingPathAlloc(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) ![]u8 {
        const name = try self.stagingNameAlloc(key, ext);
        defer self.allocator.free(name);
        return std.fs.path.join(self.allocator, &.{ self.dir_path, name });
    }

    /// True when a complete artifact for `key` is present.
    pub fn contains(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) bool {
        const name = self.nameAlloc(key, ext) catch return false;
        defer self.allocator.free(name);
        const stat = self.dir.statFile(self.io, name, .{}) catch return false;
        return stat.kind == .file and stat.size > 0;
    }

    /// Atomically publish the staged file for `key` and return its cwd-relative
    /// path. If a complete artifact already exists, it wins and the staging
    /// file is discarded.
    pub fn publish(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8) ![]u8 {
        const name = try self.nameAlloc(key, ext);
        defer self.allocator.free(name);
        const staging_name = try self.stagingNameAlloc(key, ext);
        defer self.allocator.free(staging_name);

        if (self.contains(key, ext)) {
            self.dir.deleteFile(self.io, staging_name) catch {};
            return self.pathAlloc(key, ext);
        }
        try self.dir.rename(staging_name, self.dir, name, self.io);
        return self.pathAlloc(key, ext);
    }

    /// Store `bytes` under `key` and return the cwd-relative path.
    pub fn writeBytes(self: *const ArtifactCache, key: hashing.Digest, ext: []const u8, bytes: []const u8) ![]u8 {
        const staging_name = try self.stagingNameAlloc(key, ext);
        defer self.allocator.free(staging_name);
        try self.dir.writeFile(self.io, .{ .sub_path = staging_name, .data = bytes });
        return self.publish(key, ext);
    }
};
