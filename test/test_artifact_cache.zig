const std = @import("std");
const testing = std.testing;

const hashing = @import("../src/utils/hashing.zig");
const ArtifactCache = @import("../src/utils/artifact_cache.zig").ArtifactCache;

fn testCache(tmp: *std.testing.TmpDir) !ArtifactCache {
    return ArtifactCache.initInDir(testing.io, testing.allocator, tmp.dir, ".");
}

test "artifact cache: miss, store, hit, content round-trips" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var cache = try testCache(&tmp);
    defer cache.deinit();

    const key = hashing.hashBytes("payload-one");
    try testing.expect(!cache.contains(key, "o"));

    const name = try cache.nameAlloc(key, "o");
    defer testing.allocator.free(name);
    const staging = try cache.stagingNameAlloc(key, "o");
    defer testing.allocator.free(staging);

    const published = try cache.writeBytes(key, "o", "artifact-bytes");
    defer testing.allocator.free(published);

    try testing.expect(cache.contains(key, "o"));
    // Publishing renames the staging file away.
    try testing.expectError(error.FileNotFound, tmp.dir.statFile(testing.io, staging, .{}));

    const got = try tmp.dir.readFileAlloc(testing.io, name, testing.allocator, .limited(64));
    defer testing.allocator.free(got);
    try testing.expectEqualStrings("artifact-bytes", got);
}

test "artifact cache: an existing artifact wins over a second store" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var cache = try testCache(&tmp);
    defer cache.deinit();

    const key = hashing.hashBytes("payload-two");
    const name = try cache.nameAlloc(key, "obj");
    defer testing.allocator.free(name);

    const first = try cache.writeBytes(key, "obj", "first");
    defer testing.allocator.free(first);
    const second = try cache.writeBytes(key, "obj", "second");
    defer testing.allocator.free(second);

    try testing.expectEqualStrings(first, second);
    const got = try tmp.dir.readFileAlloc(testing.io, name, testing.allocator, .limited(64));
    defer testing.allocator.free(got);
    try testing.expectEqualStrings("first", got);
}

test "artifact cache: distinct keys and extensions are distinct artifacts" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var cache = try testCache(&tmp);
    defer cache.deinit();

    const a = hashing.hashBytes("key-a");
    const b = hashing.hashBytes("key-b");
    const path_a = try cache.writeBytes(a, "o", "A");
    defer testing.allocator.free(path_a);
    const path_b = try cache.writeBytes(b, "o", "B");
    defer testing.allocator.free(path_b);
    const path_a_asm = try cache.writeBytes(a, "a", "A-asm");
    defer testing.allocator.free(path_a_asm);

    try testing.expect(!std.mem.eql(u8, path_a, path_b));
    try testing.expect(!std.mem.eql(u8, path_a, path_a_asm));
    try testing.expect(cache.contains(a, "o"));
    try testing.expect(cache.contains(a, "a"));
    try testing.expect(cache.contains(b, "o"));
    try testing.expect(!cache.contains(a, "s"));
}
