const std = @import("std");
const testing = std.testing;

const hashing = @import("../src/utils/hashing.zig");

test "hashing: hashBytes is deterministic" {
    try testing.expect(hashing.equal(hashing.hashBytes("hello"), hashing.hashBytes("hello")));
}

test "hashing: different content yields different digests" {
    try testing.expect(!hashing.equal(hashing.hashBytes("hello"), hashing.hashBytes("world")));
}

test "hashing: known-answer vectors" {
    {
        const digest = hashing.hashBytes("");
        const hex = hashing.hexOf(digest);
        try testing.expectEqualStrings("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", &hex);
    }
    {
        const digest = hashing.hashBytes("abc");
        const hex = hashing.hexOf(digest);
        try testing.expectEqualStrings("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", &hex);
    }
}

test "hashing: hashFile is content-addressed, not path-addressed" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{ .sub_path = "a.txt", .data = "same bytes" });
    try tmp.dir.writeFile(.{ .sub_path = "b.txt", .data = "same bytes" });
    try tmp.dir.writeFile(.{ .sub_path = "c.txt", .data = "other bytes" });

    const path_a = try tmp.dir.realpathAlloc(testing.allocator, "a.txt");
    defer testing.allocator.free(path_a);
    const path_b = try tmp.dir.realpathAlloc(testing.allocator, "b.txt");
    defer testing.allocator.free(path_b);
    const path_c = try tmp.dir.realpathAlloc(testing.allocator, "c.txt");
    defer testing.allocator.free(path_c);

    const digest_a = try hashing.hashFile(testing.allocator, path_a);
    try testing.expect(hashing.equal(digest_a, try hashing.hashFile(testing.allocator, path_b)));
    try testing.expect(hashing.equal(digest_a, hashing.hashBytes("same bytes")));
    try testing.expect(!hashing.equal(digest_a, try hashing.hashFile(testing.allocator, path_c)));
}

test "hashing: key builder is order-sensitive" {
    var a = hashing.KeyBuilder.init("");
    a.addBytes("one");
    a.addBytes("two");

    var b = hashing.KeyBuilder.init("");
    b.addBytes("two");
    b.addBytes("one");

    try testing.expect(!hashing.equal(a.finish(), b.finish()));
}

test "hashing: length framing disambiguates concatenation" {
    var a = hashing.KeyBuilder.init("");
    a.addBytes("ab");
    a.addBytes("c");

    var b = hashing.KeyBuilder.init("");
    b.addBytes("a");
    b.addBytes("bc");

    try testing.expect(!hashing.equal(a.finish(), b.finish()));
}

test "hashing: seed changes the key" {
    var a = hashing.KeyBuilder.init("seed-v1");
    a.addBytes("body");

    var b = hashing.KeyBuilder.init("seed-v2");
    b.addBytes("body");

    try testing.expect(!hashing.equal(a.finish(), b.finish()));
}

test "hashing: addSource distinguishes same content under different names" {
    var a = hashing.KeyBuilder.init("");
    a.addSource(.{ .name = "main.doxa", .content = "x" });

    var b = hashing.KeyBuilder.init("");
    b.addSource(.{ .name = "lib.doxa", .content = "x" });

    try testing.expect(!hashing.equal(a.finish(), b.finish()));
}

test "hashing: addDigest includes the full digest" {
    const inner = hashing.hashBytes("inner");

    var with_digest = hashing.KeyBuilder.init("");
    with_digest.addDigest(inner);

    var with_bytes = hashing.KeyBuilder.init("");
    with_bytes.addBytes("inner");

    try testing.expect(!hashing.equal(with_digest.finish(), with_bytes.finish()));
}

test "hashing: compileKey is stable and order-sensitive" {
    var sources = [_]hashing.Source{
        .{ .name = "main.doxa", .content = "print(1)" },
        .{ .name = "std.doxa", .content = "..." },
    };
    const input = hashing.CompileInput{
        .seed = "v1",
        .options = "-O2",
        .sources = &sources,
    };

    const key = hashing.compileKey(input);
    try testing.expect(hashing.equal(key, hashing.compileKey(input)));

    std.mem.swap(hashing.Source, &sources[0], &sources[1]);
    try testing.expect(!hashing.equal(key, hashing.compileKey(input)));

    sources[1].content = "changed";
    try testing.expect(!hashing.equal(key, hashing.compileKey(input)));
}

test "hashing: shortHexOf is the first 16 hex chars" {
    const full = hashing.hexOf(hashing.hashBytes("abc"));
    const short = hashing.shortHexOf(hashing.hashBytes("abc"));
    try testing.expectEqualStrings(full[0..16], &short);
}
