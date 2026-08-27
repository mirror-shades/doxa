const std = @import("std");

pub const Sha256 = std.crypto.hash.sha2.Sha256;
pub const Digest = [Sha256.digest_length]u8;
pub const DigestHex = [2 * Sha256.digest_length]u8;
pub const ShortHex = [16]u8;

/// A source file identified by name, paired with its content. `name` is the
/// identity; `content` is what the digest depends on.
pub const Source = struct {
    name: []const u8,
    content: []const u8,
};

/// Content hash of `bytes`. Content-addressed: equal inputs always yield equal
/// digests regardless of origin or caller.
pub fn hashBytes(bytes: []const u8) Digest {
    var digest: Digest = undefined;
    Sha256.hash(bytes, &digest, .{});
    return digest;
}

/// Content hash of the file at `path`. The path itself is not part of the
/// digest; key identity separately with `KeyBuilder.addSource`.
pub fn hashFile(allocator: std.mem.Allocator, path: []const u8) !Digest {
    const content = try readFile(allocator, path);
    defer allocator.free(content);
    return hashBytes(content);
}

/// Streaming composite key builder. Every field is framed with its byte length
/// (big-endian u64), so concatenated fields are unambiguous — `("ab", "c")`
/// can never collide with `("a", "bc")` — and ordering always matters.
pub const KeyBuilder = struct {
    hasher: Sha256,

    pub fn init(seed: []const u8) KeyBuilder {
        var self: KeyBuilder = .{ .hasher = Sha256.init(.{}) };
        self.addBytes(seed);
        return self;
    }

    pub fn addBytes(self: *KeyBuilder, bytes: []const u8) void {
        addField(&self.hasher, bytes);
    }

    pub fn addSource(self: *KeyBuilder, source: Source) void {
        addField(&self.hasher, source.name);
        addField(&self.hasher, source.content);
    }

    pub fn addDigest(self: *KeyBuilder, digest: Digest) void {
        self.hasher.update(&digest);
    }

    /// Reads `path` and adds it as `(path, content)`.
    pub fn addFile(self: *KeyBuilder, allocator: std.mem.Allocator, path: []const u8) !void {
        const content = try readFile(allocator, path);
        defer allocator.free(content);
        self.addSource(.{ .name = path, .content = content });
    }

    pub fn finish(self: *KeyBuilder) Digest {
        var digest: Digest = undefined;
        self.hasher.final(&digest);
        return digest;
    }
};

fn addField(hasher: *Sha256, bytes: []const u8) void {
    var len_buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_buf, @intCast(bytes.len), .big);
    hasher.update(&len_buf);
    hasher.update(bytes);
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

pub const CompileInput = struct {
    /// Bumps when the compiler or bundled runtime changes, invalidating every
    /// previously produced key.
    seed: []const u8,
    /// Canonical serialization of codegen-affecting options (target triple,
    /// opt level, ...). Order-sensitive.
    options: []const u8,
    /// Entry plus transitively imported sources and inline-Zig modules,
    /// ordered.
    sources: []const Source,
};

/// The full key for a compile unit: seed + options + every source file. Equal
/// keys mean the same program; unequal keys mean something changed.
pub fn compileKey(input: CompileInput) Digest {
    var kb = KeyBuilder.init(input.seed);
    kb.addBytes(input.options);
    for (input.sources) |source| kb.addSource(source);
    return kb.finish();
}

pub fn hexOf(digest: Digest) DigestHex {
    return std.fmt.bytesToHex(digest, .lower);
}

pub fn shortHexOf(digest: Digest) ShortHex {
    var out: ShortHex = undefined;
    @memcpy(&out, hexOf(digest)[0..out.len]);
    return out;
}

pub fn equal(a: Digest, b: Digest) bool {
    return std.mem.eql(u8, &a, &b);
}
