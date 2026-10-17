//! Struct layout: allocation, descriptors, and the scalar word-copy paths.

const scope_arena = @import("scope_arena.zig");
const abi = @import("abi.zig");

const Scope = scope_arena.Scope;
const StructDesc = abi.StructDesc;
const StructHeader = abi.StructHeader;

pub fn headerOf(instance: *const anyopaque) *StructHeader {
    return @ptrFromInt(@intFromPtr(instance) - @sizeOf(StructHeader));
}

/// A zeroed struct of `words` field words in `scope`, behind its header.
pub fn structAllocIn(scope: *Scope, desc: *const StructDesc, words: usize) *anyopaque {
    const block = scope_arena.allocSlice(scope, i64, 2 + words);
    @memset(block, 0);
    const header: *StructHeader = @ptrCast(@alignCast(block.ptr));
    header.* = .{ .desc = desc, .scope = scope };
    return @ptrCast(block[2..].ptr);
}

/// A new described struct in `scope`, its fields zeroed.
pub export fn doxa_struct_new(scope: *Scope, desc: *const StructDesc) callconv(.c) *anyopaque {
    const field_count: usize = @intCast(desc.field_count);
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    return structAllocIn(scope, desc, structTotalWords(tags, field_count));
}

pub fn structDescOf(ptr: *const anyopaque) *const StructDesc {
    return headerOf(ptr).desc;
}

/// Word count of a single struct field given its runtime tag. String fields
/// (tag 3) occupy two `i64` words (ptr + len); everything else occupies one.
pub fn structFieldWordCount(tag: u64) usize {
    return if (tag == 3) 2 else 1;
}

/// Total number of `i64` words occupied by a struct's fields. When the tag
/// list is absent (defensive fallback), every field is treated as one word.
pub fn structTotalWords(tags: []const u64, field_count: usize) usize {
    var total: usize = field_count;
    var i: usize = 0;
    while (i < tags.len and i < field_count) : (i += 1) {
        if (tags[i] == 3) total += 1;
    }
    return total;
}

/// Scalar-struct typed clone: copy `word_count` i64 words into `scope`
/// verbatim. A struct with only scalar fields has no heap field to re-clone,
/// so it needs no descriptor, and carries no header.
pub export fn doxa_struct_clone_scalar(scope: *Scope, word_count: u64, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structCloneScalarInto(scope, @intCast(word_count), ptr);
}

pub fn structCloneScalarInto(scope: *Scope, word_count: usize, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const dst = scope_arena.allocSlice(scope, i64, word_count);
    const src_words: [*]const i64 = @ptrCast(@alignCast(src));
    @memcpy(dst, src_words[0..word_count]);
    return @ptrCast(dst.ptr);
}
