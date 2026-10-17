//! Strings: the `DoxaString` helpers and the exported string operations.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const abi = @import("abi.zig");

const Scope = scope_arena.Scope;
const DoxaString = abi.DoxaString;

pub fn allocDoxaString(scope: *Scope, bytes: []const u8) DoxaString {
    if (bytes.len == 0) return .{ .ptr = null, .len = 0 };
    const buf = scope_arena.allocSlice(scope, u8, bytes.len);
    @memcpy(buf, bytes);
    return .{ .ptr = buf.ptr, .len = bytes.len };
}

pub fn sliceFromDoxaString(s: DoxaString) []const u8 {
    if (s.ptr) |p| return p[0..@intCast(s.len)];
    return "";
}

pub export fn doxa_str_eq(a_ptr: ?[*]const u8, a_len: u64, b_ptr: ?[*]const u8, b_len: u64) callconv(.c) bool {
    const a = sliceFromDoxaString(.{ .ptr = a_ptr, .len = a_len });
    const b = sliceFromDoxaString(.{ .ptr = b_ptr, .len = b_len });
    return std.mem.eql(u8, a, b);
}

/// The order of two strings: -1, 0 or 1 as `a` sorts before, with or after
/// `b`. Byte-wise lexicographic, which for UTF-8 is Unicode scalar order.
pub export fn doxa_str_cmp(a_ptr: ?[*]const u8, a_len: u64, b_ptr: ?[*]const u8, b_len: u64) callconv(.c) i32 {
    const a = sliceFromDoxaString(.{ .ptr = a_ptr, .len = a_len });
    const b = sliceFromDoxaString(.{ .ptr = b_ptr, .len = b_len });
    return switch (std.mem.order(u8, a, b)) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

pub export fn doxa_str_concat(scope: *Scope, a_ptr: ?[*]const u8, a_len: u64, b_ptr: ?[*]const u8, b_len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const as = sliceFromDoxaString(.{ .ptr = a_ptr, .len = a_len });
    const bs = sliceFromDoxaString(.{ .ptr = b_ptr, .len = b_len });
    const total_len = as.len + bs.len;
    if (total_len == 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const buf = scope_arena.allocSlice(scope, u8, total_len);
    @memcpy(buf[0..as.len], as);
    @memcpy(buf[as.len..total_len], bs);
    out_ptr.* = buf.ptr;
    out_len.* = total_len;
}

/// Clone a string into `scope`.
pub fn strCloneInto(scope: *Scope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) void {
    const bytes: []const u8 = if (ptr) |p| p[0..@intCast(len)] else "";
    // Allocate at least one byte so an empty string keeps a non-null pointer;
    // string indexing assumes the backing pointer is valid even when len == 0.
    const alloc_len: usize = if (bytes.len == 0) 1 else bytes.len;
    const buf = scope_arena.alloc(scope, alloc_len, .fromByteUnits(@alignOf(u8)), @returnAddress());
    @memset(buf[0..alloc_len], 0);
    @memcpy(buf[0..bytes.len], bytes);
    out_ptr.* = buf;
    out_len.* = bytes.len;
}

/// Clone a string into `scope`: `clone` of a string. Also the inline-Zig
/// string-return boundary: a wrapper hands its result across the ABI already
/// owned by the caller's arena (its hidden first argument).
pub export fn doxa_str_clone(scope: *Scope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    strCloneInto(scope, ptr, len, out_ptr, out_len);
}

pub export fn doxa_char_to_string(scope: *Scope, ch: u8, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const buf = scope_arena.allocSlice(scope, u8, 1);
    buf[0] = ch;
    out_ptr.* = buf.ptr;
    out_len.* = 1;
}

pub export fn doxa_find_str(h_ptr: ?[*]const u8, h_len: u64, n_ptr: ?[*]const u8, n_len: u64) callconv(.c) i64 {
    const str = sliceFromDoxaString(.{ .ptr = h_ptr, .len = h_len });
    const ndl = sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    if (ndl.len == 0) return 0;
    return if (std.mem.indexOf(u8, str, ndl)) |found| @intCast(found) else -1;
}

pub export fn doxa_substring(scope: *Scope, ptr: ?[*]const u8, len: u64, start: i64, length: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const src = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    if (start < 0 or length < 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const start_u: usize = @intCast(@as(u64, @intCast(start)));
    const len_u: usize = @intCast(@as(u64, @intCast(length)));
    if (start_u >= src.len) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const end_unclamped: usize = start_u +| len_u;
    const end_u: usize = if (end_unclamped > src.len) src.len else end_unclamped;
    const ds = allocDoxaString(scope, src[start_u..end_u]);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_str_insert(scope: *Scope, s_ptr: ?[*]const u8, s_len: u64, idx: i64, ins_ptr: ?[*]const u8, ins_len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const src = sliceFromDoxaString(.{ .ptr = s_ptr, .len = s_len });
    const add = sliceFromDoxaString(.{ .ptr = ins_ptr, .len = ins_len });
    const idx_clamped: usize = blk: {
        if (idx <= 0) break :blk 0;
        const iu: usize = @intCast(@as(u64, @intCast(idx)));
        break :blk if (iu > src.len) src.len else iu;
    };
    const total_len = src.len + add.len;
    if (total_len == 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const out = scope_arena.allocSlice(scope, u8, total_len);
    @memcpy(out[0..idx_clamped], src[0..idx_clamped]);
    @memcpy(out[idx_clamped .. idx_clamped + add.len], add);
    @memcpy(out[idx_clamped + add.len .. total_len], src[idx_clamped..]);
    out_ptr.* = out.ptr;
    out_len.* = total_len;
}

pub export fn doxa_str_remove(
    scope: *Scope,
    s_ptr: ?[*]const u8,
    s_len: u64,
    idx: i64,
    out_remaining: *DoxaString,
    out_removed: *DoxaString,
) callconv(.c) u8 {
    const src = sliceFromDoxaString(.{ .ptr = s_ptr, .len = s_len });
    if (src.len == 0 or idx < 0) {
        out_remaining.* = allocDoxaString(scope, src);
        out_removed.* = .{ .ptr = null, .len = 0 };
        return 0;
    }
    const iu: usize = @intCast(@as(u64, @intCast(idx)));
    if (iu >= src.len) {
        out_remaining.* = allocDoxaString(scope, src);
        out_removed.* = .{ .ptr = null, .len = 0 };
        return 0;
    }
    out_removed.* = allocDoxaString(scope, src[iu .. iu + 1]);
    const out_len = src.len - 1;
    const out = scope_arena.allocSlice(scope, u8, out_len);
    @memcpy(out[0..iu], src[0..iu]);
    @memcpy(out[iu..out_len], src[iu + 1 ..]);
    out_remaining.* = .{ .ptr = out.ptr, .len = out_len };
    return 1;
}

pub export fn doxa_str_pop(scope: *Scope, s_ptr: ?[*]const u8, s_len: u64, out_remaining: *DoxaString, out_popped: *DoxaString) callconv(.c) u8 {
    const src = sliceFromDoxaString(.{ .ptr = s_ptr, .len = s_len });
    if (src.len == 0) {
        out_remaining.* = .{ .ptr = null, .len = 0 };
        out_popped.* = .{ .ptr = null, .len = 0 };
        return 0;
    }

    var last_char_start: usize = src.len;
    var i: usize = src.len;
    while (i > 0) {
        i -= 1;
        const byte = src[i];
        if ((byte & 0x80) == 0) {
            last_char_start = i;
            break;
        } else if ((byte & 0xC0) == 0xC0) {
            last_char_start = i;
            break;
        }
    }

    out_remaining.* = allocDoxaString(scope, src[0..last_char_start]);
    out_popped.* = allocDoxaString(scope, src[last_char_start..src.len]);
    return 1;
}

pub export fn doxa_str_len(ptr: ?[*]const u8, len: u64) callconv(.c) i64 {
    _ = ptr;
    return @intCast(len);
}
