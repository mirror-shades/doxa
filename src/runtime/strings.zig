//! Strings: the `DoxaString` helpers and the exported string operations.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const abi = @import("abi.zig");
const instrument = @import("instrument.zig");
const fault = @import("fault.zig");
const numbers = @import("numbers.zig");

const Scope = scope_arena.Scope;
const DoxaString = abi.DoxaString;
const DoxaStringBuffer = abi.DoxaStringBuffer;

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

/// A concatenation's part (`plan/strings.md`, Phase 3): a string's bytes, or
/// the text of an int, a float, a byte or a tetra, formatted into `scratch`
/// exactly as `to_string` formats it.
fn partText(part: *const abi.DoxaValue, scratch: *numbers.Scratch) []const u8 {
    return switch (@as(abi.DoxaTag, @enumFromInt(part.tag))) {
        .String => sliceFromDoxaString(.{ .ptr = @ptrFromInt(@as(usize, @bitCast(part.payload_bits))), .len = @intCast(part.payload_len) }),
        .Int => numbers.intText(scratch, part.payload_bits),
        .Float => numbers.formatFloat(scratch, @bitCast(part.payload_bits)),
        .Byte => numbers.byteText(scratch, @truncate(@as(u64, @bitCast(part.payload_bits)))),
        .Tetra => numbers.tetraName(@truncate(@as(u64, @bitCast(part.payload_bits)))),
        else => unreachable, // lowering passes only strings and scalars
    };
}

/// The length of `parts`' text.
fn partsLen(parts: []const abi.DoxaValue) usize {
    var scratch: numbers.Scratch = undefined;
    var total: usize = 0;
    for (parts) |*part| total += partText(part, &scratch).len;
    return total;
}

/// Write `parts`' text into `out`, which is exactly as long.
fn writeParts(out: []u8, parts: []const abi.DoxaValue) void {
    var scratch: numbers.Scratch = undefined;
    var at: usize = 0;
    for (parts) |*part| {
        const text = partText(part, &scratch);
        @memcpy(out[at..][0..text.len], text);
        at += text.len;
    }
}

/// `parts` joined into one string in `scope`: measured, allocated once,
/// written once.
pub export fn doxa_str_concat(scope: *Scope, parts: [*]const abi.DoxaValue, count: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const all = parts[0..@intCast(count)];
    const total = partsLen(all);
    out_ptr.* = null;
    out_len.* = 0;
    if (total == 0) return;
    const out = scope_arena.allocSlice(scope, u8, total);
    writeParts(out, all);
    out_ptr.* = out.ptr;
    out_len.* = total;
}

/// Clone a string into `scope`.
/// The empty string is `(null, 0)`; no code reads through an empty string's
/// pointer, so a view can be empty at any offset.
pub fn strCloneInto(scope: *Scope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) void {
    const copy = allocDoxaString(scope, sliceFromDoxaString(.{ .ptr = ptr, .len = len }));
    out_ptr.* = @constCast(copy.ptr);
    out_len.* = copy.len;
}

/// Clone a string into `scope`: `clone` of a string. Also the inline-Zig
/// string-return boundary: a wrapper hands its result across the ABI already
/// owned by the caller's arena (its hidden first argument).
pub export fn doxa_str_clone(scope: *Scope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    strCloneInto(scope, ptr, len, out_ptr, out_len);
}

pub export fn doxa_find_str(h_ptr: ?[*]const u8, h_len: u64, n_ptr: ?[*]const u8, n_len: u64) callconv(.c) i64 {
    const str = sliceFromDoxaString(.{ .ptr = h_ptr, .len = h_len });
    const ndl = sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    if (ndl.len == 0) return 0;
    return if (std.mem.indexOf(u8, str, ndl)) |found| @intCast(found) else -1;
}

/// The bytes of `s` from `start`, `length` long, clamped to its end; empty
/// when `start` or `length` is negative or `start` is past the end. A view:
/// it allocates nothing, since a string's bytes are written once
/// (`plan/strings.md`).
pub export fn doxa_substring(ptr: ?[*]const u8, len: u64, start: i64, length: i64, out_ptr: *?[*]const u8, out_len: *u64) callconv(.c) void {
    const src = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    out_ptr.* = null;
    out_len.* = 0;
    if (start < 0 or length < 0) return;
    const start_u: usize = @intCast(start);
    if (start_u >= src.len) return;
    const end_u: usize = @min(start_u +| @as(usize, @intCast(length)), src.len);
    out_ptr.* = src[start_u..].ptr;
    out_len.* = end_u - start_u;
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

/// `s` without its byte at `idx`; `s` itself when `idx` is out of range.
/// Removing the first or the last byte leaves a view; removing one between
/// them is the one case that allocates, in `scope`.
pub export fn doxa_str_remove(scope: *Scope, s_ptr: ?[*]const u8, s_len: u64, idx: i64, out_ptr: *?[*]const u8, out_len: *u64) callconv(.c) void {
    const src = sliceFromDoxaString(.{ .ptr = s_ptr, .len = s_len });
    out_ptr.* = s_ptr;
    out_len.* = s_len;
    if (idx < 0 or @as(usize, @intCast(idx)) >= src.len) return;
    const iu: usize = @intCast(idx);
    const rest_len = src.len - 1;
    if (iu == 0) {
        out_ptr.* = src[1..].ptr;
    } else if (iu < rest_len) {
        const out = scope_arena.allocSlice(scope, u8, rest_len);
        @memcpy(out[0..iu], src[0..iu]);
        @memcpy(out[iu..], src[iu + 1 ..]);
        out_ptr.* = out.ptr;
    }
    out_len.* = rest_len;
}

/// Under memcheck, what a buffer's unwritten spare room holds.
const spare_poison: u8 = 0xAB;

/// `buf` becomes `from` followed by `parts` (`plan/strings.md`, "The leading
/// read"). A part may be bytes of `buf` itself: they lie below its length.
pub export fn doxa_str_append(scope: *Scope, buf: *DoxaStringBuffer, from_ptr: ?[*]const u8, from_len: u64, parts: [*]const abi.DoxaValue, count: u64) callconv(.c) void {
    const all = parts[0..@intCast(count)];
    writeParts(roomFor(scope, buf, from_ptr, from_len, partsLen(all)), all);
}

/// `doxa_str_append` of one string, `@push(s, t)` and `s += t`: the bytes are
/// copied straight in, with no parts to read.
pub export fn doxa_str_append_string(scope: *Scope, buf: *DoxaStringBuffer, from_ptr: ?[*]const u8, from_len: u64, part_ptr: ?[*]const u8, part_len: u64) callconv(.c) void {
    const part = sliceFromDoxaString(.{ .ptr = part_ptr, .len = part_len });
    @memcpy(roomFor(scope, buf, from_ptr, from_len, part.len), part);
}

/// Make `buf` `from` followed by `added` bytes, and return where those bytes
/// go. In place when `buf` still holds `from` (the same `(ptr, len)`, so the
/// same bytes: bytes are written once) and they fit in its spare room.
/// Otherwise `buf` moves to a new buffer in `scope` with room to grow, holding
/// a copy of `from`, and the old bytes stay intact for every value that still
/// shows them.
fn roomFor(scope: *Scope, buf: *DoxaStringBuffer, from_ptr: ?[*]const u8, from_len: u64, added: usize) []u8 {
    const from = sliceFromDoxaString(.{ .ptr = from_ptr, .len = from_len });
    const holds = buf.len == from.len and (from.len == 0 or buf.ptr == @as(?[*]u8, @constCast(from_ptr)));
    const total = from.len + added;
    if (holds and total <= buf.cap) {
        const room = buf.ptr.?[from.len..total];
        if (instrument.memcheck) {
            for (room) |b| if (b != spare_poison) fault.stringRewritten();
        }
        buf.len = total;
        return room;
    }
    if (added == 0) {
        // Nothing to append: `buf` holds `from`, with no spare room of its own.
        buf.* = .{ .ptr = @constCast(from_ptr), .len = from.len, .cap = from.len };
        return &.{};
    }
    const cap = scope_arena.grownCapacity(if (holds) buf.cap else 0, total);
    const bytes = scope_arena.allocSlice(scope, u8, @intCast(cap));
    @memcpy(bytes[0..from.len], from);
    if (instrument.memcheck) @memset(bytes[total..], spare_poison);
    buf.* = .{ .ptr = bytes.ptr, .len = total, .cap = cap };
    return bytes[from.len..total];
}
