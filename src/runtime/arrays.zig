//! Dynamic arrays: allocation, growth, element access, and in-place mutation.
//!
//! Deep copies (`clone`, `rehome`, `concat`, `slice`) live in `clone.zig`,
//! which is the only module that needs the whole copy graph.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const abi = @import("abi.zig");
const strings = @import("strings.zig");
const structs = @import("structs.zig");

const Scope = scope_arena.Scope;
const ArrayHeader = abi.ArrayHeader;
const ARRAY_TAG = abi.ARRAY_TAG;
const VALUE_TAG = abi.VALUE_TAG;

const ARRAY_MIN_CAPACITY: u64 = 8;

fn clampMin(a: u64, b: u64) u64 {
    return if (a < b) b else a;
}

fn ensureArrayCapacity(hdr: *ArrayHeader, required_len: u64) bool {
    if (required_len <= hdr.cap) return true;

    // Grow exponentially to avoid repeated reallocations on append-heavy paths.
    var new_cap: u64 = if (hdr.cap == 0) ARRAY_MIN_CAPACITY else hdr.cap;
    while (new_cap < required_len) {
        const doubled = std.math.mul(u64, new_cap, 2) catch {
            new_cap = required_len;
            break;
        };
        new_cap = doubled;
    }

    if (hdr.elem_size == 0) {
        hdr.cap = new_cap;
        return true;
    }

    if (hdr.elem_size == 8) {
        const new_slice = scope_arena.allocSlice(hdr.scope, i64, @intCast(new_cap));
        @memset(new_slice, 0);

        if (hdr.data) |old_data| {
            const old_slice_ptr: [*]i64 = @ptrCast(@alignCast(old_data));
            const old_slice = old_slice_ptr[0..@intCast(hdr.cap)];
            const copy_len: usize = @intCast(@min(hdr.len, hdr.cap));
            @memcpy(new_slice[0..copy_len], old_slice[0..copy_len]);
        }

        hdr.data = @ptrCast(new_slice.ptr);
        hdr.cap = new_cap;
        return true;
    }

    const new_bytes_u64 = std.math.mul(u64, hdr.elem_size, new_cap) catch return false;
    const old_bytes_u64 = std.math.mul(u64, hdr.elem_size, hdr.cap) catch return false;
    if (new_bytes_u64 > std.math.maxInt(usize)) return false;
    if (old_bytes_u64 > std.math.maxInt(usize)) return false;

    // 8-byte aligned so string elements (16-byte slots) stay aligned.
    const new_buf = scope_arena.alloc(hdr.scope, @intCast(new_bytes_u64), .fromByteUnits(8), @returnAddress());
    @memset(new_buf[0..@intCast(new_bytes_u64)], 0);

    if (hdr.data) |old_data| {
        const old_buf_ptr: [*]u8 = @ptrCast(old_data);
        const old_buf = old_buf_ptr[0..@intCast(old_bytes_u64)];
        const used_bytes_u64 = std.math.mul(u64, hdr.elem_size, @min(hdr.len, hdr.cap)) catch 0;
        const used_bytes: usize = @intCast(@min(used_bytes_u64, new_bytes_u64));
        @memcpy(new_buf[0..used_bytes], old_buf[0..used_bytes]);
    }

    hdr.data = @ptrCast(new_buf);
    hdr.cap = new_cap;
    return true;
}

/// 0=int(i64), 1=byte(u8), 2=float(f64), 3=string(i8*), 4=tetra(u8 lower 2 bits),
/// 5=nothing, 6=array(*ArrayHeader), 7=struct(ptr), 8=enum(i64 variant index),
/// 9=value(DoxaValue, a union or group element).
pub fn arrayNewIn(scope: *Scope, elem_size: u64, elem_tag: u64, init_len: u64) *ArrayHeader {
    const cap = clampMin(init_len, ARRAY_MIN_CAPACITY);
    const hdr_ptr = scope_arena.create(scope, ArrayHeader);
    const data_bytes: usize = @intCast(elem_size * cap);
    var data_ptr: ?*anyopaque = null;
    if (data_bytes != 0) {
        if (elem_size == 8) {
            const slice = scope_arena.allocSlice(scope, i64, @intCast(cap));
            @memset(slice, 0);
            data_ptr = @ptrCast(slice.ptr);
        } else {
            // Allocate with 8-byte alignment so string elements (16-byte slots)
            // can be read/written as (ptr, i64) pairs without misalignment.
            const buf = scope_arena.alloc(scope, data_bytes, .fromByteUnits(8), @returnAddress());
            @memset(buf[0..data_bytes], 0);
            data_ptr = @ptrCast(buf);
        }
    }
    hdr_ptr.* = ArrayHeader{
        .data = data_ptr,
        .len = init_len,
        .cap = cap,
        .elem_size = elem_size,
        .elem_tag = elem_tag,
        .scope = scope,
        .elem_words = 0,
    };
    return hdr_ptr;
}

/// Record that `hdr` holds descriptor-free scalar structs of `words` words.
/// Emitted right after a dynamic array of such a struct is created.
pub export fn doxa_array_set_elem_words(hdr: *ArrayHeader, words: u64) callconv(.c) void {
    hdr.elem_words = words;
}

/// A dynamic array in `scope`. Its header records the scope, so elements
/// stored into it later are re-homed there.
pub export fn doxa_array_new(scope: *Scope, elem_size: u64, elem_tag: u64, init_len: u64) callconv(.c) *ArrayHeader {
    return arrayNewIn(scope, elem_size, elem_tag, init_len);
}

pub export fn doxa_array_range(scope: *Scope, start: i64, end: i64) callconv(.c) *ArrayHeader {
    const count: u64 = if (end >= start) @intCast(end - start + 1) else 0;
    const hdr = arrayNewIn(scope, 8, 0, count);
    if (count == 0) return hdr;

    const data = hdr.data orelse return hdr;
    const i64_data: [*]i64 = @ptrCast(@alignCast(data));
    var i: u64 = 0;
    while (i < count) : (i += 1) {
        i64_data[@intCast(i)] = start + @as(i64, @intCast(i));
    }
    return hdr;
}

pub export fn doxa_array_new_nested(
    scope: *Scope,
    elem_size: u64,
    elem_tag: u64,
    init_len: u64,
    nested_sizes: [*]const u64,
    nested_depth: u64,
    inner_elem_size: u64,
    inner_elem_tag: u64,
) callconv(.c) *ArrayHeader {
    const outer = arrayNewIn(scope, elem_size, elem_tag, init_len);
    if (nested_depth == 0 or elem_tag != ARRAY_TAG) return outer;

    var idx: u64 = 0;
    while (idx < init_len) : (idx += 1) {
        const inner: *ArrayHeader = if (nested_depth == 1)
            arrayNewIn(scope, inner_elem_size, inner_elem_tag, nested_sizes[0])
        else
            doxa_array_new_nested(
                scope,
                @sizeOf(*anyopaque),
                ARRAY_TAG,
                nested_sizes[0],
                nested_sizes + 1,
                nested_depth - 1,
                inner_elem_size,
                inner_elem_tag,
            );
        doxa_array_set_i64(outer, idx, @as(i64, @intCast(@intFromPtr(inner))));
    }
    return outer;
}

/// Materialize a fixed-size array (row-major, all dimensions) into a heap
/// nested `ArrayHeader` tree in `scope`: `array.from_fixed`. `sizes` lists each dimension outermost-first (`depth` entries); the
/// innermost element is `inner_elem_size` bytes and carries `inner_elem_tag`.
///
/// A struct field is a single heap pointer, so a fixed (flat, stack-owned)
/// array cannot be stored by reference: it is promoted to the heap form that
/// dynamic arrays already use. Reads of the field therefore see a dynamic
/// array, which is the general representation.
pub export fn doxa_array_from_fixed(
    scope: *Scope,
    src: [*]const u8,
    sizes: [*]const u64,
    depth: u64,
    inner_elem_size: u64,
    inner_elem_tag: u64,
) callconv(.c) *ArrayHeader {
    return fixedArrayToNested(scope, src, sizes, depth, inner_elem_size, inner_elem_tag);
}

/// Materialize a fixed, flat array of scalar-only struct slots into a heap
/// `ArrayHeader` of struct pointers in `scope`.
///
/// A fixed array of structs stores each element inline as its raw
/// `struct_words` i64 words (`emitArrayNew`'s flat path). A dynamic array's
/// struct element is a heap pointer (tag 7), so each slot is copied into
/// `scope` on its own. The struct is descriptor-free, so it has no header:
/// the array carries its word count (`elem_words`), and every later copy of
/// an element is that many words.
pub export fn doxa_array_from_fixed_structs(
    scope: *Scope,
    src: [*]const u8,
    count: u64,
    struct_words: u64,
) callconv(.c) *ArrayHeader {
    const arr = arrayNewIn(scope, @sizeOf(*anyopaque), 7, count);
    arr.elem_words = struct_words;
    if (arr.data == null or count == 0) return arr;

    const src_words: [*]const i64 = @ptrCast(@alignCast(src));
    const slots: [*]?*anyopaque = @ptrCast(@alignCast(arr.data.?));
    const words: usize = @intCast(struct_words);
    for (0..@intCast(count)) |idx| {
        slots[idx] = structs.structCloneScalarInto(scope, words, @ptrCast(@constCast(src_words[idx * words ..])));
    }
    return arr;
}

fn fixedArrayToNested(
    scope: *Scope,
    src: [*]const u8,
    sizes: [*]const u64,
    depth: u64,
    inner_elem_size: u64,
    inner_elem_tag: u64,
) *ArrayHeader {
    const n = sizes[0];
    if (depth <= 1) {
        const arr = arrayNewIn(scope, inner_elem_size, inner_elem_tag, n);
        const bytes: usize = @intCast(inner_elem_size * n);
        if (arr.data != null and bytes != 0) {
            const dst: [*]u8 = @ptrCast(arr.data.?);
            @memcpy(dst[0..bytes], src[0..bytes]);
        }
        return arr;
    }

    const outer = arrayNewIn(scope, @sizeOf(*anyopaque), ARRAY_TAG, n);
    var stride: u64 = inner_elem_size;
    var d: u64 = 1;
    while (d < depth) : (d += 1) stride *= sizes[d];

    const data: [*]?*ArrayHeader = @ptrCast(@alignCast(outer.data.?));
    var idx: u64 = 0;
    while (idx < n) : (idx += 1) {
        data[@intCast(idx)] = fixedArrayToNested(
            scope,
            src + @as(usize, @intCast(idx * stride)),
            sizes + 1,
            depth - 1,
            inner_elem_size,
            inner_elem_tag,
        );
    }
    return outer;
}

/// Copy `src`'s elements back into the fixed (flat, row-major) array `dst`,
/// as many as both hold: the inverse of `doxa_array_from_fixed`, for a fixed
/// array lent to a dynamic `^` parameter.
pub export fn doxa_array_copy_to_fixed(src: *ArrayHeader, dst: [*]u8, sizes: [*]const u64, depth: u64, inner_elem_size: u64) callconv(.c) void {
    copyToFixed(src, dst, sizes, depth, inner_elem_size);
}

fn copyToFixed(src: *ArrayHeader, dst: [*]u8, sizes: [*]const u64, depth: u64, inner_elem_size: u64) void {
    const n = @min(src.len, sizes[0]);
    const data = src.data orelse return;
    if (depth <= 1) {
        const bytes: usize = @intCast(n * inner_elem_size);
        @memcpy(dst[0..bytes], @as([*]const u8, @ptrCast(data))[0..bytes]);
        return;
    }
    var stride: u64 = inner_elem_size;
    var d: u64 = 1;
    while (d < depth) : (d += 1) stride *= sizes[d];
    const rows: [*]const ?*ArrayHeader = @ptrCast(@alignCast(data));
    for (0..@intCast(n)) |i| {
        const row = rows[i] orelse continue;
        copyToFixed(row, dst + @as(usize, @intCast(i * stride)), sizes + 1, depth - 1, inner_elem_size);
    }
}

/// `doxa_array_copy_to_fixed` for a fixed array of flat structs: each
/// element's words are copied back into its inline slot.
pub export fn doxa_array_copy_to_fixed_structs(src: *ArrayHeader, dst: [*]i64, count: u64, struct_words: u64) callconv(.c) void {
    const n = @min(src.len, count);
    const data = src.data orelse return;
    const slots: [*]const ?*anyopaque = @ptrCast(@alignCast(data));
    const words: usize = @intCast(struct_words);
    for (0..@intCast(n)) |i| {
        const element = slots[i] orelse continue;
        @memcpy(dst[i * words ..][0..words], @as([*]const i64, @ptrCast(@alignCast(element)))[0..words]);
    }
}

/// Move element `from` of `h` to `to`: the element stays in `h`'s arena, so
/// nothing is copied.
fn moveElement(h: *ArrayHeader, to: u64, from: u64) void {
    const base: [*]u8 = @ptrCast(h.data.?);
    const size: usize = @intCast(h.elem_size);
    @memcpy(base[@as(usize, @intCast(to)) * size ..][0..size], base[@as(usize, @intCast(from)) * size ..][0..size]);
}

pub export fn doxa_array_len(hdr: *ArrayHeader) callconv(.c) u64 {
    return hdr.len;
}

/// Borrowed view of an array's backing buffer, for inline-Zig boundary adapters
/// that must present a contiguous slice. The buffer belongs to the array's
/// scope arena; the caller must not free it and must not use it past the
/// array's lifetime. Returns null when the array has no backing buffer.
pub export fn doxa_array_data(hdr: ?*ArrayHeader) callconv(.c) ?[*]u8 {
    const h = hdr orelse return null;
    return if (h.data) |d| @ptrCast(d) else null;
}

pub export fn doxa_array_get_i64(hdr: *ArrayHeader, idx: u64) callconv(.c) i64 {
    if (hdr.data == null or idx >= hdr.len) return 0;

    const base: [*]const u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;

    return switch (hdr.elem_tag) {
        0 => blk_int: { // int (i64)
            const ip: *const i64 = @ptrCast(@alignCast(p));
            break :blk_int ip.*;
        },
        1 => blk_byte: { // byte (u8)
            const bp: *const u8 = @ptrCast(p);
            break :blk_byte @as(i64, bp.*);
        },
        2 => blk_float: { // float (f64)
            const fp: *const f64 = @ptrCast(@alignCast(p));
            const bits: i64 = @bitCast(fp.*);
            break :blk_float bits;
        },
        3 => blk_str: { // string: 16-byte DoxaString, first word is the pointer
            const ip: *const i64 = @ptrCast(@alignCast(p));
            break :blk_str ip.*;
        },
        4 => blk_tetra: { // tetra (2-bit stored in u8)
            const tp: *const u8 = @ptrCast(p);
            const v: u8 = tp.* & 0x3;
            break :blk_tetra @as(i64, v);
        },
        6 => blk_array: { // array (*ArrayHeader pointer encoded as bits)
            const ap: *const ?*ArrayHeader = @ptrCast(@alignCast(p));
            const a_ptr = ap.* orelse null;
            const addr: u64 = if (a_ptr) |ptr|
                @intFromPtr(ptr)
            else
                0;
            break :blk_array @bitCast(addr);
        },
        else => blk_raw: {
            const ip: *const i64 = @ptrCast(@alignCast(p));
            break :blk_raw ip.*;
        },
    };
}

/// Store element `idx`, growing the array to reach it. Every element store
/// keeps its value as given: the compiler placed a heap element in the
/// array's arena first (`arena_of`), and the runtime's own copies
/// (`copyElement`) clone before they store.
pub export fn doxa_array_set_i64(hdr: *ArrayHeader, idx: u64, value: i64) callconv(.c) void {
    const needed_len = idx + 1;
    if (!ensureArrayCapacity(hdr, needed_len)) return;
    if (hdr.data == null and hdr.elem_size != 0) return;
    if (idx >= hdr.len) hdr.len = idx + 1;
    const base: [*]u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;

    switch (hdr.elem_tag) {
        0 => { // int (i64)
            const ip: *i64 = @ptrCast(@alignCast(p));
            ip.* = value;
        },
        1 => { // byte (u8)
            const bp: *u8 = @ptrCast(p);
            bp.* = @intCast(@as(u8, @intCast(value)) & 0xff);
        },
        2 => { // float (f64)
            const fp: *f64 = @ptrCast(@alignCast(p));
            const f: f64 = @bitCast(value);
            fp.* = f;
        },
        3, VALUE_TAG => { // string and value: handled by their own setters
            return;
        },
        4 => { // tetra (2-bit stored in u8)
            const tp: *u8 = @ptrCast(p);
            tp.* = @intCast(@as(u8, @intCast(value)) & 0x3);
        },
        // An array, a struct (pointers as bits), or a scalar word: stored as
        // given. A heap element is already placed in the array's arena.
        else => {
            const ip: *i64 = @ptrCast(@alignCast(p));
            ip.* = value;
        },
    }
}

pub export fn doxa_array_get_str(hdr: *ArrayHeader, idx: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (hdr.data == null or idx >= hdr.len) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const base: [*]const u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    if (hdr.elem_tag == 3) {
        const ptr_slot: *const i64 = @ptrCast(@alignCast(p));
        const len_slot: *const i64 = @ptrCast(@alignCast(p + 8));
        const addr: u64 = @bitCast(ptr_slot.*);
        const ptr: ?[*]const u8 = if (addr == 0) null else @ptrFromInt(@as(usize, @intCast(addr)));
        const len_u64: u64 = @bitCast(len_slot.*);
        out_ptr.* = @constCast(ptr);
        out_len.* = @intCast(len_u64);
        return;
    }
    out_ptr.* = null;
    out_len.* = 0;
}

pub export fn doxa_array_set_str(hdr: *ArrayHeader, idx: u64, str_ptr: ?[*]const u8, str_len: u64) callconv(.c) void {
    const needed_len = idx + 1;
    if (!ensureArrayCapacity(hdr, needed_len)) return;
    if (hdr.data == null and hdr.elem_size != 0) return;
    if (idx >= hdr.len) hdr.len = idx + 1;
    if (hdr.elem_tag != 3) return;
    const base: [*]u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ptr_slot: *i64 = @ptrCast(@alignCast(p));
    const len_slot: *i64 = @ptrCast(@alignCast(p + 8));
    const addr: u64 = if (str_ptr) |vp| @intFromPtr(vp) else 0;
    ptr_slot.* = @bitCast(addr);
    len_slot.* = @bitCast(@as(u64, str_len));
}

/// Read a union or group element (tag 9) into `out`. Out of range, or any
/// other element kind, reads as a `nothing` box.
pub export fn doxa_array_get_value(hdr: *ArrayHeader, idx: u64, out: *abi.DoxaValue) callconv(.c) void {
    if (hdr.data == null or idx >= hdr.len or hdr.elem_tag != VALUE_TAG) {
        out.* = .{ .tag = @intFromEnum(abi.DoxaTag.Nothing), .reserved = 0, .payload_bits = 0, .payload_len = 0 };
        return;
    }
    const slots: [*]const abi.DoxaValue = @ptrCast(@alignCast(hdr.data.?));
    out.* = slots[@intCast(idx)];
}

/// Store a union or group element (tag 9), as `doxa_array_set_i64` stores.
pub export fn doxa_array_set_value(hdr: *ArrayHeader, idx: u64, value: *const abi.DoxaValue) callconv(.c) void {
    if (!ensureArrayCapacity(hdr, idx + 1)) return;
    if (hdr.data == null or hdr.elem_tag != VALUE_TAG) return;
    if (idx >= hdr.len) hdr.len = idx + 1;
    const slots: [*]abi.DoxaValue = @ptrCast(@alignCast(hdr.data.?));
    slots[@intCast(idx)] = value.*;
}

pub export fn doxa_pack_bytes(scope: *Scope, hdr: ?*ArrayHeader, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (hdr == null) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const arr = hdr.?;
    const buf = scope_arena.allocSlice(scope, u8, @intCast(arr.len));
    for (0..@intCast(arr.len)) |i| {
        buf[i] = @intCast(doxa_array_get_i64(arr, i));
    }
    out_ptr.* = buf.ptr;
    out_len.* = @intCast(arr.len);
}

pub export fn doxa_unpack_bytes(scope: *Scope, ptr: ?[*]const u8, len: u64) callconv(.c) ?*ArrayHeader {
    const bytes = strings.sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    const result = doxa_array_new(scope, 1, 1, bytes.len);
    for (bytes, 0..) |ch, i| {
        doxa_array_set_i64(result, i, ch);
    }
    return result;
}

pub export fn doxa_find_array(hdr: ?*ArrayHeader, value: i64) callconv(.c) i64 {
    const h = hdr orelse return -1;
    var idx: u64 = 0;
    while (idx < h.len) : (idx += 1) {
        const elem = doxa_array_get_i64(h, idx);
        if (elem == value) return @intCast(idx);
    }
    return -1;
}

pub export fn doxa_find_array_str(hdr: ?*ArrayHeader, n_ptr: ?[*]const u8, n_len: u64) callconv(.c) i64 {
    const h = hdr orelse return -1;
    const ndl = strings.sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    var idx: u64 = 0;
    while (idx < h.len) : (idx += 1) {
        var elem_ptr: ?[*]u8 = null;
        var elem_len: u64 = 0;
        doxa_array_get_str(h, idx, &elem_ptr, &elem_len);
        const elem = strings.sliceFromDoxaString(.{ .ptr = elem_ptr, .len = elem_len });
        if (std.mem.eql(u8, elem, ndl)) return @intCast(idx);
    }
    return -1;
}

/// Open a gap at `pos` by moving every element from `pos` on one place up.
/// The caller has already grown `h.len` by one.
fn shiftUp(h: *ArrayHeader, pos: u64, old_len: u64) void {
    var i: u64 = old_len;
    while (i > pos) : (i -= 1) moveElement(h, i, i - 1);
}

/// Close the gap at `pos` by moving every later element one place down.
fn shiftDown(h: *ArrayHeader, pos: u64) void {
    var i: u64 = pos;
    while (i + 1 < h.len) : (i += 1) moveElement(h, i, i + 1);
}

/// The insertion point for `idx` in `h`, with room for one more element, or
/// null when `idx` is out of range or the array cannot grow.
fn openInsert(h: *ArrayHeader, idx: i64) ?u64 {
    if (idx < 0) return null;
    const pos: u64 = @intCast(idx);
    if (pos > h.len) return null;
    const old_len = h.len;
    if (!ensureArrayCapacity(h, old_len + 1)) return null;
    h.len = old_len + 1;
    shiftUp(h, pos, old_len);
    return pos;
}

// The length-changing operations mutate in place: a dynamic array's header
// never moves, so the register HIR treats them as effects on the array value.

pub export fn doxa_array_insert(h: *ArrayHeader, idx: i64, value: i64) callconv(.c) void {
    const pos = openInsert(h, idx) orelse return;
    doxa_array_set_i64(h, pos, value);
}

pub export fn doxa_array_remove(h: *ArrayHeader, idx: i64) callconv(.c) i64 {
    const pos = removalIndex(h, idx) orelse return 0;
    const removed = doxa_array_get_i64(h, pos);
    shiftDown(h, pos);
    h.len -= 1;
    return removed;
}

pub export fn doxa_array_insert_str(h: *ArrayHeader, idx: i64, str_ptr: ?[*]const u8, str_len: u64) callconv(.c) void {
    const pos = openInsert(h, idx) orelse return;
    doxa_array_set_str(h, pos, str_ptr, str_len);
}

pub export fn doxa_array_remove_str(h: *ArrayHeader, idx: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    out_ptr.* = null;
    out_len.* = 0;
    const pos = removalIndex(h, idx) orelse return;
    doxa_array_get_str(h, pos, out_ptr, out_len);
    shiftDown(h, pos);
    h.len -= 1;
}

/// Insert a union or group element (tag 9) at `idx`.
pub export fn doxa_array_insert_value(h: *ArrayHeader, idx: i64, value: *const abi.DoxaValue) callconv(.c) void {
    const pos = openInsert(h, idx) orelse return;
    doxa_array_set_value(h, pos, value);
}

/// Remove the union or group element (tag 9) at `idx` into `out_removed`.
pub export fn doxa_array_remove_value(h: *ArrayHeader, idx: i64, out_removed: *abi.DoxaValue) callconv(.c) void {
    const pos = removalIndex(h, idx) orelse {
        doxa_array_get_value(h, std.math.maxInt(u64), out_removed);
        return;
    };
    doxa_array_get_value(h, pos, out_removed);
    shiftDown(h, pos);
    h.len -= 1;
}

/// The position `idx` names in `h`, or null when it is out of range.
fn removalIndex(h: *const ArrayHeader, idx: i64) ?u64 {
    if (idx < 0) return null;
    const pos: u64 = @intCast(idx);
    return if (pos < h.len) pos else null;
}

/// `@clear` of an array: empty it in place.
pub export fn doxa_array_clear(h: *ArrayHeader) callconv(.c) void {
    h.len = 0;
}
