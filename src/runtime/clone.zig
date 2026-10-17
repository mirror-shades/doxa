//! Deep copies: the one place that knows how to duplicate every heap value
//! into another arena. `clone` makes a fresh copy; `rehome` makes one only
//! when the source arena does not already outlive the destination.

const scope_arena = @import("scope_arena.zig");
const instrument = @import("instrument.zig");
const abi = @import("abi.zig");
const strings = @import("strings.zig");
const arrays = @import("arrays.zig");
const structs = @import("structs.zig");

const Scope = scope_arena.Scope;
const ArrayHeader = abi.ArrayHeader;
const DoxaValue = abi.DoxaValue;
const DoxaTag = abi.DoxaTag;
const ARRAY_TAG = abi.ARRAY_TAG;
const VALUE_TAG = abi.VALUE_TAG;

/// A `rehome` that keeps the value itself, counted under memtrace.
fn kept(value: anytype) @TypeOf(value) {
    instrument.rehomed(false);
    return value;
}

/// Re-home the heap payload of a boxed `DoxaValue` (String/Array/Struct) into
/// `scope`. Unboxed payloads (Int, Float, Byte, Tetra, Enum, Nothing) are left
/// unchanged.
pub fn cloneDoxaValueInto(scope: *Scope, val: *DoxaValue) void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .String => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr != 0) {
                const ptr: [*]const u8 = @ptrFromInt(@as(usize, @intCast(addr)));
                const len: usize = @intCast(val.payload_len);
                var cloned_ptr: ?[*]u8 = null;
                var cloned_len: u64 = 0;
                strings.strCloneInto(scope, ptr, @intCast(len), &cloned_ptr, &cloned_len);
                val.payload_bits = @bitCast(@as(u64, if (cloned_ptr) |p| @intFromPtr(p) else 0));
                val.payload_len = @intCast(cloned_len);
            }
        },
        .Array => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr != 0) {
                const hdr: *ArrayHeader = @ptrFromInt(@as(usize, @intCast(addr)));
                val.payload_bits = @intCast(@intFromPtr(arrayCloneIn(scope, hdr)));
            }
        },
        .Struct => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr != 0) {
                if (structCloneInto(scope, @ptrFromInt(@as(usize, @intCast(addr))))) |cloned| {
                    val.payload_bits = @intCast(@intFromPtr(cloned));
                }
            }
        },
        else => {},
    }
}

/// Deep-copy the heap payload of a box into `scope`: `clone` of a union or
/// group value. Unboxed payloads (Int, Float, Byte, Tetra, Enum, Nothing) are
/// left unchanged.
pub export fn doxa_clone_doxa_value(scope: *Scope, val: *DoxaValue) callconv(.c) void {
    cloneDoxaValueInto(scope, val);
}

/// Default the struct elements of a fixed array that stores pointers: each
/// slot of `buffer` gets its own copy of `template`, the struct's default
/// value, in `scope`.
pub export fn doxa_fixed_structs_default(scope: *Scope, buffer: [*]?*anyopaque, count: u64, template: *anyopaque) callconv(.c) void {
    for (0..@intCast(count)) |i| buffer[i] = structCloneInto(scope, template);
}

fn structCloneInto(scope: *Scope, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const desc = structs.structDescOf(src);
    const field_count: usize = @intCast(desc.field_count);
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    const dst: [*]i64 = @ptrCast(@alignCast(structs.structAllocIn(scope, desc, structs.structTotalWords(tags, field_count))));
    const src_fields: [*]const i64 = @ptrCast(@alignCast(src));
    var word: usize = 0;
    for (0..field_count) |i| {
        const tag: u64 = if (i < tags.len) tags[i] else 255;
        const bits = src_fields[word];
        if (tag == 3) {
            const len_bits = src_fields[word + 1];
            const addr: u64 = @bitCast(bits);
            const len: usize = @intCast(@as(u64, @bitCast(len_bits)));
            if (addr != 0) {
                var cloned_ptr: ?[*]u8 = null;
                var cloned_len: u64 = 0;
                strings.strCloneInto(scope, @ptrFromInt(@as(usize, @intCast(addr))), @intCast(len), &cloned_ptr, &cloned_len);
                dst[word] = @bitCast(@as(u64, if (cloned_ptr) |p| @intFromPtr(p) else 0));
                dst[word + 1] = @bitCast(@as(u64, cloned_len));
            } else {
                dst[word] = 0;
                dst[word + 1] = 0;
            }
            word += 2;
        } else if (tag == 6 and bits != 0) {
            const hdr: *ArrayHeader = @ptrFromInt(@as(usize, @intCast(bits)));
            const cloned = arrayCloneIn(scope, hdr);
            dst[word] = @intCast(@intFromPtr(cloned));
            word += 1;
        } else if (tag == 7 and bits != 0) {
            // Nested struct field: the source was allocated in a scope that may
            // be freed as soon as the outer struct is cloned (e.g. a function's
            // scope), so deep-copy it into the destination scope rather than
            // copying the pointer verbatim.
            const nested_src: ?*anyopaque = @ptrFromInt(@as(usize, @intCast(bits)));
            const nested_words: u64 = if (desc.field_struct_words) |fw| fw[i] else 0;
            const nested = if (nested_words != 0)
                structs.structCloneScalarInto(scope, @intCast(nested_words), nested_src)
            else
                structCloneInto(scope, nested_src);
            dst[word] = @intCast(@intFromPtr(nested.?));
            word += 1;
        } else if (tag == 9 and bits != 0) {
            // Boxed union/group field: the word is a pointer to a %DoxaValue.
            // Deep-copy the box and its heap payload into the destination scope
            // so the union survives the source arena being freed.
            const src_box: *const DoxaValue = @ptrFromInt(@as(usize, @intCast(bits)));
            const dst_words = scope_arena.allocSlice(scope, i64, 3);
            const dst_box: *DoxaValue = @ptrCast(@alignCast(dst_words.ptr));
            dst_box.* = src_box.*;
            cloneDoxaValueInto(scope, dst_box);
            dst[word] = @intCast(@intFromPtr(dst_box));
            word += 1;
        } else {
            dst[word] = bits;
            word += 1;
        }
    }
    return @ptrCast(dst);
}

/// Deep-copy a struct into `scope`: `clone` of a described struct. String
/// (tag 3) and array (tag 6) fields are cloned recursively so their storage
/// outlives the scope that constructed the struct; other fields are copied
/// verbatim.
pub export fn doxa_struct_clone(scope: *Scope, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structCloneInto(scope, ptr);
}

/// Rehome for a descriptor-free scalar struct, which has no header to say
/// which arena owns it (it may be an element stored flat in a fixed array).
/// Keeps the original pointer when its arena outlives `scope`, or when no live
/// arena holds it, and otherwise copies `word_count` words into `scope`.
/// Identity therefore survives the same cases it does for a described struct,
/// such as an `each` binding writing through to an element.
fn structRehomeScalarInto(scope: *Scope, word_count: usize, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const owner = scope_arena.ownerOf(src) orelse return kept(src);
    if (scope_arena.isEqualOrDescendant(scope, owner)) return kept(src);
    instrument.rehomed(true);
    return structs.structCloneScalarInto(scope, word_count, src);
}

pub export fn doxa_struct_rehome_scalar(scope: *Scope, word_count: u64, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structRehomeScalarInto(scope, @intCast(word_count), ptr);
}

/// `rehome` of a described struct, the one runtime-checked copy
/// (`plan/register-hir.md`, "What stays a runtime check").
/// Like `structCloneInto`, but keep the original pointer when its owning
/// arena already outlives `scope` (same arena or an ancestor): a fresh value
/// that already lives long enough is not copied.
fn structRehomeInto(scope: *Scope, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    if (scope_arena.isEqualOrDescendant(scope, structs.headerOf(src).scope)) return kept(src);
    instrument.rehomed(true);
    return structCloneInto(scope, src);
}

pub export fn doxa_struct_rehome(scope: *Scope, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structRehomeInto(scope, ptr);
}

fn arrayCloneIn(scope: *Scope, src: *ArrayHeader) *ArrayHeader {
    const result = arrays.arrayNewIn(scope, src.elem_size, src.elem_tag, src.len);
    result.elem_words = src.elem_words;
    var idx: u64 = 0;
    while (idx < src.len) : (idx += 1) copyElement(result, idx, src, idx);
    return result;
}

/// Deep-copy an array into `scope`: `clone` of an array.
pub export fn doxa_array_clone(scope: *Scope, hdr: *ArrayHeader) callconv(.c) *ArrayHeader {
    return arrayCloneIn(scope, hdr);
}

/// `rehome` of an array: keep it when its arena already outlives `scope`, and
/// clone it there otherwise. The one runtime-checked copy.
pub export fn doxa_array_rehome(scope: *Scope, hdr: *ArrayHeader) callconv(.c) *ArrayHeader {
    if (scope_arena.isEqualOrDescendant(scope, hdr.scope)) return kept(hdr);
    instrument.rehomed(true);
    return arrayCloneIn(scope, hdr);
}

/// Copy element `src_idx` of `src` to `dst_idx` of `dst`, whatever the
/// element representation: a deep copy of a heap element into `dst`'s arena.
fn copyElement(dst: *ArrayHeader, dst_idx: u64, src: *ArrayHeader, src_idx: u64) void {
    switch (src.elem_tag) {
        3 => {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: u64 = undefined;
            arrays.doxa_array_get_str(src, src_idx, &str_ptr, &str_len);
            strings.strCloneInto(dst.scope, str_ptr, str_len, &str_ptr, &str_len);
            arrays.doxa_array_set_str(dst, dst_idx, str_ptr, str_len);
        },
        VALUE_TAG => {
            var value: DoxaValue = undefined;
            arrays.doxa_array_get_value(src, src_idx, &value);
            cloneDoxaValueInto(dst.scope, &value);
            arrays.doxa_array_set_value(dst, dst_idx, &value);
        },
        ARRAY_TAG => {
            const bits = arrays.doxa_array_get_i64(src, src_idx);
            const inner: ?*ArrayHeader = if (bits == 0) null else @ptrFromInt(@as(usize, @intCast(@as(u64, @bitCast(bits)))));
            const copied: u64 = if (inner) |h| @intFromPtr(arrayCloneIn(dst.scope, h)) else 0;
            arrays.doxa_array_set_i64(dst, dst_idx, @bitCast(copied));
        },
        7 => {
            const bits = arrays.doxa_array_get_i64(src, src_idx);
            const element: ?*anyopaque = if (bits == 0) null else @ptrFromInt(@as(usize, @intCast(@as(u64, @bitCast(bits)))));
            const copied: ?*anyopaque = if (element == null)
                null
            else if (src.elem_words != 0)
                structs.structCloneScalarInto(dst.scope, @intCast(src.elem_words), element)
            else
                structCloneInto(dst.scope, element.?);
            arrays.doxa_array_set_i64(dst, dst_idx, @bitCast(@as(u64, if (copied) |c| @intFromPtr(c) else 0)));
        },
        else => arrays.doxa_array_set_i64(dst, dst_idx, arrays.doxa_array_get_i64(src, src_idx)),
    }
}

pub export fn doxa_array_concat(scope: *Scope, a: *ArrayHeader, b: *ArrayHeader) callconv(.c) *ArrayHeader {
    const result = arrays.arrayNewIn(scope, a.elem_size, a.elem_tag, a.len + b.len);
    result.elem_words = a.elem_words;
    var idx: u64 = 0;
    while (idx < a.len) : (idx += 1) copyElement(result, idx, a, idx);
    idx = 0;
    while (idx < b.len) : (idx += 1) copyElement(result, a.len + idx, b, idx);
    return result;
}

pub export fn doxa_array_slice(scope: *Scope, h: *ArrayHeader, start: i64, length: i64) callconv(.c) *ArrayHeader {
    const out_len: u64 = if (start < 0 or length < 0 or @as(u64, @intCast(start)) >= h.len or length == 0)
        0
    else
        @min(@as(u64, @intCast(length)), h.len - @as(u64, @intCast(start)));
    const out = arrays.arrayNewIn(scope, h.elem_size, h.elem_tag, out_len);
    out.elem_words = h.elem_words;
    if (out_len == 0) return out;
    const s: u64 = @intCast(start);
    var i: u64 = 0;
    while (i < out_len) : (i += 1) copyElement(out, i, h, s + i);
    return out;
}
