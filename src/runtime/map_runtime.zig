const std = @import("std");

/// Simple runtime map representation used by the native (LLVM) backend.
/// Keys and values are stored as raw i64 payloads. The compiler is
/// responsible for encoding/decoding according to the element tags.
///
/// Tags follow the same convention as `arrayElementTag` in the LLVM
/// IR printer:
///   0 = int(i64)
///   1 = byte(u8)
///   2 = float(f64)
///   3 = string(i8*)
///   4 = tetra(u8 lower 2 bits)
///   5 = nothing
///   6 = array(header ptr)
///   7 = struct(ptr)
///   8 = enum(discriminant i64)
const MapEntry = struct {
    key: i64,
    value: i64,
};

pub const MapHeader = struct {
    entries: [*]MapEntry,
    len: usize,
    capacity: usize,
    key_tag: u64,
    value_tag: u64,
};

fn mapAllocator() std.mem.Allocator {
    // Use the page allocator for deterministic, long‑lived storage.
    return std.heap.page_allocator;
}

pub fn mapNew(capacity_raw: i64, key_tag: i64, value_tag: i64) *MapHeader {
    const alloc = mapAllocator();

    const cap: usize = if (capacity_raw <= 0)
        0
    else
        @intCast(capacity_raw);

    const header = alloc.create(MapHeader) catch @panic("doxa_map_new: OOM creating map header");
    const slice = if (cap == 0)
        alloc.alloc(MapEntry, 1) catch @panic("doxa_map_new: OOM allocating entries")
    else
        alloc.alloc(MapEntry, cap) catch @panic("doxa_map_new: OOM allocating entries");

    header.entries = slice.ptr;
    header.len = 0;
    header.capacity = if (cap == 0) 0 else cap;
    header.key_tag = @intCast(key_tag);
    header.value_tag = @intCast(value_tag);
    return header;
}

fn strFromKeyBits(bits: i64) []const u8 {
    if (bits == 0) return "";
    const ptr: [*:0]const u8 = @ptrFromInt(@as(usize, @intCast(bits)));
    return std.mem.span(ptr);
}

fn keysEqual(key_tag: u64, a_bits: i64, b_bits: i64) bool {
    return switch (key_tag) {
        // int / enum discriminant / generic numeric: raw equality
        0, 2, 4, 8 => a_bits == b_bits,

        // string keys: compare C‑strings by content
        3 => blk: {
            const a = strFromKeyBits(a_bits);
            const b = strFromKeyBits(b_bits);
            break :blk std.mem.eql(u8, a, b);
        },

        else => a_bits == b_bits,
    };
}

pub fn mapSetI64(map: *MapHeader, key: i64, value: i64) void {
    const alloc = mapAllocator();

    // 1) Update existing entry if key already present
    var i: usize = 0;
    while (i < map.len) : (i += 1) {
        const entry = map.entries[i];
        if (keysEqual(map.key_tag, entry.key, key)) {
            map.entries[i].value = value;
            return;
        }
    }

    // 2) Append new entry, growing if needed
    if (map.len == map.capacity) {
        const new_cap: usize = if (map.capacity == 0) 4 else map.capacity * 2;
        const new_slice = alloc.alloc(MapEntry, new_cap) catch @panic("doxa_map_set_i64: OOM growing map");

        if (map.capacity > 0) {
            const old_slice = map.entries[0..map.capacity];
            @memcpy(new_slice[0..map.len], old_slice[0..map.len]);
            alloc.free(old_slice);
        }

        map.entries = new_slice.ptr;
        map.capacity = new_cap;
    }

    map.entries[map.len] = MapEntry{ .key = key, .value = value };
    map.len += 1;
}

pub fn mapGetI64(map: *MapHeader, key: i64) i64 {
    var i: usize = 0;
    while (i < map.len) : (i += 1) {
        const entry = map.entries[i];
        if (keysEqual(map.key_tag, entry.key, key)) {
            return entry.value;
        }
    }

    // No entry found – return 0 as a generic "nothing" payload; the
    // compiler will interpret this according to the map's value type.
    return 0;
}
