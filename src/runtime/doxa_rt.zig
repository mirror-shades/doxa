const std = @import("std");
const builtin = @import("builtin");
const MapRuntime = @import("map_runtime.zig");

//------------------------------------------------------------------------------
// DOXA Runtime Support
//
// This module consolidates the exported helpers that the generated code
// expects to link against.  It currently exposes the legacy array helpers and
// serves as the place where future debug/peek/print shims will live.
//------------------------------------------------------------------------------

fn writeStdout(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const win = std.os.windows;
        const handle = win.kernel32.GetStdHandle(win.STD_OUTPUT_HANDLE);
        if (handle == win.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win.kernel32.WriteFile(handle.?, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    _ = stdout.write(slice) catch return;
    _ = stdout.flush() catch return;
}

fn writeStderr(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const win = std.os.windows;
        const handle = win.kernel32.GetStdHandle(win.STD_ERROR_HANDLE);
        if (handle == win.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win.kernel32.WriteFile(handle.?, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    _ = stderr.write(slice) catch return;
    _ = stderr.flush() catch return;
}

pub export fn doxa_write_cstr(ptr: ?[*:0]const u8) callconv(.c) void {
    if (ptr) |p| {
        const slice = std.mem.span(p);
        writeStdout(slice);
    }
}

pub export fn doxa_write_quoted_string(ptr: ?[*:0]const u8) callconv(.c) void {
    if (ptr) |p| {
        const slice = std.mem.span(p);
        writeStdout("\"");
        writeStdout(slice);
        writeStdout("\"");
    }
}

// Simple peek function for strings - called by LLVM IR with raw string pointers
pub export fn doxa_peek_string(ptr: ?[*:0]const u8) callconv(.c) void {
    writeStdout("\"");
    if (ptr) |p| {
        const slice = std.mem.span(p);
        writeStdout(slice);
    }
    writeStdout("\"");
}

pub export fn doxa_str_eq(a: ?[*:0]const u8, b: ?[*:0]const u8) callconv(.c) bool {
    const as = if (a) |p| std.mem.span(p) else "";
    const bs = if (b) |p| std.mem.span(p) else "";
    return std.mem.eql(u8, as, bs);
}

fn allocSentinelString(bytes: []const u8) ?[*:0]u8 {
    const out = std.heap.page_allocator.allocSentinel(u8, bytes.len, 0) catch return null;
    @memcpy(out[0..bytes.len], bytes);
    return out.ptr;
}

pub export fn doxa_int_from_cstr(ptr: ?[*:0]const u8) callconv(.c) i64 {
    if (ptr == null) return 0;
    const raw = std.mem.span(ptr.?);
    const s = std.mem.trim(u8, raw, " \t\r\n");
    if (s.len == 0) return 0;

    // Hex prefix (supports negative hex like -0x0A)
    const is_neg = s[0] == '-';
    const hex_start: usize = if (is_neg) 1 else 0;
    if (s.len >= hex_start + 2 and s[hex_start] == '0' and (s[hex_start + 1] == 'x' or s[hex_start + 1] == 'X')) {
        const digits = s[hex_start + 2 ..];
        const parsed = std.fmt.parseInt(i64, digits, 16) catch return 0;
        return if (is_neg) -parsed else parsed;
    }

    // Float-like strings: parse as float then truncate toward zero.
    if (std.mem.indexOfScalar(u8, s, '.') != null) {
        const f = std.fmt.parseFloat(f64, s) catch return 0;
        return @intFromFloat(f);
    }

    return std.fmt.parseInt(i64, s, 10) catch 0;
}

pub export fn doxa_float_from_cstr(ptr: ?[*:0]const u8) callconv(.c) f64 {
    if (ptr == null) return 0.0;
    const raw = std.mem.span(ptr.?);
    const s = std.mem.trim(u8, raw, " \t\r\n");
    if (s.len == 0) return 0.0;

    const is_neg = s[0] == '-';
    const hex_start: usize = if (is_neg) 1 else 0;
    if (s.len >= hex_start + 2 and s[hex_start] == '0' and (s[hex_start + 1] == 'x' or s[hex_start + 1] == 'X')) {
        const digits = s[hex_start + 2 ..];
        const parsed = std.fmt.parseInt(i64, digits, 16) catch return 0.0;
        const signed = if (is_neg) -parsed else parsed;
        return @floatFromInt(signed);
    }

    // Accept plain integers and floats.
    return std.fmt.parseFloat(f64, s) catch 0.0;
}

pub export fn doxa_int_to_string(value: i64) callconv(.c) ?[*:0]u8 {
    var buf: [64]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return null;
    return allocSentinelString(s);
}

pub export fn doxa_float_to_string(value: f64) callconv(.c) ?[*:0]u8 {
    var buf: [64]u8 = undefined;
    const rounded_down = std.math.floor(value);
    const s = if (value - rounded_down == 0)
        std.fmt.bufPrint(&buf, "{d}.0", .{value}) catch return null
    else
        std.fmt.bufPrint(&buf, "{d}", .{value}) catch return null;
    return allocSentinelString(s);
}

pub export fn doxa_byte_to_string(value: i64) callconv(.c) ?[*:0]u8 {
    var buf: [16]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const s = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch return null;
    return allocSentinelString(s);
}

pub export fn doxa_tetra_to_string(value: i64) callconv(.c) ?[*:0]u8 {
    const v: u8 = @intCast(value & 0x3);
    const name: []const u8 = switch (v) {
        0 => "false",
        1 => "true",
        2 => "both",
        3 => "neither",
        else => "invalid",
    };
    return allocSentinelString(name);
}

pub export fn doxa_enum_to_string(type_name_ptr: ?[*:0]const u8, bits: i64) callconv(.c) ?[*:0]u8 {
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printEnumImpl(w, type_name_ptr, bits) catch return null;
    return allocSentinelString(list.items);
}

pub export fn doxa_struct_to_string(instance: ?*anyopaque) callconv(.c) ?[*:0]u8 {
    if (instance == null) return doxa_str_clone(null);
    const addr: u64 = @intFromPtr(instance.?);
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printStructImpl(w, addr) catch return null;
    return allocSentinelString(list.items);
}

pub export fn doxa_array_to_string(hdr: ?*ArrayHeader) callconv(.c) ?[*:0]u8 {
    if (hdr == null) return doxa_str_clone(null);
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printArrayHdrImpl(w, hdr.?) catch return null;
    return allocSentinelString(list.items);
}

pub export fn doxa_str_concat(a: ?[*:0]const u8, b: ?[*:0]const u8) callconv(.c) ?[*:0]u8 {
    const as = if (a) |p| std.mem.span(p) else "";
    const bs = if (b) |p| std.mem.span(p) else "";

    const total_len: usize = as.len + bs.len;
    const buf = std.heap.page_allocator.alloc(u8, total_len + 1) catch return null;
    @memcpy(buf[0..as.len], as);
    @memcpy(buf[as.len .. as.len + bs.len], bs);
    buf[total_len] = 0;

    return buf[0..total_len :0].ptr;
}

pub export fn doxa_str_clone(a: ?[*:0]const u8) callconv(.c) ?[*:0]u8 {
    const as = if (a) |p| std.mem.span(p) else "";

    const buf = std.heap.page_allocator.alloc(u8, as.len + 1) catch return null;
    @memcpy(buf[0..as.len], as);
    buf[as.len] = 0;

    return buf[0..as.len :0].ptr;
}

pub export fn doxa_char_to_string(ch: u8) callconv(.c) ?[*:0]u8 {
    const buf = std.heap.page_allocator.alloc(u8, 2) catch return null;
    buf[0] = ch;
    buf[1] = 0;
    return buf[0..1 :0].ptr;
}

pub export fn doxa_print_i64(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_u64(value: u64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_f64(value: f64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rounded_down = std.math.floor(value);
    const rendered = if (value - rounded_down == 0)
        std.fmt.bufPrint(&buf, "{d}.0", .{value}) catch return
    else
        std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_byte(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const rendered = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch return;
    writeStdout(rendered);
}

fn getEnumVariantName(type_name: []const u8, variant_index: i64) []const u8 {
    // For now, we'll implement a simple mapping for common enum types
    // In a full implementation, this would look up the enum type registry
    if (std.mem.eql(u8, type_name, "Color")) {
        return switch (variant_index) {
            0 => "Red",
            1 => "Green",
            2 => "Blue",
            else => "Unknown",
        };
    }

    // Default fallback
    return switch (variant_index) {
        0 => "Variant0",
        1 => "Variant1",
        2 => "Variant2",
        3 => "Variant3",
        4 => "Variant4",
        5 => "Variant5",
        6 => "Variant6",
        7 => "Variant7",
        else => "Unknown",
    };
}

// Built-in functions
var rng_state: ?std.Random.DefaultPrng = null;

pub export fn doxa_random() callconv(.c) f64 {
    if (rng_state == null) {
        const seed = @as(u64, @intCast(std.time.timestamp()));
        rng_state = std.Random.DefaultPrng.init(seed);
    }
    return rng_state.?.random().float(f64);
}

// Fast integer-only random number generator for dice rolls
pub export fn doxa_random_int() callconv(.c) i64 {
    if (rng_state == null) {
        const seed = @as(u64, @intCast(std.time.timestamp()));
        rng_state = std.Random.DefaultPrng.init(seed);
    }
    return rng_state.?.random().intRangeAtMost(i64, 0, 5) + 1;
}

// Ultra-fast dice roll - single function call, no floating point
pub export fn doxa_dice_roll() callconv(.c) i64 {
    if (rng_state == null) {
        const seed = @as(u64, @intCast(std.time.timestamp()));
        rng_state = std.Random.DefaultPrng.init(seed);
    }
    return rng_state.?.random().intRangeAtMost(i64, 1, 6);
}

pub export fn doxa_int(value: f64) callconv(.c) i64 {
    return @intFromFloat(value);
}

pub export fn doxa_tick() callconv(.c) i64 {
    return @intCast(std.time.nanoTimestamp());
}

pub export fn doxa_input() callconv(.c) ?[*:0]u8 {
    const stdin_file = std.fs.File.stdin();
    var tmp: [256]u8 = undefined;
    var bytes = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer bytes.deinit();

    while (true) {
        const n = stdin_file.read(&tmp) catch return null;
        if (n == 0) break;
        for (tmp[0..n]) |ch| {
            if (ch == '\n') break;
            if (ch == '\r') continue;
            bytes.append(ch) catch return null;
        }
        if (std.mem.indexOfScalar(u8, tmp[0..n], '\n') != null) break;
        if (bytes.items.len >= 1024 * 1024) break;
    }

    const out = std.heap.page_allocator.allocSentinel(u8, bytes.items.len, 0) catch return null;
    @memcpy(out[0..bytes.items.len], bytes.items);
    return out.ptr;
}

/// Find value in array or substring in string
/// For arrays: collection is ArrayHeader*, value is i64 (or other type encoded as i64)
/// For strings: collection is ptr to string, value is ptr to substring
pub export fn doxa_find(collection: ?*anyopaque, value: i64) callconv(.c) i64 {
    if (collection == null) return -1;

    const maybe_hdr = asArrayHeader(collection);
    if (maybe_hdr) |hdr| {
        return doxa_find_array(hdr, value);
    }

    // Interpret as string (C string)
    const str_ptr = @as(?[*:0]const u8, @ptrCast(collection));
    if (str_ptr) |s| {
        const needle_ptr = @as(?[*:0]const u8, @ptrFromInt(@as(usize, @intCast(value))));
        return doxa_find_str(s, needle_ptr);
    }

    return -1;
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

pub export fn doxa_find_str(str_ptr: ?[*:0]const u8, needle_ptr: ?[*:0]const u8) callconv(.c) i64 {
    const s_ptr = str_ptr orelse return -1;
    const n_ptr = needle_ptr orelse return -1;
    const str = std.mem.span(s_ptr);
    const needle = std.mem.span(n_ptr);
    if (needle.len == 0) return 0;
    return if (std.mem.indexOf(u8, str, needle)) |found|
        @intCast(found)
    else
        -1;
}

//------------------------------------------------------------------------------
// Map helpers for native (LLVM) backend
//------------------------------------------------------------------------------

pub export fn doxa_map_new(capacity: i64, key_tag: i64, value_tag: i64) callconv(.c) *MapRuntime.MapHeader {
    return MapRuntime.mapNew(capacity, key_tag, value_tag);
}

pub export fn doxa_map_set_i64(map: *MapRuntime.MapHeader, key: i64, value: i64) callconv(.c) void {
    MapRuntime.mapSetI64(map, key, value);
}

pub export fn doxa_map_set_else_i64(map: *MapRuntime.MapHeader, value: i64) callconv(.c) void {
    MapRuntime.mapSetElseI64(map, value);
}

pub export fn doxa_map_get_i64(map: *MapRuntime.MapHeader, key: i64) callconv(.c) i64 {
    return MapRuntime.mapGetI64(map, key);
}

pub export fn doxa_map_try_get_i64(map: *MapRuntime.MapHeader, key: i64, out_value: *i64) callconv(.c) u8 {
    return if (MapRuntime.mapTryGetI64(map, key, out_value)) 1 else 0;
}

pub const DoxaPeekInfo = extern struct {
    file: ?[*:0]const u8,
    name: ?[*:0]const u8,
    type_name: ?[*:0]const u8,
    union_members: ?*const [*:0]const u8,
    union_member_count: u32,
    active_member_index: i32,
    has_location: u32,
    line: u32,
    column: u32,
};

/// View for C/LLVM callers to construct a `DoxaValue` without knowing its full
/// Zig definition. This mirrors the `%DoxaValue` layout in the LLVM IR.
pub const DoxaValueC = extern struct {
    tag: u32,
    reserved: u32,
    payload_bits: i64,
};

pub export fn doxa_debug_peek(info_ptr: ?*const DoxaPeekInfo) callconv(.c) void {
    const info = info_ptr orelse return;

    if (info.has_location != 0) {
        if (info.file) |file_ptr| {
            const file_slice = std.mem.span(file_ptr);
            var buf: [256]u8 = undefined;
            const location = std.fmt.bufPrint(&buf, "[{s}:{d}:{d}] ", .{ file_slice, info.line, info.column }) catch return;
            writeStdout(location);
        }
    }

    const union_member_count: usize = @intCast(info.union_member_count);
    const empty_members = [_][*:0]const u8{};
    const union_members = if (info.union_members) |members_ptr| blk: {
        const members_raw: [*]const [*:0]const u8 = @ptrCast(members_ptr);
        break :blk members_raw[0..union_member_count];
    } else empty_members[0..0];

    const type_slice = if (info.type_name) |ty_ptr|
        std.mem.span(ty_ptr)
    else if (union_members.len > 0)
        std.mem.span(union_members[0])
    else
        "value";

    const active_index_usize: ?usize = if (info.active_member_index >= 0)
        std.math.cast(usize, info.active_member_index) orelse null
    else
        null;

    if (info.name) |name_ptr| {
        const name_slice = std.mem.span(name_ptr);
        if (union_members.len > 1) {
            var prefix_buf: [256]u8 = undefined;
            const prefix = std.fmt.bufPrint(&prefix_buf, "{s} :: ", .{name_slice}) catch return;
            writeStdout(prefix);
            for (union_members, 0..) |member_ptr, idx| {
                if (idx != 0) writeStdout(" | ");
                if (active_index_usize) |active_idx| {
                    if (active_idx == idx) writeStdout(">");
                }
                writeStdout(std.mem.span(member_ptr));
            }
            writeStdout(" is ");
        } else {
            var prefix_buf: [256]u8 = undefined;
            const prefix = std.fmt.bufPrint(&prefix_buf, "{s} :: {s} is ", .{ name_slice, type_slice }) catch return;
            writeStdout(prefix);
        }
    } else {
        if (union_members.len > 1) {
            writeStdout(":: ");
            for (union_members, 0..) |member_ptr, idx| {
                if (idx != 0) writeStdout(" | ");
                if (active_index_usize) |active_idx| {
                    if (active_idx == idx) writeStdout(">");
                }
                writeStdout(std.mem.span(member_ptr));
            }
            writeStdout(" is ");
        } else {
            var prefix_buf: [256]u8 = undefined;
            const prefix = std.fmt.bufPrint(&prefix_buf, ":: {s} is ", .{type_slice}) catch return;
            writeStdout(prefix);
        }
    }
}

/// Convert a C-string to a byte value, mirroring the VM's @byte(string) semantics
/// for the common cases used by compiled code (single characters, decimal, hex,
/// and simple float strings). On invalid input, this returns 0 instead of raising
/// a runtime error, to keep the native code path simple.
pub export fn doxa_byte_from_cstr(ptr: ?[*:0]const u8) callconv(.c) i64 {
    if (ptr == null) return 0;
    const s_val = std.mem.span(ptr.?);

    if (s_val.len == 0) {
        return 0;
    }

    // Single ASCII character
    if (s_val.len == 1) {
        return @as(i64, @intCast(s_val[0]));
    }

    // Hex literal of form "0xFF"
    if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
        const hex_str = s_val[2..];
        const parsed_hex_byte = std.fmt.parseInt(u8, hex_str, 16) catch return 0;
        return @as(i64, @intCast(parsed_hex_byte));
    }

    // Try decimal integer
    const parsed_int_opt: ?i64 = std.fmt.parseInt(i64, s_val, 10) catch null;
    if (parsed_int_opt) |parsed_int| {
        if (parsed_int >= 0 and parsed_int <= 255) {
            return parsed_int;
        }
        return 0;
    }

    // Fallback: parse as float and clamp into byte range
    const parsed_float = std.fmt.parseFloat(f64, s_val) catch return 0;
    if (!std.math.isFinite(parsed_float)) return 0;
    const rounded: i64 = @intFromFloat(parsed_float);
    if (rounded >= 0 and rounded <= 255) {
        return rounded;
    }
    return 0;
}

pub export fn doxa_byte_from_i64(value: i64) callconv(.c) i64 {
    if (value >= 0 and value <= 255) return value;
    return 0;
}

pub export fn doxa_byte_from_f64(value: f64) callconv(.c) i64 {
    if (!std.math.isFinite(value)) return 0;
    const rounded: i64 = @intFromFloat(value);
    if (rounded >= 0 and rounded <= 255) return rounded;
    return 0;
}

pub export fn doxa_substring(s: ?[*:0]const u8, start: i64, length: i64) callconv(.c) ?[*:0]u8 {
    const src = if (s) |p| std.mem.span(p) else "";
    if (start < 0 or length < 0) return doxa_str_clone(null);
    const start_u: usize = @intCast(@as(u64, @intCast(start)));
    const len_u: usize = @intCast(@as(u64, @intCast(length)));
    if (start_u >= src.len) return doxa_str_clone(null);
    const end_unclamped: usize = start_u +| len_u;
    const end_u: usize = if (end_unclamped > src.len) src.len else end_unclamped;
    return allocSentinelString(src[start_u..end_u]);
}

pub export fn doxa_str_pop(s: ?[*:0]const u8, out_remaining: *?[*:0]u8, out_popped: *?[*:0]u8) callconv(.c) u8 {
    const src = if (s) |p| std.mem.span(p) else "";
    if (src.len == 0) {
        out_remaining.* = doxa_str_clone(null);
        out_popped.* = doxa_str_clone(null);
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

    out_remaining.* = allocSentinelString(src[0..last_char_start]);
    out_popped.* = allocSentinelString(src[last_char_start..src.len]);
    return 1;
}

// Return length of a C string (0 if null)
pub export fn doxa_str_len(ptr: ?[*:0]const u8) callconv(.c) i64 {
    if (ptr) |p| {
        return @intCast(std.mem.len(p));
    }
    return 0;
}

pub const ArrayHeader = extern struct {
    data: ?*anyopaque,
    len: u64,
    cap: u64,
    elem_size: u64,
    elem_tag: u64,
};

pub const StructDesc = extern struct {
    type_name: ?[*:0]const u8,
    field_count: u64,
    field_names: ?[*]const ?[*:0]const u8,
    field_tags: ?[*]const u64,
    field_enum_type_names: ?[*]const ?[*:0]const u8,
};

var struct_registry: std.AutoHashMapUnmanaged(usize, *const StructDesc) = .{};

pub export fn doxa_struct_register(instance: ?*anyopaque, desc: ?*const StructDesc) callconv(.c) void {
    const inst = instance orelse return;
    const sd = desc orelse return;
    struct_registry.put(std.heap.page_allocator, @intFromPtr(inst), sd) catch {};
}

pub const EnumDesc = extern struct {
    type_name: ?[*:0]const u8,
    variant_count: u64,
    variant_names: ?[*]const ?[*:0]const u8,
};

var enum_registry: std.AutoHashMapUnmanaged(usize, *const EnumDesc) = .{};

pub export fn doxa_enum_register(desc: ?*const EnumDesc) callconv(.c) void {
    const ed = desc orelse return;
    const tn = ed.type_name orelse return;
    enum_registry.put(std.heap.page_allocator, @intFromPtr(tn), ed) catch {};
}

fn clampMin(a: u64, b: u64) u64 {
    return if (a < b) b else a;
}

/// Allocate an ArrayHeader and backing buffer. elem_tag values mirror the
/// compiler's mapping:
/// 0=int(i64), 1=byte(u8), 2=float(f64), 3=string(i8*), 4=tetra(u8 lower 2 bits),
/// 5=nothing, 6=array(*ArrayHeader), 7=struct(ptr), 8=enum(i64 variant index).
pub export fn doxa_array_new(elem_size: u64, elem_tag: u64, init_len: u64) callconv(.c) *ArrayHeader {
    const cap = clampMin(init_len, 8);
    const hdr_ptr = std.heap.page_allocator.create(ArrayHeader) catch @panic("oom allocating ArrayHeader");
    const data_bytes: usize = @intCast(elem_size * cap);
    var data_ptr: ?*anyopaque = null;
    if (data_bytes != 0) {
        if (elem_size == 8) {
            const slice = std.heap.page_allocator.alloc(i64, @intCast(cap)) catch null;
            if (slice) |s| {
                @memset(s, 0);
                data_ptr = @ptrCast(s.ptr);
            }
        } else {
            const buf = std.heap.page_allocator.alloc(u8, data_bytes) catch null;
            if (buf) |b| {
                @memset(b, 0);
                data_ptr = @ptrCast(b.ptr);
            }
        }
    }
    hdr_ptr.* = ArrayHeader{
        .data = data_ptr,
        .len = init_len,
        .cap = cap,
        .elem_size = elem_size,
        .elem_tag = elem_tag,
    };
    return hdr_ptr;
}

pub export fn doxa_array_len(hdr: *ArrayHeader) callconv(.c) u64 {
    return hdr.len;
}

pub export fn doxa_array_get_i64(hdr: *ArrayHeader, idx: u64) callconv(.c) i64 {
    if (hdr.data == null or idx >= hdr.len) return 0;

    // Treat the backing buffer as raw bytes; decode per element tag to avoid
    // misaligned @alignCast when elem_size is smaller than 8 (bytes/tetras).
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
        3 => blk_str: { // string (i8* -> C string pointer encoded as bits)
            const sp: *const ?[*:0]const u8 = @ptrCast(@alignCast(p));
            const s_ptr = sp.* orelse null;
            const addr: u64 = if (s_ptr) |ptr|
                @intFromPtr(ptr)
            else
                0;
            break :blk_str @bitCast(addr);
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
        // Treat other tags (struct, enum, unknown) as raw 64-bit slots.
        else => blk_raw: {
            const ip: *const i64 = @ptrCast(@alignCast(p));
            break :blk_raw ip.*;
        },
    };
}

pub export fn doxa_array_set_i64(hdr: *ArrayHeader, idx: u64, value: i64) callconv(.c) void {
    if (hdr.data == null) return;
    if (idx >= hdr.cap) return; // no resize in minimal runtime
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
        3 => { // string (i8* -> C string pointer encoded as bits)
            const sp: *?[*:0]const u8 = @ptrCast(@alignCast(p));
            const addr: u64 = @bitCast(value);
            if (addr == 0) {
                sp.* = null;
            } else {
                const src: ?[*:0]const u8 = @ptrFromInt(@as(usize, addr));
                // Clone on write so arrays don't retain pointers to stack temporaries.
                sp.* = doxa_str_clone(src);
            }
        },
        4 => { // tetra (2-bit stored in u8)
            const tp: *u8 = @ptrCast(p);
            tp.* = @intCast(@as(u8, @intCast(value)) & 0x3);
        },
        6 => { // array (*ArrayHeader pointer encoded as bits)
            const ap: *?*ArrayHeader = @ptrCast(@alignCast(p));
            const addr: u64 = @bitCast(value);
            ap.* = if (addr == 0)
                null
            else
                @ptrFromInt(@as(usize, addr));
        },
        // Default: store raw 64-bit payload (pointers/unknown)
        else => {
            const ip: *i64 = @ptrCast(@alignCast(p));
            ip.* = value;
        },
    }
}

/// Concatenate two arrays by allocating a new header and copying elements.
pub export fn doxa_array_concat(a: ?*ArrayHeader, b: ?*ArrayHeader, elem_size: u64, elem_tag: u64) callconv(.c) *ArrayHeader {
    const len_a: u64 = if (a) |hdr| hdr.len else 0;
    const len_b: u64 = if (b) |hdr| hdr.len else 0;
    const result = doxa_array_new(elem_size, elem_tag, len_a + len_b);

    if (a) |hdr_a| {
        var idx: u64 = 0;
        while (idx < len_a) : (idx += 1) {
            const val = doxa_array_get_i64(hdr_a, idx);
            doxa_array_set_i64(result, idx, val);
        }
    }

    if (b) |hdr_b| {
        var idx: u64 = 0;
        while (idx < len_b) : (idx += 1) {
            const val = doxa_array_get_i64(hdr_b, idx);
            doxa_array_set_i64(result, len_a + idx, val);
        }
    }

    return result;
}

/// Clear an array by setting its length to 0
/// For strings, this is a no-op since strings are immutable
pub export fn doxa_clear(collection: ?*anyopaque) callconv(.c) void {
    if (collection == null) return;

    const maybe_hdr = asArrayHeader(collection);
    if (maybe_hdr) |hdr| {
        // It's an array - clear by setting length to 0
        hdr.len = 0;
        return;
    }

    // For strings, this is a no-op (strings are immutable)
    // The compiler should prevent calling clear on strings, but if it happens, just return
}

fn asArrayHeader(ptr: ?*anyopaque) ?*ArrayHeader {
    const p = ptr orelse return null;
    const addr = @intFromPtr(p);
    if (addr % @alignOf(ArrayHeader) != 0) return null;

    const hdr: *ArrayHeader = @ptrCast(@alignCast(p));

    // Validate minimal invariants to avoid treating C strings as ArrayHeaders.
    if (hdr.elem_tag > 8) return null;
    if (!(hdr.elem_size == 0 or hdr.elem_size == 1 or hdr.elem_size == 8)) return null;
    if (hdr.len > hdr.cap) return null;
    if (hdr.elem_size == 0) return hdr;
    if (hdr.cap == 0) return hdr;
    if (hdr.data == null) return null;

    return hdr;
}

// Shared array header printer for compiled programs and the VM.
// This mirrors the VM's array printing, including support for nested arrays,
// and uses a single writer so recursion composes correctly.
pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) callconv(.c) void {
    var buf: [1024]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buf);
    const out = &bw.interface;
    printArrayHdrImpl(out, hdr) catch return;
    _ = out.flush() catch {};
}

fn printTaggedBitsImpl(out: anytype, tag: u64, bits: i64) anyerror!void {
    switch (tag) {
        0 => { // int (i64)
            try out.print("{d}", .{bits});
        },
        1 => { // byte (u8)
            const b: u8 = asByte(bits);
            try out.print("{d}", .{b});
        },
        2 => { // float (f64)
            const f: f64 = asFloat(bits);
            try out.print("{d}", .{f});
        },
        3 => { // string (i8*)
            const s_ptr = cStringFromBits(bits);
            if (s_ptr) |p| {
                const s = std.mem.span(p);
                try out.print("\"{s}\"", .{s});
            } else {
                try out.print("\"\"", .{});
            }
        },
        4 => { // tetra (2-bit stored in u8)
            const t: u2 = asTetra(bits);
            const v: u8 = @intCast(t);
            const name = switch (v) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            };
            try out.print("{s}", .{name});
        },
        5 => { // nothing
            try out.print("nothing", .{});
        },
        6 => { // array (ptr)
            const addr: u64 = @bitCast(bits);
            if (addr == 0) {
                try out.print("[]", .{});
            } else {
                const nested_hdr = @as(
                    *ArrayHeader,
                    @ptrCast(@alignCast(@as(?*anyopaque, @ptrFromInt(@as(usize, addr))))),
                );
                try printArrayHdrImpl(out, nested_hdr);
            }
        },
        7 => { // struct (ptr)
            const addr: u64 = @bitCast(bits);
            if (addr == 0) {
                try out.print("<struct:null>", .{});
            } else {
                try printStructImpl(out, addr);
            }
        },
        8 => { // enum (discriminant)
            try out.print("<enum:{d}>", .{bits});
        },
        else => {
            try out.print("?", .{});
        },
    }
}

fn printEnumImpl(out: anytype, type_name_ptr: ?[*:0]const u8, bits: i64) anyerror!void {
    const tn = type_name_ptr orelse {
        try out.print("<enum:{d}>", .{bits});
        return;
    };
    const desc = enum_registry.get(@intFromPtr(tn)) orelse {
        try out.print("<enum:{d}>", .{bits});
        return;
    };

    const count: usize = @intCast(desc.variant_count);
    const idx_i64 = bits;
    if (idx_i64 < 0) {
        try out.print(".Unknown", .{});
        return;
    }
    const idx: usize = @intCast(@as(u64, @intCast(idx_i64)));
    if (idx >= count) {
        try out.print(".Unknown", .{});
        return;
    }

    if (desc.variant_names) |names_ptr| {
        const names = names_ptr[0..count];
        if (names[idx]) |n| {
            const s = std.mem.span(n);
            try out.print(".{s}", .{s});
            return;
        }
    }
    try out.print(".Unknown", .{});
}

fn printStructImpl(out: anytype, addr: u64) anyerror!void {
    const key: usize = @intCast(addr);
    const desc = struct_registry.get(key) orelse {
        try out.print("<struct@0x{x}>", .{addr});
        return;
    };

    const field_count: usize = @intCast(desc.field_count);
    const names = if (desc.field_names) |p| p[0..field_count] else &[_]?[*:0]const u8{};
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    const enum_type_names = if (desc.field_enum_type_names) |p| p[0..field_count] else &[_]?[*:0]const u8{};
    const fields: [*]const i64 = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(key))));

    try out.print("{{ ", .{});
    var first: bool = true;
    var idx: usize = field_count;
    while (idx > 0) {
        idx -= 1;
        if (!first) try out.print(", ", .{});

        const name_slice: []const u8 = if (idx < names.len) blk: {
            if (names[idx]) |n| break :blk std.mem.span(n);
            break :blk "";
        } else "";

        try out.print("{s}: ", .{name_slice});

        const tag: u64 = if (idx < tags.len) tags[idx] else 255;
        const bits: i64 = fields[idx];
        if (tag == 8 and idx < enum_type_names.len) {
            try printEnumImpl(out, enum_type_names[idx], bits);
        } else {
            try printTaggedBitsImpl(out, tag, bits);
        }
        first = false;
    }
    try out.print(" }}", .{});
}

fn printArrayHdrImpl(out: anytype, hdr: *ArrayHeader) anyerror!void {
    try out.print("[", .{});

    // If the array is logically empty, just print [] and exit.
    if (hdr.len == 0) {
        try out.print("]", .{});
        return;
    }

    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (i != 0) try out.print(", ", .{});
        const elem_bits = doxa_array_get_i64(hdr, i);
        try printTaggedBitsImpl(out, hdr.elem_tag, elem_bits);
    }

    try out.print("]", .{});
}

const QuantifierMode = enum { Greater, Equal };

fn asFloat(bits: i64) f64 {
    return @bitCast(bits);
}

fn asByte(bits: i64) u8 {
    const raw: u64 = @bitCast(bits);
    return @intCast(raw & 0xff);
}

fn asTetra(bits: i64) u2 {
    const raw: u64 = @bitCast(bits);
    return @intCast(raw & 0x3);
}

fn cStringFromBits(bits: i64) ?[*:0]const u8 {
    const addr: u64 = @bitCast(bits);
    if (addr == 0) return null;
    return @ptrFromInt(@as(usize, addr));
}

fn satisfiesQuantifier(tag: u64, elem_bits: i64, comparison_bits: i64, mode: QuantifierMode) bool {
    return switch (tag) {
        0 => if (mode == .Equal) elem_bits == comparison_bits else elem_bits > comparison_bits,
        1 => blk: {
            const lhs = @as(i64, asByte(elem_bits));
            const rhs = @as(i64, asByte(comparison_bits));
            break :blk if (mode == .Equal) lhs == rhs else lhs > rhs;
        },
        2 => blk_float: {
            const lhs = asFloat(elem_bits);
            const rhs = asFloat(comparison_bits);
            break :blk_float if (mode == .Equal) lhs == rhs else lhs > rhs;
        },
        3 => blk_str: {
            if (mode != .Equal) break :blk_str false;
            const lhs_ptr = cStringFromBits(elem_bits) orelse break :blk_str false;
            const rhs_ptr = cStringFromBits(comparison_bits) orelse break :blk_str false;
            break :blk_str std.mem.eql(u8, std.mem.span(lhs_ptr), std.mem.span(rhs_ptr));
        },
        4 => blk_tetra: {
            const lhs = @as(i64, asTetra(elem_bits));
            const rhs = @as(i64, asTetra(comparison_bits));
            break :blk_tetra if (mode == .Equal) lhs == rhs else lhs > rhs;
        },
        else => false,
    };
}

fn existsQuantifier(hdr_opt: ?*ArrayHeader, comparison_bits: i64, mode: QuantifierMode) u8 {
    const hdr = hdr_opt orelse return 0;
    var idx: u64 = 0;
    while (idx < hdr.len) : (idx += 1) {
        const elem_bits = doxa_array_get_i64(hdr, idx);
        if (satisfiesQuantifier(hdr.elem_tag, elem_bits, comparison_bits, mode)) {
            return 1;
        }
    }
    return 0;
}

pub export fn doxa_exists_quantifier_gt(hdr: ?*ArrayHeader, comparison_bits: i64) callconv(.c) u8 {
    return existsQuantifier(hdr, comparison_bits, .Greater);
}

pub export fn doxa_exists_quantifier_eq(hdr: ?*ArrayHeader, comparison_bits: i64) callconv(.c) u8 {
    return existsQuantifier(hdr, comparison_bits, .Equal);
}

fn forallQuantifier(hdr_opt: ?*ArrayHeader, comparison_bits: i64, mode: QuantifierMode) u8 {
    const hdr = hdr_opt orelse return 1;
    var idx: u64 = 0;
    while (idx < hdr.len) : (idx += 1) {
        const elem_bits = doxa_array_get_i64(hdr, idx);
        if (!satisfiesQuantifier(hdr.elem_tag, elem_bits, comparison_bits, mode)) {
            return 0;
        }
    }
    return 1;
}

pub export fn doxa_forall_quantifier_gt(hdr: ?*ArrayHeader, comparison_bits: i64) callconv(.c) u8 {
    return forallQuantifier(hdr, comparison_bits, .Greater);
}

pub export fn doxa_forall_quantifier_eq(hdr: ?*ArrayHeader, comparison_bits: i64) callconv(.c) u8 {
    return forallQuantifier(hdr, comparison_bits, .Equal);
}

/// Canonical runtime representation for Doxa values.
/// This is the single, shared value model for the interpreter, native
/// (LLVM) backend, and any C callers. The layout must stay in sync
/// with `%DoxaValue` in `src/codegen/llvmir/ir_printer.zig`.
///
/// Tag values:
///   0 = Int      (payload_bits = i64)
///   1 = Float    (payload_bits = bitcast f64)
///   2 = Byte     (payload_bits[0..8] = u8)
///   3 = String   (payload_bits = pointer to string storage / C-string)
///   4 = Array    (payload_bits = *ArrayHeader as bits)
///   5 = Struct   (payload_bits = pointer to struct instance)
///   6 = Enum     (payload_bits = i64 variant index; type via metadata)
///   7 = Tetra    (payload_bits[0..2] = u2)
///   8 = Nothing  (payload_bits ignored)
///   9 = Function (payload_bits = pointer to function closure/descriptor)
///  10 = Map      (payload_bits = *MapHeader as bits)
///
/// Boxing rules:
///   - Unboxed: Int, Float, Byte, Tetra, Enum discriminant
///   - Boxed:   String, Array, Struct, Function, Map
///   - Sentinel: Nothing (tag = 8, payload_bits = 0)
///
/// Union encoding:
///   - For non-union values, `reserved == 0`.
///   - For union values, `reserved` packs:
///       bit 31        : is_union flag
///       bits 16..30   : union_id (up to 32k unions)
///       bits 0..15    : active_member_index (0-based, up to 65k members)
///   - `tag` always describes the active payload kind (Int, Float, Struct, …).
pub const DoxaValue = extern struct {
    /// Discriminant for the active variant / type.
    /// See `DoxaTag` for the full set of tags.
    tag: u32,
    /// Packed flags and sub-tags. See `DoxaUnionMeta` helpers for how this is
    /// used to encode union membership.
    reserved: u32,
    /// Payload bits. For small types, stores the value directly. For heap-backed
    /// types, stores a pointer as bits.
    payload_bits: i64,
};

/// High-level tag enumeration for `DoxaValue.tag`. The numeric values are part
/// of the ABI and must stay in sync with the documentation above.
pub const DoxaTag = enum(u32) {
    Int = 0,
    Float = 1,
    Byte = 2,
    String = 3,
    Array = 4,
    Struct = 5,
    Enum = 6,
    Tetra = 7,
    Nothing = 8,
    Function = 9,
    Map = 10,
};

/// Helpers for encoding and decoding union metadata into the `reserved` field
/// of a `DoxaValue`. This keeps union representation consistent across the
/// interpreter, runtime helpers, and native code.
pub const DoxaUnionMeta = struct {
    pub const is_union_bit: u32 = 1 << 31;
    pub const union_id_shift: u5 = 16;
    pub const union_id_mask: u32 = 0x7FFF << union_id_shift; // 15 bits
    pub const member_index_mask: u32 = 0xFFFF; // 16 bits

    pub fn pack(union_id: u32, member_index: u32) u32 {
        const uid: u32 = union_id & 0x7FFF;
        const mid: u32 = member_index & member_index_mask;
        return is_union_bit | (uid << union_id_shift) | mid;
    }

    pub fn isUnion(reserved: u32) bool {
        return (reserved & is_union_bit) != 0;
    }

    pub fn unionId(reserved: u32) u32 {
        return (reserved & union_id_mask) >> union_id_shift;
    }

    pub fn memberIndex(reserved: u32) u32 {
        return reserved & member_index_mask;
    }
};

/// Print a value described by the canonical `DoxaValueC` layout. This is the
/// primary entry point for native code that wants to render values without
/// going through the interpreter.
pub export fn doxa_print_value(val: *const DoxaValueC) callconv(.c) void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .Int => {
            doxa_print_i64(val.payload_bits);
        },
        .Float => {
            const f = asFloat(val.payload_bits);
            doxa_print_f64(f);
        },
        .Byte => {
            const b: i64 = @intCast(asByte(val.payload_bits));
            doxa_print_byte(b);
        },
        .String => {
            const s_ptr = cStringFromBits(val.payload_bits);
            doxa_peek_string(s_ptr);
        },
        .Array => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr == 0) {
                writeStdout("[]");
            } else {
                const any_ptr: ?*anyopaque = @ptrFromInt(@as(usize, addr));
                const hdr = @as(*ArrayHeader, @ptrCast(@alignCast(any_ptr.?)));
                doxa_print_array_hdr(hdr);
            }
        },
        .Struct => {
            var buf: [1024]u8 = undefined;
            var bw = std.fs.File.stdout().writer(&buf);
            const out = &bw.interface;
            const addr: u64 = @bitCast(val.payload_bits);
            printTaggedBitsImpl(out, 7, @as(i64, @bitCast(addr))) catch return;
            _ = out.flush() catch {};
        },
        .Enum => {
            // Enum printing is now handled natively by the IR printer
            writeStdout("<enum>");
        },
        .Tetra => {
            const t: u2 = asTetra(val.payload_bits);
            const v: u8 = @intCast(t);
            const name = switch (v) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            };
            writeStdout(name);
        },
        .Nothing => {
            // Nothing has no textual payload; render as "nothing" for now.
            writeStdout("nothing");
        },
        .Function => {
            writeStdout("<function>");
        },
        .Map => {
            writeStdout("<map>");
        },
    }
}

/// Type checking for union types and `as` expressions.
/// Returns 1 (true) if the value matches the target type, 0 (false) otherwise.
///
/// Legacy ABI used by generated LLVM:
///   - `value`      : payload bits (i64), interpreted according to `value_type`
///   - `value_type` : legacy tag (0=int, 1=float, 2=byte, 3=string, 4=array,
///                    5=struct, 6=enum, 7=tetra/bool, 8=nothing)
///   - `target_type`: C string with a type name like "int", "string", "int[]", …
///
/// This is now implemented as a thin shim that constructs a canonical
/// `DoxaValue` and delegates to `doxa_type_check_value`. New code should
/// prefer calling `doxa_type_check_value` directly.
pub export fn doxa_type_check(value: i64, value_type: i64, target_type: ?[*:0]const u8) callconv(.c) i64 {
    // Map legacy `value_type` to canonical tag. Values outside the known range
    // are treated as `Nothing` to keep behaviour predictable.
    const tag_raw: u32 = switch (value_type) {
        0 => @intFromEnum(DoxaTag.Int),
        1 => @intFromEnum(DoxaTag.Float),
        2 => @intFromEnum(DoxaTag.Byte),
        3 => @intFromEnum(DoxaTag.String),
        4 => @intFromEnum(DoxaTag.Array),
        5 => @intFromEnum(DoxaTag.Struct),
        6 => @intFromEnum(DoxaTag.Enum),
        7 => @intFromEnum(DoxaTag.Tetra),
        8 => @intFromEnum(DoxaTag.Nothing),
        else => @intFromEnum(DoxaTag.Nothing),
    };

    const val = DoxaValueC{
        .tag = tag_raw,
        .reserved = 0, // legacy path has no union metadata
        .payload_bits = value,
    };

    return doxa_type_check_value(val, target_type);
}

/// New entry point that works directly with the canonical value representation.
/// This will eventually replace `doxa_type_check` in generated LLVM IR.
pub export fn doxa_type_check_value(val: DoxaValueC, target_type: ?[*:0]const u8) callconv(.c) i64 {
    if (target_type == null) return 0;

    const target = std.mem.span(target_type.?);

    const tag: DoxaTag = @enumFromInt(val.tag);

    const actual_type: []const u8 = switch (tag) {
        .Int => "int",
        .Float => "float",
        .Byte => "byte",
        .String => "string",
        .Array => "array",
        .Struct => "struct",
        .Enum => "enum",
        .Tetra => "tetra",
        .Nothing => "nothing",
        .Function => "function",
        .Map => "map",
    };

    if (std.mem.eql(u8, actual_type, target)) {
        return 1;
    }

    if (std.mem.endsWith(u8, target, "[]")) {
        if (tag == .Array) {
            return 1;
        }
    }

    return 0;
}
