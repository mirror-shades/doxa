const std = @import("std");
const builtin = @import("builtin");

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

pub export fn doxa_str_eq(a: ?[*:0]const u8, b: ?[*:0]const u8) callconv(.c) bool {
    if (a == null or b == null) return false;
    const as = std.mem.span(a.?);
    const bs = std.mem.span(b.?);
    return std.mem.eql(u8, as, bs);
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
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_enum(type_name: ?[*:0]const u8, variant_index: i64) callconv(.c) void {
    if (type_name) |tn| {
        const type_str = std.mem.span(tn);
        const variant_name = getEnumVariantName(type_str, variant_index);
        var buf: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&buf, ".{s}", .{variant_name}) catch return;
        writeStdout(rendered);
    } else {
        // Fallback: use generic variant names when type name is not available
        const variant_name = getEnumVariantName("", variant_index);
        var buf: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&buf, ".{s}", .{variant_name}) catch return;
        writeStdout(rendered);
    }
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

fn clampMin(a: u64, b: u64) u64 {
    return if (a < b) b else a;
}

/// Allocate an ArrayHeader and backing buffer. elem_tag values mirror the
/// compiler's mapping:
/// 0=int(i64), 1=byte(u8), 2=float(f64), 3=string(i8*), 4=tetra(u8 lower 2 bits)
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
    const base: [*]const u8 = @ptrCast(@alignCast(hdr.data.?));
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ip: *const i64 = @ptrCast(@alignCast(p));
    return ip.*;
}

pub export fn doxa_array_set_i64(hdr: *ArrayHeader, idx: u64, value: i64) callconv(.c) void {
    if (hdr.data == null) return;
    if (idx >= hdr.cap) return; // no resize in minimal runtime
    if (idx >= hdr.len) hdr.len = idx + 1;
    const base: [*]u8 = @ptrCast(@alignCast(hdr.data.?));
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ip: *i64 = @ptrCast(@alignCast(p));
    ip.* = value;
}

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) callconv(.c) void {
    var out_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&out_buf);
    const out = &stdout_writer.interface;
    _ = out.writeAll("[") catch return;

    if (hdr.data == null or hdr.len == 0) {
        _ = out.writeAll("]") catch return;
        return;
    }

    const base: [*]const u8 = @ptrCast(@alignCast(hdr.data.?));
    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (i != 0) _ = out.writeAll(", ") catch return;
        const off: usize = @intCast(i * hdr.elem_size);
        const p = base + off;
        switch (hdr.elem_tag) {
            0 => { // int (i64)
                const ip: *const i64 = @ptrCast(@alignCast(p));
                var tmp: [64]u8 = undefined;
                const s = std.fmt.bufPrint(&tmp, "{d}", .{ip.*}) catch return;
                _ = out.writeAll(s) catch return;
            },
            1 => { // byte (u8)
                const bp: *const u8 = @ptrCast(p);
                var tmp: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&tmp, "{d}", .{bp.*}) catch return;
                _ = out.writeAll(s) catch return;
            },
            2 => { // float (f64)
                const fp: *const f64 = @ptrCast(@alignCast(p));
                var tmp: [64]u8 = undefined;
                const s = std.fmt.bufPrint(&tmp, "{d}", .{fp.*}) catch return;
                _ = out.writeAll(s) catch return;
            },
            3 => { // string (i8*)
                const sp: *const ?[*:0]const u8 = @ptrCast(@alignCast(p));
                const s = sp.* orelse "";
                _ = out.writeAll(std.mem.span(s)) catch return;
            },
            4 => { // tetra (2-bit stored in u8)
                const tp: *const u8 = @ptrCast(p);
                const v: u8 = tp.* & 0x3;
                const name = switch (v) {
                    0 => "false",
                    1 => "true",
                    2 => "both",
                    3 => "neither",
                    else => "invalid",
                };
                _ = out.writeAll(name) catch return;
            },
            else => {
                _ = out.writeAll("?") catch return;
            },
        }
    }
    _ = out.writeAll("]") catch return;
    _ = out.flush() catch return;
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
