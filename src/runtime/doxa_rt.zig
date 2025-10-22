const std = @import("std");

//------------------------------------------------------------------------------
// DOXA Runtime Support
//
// This module consolidates the exported helpers that the generated code
// expects to link against.  It currently exposes the legacy array helpers and
// serves as the place where future debug/peek/print shims will live.
//------------------------------------------------------------------------------

fn writeStdout(slice: []const u8) void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    _ = stdout.writeAll(slice) catch return;
    _ = stdout.flush() catch return;
}

pub export fn doxa_write_cstr(ptr: ?[*:0]const u8) void {
    if (ptr) |p| {
        const slice = std.mem.span(p);
        writeStdout(slice);
    }
}

pub export fn doxa_print_i64(value: i64) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_u64(value: u64) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
}

pub export fn doxa_print_f64(value: f64) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    writeStdout(rendered);
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

pub export fn doxa_debug_peek(info_ptr: ?*const DoxaPeekInfo) void {
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
pub export fn doxa_array_new(elem_size: u64, elem_tag: u64, init_len: u64) *ArrayHeader {
    const cap = clampMin(init_len, 8);
    const hdr_ptr = std.heap.page_allocator.create(ArrayHeader) catch @panic("oom allocating ArrayHeader");
    const data_bytes: usize = @intCast(elem_size * cap);
    const data_buf: ?[]u8 = if (data_bytes == 0) null else std.heap.page_allocator.alloc(u8, data_bytes) catch null;
    const data_ptr: ?*anyopaque = if (data_buf) |buf| @ptrCast(buf.ptr) else null;
    hdr_ptr.* = ArrayHeader{
        .data = data_ptr,
        .len = init_len,
        .cap = cap,
        .elem_size = elem_size,
        .elem_tag = elem_tag,
    };
    if (data_buf) |buf| @memset(buf, 0);
    return hdr_ptr;
}

pub export fn doxa_array_len(hdr: *ArrayHeader) u64 {
    return hdr.len;
}

pub export fn doxa_array_get_i64(hdr: *ArrayHeader, idx: u64) i64 {
    if (hdr.data == null or idx >= hdr.len) return 0;
    const base: [*]const u8 = @ptrCast(@alignCast(hdr.data.?));
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ip: *const i64 = @ptrCast(@alignCast(p));
    return ip.*;
}

pub export fn doxa_array_set_i64(hdr: *ArrayHeader, idx: u64, value: i64) void {
    if (hdr.data == null) return;
    if (idx >= hdr.cap) return; // no resize in minimal runtime
    if (idx >= hdr.len) hdr.len = idx + 1;
    const base: [*]u8 = @ptrCast(@alignCast(hdr.data.?));
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ip: *i64 = @ptrCast(@alignCast(p));
    ip.* = value;
}

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) void {
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
