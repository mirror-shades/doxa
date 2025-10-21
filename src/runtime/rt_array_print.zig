const std = @import("std");

// Minimal runtime helper for printing array headers from LLVM-generated code.
// This file is intentionally self-contained (no project-internal imports)
// so it can be built as a standalone static library or object.

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

// Allocate an ArrayHeader and backing buffer. elem_tag values mirror the compiler's mapping:
// 0=int(i64), 1=byte(u8), 2=float(f64), 3=string(i8*), 4=tetra(u8 lower 2 bits)
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
