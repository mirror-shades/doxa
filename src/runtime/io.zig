//! The runtime's output sink: stdout, the peek channel on stderr, and the raw
//! write entry points codegen emits.

const std = @import("std");
const builtin = @import("builtin");
const win32 = @import("win32.zig");
const fault = @import("fault.zig");
const abi = @import("abi.zig");
const writeStderr = fault.writeStderr;
const writeWasiFd = fault.writeWasiFd;

var peek_output_active: bool = false;

pub fn doxaWrite(slice: []const u8) void {
    if (peek_output_active) {
        writeStderr(slice);
    } else {
        writeStdout(slice);
    }
}

/// Close an active peek record. The IR printer emits this after the value of
/// every `?` peek, instead of routing the terminating newline through
/// `doxaWrite`: a peeked string may itself contain newlines, and inferring the
/// end of the record from written content would truncate the record onto
/// stdout mid-value.
pub export fn doxa_peek_end() callconv(.c) void {
    if (!peek_output_active) return;
    writeStderr("\n");
    peek_output_active = false;
}

fn writeStdout(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const handle = win32.GetStdHandle(win32.STD_OUTPUT_HANDLE);
        if (handle == std.os.windows.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win32.WriteFile(handle, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    if (builtin.os.tag == .wasi) {
        writeWasiFd(1, slice);
        return;
    }
    // Streaming, not positional: a fresh positional writer starts at offset 0
    // and, with stdout redirected to a file, overwrites earlier output (ours or
    // a child process's sharing the handle) instead of appending.
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writerStreaming(std.Io.Threaded.global_single_threaded.io(), &stdout_buffer);
    const stdout = &stdout_writer.interface;
    _ = stdout.write(slice) catch return;
    _ = stdout.flush() catch return;
}

pub export fn doxa_write_cstr(ptr: ?[*]const u8, len: u64) callconv(.c) void {
    if (ptr) |p| {
        doxaWrite(p[0..@intCast(len)]);
    }
}

/// Write a null-terminated string to the active output sink. Used for
/// user-provided error messages and C-string values where the
/// length is not known statically.
pub export fn doxa_write_raw(ptr: ?[*:0]const u8) callconv(.c) void {
    if (ptr) |p| {
        doxaWrite(std.mem.span(p));
    }
}

/// Write `slice` with control characters and `"`/`\` escaped. Peek values are
/// rendered inside quotes on a single line, so a string containing `\n`, `\r`,
/// or `\t` (common for captured subprocess output on Windows) must not spill
/// the record across lines.
pub fn writeEscaped(out: *std.Io.Writer, slice: []const u8) !void {
    const hex_digits = "0123456789abcdef";
    var start: usize = 0;
    for (slice, 0..) |byte, i| {
        const escape: ?[]const u8 = switch (byte) {
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            '\\' => "\\\\",
            '"' => "\\\"",
            else => null,
        };
        if (escape == null and byte >= 0x20) continue;
        if (i > start) try out.writeAll(slice[start..i]);
        if (escape) |esc| {
            try out.writeAll(esc);
        } else {
            const buf = [4]u8{ '\\', 'x', hex_digits[byte >> 4], hex_digits[byte & 0x0f] };
            try out.writeAll(&buf);
        }
        start = i + 1;
    }
    if (start < slice.len) try out.writeAll(slice[start..]);
}

pub export fn doxa_peek_string(ptr: ?[*]const u8, len: u64) callconv(.c) void {
    doxaWrite("\"");
    if (ptr) |p| {
        if (len > 0) {
            var escaped = std.Io.Writer.Allocating.init(std.heap.page_allocator);
            defer escaped.deinit();
            writeEscaped(&escaped.writer, p[0..@intCast(len)]) catch {};
            doxaWrite(escaped.written());
        }
    }
    doxaWrite("\"");
}

pub export fn doxa_debug_peek(info_ptr: ?*const abi.DoxaPeekInfo) callconv(.c) void {
    const info = info_ptr orelse return;
    peek_output_active = true;

    if (info.has_location != 0) {
        if (info.file) |file_ptr| {
            const file_slice = std.mem.span(file_ptr);
            var buf: [256]u8 = undefined;
            const location = std.fmt.bufPrint(&buf, "[{s}:{d}:{d}] ", .{ file_slice, info.line, info.column }) catch {
                peek_output_active = false;
                return;
            };
            writeStderr(location);
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
            writeStderr(prefix);
            for (union_members, 0..) |member_ptr, idx| {
                if (idx != 0) writeStderr(" | ");
                if (active_index_usize) |active_idx| {
                    if (active_idx == idx) writeStderr(">");
                }
                writeStderr(std.mem.span(member_ptr));
            }
            writeStderr(" is ");
        } else {
            var prefix_buf: [256]u8 = undefined;
            const prefix = std.fmt.bufPrint(&prefix_buf, "{s} :: {s} is ", .{ name_slice, type_slice }) catch return;
            writeStderr(prefix);
        }
    } else {
        if (union_members.len > 1) {
            writeStderr(":: ");
            for (union_members, 0..) |member_ptr, idx| {
                if (idx != 0) writeStderr(" | ");
                if (active_index_usize) |active_idx| {
                    if (active_idx == idx) writeStderr(">");
                }
                writeStderr(std.mem.span(member_ptr));
            }
            writeStderr(" is ");
        } else {
            var prefix_buf: [256]u8 = undefined;
            const prefix = std.fmt.bufPrint(&prefix_buf, ":: {s} is ", .{type_slice}) catch return;
            writeStderr(prefix);
        }
    }
}
