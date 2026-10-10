const std = @import("std");
const builtin = @import("builtin");
const MapRuntime = @import("map_runtime.zig");
const scope_arena = @import("scope_arena.zig");
const win32 = @import("win32.zig");

const Scope = scope_arena.Scope;

/// The program root scope (`@root`), where globals live. `main` takes it once;
/// it is never exited.
pub export fn doxa_scope_root() callconv(.c) *Scope {
    return scope_arena.root();
}

/// Open a child of `parent`: `scope.enter` of the register HIR.
pub export fn doxa_scope_enter(parent: *Scope) callconv(.c) *Scope {
    return scope_arena.enter(parent);
}

/// Close `scope`, freeing its arena in O(1): `scope.exit`.
pub export fn doxa_scope_exit(scope: *Scope) callconv(.c) void {
    scope_arena.exit(scope);
}

/// Reclaim `scope`'s allocations and keep it open for reuse: `scope.reset`,
/// a loop body's arena at each iteration.
pub export fn doxa_scope_reset(scope: *Scope) callconv(.c) void {
    scope_arena.reset(scope);
}

/// Allocate `size` bytes in `scope`.
pub export fn doxa_scope_alloc(scope: *Scope, size: i64, alignment: i64) callconv(.c) *anyopaque {
    const align_val: u29 = if (alignment > 0) @intCast(alignment) else @alignOf(u64);
    return @ptrCast(scope_arena.alloc(scope, @intCast(size), .fromByteUnits(align_val), @returnAddress()));
}

var peek_output_active: bool = false;

fn doxaWrite(slice: []const u8) void {
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

/// Write a slice to a raw WASI file descriptor. `std.Io.Threaded`'s syscall
/// path is not usable under wasi preview1 (writes fail with ENOTCAPABLE), so —
/// like the Windows branch — the runtime uses the platform's direct write call.
fn writeWasiFd(fd: i32, slice: []const u8) void {
    if (slice.len == 0) return;
    var iov = [1]std.os.wasi.ciovec_t{.{ .base = slice.ptr, .len = slice.len }};
    var written: usize = 0;
    _ = std.os.wasi.fd_write(fd, &iov, 1, &written);
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
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(std.Io.Threaded.global_single_threaded.io(), &stdout_buffer);
    const stdout = &stdout_writer.interface;
    _ = stdout.write(slice) catch return;
    _ = stdout.flush() catch return;
}

fn writeStderr(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const handle = win32.GetStdHandle(win32.STD_ERROR_HANDLE);
        if (handle == std.os.windows.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win32.WriteFile(handle, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    if (builtin.os.tag == .wasi) {
        writeWasiFd(2, slice);
        return;
    }
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writer(std.Io.Threaded.global_single_threaded.io(), &stderr_buffer);
    const stderr = &stderr_writer.interface;
    _ = stderr.write(slice) catch return;
    _ = stderr.flush() catch return;
}

pub export fn doxa_trap_unreachable() callconv(.c) void {
    writeStderr("Reached unreachable code\n");
    std.process.exit(2);
}

/// Write a byte slice to stderr. Used by assertion failures and other
/// diagnostics that must not pollute stdout.
pub export fn doxa_write_stderr(ptr: ?[*]const u8, len: u64) callconv(.c) void {
    if (ptr) |p| {
        writeStderr(p[0..@intCast(len)]);
    }
}

/// Terminate the process with the given status code. Declared `noreturn` so the
/// optimizer treats every `@exit` site as a genuine control-flow end.
pub export fn doxa_exit(code: i64) callconv(.c) void {
    std.process.exit(@intCast(code));
}

/// Integer division or modulo by zero. LLVM's `sdiv`/`srem`/`udiv`/`urem` are
/// undefined behaviour on a zero divisor and the optimizer duly folds them to
/// junk, so the lowering guards every runtime divisor and lands here. Declared
/// `noreturn` so the optimizer treats the guard's taken arm as a genuine
/// control-flow end rather than continuing into the division.
pub export fn doxa_trap_div_by_zero() callconv(.c) void {
    writeStderr("Division by zero\n");
    std.process.exit(1);
}

/// User-facing panic: write the message to stderr and terminate with code 1.
/// Code 2 is reserved for compiler-internal traps (`doxa_trap_unreachable`).
pub export fn doxa_panic(ptr: ?[*]const u8, len: u64) callconv(.c) void {
    if (ptr) |p| {
        writeStderr(p[0..@intCast(len)]);
    }
    writeStderr("\n");
    std.process.exit(1);
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
fn writeEscaped(out: *std.Io.Writer, slice: []const u8) !void {
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

/// Canonical string representation: pointer + byte length.
///
/// This is the internal model for every layer (parser, HIR, LLVM IR).
/// It matches Zig's `[]const u8` slice semantics: valid indices are `0..len`,
/// embedded U+0000 is permitted, and length is O(1) without scanning.
///
/// Important: this struct cannot cross a `callconv(.c)` boundary by value.
/// On Windows x64 the 16-byte aggregate disagrees between sret (Zig's
/// expectation for `callconv(.c)`) and RAX:RDX register return (LLVM's
/// lowering). All exported functions therefore use explicit `(ptr, len)`
/// pairs and out-parameter returns. Only internal Zig↔Zig calls may pass
/// DoxaString by value.
pub const DoxaString = extern struct {
    ptr: ?[*]const u8,
    len: u64,
};

fn allocDoxaString(scope: *Scope, bytes: []const u8) DoxaString {
    if (bytes.len == 0) return .{ .ptr = null, .len = 0 };
    const buf = scope_arena.allocSlice(scope, u8, bytes.len);
    @memcpy(buf, bytes);
    return .{ .ptr = buf.ptr, .len = bytes.len };
}

fn sliceFromDoxaString(s: DoxaString) []const u8 {
    if (s.ptr) |p| return p[0..@intCast(s.len)];
    return "";
}

fn ds_ptr(s: DoxaString) ?[*]const u8 {
    return s.ptr;
}
fn ds_len(s: DoxaString) u64 {
    return s.len;
}
fn ds_from_parts(ptr: ?[*]const u8, len: u64) DoxaString {
    return .{ .ptr = ptr, .len = len };
}

var startup_argc: i32 = 0;
var startup_argv: ?[*][*:0]u8 = null;
var startup_environ: ?*const std.process.Environ.Map = null;

pub export fn doxa_set_args(argc: i32, argv: ?[*][*:0]u8) callconv(.c) void {
    startup_argc = argc;
    startup_argv = argv;
}

pub export fn doxa_argc() callconv(.c) i32 {
    return startup_argc;
}

pub export fn doxa_argv(index: i32) callconv(.c) ?[*:0]const u8 {
    if (index < 0 or index >= startup_argc) return null;
    const argv = startup_argv orelse return null;
    return argv[@intCast(index)];
}

pub export fn doxa_set_environ(env: ?*const std.process.Environ.Map) callconv(.c) void {
    startup_environ = env;
}

pub export fn doxa_environ() callconv(.c) ?*const std.process.Environ.Map {
    return startup_environ;
}

pub export fn doxa_getenv(name_ptr: ?[*]const u8, name_len: u64, out_len: *u64) callconv(.c) ?[*]const u8 {
    const env = startup_environ orelse return null;
    const name = if (name_ptr) |p| p[0..@intCast(name_len)] else return null;
    const value = env.get(name) orelse return null;
    out_len.* = value.len;
    return value.ptr;
}

pub export fn doxa_int_from_string(ptr: ?[*]const u8, len: u64) callconv(.c) i64 {
    const raw = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    const trimmed = std.mem.trim(u8, raw, " \t\r\n");
    if (trimmed.len == 0) return 0;

    const is_neg = trimmed[0] == '-';
    const hex_start: usize = if (is_neg) 1 else 0;
    if (trimmed.len >= hex_start + 2 and trimmed[hex_start] == '0' and (trimmed[hex_start + 1] == 'x' or trimmed[hex_start + 1] == 'X')) {
        const digits = trimmed[hex_start + 2 ..];
        const parsed = std.fmt.parseInt(i64, digits, 16) catch return 0;
        return if (is_neg) -parsed else parsed;
    }

    if (std.mem.indexOfScalar(u8, trimmed, '.') != null) {
        const f = std.fmt.parseFloat(f64, trimmed) catch return 0;
        return @intFromFloat(f);
    }

    return std.fmt.parseInt(i64, trimmed, 10) catch 0;
}

pub export fn doxa_float_from_string(ptr: ?[*]const u8, len: u64) callconv(.c) f64 {
    const raw = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    const trimmed = std.mem.trim(u8, raw, " \t\r\n");
    if (trimmed.len == 0) return 0.0;

    const is_neg = trimmed[0] == '-';
    const hex_start: usize = if (is_neg) 1 else 0;
    if (trimmed.len >= hex_start + 2 and trimmed[hex_start] == '0' and (trimmed[hex_start + 1] == 'x' or trimmed[hex_start + 1] == 'X')) {
        const digits = trimmed[hex_start + 2 ..];
        const parsed = std.fmt.parseInt(i64, digits, 16) catch return 0.0;
        const signed = if (is_neg) -parsed else parsed;
        return @floatFromInt(signed);
    }

    return std.fmt.parseFloat(f64, trimmed) catch 0.0;
}

pub export fn doxa_int_to_string(scope: *Scope, value: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{value}) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(scope, s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

/// The longest text `formatFloat` produces: a sign, "0.", the 323 zeros
/// before the smallest subnormal's first digit, and 17 significant digits.
/// The longest integral value (309 digits and ".0") fits well beneath it.
const max_float_text_len = 1 + 2 + 323 + 17;

/// A float's text: the shortest positional decimal that reads back as the same
/// value, with ".0" on an integral one so a float never reads as an int.
fn formatFloat(buf: *[max_float_text_len]u8, value: f64) []const u8 {
    const integral = value - std.math.floor(value) == 0;
    const rendered = if (integral)
        std.fmt.bufPrint(buf, "{d}.0", .{value})
    else
        std.fmt.bufPrint(buf, "{d}", .{value});
    return rendered catch unreachable;
}

pub export fn doxa_float_to_string(scope: *Scope, value: f64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var buf: [max_float_text_len]u8 = undefined;
    const ds = allocDoxaString(scope, formatFloat(&buf, value));
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_byte_to_string(scope: *Scope, value: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var buf: [16]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const s = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(scope, s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_tetra_to_string(scope: *Scope, value: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const v: u8 = @intCast(value & 0x3);
    const name: []const u8 = switch (v) {
        0 => "false",
        1 => "true",
        2 => "both",
        3 => "neither",
        else => "invalid",
    };
    const ds = allocDoxaString(scope, name);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_nothing_to_string(scope: *Scope, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const ds = allocDoxaString(scope, "nothing");
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_enum_to_string(scope: *Scope, type_name_ptr: ?[*]const u8, type_name_len: u64, bits: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    printEnumImpl(&list.writer, sliceFromDoxaString(.{ .ptr = type_name_ptr, .len = type_name_len }), bits) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(scope, list.written());
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_struct_to_string(scope: *Scope, instance: ?*anyopaque, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (instance == null) {
        const ds = allocDoxaString(scope, "");
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
        return;
    }
    const addr: u64 = @intFromPtr(instance.?);
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    printStructImpl(&list.writer, addr) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(scope, list.written());
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_array_to_string(scope: *Scope, hdr: ?*ArrayHeader, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (hdr == null) {
        const ds = allocDoxaString(scope, "");
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
        return;
    }
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    printArrayHdrImpl(&list.writer, hdr.?) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(scope, list.written());
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

/// Render a boxed `DoxaValue` as a string in `scope`, dispatching on its
/// runtime tag: the buffer-returning half of `doxa_print_value`. Only the
/// `Int` tag stores an integer in `payload_bits`, so reading it without first
/// asking the tag would format whatever the payload happens to hold — a string
/// arm would be rendered as the address of its characters.
pub export fn doxa_value_to_string(scope: *Scope, val: *const DoxaValue, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    writeValue(&list.writer, val) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    outString(scope, list.written(), out_ptr, out_len);
}

/// Write a box's text: what `@string` of it yields.
fn writeValue(out: *std.Io.Writer, val: *const DoxaValue) anyerror!void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .Int => try out.print("{d}", .{val.payload_bits}),
        .Float => {
            var buf: [max_float_text_len]u8 = undefined;
            try out.writeAll(formatFloat(&buf, asFloat(val.payload_bits)));
        },
        .Byte => try out.print("0x{X:0>2}", .{asByte(val.payload_bits)}),
        .Tetra => try out.writeAll(tetraName(asTetra(val.payload_bits))),
        .Nothing => try out.writeAll("nothing"),
        .String => try out.writeAll(stringPayload(val)),
        .Array => if (payloadAs(val, ArrayHeader)) |hdr| try printArrayHdrImpl(out, hdr),
        .Struct => if (payloadAs(val, anyopaque)) |instance| try printStructImpl(out, @intFromPtr(instance)),
        // A boxed enum is named through the box registry; a bare
        // discriminant the IR printer could not name shows its number.
        .Enum => if (!try writeBoxedEnum(out, val)) try printEnumImpl(out, "", val.payload_bits),
        .Function => try out.writeAll("<function>"),
        .Map => try out.writeAll("<map>"),
    }
}

fn tetraName(t: u2) []const u8 {
    return switch (t) {
        0 => "false",
        1 => "true",
        2 => "both",
        3 => "neither",
    };
}

/// The character buffer a `String`-tagged payload points at, or an empty slice
/// for the null sentinel.
fn stringPayload(val: *const DoxaValue) []const u8 {
    const addr: u64 = @bitCast(val.payload_bits);
    if (addr == 0) return "";
    const ptr: [*]const u8 = @ptrFromInt(@as(usize, @intCast(addr)));
    return ptr[0..@intCast(val.payload_len)];
}

/// The heap object a payload address names, or null for the 0 sentinel.
fn payloadAs(val: *const DoxaValue, comptime T: type) ?*T {
    const addr: u64 = @bitCast(val.payload_bits);
    if (addr == 0) return null;
    const any_ptr: *anyopaque = @ptrFromInt(@as(usize, @intCast(addr)));
    return @ptrCast(@alignCast(any_ptr));
}

fn outString(scope: *Scope, bytes: []const u8, out_ptr: *?[*]u8, out_len: *u64) void {
    const ds = allocDoxaString(scope, bytes);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
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
    const bytes = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    const result = doxa_array_new(scope, 1, 1, bytes.len);
    for (bytes, 0..) |ch, i| {
        doxa_array_set_i64(result, i, ch);
    }
    return result;
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

/// Recover a DoxaString from a null-terminated C-string pointer. This is the
/// one remaining raw C-string boundary: map string values still arrive as
/// C-strings.
pub export fn doxa_str_from_cstr(scope: *Scope, ptr: ?[*:0]const u8, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (ptr) |p| {
        const slice = std.mem.span(p);
        const ds = allocDoxaString(scope, slice);
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
    } else {
        out_ptr.* = null;
        out_len.* = 0;
    }
}

/// Clone a string into `scope`.
fn strCloneInto(scope: *Scope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) void {
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

/// Clone with a null terminator, returning the raw C-string pointer. Maps are
/// the only remaining consumer: they store string keys/values as a single i64
/// pointer slot and recover the length via `std.mem.span`.
pub export fn doxa_str_clone_raw(scope: *Scope, ptr: ?[*]const u8, len: u64) callconv(.c) ?[*:0]u8 {
    if (ptr) |p| {
        const slice: []const u8 = p[0..@intCast(len)];
        const out = scope_arena.allocator(scope).allocSentinel(u8, slice.len, 0) catch return null;
        @memcpy(out[0..slice.len], slice);
        return out.ptr;
    }
    return null;
}

pub export fn doxa_char_to_string(scope: *Scope, ch: u8, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const buf = scope_arena.allocSlice(scope, u8, 1);
    buf[0] = ch;
    out_ptr.* = buf.ptr;
    out_len.* = 1;
}

pub export fn doxa_print_i64(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    doxaWrite(rendered);
}

pub export fn doxa_print_u64(value: u64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    doxaWrite(rendered);
}

pub export fn doxa_print_f64(value: f64) callconv(.c) void {
    var buf: [max_float_text_len]u8 = undefined;
    doxaWrite(formatFloat(&buf, value));
}

pub export fn doxa_print_tetra(value: i64) callconv(.c) void {
    doxaWrite(tetraName(@intCast(value & 0x3)));
}

/// Print a variant of the enum `desc` describes, as `.Variant`.
pub export fn doxa_print_enum(desc: *const EnumDesc, variant: i64) callconv(.c) void {
    var buf: [256]u8 = undefined;
    var out: std.Io.Writer = .fixed(&buf);
    writeVariant(&out, desc, variant) catch return;
    doxaWrite(out.buffered());
}

fn writeVariant(out: *std.Io.Writer, desc: *const EnumDesc, variant: i64) !void {
    const names = desc.variant_names orelse return out.writeAll(".Unknown");
    if (variant < 0 or variant >= desc.variant_count) return out.writeAll(".Unknown");
    const name = names[@intCast(variant)] orelse return out.writeAll(".Unknown");
    try out.print(".{s}", .{std.mem.span(name)});
}

pub export fn doxa_print_byte(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const rendered = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch return;
    doxaWrite(rendered);
}

fn getEnumVariantName(type_name: []const u8, variant_index: i64) []const u8 {
    const desc = findEnumDescByName(type_name) orelse return "Unknown";
    if (variant_index < 0) return "Unknown";
    const idx: usize = @intCast(@as(u64, @intCast(variant_index)));
    const count: usize = @intCast(desc.variant_count);
    if (idx >= count) return "Unknown";

    if (desc.variant_names) |names_ptr| {
        const names = names_ptr[0..count];
        if (names[idx]) |n| {
            return std.mem.span(n);
        }
    }
    return "Unknown";
}

pub export fn doxa_int(value: f64) callconv(.c) i64 {
    return @intFromFloat(value);
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
    const ndl = sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    var idx: u64 = 0;
    while (idx < h.len) : (idx += 1) {
        var elem_ptr: ?[*]u8 = null;
        var elem_len: u64 = 0;
        doxa_array_get_str(h, idx, &elem_ptr, &elem_len);
        const elem = sliceFromDoxaString(.{ .ptr = elem_ptr, .len = elem_len });
        if (std.mem.eql(u8, elem, ndl)) return @intCast(idx);
    }
    return -1;
}

pub export fn doxa_find_str(h_ptr: ?[*]const u8, h_len: u64, n_ptr: ?[*]const u8, n_len: u64) callconv(.c) i64 {
    const str = sliceFromDoxaString(.{ .ptr = h_ptr, .len = h_len });
    const ndl = sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    if (ndl.len == 0) return 0;
    return if (std.mem.indexOf(u8, str, ndl)) |found| @intCast(found) else -1;
}

pub export fn doxa_map_new(scope: *Scope, capacity: i64, key_tag: i64, value_tag: i64) callconv(.c) *MapRuntime.MapHeader {
    return MapRuntime.mapNew(scope, capacity, key_tag, value_tag);
}

/// The arena that owns `map`: a string or box stored into it is placed there.
pub export fn doxa_map_scope(map: *MapRuntime.MapHeader) callconv(.c) *Scope {
    return map.scope;
}

pub export fn doxa_map_set_i64(map: *MapRuntime.MapHeader, key: i64, value: i64) callconv(.c) void {
    MapRuntime.mapSetI64(map, key, value);
}

/// Set the entry for a string key, compared by content; a new key is copied
/// into the map's arena.
pub export fn doxa_map_set_str(map: *MapRuntime.MapHeader, key_ptr: ?[*]const u8, key_len: u64, value: i64) callconv(.c) void {
    MapRuntime.mapSetStr(map, sliceFromDoxaString(.{ .ptr = key_ptr, .len = key_len }), value);
}

pub export fn doxa_map_set_else_i64(map: *MapRuntime.MapHeader, value: i64) callconv(.c) void {
    MapRuntime.mapSetElseI64(map, value);
}

pub export fn doxa_map_try_get_i64(map: *MapRuntime.MapHeader, key: i64, out_value: *i64) callconv(.c) u8 {
    return if (MapRuntime.mapTryGetI64(map, key, out_value)) 1 else 0;
}

/// Look a string key up by content, without copying it.
pub export fn doxa_map_try_get_str(map: *MapRuntime.MapHeader, key_ptr: ?[*]const u8, key_len: u64, out_value: *i64) callconv(.c) u8 {
    return if (MapRuntime.mapTryGetStr(map, sliceFromDoxaString(.{ .ptr = key_ptr, .len = key_len }), out_value)) 1 else 0;
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

/// Canonical runtime representation for Doxa values. This is the shared value
/// model for the native (LLVM) backend and any C callers. The layout must stay
/// in sync with `%DoxaValue` in `src/codegen/llvmir/ir_printer.zig`.
///
/// Tag values:
///   0 = Int      (payload_bits = i64)
///   1 = Float    (payload_bits = bitcast f64)
///   2 = Byte     (payload_bits[0..8] = u8)
///   3 = String   (payload_bits = pointer bits, payload_len = byte length)
///   4 = Array    (payload_bits = *ArrayHeader as bits)
///   5 = Struct   (payload_bits = pointer to struct instance)
///   6 = Enum     (payload_bits = i64 variant index; type via metadata)
///   7 = Tetra    (payload_bits[0..2] = u2)
///   8 = Nothing  (payload_bits ignored)
///   9 = Function (payload_bits = pointer to function closure/descriptor)
///  10 = Map      (payload_bits = *MapHeader as bits)
///
/// All non-String tags leave `payload_len = 0`; only String uses it.
///
/// Boxing rules:
///   - Unboxed: Int, Float, Byte, Tetra, Enum discriminant
///   - Boxed:   String, Array, Struct, Function, Map
///   - Sentinel: Nothing (tag = 8, payload_bits = 0)
///
/// Box encoding (a value of a union or group type):
///   - For an unboxed value, `reserved == 0`.
///   - For a box, `reserved` packs:
///       bit 31        : is_boxed flag
///       bits 16..30   : box_id — the box type, one id space for unions and
///                       groups, indexing the box registry (`BoxDesc`)
///       bits 0..15    : member_index — the member of that box type held
///   - `tag` always describes the active payload kind (Int, Float, Struct, …).
pub const DoxaValue = extern struct {
    tag: u32,
    reserved: u32,
    payload_bits: i64,
    payload_len: i64,
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

/// Encoding and decoding of a box's `reserved` word. Kept in sync with the
/// IR printer, which writes it (`buildDoxaValue`, `repackBox`).
pub const DoxaBoxMeta = struct {
    pub const is_boxed_bit: u32 = 1 << 31;
    pub const box_id_shift: u5 = 16;
    pub const box_id_mask: u32 = 0x7FFF << box_id_shift; // 15 bits
    pub const max_box_id: u32 = 0x7FFF;
    pub const member_index_mask: u32 = 0xFFFF; // 16 bits

    pub fn isBoxed(reserved: u32) bool {
        return (reserved & is_boxed_bit) != 0;
    }

    pub fn boxId(reserved: u32) u32 {
        return (reserved & box_id_mask) >> box_id_shift;
    }

    pub fn memberIndex(reserved: u32) u32 {
        return reserved & member_index_mask;
    }
};

/// A box type's members, in the order its member indexes name them. An enum
/// member has its display name and descriptor; any other member has neither.
pub const BoxDesc = extern struct {
    member_count: u64,
    member_names: [*]const ?[*:0]const u8,
    member_enums: [*]const ?*const EnumDesc,
};

/// Every box type of the program, indexed by box id. Set once at startup.
var box_registry: []const *const BoxDesc = &.{};

pub export fn doxa_box_registry_init(descs: [*]const *const BoxDesc, count: *const u64) callconv(.c) void {
    box_registry = descs[0..@intCast(count.*)];
}

/// Write a boxed enum as `Type.Variant`, naming it through the box registry.
/// False when the box names no enum member the registry knows.
fn writeBoxedEnum(out: *std.Io.Writer, val: *const DoxaValue) !bool {
    if (!DoxaBoxMeta.isBoxed(val.reserved)) return false;
    const box_id = DoxaBoxMeta.boxId(val.reserved);
    if (box_id >= box_registry.len) return false;
    const desc = box_registry[box_id];
    const member = DoxaBoxMeta.memberIndex(val.reserved);
    if (member >= desc.member_count) return false;
    const type_name = desc.member_names[member] orelse return false;
    const enum_desc = desc.member_enums[member] orelse return false;
    const variant: u64 = @bitCast(val.payload_bits);
    if (variant >= enum_desc.variant_count) return false;
    const names = enum_desc.variant_names orelse return false;
    const variant_name = names[@intCast(variant)] orelse return false;
    try out.print("{s}.{s}", .{ std.mem.span(type_name), std.mem.span(variant_name) });
    return true;
}

/// Re-home the heap payload of a boxed `DoxaValue` (String/Array/Struct) into
/// `scope`. Unboxed payloads (Int, Float, Byte, Tetra, Enum, Nothing) are left
/// unchanged.
fn cloneDoxaValueInto(scope: *Scope, val: *DoxaValue) void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .String => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr != 0) {
                const ptr: [*]const u8 = @ptrFromInt(@as(usize, @intCast(addr)));
                const len: usize = @intCast(val.payload_len);
                var cloned_ptr: ?[*]u8 = null;
                var cloned_len: u64 = 0;
                strCloneInto(scope, ptr, @intCast(len), &cloned_ptr, &cloned_len);
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

pub export fn doxa_debug_peek(info_ptr: ?*const DoxaPeekInfo) callconv(.c) void {
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

pub export fn doxa_byte_from_string(ptr: ?[*]const u8, len: u64) callconv(.c) i64 {
    const s_val = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    if (s_val.len == 0) return 0;
    if (s_val.len == 1) return @as(i64, @intCast(s_val[0]));

    if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
        const hex_str = s_val[2..];
        const parsed_hex_byte = std.fmt.parseInt(u8, hex_str, 16) catch return 0;
        return @as(i64, @intCast(parsed_hex_byte));
    }

    const parsed_int_opt: ?i64 = std.fmt.parseInt(i64, s_val, 10) catch null;
    if (parsed_int_opt) |parsed_int| {
        if (parsed_int >= 0 and parsed_int <= 255) return parsed_int;
        return 0;
    }

    const parsed_float = std.fmt.parseFloat(f64, s_val) catch return 0;
    if (!std.math.isFinite(parsed_float)) return 0;
    const rounded: i64 = @intFromFloat(parsed_float);
    if (rounded >= 0 and rounded <= 255) return rounded;
    return 0;
}

pub export fn doxa_byte_from_f64(value: f64) callconv(.c) i64 {
    if (!std.math.isFinite(value)) return 0;
    const rounded: i64 = @intFromFloat(value);
    if (rounded >= 0 and rounded <= 255) return rounded;
    return 0;
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

pub const ArrayHeader = extern struct {
    data: ?*anyopaque,
    len: u64,
    cap: u64,
    elem_size: u64,
    elem_tag: u64,
    /// The scope arena this array was allocated in. Heap elements pushed into
    /// the array are re-homed here so they survive the pushing scope's teardown.
    scope: *Scope,
    /// For struct elements (tag 7) of a descriptor-free scalar struct, the
    /// struct's size in i64 words, so an element store copies it without a
    /// registry lookup; 0 otherwise. Set by `doxa_array_set_elem_words` and
    /// carried to every array built from this one.
    elem_words: u64,
};

pub const StructDesc = extern struct {
    type_name: ?[*:0]const u8,
    field_count: u64,
    field_names: ?[*]const ?[*:0]const u8,
    field_tags: ?[*]const u64,
    field_enum_type_names: ?[*]const ?[*:0]const u8,
    /// Per field: the word count of a nested descriptor-free scalar struct
    /// (tag 7), which has no registry entry to clone it by; 0 for any other
    /// field. Null when the struct has no such field.
    field_struct_words: ?[*]const u64,
};

/// What the runtime knows about a heap struct instance: its layout descriptor
/// (null when it was registered for scope tracking only) and the arena that
/// owns it. One map keyed by instance address, so registering an instance is a
/// single hash-table write.
const StructMeta = struct {
    desc: ?*const StructDesc,
    scope: *Scope,
};

var struct_meta: std.AutoHashMapUnmanaged(usize, StructMeta) = .{};

/// Starting size of `struct_meta`, so a program that registers many structs
/// does not pay for the table's first several rehashes.
const struct_meta_initial_capacity = 4096;

fn registerStruct(inst: *anyopaque, desc: ?*const StructDesc, scope: *Scope) void {
    // Best-effort registration; OOM in a runtime struct registry is non-recoverable
    if (struct_meta.capacity() == 0) struct_meta.ensureTotalCapacity(std.heap.page_allocator, struct_meta_initial_capacity) catch {};
    struct_meta.put(std.heap.page_allocator, @intFromPtr(inst), .{ .desc = desc, .scope = scope }) catch {};
}

/// The descriptor registered for the struct at `ptr`, if any.
fn structDescOf(ptr: *const anyopaque) ?*const StructDesc {
    const meta = struct_meta.get(@intFromPtr(ptr)) orelse return null;
    return meta.desc;
}

/// Register a struct allocated in `scope` under its descriptor, so it can be
/// printed, cloned and re-homed.
pub export fn doxa_struct_register(scope: *Scope, instance: *anyopaque, desc: *const StructDesc) callconv(.c) void {
    registerStruct(instance, desc, scope);
}

/// The arena that owns a registered struct: where a field store places a
/// string or a box. Every struct with a heap field is registered; a struct
/// without one has no field that needs placing.
pub export fn doxa_struct_scope(instance: *anyopaque) callconv(.c) *Scope {
    return (struct_meta.get(@intFromPtr(instance)) orelse @panic("doxa_struct_scope: an unregistered struct has a heap field")).scope;
}

/// Default the struct elements of a fixed array that stores pointers: each
/// slot of `buffer` gets its own copy of `template`, the struct's default
/// value, in `scope`.
pub export fn doxa_fixed_structs_default(scope: *Scope, buffer: [*]?*anyopaque, count: u64, template: *anyopaque) callconv(.c) void {
    for (0..@intCast(count)) |i| buffer[i] = structCloneInto(scope, template);
}

/// Word count of a single struct field given its runtime tag. String fields
/// (tag 3) occupy two `i64` words (ptr + len); everything else occupies one.
fn structFieldWordCount(tag: u64) usize {
    return if (tag == 3) 2 else 1;
}

/// Total number of `i64` words occupied by a struct's fields. When the tag
/// list is absent (defensive fallback), every field is treated as one word.
fn structTotalWords(tags: []const u64, field_count: usize) usize {
    var total: usize = field_count;
    var i: usize = 0;
    while (i < tags.len and i < field_count) : (i += 1) {
        if (tags[i] == 3) total += 1;
    }
    return total;
}

/// Construct defaults for a fixed array of structs that is not flat-allocatable
/// (a struct with a heap/union field, or a multidimensional struct array).
/// Every innermost struct-tagged slot is filled with a zeroed struct registered
/// under `desc`. Scalars default to zero and a union/group field (tag 9)
/// defaults to a `nothing` box. A string/array/nested-struct field is left
/// null. TODO: recursively default those field kinds.
pub export fn doxa_array_fill_default_structs(hdr: *ArrayHeader, desc: ?*const StructDesc) callconv(.c) void {
    const d = desc orelse return;
    fillDefaultStructsRec(hdr, d);
}

fn fillDefaultStructsRec(hdr: *ArrayHeader, desc: *const StructDesc) void {
    if (hdr.len == 0) return;

    if (hdr.elem_tag == ARRAY_TAG) {
        var i: u64 = 0;
        while (i < hdr.len) : (i += 1) {
            const elem = doxa_array_get_i64(hdr, i);
            if (elem == 0) continue;
            fillDefaultStructsRec(@ptrFromInt(@as(usize, @intCast(elem))), desc);
        }
        return;
    }
    if (hdr.elem_tag != 7) return;

    const field_count: usize = @intCast(desc.field_count);
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    const words = structTotalWords(tags, field_count);
    const scope = hdr.scope;

    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (doxa_array_get_i64(hdr, i) != 0) continue;

        const dst = scope_arena.allocSlice(scope, i64, words);
        @memset(dst, 0);

        var word: usize = 0;
        for (0..field_count) |fi| {
            const ftag: u64 = if (fi < tags.len) tags[fi] else 255;
            if (ftag == 9) {
                const box_words = scope_arena.allocSlice(scope, i64, 3);
                const box: *DoxaValue = @ptrCast(@alignCast(box_words.ptr));
                box.* = .{ .tag = @intFromEnum(DoxaTag.Nothing), .reserved = 0, .payload_bits = 0, .payload_len = 0 };
                dst[word] = @intCast(@intFromPtr(box));
            }
            word += structFieldWordCount(ftag);
        }

        const inst: *anyopaque = @ptrCast(dst.ptr);
        registerStruct(inst, desc, scope);
        doxa_array_set_i64(hdr, i, @as(i64, @intCast(@intFromPtr(inst))));
    }
}

/// Word count of a registered struct instance, or null when it has no
/// descriptor. Used to write an assigned element through a non-owning view's
/// existing slot.
fn structWordCountInRegistry(ptr: *anyopaque) ?usize {
    const desc = structDescOf(ptr) orelse return null;
    const field_count: usize = @intCast(desc.field_count);
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    return structTotalWords(tags, field_count);
}

fn structCloneInto(scope: *Scope, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const desc = structDescOf(src) orelse return null;
    const field_count: usize = @intCast(desc.field_count);
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    const dst = scope_arena.allocSlice(scope, i64, structTotalWords(tags, field_count));
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
                strCloneInto(scope, @ptrFromInt(@as(usize, @intCast(addr))), @intCast(len), &cloned_ptr, &cloned_len);
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
                structCloneScalarInto(scope, @intCast(nested_words), nested_src)
            else
                structCloneInto(scope, nested_src);
            dst[word] = @intCast(@intFromPtr(nested orelse nested_src));
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
    const result: ?*anyopaque = @ptrCast(dst.ptr);
    // Register the clone so its fields can be introspected/printed (and cloned
    // again) under the same descriptor as the source.
    registerStruct(@ptrCast(dst.ptr), desc, scope);
    return result;
}

/// Deep-copy a struct into `scope`: `clone` of a registered struct. String
/// (tag 3) and array (tag 6) fields are cloned recursively so their storage
/// outlives the scope that constructed the struct; other fields are copied
/// verbatim.
pub export fn doxa_struct_clone(scope: *Scope, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structCloneInto(scope, ptr);
}

/// B2/B3 scalar-struct typed clone: copy `word_count` i64 words into `scope`
/// verbatim. A struct with only scalar fields has no heap field to re-clone,
/// so it needs no descriptor registry lookup — this is what lets a
/// non-reflected scalar struct skip `doxa_struct_register` entirely.
pub export fn doxa_struct_clone_scalar(scope: *Scope, word_count: u64, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structCloneScalarInto(scope, @intCast(word_count), ptr);
}

/// Rehome for a descriptor-free scalar struct, which has no `struct_meta`
/// entry to say which arena owns it. Keeps the original pointer when its arena
/// outlives `scope`, or when no live arena holds it (as the registry path does
/// for an unregistered pointer), and otherwise copies `word_count` words into
/// `scope`. Identity therefore survives the same cases it does for a
/// registered struct, such as an `each` binding writing through to an element.
fn structRehomeScalarInto(scope: *Scope, word_count: usize, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const owner = scope_arena.ownerOf(src) orelse return src;
    if (scope_arena.isEqualOrDescendant(scope, owner)) return src;
    return structCloneScalarInto(scope, word_count, src);
}

pub export fn doxa_struct_rehome_scalar(scope: *Scope, word_count: u64, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structRehomeScalarInto(scope, @intCast(word_count), ptr);
}

fn structCloneScalarInto(scope: *Scope, word_count: usize, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    const dst = scope_arena.allocSlice(scope, i64, word_count);
    const src_words: [*]const i64 = @ptrCast(@alignCast(src));
    @memcpy(dst, src_words[0..word_count]);
    return @ptrCast(dst.ptr);
}

/// `rehome` of a registered struct, the one runtime-checked copy
/// (`plan/register-hir.md`, "What stays a runtime check").
/// Like `structCloneInto`, but keep the original pointer when its allocating
/// arena already outlives `scope` (same arena or an ancestor). That preserves
/// identity for `each n in arr { n.field is ... }` — a snapshot clone would
/// disconnect `n` from the array element.
///
/// Unknown allocation scope keeps identity too. Array elements and other
/// long-lived heap objects are often not in `struct_meta`; cloning them
/// would silently snapshot field writes off the source.
fn structRehomeInto(scope: *Scope, ptr: ?*anyopaque) ?*anyopaque {
    const src = ptr orelse return null;
    if (struct_meta.get(@intFromPtr(src))) |meta| {
        if (scope_arena.isEqualOrDescendant(scope, meta.scope)) return src;
        return structCloneInto(scope, src);
    }
    return src;
}

pub export fn doxa_struct_rehome(scope: *Scope, ptr: ?*anyopaque) callconv(.c) ?*anyopaque {
    return structRehomeInto(scope, ptr);
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
    // Best-effort registration; OOM in a runtime enum registry is non-recoverable
    enum_registry.put(std.heap.page_allocator, @intFromPtr(tn), ed) catch {};
}

fn findEnumDescByName(type_name: []const u8) ?*const EnumDesc {
    var it = enum_registry.iterator();
    while (it.next()) |entry| {
        const registered_ptr: [*:0]const u8 = @ptrFromInt(entry.key_ptr.*);
        const registered_name = std.mem.span(registered_ptr);
        if (std.mem.eql(u8, registered_name, type_name)) {
            return entry.value_ptr.*;
        }
    }
    return null;
}

fn lookupEnumDesc(type_name_ptr: ?[*:0]const u8) ?*const EnumDesc {
    const tn = type_name_ptr orelse return null;
    if (enum_registry.get(@intFromPtr(tn))) |desc| return desc;
    return findEnumDescByName(std.mem.span(tn));
}

fn lookupEnumDescBySlice(type_name: []const u8) ?*const EnumDesc {
    return findEnumDescByName(type_name);
}

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
fn arrayNewIn(scope: *Scope, elem_size: u64, elem_tag: u64, init_len: u64) *ArrayHeader {
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

const ARRAY_TAG: u64 = 6;
/// A union or group element: a `DoxaValue` stored inline, 24 bytes. The box
/// keeps the member it holds, which a bare payload word would lose. Struct
/// fields use the same tag for a union or group field.
const VALUE_TAG: u64 = 9;

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
/// `ArrayHeader` of boxed struct pointers in `scope`.
///
/// A fixed array of structs stores each element inline as its raw
/// `struct_words` i64 words (`emitArrayNew`'s flat path). A dynamic array's
/// struct element is a heap pointer (tag 7) that the descriptor registry clones
/// through, so each flat slot is boxed — allocated in the owning arena and
/// registered under `desc` — and the array holds the box pointers.
pub export fn doxa_array_from_fixed_structs(
    scope: *Scope,
    src: [*]const u8,
    count: u64,
    struct_words: u64,
    desc: ?*const StructDesc,
) callconv(.c) *ArrayHeader {
    const arr = arrayNewIn(scope, @sizeOf(*anyopaque), 7, count);
    if (arr.data == null or count == 0) return arr;

    const src_words: [*]const i64 = @ptrCast(@alignCast(src));
    const slots: [*]?*anyopaque = @ptrCast(@alignCast(arr.data.?));
    const words: usize = @intCast(struct_words);
    var idx: u64 = 0;
    while (idx < count) : (idx += 1) {
        const box = scope_arena.allocSlice(scope, i64, words);
        @memcpy(box, src_words[@intCast(idx * struct_words)..][0..words]);
        const box_ptr: *anyopaque = @ptrCast(box.ptr);
        registerStruct(box_ptr, desc, scope);
        slots[@intCast(idx)] = box_ptr;
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

/// Copy element `src_idx` of `src` to `dst_idx` of `dst`, whatever the
/// element representation, re-homing a heap element into `dst`'s arena.
fn copyElement(dst: *ArrayHeader, dst_idx: u64, src: *ArrayHeader, src_idx: u64) void {
    switch (src.elem_tag) {
        3 => {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: u64 = undefined;
            doxa_array_get_str(src, src_idx, &str_ptr, &str_len);
            doxa_array_set_str(dst, dst_idx, str_ptr, str_len);
        },
        VALUE_TAG => {
            var value: DoxaValue = undefined;
            doxa_array_get_value(src, src_idx, &value);
            doxa_array_set_value(dst, dst_idx, &value);
        },
        else => doxa_array_set_i64(dst, dst_idx, doxa_array_get_i64(src, src_idx)),
    }
}

fn arrayCloneIn(scope: *Scope, src: *ArrayHeader) *ArrayHeader {
    const result = arrayNewIn(scope, src.elem_size, src.elem_tag, src.len);
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
    if (scope_arena.isEqualOrDescendant(scope, hdr.scope)) return hdr;
    return arrayCloneIn(scope, hdr);
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
        6 => { // array (*ArrayHeader pointer encoded as bits)
            const ap: *?*ArrayHeader = @ptrCast(@alignCast(p));
            const addr: u64 = @bitCast(value);
            ap.* = if (addr == 0)
                null
            else
                arrayCloneIn(hdr.scope, @ptrFromInt(@as(usize, @intCast(addr))));
        },
        7 => { // struct (pointer encoded as bits)
            const sp: *?*anyopaque = @ptrCast(@alignCast(p));
            const addr: u64 = @bitCast(value);
            const src: ?*anyopaque = if (addr == 0) null else @ptrFromInt(@as(usize, @intCast(addr)));
            if (src == null) {
                sp.* = null;
            } else if (hdr.elem_words != 0) {
                sp.* = structCloneScalarInto(hdr.scope, @intCast(hdr.elem_words), src);
            } else {
                sp.* = structCloneInto(hdr.scope, src.?);
            }
        },
        // Default: store raw 64-bit payload (pointers/unknown).
        // When the element type is unknown (e.g. an empty `[]` literal) and the
        // payload is a registered struct pointer, re-home it into this array's
        // scope so it survives the source scope being freed.
        else => {
            const addr: u64 = @bitCast(value);
            if (addr != 0) {
                if (structCloneInto(hdr.scope, @ptrFromInt(@as(usize, @intCast(addr))))) |cloned| {
                    const ip: *i64 = @ptrCast(@alignCast(p));
                    ip.* = @intCast(@intFromPtr(cloned));
                    return;
                }
            }
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
    var cloned_ptr: ?[*]u8 = null;
    var cloned_len: u64 = 0;
    strCloneInto(hdr.scope, str_ptr, str_len, &cloned_ptr, &cloned_len);
    const base: [*]u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ptr_slot: *i64 = @ptrCast(@alignCast(p));
    const len_slot: *i64 = @ptrCast(@alignCast(p + 8));
    const addr: u64 = if (cloned_ptr) |vp| @intFromPtr(vp) else 0;
    ptr_slot.* = @bitCast(addr);
    len_slot.* = @bitCast(@as(u64, cloned_len));
}

/// Read a union or group element (tag 9) into `out`. Out of range, or any
/// other element kind, reads as a `nothing` box.
pub export fn doxa_array_get_value(hdr: *ArrayHeader, idx: u64, out: *DoxaValue) callconv(.c) void {
    if (hdr.data == null or idx >= hdr.len or hdr.elem_tag != VALUE_TAG) {
        out.* = .{ .tag = @intFromEnum(DoxaTag.Nothing), .reserved = 0, .payload_bits = 0, .payload_len = 0 };
        return;
    }
    const slots: [*]const DoxaValue = @ptrCast(@alignCast(hdr.data.?));
    out.* = slots[@intCast(idx)];
}

/// Store a union or group element (tag 9), re-homing its heap payload into
/// the array's own arena so it outlives the storing scope.
pub export fn doxa_array_set_value(hdr: *ArrayHeader, idx: u64, value: *const DoxaValue) callconv(.c) void {
    if (!ensureArrayCapacity(hdr, idx + 1)) return;
    if (hdr.data == null or hdr.elem_tag != VALUE_TAG) return;
    if (idx >= hdr.len) hdr.len = idx + 1;
    var stored = value.*;
    cloneDoxaValueInto(hdr.scope, &stored);
    const slots: [*]DoxaValue = @ptrCast(@alignCast(hdr.data.?));
    slots[@intCast(idx)] = stored;
}

pub export fn doxa_array_concat(scope: *Scope, a: *ArrayHeader, b: *ArrayHeader) callconv(.c) *ArrayHeader {
    const result = arrayNewIn(scope, a.elem_size, a.elem_tag, a.len + b.len);
    result.elem_words = a.elem_words;
    var idx: u64 = 0;
    while (idx < a.len) : (idx += 1) copyElement(result, idx, a, idx);
    idx = 0;
    while (idx < b.len) : (idx += 1) copyElement(result, a.len + idx, b, idx);
    return result;
}

/// Open a gap at `pos` by moving every element from `pos` on one place up.
/// The caller has already grown `h.len` by one.
fn shiftUp(h: *ArrayHeader, pos: u64, old_len: u64) void {
    var i: u64 = old_len;
    while (i > pos) : (i -= 1) copyElement(h, i, h, i - 1);
}

/// Close the gap at `pos` by moving every later element one place down.
fn shiftDown(h: *ArrayHeader, pos: u64) void {
    var i: u64 = pos;
    while (i + 1 < h.len) : (i += 1) copyElement(h, i, h, i + 1);
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
pub export fn doxa_array_insert_value(h: *ArrayHeader, idx: i64, value: *const DoxaValue) callconv(.c) void {
    const pos = openInsert(h, idx) orelse return;
    doxa_array_set_value(h, pos, value);
}

/// Remove the union or group element (tag 9) at `idx` into `out_removed`.
pub export fn doxa_array_remove_value(h: *ArrayHeader, idx: i64, out_removed: *DoxaValue) callconv(.c) void {
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

pub export fn doxa_array_slice(scope: *Scope, h: *ArrayHeader, start: i64, length: i64) callconv(.c) *ArrayHeader {
    const out_len: u64 = if (start < 0 or length < 0 or @as(u64, @intCast(start)) >= h.len or length == 0)
        0
    else
        @min(@as(u64, @intCast(length)), h.len - @as(u64, @intCast(start)));
    const out = arrayNewIn(scope, h.elem_size, h.elem_tag, out_len);
    out.elem_words = h.elem_words;
    if (out_len == 0) return out;
    const s: u64 = @intCast(start);
    var i: u64 = 0;
    while (i < out_len) : (i += 1) copyElement(out, i, h, s + i);
    return out;
}

/// `@clear` of an array: empty it in place.
pub export fn doxa_array_clear(h: *ArrayHeader) callconv(.c) void {
    h.len = 0;
}

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) callconv(.c) void {
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    printArrayHdrImpl(&list.writer, hdr) catch return;
    doxaWrite(list.written());
}

fn printTaggedBitsImpl(out: *std.Io.Writer, tag: u64, bits: i64) anyerror!void {
    switch (tag) {
        0 => { // int (i64)
            try out.print("{d}", .{bits});
        },
        1 => { // byte (u8)
            const b: u8 = asByte(bits);
            try out.print("{d}", .{b});
        },
        2 => { // float (f64)
            var buf: [max_float_text_len]u8 = undefined;
            try out.writeAll(formatFloat(&buf, asFloat(bits)));
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
                    @ptrCast(@alignCast(@as(?*anyopaque, @ptrFromInt(@as(usize, @intCast(addr)))))),
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

fn printEnumImpl(out: *std.Io.Writer, type_name: []const u8, bits: i64) anyerror!void {
    if (type_name.len == 0) {
        try out.print("<enum:{d}>", .{bits});
        return;
    }
    const desc = lookupEnumDescBySlice(type_name) orelse {
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

    const name = getEnumVariantName(type_name, bits);
    if (!std.mem.eql(u8, name, "Unknown")) {
        try out.print(".{s}", .{name});
    } else {
        try out.print(".Unknown", .{});
    }
}

fn printStructImpl(out: *std.Io.Writer, addr: u64) anyerror!void {
    const key: usize = @intCast(addr);
    const desc = structDescOf(@ptrFromInt(key)) orelse {
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
    var word: usize = structTotalWords(tags, field_count);
    var idx: usize = field_count;
    while (idx > 0) {
        idx -= 1;
        const tag: u64 = if (idx < tags.len) tags[idx] else 255;
        word -= structFieldWordCount(tag);
        const bits: i64 = fields[word];
        if (!first) try out.print(", ", .{});

        const name_slice: []const u8 = if (idx < names.len) blk: {
            if (names[idx]) |n| break :blk std.mem.span(n);
            break :blk "";
        } else "";

        try out.print("{s}: ", .{name_slice});

        if (tag == 3) {
            const str_addr: u64 = @bitCast(bits);
            const len: usize = @intCast(@as(u64, @bitCast(fields[word + 1])));
            const s: []const u8 = if (str_addr == 0) "" else @as([*]const u8, @ptrFromInt(@as(usize, @intCast(str_addr))))[0..len];
            try out.writeAll("\"");
            try writeEscaped(out, s);
            try out.writeAll("\"");
        } else if (tag == 8 and idx < enum_type_names.len) {
            const etn: []const u8 = if (enum_type_names[idx]) |n| std.mem.span(n) else "";
            try printEnumImpl(out, etn, bits);
        } else if (tag == 9) {
            // Boxed union/group field: render the boxed %DoxaValue.
            try writeValue(out, @ptrFromInt(@as(usize, @intCast(bits))));
        } else {
            try printTaggedBitsImpl(out, tag, bits);
        }
        first = false;
    }
    try out.print(" }}", .{});
}

/// A union or group element of a printed array: a string quoted like a string
/// element, anything else as the value prints.
fn printValueElement(out: *std.Io.Writer, value: *const DoxaValue) anyerror!void {
    if (value.tag == @intFromEnum(DoxaTag.String)) {
        try out.writeAll("\"");
        try writeEscaped(out, stringPayload(value));
        try out.writeAll("\"");
        return;
    }
    try writeValue(out, value);
}

fn printArrayHdrImpl(out: *std.Io.Writer, hdr: *ArrayHeader) anyerror!void {
    try out.print("[", .{});

    if (hdr.len == 0) {
        try out.print("]", .{});
        return;
    }

    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (i != 0) try out.print(", ", .{});
        if (hdr.elem_tag == 3) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: u64 = undefined;
            doxa_array_get_str(hdr, i, &str_ptr, &str_len);
            const s = if (str_ptr) |p| p[0..@intCast(str_len)] else "";
            try out.writeAll("\"");
            try writeEscaped(out, s);
            try out.writeAll("\"");
        } else if (hdr.elem_tag == VALUE_TAG) {
            var value: DoxaValue = undefined;
            doxa_array_get_value(hdr, i, &value);
            try printValueElement(out, &value);
        } else {
            const elem_bits = doxa_array_get_i64(hdr, i);
            try printTaggedBitsImpl(out, hdr.elem_tag, elem_bits);
        }
    }

    try out.print("]", .{});
}

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

/// Print a value described by the canonical `DoxaValue` layout. This is the
/// primary entry point for native code that renders values via the runtime.
pub export fn doxa_print_value(val: *const DoxaValue) callconv(.c) void {
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
            const addr: u64 = @bitCast(val.payload_bits);
            const len: usize = @intCast(val.payload_len);
            const ptr: ?[*]const u8 = if (addr == 0) null else @ptrFromInt(@as(usize, @intCast(addr)));
            doxa_peek_string(ptr, len);
        },
        .Array => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr == 0) {
                doxaWrite("[]");
            } else {
                const any_ptr: ?*anyopaque = @ptrFromInt(@as(usize, @intCast(addr)));
                const hdr = @as(*ArrayHeader, @ptrCast(@alignCast(any_ptr.?)));
                doxa_print_array_hdr(hdr);
            }
        },
        .Struct => {
            var buf: [1024]u8 = undefined;
            var fbs: std.Io.Writer = .fixed(&buf);
            const addr: u64 = @bitCast(val.payload_bits);
            printTaggedBitsImpl(&fbs, 7, @as(i64, @bitCast(addr))) catch return;
            doxaWrite(fbs.buffered());
        },
        .Enum => {
            var buf: [256]u8 = undefined;
            var fbs: std.Io.Writer = .fixed(&buf);
            if (writeBoxedEnum(&fbs, val) catch false) {
                doxaWrite(fbs.buffered());
            } else {
                printTaggedBitsImpl(&fbs, 8, val.payload_bits) catch return;
                doxaWrite(fbs.buffered());
            }
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
            doxaWrite(name);
        },
        .Nothing => {
            // TODO: render Nothing with proper textual payload
            doxaWrite("nothing");
        },
        .Function => {
            doxaWrite("<function>");
        },
        .Map => {
            doxaWrite("<map>");
        },
    }
}
