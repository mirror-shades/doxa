const std = @import("std");
const builtin = @import("builtin");
const MapRuntime = @import("map_runtime.zig");

// Force the arena scope C-ABI exports (doxa_scope_enter/exit/alloc) into the
// compiled runtime. They live in arena.zig but are referenced only by emitted
// IR, so without this they would be dropped from the link.
comptime {
    _ = @import("arena.zig");
}

var peek_output_active: bool = false;

fn doxaWrite(slice: []const u8) void {
    if (peek_output_active) {
        writeStderr(slice);
        if (std.mem.endsWith(u8, slice, "\n")) {
            peek_output_active = false;
        }
    } else {
        writeStdout(slice);
    }
}

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

pub export fn doxa_write_cstr(ptr: ?[*]const u8, len: usize) callconv(.c) void {
    if (ptr) |p| {
        doxaWrite(p[0..len]);
    }
}

/// Write a null-terminated string to the active output sink. Used for
/// user-provided error messages and legacy pointer values where the
/// length is not known statically.
pub export fn doxa_write_raw(ptr: ?[*:0]const u8) callconv(.c) void {
    if (ptr) |p| {
        doxaWrite(std.mem.span(p));
    }
}

pub export fn doxa_write_quoted_string(s: DoxaString) callconv(.c) void {
    doxaWrite("\"");
    doxaWrite(sliceFromDoxaString(s));
    doxaWrite("\"");
}

pub export fn doxa_peek_string(ptr: ?[*]const u8, len: usize) callconv(.c) void {
    doxaWrite("\"");
    if (ptr) |p| {
        if (len > 0) {
            doxaWrite(p[0..len]);
        }
    }
    doxaWrite("\"");
}

pub export fn doxa_str_eq(a_ptr: ?[*]const u8, a_len: usize, b_ptr: ?[*]const u8, b_len: usize) callconv(.c) bool {
    const a = sliceFromDoxaString(.{ .ptr = a_ptr, .len = a_len });
    const b = sliceFromDoxaString(.{ .ptr = b_ptr, .len = b_len });
    return std.mem.eql(u8, a, b);
}

/// Canonical string representation: pointer + byte length.
///
/// This is the internal model for every layer (parser, HIR, VM, LLVM IR).
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
    len: usize,
};

fn allocDoxaString(bytes: []const u8) DoxaString {
    if (bytes.len == 0) return .{ .ptr = null, .len = 0 };
    const buf = std.heap.page_allocator.alloc(u8, bytes.len) catch return .{ .ptr = null, .len = 0 };
    @memcpy(buf, bytes);
    return .{ .ptr = buf.ptr, .len = bytes.len };
}

fn sliceFromDoxaString(s: DoxaString) []const u8 {
    if (s.ptr) |p| return p[0..s.len];
    return "";
}

fn doxaStringFromBits(bits: i64) DoxaString {
    const addr: u64 = @bitCast(bits);
    if (addr == 0) return .{ .ptr = null, .len = 0 };
    const ptr: [*:0]const u8 = @ptrFromInt(@as(usize, addr));
    const slice = std.mem.span(ptr);
    return .{ .ptr = slice.ptr, .len = slice.len };
}

fn ds_ptr(s: DoxaString) ?[*]const u8 { return s.ptr; }
fn ds_len(s: DoxaString) usize { return s.len; }
fn ds_from_parts(ptr: ?[*]const u8, len: usize) DoxaString { return .{ .ptr = ptr, .len = len }; }

var startup_argc: i32 = 0;
var startup_argv: ?[*][*:0]u8 = null;

pub export fn doxa_set_args(argc: i32, argv: ?[*][*:0]u8) callconv(.c) void {
    startup_argc = argc;
    startup_argv = argv;
}


pub export fn doxa_int_from_string(ptr: ?[*]const u8, len: usize) callconv(.c) i64 {
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

pub export fn doxa_float_from_string(ptr: ?[*]const u8, len: usize) callconv(.c) f64 {
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

pub export fn doxa_int_to_string(value: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{value}) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_float_to_string(value: f64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rounded_down = std.math.floor(value);
    const s = if (value - rounded_down == 0)
        std.fmt.bufPrint(&buf, "{d}.0", .{value}) catch {
            out_ptr.* = null;
            out_len.* = 0;
            return;
        }
    else
        std.fmt.bufPrint(&buf, "{d}", .{value}) catch {
            out_ptr.* = null;
            out_len.* = 0;
            return;
        };
    const ds = allocDoxaString(s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_byte_to_string(value: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    var buf: [16]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const s = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_tetra_to_string(value: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    const v: u8 = @intCast(value & 0x3);
    const name: []const u8 = switch (v) {
        0 => "false",
        1 => "true",
        2 => "both",
        3 => "neither",
        else => "invalid",
    };
    const ds = allocDoxaString(name);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_nothing_to_string(out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    const ds = allocDoxaString("nothing");
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_enum_to_string(type_name_ptr: ?[*]const u8, type_name_len: usize, bits: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printEnumImpl(w, sliceFromDoxaString(.{ .ptr = type_name_ptr, .len = type_name_len }), bits) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(list.items);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_struct_to_string(instance: ?*anyopaque, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    if (instance == null) {
        const ds = allocDoxaString("");
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
        return;
    }
    const addr: u64 = @intFromPtr(instance.?);
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printStructImpl(w, addr) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(list.items);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_array_to_string(hdr: ?*ArrayHeader, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    if (hdr == null) {
        const ds = allocDoxaString("");
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
        return;
    }
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const w = list.writer();
    printArrayHdrImpl(w, hdr.?) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = allocDoxaString(list.items);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_pack_bytes(hdr: ?*ArrayHeader, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    if (hdr == null) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const arr = hdr.?;
    const buf = std.heap.page_allocator.alloc(u8, arr.len) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    for (0..arr.len) |i| {
        buf[i] = @intCast(doxa_array_get_i64(arr, i));
    }
    out_ptr.* = buf.ptr;
    out_len.* = arr.len;
}

pub export fn doxa_unpack_bytes(ptr: ?[*]const u8, len: usize) callconv(.c) ?*ArrayHeader {
    const bytes = sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    const result = doxa_array_new(1, 1, bytes.len);
    for (bytes, 0..) |ch, i| {
        doxa_array_set_i64(result, i, ch);
    }
    return result;
}

pub export fn doxa_str_concat(a_ptr: ?[*]const u8, a_len: usize, b_ptr: ?[*]const u8, b_len: usize, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    const as = sliceFromDoxaString(.{ .ptr = a_ptr, .len = a_len });
    const bs = sliceFromDoxaString(.{ .ptr = b_ptr, .len = b_len });
    const total_len = as.len + bs.len;
    if (total_len == 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const buf = std.heap.page_allocator.alloc(u8, total_len) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    @memcpy(buf[0..as.len], as);
    @memcpy(buf[as.len..total_len], bs);
    out_ptr.* = buf.ptr;
    out_len.* = total_len;
}

/// Recover a DoxaString from a legacy null-terminated C-string pointer.
/// Used when reading struct fields that store only a raw pointer.
pub export fn doxa_str_from_cstr(ptr: ?[*:0]const u8, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    if (ptr) |p| {
        const slice = std.mem.span(p);
        const ds = allocDoxaString(slice);
        out_ptr.* = @constCast(ds.ptr);
        out_len.* = ds.len;
    } else {
        out_ptr.* = null;
        out_len.* = 0;
    }
}

pub export fn doxa_str_clone(ptr: ?[*]const u8, len: usize, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    const bytes: []const u8 = if (ptr) |p| p[0..len] else "";
    const ds = allocDoxaString(bytes);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

/// Clone with a null terminator, returning the raw C-string pointer.
/// Used for struct field storage where only an 8-byte pointer slot is available.
pub export fn doxa_str_clone_raw(ptr: ?[*]const u8, len: usize) callconv(.c) ?[*:0]u8 {
    if (ptr) |p| {
        const slice: []const u8 = p[0..len];
        const out = std.heap.page_allocator.allocSentinel(u8, slice.len, 0) catch return null;
        @memcpy(out[0..slice.len], slice);
        return out.ptr;
    }
    return null;
}

pub export fn doxa_char_to_string(ch: u8, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    const buf = std.heap.page_allocator.alloc(u8, 1) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
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
    var buf: [64]u8 = undefined;
    const rounded_down = std.math.floor(value);
    const rendered = if (value - rounded_down == 0)
        std.fmt.bufPrint(&buf, "{d}.0", .{value}) catch return
    else
        std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    doxaWrite(rendered);
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

var rng_state: ?std.Random.DefaultPrng = null;

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

pub export fn doxa_find(collection: ?*anyopaque, value: i64) callconv(.c) i64 {
    if (collection == null) return -1;

    const maybe_hdr = asArrayHeader(collection);
    if (maybe_hdr) |hdr| {
        return doxa_find_array(hdr, value);
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

pub export fn doxa_find_str(h_ptr: ?[*]const u8, h_len: usize, n_ptr: ?[*]const u8, n_len: usize) callconv(.c) i64 {
    const str = sliceFromDoxaString(.{ .ptr = h_ptr, .len = h_len });
    const ndl = sliceFromDoxaString(.{ .ptr = n_ptr, .len = n_len });
    if (ndl.len == 0) return 0;
    return if (std.mem.indexOf(u8, str, ndl)) |found| @intCast(found) else -1;
}

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

pub const DoxaValueC = extern struct {
    tag: u32,
    reserved: u32,
    payload_bits: i64,
};

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

pub export fn doxa_byte_from_string(ptr: ?[*]const u8, len: usize) callconv(.c) i64 {
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

pub export fn doxa_substring(ptr: ?[*]const u8, len: usize, start: i64, length: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
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
    const ds = allocDoxaString(src[start_u..end_u]);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_str_insert(s_ptr: ?[*]const u8, s_len: usize, idx: i64, ins_ptr: ?[*]const u8, ins_len: usize, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
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
    const out = std.heap.page_allocator.alloc(u8, total_len) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    @memcpy(out[0..idx_clamped], src[0..idx_clamped]);
    @memcpy(out[idx_clamped .. idx_clamped + add.len], add);
    @memcpy(out[idx_clamped + add.len .. total_len], src[idx_clamped..]);
    out_ptr.* = out.ptr;
    out_len.* = total_len;
}

pub export fn doxa_str_remove(
    s_ptr: ?[*]const u8,
    s_len: usize,
    idx: i64,
    out_remaining: *DoxaString,
    out_removed: *DoxaString,
) callconv(.c) u8 {
    const src = sliceFromDoxaString(.{ .ptr = s_ptr, .len = s_len });
    if (src.len == 0 or idx < 0) {
        out_remaining.* = allocDoxaString(src);
        out_removed.* = .{ .ptr = null, .len = 0 };
        return 0;
    }
    const iu: usize = @intCast(@as(u64, @intCast(idx)));
    if (iu >= src.len) {
        out_remaining.* = allocDoxaString(src);
        out_removed.* = .{ .ptr = null, .len = 0 };
        return 0;
    }
    out_removed.* = allocDoxaString(src[iu .. iu + 1]);
    const out_len = src.len - 1;
    const out = std.heap.page_allocator.alloc(u8, out_len) catch {
        out_remaining.* = allocDoxaString(src);
        return 0;
    };
    @memcpy(out[0..iu], src[0..iu]);
    @memcpy(out[iu..out_len], src[iu + 1 ..]);
    out_remaining.* = .{ .ptr = out.ptr, .len = out_len };
    return 1;
}

pub export fn doxa_str_pop(s_ptr: ?[*]const u8, s_len: usize, out_remaining: *DoxaString, out_popped: *DoxaString) callconv(.c) u8 {
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

    out_remaining.* = allocDoxaString(src[0..last_char_start]);
    out_popped.* = allocDoxaString(src[last_char_start..src.len]);
    return 1;
}

pub export fn doxa_str_len(ptr: ?[*]const u8, len: usize) callconv(.c) i64 {
    _ = ptr;
    return @intCast(len);
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
    // Best-effort registration; OOM in a runtime struct registry is non-recoverable
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
        const new_slice = std.heap.page_allocator.alloc(i64, @intCast(new_cap)) catch return false;
        @memset(new_slice, 0);

        if (hdr.data) |old_data| {
            const old_slice_ptr: [*]i64 = @ptrCast(@alignCast(old_data));
            const old_slice = old_slice_ptr[0..@intCast(hdr.cap)];
            const copy_len: usize = @intCast(@min(hdr.len, hdr.cap));
            @memcpy(new_slice[0..copy_len], old_slice[0..copy_len]);
            std.heap.page_allocator.free(old_slice);
        }

        hdr.data = @ptrCast(new_slice.ptr);
        hdr.cap = new_cap;
        return true;
    }

    const new_bytes_u64 = std.math.mul(u64, hdr.elem_size, new_cap) catch return false;
    const old_bytes_u64 = std.math.mul(u64, hdr.elem_size, hdr.cap) catch return false;
    if (new_bytes_u64 > std.math.maxInt(usize)) return false;
    if (old_bytes_u64 > std.math.maxInt(usize)) return false;

    const new_buf = std.heap.page_allocator.alloc(u8, @intCast(new_bytes_u64)) catch return false;
    @memset(new_buf, 0);

    if (hdr.data) |old_data| {
        const old_buf_ptr: [*]u8 = @ptrCast(old_data);
        const old_buf = old_buf_ptr[0..@intCast(old_bytes_u64)];
        const used_bytes_u64 = std.math.mul(u64, hdr.elem_size, @min(hdr.len, hdr.cap)) catch 0;
        const used_bytes: usize = @intCast(@min(used_bytes_u64, new_bytes_u64));
        @memcpy(new_buf[0..used_bytes], old_buf[0..used_bytes]);
        std.heap.page_allocator.free(old_buf);
    }

    hdr.data = @ptrCast(new_buf.ptr);
    hdr.cap = new_cap;
    return true;
}

/// 0=int(i64), 1=byte(u8), 2=float(f64), 3=string(i8*), 4=tetra(u8 lower 2 bits),
/// 5=nothing, 6=array(*ArrayHeader), 7=struct(ptr), 8=enum(i64 variant index).
pub export fn doxa_array_new(elem_size: u64, elem_tag: u64, init_len: u64) callconv(.c) *ArrayHeader {
    const cap = clampMin(init_len, ARRAY_MIN_CAPACITY);
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

pub export fn doxa_array_range(start: i64, end: i64) callconv(.c) *ArrayHeader {
    const count: u64 = if (end >= start) @intCast(end - start + 1) else 0;
    const hdr = doxa_array_new(8, 0, count);
    if (count == 0) return hdr;

    const data = hdr.data orelse return hdr;
    const i64_data: [*]i64 = @ptrCast(@alignCast(data));
    var i: u64 = 0;
    while (i < count) : (i += 1) {
        i64_data[i] = start + @as(i64, @intCast(i));
    }
    return hdr;
}

const ARRAY_TAG: u64 = 6;

pub export fn doxa_array_new_nested(
    elem_size: u64,
    elem_tag: u64,
    init_len: u64,
    nested_sizes: [*]const u64,
    nested_depth: u64,
    inner_elem_size: u64,
    inner_elem_tag: u64,
) callconv(.c) *ArrayHeader {
    const outer = doxa_array_new(elem_size, elem_tag, init_len);
    if (nested_depth == 0 or elem_tag != ARRAY_TAG) return outer;

    var idx: u64 = 0;
    while (idx < init_len) : (idx += 1) {
        const inner: *ArrayHeader = if (nested_depth == 1)
            doxa_array_new(inner_elem_size, inner_elem_tag, nested_sizes[0])
        else
            doxa_array_new_nested(
                @sizeOf(*anyopaque),
                ARRAY_TAG,
                nested_sizes[0],
                nested_sizes + 1,
                nested_depth - 1,
                inner_elem_size,
                inner_elem_tag,
            );
        doxa_array_set_i64(outer, idx, @as(i64, @bitCast(@intFromPtr(inner))));
    }
    return outer;
}

pub export fn doxa_array_clone(hdr: ?*ArrayHeader) callconv(.c) *ArrayHeader {
    const src = hdr orelse return doxa_array_new(8, 0, 0);
    const result = doxa_array_new(src.elem_size, src.elem_tag, src.len);
    if (src.elem_tag == 3 and src.elem_size >= 16) {
        var idx: u64 = 0;
        while (idx < src.len) : (idx += 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(src, idx, &str_ptr, &str_len);
            doxa_array_set_str(result, idx, str_ptr, str_len);
        }
    } else {
        var idx: u64 = 0;
        while (idx < src.len) : (idx += 1) {
            const val = doxa_array_get_i64(src, idx);
            doxa_array_set_i64(result, idx, val);
        }
    }
    return result;
}

pub export fn doxa_array_len(hdr: *ArrayHeader) callconv(.c) u64 {
    return hdr.len;
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
         3 => blk_str: { // string: elem_size 8 = legacy C-string ptr; 16 = DoxaString
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
         3 => { // string: handled by doxa_array_set_str
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
                @ptrFromInt(@as(usize, addr));
        },
        // Default: store raw 64-bit payload (pointers/unknown)
        else => {
            const ip: *i64 = @ptrCast(@alignCast(p));
            ip.* = value;
        },
    }
}

pub export fn doxa_array_get_str(hdr: *ArrayHeader, idx: u64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) void {
    if (hdr.data == null or idx >= hdr.len) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const base: [*]const u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    if (hdr.elem_tag == 3) {
        if (hdr.elem_size >= 16) {
            const ptr_slot: *const i64 = @ptrCast(@alignCast(p));
            const len_slot: *const i64 = @ptrCast(@alignCast(p + 8));
            const addr: u64 = @bitCast(ptr_slot.*);
            const ptr: ?[*]const u8 = if (addr == 0) null else @ptrFromInt(@as(usize, addr));
            const len_u64: u64 = @bitCast(len_slot.*);
            out_ptr.* = @constCast(ptr);
            out_len.* = @intCast(len_u64);
            return;
        }
    }
    out_ptr.* = null;
    out_len.* = 0;
}

pub export fn doxa_array_set_str(hdr: *ArrayHeader, idx: u64, str_ptr: ?[*]const u8, str_len: usize) callconv(.c) void {
    const needed_len = idx + 1;
    if (!ensureArrayCapacity(hdr, needed_len)) return;
    if (hdr.data == null and hdr.elem_size != 0) return;
    if (idx >= hdr.len) hdr.len = idx + 1;
    if (hdr.elem_tag != 3 or hdr.elem_size < 16) return;
    const base: [*]u8 = @ptrCast(hdr.data.?);
    const off: usize = @intCast(idx * hdr.elem_size);
    const p = base + off;
    const ptr_slot: *i64 = @ptrCast(@alignCast(p));
    const len_slot: *i64 = @ptrCast(@alignCast(p + 8));
    const addr: u64 = if (str_ptr) |vp| @intFromPtr(vp) else 0;
    ptr_slot.* = @bitCast(addr);
    len_slot.* = @bitCast(@as(u64, str_len));
}

pub export fn doxa_array_concat(a: ?*ArrayHeader, b: ?*ArrayHeader, elem_size: u64, elem_tag: u64) callconv(.c) *ArrayHeader {
    const len_a: u64 = if (a) |hdr| hdr.len else 0;
    const len_b: u64 = if (b) |hdr| hdr.len else 0;
    const result = doxa_array_new(elem_size, elem_tag, len_a + len_b);

    if (elem_tag == 3 and elem_size >= 16) {
        if (a) |hdr_a| {
            var idx: u64 = 0;
            while (idx < len_a) : (idx += 1) {
                var str_ptr: ?[*]u8 = undefined;
                var str_len: usize = undefined;
                doxa_array_get_str(hdr_a, idx, &str_ptr, &str_len);
                doxa_array_set_str(result, idx, str_ptr, str_len);
            }
        }
        if (b) |hdr_b| {
            var idx: u64 = 0;
            while (idx < len_b) : (idx += 1) {
                var str_ptr: ?[*]u8 = undefined;
                var str_len: usize = undefined;
                doxa_array_get_str(hdr_b, idx, &str_ptr, &str_len);
                doxa_array_set_str(result, len_a + idx, str_ptr, str_len);
            }
        }
    } else {
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
    }

    return result;
}

pub export fn doxa_array_insert(hdr: ?*ArrayHeader, idx: i64, value: i64) callconv(.c) *ArrayHeader {
    const h = hdr orelse return doxa_array_new(8, 0, 0);
    if (idx < 0) return h;
    const pos: u64 = @intCast(@as(u64, @intCast(idx)));
    if (pos > h.len) return h;

    const old_len = h.len;
    if (!ensureArrayCapacity(h, old_len + 1)) return h;
    h.len = old_len + 1;
    if (h.elem_tag == 3 and h.elem_size >= 16) {
        var i: u64 = old_len;
        while (i > pos) : (i -= 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(h, i - 1, &str_ptr, &str_len);
            doxa_array_set_str(h, i, str_ptr, str_len);
        }
    } else {
        var i: u64 = old_len;
        while (i > pos) : (i -= 1) {
            const prev = doxa_array_get_i64(h, i - 1);
            doxa_array_set_i64(h, i, prev);
        }
    }
    doxa_array_set_i64(h, pos, value);
    return h;
}

pub export fn doxa_array_remove(hdr: ?*ArrayHeader, idx: i64, out_removed: *i64) callconv(.c) *ArrayHeader {
    const h = hdr orelse return doxa_array_new(8, 0, 0);
    out_removed.* = 0;
    if (idx < 0) return h;
    const pos: u64 = @intCast(@as(u64, @intCast(idx)));
    if (pos >= h.len) return h;

    out_removed.* = doxa_array_get_i64(h, pos);
    if (h.elem_tag == 3 and h.elem_size >= 16) {
        var i: u64 = pos;
        while (i + 1 < h.len) : (i += 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(h, i + 1, &str_ptr, &str_len);
            doxa_array_set_str(h, i, str_ptr, str_len);
        }
    } else {
        var i: u64 = pos;
        while (i + 1 < h.len) : (i += 1) {
            const next = doxa_array_get_i64(h, i + 1);
            doxa_array_set_i64(h, i, next);
        }
    }
    if (h.len > 0) h.len -= 1;
    return h;
}

pub export fn doxa_array_insert_str(hdr: ?*ArrayHeader, idx: i64, str_ptr: ?[*]const u8, str_len: usize) callconv(.c) *ArrayHeader {
    const h = hdr orelse return doxa_array_new(16, 3, 0);
    if (idx < 0) return h;
    const pos: u64 = @intCast(@as(u64, @intCast(idx)));
    if (pos > h.len) return h;

    const old_len = h.len;
    if (!ensureArrayCapacity(h, old_len + 1)) return h;
    h.len = old_len + 1;
    if (h.elem_tag == 3 and h.elem_size >= 16) {
        var i: u64 = old_len;
        while (i > pos) : (i -= 1) {
            var prev_ptr: ?[*]u8 = undefined;
            var prev_len: usize = undefined;
            doxa_array_get_str(h, i - 1, &prev_ptr, &prev_len);
            doxa_array_set_str(h, i, prev_ptr, prev_len);
        }
        doxa_array_set_str(h, pos, str_ptr, str_len);
    }
    return h;
}

pub export fn doxa_array_remove_str(hdr: ?*ArrayHeader, idx: i64, out_ptr: *?[*]u8, out_len: *usize) callconv(.c) *ArrayHeader {
    const h = hdr orelse return doxa_array_new(16, 3, 0);
    out_ptr.* = null;
    out_len.* = 0;
    if (idx < 0) return h;
    const pos: u64 = @intCast(@as(u64, @intCast(idx)));
    if (pos >= h.len) return h;

    doxa_array_get_str(h, pos, out_ptr, out_len);
    if (h.elem_tag == 3 and h.elem_size >= 16) {
        var i: u64 = pos;
        while (i + 1 < h.len) : (i += 1) {
            var next_ptr: ?[*]u8 = undefined;
            var next_len: usize = undefined;
            doxa_array_get_str(h, i + 1, &next_ptr, &next_len);
            doxa_array_set_str(h, i, next_ptr, next_len);
        }
    }
    if (h.len > 0) h.len -= 1;
    return h;
}

pub export fn doxa_array_slice(hdr: ?*ArrayHeader, start: i64, length: i64) callconv(.c) *ArrayHeader {
    const h = hdr orelse return doxa_array_new(8, 0, 0);
    if (start < 0 or length < 0) return doxa_array_new(h.elem_size, h.elem_tag, 0);
    const s: u64 = @intCast(@as(u64, @intCast(start)));
    const n: u64 = @intCast(@as(u64, @intCast(length)));
    if (s >= h.len or n == 0) return doxa_array_new(h.elem_size, h.elem_tag, 0);
    const max_len = h.len - s;
    const out_len: u64 = if (n > max_len) max_len else n;
    const out = doxa_array_new(h.elem_size, h.elem_tag, out_len);
    if (h.elem_tag == 3 and h.elem_size >= 16) {
        var i: u64 = 0;
        while (i < out_len) : (i += 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(h, s + i, &str_ptr, &str_len);
            doxa_array_set_str(out, i, str_ptr, str_len);
        }
    } else {
        var i: u64 = 0;
        while (i < out_len) : (i += 1) {
            const v = doxa_array_get_i64(h, s + i);
            doxa_array_set_i64(out, i, v);
        }
    }
    return out;
}

pub export fn doxa_clear(collection: ?*anyopaque) callconv(.c) void {
    if (collection == null) return;

    const maybe_hdr = asArrayHeader(collection);
    if (maybe_hdr) |hdr| {
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

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) callconv(.c) void {
    var list = std.array_list.Managed(u8).init(std.heap.page_allocator);
    defer list.deinit();
    const out = list.writer();
    printArrayHdrImpl(out, hdr) catch return;
    doxaWrite(list.items);
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
            const rounded_down = std.math.floor(f);
            if (f - rounded_down == 0)
                try out.print("{d}.0", .{f})
            else
                try out.print("{d}", .{f});
        },
         3 => { // string (i8* + len stored separately in array or as DoxaString)
            const ds = doxaStringFromBits(bits);
            const s = sliceFromDoxaString(ds);
            try out.print("\"{s}\"", .{s});
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

fn printEnumImpl(out: anytype, type_name: []const u8, bits: i64) anyerror!void {
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
            const etn: []const u8 = if (enum_type_names[idx]) |n| std.mem.span(n) else "";
            try printEnumImpl(out, etn, bits);
        } else {
            try printTaggedBitsImpl(out, tag, bits);
        }
        first = false;
    }
    try out.print(" }}", .{});
}

fn printArrayHdrImpl(out: anytype, hdr: *ArrayHeader) anyerror!void {
    try out.print("[", .{});

    if (hdr.len == 0) {
        try out.print("]", .{});
        return;
    }

    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (i != 0) try out.print(", ", .{});
        if (hdr.elem_tag == 3 and hdr.elem_size >= 16) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(hdr, i, &str_ptr, &str_len);
            const s = if (str_ptr) |p| p[0..str_len] else "";
            try out.print("\"{s}\"", .{s});
        } else {
            const elem_bits = doxa_array_get_i64(hdr, i);
            try printTaggedBitsImpl(out, hdr.elem_tag, elem_bits);
        }
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
            const lhs = sliceFromDoxaString(doxaStringFromBits(elem_bits));
            const rhs = sliceFromDoxaString(doxaStringFromBits(comparison_bits));
            break :blk_str std.mem.eql(u8, lhs, rhs);
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
    if (hdr.elem_tag == 3 and hdr.elem_size >= 16) {
        const cs = sliceFromDoxaString(doxaStringFromBits(comparison_bits));
        var idx: u64 = 0;
        while (idx < hdr.len) : (idx += 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(hdr, idx, &str_ptr, &str_len);
            const lhs = if (str_ptr) |p| p[0..str_len] else "";
            if (if (mode == .Equal) std.mem.eql(u8, lhs, cs) else lhs.len > cs.len) {
                return 1;
            }
        }
        return 0;
    }
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
    if (hdr.elem_tag == 3 and hdr.elem_size >= 16) {
        const cs = sliceFromDoxaString(doxaStringFromBits(comparison_bits));
        var idx: u64 = 0;
        while (idx < hdr.len) : (idx += 1) {
            var str_ptr: ?[*]u8 = undefined;
            var str_len: usize = undefined;
            doxa_array_get_str(hdr, idx, &str_ptr, &str_len);
            const lhs = if (str_ptr) |p| p[0..str_len] else "";
            const ok = if (mode == .Equal) std.mem.eql(u8, lhs, cs) else lhs.len > cs.len;
            if (!ok) {
                return 0;
            }
        }
        return 1;
    }
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
            const ds = doxaStringFromBits(val.payload_bits);
            doxa_peek_string(ds.ptr, ds.len);
        },
        .Array => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr == 0) {
                doxaWrite("[]");
            } else {
                const any_ptr: ?*anyopaque = @ptrFromInt(@as(usize, addr));
                const hdr = @as(*ArrayHeader, @ptrCast(@alignCast(any_ptr.?)));
                doxa_print_array_hdr(hdr);
            }
        },
        .Struct => {
            var buf: [1024]u8 = undefined;
            var fbs = std.io.fixedBufferStream(&buf);
            const out = fbs.writer();
            const addr: u64 = @bitCast(val.payload_bits);
            printTaggedBitsImpl(out, 7, @as(i64, @bitCast(addr))) catch return;
            doxaWrite(fbs.getWritten());
        },
        .Enum => {
            // Enum printing is now handled natively by the IR printer
            doxaWrite("<enum>");
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
