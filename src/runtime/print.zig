//! Rendering: the textual form of every runtime value, both into an arena
//! (`@string`) and to the program's output sink.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const fault = @import("fault.zig");
const io = @import("io.zig");
const numbers = @import("numbers.zig");
const values = @import("values.zig");
const arrays = @import("arrays.zig");
const structs = @import("structs.zig");
const abi = @import("abi.zig");

const Scope = scope_arena.Scope;
const ArrayHeader = abi.ArrayHeader;
const DoxaValue = abi.DoxaValue;
const DoxaTag = abi.DoxaTag;
const VALUE_TAG = abi.VALUE_TAG;

pub export fn doxa_enum_to_string(scope: *Scope, desc: *const abi.EnumDesc, bits: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    renderInto(scope, values.writeVariant, .{ desc, bits }, out_ptr, out_len);
}

pub export fn doxa_struct_to_string(scope: *Scope, instance: ?*anyopaque, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (instance == null) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    renderInto(scope, printStructImpl, .{@as(u64, @intFromPtr(instance.?))}, out_ptr, out_len);
}

pub export fn doxa_array_to_string(scope: *Scope, hdr: ?*ArrayHeader, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    if (hdr == null) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    renderInto(scope, printArrayHdrImpl, .{hdr.?}, out_ptr, out_len);
}

/// Render a boxed `DoxaValue` as a string in `scope`, dispatching on its
/// runtime tag: the buffer-returning half of `doxa_print_value`. Only the
/// `Int` tag stores an integer in `payload_bits`, so reading it without first
/// asking the tag would format whatever the payload happens to hold — a string
/// arm would be rendered as the address of its characters.
pub export fn doxa_value_to_string(scope: *Scope, val: *const DoxaValue, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    renderInto(scope, writeValue, .{val}, out_ptr, out_len);
}

/// Render `render(writer, args...)` straight into `scope`'s arena: the text is
/// built where it lives, grown in place while it is the arena's newest
/// allocation and trimmed to its length after, so every byte is counted to
/// the site that asked for it and running out of memory is the arena's fault.
fn renderInto(scope: *Scope, render: anytype, args: anytype, out_ptr: *?[*]u8, out_len: *u64) void {
    var text: std.Io.Writer.Allocating = .init(scope_arena.allocator(scope));
    @call(.auto, render, .{&text.writer} ++ args) catch fault.outOfMemory();
    const bytes = text.toOwnedSlice() catch fault.outOfMemory();
    out_ptr.* = if (bytes.len == 0) null else bytes.ptr;
    out_len.* = bytes.len;
}

/// Write a box's text: what `@string` of it yields.
fn writeValue(out: *std.Io.Writer, val: *const DoxaValue) anyerror!void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .Int => try out.print("{d}", .{val.payload_bits}),
        .Float => {
            var buf: [numbers.max_float_text_len]u8 = undefined;
            try out.writeAll(numbers.formatFloat(&buf, values.asFloat(val.payload_bits)));
        },
        .Byte => try out.print("0x{X:0>2}", .{values.asByte(val.payload_bits)}),
        .Tetra => try out.writeAll(numbers.tetraName(values.asTetra(val.payload_bits))),
        .Nothing => try out.writeAll("nothing"),
        .String => try out.writeAll(values.stringPayload(val)),
        .Array => if (values.payloadAs(val, ArrayHeader)) |hdr| try printArrayHdrImpl(out, hdr),
        .Struct => if (values.payloadAs(val, anyopaque)) |instance| try printStructImpl(out, @intFromPtr(instance)),
        // A boxed enum is named through the box registry; a bare
        // discriminant the IR printer could not name shows its number.
        .Enum => if (!try values.writeBoxedEnum(out, val)) try out.print("<enum:{d}>", .{val.payload_bits}),
        .Function => try out.writeAll("<function>"),
    }
}

/// Print a variant of the enum `desc` describes, as `.Variant`.
pub export fn doxa_print_enum(desc: *const abi.EnumDesc, variant: i64) callconv(.c) void {
    var buf: [256]u8 = undefined;
    var out: std.Io.Writer = .fixed(&buf);
    values.writeVariant(&out, desc, variant) catch return;
    io.doxaWrite(out.buffered());
}

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) callconv(.c) void {
    var list = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer list.deinit();
    printArrayHdrImpl(&list.writer, hdr) catch return;
    io.doxaWrite(list.written());
}

fn printTaggedBitsImpl(out: *std.Io.Writer, tag: u64, bits: i64) anyerror!void {
    switch (tag) {
        0 => { // int (i64)
            try out.print("{d}", .{bits});
        },
        1 => { // byte (u8)
            const b: u8 = values.asByte(bits);
            try out.print("{d}", .{b});
        },
        2 => { // float (f64)
            var buf: [numbers.max_float_text_len]u8 = undefined;
            try out.writeAll(numbers.formatFloat(&buf, values.asFloat(bits)));
        },
        4 => { // tetra (2-bit stored in u8)
            try out.writeAll(numbers.tetraName(values.asTetra(bits)));
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

fn printStructImpl(out: *std.Io.Writer, addr: u64) anyerror!void {
    const key: usize = @intCast(addr);
    const desc = structs.structDescOf(@ptrFromInt(key));

    const field_count: usize = @intCast(desc.field_count);
    const names = if (desc.field_names) |p| p[0..field_count] else &[_]?[*:0]const u8{};
    const tags = if (desc.field_tags) |p| p[0..field_count] else &[_]u64{};
    const enums = if (desc.field_enums) |p| p[0..field_count] else &[_]?*const abi.EnumDesc{};
    const fields: [*]const i64 = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(key))));

    try out.print("{{ ", .{});
    var first: bool = true;
    var word: usize = structs.structTotalWords(tags, field_count);
    var idx: usize = field_count;
    while (idx > 0) {
        idx -= 1;
        const tag: u64 = if (idx < tags.len) tags[idx] else 255;
        word -= structs.structFieldWordCount(tag);
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
            try io.writeEscaped(out, s);
            try out.writeAll("\"");
        } else if (tag == 8 and idx < enums.len) {
            if (enums[idx]) |ed| try values.writeVariant(out, ed, bits) else try out.print("<enum:{d}>", .{bits});
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
        try io.writeEscaped(out, values.stringPayload(value));
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
            arrays.doxa_array_get_str(hdr, i, &str_ptr, &str_len);
            const s = if (str_ptr) |p| p[0..@intCast(str_len)] else "";
            try out.writeAll("\"");
            try io.writeEscaped(out, s);
            try out.writeAll("\"");
        } else if (hdr.elem_tag == VALUE_TAG) {
            var value: DoxaValue = undefined;
            arrays.doxa_array_get_value(hdr, i, &value);
            try printValueElement(out, &value);
        } else {
            const elem_bits = arrays.doxa_array_get_i64(hdr, i);
            try printTaggedBitsImpl(out, hdr.elem_tag, elem_bits);
        }
    }

    try out.print("]", .{});
}

/// Print a value described by the canonical `DoxaValue` layout. This is the
/// primary entry point for native code that renders values via the runtime.
pub export fn doxa_print_value(val: *const DoxaValue) callconv(.c) void {
    const tag: DoxaTag = @enumFromInt(val.tag);
    switch (tag) {
        .Int => {
            numbers.doxa_print_i64(val.payload_bits);
        },
        .Float => {
            const f = values.asFloat(val.payload_bits);
            numbers.doxa_print_f64(f);
        },
        .Byte => {
            const b: i64 = @intCast(values.asByte(val.payload_bits));
            numbers.doxa_print_byte(b);
        },
        .String => {
            const addr: u64 = @bitCast(val.payload_bits);
            const len: usize = @intCast(val.payload_len);
            const ptr: ?[*]const u8 = if (addr == 0) null else @ptrFromInt(@as(usize, @intCast(addr)));
            io.doxa_peek_string(ptr, len);
        },
        .Array => {
            const addr: u64 = @bitCast(val.payload_bits);
            if (addr == 0) {
                io.doxaWrite("[]");
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
            io.doxaWrite(fbs.buffered());
        },
        .Enum => {
            var buf: [256]u8 = undefined;
            var fbs: std.Io.Writer = .fixed(&buf);
            if (values.writeBoxedEnum(&fbs, val) catch false) {
                io.doxaWrite(fbs.buffered());
            } else {
                printTaggedBitsImpl(&fbs, 8, val.payload_bits) catch return;
                io.doxaWrite(fbs.buffered());
            }
        },
        .Tetra => {
            io.doxaWrite(numbers.tetraName(values.asTetra(val.payload_bits)));
        },
        .Nothing => {
            // TODO: render Nothing with proper textual payload
            io.doxaWrite("nothing");
        },
        .Function => {
            io.doxaWrite("<function>");
        },
    }
}
