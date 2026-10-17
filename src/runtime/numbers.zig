//! Scalar text: parsing numbers out of text and formatting them back.

const std = @import("std");
const scope_arena = @import("scope_arena.zig");
const fault = @import("fault.zig");
const io = @import("io.zig");
const strings = @import("strings.zig");

const Scope = scope_arena.Scope;

// ── Parsing numbers from text ──
//
// `@int`, `@float` and `@byte` of a string. Each parser is the language's one
// definition of what text names a number: the exports below trap on text that
// names none, and the compiler's constant evaluator (`consteval.zig`) calls
// the same functions, so a folded conversion and a run one cannot disagree.
// A conversion that must not trap is the standard library's (`std.methods`).

/// The int `text` names: decimal, `0x` hex (either sign), or a decimal float
/// truncated toward zero, between optional whitespace. Null for anything else.
pub fn parseIntText(text: []const u8) ?i64 {
    const trimmed = std.mem.trim(u8, text, whitespace);
    if (hexDigits(trimmed)) |hex| {
        const magnitude = std.fmt.parseInt(i64, hex.digits, 16) catch return null;
        return if (hex.negative) -magnitude else magnitude;
    }
    if (std.mem.indexOfScalar(u8, trimmed, '.') != null) {
        return truncateToInt(std.fmt.parseFloat(f64, trimmed) catch return null);
    }
    return std.fmt.parseInt(i64, trimmed, 10) catch null;
}

/// The float `text` names: a decimal float (`inf` and `nan` included) or
/// `0x` hex, between optional whitespace. Null for anything else.
pub fn parseFloatText(text: []const u8) ?f64 {
    const trimmed = std.mem.trim(u8, text, whitespace);
    if (hexDigits(trimmed)) |hex| {
        const magnitude = std.fmt.parseInt(i64, hex.digits, 16) catch return null;
        const value: f64 = @floatFromInt(magnitude);
        return if (hex.negative) -value else value;
    }
    if (trimmed.len == 0) return null;
    return std.fmt.parseFloat(f64, trimmed) catch null;
}

/// The byte `text` names: a single character is its own code; otherwise a
/// decimal, `0x` hex, or decimal float truncated toward zero, in 0–255. Null
/// for anything else.
pub fn parseByteText(text: []const u8) ?u8 {
    if (text.len == 1) return text[0];
    if (text.len > 2 and std.mem.eql(u8, text[0..2], "0x")) {
        return std.fmt.parseInt(u8, text[2..], 16) catch null;
    }
    const wide = std.fmt.parseInt(i64, text, 10) catch
        truncateToInt(std.fmt.parseFloat(f64, text) catch return null) orelse return null;
    return std.math.cast(u8, wide);
}

const whitespace = " \t\r\n";

const HexText = struct { negative: bool, digits: []const u8 };

fn hexDigits(text: []const u8) ?HexText {
    const negative = text.len > 0 and text[0] == '-';
    const body = text[@intFromBool(negative)..];
    if (body.len < 2 or body[0] != '0' or (body[1] != 'x' and body[1] != 'X')) return null;
    return .{ .negative = negative, .digits = body[2..] };
}

/// The int a float truncates to, when it has one.
pub fn truncateToInt(value: f64) ?i64 {
    // 2^63 is exact as a double; every finite value strictly inside
    // (-2^63 - 1, 2^63) truncates to an i64.
    const limit: f64 = 9223372036854775808.0;
    if (!(value > -limit - 1.0 and value < limit)) return null;
    return @intFromFloat(value);
}

/// Text that names no number of the kind `site` converts to.
fn faultUnparsable(site: *const fault.DoxaFaultSite, text: []const u8) noreturn {
    fault.doxa_fault(site, text.ptr, text.len, 0, 0);
}

pub export fn doxa_int_from_string(ptr: ?[*]const u8, len: u64, site: *const fault.DoxaFaultSite) callconv(.c) i64 {
    const text = strings.sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    return parseIntText(text) orelse faultUnparsable(site, text);
}

pub export fn doxa_float_from_string(ptr: ?[*]const u8, len: u64, site: *const fault.DoxaFaultSite) callconv(.c) f64 {
    const text = strings.sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    return parseFloatText(text) orelse faultUnparsable(site, text);
}

pub export fn doxa_int_to_string(scope: *Scope, value: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{value}) catch {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    };
    const ds = strings.allocDoxaString(scope, s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

/// The longest text `formatFloat` produces: a sign, "0.", the 323 zeros
/// before the smallest subnormal's first digit, and 17 significant digits.
/// The longest integral value (309 digits and ".0") fits well beneath it.
pub const max_float_text_len = 1 + 2 + 323 + 17;

/// A float's text: the shortest positional decimal that reads back as the same
/// value, with ".0" on an integral one so a float never reads as an int.
pub fn formatFloat(buf: *[max_float_text_len]u8, value: f64) []const u8 {
    const integral = value - std.math.floor(value) == 0;
    const rendered = if (integral)
        std.fmt.bufPrint(buf, "{d}.0", .{value})
    else
        std.fmt.bufPrint(buf, "{d}", .{value});
    return rendered catch unreachable;
}

pub export fn doxa_float_to_string(scope: *Scope, value: f64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    var buf: [max_float_text_len]u8 = undefined;
    const ds = strings.allocDoxaString(scope, formatFloat(&buf, value));
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
    const ds = strings.allocDoxaString(scope, s);
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_tetra_to_string(scope: *Scope, value: i64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const ds = strings.allocDoxaString(scope, tetraName(@intCast(value & 0x3)));
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub export fn doxa_nothing_to_string(scope: *Scope, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void {
    const ds = strings.allocDoxaString(scope, "nothing");
    out_ptr.* = @constCast(ds.ptr);
    out_len.* = ds.len;
}

pub fn tetraName(t: u2) []const u8 {
    return switch (t) {
        0 => "false",
        1 => "true",
        2 => "both",
        3 => "neither",
    };
}

pub export fn doxa_print_i64(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    io.doxaWrite(rendered);
}

pub export fn doxa_print_u64(value: u64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    io.doxaWrite(rendered);
}

pub export fn doxa_print_f64(value: f64) callconv(.c) void {
    var buf: [max_float_text_len]u8 = undefined;
    io.doxaWrite(formatFloat(&buf, value));
}

pub export fn doxa_print_tetra(value: i64) callconv(.c) void {
    io.doxaWrite(tetraName(@intCast(value & 0x3)));
}

pub export fn doxa_print_byte(value: i64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    const byte_val: u8 = @intCast(value & 0xff);
    const rendered = std.fmt.bufPrint(&buf, "0x{X:0>2}", .{byte_val}) catch return;
    io.doxaWrite(rendered);
}

pub export fn doxa_int(value: f64) callconv(.c) i64 {
    return @intFromFloat(value);
}

pub export fn doxa_byte_from_string(ptr: ?[*]const u8, len: u64, site: *const fault.DoxaFaultSite) callconv(.c) i64 {
    const text = strings.sliceFromDoxaString(.{ .ptr = ptr, .len = len });
    return parseByteText(text) orelse faultUnparsable(site, text);
}

pub export fn doxa_byte_from_f64(value: f64) callconv(.c) i64 {
    if (!std.math.isFinite(value)) return 0;
    const rounded: i64 = @intFromFloat(value);
    if (rounded >= 0 and rounded <= 255) return rounded;
    return 0;
}
