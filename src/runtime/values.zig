//! Boxed values: the box registry, member naming, and the tag/payload decoding
//! shared by the renderers.

const std = @import("std");
const abi = @import("abi.zig");

const DoxaValue = abi.DoxaValue;
const DoxaTag = abi.DoxaTag;
const DoxaBoxMeta = abi.DoxaBoxMeta;
const BoxDesc = abi.BoxDesc;

/// Every box type of the program, indexed by box id. Set once at startup.
var box_registry: []const *const BoxDesc = &.{};

pub export fn doxa_box_registry_init(descs: [*]const *const BoxDesc, count: *const u64) callconv(.c) void {
    box_registry = descs[0..@intCast(count.*)];
}

pub fn writeVariant(out: *std.Io.Writer, desc: *const abi.EnumDesc, variant: i64) !void {
    const names = desc.variant_names orelse return out.writeAll(".Unknown");
    if (variant < 0 or variant >= desc.variant_count) return out.writeAll(".Unknown");
    const name = names[@intCast(variant)] orelse return out.writeAll(".Unknown");
    try out.print(".{s}", .{std.mem.span(name)});
}

/// Write a boxed enum as `Type.Variant`, naming it through the box registry.
/// False when the box names no enum member the registry knows.
pub fn writeBoxedEnum(out: *std.Io.Writer, val: *const DoxaValue) !bool {
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

/// The character buffer a `String`-tagged payload points at, or an empty slice
/// for the null sentinel.
pub fn stringPayload(val: *const DoxaValue) []const u8 {
    const addr: u64 = @bitCast(val.payload_bits);
    if (addr == 0) return "";
    const ptr: [*]const u8 = @ptrFromInt(@as(usize, @intCast(addr)));
    return ptr[0..@intCast(val.payload_len)];
}

/// The heap object a payload address names, or null for the 0 sentinel.
pub fn payloadAs(val: *const DoxaValue, comptime T: type) ?*T {
    const addr: u64 = @bitCast(val.payload_bits);
    if (addr == 0) return null;
    const any_ptr: *anyopaque = @ptrFromInt(@as(usize, @intCast(addr)));
    return @ptrCast(@alignCast(any_ptr));
}

pub fn asFloat(bits: i64) f64 {
    return @bitCast(bits);
}

pub fn asByte(bits: i64) u8 {
    const raw: u64 = @bitCast(bits);
    return @intCast(raw & 0xff);
}

pub fn asTetra(bits: i64) u2 {
    const raw: u64 = @bitCast(bits);
    return @intCast(raw & 0x3);
}
