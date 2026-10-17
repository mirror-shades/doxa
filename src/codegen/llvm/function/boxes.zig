const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const layout = @import("../layout.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

// ── Boxes ──

/// Box a member as `boxed`: its runtime tag, the box header with the
/// member's index, and its payload (a scalar's bits, a pointer, or a
/// string's pointer and length).
pub fn box(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
    const boxed = self.f.typeOf(inst.result.?).doxa;
    const member = self.doxaOf(operand);
    const index = try memberIndex(self, boxed, member);
    const header = try self.e.boxHeader(boxed);
    const payload = try payloadWords(self, self.name(operand), member);
    const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {d}, 0", .{@intFromEnum(layout.valueTag(member))});
    const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 {d}, 1", .{ v0, header | index });
    const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, payload.bits });
    try self.def(inst, "insertvalue %DoxaValue {s}, i64 {s}, 3", .{ v2, payload.len });
}

pub fn memberIndex(self: *FunctionEmitter, boxed: HIRType, member: HIRType) Error!u32 {
    for (try self.e.boxMembers(boxed), 0..) |m, i| {
        if (m.eql(member)) return @intCast(i);
    }
    return self.e.faultf("boxes a {s}, which its box type does not hold", .{@tagName(member)});
}

const Payload = struct { bits: []const u8, len: []const u8 };

pub fn payloadWords(self: *FunctionEmitter, v: []const u8, t: HIRType) Error!Payload {
    return switch (t) {
        .String => blk: {
            const s = try self.stringParts(v);
            break :blk .{ .bits = try self.tmp("ptrtoint ptr {s} to i64", .{s.ptr}), .len = s.len };
        },
        else => .{ .bits = try storageBits(self, v, t), .len = "0" },
    };
}

/// A value of `t` as one `i64` word: a scalar's bits, a pointer's
/// address. What a struct field, a map entry or a box payload stores.
pub fn storageBits(self: *FunctionEmitter, v: []const u8, t: HIRType) Error![]const u8 {
    return switch (t) {
        .Int, .Enum => v,
        .Byte => self.tmp("zext i8 {s} to i64", .{v}),
        .Tetra => self.tmp("zext i2 {s} to i64", .{v}),
        .Float => self.tmp("bitcast double {s} to i64", .{v}),
        .Nothing => "0",
        .Array, .Struct, .Function => self.tmp("ptrtoint ptr {s} to i64", .{v}),
        .String, .Union, .Group, .Unknown, .Poison => self.e.faultf("stores a {s} as one word", .{@tagName(t)}),
    };
}

/// The value of `t` an `i64` word holds: the inverse of `storageBits`.
pub fn fromBits(self: *FunctionEmitter, bits: []const u8, t: HIRType) Error![]const u8 {
    return switch (t) {
        .Int, .Enum => bits,
        .Byte => self.tmp("trunc i64 {s} to i8", .{bits}),
        .Tetra => self.tmp("trunc i64 {s} to i2", .{bits}),
        .Float => self.tmp("bitcast i64 {s} to double", .{bits}),
        .Nothing => "0",
        .Array, .Struct, .Function => self.tmp("inttoptr i64 {s} to ptr", .{bits}),
        .String, .Union, .Group, .Unknown, .Poison => self.e.faultf("reads a {s} from one word", .{@tagName(t)}),
    };
}

pub fn unbox(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
    const member = self.f.typeOf(inst.result.?).doxa;
    const v = self.name(operand);
    if (member.isBoxed()) {
        // A narrowing to a smaller box re-packs it.
        return repack(self, inst, operand);
    }
    const bits = try self.tmp("extractvalue %DoxaValue {s}, 2", .{v});
    if (member == .String) {
        const p = try self.tmp("inttoptr i64 {s} to ptr", .{bits});
        const len = try self.tmp("extractvalue %DoxaValue {s}, 3", .{v});
        const s = try self.makeString(p, len);
        self.names[@intFromEnum(inst.result.?)] = s;
        return;
    }
    const value = try fromBits(self, bits, member);
    if (member == .Nothing) {
        self.names[@intFromEnum(inst.result.?)] = "0";
        return;
    }
    self.names[@intFromEnum(inst.result.?)] = value;
}

/// Re-pack a box for another box type holding the same member: the
/// member index is rewritten to the target's index for that member.
pub fn repack(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
    const source = self.doxaOf(operand);
    const target = self.f.typeOf(inst.result.?).doxa;
    const header = try self.e.boxHeader(target);
    const v = self.name(operand);
    const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{v});
    const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
    var acc: ?[]const u8 = null;
    const target_members = try self.e.boxMembers(target);
    for (try self.e.boxMembers(source), 0..) |m, source_index| {
        const target_index = for (target_members, 0..) |t, ti| {
            if (t.eql(m)) break ti;
        } else continue;
        const word = header | @as(u32, @intCast(target_index));
        if (acc) |previous| {
            const is = try self.tmp("icmp eq i32 {s}, {d}", .{ index, source_index });
            acc = try self.tmp("select i1 {s}, i32 {d}, i32 {s}", .{ is, word, previous });
        } else {
            acc = try std.fmt.allocPrint(self.e.alloc, "{d}", .{word});
        }
    }
    const repacked = acc orelse return self.e.faultf("re-packs a box as a type holding none of its members", .{});
    try self.def(inst, "insertvalue %DoxaValue {s}, i32 {s}, 1", .{ v, repacked });
}

/// Whether a box holds one of `members`.
pub fn memberTest(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId, members: []const u32) Error!void {
    const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{self.name(operand)});
    const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
    var acc: []const u8 = "false";
    for (members, 0..) |m, i| {
        const is = try self.tmp("icmp eq i32 {s}, {d}", .{ index, m });
        acc = if (i == 0) is else try self.tmp("or i1 {s}, {s}", .{ acc, is });
    }
    self.names[@intFromEnum(inst.result.?)] = acc;
}
