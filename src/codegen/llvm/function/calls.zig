const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const layout = @import("../layout.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

// ── Calls ──

pub fn callDoxa(self: *FunctionEmitter, inst: *const ir.Inst, callee: ir.FunctionId, args: []const ValueId) Error!void {
    const sig = self.e.module.program.functions[@intFromEnum(callee)];
    var text: std.ArrayListUnmanaged(u8) = .empty;
    for (args, 0..) |a, i| {
        if (i != 0) try text.appendSlice(self.e.alloc, ", ");
        try text.print(self.e.alloc, "{s} {s}", .{ layout.llvmType(self.typeOf(a)), self.name(a) });
    }
    // With debug info, the call into the entry goes through
    // `__doxa_enter`, where a backtrace ends (`Emitter.emitEnter`).
    const enters = self.e.debug != null and self.id == self.e.module.init and callee == self.e.module.entry;
    const symbol = if (enters) "@__doxa_enter" else try self.e.functionSymbol(callee);
    if (inst.result) |_| {
        try self.def(inst, "call {s} {s}({s})", .{ layout.returnType(sig.ret), symbol, text.items });
    } else {
        try self.line("call {s} {s}({s})", .{ layout.returnType(sig.ret), symbol, text.items });
    }
}

pub fn callZig(self: *FunctionEmitter, inst: *const ir.Inst, callee: ir.ZigFunctionId, args: []const ValueId) Error!void {
    const sig = self.e.module.program.zig_functions[@intFromEnum(callee)];
    const symbol = try std.fmt.allocPrint(self.e.alloc, "@{s}", .{self.e.module.program.zig_function_names[@intFromEnum(callee)]});
    var text: std.ArrayListUnmanaged(u8) = .empty;
    for (args, sig.roles, 0..) |a, role, i| {
        if (i != 0) try text.appendSlice(self.e.alloc, ", ");
        if (role == .caller_arena) {
            try text.print(self.e.alloc, "ptr {s}", .{self.name(a)});
            continue;
        }
        switch (self.doxaOf(a)) {
            .String => {
                const s = try self.stringParts(self.name(a));
                try text.print(self.e.alloc, "ptr {s}, i64 {s}", .{ s.ptr, s.len });
            },
            // A tetra crosses as a Zig `bool`: true when it holds.
            .Tetra => {
                const hi = try self.tmp("lshr i2 {s}, 1", .{self.name(a)});
                const differ = try self.tmp("xor i2 {s}, {s}", .{ hi, self.name(a) });
                try text.print(self.e.alloc, "i1 {s}", .{try self.tmp("trunc i2 {s} to i1", .{differ})});
            },
            else => |t| try text.print(self.e.alloc, "{s} {s}", .{ layout.doxaType(t), self.name(a) }),
        }
    }
    const sep = if (text.items.len == 0) "" else ", ";
    switch (sig.ret) {
        .Nothing => try self.line("call void {s}({s})", .{ symbol, text.items }),
        .String => {
            const slots = try self.stringSlots();
            try self.line("call void {s}({s}{s}ptr {s}, ptr {s})", .{ symbol, text.items, sep, slots.ptr, slots.len });
            self.names[@intFromEnum(inst.result.?)] = try self.loadString(slots.ptr, slots.len);
        },
        .Tetra => {
            const b = try self.tmp("call i1 {s}({s})", .{ symbol, text.items });
            try self.def(inst, "zext i1 {s} to i2", .{b});
        },
        .Union => try fallibleZig(self, inst, symbol, text.items, sig.ret),
        else => try self.def(inst, "call {s} {s}({s})", .{ layout.doxaType(sig.ret), symbol, text.items }),
    }
}

/// A fallible Zig call (`DoxaError_<path>!<payload>`): the wrapper
/// returns -1 on success, else the error's variant, and writes a payload
/// through out-parameters. The result is the union, boxed either way.
pub fn fallibleZig(self: *FunctionEmitter, inst: *const ir.Inst, symbol: []const u8, args: []const u8, result_type: HIRType) Error!void {
    var error_index: u32 = 0;
    var payload_index: u32 = 0;
    var payload: HIRType = .Nothing;
    for (try self.e.boxMembers(result_type), 0..) |m, i| {
        if (m == .Enum) error_index = @intCast(i) else {
            payload_index = @intCast(i);
            payload = m;
        }
    }
    const sep = if (args.len == 0) "" else ", ";
    var word: []const u8 = "0";
    var len: []const u8 = "0";
    const raw = switch (payload) {
        .Nothing => try self.tmp("call i64 {s}({s})", .{ symbol, args }),
        .String => blk: {
            const slots = try self.stringSlots();
            try self.line("store ptr null, ptr {s}", .{slots.ptr});
            try self.line("store i64 0, ptr {s}", .{slots.len});
            const r = try self.tmp("call i64 {s}({s}{s}ptr {s}, ptr {s})", .{ symbol, args, sep, slots.ptr, slots.len });
            word = try self.tmp("ptrtoint ptr {s} to i64", .{try self.tmp("load ptr, ptr {s}", .{slots.ptr})});
            len = try self.tmp("load i64, ptr {s}", .{slots.len});
            break :blk r;
        },
        .Array, .Int => blk: {
            const slot = try self.alloca(if (payload == .Int) "i64" else "ptr");
            try self.line("store {s} {s}, ptr {s}", .{ if (payload == .Int) "i64" else "ptr", if (payload == .Int) "0" else "null", slot });
            const r = try self.tmp("call i64 {s}({s}{s}ptr {s})", .{ symbol, args, sep, slot });
            word = if (payload == .Int) try self.tmp("load i64, ptr {s}", .{slot}) else try self.tmp("ptrtoint ptr {s} to i64", .{try self.tmp("load ptr, ptr {s}", .{slot})});
            break :blk r;
        },
        else => return self.e.faultf("a fallible Zig call has an unsupported {s} payload", .{@tagName(payload)}),
    };
    const header = try self.e.boxHeader(result_type);
    const failed = try self.tmp("icmp ne i64 {s}, -1", .{raw});
    const tag = try self.tmp("select i1 {s}, i32 {d}, i32 {d}", .{ failed, @intFromEnum(rt.DoxaTag.Enum), @intFromEnum(layout.valueTag(payload)) });
    const reserved = try self.tmp("select i1 {s}, i32 {d}, i32 {d}", .{ failed, header | error_index, header | payload_index });
    const bits = try self.tmp("select i1 {s}, i64 {s}, i64 {s}", .{ failed, raw, word });
    const length = try self.tmp("select i1 {s}, i64 0, i64 {s}", .{ failed, len });
    const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {s}, 0", .{tag});
    const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 {s}, 1", .{ v0, reserved });
    const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, bits });
    try self.def(inst, "insertvalue %DoxaValue {s}, i64 {s}, 3", .{ v2, length });
}
