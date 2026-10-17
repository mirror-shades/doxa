const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const layout = @import("../layout.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;

// ── Terminators ──

pub fn terminator(self: *FunctionEmitter, term: *const ir.Terminator) Error!void {
    switch (term.*) {
        .jump => |call| try self.line("br label %{s}", .{try edge(self, call)}),
        .branch => |b| {
            const then = try edge(self, b.then);
            const otherwise = if (b.@"else".block == b.then.block and b.then.args.len != 0) try trampoline(self, b.@"else") else try edge(self, b.@"else");
            try self.line("br i1 {s}, label %{s}, label %{s}", .{ self.name(b.cond), then, otherwise });
        },
        .@"switch" => |s| {
            const ty = layout.doxaType(self.doxaOf(s.operand));
            try self.w.print("  switch {s} {s}, label %{s} [", .{ ty, self.name(s.operand), try edge(self, s.default) });
            for (s.cases) |case| try self.w.print(" {s} {d}, label %{s}", .{ ty, case.value, try edge(self, case.target) });
            try self.w.writeAll(" ]");
            try self.eol();
        },
        .@"return" => |v| if (v) |value| {
            try self.line("ret {s} {s}", .{ layout.doxaType(self.f.ret), self.name(value) });
        } else {
            try self.line("ret void", .{});
        },
        .@"unreachable" => try self.fault(.unreachable_code, null, "0", "0"),
        .panic => |m| try self.fault(.panic, try self.stringParts(self.name(m)), "0", "0"),
        .exit => |code| {
            try self.line("call void @doxa_exit(i64 {s})", .{self.name(code)});
            try self.line("unreachable", .{});
        },
        .assert_fail => |a| try self.fault(.assert_fail, if (a.message) |m| try self.stringParts(self.name(m)) else null, "0", "0"),
    }
}

/// The label control transfers to for `call`, recording its arguments
/// for the target's phis.
pub fn edge(self: *FunctionEmitter, call: ir.BlockCall) Error![]const u8 {
    const target = std.fmt.allocPrint(self.e.alloc, "b{d}", .{@intFromEnum(call.block)}) catch |err| return err;
    if (call.args.len == 0) return target;
    const entry = try self.incoming.getOrPut(self.e.alloc, @intFromEnum(call.block));
    if (!entry.found_existing) entry.value_ptr.* = .empty;
    try entry.value_ptr.append(self.e.alloc, .{ .label = self.label, .args = call.args });
    return target;
}

/// An edge to a block this terminator already targets with other
/// arguments: a block of its own, so each phi entry names one
/// predecessor.
pub fn trampoline(self: *FunctionEmitter, call: ir.BlockCall) Error![]const u8 {
    const label = try self.split("edge");
    const entry = try self.incoming.getOrPut(self.e.alloc, @intFromEnum(call.block));
    if (!entry.found_existing) entry.value_ptr.* = .empty;
    try entry.value_ptr.append(self.e.alloc, .{ .label = label, .args = call.args });
    try self.edges.append(self.e.alloc, try std.fmt.allocPrint(self.e.alloc, "{s}:\n  br label %b{d}\n", .{ label, @intFromEnum(call.block) }));
    return label;
}
