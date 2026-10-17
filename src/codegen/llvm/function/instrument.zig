const std = @import("std");
const ir = @import("../../hir/register/ir.zig");
const loops = @import("../../hir/register/loops.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const ValueId = ir.ValueId;
const Writer = std.Io.Writer;

/// Under `--instrument=lines`, a statement's head counts the statement,
/// at its own start. A statement whose head the HIR does not mark (one
/// lowering folded into its neighbour) has no counter of its own.
pub fn countLine(self: *FunctionEmitter, span: ir.SrcSpan) Error!void {
    if (!self.e.instrument.lines or !span.is_stmt) return;
    try countSite(self, self.w, try self.e.site("line", self.f, self.f.statements[span.stmt]));
}

/// Under `--instrument=memtrace`, name the site whose runtime work this
/// instruction is: what it allocates, stores into a container, opens or
/// re-homes counts to it, nested allocations included.
pub fn traceSite(self: *FunctionEmitter, inst: *const ir.Inst) Error!void {
    if (!self.e.instrument.memtrace) return;
    const kind: []const u8 = switch (inst.op) {
        .scope_enter => "arena",
        .rehome => "rehome",
        .clone => "clone",
        .call_zig => |c| if (self.e.module.zig_takes_arena[@intFromEnum(c.callee)]) "zig" else return,
        else => if (loops.allocArena(inst.op) != null) "alloc" else if (loops.storeInto(inst.op) != null) "store" else return,
    };
    const id = try self.e.site(kind, self.f, inst.span);
    try self.w.print("  store i32 {d}, ptr @doxa_memtrace_site\n", .{id});
}

/// Under `--instrument=memcheck`, check each heap value `inst` reads
/// through (a string's bytes, an array, a struct, a map) before it does:
/// a value in memory its arena reclaimed faults here. Passing, storing or
/// boxing a pointer reads nothing through it, so those are not checked;
/// the read that follows is.
pub fn checkReads(self: *FunctionEmitter, inst: *const ir.Inst) Error!void {
    if (!self.e.instrument.memcheck) return;
    switch (inst.op) {
        .debug_value, .store, .global_store, .call, .call_zig, .box, .repack, .unbox, .member_test, .member_index, .constant => return,
        else => {},
    }
    const Visit = struct {
        fx: *FunctionEmitter,
        fn operand(ctx: @This(), v: ValueId) Error!void {
            const t = switch (ctx.fx.typeOf(v)) {
                .doxa => |t| t,
                else => return,
            };
            const ptr = switch (t) {
                .String => (try ctx.fx.stringParts(ctx.fx.name(v))).ptr,
                .Array, .Struct => ctx.fx.name(v),
                else => return,
            };
            const site = try ctx.fx.e.faultSite(.use_after_reclaim, ctx.fx.span);
            try ctx.fx.line("call void @doxa_memcheck(ptr {s}, ptr {s})", .{ ptr, site });
        }
    };
    try ir.forEachOperand(&inst.op, Visit{ .fx = self }, Visit.operand);
}

/// Increment site `id`'s counter, written to `w`. The increment carries
/// no location: the statement's key instruction is still the next one
/// with a line.
pub fn countSite(self: *FunctionEmitter, w: *Writer, id: u32) Error!void {
    const counter = try std.fmt.allocPrint(self.e.alloc, "getelementptr inbounds (i64, ptr @.doxa.counts, i64 {d})", .{id});
    const old = try self.temp();
    const new = try self.temp();
    try w.print("  {s} = load i64, ptr {s}, align 8\n  {s} = add i64 {s}, 1\n  store i64 {s}, ptr {s}, align 8\n", .{ old, counter, new, old, new, counter });
}
