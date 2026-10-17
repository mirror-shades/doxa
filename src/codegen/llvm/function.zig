//! Lowers one register-HIR function to LLVM IR. Each HIR block becomes one
//! LLVM block, labelled `b<n>`; a lowering that branches (a trap, a guard)
//! continues in `b<n>.<k>`, and the label a block ends in is what its
//! successors' phis name. A block parameter is one `phi`; every value one SSA
//! name, `%v<n>`, or an inline constant.
//!
//! The lowering groups live under `function/`; this file holds the state, the
//! per-function run, and the helpers every group shares.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const rt = @import("../../runtime/doxa_rt.zig");
const layout = @import("layout.zig");
const int_range = @import("int_range.zig");
const emit_mod = @import("emit.zig");
const ranges_mod = @import("ranges.zig");
const instructions = @import("function/instructions.zig");
const instrument = @import("function/instrument.zig");
const copies = @import("function/copies.zig");
const terminators = @import("function/terminators.zig");

const Emitter = emit_mod.Emitter;
pub const Error = emit_mod.Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;
const BlockId = ir.BlockId;
const IntRange = int_range.IntRange;
const Writer = std.Io.Writer;

pub fn emitFunction(e: *Emitter, w: *Writer, id: ir.FunctionId, f: *const ir.Function) Error!void {
    var fx = FunctionEmitter{
        .e = e,
        .f = f,
        .id = id,
        .names = try e.alloc.alloc([]const u8, f.values.len),
        .ranges = try e.ranges.function(id),
        .bodies = try e.alloc.alloc(std.Io.Writer.Allocating, f.blocks.len),
        .exit_labels = try e.alloc.alloc([]const u8, f.blocks.len),
        .entry = std.Io.Writer.Allocating.init(e.alloc),
    };
    try fx.run(w);
}

pub const FunctionEmitter = struct {
    e: *Emitter,
    f: *const ir.Function,
    id: ir.FunctionId,
    names: [][]const u8,
    ranges: []const IntRange,
    bodies: []std.Io.Writer.Allocating,
    /// The label each block ends in, for its successors' phis.
    exit_labels: [][]const u8,
    /// Allocas, placed at the top of the entry block.
    entry: std.Io.Writer.Allocating,
    /// Trampoline blocks, for an edge that needs one.
    edges: std.ArrayListUnmanaged([]const u8) = .empty,
    /// Incoming phi entries by target block: (label, args).
    incoming: std.AutoHashMapUnmanaged(u32, std.ArrayListUnmanaged(Incoming)) = .empty,
    next_temp: u32 = 0,
    next_split: u32 = 0,
    block: BlockId = .entry,
    label: []const u8 = "b0",
    /// The current block's writer.
    w: *Writer = undefined,
    str_slots: ?struct { ptr: []const u8, len: []const u8 } = null,
    /// The span of the instruction or terminator being lowered: where a
    /// fault it raises happened.
    span: ir.SrcSpan = undefined,
    /// Each slot's alloca, made zeroed at the top of the entry block.
    slot_storage: []const []const u8 = &.{},
    /// By value: a copy only stored inline (`storedInline`), which the
    /// store's own byte copy makes.
    inline_only: []const bool = &.{},
    /// Under `-g`: the function's subprogram, the statement whose key
    /// instruction is still to be written, and each location made so far.
    scope: ?u32 = null,
    head: ?u32 = null,
    locations: std.AutoHashMapUnmanaged(Location, u32) = .empty,

    const Location = struct { line: u32, column: u32, key_stmt: ?u32 };

    const Incoming = struct { label: []const u8, args: []const ValueId };

    fn run(self: *FunctionEmitter, out: *Writer) Error!void {
        const f = self.f;
        for (f.params(), 0..) |p, i| self.names[@intFromEnum(p)] = try std.fmt.allocPrint(self.e.alloc, "%p{d}", .{i});
        for (f.values, 0..) |v, i| {
            if (v.def == .param and v.def.param.block == .entry) continue;
            self.names[i] = try std.fmt.allocPrint(self.e.alloc, "%v{d}", .{i});
        }
        const storage = try self.e.alloc.alloc([]const u8, f.slots.len);
        for (f.slots, storage) |slot, *s| {
            s.* = try self.alloca(layout.doxaType(slot.ty));
            try self.entry.writer.print("  store {s} zeroinitializer, ptr {s}\n", .{ layout.doxaType(slot.ty), s.* });
        }
        self.slot_storage = storage;
        self.inline_only = try copies.storedInline(self);
        // `__doxa_init` is entered once, by the runtime: no call of the program's.
        if (self.e.instrument.calls and self.id != self.e.module.init) try instrument.countSite(self, &self.entry.writer, try self.e.site("call", f, f.span));
        if (self.e.debug) |*d| self.scope = try d.subprogram(f, self.e.module.program.function_names[@intFromEnum(self.id)]);
        // A named slot is declared on its alloca: reads and writes through the
        // slot are then visible without a `#dbg_value`.
        if (self.e.debug) |*d| {
            for (f.locals, 0..) |local, i| {
                const slot = local.slot orelse continue;
                const var_node = try d.localVariable(f, self.id, @enumFromInt(i), .{ .doxa = f.slots[@intFromEnum(slot)].ty }, self.scope.?, local.arg);
                const loc = try d.location(self.scope.?, f, f.span, false);
                try self.entry.writer.print("  call void @llvm.dbg.declare(metadata ptr {s}, metadata !{d}, metadata !DIExpression()), !dbg !{d}\n", .{ storage[@intFromEnum(slot)], var_node, loc });
            }
        }
        // Constants are operands, not instructions.
        for (f.blocks) |block| {
            for (block.insts) |inst| {
                if (inst.op == .constant) self.names[@intFromEnum(inst.result.?)] = try self.constantText(inst.op.constant, f.typeOf(inst.result.?));
            }
        }

        // Bodies are lowered in reverse postorder, so a definition is named
        // before any use of it; they are written in block order.
        for (try ranges_mod.reversePostorder(self.e.alloc, f)) |block_id| {
            const b = @intFromEnum(block_id);
            const block = &f.blocks[b];
            self.bodies[b] = std.Io.Writer.Allocating.init(self.e.alloc);
            self.block = block_id;
            self.label = try std.fmt.allocPrint(self.e.alloc, "b{d}", .{b});
            self.w = &self.bodies[b].writer;
            for (block.insts) |*inst| {
                self.setSpan(inst.span);
                try instrument.countLine(self, inst.span);
                try instrument.traceSite(self, inst);
                try instrument.checkReads(self, inst);
                try instructions.lower(self, inst);
            }
            self.setSpan(block.term_span);
            try instrument.countLine(self, block.term_span);
            try terminators.terminator(self, &block.term);
            self.exit_labels[b] = self.label;
        }

        // The function.
        try out.print("\ndefine {s} {s}(", .{ layout.returnType(f.ret), try self.e.functionSymbol(self.id) });
        for (f.params(), 0..) |p, i| {
            if (i != 0) try out.writeAll(", ");
            try out.print("{s} {s}", .{ layout.llvmType(f.typeOf(p)), self.names[@intFromEnum(p)] });
        }
        try out.writeAll(") #0");
        if (self.scope) |scope| try out.print(" !dbg !{d}", .{scope});
        try out.writeAll(" {\n");
        for (f.blocks, 0..) |block, b| {
            try out.print("b{d}:\n", .{b});
            // The entry block's parameters are the function's, not phis.
            if (b == 0) {
                try out.writeAll(self.entry.written());
                try out.writeAll(self.bodies[b].written());
                continue;
            }
            for (block.params, 0..) |p, i| {
                try out.print("  {s} = phi {s} ", .{ self.names[@intFromEnum(p)], layout.llvmType(f.typeOf(p)) });
                const list = self.incoming.get(@intCast(b)) orelse return self.e.faultf("{s}: block b{d} has parameters but no predecessor", .{ f.name, b });
                for (list.items, 0..) |in, k| {
                    if (k != 0) try out.writeAll(", ");
                    try out.print("[ {s}, %{s} ]", .{ self.names[@intFromEnum(in.args[i])], in.label });
                }
                try out.writeAll("\n");
            }
            try out.writeAll(self.bodies[b].written());
        }
        for (self.edges.items) |text| try out.writeAll(text);
        try out.writeAll("}\n");
    }

    // ── Helpers ──

    pub fn name(self: *const FunctionEmitter, v: ValueId) []const u8 {
        return self.names[@intFromEnum(v)];
    }

    pub fn typeOf(self: *const FunctionEmitter, v: ValueId) ir.Type {
        return self.f.typeOf(v);
    }

    pub fn doxaOf(self: *const FunctionEmitter, v: ValueId) HIRType {
        return self.f.typeOf(v).doxa;
    }

    pub fn temp(self: *FunctionEmitter) Error![]const u8 {
        defer self.next_temp += 1;
        return std.fmt.allocPrint(self.e.alloc, "%t{d}", .{self.next_temp});
    }

    /// Lower what follows at `span`. A statement's head makes the next
    /// instruction written its key instruction.
    fn setSpan(self: *FunctionEmitter, span: ir.SrcSpan) void {
        self.span = span;
        if (span.is_stmt) self.head = span.stmt;
    }

    /// End an instruction's line, with its location under `-g`.
    pub fn eol(self: *FunctionEmitter) Error!void {
        const scope = self.scope orelse return self.w.writeAll("\n");
        var key: Location = .{ .line = self.span.line, .column = self.span.column, .key_stmt = null };
        if (self.span.artificial) {
            key = .{ .line = 0, .column = 0, .key_stmt = null };
        } else if (self.head == self.span.stmt) {
            key.key_stmt = self.span.stmt;
            self.head = null;
        }
        const entry = try self.locations.getOrPut(self.e.alloc, key);
        if (!entry.found_existing) entry.value_ptr.* = try self.e.debug.?.location(scope, self.f, self.span, key.key_stmt != null);
        try self.w.print(", !dbg !{d}\n", .{entry.value_ptr.*});
    }

    pub fn line(self: *FunctionEmitter, comptime fmt: []const u8, args: anytype) Error!void {
        try self.w.print("  " ++ fmt, args);
        try self.eol();
    }

    /// Define the result of `inst` as `rhs`.
    pub fn def(self: *FunctionEmitter, inst: *const ir.Inst, comptime fmt: []const u8, args: anytype) Error!void {
        try self.w.print("  {s} = ", .{self.name(inst.result.?)});
        try self.w.print(fmt, args);
        try self.eol();
    }

    /// A fresh temporary defined as `rhs`.
    pub fn tmp(self: *FunctionEmitter, comptime fmt: []const u8, args: anytype) Error![]const u8 {
        const t = try self.temp();
        try self.w.print("  {s} = " ++ fmt, .{t} ++ args);
        try self.eol();
        return t;
    }

    /// Start a new LLVM block continuing the current HIR block.
    pub fn split(self: *FunctionEmitter, comptime tag: []const u8) Error![]const u8 {
        defer self.next_split += 1;
        return std.fmt.allocPrint(self.e.alloc, "b{d}." ++ tag ++ ".{d}", .{ @intFromEnum(self.block), self.next_split });
    }

    pub fn enter(self: *FunctionEmitter, label: []const u8) Error!void {
        try self.w.print("{s}:\n", .{label});
        self.label = label;
    }

    /// An alloca in the entry block.
    pub fn alloca(self: *FunctionEmitter, ty: []const u8) Error![]const u8 {
        const t = try self.temp();
        try self.entry.writer.print("  {s} = alloca {s}, align 8\n", .{ t, ty });
        return t;
    }

    pub fn stringSlots(self: *FunctionEmitter) Error!struct { ptr: []const u8, len: []const u8 } {
        if (self.str_slots) |s| return .{ .ptr = s.ptr, .len = s.len };
        const p = try self.alloca("ptr");
        const l = try self.alloca("i64");
        self.str_slots = .{ .ptr = p, .len = l };
        return .{ .ptr = p, .len = l };
    }

    /// Call a runtime function that writes a string through `(out_ptr,
    /// out_len)` after `args`, and assemble the `%DoxaString`.
    pub fn stringCall(self: *FunctionEmitter, func: []const u8, comptime args_fmt: []const u8, args: anytype) Error![]const u8 {
        const slots = try self.stringSlots();
        try self.w.print("  call void @{s}(" ++ args_fmt, .{func} ++ args);
        try self.w.print("{s}ptr {s}, ptr {s})", .{ if (args_fmt.len == 0) "" else ", ", slots.ptr, slots.len });
        try self.eol();
        return self.loadString(slots.ptr, slots.len);
    }

    pub fn loadString(self: *FunctionEmitter, ptr_slot: []const u8, len_slot: []const u8) Error![]const u8 {
        const p = try self.tmp("load ptr, ptr {s}", .{ptr_slot});
        const l = try self.tmp("load i64, ptr {s}", .{len_slot});
        return self.makeString(p, l);
    }

    pub fn makeString(self: *FunctionEmitter, p: []const u8, l: []const u8) Error![]const u8 {
        const s0 = try self.tmp("insertvalue %DoxaString undef, ptr {s}, 0", .{p});
        return self.tmp("insertvalue %DoxaString {s}, i64 {s}, 1", .{ s0, l });
    }

    pub const Parts = struct { ptr: []const u8, len: []const u8 };

    /// End the current block in a `kind` fault at the current span, with the
    /// site's text (a message, or text that failed to parse) and the two
    /// numbers an out-of-bounds fault reports.
    pub fn fault(self: *FunctionEmitter, kind: rt.FaultKind, text: ?Parts, aux0: []const u8, aux1: []const u8) Error!void {
        const site = try self.e.faultSite(kind, self.span);
        const t = text orelse Parts{ .ptr = "null", .len = "0" };
        try self.line("call void @doxa_fault(ptr {s}, ptr {s}, i64 {s}, i64 {s}, i64 {s})", .{ site, t.ptr, t.len, aux0, aux1 });
        try self.line("unreachable", .{});
    }

    pub fn stringParts(self: *FunctionEmitter, s: []const u8) Error!Parts {
        return .{
            .ptr = try self.tmp("extractvalue %DoxaString {s}, 0", .{s}),
            .len = try self.tmp("extractvalue %DoxaString {s}, 1", .{s}),
        };
    }

    /// `v`, a `%DoxaValue`, in memory, for a runtime call that takes it by
    /// address.
    pub fn spillValue(self: *FunctionEmitter, v: []const u8) Error![]const u8 {
        const slot = try self.alloca("%DoxaValue");
        try self.line("store %DoxaValue {s}, ptr {s}", .{ v, slot });
        return slot;
    }

    fn constantText(self: *FunctionEmitter, c: ir.Constant, ty: ir.Type) Error![]const u8 {
        const a = self.e.alloc;
        return switch (c) {
            .int => |v| std.fmt.allocPrint(a, "{d}", .{v}),
            .byte => |v| std.fmt.allocPrint(a, "{d}", .{v}),
            // A double as its bit pattern: exact for every value.
            .float => |v| std.fmt.allocPrint(a, "0x{X:0>16}", .{@as(u64, @bitCast(v))}),
            .tetra => |v| std.fmt.allocPrint(a, "{d}", .{@intFromEnum(v)}),
            .nothing => "0",
            .enum_variant => |v| std.fmt.allocPrint(a, "{d}", .{v}),
            .string => |s| self.e.stringConstant(s),
            .function => |fid| self.e.functionSymbol(fid),
        } catch |err| {
            _ = ty;
            return err;
        };
    }

    /// `#dbg_value`: the value a named binding holds here. Nothing without
    /// `-g`.
    pub fn debugValue(self: *FunctionEmitter, local: ir.LocalId, value: ValueId) Error!void {
        const d = if (self.e.debug) |*d| d else return;
        const var_node = try d.localVariable(self.f, self.id, local, self.f.typeOf(value), self.scope.?, self.f.locals[@intFromEnum(local)].arg);
        try self.w.print("  call void @llvm.dbg.value(metadata {s}, metadata !{d}, metadata !DIExpression()), !dbg !{d}\n", .{ try self.valueMetadata(value), var_node, try self.debugLocation() });
    }

    /// The current span's `!DILocation`, interned as `eol` does. A debug
    /// record needs a location, but it is never a step's key instruction.
    fn debugLocation(self: *FunctionEmitter) Error!u32 {
        const d = if (self.e.debug) |*d| d else unreachable;
        const key = Location{ .line = self.span.line, .column = self.span.column, .key_stmt = null };
        const entry = try self.locations.getOrPut(self.e.alloc, key);
        if (!entry.found_existing) entry.value_ptr.* = try d.location(self.scope.?, self.f, self.span, false);
        return entry.value_ptr.*;
    }

    /// The metadata operand a `#dbg_value` names. LLVM's assembler wants the
    /// type spelled with the value; a constant has no SSA name, so its text
    /// stands in for one.
    fn valueMetadata(self: *FunctionEmitter, value: ValueId) Error![]const u8 {
        const ty = self.f.typeOf(value);
        const definition = self.f.values[@intFromEnum(value)].def;
        const d = switch (definition) {
            .param => return std.fmt.allocPrint(self.e.alloc, "{s} {s}", .{ layout.llvmType(ty), self.name(value) }),
            .inst => |d| d,
        };
        const inst = self.f.blocks[@intFromEnum(d.block)].insts[d.index];
        if (std.meta.activeTag(inst.op) != .constant) return std.fmt.allocPrint(self.e.alloc, "{s} {s}", .{ layout.llvmType(ty), self.name(value) });
        return std.fmt.allocPrint(self.e.alloc, "{s} {s}", .{ layout.llvmType(ty), try self.constantText(inst.op.constant, ty) });
    }

    pub fn defString(self: *FunctionEmitter, inst: *const ir.Inst, s: []const u8) Error!void {
        self.names[@intFromEnum(inst.result.?)] = s;
    }
};
