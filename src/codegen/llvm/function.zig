//! Lowers one register-HIR function to LLVM IR. Each HIR block becomes one
//! LLVM block, labelled `b<n>`; a lowering that branches (a trap, a guard)
//! continues in `b<n>.<k>`, and the label a block ends in is what its
//! successors' phis name. A block parameter is one `phi`; every value one SSA
//! name, `%v<n>`, or an inline constant.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const rt = @import("../../runtime/doxa_rt.zig");
const layout = @import("layout.zig");
const int_range = @import("int_range.zig");
const emit_mod = @import("emit.zig");
const ranges_mod = @import("ranges.zig");

const Emitter = emit_mod.Emitter;
const Error = emit_mod.Error;
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

const FunctionEmitter = struct {
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
    /// Each slot's alloca, made zeroed at the top of the entry block.
    slot_storage: []const []const u8 = &.{},

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
            for (block.insts) |*inst| try self.instruction(inst);
            try self.terminator(&block.term);
            self.exit_labels[b] = self.label;
        }

        // The function.
        try out.print("\ndefine {s} {s}(", .{ layout.returnType(f.ret), try self.e.functionSymbol(self.id) });
        for (f.params(), 0..) |p, i| {
            if (i != 0) try out.writeAll(", ");
            try out.print("{s} {s}", .{ layout.llvmType(f.typeOf(p)), self.names[@intFromEnum(p)] });
        }
        try out.writeAll(") #0 {\n");
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

    fn name(self: *const FunctionEmitter, v: ValueId) []const u8 {
        return self.names[@intFromEnum(v)];
    }

    fn typeOf(self: *const FunctionEmitter, v: ValueId) ir.Type {
        return self.f.typeOf(v);
    }

    fn doxaOf(self: *const FunctionEmitter, v: ValueId) HIRType {
        return self.f.typeOf(v).doxa;
    }

    fn temp(self: *FunctionEmitter) Error![]const u8 {
        defer self.next_temp += 1;
        return std.fmt.allocPrint(self.e.alloc, "%t{d}", .{self.next_temp});
    }

    fn line(self: *FunctionEmitter, comptime fmt: []const u8, args: anytype) Error!void {
        try self.w.print("  " ++ fmt ++ "\n", args);
    }

    /// Define the result of `inst` as `rhs`.
    fn def(self: *FunctionEmitter, inst: *const ir.Inst, comptime fmt: []const u8, args: anytype) Error!void {
        try self.w.print("  {s} = ", .{self.name(inst.result.?)});
        try self.w.print(fmt ++ "\n", args);
    }

    /// A fresh temporary defined as `rhs`.
    fn tmp(self: *FunctionEmitter, comptime fmt: []const u8, args: anytype) Error![]const u8 {
        const t = try self.temp();
        try self.w.print("  {s} = " ++ fmt ++ "\n", .{t} ++ args);
        return t;
    }

    /// Start a new LLVM block continuing the current HIR block.
    fn split(self: *FunctionEmitter, comptime tag: []const u8) Error![]const u8 {
        defer self.next_split += 1;
        return std.fmt.allocPrint(self.e.alloc, "b{d}." ++ tag ++ ".{d}", .{ @intFromEnum(self.block), self.next_split });
    }

    fn enter(self: *FunctionEmitter, label: []const u8) Error!void {
        try self.w.print("{s}:\n", .{label});
        self.label = label;
    }

    /// An alloca in the entry block.
    fn alloca(self: *FunctionEmitter, ty: []const u8) Error![]const u8 {
        const t = try self.temp();
        try self.entry.writer.print("  {s} = alloca {s}, align 8\n", .{ t, ty });
        return t;
    }

    fn stringSlots(self: *FunctionEmitter) Error!struct { ptr: []const u8, len: []const u8 } {
        if (self.str_slots) |s| return .{ .ptr = s.ptr, .len = s.len };
        const p = try self.alloca("ptr");
        const l = try self.alloca("i64");
        self.str_slots = .{ .ptr = p, .len = l };
        return .{ .ptr = p, .len = l };
    }

    /// Call a runtime function that writes a string through `(out_ptr,
    /// out_len)` after `args`, and assemble the `%DoxaString`.
    fn stringCall(self: *FunctionEmitter, func: []const u8, comptime args_fmt: []const u8, args: anytype) Error![]const u8 {
        const slots = try self.stringSlots();
        try self.w.print("  call void @{s}(" ++ args_fmt, .{func} ++ args);
        try self.w.print("{s}ptr {s}, ptr {s})\n", .{ if (args_fmt.len == 0) "" else ", ", slots.ptr, slots.len });
        return self.loadString(slots.ptr, slots.len);
    }

    fn loadString(self: *FunctionEmitter, ptr_slot: []const u8, len_slot: []const u8) Error![]const u8 {
        const p = try self.tmp("load ptr, ptr {s}", .{ptr_slot});
        const l = try self.tmp("load i64, ptr {s}", .{len_slot});
        return self.makeString(p, l);
    }

    fn makeString(self: *FunctionEmitter, p: []const u8, l: []const u8) Error![]const u8 {
        const s0 = try self.tmp("insertvalue %DoxaString undef, ptr {s}, 0", .{p});
        return self.tmp("insertvalue %DoxaString {s}, i64 {s}, 1", .{ s0, l });
    }

    const Parts = struct { ptr: []const u8, len: []const u8 };

    fn stringParts(self: *FunctionEmitter, s: []const u8) Error!Parts {
        return .{
            .ptr = try self.tmp("extractvalue %DoxaString {s}, 0", .{s}),
            .len = try self.tmp("extractvalue %DoxaString {s}, 1", .{s}),
        };
    }

    /// `v`, a `%DoxaValue`, in memory, for a runtime call that takes it by
    /// address.
    fn spillValue(self: *FunctionEmitter, v: []const u8) Error![]const u8 {
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

    // ── Instructions ──

    fn instruction(self: *FunctionEmitter, inst: *const ir.Inst) Error!void {
        switch (inst.op) {
            .constant => {},
            .arith => |a| try self.arith(inst, a.op, a.lhs, a.rhs),
            .neg => |u| switch (self.doxaOf(u.operand)) {
                .Float => try self.def(inst, "fneg double {s}", .{self.name(u.operand)}),
                .Int => self.names[@intFromEnum(inst.result.?)] = try self.intArith(.Sub, "0", self.name(u.operand), .exact(0), self.ranges[@intFromEnum(u.operand)]),
                .Byte => try self.def(inst, "sub i8 0, {s}", .{self.name(u.operand)}),
                else => return self.e.faultf("negates a {s}", .{@tagName(self.doxaOf(u.operand))}),
            },
            .convert => |u| try self.convert(inst, u.operand),
            .cmp => |c| try self.compare(inst, c.op, c.lhs, c.rhs),
            .tetra_binary => |t| {
                const lut = switch (t.op) {
                    .@"and" => "@tetra_and_lut",
                    .@"or" => "@tetra_or_lut",
                    .iff => "@tetra_iff_lut",
                    .xor => "@tetra_xor_lut",
                    .nand => "@tetra_nand_lut",
                    .nor => "@tetra_nor_lut",
                    .implies => "@tetra_implies_lut",
                };
                const l = try self.tmp("zext i2 {s} to i64", .{self.name(t.lhs)});
                const r = try self.tmp("zext i2 {s} to i64", .{self.name(t.rhs)});
                const slot = try self.tmp("getelementptr inbounds [4 x [4 x i8]], ptr {s}, i64 0, i64 {s}, i64 {s}", .{ lut, l, r });
                const byte = try self.tmp("load i8, ptr {s}", .{slot});
                try self.def(inst, "trunc i8 {s} to i2", .{byte});
            },
            // not: false <-> true; `both` and `neither` are their own
            // negation (`docs/tetras.md`: not `both` still holds).
            .tetra_not => |u| {
                const two_valued = try self.tmp("icmp ult i2 {s}, -2", .{self.name(u.operand)});
                const flip = try self.tmp("zext i1 {s} to i2", .{two_valued});
                try self.def(inst, "xor i2 {s}, {s}", .{ self.name(u.operand), flip });
            },
            .tetra_from_cond => |u| try self.def(inst, "zext i1 {s} to i2", .{self.name(u.operand)}),
            // `true` (1) and `both` (2) hold: exactly the values whose two bits differ.
            .tetra_holds => |u| {
                const hi = try self.tmp("lshr i2 {s}, 1", .{self.name(u.operand)});
                const differ = try self.tmp("xor i2 {s}, {s}", .{ hi, self.name(u.operand) });
                try self.def(inst, "trunc i2 {s} to i1", .{differ});
            },
            .cond_not => |u| try self.def(inst, "xor i1 {s}, true", .{self.name(u.operand)}),
            .cond_binary => |c| try self.def(inst, "{s} i1 {s}, {s}", .{ if (c.op == .@"and") "and" else "or", self.name(c.lhs), self.name(c.rhs) }),

            .box => |u| try self.box(inst, u.operand),
            .unbox => |u| try self.unbox(inst, u.operand),
            .repack => |u| try self.repack(inst, u.operand),
            .member_test => |m| try self.memberTest(inst, m.operand, m.members),
            .member_index => |u| {
                const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{self.name(u.operand)});
                const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
                try self.def(inst, "zext i32 {s} to i64", .{index});
            },

            .str_concat => |s| {
                const a = try self.stringParts(self.name(s.lhs));
                const b = try self.stringParts(self.name(s.rhs));
                try self.defString(inst, try self.stringCall("doxa_str_concat", "ptr {s}, ptr {s}, i64 {s}, ptr {s}, i64 {s}", .{ self.name(s.arena), a.ptr, a.len, b.ptr, b.len }));
            },
            .str_len => |u| try self.def(inst, "extractvalue %DoxaString {s}, 1", .{self.name(u.operand)}),
            .str_substring => |s| {
                const a = try self.stringParts(self.name(s.string));
                try self.defString(inst, try self.stringCall("doxa_substring", "ptr {s}, ptr {s}, i64 {s}, i64 {s}, i64 {s}", .{ self.name(s.arena), a.ptr, a.len, self.name(s.start), self.name(s.length) }));
            },
            .str_last, .str_drop_last => |s| {
                const a = try self.stringParts(self.name(s.string));
                const rest = try self.alloca("%DoxaString");
                const last = try self.alloca("%DoxaString");
                _ = try self.tmp("call i8 @doxa_str_pop(ptr {s}, ptr {s}, i64 {s}, ptr {s}, ptr {s})", .{ self.name(s.arena), a.ptr, a.len, rest, last });
                try self.def(inst, "load %DoxaString, ptr {s}", .{if (inst.op == .str_last) last else rest});
            },
            .str_find => |s| {
                const a = try self.stringParts(self.name(s.string));
                const b = try self.stringParts(self.name(s.needle));
                try self.def(inst, "call i64 @doxa_find_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len });
            },
            .str_insert => |s| {
                const a = try self.stringParts(self.name(s.string));
                const b = try self.stringParts(self.name(s.insert));
                try self.defString(inst, try self.stringCall("doxa_str_insert", "ptr {s}, ptr {s}, i64 {s}, i64 {s}, ptr {s}, i64 {s}", .{ self.name(s.arena), a.ptr, a.len, self.name(s.index), b.ptr, b.len }));
            },
            .str_remove => |s| {
                const a = try self.stringParts(self.name(s.string));
                const rest = try self.alloca("%DoxaString");
                const removed = try self.alloca("%DoxaString");
                _ = try self.tmp("call i8 @doxa_str_remove(ptr {s}, ptr {s}, i64 {s}, i64 {s}, ptr {s}, ptr {s})", .{ self.name(s.arena), a.ptr, a.len, self.name(s.index), rest, removed });
                try self.def(inst, "load %DoxaString, ptr {s}", .{rest});
            },
            .str_char => |s| {
                const a = try self.stringParts(self.name(s.string));
                const at = try self.tmp("getelementptr inbounds i8, ptr {s}, i64 {s}", .{ a.ptr, self.name(s.index) });
                const byte = try self.tmp("load i8, ptr {s}", .{at});
                try self.defString(inst, try self.stringCall("doxa_char_to_string", "ptr {s}, i8 {s}", .{ self.name(s.arena), byte }));
            },
            .str_to_int, .str_to_float, .str_to_byte => |u| {
                const a = try self.stringParts(self.name(u.operand));
                switch (inst.op) {
                    .str_to_int => try self.def(inst, "call i64 @doxa_int_from_string(ptr {s}, i64 {s})", .{ a.ptr, a.len }),
                    .str_to_float => try self.def(inst, "call double @doxa_float_from_string(ptr {s}, i64 {s})", .{ a.ptr, a.len }),
                    else => {
                        const wide = try self.tmp("call i64 @doxa_byte_from_string(ptr {s}, i64 {s})", .{ a.ptr, a.len });
                        try self.def(inst, "trunc i64 {s} to i8", .{wide});
                    },
                }
            },
            .str_pack => |s| try self.defString(inst, try self.stringCall("doxa_pack_bytes", "ptr {s}, ptr {s}", .{ self.name(s.arena), self.name(s.bytes) })),
            .str_unpack => |s| {
                const a = try self.stringParts(self.name(s.string));
                try self.def(inst, "call ptr @doxa_unpack_bytes(ptr {s}, ptr {s}, i64 {s})", .{ self.name(s.arena), a.ptr, a.len });
            },
            .to_string => |s| try self.defString(inst, try self.toString(self.name(s.arena), s.operand)),

            .array_new => |a| try self.arrayNew(inst, a.arena, a.length),
            .array_from_fixed => |a| try self.arrayFromFixed(inst, a.arena, a.operand),
            .array_range => |a| try self.def(inst, "call ptr @doxa_array_range(ptr {s}, i64 {s}, i64 {s})", .{ self.name(a.arena), self.name(a.start), self.name(a.end) }),
            .array_get => |a| try self.arrayGet(inst, a.array, a.index),
            .array_set => |a| try self.arraySet(a.array, a.index, a.value),
            .array_len => |u| {
                const t = self.doxaOf(u.operand);
                if (t.Array.size) |n| {
                    try self.def(inst, "add i64 0, {d}", .{n});
                } else {
                    const slot = try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 1", .{self.name(u.operand)});
                    try self.def(inst, "load i64, ptr {s}", .{slot});
                }
            },
            .array_push => |a| {
                const len = try self.arrayLen(self.name(a.array));
                try self.arraySetDynamic(self.name(a.array), len, a.value);
            },
            .array_pop => |u| try self.arrayPop(inst, u.operand),
            .array_insert => |a| try self.arrayInsert(a.array, a.index, a.value),
            .array_remove => |a| try self.arrayRemove(inst, a.array, a.index),
            .array_clear => |u| try self.line("call void @doxa_array_clear(ptr {s})", .{self.name(u.operand)}),
            .array_slice => |a| try self.def(inst, "call ptr @doxa_array_slice(ptr {s}, ptr {s}, i64 {s}, i64 {s})", .{ self.name(a.arena), self.name(a.array), self.name(a.start), self.name(a.length) }),
            .array_concat => |a| try self.def(inst, "call ptr @doxa_array_concat(ptr {s}, ptr {s}, ptr {s})", .{ self.name(a.arena), self.name(a.lhs), self.name(a.rhs) }),
            .array_copy_to_fixed => |c| try self.arrayCopyToFixed(c.fixed, c.array),
            .array_find => |a| {
                const element = self.doxaOf(a.array).Array.element.*;
                if (element == .String) {
                    const n = try self.stringParts(self.name(a.value));
                    try self.def(inst, "call i64 @doxa_find_array_str(ptr {s}, ptr {s}, i64 {s})", .{ self.name(a.array), n.ptr, n.len });
                } else {
                    const bits = try self.storageBits(self.name(a.value), element);
                    try self.def(inst, "call i64 @doxa_find_array(ptr {s}, i64 {s})", .{ self.name(a.array), bits });
                }
            },

            .map_new => |m| try self.mapNew(inst, m.arena, m.else_value),
            .map_get => |m| try self.mapGet(inst, m.map, m.key),
            .map_set => |m| try self.mapSet(m.map, m.key, m.value),

            .struct_new => |s| try self.structNew(inst, s.arena, s.fields),
            .field_get => |g| try self.fieldGet(inst, g.object, g.index),
            .field_set => |s| try self.fieldSet(s.object, s.index, s.value),

            // A slot's address is its alloca.
            .slot_addr => |s| self.names[@intFromEnum(inst.result.?)] = self.slot_storage[@intFromEnum(s)],
            .load => |u| try self.def(inst, "load {s}, ptr {s}", .{ layout.doxaType(self.typeOf(u.operand).ref), self.name(u.operand) }),
            .store => |s| try self.line("store {s} {s}, ptr {s}", .{ layout.doxaType(self.doxaOf(s.value)), self.name(s.value), self.name(s.ref) }),
            .global_load => |g| try self.def(inst, "load {s}, ptr {s}", .{ layout.doxaType(self.e.module.program.globals[@intFromEnum(g)].ty), try self.e.globalSymbol(g) }),
            .global_addr => |g| self.names[@intFromEnum(inst.result.?)] = try self.e.globalSymbol(g.global),
            .global_store => |g| try self.line("store {s} {s}, ptr {s}", .{ layout.doxaType(self.e.module.program.globals[@intFromEnum(g.global)].ty), self.name(g.value), try self.e.globalSymbol(g.global) }),

            .root_arena => try self.def(inst, "call ptr @doxa_scope_root()", .{}),
            .scope_enter => |s| try self.def(inst, "call ptr @doxa_scope_enter(ptr {s})", .{self.name(s.parent)}),
            .scope_exit => |s| try self.line("call void @doxa_scope_exit(ptr {s})", .{self.name(s.arena)}),
            .scope_reset => |s| {
                try self.line("call void @doxa_scope_reset(ptr {s})", .{self.name(s.arena)});
                // The reset arena is the same scope, emptied.
                self.names[@intFromEnum(inst.result.?)] = self.name(s.arena);
            },
            .clone => |c| try self.copy(inst, c.arena, c.operand, .clone),
            .rehome => |c| try self.copy(inst, c.arena, c.operand, .rehome),

            .call => |c| try self.callDoxa(inst, c.callee, c.args),
            .call_zig => |c| try self.callZig(inst, c.callee, c.args),

            .print => |u| {
                const s = try self.stringParts(self.name(u.operand));
                try self.line("call void @doxa_write_cstr(ptr {s}, i64 {s})", .{ s.ptr, s.len });
            },
            .peek => |p| try self.peek(p.operand, p.display),
        }
    }

    fn defString(self: *FunctionEmitter, inst: *const ir.Inst, s: []const u8) Error!void {
        self.names[@intFromEnum(inst.result.?)] = s;
    }

    // ── Numbers ──

    fn arith(self: *FunctionEmitter, inst: *const ir.Inst, op: ir.ArithOp, lhs: ValueId, rhs: ValueId) Error!void {
        const l = self.name(lhs);
        const r = self.name(rhs);
        switch (self.doxaOf(lhs)) {
            .Int => {
                const lr = self.ranges[@intFromEnum(lhs)];
                const rr = self.ranges[@intFromEnum(rhs)];
                const result = switch (op) {
                    .Add, .Sub, .Mul => try self.intArith(op, l, r, lr, rr),
                    .IntDiv => try self.flooredDiv(l, r, int_range.signFacts(lr, rr)),
                    .Mod => try self.flooredMod(l, r, int_range.signFacts(lr, rr)),
                    .Div => blk: {
                        try self.divisorGuard(r, int_range.signFacts(lr, rr));
                        break :blk try self.tmp("sdiv i64 {s}, {s}", .{ l, r });
                    },
                    .Pow => blk: {
                        const a = try self.tmp("sitofp i64 {s} to double", .{l});
                        const b = try self.tmp("sitofp i64 {s} to double", .{r});
                        const p = try self.tmp("call double @llvm.pow.f64(double {s}, double {s})", .{ a, b });
                        break :blk try self.tmp("fptosi double {s} to i64", .{p});
                    },
                };
                self.names[@intFromEnum(inst.result.?)] = result;
            },
            .Float => switch (op) {
                .Add => try self.def(inst, "fadd double {s}, {s}", .{ l, r }),
                .Sub => try self.def(inst, "fsub double {s}, {s}", .{ l, r }),
                .Mul => try self.def(inst, "fmul double {s}, {s}", .{ l, r }),
                .Div => try self.def(inst, "fdiv double {s}, {s}", .{ l, r }),
                .Mod => try self.def(inst, "frem double {s}, {s}", .{ l, r }),
                .Pow => try self.def(inst, "call double @llvm.pow.f64(double {s}, double {s})", .{ l, r }),
                .IntDiv => return self.e.faultf("floor-divides floats", .{}),
            },
            // A byte is unsigned and its arithmetic wraps mod 256.
            .Byte => switch (op) {
                .Add => try self.def(inst, "add i8 {s}, {s}", .{ l, r }),
                .Sub => try self.def(inst, "sub i8 {s}, {s}", .{ l, r }),
                .Mul => try self.def(inst, "mul i8 {s}, {s}", .{ l, r }),
                .Div, .IntDiv => try self.def(inst, "udiv i8 {s}, {s}", .{ l, r }),
                .Mod => try self.def(inst, "urem i8 {s}, {s}", .{ l, r }),
                .Pow => try self.def(inst, "call i8 @doxa.byte.pow(i8 {s}, i8 {s})", .{ l, r }),
            },
            else => return self.e.faultf("arithmetic on a {s}", .{@tagName(self.doxaOf(lhs))}),
        }
    }

    /// Signed `add`, `sub` or `mul`: bare under `wrap` or when the ranges
    /// prove it fits, else checked with a real trap on overflow.
    fn intArith(self: *FunctionEmitter, op: ir.ArithOp, l: []const u8, r: []const u8, lr: IntRange, rr: IntRange) Error![]const u8 {
        const bare = switch (op) {
            .Add => "add",
            .Sub => "sub",
            .Mul => "mul",
            else => unreachable,
        };
        if (self.e.overflow == .wrap or int_range.intArithCannotOverflow(op, lr, rr)) {
            return self.tmp("{s} i64 {s}, {s}", .{ bare, l, r });
        }
        const pair = try self.tmp("call {{ i64, i1 }} @llvm.s{s}.with.overflow.i64(i64 {s}, i64 {s})", .{ if (op == .Mul) "mul" else bare, l, r });
        const result = try self.tmp("extractvalue {{ i64, i1 }} {s}, 0", .{pair});
        const overflowed = try self.tmp("extractvalue {{ i64, i1 }} {s}, 1", .{pair});
        const trap = try self.split("ovf.trap");
        const cont = try self.split("ovf.cont");
        try self.line("br i1 {s}, label %{s}, label %{s}", .{ overflowed, trap, cont });
        try self.enter(trap);
        try self.line("call void @llvm.trap()", .{});
        try self.line("unreachable", .{});
        try self.enter(cont);
        return result;
    }

    /// Trap when a divisor the ranges do not prove non-zero is zero:
    /// `sdiv`/`srem` by zero is undefined in LLVM.
    fn divisorGuard(self: *FunctionEmitter, divisor: []const u8, facts: int_range.SignFacts) Error!void {
        if (facts.divisorNonZero()) return;
        const zero = try self.tmp("icmp eq i64 {s}, 0", .{divisor});
        const trap = try self.split("div0.trap");
        const cont = try self.split("div0.cont");
        try self.line("br i1 {s}, label %{s}, label %{s}", .{ zero, trap, cont });
        try self.enter(trap);
        try self.line("call void @doxa_trap_div_by_zero()", .{});
        try self.line("unreachable", .{});
        try self.enter(cont);
    }

    fn correction(self: *FunctionEmitter, remainder: []const u8, dividend: []const u8, divisor: []const u8, dividend_sign_decided: bool) Error![]const u8 {
        const non_zero = try self.tmp("icmp ne i64 {s}, 0", .{remainder});
        const sign = if (dividend_sign_decided) dividend else try self.tmp("xor i64 {s}, {s}", .{ dividend, divisor });
        const negative = try self.tmp("icmp slt i64 {s}, 0", .{sign});
        return self.tmp("and i1 {s}, {s}", .{ non_zero, negative });
    }

    /// Floored `%` in the cheapest exact shape the facts allow
    /// (`int_range.planModulo`).
    fn flooredMod(self: *FunctionEmitter, dividend: []const u8, divisor: []const u8, facts: int_range.SignFacts) Error![]const u8 {
        switch (int_range.planModulo(facts)) {
            .urem_const => |mag| return self.tmp("urem i64 {s}, {d}", .{ dividend, mag }),
            .urem_const_negated => |mag| {
                const residue = try self.tmp("urem i64 {s}, {d}", .{ dividend, mag });
                const non_zero = try self.tmp("icmp ne i64 {s}, 0", .{residue});
                const adjust = try self.tmp("select i1 {s}, i64 {d}, i64 0", .{ non_zero, mag });
                return self.tmp("sub i64 {s}, {s}", .{ residue, adjust });
            },
            .srem_bare => {
                try self.divisorGuard(divisor, facts);
                return self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
            },
            .fixup_dividend_sign, .fixup_general => {
                try self.divisorGuard(divisor, facts);
                const residue = try self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
                const cond = try self.correction(residue, dividend, divisor, int_range.planModulo(facts) == .fixup_dividend_sign);
                const adjust = try self.tmp("select i1 {s}, i64 {s}, i64 0", .{ cond, divisor });
                return self.tmp("add i64 {s}, {s}", .{ residue, adjust });
            },
        }
    }

    /// Floored `//` (`int_range.planDiv`).
    fn flooredDiv(self: *FunctionEmitter, dividend: []const u8, divisor: []const u8, facts: int_range.SignFacts) Error![]const u8 {
        switch (int_range.planDiv(facts)) {
            .shift_floor => |k| {
                const residue = try self.tmp("urem i64 {s}, {d}", .{ dividend, @as(u64, 1) << k });
                const cleared = try self.tmp("sub i64 {s}, {s}", .{ dividend, residue });
                return self.tmp("ashr i64 {s}, {d}", .{ cleared, k });
            },
            .sdiv_bare => {
                try self.divisorGuard(divisor, facts);
                return self.tmp("sdiv i64 {s}, {s}", .{ dividend, divisor });
            },
            .fixup_dividend_sign, .fixup_general => {
                try self.divisorGuard(divisor, facts);
                const quotient = try self.tmp("sdiv i64 {s}, {s}", .{ dividend, divisor });
                const residue = try self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
                const cond = try self.correction(residue, dividend, divisor, int_range.planDiv(facts) == .fixup_dividend_sign);
                const adjust = try self.tmp("zext i1 {s} to i64", .{cond});
                return self.tmp("sub i64 {s}, {s}", .{ quotient, adjust });
            },
        }
    }

    fn convert(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
        const from = self.doxaOf(operand);
        const to = self.f.typeOf(inst.result.?).doxa;
        const v = self.name(operand);
        switch (to) {
            .Float => switch (from) {
                .Int => try self.def(inst, "sitofp i64 {s} to double", .{v}),
                .Byte => try self.def(inst, "uitofp i8 {s} to double", .{v}),
                else => return self.e.faultf("converts a {s} to a float", .{@tagName(from)}),
            },
            .Int => switch (from) {
                .Byte => try self.def(inst, "zext i8 {s} to i64", .{v}),
                .Float => try self.def(inst, "fptosi double {s} to i64", .{v}),
                else => return self.e.faultf("converts a {s} to an int", .{@tagName(from)}),
            },
            .Byte => switch (from) {
                .Int => try self.def(inst, "trunc i64 {s} to i8", .{v}),
                .Float => {
                    const wide = try self.tmp("call i64 @doxa_byte_from_f64(double {s})", .{v});
                    try self.def(inst, "trunc i64 {s} to i8", .{wide});
                },
                else => return self.e.faultf("converts a {s} to a byte", .{@tagName(from)}),
            },
            else => return self.e.faultf("converts to a {s}", .{@tagName(to)}),
        }
    }

    fn compare(self: *FunctionEmitter, inst: *const ir.Inst, op: ir.CompareOp, lhs: ValueId, rhs: ValueId) Error!void {
        const l = self.name(lhs);
        const r = self.name(rhs);
        const signed = [_][]const u8{ "eq", "ne", "slt", "sle", "sgt", "sge" };
        const unsigned = [_][]const u8{ "eq", "ne", "ult", "ule", "ugt", "uge" };
        const ordered = [_][]const u8{ "oeq", "une", "olt", "ole", "ogt", "oge" };
        const i = @intFromEnum(op);
        switch (self.doxaOf(lhs)) {
            .Int, .Enum => try self.def(inst, "icmp {s} i64 {s}, {s}", .{ signed[i], l, r }),
            .Byte => try self.def(inst, "icmp {s} i8 {s}, {s}", .{ unsigned[i], l, r }),
            .Tetra => try self.def(inst, "icmp {s} i2 {s}, {s}", .{ signed[i], l, r }),
            .Float => try self.def(inst, "fcmp {s} double {s}, {s}", .{ ordered[i], l, r }),
            .Nothing => try self.def(inst, "icmp {s} i1 0, 0", .{signed[i]}),
            .String => {
                const a = try self.stringParts(l);
                const b = try self.stringParts(r);
                switch (op) {
                    .Eq => try self.def(inst, "call i1 @doxa_str_eq(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len }),
                    .Ne => {
                        const eq = try self.tmp("call i1 @doxa_str_eq(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len });
                        try self.def(inst, "xor i1 {s}, true", .{eq});
                    },
                    // Byte-wise lexicographic (`docs/strings.md`).
                    else => {
                        const order = try self.tmp("call i32 @doxa_str_cmp(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len });
                        try self.def(inst, "icmp {s} i32 {s}, 0", .{ signed[i], order });
                    },
                }
            },
            else => |t| return self.e.faultf("compares {s} values", .{@tagName(t)}),
        }
    }

    // ── Boxes ──

    /// Box a member as `boxed`: its runtime tag, the box header with the
    /// member's index, and its payload (a scalar's bits, a pointer, or a
    /// string's pointer and length).
    fn box(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
        const boxed = self.f.typeOf(inst.result.?).doxa;
        const member = self.doxaOf(operand);
        const index = try self.memberIndex(boxed, member);
        const header = try self.e.boxHeader(boxed);
        const payload = try self.payloadWords(self.name(operand), member);
        const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {d}, 0", .{@intFromEnum(layout.valueTag(member))});
        const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 {d}, 1", .{ v0, header | index });
        const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, payload.bits });
        try self.def(inst, "insertvalue %DoxaValue {s}, i64 {s}, 3", .{ v2, payload.len });
    }

    fn memberIndex(self: *FunctionEmitter, boxed: HIRType, member: HIRType) Error!u32 {
        for (try self.e.boxMembers(boxed), 0..) |m, i| {
            if (m.eql(member)) return @intCast(i);
        }
        return self.e.faultf("boxes a {s}, which its box type does not hold", .{@tagName(member)});
    }

    const Payload = struct { bits: []const u8, len: []const u8 };

    fn payloadWords(self: *FunctionEmitter, v: []const u8, t: HIRType) Error!Payload {
        return switch (t) {
            .String => blk: {
                const s = try self.stringParts(v);
                break :blk .{ .bits = try self.tmp("ptrtoint ptr {s} to i64", .{s.ptr}), .len = s.len };
            },
            else => .{ .bits = try self.storageBits(v, t), .len = "0" },
        };
    }

    /// A value of `t` as one `i64` word: a scalar's bits, a pointer's
    /// address. What a struct field, a map entry or a box payload stores.
    fn storageBits(self: *FunctionEmitter, v: []const u8, t: HIRType) Error![]const u8 {
        return switch (t) {
            .Int, .Enum => v,
            .Byte => self.tmp("zext i8 {s} to i64", .{v}),
            .Tetra => self.tmp("zext i2 {s} to i64", .{v}),
            .Float => self.tmp("bitcast double {s} to i64", .{v}),
            .Nothing => "0",
            .Array, .Map, .Struct, .Function => self.tmp("ptrtoint ptr {s} to i64", .{v}),
            .String, .Union, .Group, .Unknown, .Poison => self.e.faultf("stores a {s} as one word", .{@tagName(t)}),
        };
    }

    /// The value of `t` an `i64` word holds: the inverse of `storageBits`.
    fn fromBits(self: *FunctionEmitter, bits: []const u8, t: HIRType) Error![]const u8 {
        return switch (t) {
            .Int, .Enum => bits,
            .Byte => self.tmp("trunc i64 {s} to i8", .{bits}),
            .Tetra => self.tmp("trunc i64 {s} to i2", .{bits}),
            .Float => self.tmp("bitcast i64 {s} to double", .{bits}),
            .Nothing => "0",
            .Array, .Map, .Struct, .Function => self.tmp("inttoptr i64 {s} to ptr", .{bits}),
            .String, .Union, .Group, .Unknown, .Poison => self.e.faultf("reads a {s} from one word", .{@tagName(t)}),
        };
    }

    fn unbox(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
        const member = self.f.typeOf(inst.result.?).doxa;
        const v = self.name(operand);
        if (member.isBoxed()) {
            // A narrowing to a smaller box re-packs it.
            return self.repack(inst, operand);
        }
        const bits = try self.tmp("extractvalue %DoxaValue {s}, 2", .{v});
        if (member == .String) {
            const p = try self.tmp("inttoptr i64 {s} to ptr", .{bits});
            const len = try self.tmp("extractvalue %DoxaValue {s}, 3", .{v});
            const s = try self.makeString(p, len);
            self.names[@intFromEnum(inst.result.?)] = s;
            return;
        }
        const value = try self.fromBits(bits, member);
        if (member == .Nothing) {
            self.names[@intFromEnum(inst.result.?)] = "0";
            return;
        }
        self.names[@intFromEnum(inst.result.?)] = value;
    }

    /// Re-pack a box for another box type holding the same member: the
    /// member index is rewritten to the target's index for that member.
    fn repack(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
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
    fn memberTest(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId, members: []const u32) Error!void {
        const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{self.name(operand)});
        const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
        var acc: []const u8 = "false";
        for (members, 0..) |m, i| {
            const is = try self.tmp("icmp eq i32 {s}, {d}", .{ index, m });
            acc = if (i == 0) is else try self.tmp("or i1 {s}, {s}", .{ acc, is });
        }
        self.names[@intFromEnum(inst.result.?)] = acc;
    }

    // ── Text ──

    /// The text of a value, in arena `arena`.
    fn toString(self: *FunctionEmitter, arena: []const u8, operand: ValueId) Error![]const u8 {
        const v = self.name(operand);
        return switch (self.doxaOf(operand)) {
            .String => v,
            .Int => self.stringCall("doxa_int_to_string", "ptr {s}, i64 {s}", .{ arena, v }),
            .Float => self.stringCall("doxa_float_to_string", "ptr {s}, double {s}", .{ arena, v }),
            .Byte => self.stringCall("doxa_byte_to_string", "ptr {s}, i64 {s}", .{ arena, try self.tmp("zext i8 {s} to i64", .{v}) }),
            .Tetra => self.stringCall("doxa_tetra_to_string", "ptr {s}, i64 {s}", .{ arena, try self.tmp("zext i2 {s} to i64", .{v}) }),
            .Nothing => self.stringCall("doxa_nothing_to_string", "ptr {s}", .{arena}),
            .Enum => |id| blk: {
                const key = self.e.module.program.enum_keys[id];
                break :blk self.stringCall("doxa_enum_to_string", "ptr {s}, ptr {s}, i64 {d}, i64 {s}", .{ arena, try self.e.string(key), key.len, v });
            },
            .Struct => self.stringCall("doxa_struct_to_string", "ptr {s}, ptr {s}", .{ arena, v }),
            .Array => |a| if (a.size != null)
                self.e.faultf("renders a fixed array; the generator converts it first", .{})
            else
                self.stringCall("doxa_array_to_string", "ptr {s}, ptr {s}", .{ arena, v }),
            .Union, .Group => self.stringCall("doxa_value_to_string", "ptr {s}, ptr {s}", .{ arena, try self.spillValue(v) }),
            .Map => self.e.stringConstant("<map>"),
            .Function => self.e.stringConstant("<function>"),
            .Unknown, .Poison => unreachable,
        };
    }

    // ── Arrays ──

    fn arrayLen(self: *FunctionEmitter, array: []const u8) Error![]const u8 {
        const slot = try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 1", .{array});
        return self.tmp("load i64, ptr {s}", .{slot});
    }

    fn arrayNew(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, length: ?ValueId) Error!void {
        const ty = self.f.typeOf(inst.result.?).doxa;
        const element = ty.Array.element.*;
        if (ty.Array.size != null) {
            // A fixed array is a zeroed flat buffer in its arena.
            const bytes = layout.fixedBytes(&self.e.layout, ty);
            try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ self.name(arena), bytes });
            try self.line("call void @llvm.memset.p0.i64(ptr align 8 {s}, i8 0, i64 {d}, i1 false)", .{ self.name(inst.result.?), bytes });
            try self.defaultStructs(self.name(inst.result.?), self.name(arena), ty);
            return;
        }
        try self.def(inst, "call ptr @doxa_array_new(ptr {s}, i64 {d}, i64 {d}, i64 {s})", .{
            self.name(arena), layout.elementSize(element), layout.elementTag(element), self.name(length.?),
        });
        const words = self.e.layout.skippedWords(element);
        if (words != 0) try self.line("call void @doxa_array_set_elem_words(ptr {s}, i64 {d})", .{ self.name(inst.result.?), words });
    }

    /// A fixed array of a struct with a descriptor holds pointers, each to a
    /// zeroed struct of its own, registered in `arena`.
    /// Nested fixed arrays are inline, so the struct pointers of every
    /// dimension are one contiguous run.
    fn defaultStructs(self: *FunctionEmitter, buffer: []const u8, arena: []const u8, ty: HIRType) Error!void {
        var count: u64 = 1;
        var cursor = ty;
        while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) count *= cursor.Array.size.?;
        if (cursor != .Struct or self.e.layout.isFlat(cursor.Struct)) return;
        const template = try self.defaultStruct(arena, cursor.Struct);
        try self.line("call void @doxa_fixed_structs_default(ptr {s}, ptr {s}, i64 {d}, ptr {s})", .{ arena, buffer, count, template });
    }

    /// A struct's default value, built in `arena`: zero for a scalar, `""`
    /// for a string, and `nothing` boxed for a union or group field — the
    /// member a declaration without an initializer holds.
    fn defaultStruct(self: *FunctionEmitter, arena: []const u8, id: ir.StructId) Error![]const u8 {
        const field_types = self.e.layout.struct_fields[id];
        const words = @max(1, layout.structWords(field_types));
        const object = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ arena, words * 8 });
        try self.line("call void @llvm.memset.p0.i64(ptr align 8 {s}, i8 0, i64 {d}, i1 false)", .{ object, words * 8 });
        for (field_types, 0..) |t, i| {
            if (!t.isBoxed()) continue;
            const header = try self.e.boxHeader(t);
            const index = self.memberIndex(t, .Nothing) catch return self.e.faultf("a {s} field without `nothing` has no default", .{@tagName(t)});
            const cell = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 24, i64 8)", .{arena});
            try self.line("store %DoxaValue {{ i32 {d}, i32 {d}, i64 0, i64 0 }}, ptr {s}", .{ @intFromEnum(rt.DoxaTag.Nothing), header | index, cell });
            const bits = try self.tmp("ptrtoint ptr {s} to i64", .{cell});
            try self.line("store i64 {s}, ptr {s}", .{ bits, try self.fieldPtr(object, id, layout.fieldOffset(field_types, @intCast(i))) });
        }
        if (!self.e.layout.skip_descriptor[id]) {
            try self.line("call void @doxa_struct_register(ptr {s}, ptr {s}, ptr {s})", .{ arena, object, try self.e.structDesc(id) });
        }
        return object;
    }

    fn arrayFromFixed(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, operand: ValueId) Error!void {
        const from = self.doxaOf(operand);
        // Dimensions, outermost first, and the innermost element.
        var sizes: std.ArrayListUnmanaged(u64) = .empty;
        var cursor = from;
        while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) {
            try sizes.append(self.e.alloc, cursor.Array.size.?);
        }
        const element = cursor;
        if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
            if (sizes.items.len != 1) return self.e.faultf("converts a nested fixed array of flat structs", .{});
            try self.def(inst, "call ptr @doxa_array_from_fixed_structs(ptr {s}, ptr {s}, i64 {d}, i64 {d}, ptr null)", .{
                self.name(arena), self.name(operand), sizes.items[0], self.e.layout.structWordsOf(element.Struct),
            });
            return;
        }
        const dims = try self.alloca(try std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{sizes.items.len}));
        for (sizes.items, 0..) |n, i| {
            const at = try self.tmp("getelementptr inbounds [{d} x i64], ptr {s}, i64 0, i64 {d}", .{ sizes.items.len, dims, i });
            try self.line("store i64 {d}, ptr {s}", .{ n, at });
        }
        try self.def(inst, "call ptr @doxa_array_from_fixed(ptr {s}, ptr {s}, ptr {s}, i64 {d}, i64 {d}, i64 {d})", .{
            self.name(arena), self.name(operand), dims, sizes.items.len, layout.elementSize(element), layout.elementTag(element),
        });
    }

    /// Copy `array`'s elements back into the fixed array `fixed`, as many as
    /// both hold: the inverse of `array.from_fixed`.
    fn arrayCopyToFixed(self: *FunctionEmitter, fixed: ValueId, array: ValueId) Error!void {
        const t = self.doxaOf(fixed);
        var sizes: std.ArrayListUnmanaged(u64) = .empty;
        var cursor = t;
        while (cursor == .Array and cursor.Array.size != null) : (cursor = cursor.Array.element.*) {
            try sizes.append(self.e.alloc, cursor.Array.size.?);
        }
        const element = cursor;
        if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
            if (sizes.items.len != 1) return self.e.faultf("copies into a nested fixed array of flat structs", .{});
            try self.line("call void @doxa_array_copy_to_fixed_structs(ptr {s}, ptr {s}, i64 {d}, i64 {d})", .{ self.name(array), self.name(fixed), sizes.items[0], self.e.layout.structWordsOf(element.Struct) });
            return;
        }
        const dims = try self.alloca(try std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{sizes.items.len}));
        for (sizes.items, 0..) |n, i| {
            const at = try self.tmp("getelementptr inbounds [{d} x i64], ptr {s}, i64 0, i64 {d}", .{ sizes.items.len, dims, i });
            try self.line("store i64 {d}, ptr {s}", .{ n, at });
        }
        try self.line("call void @doxa_array_copy_to_fixed(ptr {s}, ptr {s}, ptr {s}, i64 {d}, i64 {d})", .{ self.name(array), self.name(fixed), dims, sizes.items.len, layout.elementSize(element) });
    }

    /// The address of element `index` of `array`.
    fn elementPtr(self: *FunctionEmitter, array: ValueId, index: []const u8) Error![]const u8 {
        const t = self.doxaOf(array);
        if (t.Array.size != null) {
            return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {s}", .{ try layout.fixedStorageType(self.e.alloc, &self.e.layout, t), self.name(array), index });
        }
        const data_slot = try self.tmp("getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 0", .{self.name(array)});
        const data = try self.tmp("load ptr, ptr {s}", .{data_slot});
        return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 {s}", .{ layout.elementStorageType(t.Array.element.*), data, index });
    }

    /// Element reads are unchecked (`docs/methods.md`): the address is
    /// computed in the IR, so LLVM can hoist, unroll and vectorize the loop.
    fn arrayGet(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: ValueId) Error!void {
        const t = self.doxaOf(array);
        const element = t.Array.element.*;
        const at = try self.elementPtr(array, self.name(index));
        const fixed = t.Array.size != null;
        // An inline element — a nested fixed array, a flat struct — is its
        // address.
        if (fixed and ((element == .Array and element.Array.size != null) or (element == .Struct and self.e.layout.isFlat(element.Struct)))) {
            self.names[@intFromEnum(inst.result.?)] = at;
            return;
        }
        switch (element) {
            .Tetra => {
                const byte = try self.tmp("load i8, ptr {s}", .{at});
                try self.def(inst, "trunc i8 {s} to i2", .{byte});
            },
            .Nothing => self.names[@intFromEnum(inst.result.?)] = "0",
            else => try self.def(inst, "load {s}, ptr {s}", .{ layout.elementStorageType(element), at }),
        }
    }

    fn arraySet(self: *FunctionEmitter, array: ValueId, index: ValueId, value: ValueId) Error!void {
        const t = self.doxaOf(array);
        if (t.Array.size == null) return self.arraySetDynamic(self.name(array), self.name(index), value);
        const element = t.Array.element.*;
        const at = try self.elementPtr(array, self.name(index));
        const v = self.name(value);
        if (element == .Array and element.Array.size != null) {
            try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ at, v, layout.fixedBytes(&self.e.layout, element) });
        } else if (element == .Struct and self.e.layout.isFlat(element.Struct)) {
            try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ at, v, @as(u64, self.e.layout.structWordsOf(element.Struct)) * 8 });
        } else switch (element) {
            .Tetra => try self.line("store i8 {s}, ptr {s}", .{ try self.tmp("zext i2 {s} to i8", .{v}), at }),
            .Nothing => {},
            else => try self.line("store {s} {s}, ptr {s}", .{ layout.elementStorageType(element), v, at }),
        }
    }

    /// An element store into a dynamic array goes through the runtime, which
    /// grows the array and re-homes a heap element into the array's arena.
    fn arraySetDynamic(self: *FunctionEmitter, array: []const u8, index: []const u8, value: ValueId) Error!void {
        const element = self.doxaOf(value);
        const v = self.name(value);
        switch (element) {
            .String => {
                const s = try self.stringParts(v);
                try self.line("call void @doxa_array_set_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ array, index, s.ptr, s.len });
            },
            .Union, .Group => try self.line("call void @doxa_array_set_value(ptr {s}, i64 {s}, ptr {s})", .{ array, index, try self.spillValue(v) }),
            else => try self.line("call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})", .{ array, index, try self.storageBits(v, element) }),
        }
    }

    fn arrayPop(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
        const array = self.name(operand);
        const len = try self.arrayLen(array);
        const last = try self.tmp("sub i64 {s}, 1", .{len});
        try self.arrayRemoveAt(inst, operand, last);
    }

    fn arrayRemove(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: ValueId) Error!void {
        try self.arrayRemoveAt(inst, array, self.name(index));
    }

    fn arrayRemoveAt(self: *FunctionEmitter, inst: *const ir.Inst, array: ValueId, index: []const u8) Error!void {
        const element = self.doxaOf(array).Array.element.*;
        const a = self.name(array);
        switch (element) {
            .String => {
                const slots = try self.stringSlots();
                try self.line("call void @doxa_array_remove_str(ptr {s}, i64 {s}, ptr {s}, ptr {s})", .{ a, index, slots.ptr, slots.len });
                self.names[@intFromEnum(inst.result.?)] = try self.loadString(slots.ptr, slots.len);
            },
            .Union, .Group => {
                const slot = try self.alloca("%DoxaValue");
                try self.line("call void @doxa_array_remove_value(ptr {s}, i64 {s}, ptr {s})", .{ a, index, slot });
                try self.def(inst, "load %DoxaValue, ptr {s}", .{slot});
            },
            else => {
                const bits = try self.tmp("call i64 @doxa_array_remove(ptr {s}, i64 {s})", .{ a, index });
                self.names[@intFromEnum(inst.result.?)] = try self.fromBits(bits, element);
            },
        }
    }

    fn arrayInsert(self: *FunctionEmitter, array: ValueId, index: ValueId, value: ValueId) Error!void {
        const element = self.doxaOf(value);
        const a = self.name(array);
        const i = self.name(index);
        const v = self.name(value);
        switch (element) {
            .String => {
                const s = try self.stringParts(v);
                try self.line("call void @doxa_array_insert_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a, i, s.ptr, s.len });
            },
            .Union, .Group => try self.line("call void @doxa_array_insert_value(ptr {s}, i64 {s}, ptr {s})", .{ a, i, try self.spillValue(v) }),
            else => try self.line("call void @doxa_array_insert(ptr {s}, i64 {s}, i64 {s})", .{ a, i, try self.storageBits(v, element) }),
        }
    }

    // ── Maps ──
    //
    // A map stores raw `i64` words: a string key or value as a C string in
    // the map's own arena, a box as a pointer to a `%DoxaValue` there.

    fn mapWord(self: *FunctionEmitter, map: []const u8, v: []const u8, t: HIRType) Error![]const u8 {
        return switch (t) {
            .String => blk: {
                const scope = try self.tmp("call ptr @doxa_map_scope(ptr {s})", .{map});
                const s = try self.stringParts(v);
                const raw = try self.tmp("call ptr @doxa_str_clone_raw(ptr {s}, ptr {s}, i64 {s})", .{ scope, s.ptr, s.len });
                break :blk self.tmp("ptrtoint ptr {s} to i64", .{raw});
            },
            .Union, .Group => blk: {
                const scope = try self.tmp("call ptr @doxa_map_scope(ptr {s})", .{map});
                const cell = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 24, i64 8)", .{scope});
                try self.line("store %DoxaValue {s}, ptr {s}", .{ v, cell });
                break :blk self.tmp("ptrtoint ptr {s} to i64", .{cell});
            },
            else => self.storageBits(v, t),
        };
    }

    /// The value a map word holds; a string is copied into `arena`.
    fn mapValue(self: *FunctionEmitter, arena: []const u8, bits: []const u8, t: HIRType) Error![]const u8 {
        return switch (t) {
            .String => self.stringCall("doxa_str_from_cstr", "ptr {s}, ptr {s}", .{ arena, try self.tmp("inttoptr i64 {s} to ptr", .{bits}) }),
            .Union, .Group => self.tmp("load %DoxaValue, ptr {s}", .{try self.tmp("inttoptr i64 {s} to ptr", .{bits})}),
            else => self.fromBits(bits, t),
        };
    }

    fn mapNew(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, else_value: ?ValueId) Error!void {
        const t = self.f.typeOf(inst.result.?).doxa;
        try self.def(inst, "call ptr @doxa_map_new(ptr {s}, i64 0, i64 {d}, i64 {d})", .{ self.name(arena), layout.elementTag(t.Map.key.*), layout.elementTag(t.Map.value.*) });
        if (else_value) |ev| {
            const map = self.name(inst.result.?);
            try self.line("call void @doxa_map_set_else_i64(ptr {s}, i64 {s})", .{ map, try self.mapWord(map, self.name(ev), t.Map.value.*) });
        }
    }

    fn mapSet(self: *FunctionEmitter, map: ValueId, key: ValueId, value: ValueId) Error!void {
        const t = self.doxaOf(map).Map;
        const m = self.name(map);
        const v = try self.mapWord(m, self.name(value), t.value.*);
        if (t.key.* == .String) {
            // The runtime copies a new key into the map's arena.
            const k = try self.stringParts(self.name(key));
            try self.line("call void @doxa_map_set_str(ptr {s}, ptr {s}, i64 {s}, i64 {s})", .{ m, k.ptr, k.len, v });
            return;
        }
        try self.line("call void @doxa_map_set_i64(ptr {s}, i64 {s}, i64 {s})", .{ m, try self.storageBits(self.name(key), t.key.*), v });
    }

    /// Look `key` up in `map`; the found word is written to `slot`. A string
    /// key is compared in place.
    fn mapLookup(self: *FunctionEmitter, map: []const u8, key: ValueId, key_type: HIRType, slot: []const u8) Error![]const u8 {
        if (key_type == .String) {
            const k = try self.stringParts(self.name(key));
            return self.tmp("call i8 @doxa_map_try_get_str(ptr {s}, ptr {s}, i64 {s}, ptr {s})", .{ map, k.ptr, k.len, slot });
        }
        return self.tmp("call i8 @doxa_map_try_get_i64(ptr {s}, i64 {s}, ptr {s})", .{ map, try self.storageBits(self.name(key), key_type), slot });
    }

    /// `m[k]`: the map's value, or for a map without `else` the value boxed
    /// as `value | nothing`, `nothing` when the key is absent.
    fn mapGet(self: *FunctionEmitter, inst: *const ir.Inst, map: ValueId, key: ValueId) Error!void {
        const t = self.doxaOf(map).Map;
        const result_type = self.f.typeOf(inst.result.?).doxa;
        const m = self.name(map);
        const scope = try self.tmp("call ptr @doxa_map_scope(ptr {s})", .{m});
        const slot = try self.alloca("i64");
        const found = try self.mapLookup(m, key, t.key.*, slot);
        const bits = try self.tmp("load i64, ptr {s}", .{slot});
        // A map with an `else` always finds a value.
        if (result_type.eql(t.value.*)) {
            self.names[@intFromEnum(inst.result.?)] = try self.mapValue(scope, bits, t.value.*);
            return;
        }
        const value = try self.mapValue(scope, bits, t.value.*);
        const header = try self.e.boxHeader(result_type);
        const value_index = try self.memberIndex(result_type, t.value.*);
        const nothing_index = try self.memberIndex(result_type, .Nothing);
        const payload = try self.payloadWords(value, t.value.*);
        const hit = try self.tmp("icmp ne i8 {s}, 0", .{found});
        const tag = try self.tmp("select i1 {s}, i32 {d}, i32 {d}", .{ hit, @intFromEnum(layout.valueTag(t.value.*)), @intFromEnum(rt.DoxaTag.Nothing) });
        const reserved = try self.tmp("select i1 {s}, i32 {d}, i32 {d}", .{ hit, header | value_index, header | nothing_index });
        const word = try self.tmp("select i1 {s}, i64 {s}, i64 0", .{ hit, payload.bits });
        const len = try self.tmp("select i1 {s}, i64 {s}, i64 0", .{ hit, payload.len });
        const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {s}, 0", .{tag});
        const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 {s}, 1", .{ v0, reserved });
        const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, word });
        try self.def(inst, "insertvalue %DoxaValue {s}, i64 {s}, 3", .{ v2, len });
    }

    // ── Structs ──

    fn structType(self: *FunctionEmitter, id: ir.StructId) Error![]const u8 {
        return std.fmt.allocPrint(self.e.alloc, "[{d} x i64]", .{@max(1, self.e.layout.structWordsOf(id))});
    }

    fn fieldPtr(self: *FunctionEmitter, object: []const u8, id: ir.StructId, word: u32) Error![]const u8 {
        return self.tmp("getelementptr inbounds {s}, ptr {s}, i64 0, i64 {d}", .{ try self.structType(id), object, word });
    }

    fn structNew(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, fields: []const ValueId) Error!void {
        const id = self.f.typeOf(inst.result.?).doxa.Struct;
        const field_types = self.e.layout.struct_fields[id];
        const words = @max(1, layout.structWords(field_types));
        try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ self.name(arena), words * 8 });
        const object = self.name(inst.result.?);
        for (fields, field_types, 0..) |v, t, i| {
            try self.storeField(object, self.name(arena), id, @intCast(i), t, self.name(v));
        }
        if (!self.e.layout.skip_descriptor[id]) {
            try self.line("call void @doxa_struct_register(ptr {s}, ptr {s}, ptr {s})", .{ self.name(arena), object, try self.e.structDesc(id) });
        }
    }

    /// Store `v` into field `index` of `object`, which lives in `scope`: a
    /// string is copied there, a box is given a cell there.
    fn storeField(self: *FunctionEmitter, object: []const u8, scope: []const u8, id: ir.StructId, index: u32, t: HIRType, v: []const u8) Error!void {
        const field_types = self.e.layout.struct_fields[id];
        const word = layout.fieldOffset(field_types, index);
        switch (t) {
            .String => {
                const s = try self.stringParts(v);
                const placed = try self.stringCall("doxa_str_clone", "ptr {s}, ptr {s}, i64 {s}", .{ scope, s.ptr, s.len });
                const c = try self.stringParts(placed);
                try self.line("store ptr {s}, ptr {s}", .{ c.ptr, try self.fieldPtr(object, id, word) });
                try self.line("store i64 {s}, ptr {s}", .{ c.len, try self.fieldPtr(object, id, word + 1) });
            },
            .Union, .Group => {
                const cell = try self.tmp("call ptr @doxa_scope_alloc(ptr {s}, i64 24, i64 8)", .{scope});
                try self.line("store %DoxaValue {s}, ptr {s}", .{ v, cell });
                const bits = try self.tmp("ptrtoint ptr {s} to i64", .{cell});
                try self.line("store i64 {s}, ptr {s}", .{ bits, try self.fieldPtr(object, id, word) });
            },
            else => try self.line("store i64 {s}, ptr {s}", .{ try self.storageBits(v, t), try self.fieldPtr(object, id, word) }),
        }
    }

    fn fieldGet(self: *FunctionEmitter, inst: *const ir.Inst, object: ValueId, index: u32) Error!void {
        const id = self.doxaOf(object).Struct;
        const t = self.e.layout.struct_fields[id][index];
        const word = layout.fieldOffset(self.e.layout.struct_fields[id], index);
        const o = self.name(object);
        switch (t) {
            .String => {
                const p = try self.tmp("load ptr, ptr {s}", .{try self.fieldPtr(o, id, word)});
                const l = try self.tmp("load i64, ptr {s}", .{try self.fieldPtr(o, id, word + 1)});
                self.names[@intFromEnum(inst.result.?)] = try self.makeString(p, l);
            },
            .Union, .Group => {
                const bits = try self.tmp("load i64, ptr {s}", .{try self.fieldPtr(o, id, word)});
                const cell = try self.tmp("inttoptr i64 {s} to ptr", .{bits});
                try self.def(inst, "load %DoxaValue, ptr {s}", .{cell});
            },
            else => {
                const bits = try self.tmp("load i64, ptr {s}", .{try self.fieldPtr(o, id, word)});
                self.names[@intFromEnum(inst.result.?)] = try self.fromBits(bits, t);
            },
        }
    }

    /// A field store after construction: a heap value is placed in the
    /// struct's own arena, which the runtime's registry records for every
    /// struct with a heap field (`doxa_struct_scope`).
    fn fieldSet(self: *FunctionEmitter, object: ValueId, index: u32, value: ValueId) Error!void {
        const id = self.doxaOf(object).Struct;
        const t = self.e.layout.struct_fields[id][index];
        const o = self.name(object);
        const scope = switch (t) {
            .String, .Union, .Group => try self.tmp("call ptr @doxa_struct_scope(ptr {s})", .{o}),
            else => "null",
        };
        try self.storeField(o, scope, id, index, t, self.name(value));
    }

    // ── Copies ──

    const Copy = enum { clone, rehome };

    /// A copy of a heap value in `arena`: always (`clone`), or only when it
    /// does not already live in `arena` or an ancestor (`rehome`). A string
    /// carries no arena of its own, so its `rehome` is a clone; a map is not
    /// copied.
    fn copy(self: *FunctionEmitter, inst: *const ir.Inst, arena: ValueId, operand: ValueId, kind: Copy) Error!void {
        const a = self.name(arena);
        const v = self.name(operand);
        const t = self.doxaOf(operand);
        switch (t) {
            .String => {
                const s = try self.stringParts(v);
                self.names[@intFromEnum(inst.result.?)] = try self.stringCall("doxa_str_clone", "ptr {s}, ptr {s}, i64 {s}", .{ a, s.ptr, s.len });
            },
            .Array => |arr| {
                if (arr.size != null) {
                    // A fixed array is copied whole into `arena`.
                    const bytes = layout.fixedBytes(&self.e.layout, t);
                    try self.def(inst, "call ptr @doxa_scope_alloc(ptr {s}, i64 {d}, i64 8)", .{ a, bytes });
                    try self.line("call void @llvm.memcpy.p0.p0.i64(ptr {s}, ptr {s}, i64 {d}, i1 false)", .{ self.name(inst.result.?), v, bytes });
                } else {
                    try self.def(inst, "call ptr @doxa_array_{s}(ptr {s}, ptr {s})", .{ @tagName(kind), a, v });
                }
            },
            .Struct => |id| {
                const words = self.e.layout.skippedWords(t);
                if (words != 0) {
                    const which = if (kind == .clone) "clone_scalar" else "rehome_scalar";
                    try self.def(inst, "call ptr @doxa_struct_{s}(ptr {s}, i64 {d}, ptr {s})", .{ which, a, words, v });
                } else {
                    _ = id;
                    try self.def(inst, "call ptr @doxa_struct_{s}(ptr {s}, ptr {s})", .{ @tagName(kind), a, v });
                }
            },
            .Union, .Group => {
                const slot = try self.spillValue(v);
                try self.line("call void @doxa_clone_doxa_value(ptr {s}, ptr {s})", .{ a, slot });
                try self.def(inst, "load %DoxaValue, ptr {s}", .{slot});
            },
            .Map => self.names[@intFromEnum(inst.result.?)] = v,
            else => return self.e.faultf("copies a {s}", .{@tagName(t)}),
        }
    }

    // ── Calls ──

    fn callDoxa(self: *FunctionEmitter, inst: *const ir.Inst, callee: ir.FunctionId, args: []const ValueId) Error!void {
        const sig = self.e.module.program.functions[@intFromEnum(callee)];
        var text: std.ArrayListUnmanaged(u8) = .empty;
        for (args, 0..) |a, i| {
            if (i != 0) try text.appendSlice(self.e.alloc, ", ");
            try text.print(self.e.alloc, "{s} {s}", .{ layout.llvmType(self.typeOf(a)), self.name(a) });
        }
        const symbol = try self.e.functionSymbol(callee);
        if (inst.result) |_| {
            try self.def(inst, "call {s} {s}({s})", .{ layout.returnType(sig.ret), symbol, text.items });
        } else {
            try self.line("call {s} {s}({s})", .{ layout.returnType(sig.ret), symbol, text.items });
        }
    }

    fn callZig(self: *FunctionEmitter, inst: *const ir.Inst, callee: ir.ZigFunctionId, args: []const ValueId) Error!void {
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
            .Union => try self.fallibleZig(inst, symbol, text.items, sig.ret),
            else => try self.def(inst, "call {s} {s}({s})", .{ layout.doxaType(sig.ret), symbol, text.items }),
        }
    }

    /// A fallible Zig call (`DoxaError_<path>!<payload>`): the wrapper
    /// returns -1 on success, else the error's variant, and writes a payload
    /// through out-parameters. The result is the union, boxed either way.
    fn fallibleZig(self: *FunctionEmitter, inst: *const ir.Inst, symbol: []const u8, args: []const u8, result_type: HIRType) Error!void {
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

    // ── Peek ──

    /// `v?`: the location, name and type, then the value, on stderr.
    fn peek(self: *FunctionEmitter, operand: ValueId, display: ir.PeekDisplay) Error!void {
        const t = self.doxaOf(operand);
        const info = try self.alloca("%DoxaPeekInfo");
        const fields = [_][]const u8{ "file", "name", "type", "members" };
        const file = try self.e.string(display.location.file);
        const label = try self.peekLabel(t, display);
        const values = [_][]const u8{
            file,
            if (display.path) |p| try self.e.string(p) else "null",
            try self.e.string(label),
            if (display.members) |m| try self.e.nameList(m) else "null",
        };
        for (fields, values, 0..) |_, v, i| {
            const at = try self.tmp("getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 {d}", .{ info, i });
            try self.line("store ptr {s}, ptr {s}", .{ v, at });
        }
        const count: usize = if (display.members) |m| m.len else 0;
        const active = if (t.isBoxed()) try self.activeMember(self.name(operand), display) else "-1";
        const ints = [_][]const u8{
            try std.fmt.allocPrint(self.e.alloc, "{d}", .{count}),
            active,
            "1",
            try std.fmt.allocPrint(self.e.alloc, "{d}", .{display.location.range.start_line}),
            try std.fmt.allocPrint(self.e.alloc, "{d}", .{display.location.range.start_col}),
        };
        for (ints, 4..) |v, i| {
            const at = try self.tmp("getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 {d}", .{ info, i });
            try self.line("store i32 {s}, ptr {s}", .{ v, at });
        }
        try self.line("call void @doxa_debug_peek(ptr {s})", .{info});
        try self.printValue(self.name(operand), t);
        try self.line("call void @doxa_peek_end()", .{});
    }

    /// The marker slot of the member a box holds: its member index, through
    /// `member_slots` when written groups collapse members.
    fn activeMember(self: *FunctionEmitter, v: []const u8, display: ir.PeekDisplay) Error![]const u8 {
        const reserved = try self.tmp("extractvalue %DoxaValue {s}, 1", .{v});
        const index = try self.tmp("and i32 {s}, {d}", .{ reserved, rt.DoxaBoxMeta.member_index_mask });
        const slots = display.member_slots orelse return index;
        var acc: []const u8 = "-1";
        for (slots, 0..) |slot, member| {
            const is = try self.tmp("icmp eq i32 {s}, {d}", .{ index, member });
            acc = try self.tmp("select i1 {s}, i32 {d}, i32 {s}", .{ is, slot, acc });
        }
        return acc;
    }

    /// The type a peek shows for a value of `t`.
    fn peekLabel(self: *FunctionEmitter, t: HIRType, display: ir.PeekDisplay) Error![]const u8 {
        const p = &self.e.module.program;
        return switch (t) {
            .Int => "int",
            .Float => "float",
            .Byte => "byte",
            .Tetra => "tetra",
            .String => "string",
            .Nothing => "nothing",
            .Enum => |id| p.enum_names[id],
            .Struct => |id| p.struct_names[id],
            .Array => try self.typeText(t),
            .Union => if (display.members) |m| m[0] else "union",
            .Group => |gid| p.group_names[gid],
            .Map => "map",
            .Function => "function",
            .Unknown, .Poison => unreachable,
        };
    }

    fn typeText(self: *FunctionEmitter, t: HIRType) Error![]const u8 {
        const p = &self.e.module.program;
        return switch (t) {
            .Array => |a| std.fmt.allocPrint(self.e.alloc, "{s}[]", .{try self.typeText(a.element.*)}),
            .Struct => |id| p.struct_names[id],
            .Enum => "enum",
            .Union => "union",
            .Group => |gid| p.group_names[gid],
            else => try self.peekLabel(t, .{ .path = null, .location = undefined }),
        };
    }

    fn printValue(self: *FunctionEmitter, v: []const u8, t: HIRType) Error!void {
        switch (t) {
            .Int => try self.line("call void @doxa_print_i64(i64 {s})", .{v}),
            .Float => try self.line("call void @doxa_print_f64(double {s})", .{v}),
            .Byte => try self.line("call void @doxa_print_byte(i64 {s})", .{try self.tmp("zext i8 {s} to i64", .{v})}),
            .Tetra => try self.line("call void @doxa_print_tetra(i64 {s})", .{try self.tmp("zext i2 {s} to i64", .{v})}),
            .String => {
                const s = try self.stringParts(v);
                try self.line("call void @doxa_peek_string(ptr {s}, i64 {s})", .{ s.ptr, s.len });
            },
            .Nothing => {
                const text = try self.e.string("nothing");
                try self.line("call void @doxa_write_cstr(ptr {s}, i64 7)", .{text});
            },
            .Enum => |id| try self.line("call void @doxa_print_enum(ptr {s}, i64 {s})", .{ try self.e.enumDesc(id), v }),
            .Union, .Group => try self.line("call void @doxa_print_value(ptr {s})", .{try self.spillValue(v)}),
            .Struct => {
                const bits = try self.tmp("ptrtoint ptr {s} to i64", .{v});
                const v0 = try self.tmp("insertvalue %DoxaValue undef, i32 {d}, 0", .{@intFromEnum(rt.DoxaTag.Struct)});
                const v1 = try self.tmp("insertvalue %DoxaValue {s}, i32 0, 1", .{v0});
                const v2 = try self.tmp("insertvalue %DoxaValue {s}, i64 {s}, 2", .{ v1, bits });
                const boxed = try self.tmp("insertvalue %DoxaValue {s}, i64 0, 3", .{v2});
                try self.line("call void @doxa_print_value(ptr {s})", .{try self.spillValue(boxed)});
            },
            .Array => |a| {
                if (a.size != null) return self.e.faultf("peeks a fixed array; the generator converts it first", .{});
                try self.line("call void @doxa_print_array_hdr(ptr {s})", .{v});
            },
            .Map => try self.line("call void @doxa_write_cstr(ptr {s}, i64 5)", .{try self.e.string("<map>")}),
            .Function => try self.line("call void @doxa_write_cstr(ptr {s}, i64 10)", .{try self.e.string("<function>")}),
            .Unknown, .Poison => unreachable,
        }
    }

    // ── Terminators ──

    fn terminator(self: *FunctionEmitter, term: *const ir.Terminator) Error!void {
        switch (term.*) {
            .jump => |call| try self.line("br label %{s}", .{try self.edge(call)}),
            .branch => |b| {
                const then = try self.edge(b.then);
                const otherwise = if (b.@"else".block == b.then.block and b.then.args.len != 0) try self.trampoline(b.@"else") else try self.edge(b.@"else");
                try self.line("br i1 {s}, label %{s}, label %{s}", .{ self.name(b.cond), then, otherwise });
            },
            .@"switch" => |s| {
                const ty = layout.doxaType(self.doxaOf(s.operand));
                try self.w.print("  switch {s} {s}, label %{s} [", .{ ty, self.name(s.operand), try self.edge(s.default) });
                for (s.cases) |case| try self.w.print(" {s} {d}, label %{s}", .{ ty, case.value, try self.edge(case.target) });
                try self.w.writeAll(" ]\n");
            },
            .@"return" => |v| if (v) |value| {
                try self.line("ret {s} {s}", .{ layout.doxaType(self.f.ret), self.name(value) });
            } else {
                try self.line("ret void", .{});
            },
            .@"unreachable" => {
                try self.line("call void @doxa_trap_unreachable()", .{});
                try self.line("unreachable", .{});
            },
            .panic => |m| {
                const s = try self.stringParts(self.name(m));
                try self.line("call void @doxa_panic(ptr {s}, i64 {s})", .{ s.ptr, s.len });
                try self.line("unreachable", .{});
            },
            .exit => |code| {
                try self.line("call void @doxa_exit(i64 {s})", .{self.name(code)});
                try self.line("unreachable", .{});
            },
            .assert_fail => |a| {
                const newline = try self.e.string("\n");
                if (a.message) |m| {
                    const s = try self.stringParts(self.name(m));
                    try self.line("call void @doxa_write_stderr(ptr {s}, i64 {s})", .{ s.ptr, s.len });
                    try self.line("call void @doxa_write_stderr(ptr {s}, i64 1)", .{newline});
                }
                try self.line("call void @doxa_write_stderr(ptr {s}, i64 16)", .{try self.e.string("Assertion failed")});
                try self.line("call void @doxa_write_stderr(ptr {s}, i64 1)", .{newline});
                try self.line("call void @doxa_exit(i64 1)", .{});
                try self.line("unreachable", .{});
            },
        }
    }

    /// The label control transfers to for `call`, recording its arguments
    /// for the target's phis.
    fn edge(self: *FunctionEmitter, call: ir.BlockCall) Error![]const u8 {
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
    fn trampoline(self: *FunctionEmitter, call: ir.BlockCall) Error![]const u8 {
        const label = try self.split("edge");
        const entry = try self.incoming.getOrPut(self.e.alloc, @intFromEnum(call.block));
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.e.alloc, .{ .label = label, .args = call.args });
        try self.edges.append(self.e.alloc, try std.fmt.allocPrint(self.e.alloc, "{s}:\n  br label %b{d}\n", .{ label, @intFromEnum(call.block) }));
        return label;
    }
};
