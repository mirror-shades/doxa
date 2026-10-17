const ir = @import("../../hir/register/ir.zig");
const rt = @import("../../../runtime/doxa_rt.zig");
const layout = @import("../layout.zig");
const numbers = @import("numbers.zig");
const boxes = @import("boxes.zig");
const text = @import("text.zig");
const arrays = @import("arrays.zig");
const structs = @import("structs.zig");
const copies = @import("copies.zig");
const calls = @import("calls.zig");
const peek = @import("peek.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;

// ── Instructions ──

pub fn lower(self: *FunctionEmitter, inst: *const ir.Inst) Error!void {
    switch (inst.op) {
        .constant => {},
        .arith => |a| try numbers.arith(self, inst, a.op, a.lhs, a.rhs),
        .neg => |u| switch (self.doxaOf(u.operand)) {
            .Float => try self.def(inst, "fneg double {s}", .{self.name(u.operand)}),
            .Int => self.names[@intFromEnum(inst.result.?)] = try numbers.checkedArith(self, .Sub, .Int, "0", self.name(u.operand), .exact(0), self.ranges[@intFromEnum(u.operand)]),
            .Byte => self.names[@intFromEnum(inst.result.?)] = try numbers.checkedArith(self, .Sub, .Byte, "0", self.name(u.operand), .exact(0), self.ranges[@intFromEnum(u.operand)]),
            else => return self.e.faultf("negates a {s}", .{@tagName(self.doxaOf(u.operand))}),
        },
        .convert => |u| try numbers.convert(self, inst, u.operand),
        .cmp => |c| try numbers.compare(self, inst, c.op, c.lhs, c.rhs),
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

        .box => |u| try boxes.box(self, inst, u.operand),
        .unbox => |u| try boxes.unbox(self, inst, u.operand),
        .repack => |u| try boxes.repack(self, inst, u.operand),
        .member_test => |m| try boxes.memberTest(self, inst, m.operand, m.members),
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
            try arrays.boundsCheck(self, s.index, a.len, null, .element);
            const at = try self.tmp("getelementptr inbounds i8, ptr {s}, i64 {s}", .{ a.ptr, self.name(s.index) });
            const byte = try self.tmp("load i8, ptr {s}", .{at});
            try self.defString(inst, try self.stringCall("doxa_char_to_string", "ptr {s}, i8 {s}", .{ self.name(s.arena), byte }));
        },
        .str_to_int, .str_to_float, .str_to_byte => |u| {
            const a = try self.stringParts(self.name(u.operand));
            const kind: rt.FaultKind = switch (inst.op) {
                .str_to_int => .parse_int,
                .str_to_float => .parse_float,
                else => .parse_byte,
            };
            const site = try self.e.faultSite(kind, self.span);
            switch (inst.op) {
                .str_to_int => try self.def(inst, "call i64 @doxa_int_from_string(ptr {s}, i64 {s}, ptr {s})", .{ a.ptr, a.len, site }),
                .str_to_float => try self.def(inst, "call double @doxa_float_from_string(ptr {s}, i64 {s}, ptr {s})", .{ a.ptr, a.len, site }),
                else => {
                    const wide = try self.tmp("call i64 @doxa_byte_from_string(ptr {s}, i64 {s}, ptr {s})", .{ a.ptr, a.len, site });
                    try self.def(inst, "trunc i64 {s} to i8", .{wide});
                },
            }
        },
        .str_pack => |s| try self.defString(inst, try self.stringCall("doxa_pack_bytes", "ptr {s}, ptr {s}", .{ self.name(s.arena), self.name(s.bytes) })),
        .str_unpack => |s| {
            const a = try self.stringParts(self.name(s.string));
            try self.def(inst, "call ptr @doxa_unpack_bytes(ptr {s}, ptr {s}, i64 {s})", .{ self.name(s.arena), a.ptr, a.len });
        },
        .to_string => |s| try self.defString(inst, try text.toString(self, self.name(s.arena), s.operand)),

        .array_new => |a| try arrays.arrayNew(self, inst, a.arena, a.length),
        .array_from_fixed => |a| try arrays.arrayFromFixed(self, inst, a.arena, a.operand),
        .array_range => |a| try self.def(inst, "call ptr @doxa_array_range(ptr {s}, i64 {s}, i64 {s})", .{ self.name(a.arena), self.name(a.start), self.name(a.end) }),
        .array_get => |a| try arrays.arrayGet(self, inst, a.array, a.index),
        .array_set => |a| try arrays.arraySet(self, a.array, a.index, a.value),
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
            const len = try arrays.arrayLen(self, self.name(a.array));
            try arrays.arraySetDynamic(self, self.name(a.array), len, a.value);
        },
        .array_pop => |u| try arrays.arrayPop(self, inst, u.operand),
        .array_insert => |a| try arrays.arrayInsert(self, a.array, a.index, a.value),
        .array_remove => |a| try arrays.arrayRemove(self, inst, a.array, a.index),
        .array_clear => |u| try self.line("call void @doxa_array_clear(ptr {s})", .{self.name(u.operand)}),
        .array_slice => |a| try self.def(inst, "call ptr @doxa_array_slice(ptr {s}, ptr {s}, i64 {s}, i64 {s})", .{ self.name(a.arena), self.name(a.array), self.name(a.start), self.name(a.length) }),
        .array_concat => |a| try self.def(inst, "call ptr @doxa_array_concat(ptr {s}, ptr {s}, ptr {s})", .{ self.name(a.arena), self.name(a.lhs), self.name(a.rhs) }),
        .array_copy_to_fixed => |c| try arrays.arrayCopyToFixed(self, c.fixed, c.array, c.arena),
        .array_find => |a| {
            const element = self.doxaOf(a.array).Array.element.*;
            if (element == .String) {
                const n = try self.stringParts(self.name(a.value));
                try self.def(inst, "call i64 @doxa_find_array_str(ptr {s}, ptr {s}, i64 {s})", .{ self.name(a.array), n.ptr, n.len });
            } else {
                const bits = try boxes.storageBits(self, self.name(a.value), element);
                try self.def(inst, "call i64 @doxa_find_array(ptr {s}, i64 {s})", .{ self.name(a.array), bits });
            }
        },

        .struct_new => |s| try structs.structNew(self, inst, s.arena, s.fields),
        .field_get => |g| try structs.fieldGet(self, inst, g.object, g.index),
        .field_set => |s| try structs.fieldSet(self, s.object, s.index, s.value),
        .arena_of => |u| try copies.arenaOf(self, inst, u.operand),

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
        .clone, .rehome => |c| if (self.inline_only[@intFromEnum(inst.result.?)]) {
            // Stored inline, as its bytes: the store copies it.
            self.names[@intFromEnum(inst.result.?)] = self.name(c.operand);
        } else try copies.copy(self, inst, c.arena, c.operand, if (inst.op == .clone) .clone else .rehome),

        .call => |c| try calls.callDoxa(self, inst, c.callee, c.args),
        .call_zig => |c| try calls.callZig(self, inst, c.callee, c.args),

        .print => |u| {
            const s = try self.stringParts(self.name(u.operand));
            try self.line("call void @doxa_write_cstr(ptr {s}, i64 {s})", .{ s.ptr, s.len });
        },
        .peek => |p| try peek.peek(self, p.operand, p.display, inst.span),
        .debug_value => |v| try self.debugValue(v.local, v.value),
    }
}
