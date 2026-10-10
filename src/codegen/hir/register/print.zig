//! The textual form of the register HIR (`--emit-hir`). Verifier faults quote
//! it, and HIR-shape tests compare against it, so it is stable: one line per
//! instruction, values as `%n`, blocks as `bn`, slots as `sn`.
//!
//! ```
//! fn sum(%0: int[]) -> int
//! b0(%0: int[]):
//!     %1 = const int 0
//!     jump b1(%1, %1)
//! ```

const std = @import("std");
const ir = @import("ir.zig");

const Writer = std.Io.Writer;

pub fn writeFunction(w: *Writer, program: *const ir.Program, f: *const ir.Function) Writer.Error!void {
    try w.print("fn {s}(", .{f.name});
    for (f.params(), f.roles, 0..) |param, role, i| {
        if (i != 0) try w.writeAll(", ");
        try writeValueDecl(w, program, f, param);
        switch (role) {
            .value => {},
            .caller_arena => try w.writeAll(" @caller"),
            .alias => |a| try w.print(" ^@{d}", .{a.arena}),
            .alias_arena => try w.writeAll(" @alias"),
        }
    }
    try w.writeAll(") -> ");
    try writeHIRType(w, program, f.ret);
    try w.writeAll("\n");
    for (f.slots, 0..) |slot, i| {
        try w.print("  s{d}: ", .{i});
        try writeHIRType(w, program, slot.ty);
        try w.print(" in %{d}  # {s}\n", .{ @intFromEnum(slot.arena), slot.name });
    }
    for (f.blocks, 0..) |*block, i| try writeBlock(w, program, f, @enumFromInt(i), block);
}

pub fn writeBlock(w: *Writer, program: *const ir.Program, f: *const ir.Function, id: ir.BlockId, block: *const ir.Block) Writer.Error!void {
    try w.print("b{d}", .{@intFromEnum(id)});
    if (block.params.len != 0) {
        try w.writeAll("(");
        for (block.params, 0..) |param, i| {
            if (i != 0) try w.writeAll(", ");
            try writeValueDecl(w, program, f, param);
        }
        try w.writeAll(")");
    }
    try w.writeAll(":\n");
    for (block.insts) |*inst| {
        try w.writeAll("    ");
        try writeInst(w, program, f, inst);
        try w.writeAll("\n");
    }
    try w.writeAll("    ");
    try writeTerminator(w, program, &block.term);
    try w.writeAll("\n");
}

fn writeValueDecl(w: *Writer, program: *const ir.Program, f: *const ir.Function, v: ir.ValueId) Writer.Error!void {
    try w.print("%{d}: ", .{@intFromEnum(v)});
    try writeType(w, program, f.typeOf(v));
}

pub fn writeInst(w: *Writer, program: *const ir.Program, f: *const ir.Function, inst: *const ir.Inst) Writer.Error!void {
    if (inst.result) |r| {
        try w.print("%{d} = ", .{@intFromEnum(r)});
    }
    const name = opName(inst.op);
    try w.writeAll(name);
    if (inst.result) |r| {
        // The result type is what an instruction's operands do not say.
        if (showsResultType(inst.op)) {
            try w.writeAll(" ");
            try writeType(w, program, f.typeOf(r));
        }
    }
    switch (inst.op) {
        .constant => |c| {
            try w.writeAll(" ");
            try writeConstant(w, program, c);
        },
        .arith => |a| try w.print(".{s}", .{@tagName(a.op)}),
        .cmp => |c| try w.print(".{s}", .{@tagName(c.op)}),
        .tetra_binary => |t| try w.print(".{s}", .{@tagName(t.op)}),
        .cond_binary => |c| try w.print(".{s}", .{@tagName(c.op)}),
        .member_test => |m| {
            try w.writeAll(" {");
            for (m.members, 0..) |member, i| {
                if (i != 0) try w.writeAll(", ");
                try w.print("{d}", .{member});
            }
            try w.writeAll("}");
        },
        .field_get => |g| try w.print(" .{d}", .{g.index}),
        .field_set => |s| try w.print(" .{d}", .{s.index}),
        .slot_addr => |s| try w.print(" s{d}", .{@intFromEnum(s)}),
        .global_load => |g| try w.print(" {s}", .{program.globals[@intFromEnum(g)].name}),
        .global_store => |g| try w.print(" {s}", .{program.globals[@intFromEnum(g.global)].name}),
        .global_addr => |g| try w.print(" {s}", .{program.globals[@intFromEnum(g.global)].name}),
        .call => |c| try w.print(" {s}", .{program.function_names[@intFromEnum(c.callee)]}),
        .call_zig => |c| try w.print(" {s}", .{program.zig_function_names[@intFromEnum(c.callee)]}),
        .peek => |p| if (p.display.path) |path| try w.print(" \"{s}\"", .{path}),
        else => {},
    }
    var first = true;
    try ir.forEachOperand(&inst.op, OperandWriter{ .w = w, .first = &first }, OperandWriter.write);
}

const OperandWriter = struct {
    w: *Writer,
    first: *bool,

    fn write(self: OperandWriter, v: ir.ValueId) Writer.Error!void {
        try self.w.writeAll(if (self.first.*) " " else ", ");
        self.first.* = false;
        try self.w.print("%{d}", .{@intFromEnum(v)});
    }
};

fn showsResultType(op: ir.Op) bool {
    return switch (op) {
        .constant, .convert, .box, .unbox, .repack, .array_new, .map_new, .struct_new, .array_range => true,
        else => false,
    };
}

fn opName(op: ir.Op) []const u8 {
    return switch (op) {
        .constant => "const",
        .arith => "arith",
        .neg => "neg",
        .convert => "convert",
        .cmp => "cmp",
        .tetra_binary => "tetra",
        .tetra_not => "tetra.not",
        .tetra_from_cond => "tetra.from_cond",
        .tetra_holds => "tetra.holds",
        .cond_not => "cond.not",
        .cond_binary => "cond",
        .box => "box",
        .unbox => "unbox",
        .repack => "repack",
        .member_test => "member_test",
        .member_index => "member_index",
        .str_concat => "str.concat",
        .str_len => "str.len",
        .str_substring => "str.substring",
        .str_last => "str.last",
        .str_drop_last => "str.drop_last",
        .str_find => "str.find",
        .str_insert => "str.insert",
        .str_remove => "str.remove",
        .str_char => "str.char",
        .str_to_int => "str.to_int",
        .str_to_float => "str.to_float",
        .str_to_byte => "str.to_byte",
        .str_pack => "str.pack",
        .str_unpack => "str.unpack",
        .to_string => "to_string",
        .array_new => "array.new",
        .array_from_fixed => "array.from_fixed",
        .array_range => "array.range",
        .array_get => "array.get",
        .array_set => "array.set",
        .array_len => "array.len",
        .array_push => "array.push",
        .array_pop => "array.pop",
        .array_insert => "array.insert",
        .array_remove => "array.remove",
        .array_clear => "array.clear",
        .array_slice => "array.slice",
        .array_concat => "array.concat",
        .array_find => "array.find",
        .array_copy_to_fixed => "array.copy_to_fixed",
        .map_new => "map.new",
        .map_get => "map.get",
        .map_set => "map.set",
        .struct_new => "struct.new",
        .field_get => "field.get",
        .field_set => "field.set",
        .slot_addr => "slot.addr",
        .load => "load",
        .store => "store",
        .global_load => "global.load",
        .global_store => "global.store",
        .global_addr => "global.addr",
        .root_arena => "root_arena",
        .scope_enter => "scope.enter",
        .scope_exit => "scope.exit",
        .scope_reset => "scope.reset",
        .clone => "clone",
        .rehome => "rehome",
        .call => "call",
        .call_zig => "call.zig",
        .print => "print",
        .peek => "peek",
    };
}

pub fn writeTerminator(w: *Writer, program: *const ir.Program, term: *const ir.Terminator) Writer.Error!void {
    _ = program;
    switch (term.*) {
        .jump => |call| {
            try w.writeAll("jump ");
            try writeBlockCall(w, call);
        },
        .branch => |b| {
            try w.print("branch %{d}, ", .{@intFromEnum(b.cond)});
            try writeBlockCall(w, b.then);
            try w.writeAll(", ");
            try writeBlockCall(w, b.@"else");
        },
        .@"switch" => |s| {
            try w.print("switch %{d} [", .{@intFromEnum(s.operand)});
            for (s.cases, 0..) |case, i| {
                if (i != 0) try w.writeAll(", ");
                try w.print("{d}: ", .{case.value});
                try writeBlockCall(w, case.target);
            }
            try w.writeAll("] else ");
            try writeBlockCall(w, s.default);
        },
        .@"return" => |v| if (v) |value| try w.print("return %{d}", .{@intFromEnum(value)}) else try w.writeAll("return"),
        .@"unreachable" => try w.writeAll("unreachable"),
        .panic => |v| try w.print("panic %{d}", .{@intFromEnum(v)}),
        .exit => |v| try w.print("exit %{d}", .{@intFromEnum(v)}),
        .assert_fail => |a| if (a.message) |m| try w.print("assert_fail %{d}", .{@intFromEnum(m)}) else try w.writeAll("assert_fail"),
    }
}

fn writeBlockCall(w: *Writer, call: ir.BlockCall) Writer.Error!void {
    try w.print("b{d}", .{@intFromEnum(call.block)});
    if (call.args.len == 0) return;
    try w.writeAll("(");
    for (call.args, 0..) |arg, i| {
        if (i != 0) try w.writeAll(", ");
        try w.print("%{d}", .{@intFromEnum(arg)});
    }
    try w.writeAll(")");
}

fn writeConstant(w: *Writer, program: *const ir.Program, c: ir.Constant) Writer.Error!void {
    switch (c) {
        .int => |v| try w.print("{d}", .{v}),
        .byte => |v| try w.print("{d}", .{v}),
        .float => |v| try w.print("{d}", .{v}),
        .tetra => |v| try w.writeAll(@tagName(v)),
        .nothing => try w.writeAll("nothing"),
        .enum_variant => |v| try w.print(".{d}", .{v}),
        .string => |s| {
            try w.writeAll("\"");
            for (s) |ch| switch (ch) {
                '"' => try w.writeAll("\\\""),
                '\\' => try w.writeAll("\\\\"),
                '\n' => try w.writeAll("\\n"),
                0x20...0x21, 0x23...0x5b, 0x5d...0x7e => try w.writeByte(ch),
                else => try w.print("\\x{x:0>2}", .{ch}),
            };
            try w.writeAll("\"");
        },
        .function => |f| try w.writeAll(program.function_names[@intFromEnum(f)]),
    }
}

pub fn writeType(w: *Writer, program: *const ir.Program, ty: ir.Type) Writer.Error!void {
    switch (ty) {
        .doxa => |t| try writeHIRType(w, program, t),
        .cond => try w.writeAll("cond"),
        .arena => try w.writeAll("arena"),
        .ref => |t| {
            try w.writeAll("^");
            try writeHIRType(w, program, t);
        },
    }
}

pub fn writeHIRType(w: *Writer, program: *const ir.Program, t: ir.HIRType) Writer.Error!void {
    switch (t) {
        .Int => try w.writeAll("int"),
        .Byte => try w.writeAll("byte"),
        .Float => try w.writeAll("float"),
        .String => try w.writeAll("string"),
        .Tetra => try w.writeAll("tetra"),
        .Nothing => try w.writeAll("nothing"),
        .Unknown => try w.writeAll("?unknown"),
        .Poison => try w.writeAll("?poison"),
        .Array => |array| {
            try writeHIRType(w, program, array.element.*);
            if (array.size) |size| try w.print("[{d}]", .{size}) else try w.writeAll("[]");
        },
        .Map => |m| {
            try w.writeAll("map ");
            try writeHIRType(w, program, m.key.*);
            try w.writeAll(" -> ");
            try writeHIRType(w, program, m.value.*);
        },
        .Struct => |id| try writeNamed(w, program.struct_names, "S", id),
        .Enum => |id| try writeNamed(w, program.enum_names, "E", id),
        .Group => |id| try writeNamed(w, program.group_names, "G", id),
        .Union => |u| {
            try w.writeAll("(");
            for (u.members, 0..) |member, i| {
                if (i != 0) try w.writeAll(" | ");
                try writeHIRType(w, program, member.*);
            }
            try w.writeAll(")");
        },
        .Function => |f| {
            try w.writeAll("fn(");
            for (f.params, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                try writeHIRType(w, program, param.*);
            }
            try w.writeAll(") -> ");
            try writeHIRType(w, program, f.ret.*);
        },
    }
}

fn writeNamed(w: *Writer, names: []const []const u8, prefix: []const u8, id: u32) Writer.Error!void {
    if (id < names.len) return w.writeAll(names[id]);
    try w.print("{s}{d}", .{ prefix, id });
}
