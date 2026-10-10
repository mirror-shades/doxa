//! The LLVM emitter: a one-pass lowering of the register HIR
//! (`plan/register-hir.md`, "The emitter"). Each HIR block is one LLVM block
//! (plus the blocks a trap or a guard splits off it), each block parameter one
//! `phi`, each value one SSA name. Nothing is reconstructed: a value's type,
//! arena and identity are read off the HIR.
//!
//! This file is the module around the functions: runtime declarations,
//! interned strings, struct and enum descriptors, the box registry, globals,
//! and `doxa_program_main`. `function.zig` lowers each function.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const rt = @import("../../runtime/doxa_rt.zig");
const layout_mod = @import("layout.zig");
const Ranges = @import("ranges.zig").Ranges;
const function = @import("function.zig");
const consteval = @import("../../analysis/consteval.zig");
const module_graph = @import("../../module/graph.zig");

const HIRType = ir.HIRType;
const Layout = layout_mod.Layout;

/// What signed integer overflow does (`docs/performance.md` §1).
pub const OverflowPolicy = enum { trap, wrap };

pub const Error = std.mem.Allocator.Error || std.Io.Writer.Error || error{EmitFault};

pub const Emitter = struct {
    alloc: std.mem.Allocator,
    module: *const ir.Module,
    layout: Layout,
    ranges: Ranges,
    overflow: OverflowPolicy,

    /// Module-level definitions made while functions are lowered: strings,
    /// descriptors, peek member lists. Written after the functions.
    globals: std.Io.Writer.Allocating,
    strings: std.StringHashMapUnmanaged(u32) = .empty,
    struct_descs: std.AutoHashMapUnmanaged(ir.StructId, []const u8) = .empty,
    enum_descs: std.AutoHashMapUnmanaged(ir.EnumId, []const u8) = .empty,
    /// The box id of each union and group type the program boxes, in the
    /// order they were first boxed (`DoxaBoxMeta`).
    box_ids: std.AutoHashMapUnmanaged(BoxKey, u32) = .empty,
    boxed: std.ArrayListUnmanaged(HIRType) = .empty,
    next_global: u32 = 0,
    /// A fault in the HIR the emitter was handed: a compiler bug.
    fault: ?[]const u8 = null,

    const BoxKey = struct { group: bool, id: u32 };

    pub fn init(alloc: std.mem.Allocator, module: *const ir.Module, overflow: OverflowPolicy) !Emitter {
        return .{
            .alloc = alloc,
            .module = module,
            .layout = try Layout.init(alloc, module),
            .ranges = Ranges.init(alloc, module),
            .overflow = overflow,
            .globals = std.Io.Writer.Allocating.init(alloc),
        };
    }

    pub fn emit(self: *Emitter, w: *std.Io.Writer) Error!void {
        try w.writeAll(prelude);
        if (self.overflow == .trap) try w.writeAll(overflow_prelude);
        try self.declareZigFunctions(w);

        for (self.module.functions, 0..) |*f, i| {
            try function.emitFunction(self, w, @enumFromInt(i), f);
        }
        try self.emitMain(w);
        try self.emitGlobals(w);
        try self.emitBoxRegistry();
        try w.writeAll(self.globals.written());
        try w.writeAll(byte_pow);
        try w.writeAll("\nattributes #0 = { \"tune-cpu\"=\"generic\" }\n");
    }

    pub fn faultf(self: *Emitter, comptime fmt: []const u8, args: anytype) Error {
        self.fault = try std.fmt.allocPrint(self.alloc, fmt, args);
        return error.EmitFault;
    }

    fn declareZigFunctions(self: *Emitter, w: *std.Io.Writer) Error!void {
        for (self.module.program.zig_functions, self.module.program.zig_function_names) |sig, name| {
            try w.print("declare {s} @{s}(", .{ zigReturnType(sig.ret), name });
            var first = true;
            for (sig.params, sig.roles) |param, role| {
                if (!first) try w.writeAll(", ");
                first = false;
                if (role == .caller_arena) {
                    try w.writeAll("ptr");
                    continue;
                }
                try w.writeAll(zigParamType(param.doxa));
            }
            for (zigOutParams(sig.ret)) |out| {
                if (!first) try w.writeAll(", ");
                first = false;
                try w.writeAll(out);
            }
            try w.writeAll(")\n");
        }
    }

    /// `doxa_program_main`: register the runtime's type registries, take the
    /// root arena, and run `__doxa_init`.
    fn emitMain(self: *Emitter, w: *std.Io.Writer) Error!void {
        try w.writeAll("\ndefine void @doxa_program_main() #0 {\nentry:\n");
        for (self.module.program.enum_keys, 0..) |_, id| {
            try w.print("  call void @doxa_enum_register(ptr {s})\n", .{try self.enumDesc(@intCast(id))});
        }
        try w.writeAll("  call void @doxa_box_registry_init(ptr @.doxa.boxes, ptr @.doxa.box.count)\n");
        try w.print("  call void {s}()\n  ret void\n}}\n", .{try self.functionSymbol(self.module.init)});
    }

    fn emitGlobals(self: *Emitter, w: *std.Io.Writer) Error!void {
        for (self.module.program.globals) |g| {
            try w.print("@{s} = internal global {s} zeroinitializer\n", .{ g.name, layout_mod.doxaType(g.ty) });
        }
    }

    pub fn functionSymbol(self: *Emitter, id: ir.FunctionId) Error![]const u8 {
        return std.fmt.allocPrint(self.alloc, "@{s}", .{self.module.program.function_names[@intFromEnum(id)]});
    }

    pub fn globalSymbol(self: *Emitter, id: ir.GlobalId) Error![]const u8 {
        return std.fmt.allocPrint(self.alloc, "@{s}", .{self.module.program.globals[@intFromEnum(id)].name});
    }

    // ── Strings ──

    /// A NUL-terminated constant holding `bytes`, as the name of its first
    /// byte. Equal strings share one global.
    pub fn string(self: *Emitter, bytes: []const u8) Error![]const u8 {
        const entry = try self.strings.getOrPut(self.alloc, bytes);
        if (!entry.found_existing) {
            entry.key_ptr.* = try self.alloc.dupe(u8, bytes);
            entry.value_ptr.* = self.next_global;
            self.next_global += 1;
            const g = &self.globals.writer;
            try g.print("@.s.{d} = private unnamed_addr constant [{d} x i8] c\"", .{ entry.value_ptr.*, bytes.len + 1 });
            try writeEscaped(g, bytes);
            try g.writeAll("\\00\"\n");
        }
        return std.fmt.allocPrint(self.alloc, "@.s.{d}", .{entry.value_ptr.*});
    }

    /// Each of `texts` interned (`string`).
    fn internAll(self: *Emitter, texts: []const []const u8) Error![]const []const u8 {
        const refs = try self.alloc.alloc([]const u8, texts.len);
        for (texts, refs) |t, *r| r.* = try self.string(t);
        return refs;
    }

    /// `bytes` as a `%DoxaString` constant operand.
    pub fn stringConstant(self: *Emitter, bytes: []const u8) Error![]const u8 {
        return std.fmt.allocPrint(self.alloc, "{{ ptr {s}, i64 {d} }}", .{ try self.string(bytes), bytes.len });
    }

    fn nextGlobal(self: *Emitter, comptime prefix: []const u8) Error![]const u8 {
        defer self.next_global += 1;
        return std.fmt.allocPrint(self.alloc, "@.doxa." ++ prefix ++ ".{d}", .{self.next_global});
    }

    // ── Descriptors ──

    /// The runtime descriptor of struct `id`: its display name, field names,
    /// field tags, the enum each enum field shows, and the word count of each
    /// descriptor-free nested struct field.
    pub fn structDesc(self: *Emitter, id: ir.StructId) Error![]const u8 {
        if (self.struct_descs.get(id)) |name| return name;
        const p = &self.module.program;
        const fields = p.struct_fields[id];
        const names = p.struct_field_names[id];
        const g = &self.globals.writer;

        // Every string is interned before a definition is begun: interning
        // writes a global of its own.
        const name_refs = try self.internAll(names);
        const enum_refs = try self.alloc.alloc([]const u8, fields.len);
        for (fields, enum_refs) |t, *r| r.* = if (t == .Enum) try self.string(p.enum_keys[t.Enum]) else "null";
        const type_name = try self.string(p.struct_names[id]);

        const name_list = try self.nextGlobal("struct.names");
        const tag_list = try self.nextGlobal("struct.tags");
        const enum_list = try self.nextGlobal("struct.enums");
        try g.print("{s} = private constant [{d} x ptr] [", .{ name_list, fields.len });
        for (name_refs, 0..) |name, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("ptr {s}", .{name});
        }
        try g.print("]\n{s} = private constant [{d} x i64] [", .{ tag_list, fields.len });
        for (fields, 0..) |t, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("i64 {d}", .{layout_mod.fieldTag(t)});
        }
        try g.print("]\n{s} = private constant [{d} x ptr] [", .{ enum_list, fields.len });
        for (enum_refs, 0..) |r, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("ptr {s}", .{r});
        }
        try g.writeAll("]\n");

        var words_ref: []const u8 = "ptr null";
        for (fields) |t| {
            if (self.layout.skippedWords(t) == 0) continue;
            const words = try self.nextGlobal("struct.words");
            try g.print("{s} = private constant [{d} x i64] [", .{ words, fields.len });
            for (fields, 0..) |ft, i| {
                if (i != 0) try g.writeAll(", ");
                try g.print("i64 {d}", .{self.layout.skippedWords(ft)});
            }
            try g.writeAll("]\n");
            words_ref = try std.fmt.allocPrint(self.alloc, "ptr {s}", .{words});
            break;
        }

        const desc = try self.nextGlobal("struct.desc");
        try g.print("{s} = private constant {{ ptr, i64, ptr, ptr, ptr, ptr }} {{ ptr {s}, i64 {d}, ptr {s}, ptr {s}, ptr {s}, {s} }}\n", .{
            desc, type_name, fields.len, name_list, tag_list, enum_list, words_ref,
        });
        try self.struct_descs.put(self.alloc, id, desc);
        return desc;
    }

    /// The runtime descriptor of enum `id`, found by its canonical key.
    pub fn enumDesc(self: *Emitter, id: ir.EnumId) Error![]const u8 {
        if (self.enum_descs.get(id)) |name| return name;
        const p = &self.module.program;
        const variants = p.enum_variants[id];
        const g = &self.globals.writer;
        const refs = try self.internAll(variants);
        const key = try self.string(p.enum_keys[id]);
        const names = try self.nextGlobal("enum.names");
        try g.print("{s} = private constant [{d} x ptr] [", .{ names, variants.len });
        for (refs, 0..) |r, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("ptr {s}", .{r});
        }
        try g.writeAll("]\n");
        const desc = try self.nextGlobal("enum.desc");
        try g.print("{s} = private constant {{ ptr, i64, ptr }} {{ ptr {s}, i64 {d}, ptr {s} }}\n", .{ desc, key, variants.len, names });
        try self.enum_descs.put(self.alloc, id, desc);
        return desc;
    }

    // ── Boxes ──

    /// The `reserved` word of a box of type `boxed` before its member index
    /// is or-ed in: the boxed flag and the type's box id, numbered the first
    /// time the type is boxed.
    pub fn boxHeader(self: *Emitter, boxed: HIRType) Error!u32 {
        const key: BoxKey = switch (boxed) {
            .Union => |u| .{ .group = false, .id = u.id },
            .Group => |gid| .{ .group = true, .id = gid },
            else => return self.faultf("boxes a {s}, which is not a union or a group", .{@tagName(boxed)}),
        };
        const entry = try self.box_ids.getOrPut(self.alloc, key);
        if (!entry.found_existing) {
            const id: u32 = @intCast(self.boxed.items.len);
            if (id > rt.DoxaBoxMeta.max_box_id) return self.faultf("boxes more than {d} union and group types", .{rt.DoxaBoxMeta.max_box_id + 1});
            try self.boxed.append(self.alloc, boxed);
            entry.value_ptr.* = id;
        }
        return rt.DoxaBoxMeta.is_boxed_bit | (entry.value_ptr.* << rt.DoxaBoxMeta.box_id_shift);
    }

    /// The members of a box of type `boxed`, in member-index order.
    pub fn boxMembers(self: *Emitter, boxed: HIRType) Error![]const HIRType {
        var buf: [256]HIRType = undefined;
        const members = self.module.program.boxMembers(boxed, &buf) orelse return self.faultf("a box type with no member list", .{});
        return self.alloc.dupe(HIRType, members);
    }

    /// The box registry: one `BoxDesc` per box id, naming each enum member so
    /// the runtime prints a boxed enum `Type.Variant`.
    fn emitBoxRegistry(self: *Emitter) Error!void {
        const g = &self.globals.writer;
        const p = &self.module.program;
        for (self.boxed.items, 0..) |boxed, box_id| {
            const members = try self.boxMembers(boxed);
            const names = try self.alloc.alloc([]const u8, members.len);
            const enums = try self.alloc.alloc([]const u8, members.len);
            for (members, names, enums) |m, *n, *d| {
                n.* = if (m == .Enum) try self.string(p.enum_names[m.Enum]) else "null";
                d.* = if (m == .Enum) try self.enumDesc(m.Enum) else "null";
            }
            try g.print("@.doxa.box.names.{d} = private constant [{d} x ptr] [", .{ box_id, members.len });
            for (names, 0..) |n, i| {
                if (i != 0) try g.writeAll(", ");
                try g.print("ptr {s}", .{n});
            }
            try g.print("]\n@.doxa.box.enums.{d} = private constant [{d} x ptr] [", .{ box_id, members.len });
            for (enums, 0..) |d, i| {
                if (i != 0) try g.writeAll(", ");
                try g.print("ptr {s}", .{d});
            }
            try g.print("]\n@.doxa.box.desc.{d} = private constant {{ i64, ptr, ptr }} {{ i64 {d}, ptr @.doxa.box.names.{d}, ptr @.doxa.box.enums.{d} }}\n", .{ box_id, members.len, box_id, box_id });
        }
        try g.print("@.doxa.boxes = private constant [{d} x ptr] [", .{self.boxed.items.len});
        for (self.boxed.items, 0..) |_, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("ptr @.doxa.box.desc.{d}", .{i});
        }
        try g.print("]\n@.doxa.box.count = private constant i64 {d}\n", .{self.boxed.items.len});
    }

    /// A peek's member list, as a global array of names.
    pub fn nameList(self: *Emitter, names: []const []const u8) Error![]const u8 {
        const refs = try self.internAll(names);
        const list = try self.nextGlobal("peek.members");
        const g = &self.globals.writer;
        try g.print("{s} = private constant [{d} x ptr] [", .{ list, names.len });
        for (refs, 0..) |r, i| {
            if (i != 0) try g.writeAll(", ");
            try g.print("ptr {s}", .{r});
        }
        try g.writeAll("]\n");
        return list;
    }
};

fn writeEscaped(w: *std.Io.Writer, bytes: []const u8) !void {
    for (bytes) |c| {
        if (c >= 0x20 and c < 0x7f and c != '"' and c != '\\') {
            try w.writeByte(c);
        } else {
            try w.print("\\{X:0>2}", .{c});
        }
    }
}

// ── The inline-Zig ABI (`docs/zig.md`) ──

fn zigReturnType(t: HIRType) []const u8 {
    return switch (t) {
        // A string is written through out-parameters; a fallible result
        // crosses as an `i64` (-1 for success, else the error's variant).
        .String, .Nothing => "void",
        .Union => "i64",
        .Tetra => "i1",
        else => layout_mod.doxaType(t),
    };
}

fn zigParamType(t: HIRType) []const u8 {
    return switch (t) {
        .String => "ptr, i64",
        .Tetra => "i1",
        else => layout_mod.doxaType(t),
    };
}

/// The trailing out-parameters a Zig wrapper returning `t` writes through.
pub fn zigOutParams(t: HIRType) []const []const u8 {
    return switch (t) {
        .String => &.{ "ptr", "ptr" },
        else => &.{},
    };
}

const prelude =
    \\%DoxaString = type { ptr, i64 }
    \\%DoxaValue = type { i32, i32, i64, i64 }
    \\%ArrayHeader = type { ptr, i64, i64, i64, i64, ptr, i64 }
    \\%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }
    \\
    \\declare ptr @doxa_scope_root()
    \\declare ptr @doxa_scope_enter(ptr)
    \\declare void @doxa_scope_exit(ptr)
    \\declare void @doxa_scope_reset(ptr)
    \\declare ptr @doxa_scope_alloc(ptr, i64, i64)
    \\declare void @doxa_write_cstr(ptr, i64)
    \\declare void @doxa_write_stderr(ptr, i64)
    \\declare void @doxa_exit(i64) noreturn
    \\declare void @doxa_panic(ptr, i64) noreturn
    \\declare void @doxa_trap_div_by_zero() noreturn
    \\declare void @doxa_trap_unreachable() noreturn
    \\declare void @doxa_print_i64(i64)
    \\declare void @doxa_print_f64(double)
    \\declare void @doxa_print_byte(i64)
    \\declare void @doxa_print_tetra(i64)
    \\declare void @doxa_peek_string(ptr, i64)
    \\declare void @doxa_peek_end()
    \\declare void @doxa_debug_peek(ptr)
    \\declare void @doxa_print_value(ptr)
    \\declare void @doxa_print_array_hdr(ptr)
    \\declare void @doxa_print_enum(ptr, i64)
    \\declare i1 @doxa_str_eq(ptr, i64, ptr, i64)
    \\declare i32 @doxa_str_cmp(ptr, i64, ptr, i64)
    \\declare void @doxa_str_concat(ptr, ptr, i64, ptr, i64, ptr, ptr)
    \\declare void @doxa_str_clone(ptr, ptr, i64, ptr, ptr)
    \\declare ptr @doxa_str_clone_raw(ptr, ptr, i64)
    \\declare void @doxa_str_from_cstr(ptr, ptr, ptr, ptr)
    \\declare void @doxa_substring(ptr, ptr, i64, i64, i64, ptr, ptr)
    \\declare void @doxa_str_insert(ptr, ptr, i64, i64, ptr, i64, ptr, ptr)
    \\declare i8 @doxa_str_remove(ptr, ptr, i64, i64, ptr, ptr)
    \\declare i8 @doxa_str_pop(ptr, ptr, i64, ptr, ptr)
    \\declare void @doxa_char_to_string(ptr, i8, ptr, ptr)
    \\declare i64 @doxa_find_str(ptr, i64, ptr, i64)
    \\declare i64 @doxa_int_from_string(ptr, i64)
    \\declare double @doxa_float_from_string(ptr, i64)
    \\declare i64 @doxa_byte_from_string(ptr, i64)
    \\declare i64 @doxa_byte_from_f64(double)
    \\declare void @doxa_int_to_string(ptr, i64, ptr, ptr)
    \\declare void @doxa_float_to_string(ptr, double, ptr, ptr)
    \\declare void @doxa_byte_to_string(ptr, i64, ptr, ptr)
    \\declare void @doxa_tetra_to_string(ptr, i64, ptr, ptr)
    \\declare void @doxa_nothing_to_string(ptr, ptr, ptr)
    \\declare void @doxa_enum_to_string(ptr, ptr, i64, i64, ptr, ptr)
    \\declare void @doxa_struct_to_string(ptr, ptr, ptr, ptr)
    \\declare void @doxa_array_to_string(ptr, ptr, ptr, ptr)
    \\declare void @doxa_value_to_string(ptr, ptr, ptr, ptr)
    \\declare void @doxa_pack_bytes(ptr, ptr, ptr, ptr)
    \\declare ptr @doxa_unpack_bytes(ptr, ptr, i64)
    \\declare ptr @doxa_array_new(ptr, i64, i64, i64)
    \\declare void @doxa_array_set_elem_words(ptr, i64)
    \\declare ptr @doxa_array_range(ptr, i64, i64)
    \\declare ptr @doxa_array_from_fixed(ptr, ptr, ptr, i64, i64, i64)
    \\declare ptr @doxa_array_from_fixed_structs(ptr, ptr, i64, i64, ptr)
    \\declare ptr @doxa_array_clone(ptr, ptr)
    \\declare ptr @doxa_array_rehome(ptr, ptr)
    \\declare void @doxa_array_set_i64(ptr, i64, i64)
    \\declare void @doxa_array_set_str(ptr, i64, ptr, i64)
    \\declare void @doxa_array_set_value(ptr, i64, ptr)
    \\declare ptr @doxa_array_concat(ptr, ptr, ptr)
    \\declare void @doxa_array_insert(ptr, i64, i64)
    \\declare i64 @doxa_array_remove(ptr, i64)
    \\declare void @doxa_array_insert_str(ptr, i64, ptr, i64)
    \\declare void @doxa_array_remove_str(ptr, i64, ptr, ptr)
    \\declare void @doxa_array_insert_value(ptr, i64, ptr)
    \\declare void @doxa_array_remove_value(ptr, i64, ptr)
    \\declare ptr @doxa_array_slice(ptr, ptr, i64, i64)
    \\declare void @doxa_array_clear(ptr)
    \\declare i64 @doxa_find_array(ptr, i64)
    \\declare i64 @doxa_find_array_str(ptr, ptr, i64)
    \\declare void @doxa_fixed_structs_default(ptr, ptr, i64, ptr)
    \\declare void @doxa_array_copy_to_fixed(ptr, ptr, ptr, i64, i64)
    \\declare void @doxa_array_copy_to_fixed_structs(ptr, ptr, i64, i64)
    \\declare ptr @doxa_map_new(ptr, i64, i64, i64)
    \\declare ptr @doxa_map_scope(ptr)
    \\declare void @doxa_map_set_i64(ptr, i64, i64)
    \\declare void @doxa_map_set_str(ptr, ptr, i64, i64)
    \\declare void @doxa_map_set_else_i64(ptr, i64)
    \\declare i8 @doxa_map_try_get_i64(ptr, i64, ptr)
    \\declare i8 @doxa_map_try_get_str(ptr, ptr, i64, ptr)
    \\declare void @doxa_struct_register(ptr, ptr, ptr)
    \\declare ptr @doxa_struct_scope(ptr)
    \\declare ptr @doxa_struct_clone(ptr, ptr)
    \\declare ptr @doxa_struct_clone_scalar(ptr, i64, ptr)
    \\declare ptr @doxa_struct_rehome(ptr, ptr)
    \\declare ptr @doxa_struct_rehome_scalar(ptr, i64, ptr)
    \\declare void @doxa_clone_doxa_value(ptr, ptr)
    \\declare void @doxa_enum_register(ptr)
    \\declare void @doxa_box_registry_init(ptr, ptr)
    \\declare double @llvm.pow.f64(double, double)
    \\declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
    \\declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
    \\
++ tetraTables();

/// The binary tetra operators' truth tables, from `consteval`'s: what the
/// folder computes for an operator is what the program computes.
fn tetraTables() []const u8 {
    comptime {
        var text: []const u8 = "";
        for (.{ "and", "or", "iff", "xor", "nand", "nor", "implies" }) |name| {
            const table = @field(consteval.truth_tables, name);
            var rows: []const u8 = "";
            for (table, 0..) |row, i| {
                rows = rows ++ (if (i == 0) "" else ", ") ++ std.fmt.comptimePrint("[4 x i8] [i8 {d}, i8 {d}, i8 {d}, i8 {d}]", .{ row[0], row[1], row[2], row[3] });
            }
            text = text ++ "@tetra_" ++ name ++ "_lut = private constant [4 x [4 x i8]] [" ++ rows ++ "]\n";
        }
        return text;
    }
}

const overflow_prelude =
    \\declare void @llvm.trap() noreturn
    \\declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)
    \\declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64)
    \\declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64)
    \\
;

/// `byte ** byte`, wrapping mod 256 like every byte operation: square and
/// multiply over the exponent's bits.
const byte_pow =
    \\
    \\define internal i8 @doxa.byte.pow(i8 %base, i8 %exp) #0 {
    \\entry:
    \\  br label %loop
    \\loop:
    \\  %b = phi i8 [ %base, %entry ], [ %b2, %next ]
    \\  %e = phi i8 [ %exp, %entry ], [ %e2, %next ]
    \\  %acc = phi i8 [ 1, %entry ], [ %acc2, %next ]
    \\  %done = icmp eq i8 %e, 0
    \\  br i1 %done, label %exit, label %next
    \\next:
    \\  %odd = and i8 %e, 1
    \\  %is_odd = icmp ne i8 %odd, 0
    \\  %mul = mul i8 %acc, %b
    \\  %acc2 = select i1 %is_odd, i8 %mul, i8 %acc
    \\  %b2 = mul i8 %b, %b
    \\  %e2 = lshr i8 %e, 1
    \\  br label %loop
    \\exit:
    \\  ret i8 %acc
    \\}
    \\
;
