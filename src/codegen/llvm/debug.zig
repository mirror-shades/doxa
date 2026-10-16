//! Debug metadata under `-g` (`plan/diagnostics-and-observability.md`, step
//! 3): one compile unit, a file per module record, a subprogram per function,
//! and a location on every instruction the emitter writes. Everything is read
//! off the spans the HIR carries; nothing is derived here.
//!
//! Stepping is statement-granular through LLVM's Key Instructions: a
//! statement is an atom group, and the first instruction emitted for it is
//! the group's only key instruction (rank 1), located at the statement's own
//! start. Every other instruction carries its expression's line and column
//! for attribution and no atom, so it never becomes a step stop.
//!
//! Without `-g` no `Debug` exists and the emitted IR is unchanged.

const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const layout_mod = @import("layout.zig");
const rt = @import("../../runtime/doxa_rt.zig");

const HIRType = ir.HIRType;

pub const Format = enum { dwarf, codeview };

pub const Options = struct {
    format: Format,
    /// Whether the program is optimized (`isOptimized` on the compile unit).
    optimized: bool,
};

pub const Error = std.mem.Allocator.Error || std.Io.Writer.Error;

pub const Debug = struct {
    alloc: std.mem.Allocator,
    options: Options,
    program: *const ir.Program,
    /// The emitter's layout facts: where each struct's fields sit.
    layout: layout_mod.Layout,
    /// Numbered metadata nodes, written after the functions.
    nodes: std.Io.Writer.Allocating,
    next: u32 = 0,
    unit: u32,
    /// The `!DIFile` of each module record, made on first use.
    files: std.AutoHashMapUnmanaged(ir.ModuleId, u32) = .empty,
    /// The `!DILocalVariable` of each local, made on first use: one node per
    /// binding, shared by every `#dbg_value` that names it.
    locals: std.AutoHashMapUnmanaged(LocalKey, u32) = .empty,
    /// The `!DIType` of each named type, made on first use.
    enums: std.AutoHashMapUnmanaged(ir.EnumId, u32) = .empty,
    structs: std.AutoHashMapUnmanaged(ir.StructId, u32) = .empty,
    /// The two value shapes every program shares.
    string_type: ?u32 = null,
    box_type: ?u32 = null,
    /// The `DoxaTag` enumeration the box's `tag` field holds, made once.
    tag_type: ?u32 = null,

    const LocalKey = struct { function: u32, local: u32 };

    pub fn init(alloc: std.mem.Allocator, program: *const ir.Program, layout: layout_mod.Layout, entry: ir.ModuleId, options: Options) Error!Debug {
        var self = Debug{
            .alloc = alloc,
            .options = options,
            .program = program,
            .layout = layout,
            .nodes = std.Io.Writer.Allocating.init(alloc),
            .unit = undefined,
        };
        const entry_file = try self.file(entry);
        self.unit = try self.node(
            "distinct !DICompileUnit(language: DW_LANG_C, file: !{d}, producer: \"doxa\", isOptimized: {s}, runtimeVersion: 0, emissionKind: FullDebug)",
            .{ entry_file, if (options.optimized) "true" else "false" },
        );
        return self;
    }

    fn node(self: *Debug, comptime fmt: []const u8, args: anytype) Error!u32 {
        const id = self.next;
        self.next += 1;
        try self.nodes.writer.print("!{d} = " ++ fmt ++ "\n", .{id} ++ args);
        return id;
    }

    /// Reserve a node number to write later: a recursive type names itself
    /// before its members are made.
    fn reserve(self: *Debug) u32 {
        defer self.next += 1;
        return self.next;
    }

    /// Write the node `id` reserved earlier.
    fn define(self: *Debug, id: u32, comptime fmt: []const u8, args: anytype) Error!void {
        try self.nodes.writer.print("!{d} = " ++ fmt ++ "\n", .{id} ++ args);
    }

    /// An unnamed metadata tuple `!{!a, !b, …}`.
    fn list(self: *Debug, ids: []const u32) Error!u32 {
        var text = std.Io.Writer.Allocating.init(self.alloc);
        try text.writer.writeAll("!{");
        for (ids, 0..) |id, i| {
            if (i != 0) try text.writer.writeAll(", ");
            try text.writer.print("!{d}", .{id});
        }
        try text.writer.writeAll("}");
        const list_node = self.reserve();
        try self.nodes.writer.print("!{d} = {s}\n", .{ list_node, text.written() });
        return list_node;
    }

    /// A module record's file, named by its display path: never a path on
    /// this machine. Under DWARF its directory is `.`, the project directory
    /// a debugger is pointed at: relative, yet present. An empty one makes
    /// LLVM omit the compile unit's `DW_AT_comp_dir`, and DWARF readers that
    /// require it (Zig's, which the runtime's backtrace uses) then find no
    /// line table. CodeView has no such attribute, and lld-link would root a
    /// `.` at the link's working directory, a path on this machine: there it
    /// stays empty.
    fn file(self: *Debug, module: ir.ModuleId) Error!u32 {
        const entry = try self.files.getOrPut(self.alloc, module);
        if (!entry.found_existing) {
            const directory = switch (self.options.format) {
                .dwarf => ".",
                .codeview => "",
            };
            entry.value_ptr.* = try self.node("!DIFile(filename: \"{f}\", directory: \"{s}\")", .{ escaped(self.program.files[module]), directory });
        }
        return entry.value_ptr.*;
    }

    /// The subprogram describing `f`, linked as `link_name`.
    pub fn subprogram(self: *Debug, f: *const ir.Function, link_name: []const u8) Error!u32 {
        const f_file = try self.file(f.span.module);
        return self.node(
            "distinct !DISubprogram(name: \"{f}\", linkageName: \"{f}\", scope: !{d}, file: !{d}, line: {d}, type: !{d}, scopeLine: {d}, spFlags: DISPFlagDefinition{s}, unit: !{d}, keyInstructions: true)",
            .{
                escaped(f.display_name),
                escaped(link_name),
                f_file,
                f_file,
                f.span.line,
                try self.subroutineOf(f),
                f.span.line,
                if (self.options.optimized) " | DISPFlagOptimized" else "",
                self.unit,
            },
        );
    }

    /// The `!DISubroutineType` of `f`: its return, then each parameter the
    /// program declares, in the order a call passes them. The hidden `@caller`
    /// arena and an alias's arena pointer are not the program's.
    fn subroutineOf(self: *Debug, f: *const ir.Function) Error!u32 {
        var types: std.ArrayListUnmanaged(u32) = .empty;
        try types.append(self.alloc, try self.returnType(f.ret));
        for (f.params(), f.roles) |p, role| {
            switch (role) {
                .caller_arena, .alias_arena => continue,
                else => {},
            }
            try types.append(self.alloc, try self.typeOf(f.typeOf(p)));
        }
        return self.node("!DISubroutineType(types: !{d})", .{try self.list(types.items)});
    }

    fn returnType(self: *Debug, t: HIRType) Error!u32 {
        if (t == .Nothing) return self.voidType();
        return self.doxaType(t);
    }

    /// The location of an instruction at `span` in `scope`. A statement's
    /// key instruction sits at the statement's own start.
    pub fn location(self: *Debug, scope: u32, f: *const ir.Function, span: ir.SrcSpan, key: bool) Error!u32 {
        if (span.artificial) return self.node("!DILocation(line: 0, scope: !{d})", .{scope});
        if (key) {
            const start = f.statements[span.stmt];
            return self.node("!DILocation(line: {d}, column: {d}, scope: !{d}, atomGroup: {d}, atomRank: 1)", .{ start.line, start.column, scope, span.stmt + 1 });
        }
        return self.node("!DILocation(line: {d}, column: {d}, scope: !{d})", .{ span.line, span.column, scope });
    }

    /// The `!DILocalVariable` of one of `f`'s locals, made on first use.
    /// `arg` is its 1-based argument number when it is a parameter, else 0.
    pub fn localVariable(self: *Debug, f: *const ir.Function, function: ir.FunctionId, local: ir.LocalId, ty: ir.Type, scope: u32, arg: u32) Error!u32 {
        const key = LocalKey{ .function = @intFromEnum(function), .local = @intFromEnum(local) };
        const entry = try self.locals.getOrPut(self.alloc, key);
        if (entry.found_existing) return entry.value_ptr.*;
        const info = f.locals[@intFromEnum(local)];
        const file_node = try self.file(info.span.module);
        const type_node = try self.typeOf(ty);
        entry.value_ptr.* = if (arg == 0)
            try self.node("!DILocalVariable(name: \"{f}\", scope: !{d}, file: !{d}, line: {d}, type: !{d})", .{ escaped(info.name), scope, file_node, info.span.line, type_node })
        else
            try self.node("!DILocalVariable(name: \"{f}\", scope: !{d}, file: !{d}, line: {d}, arg: {d}, type: !{d})", .{ escaped(info.name), scope, file_node, info.span.line, arg, type_node });
        return entry.value_ptr.*;
    }

    // ── Debug types ──

    /// The `!DIType` of a value of `t`, from the same `layout.zig` facts the
    /// emitter lays the value out with. Every kind is one construct DWARF and
    /// CodeView both render.
    pub fn typeOf(self: *Debug, t: ir.Type) Error!u32 {
        return switch (t) {
            .doxa => |d| self.doxaType(d),
            // A `^` parameter is an address of the value it names.
            .ref => |d| self.pointer(try self.doxaType(d)),
            .arena => self.pointer(null),
            .cond => self.node("!DIBasicType(name: \"cond\", size: 1, encoding: DW_ATE_boolean)", .{}),
        };
    }

    fn doxaType(self: *Debug, t: HIRType) Error!u32 {
        return switch (t) {
            .Int => self.node("!DIBasicType(name: \"int\", size: 64, encoding: DW_ATE_signed)", .{}),
            .Byte => self.node("!DIBasicType(name: \"byte\", size: 8, encoding: DW_ATE_unsigned)", .{}),
            .Float => self.node("!DIBasicType(name: \"float\", size: 64, encoding: DW_ATE_float)", .{}),
            .Nothing => self.node("!DIBasicType(name: \"nothing\", size: 1, encoding: DW_ATE_boolean)", .{}),
            .Tetra => self.tetraType(),
            .Enum => |id| self.enumType(id),
            .String => self.stringType(),
            // A struct value is a pointer to its word block; a container is a
            // pointer the runtime walks; a box is the box itself.
            .Struct => |id| self.pointer(try self.structType(id)),
            .Union, .Group => self.boxType(),
            .Array => |a| self.pointer(if (a.size) |_| try self.fixedArrayType(t) else try self.arrayHeaderType(a.element.*)),
            .Function => |sig| self.pointer(try self.signatureType(sig.params, sig.ret.*)),
            .Unknown, .Poison => unreachable, // analysis hands codegen complete types
        };
    }

    fn voidType(self: *Debug) Error!u32 {
        return self.node("!DIBasicType(name: \"void\", size: 0, encoding: DW_ATE_unsigned)", .{});
    }

    fn pointer(self: *Debug, base: ?u32) Error!u32 {
        const pointee = base orelse try self.voidType();
        return self.node("!DIDerivedType(tag: DW_TAG_pointer_type, baseType: !{d}, size: 64)", .{pointee});
    }

    /// `!DISubroutineType`: a return type, then each parameter.
    fn signatureType(self: *Debug, params: []const *const HIRType, ret: HIRType) Error!u32 {
        var types: std.ArrayListUnmanaged(u32) = .empty;
        defer types.deinit(self.alloc);
        try types.append(self.alloc, try self.returnType(ret));
        for (params) |p| try types.append(self.alloc, try self.doxaType(p.*));
        return self.node("!DISubroutineType(types: !{d})", .{try self.list(types.items)});
    }

    /// A fixed array's buffer, as the pointer a value carries points at: the
    /// array type with its extent, so a debugger expands it element by element.
    fn fixedArrayType(self: *Debug, t: HIRType) Error!u32 {
        const element = t.Array.element.*;
        const subrange = try self.node("!DISubrange(count: {d})", .{t.Array.size.?});
        return self.node("!DICompositeType(tag: DW_TAG_array_type, baseType: !{d}, size: {d}, elements: !{d})", .{
            try self.fixedElementType(element),
            layout_mod.fixedBytes(&self.layout, t) * 8,
            try self.list(&.{subrange}),
        });
    }

    /// A fixed array's element type: a nested array or a flat struct is
    /// stored inline, so its own type describes it, not a pointer to it.
    fn fixedElementType(self: *Debug, element: HIRType) Error!u32 {
        if (element == .Array and element.Array.size != null) return self.fixedArrayType(element);
        if (element == .Struct and self.layout.isFlat(element.Struct)) return self.structType(element.Struct);
        return self.doxaType(element);
    }

    /// A dynamic array's value is a pointer to its `ArrayHeader`, whose
    /// `data` member is a pointer to the element type and `len` its count.
    fn arrayHeaderType(self: *Debug, element: HIRType) Error!u32 {
        const data = try self.pointer(try self.doxaType(element));
        const u64t = try self.node("!DIBasicType(name: \"int\", size: 64, encoding: DW_ATE_unsigned)", .{});
        const scope = try self.pointer(null);
        const members = try self.list(&.{
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"data\", baseType: !{d}, offset: 0)", .{data}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"len\", baseType: !{d}, offset: 64)", .{u64t}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"cap\", baseType: !{d}, offset: 128)", .{u64t}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"elem_size\", baseType: !{d}, offset: 192)", .{u64t}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"elem_tag\", baseType: !{d}, offset: 256)", .{u64t}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"scope\", baseType: !{d}, offset: 320)", .{scope}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"elem_words\", baseType: !{d}, offset: 384)", .{u64t}),
        });
        return self.node("!DICompositeType(tag: DW_TAG_structure_type, name: \"array\", size: 448, elements: !{d})", .{members});
    }

    fn tetraType(self: *Debug) Error!u32 {
        const values = [_][]const u8{ "false", "true", "both", "neither" };
        const elems = try self.alloc.alloc(u32, values.len);
        for (values, 0..) |name, i| elems[i] = try self.node("!DIEnumerator(name: \"{s}\", value: {d})", .{ name, i });
        const underlying = try self.node("!DIBasicType(name: \"byte\", size: 8, encoding: DW_ATE_unsigned)", .{});
        return self.node("!DICompositeType(tag: DW_TAG_enumeration_type, name: \"tetra\", baseType: !{d}, size: 8, elements: !{d})", .{ underlying, try self.list(elems) });
    }

    fn enumType(self: *Debug, id: ir.EnumId) Error!u32 {
        if (self.enums.get(id)) |n| return n;
        const variants = self.program.enum_variants[id];
        const elems = try self.alloc.alloc(u32, variants.len);
        for (variants, 0..) |name, i| elems[i] = try self.node("!DIEnumerator(name: \"{f}\", value: {d})", .{ escaped(name), i });
        const underlying = try self.node("!DIBasicType(name: \"int\", size: 64, encoding: DW_ATE_signed)", .{});
        const type_node = try self.node("distinct !DICompositeType(tag: DW_TAG_enumeration_type, name: \"{f}\", baseType: !{d}, size: 64, elements: !{d})", .{ escaped(self.program.enum_names[id]), underlying, try self.list(elems) });
        try self.enums.put(self.alloc, id, type_node);
        return type_node;
    }

    /// A string is `{ ptr: *byte, len: int }`, the `%DoxaString` the runtime
    /// passes and stores.
    fn stringType(self: *Debug) Error!u32 {
        if (self.string_type) |n| return n;
        const byte = try self.node("!DIBasicType(name: \"byte\", size: 8, encoding: DW_ATE_unsigned)", .{});
        const int = try self.node("!DIBasicType(name: \"int\", size: 64, encoding: DW_ATE_signed)", .{});
        const ptr = try self.pointer(byte);
        const members = try self.list(&.{
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"ptr\", baseType: !{d}, offset: 0)", .{ptr}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"len\", baseType: !{d}, offset: 64)", .{int}),
        });
        const type_node = try self.node("distinct !DICompositeType(tag: DW_TAG_structure_type, name: \"string\", size: 128, elements: !{d})", .{members});
        self.string_type = type_node;
        return type_node;
    }

    /// A union or group value is the `%DoxaValue` box the runtime passes:
    /// `{ tag: DoxaTag, reserved, payload_bits, payload_len }`, exactly as
    /// `doxa_rt.zig` lays it out.
    fn boxType(self: *Debug) Error!u32 {
        if (self.box_type) |n| return n;
        const tag = try self.tagType();
        const u32t = try self.node("!DIBasicType(name: \"int\", size: 32, encoding: DW_ATE_unsigned)", .{});
        const int = try self.node("!DIBasicType(name: \"int\", size: 64, encoding: DW_ATE_signed)", .{});
        const members = try self.list(&.{
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"tag\", baseType: !{d}, offset: 0)", .{tag}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"reserved\", baseType: !{d}, offset: 32)", .{u32t}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"payload_bits\", baseType: !{d}, offset: 64)", .{int}),
            try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"payload_len\", baseType: !{d}, offset: 128)", .{int}),
        });
        const type_node = try self.node("distinct !DICompositeType(tag: DW_TAG_structure_type, name: \"value\", size: 192, elements: !{d})", .{members});
        self.box_type = type_node;
        return type_node;
    }

    /// `DoxaTag` (`doxa_rt.zig`): the kind of value a box holds, read from
    /// the runtime's own enum so the two cannot drift apart.
    fn tagType(self: *Debug) Error!u32 {
        if (self.tag_type) |n| return n;
        const tags = std.meta.fields(rt.DoxaTag);
        const elems = try self.alloc.alloc(u32, tags.len);
        inline for (tags, 0..) |tag, i| elems[i] = try self.node("!DIEnumerator(name: \"{s}\", value: {d})", .{ tag.name, tag.value });
        const underlying = try self.node("!DIBasicType(name: \"int\", size: 32, encoding: DW_ATE_unsigned)", .{});
        const type_node = try self.node("distinct !DICompositeType(tag: DW_TAG_enumeration_type, name: \"tag\", baseType: !{d}, size: 32, elements: !{d})", .{ underlying, try self.list(elems) });
        self.tag_type = type_node;
        return type_node;
    }

    /// A struct value's block: a word per field (a string takes two). Its
    /// members sit where `layout.zig` puts them, so a debugger reads them
    /// without knowing how Doxa words pack.
    fn structType(self: *Debug, id: ir.StructId) Error!u32 {
        if (self.structs.get(id)) |n| return n;
        const type_node = self.reserve();
        try self.structs.put(self.alloc, id, type_node);
        const fields = self.layout.struct_fields[id];
        const names = self.program.struct_field_names[id];
        const members = try self.alloc.alloc(u32, fields.len);
        for (fields, 0..) |t, i| {
            const member_type = try self.fieldType(t);
            const offset = layout_mod.fieldOffset(fields, @intCast(i)) * 64;
            const name = if (i < names.len) names[i] else "?";
            members[i] = try self.node("!DIDerivedType(tag: DW_TAG_member, name: \"{f}\", baseType: !{d}, offset: {d})", .{ escaped(name), member_type, offset });
        }
        const words = @max(1, layout_mod.structWords(fields));
        try self.define(type_node, "distinct !DICompositeType(tag: DW_TAG_structure_type, name: \"{f}\", size: {d}, elements: !{d})", .{ escaped(self.program.struct_names[id]), words * 64, try self.list(members) });
        return type_node;
    }

    /// A struct field's type: the word holds a pointer to a boxed union or
    /// group, not the box itself.
    fn fieldType(self: *Debug, t: HIRType) Error!u32 {
        return switch (t) {
            .Union, .Group => self.pointer(null),
            else => self.doxaType(t),
        };
    }

    /// The `!DIGlobalVariableExpression` of `g`, made on first use: what makes
    /// a debugger list the global, typed and at its declaration. A global is
    /// found by name and address from this; a debug record cannot name one
    /// (LLVM requires a local variable), so no access site carries one.
    pub fn global(self: *Debug, g: ir.Global) Error!u32 {
        const file_node = try self.file(g.span.module);
        const var_node = try self.node(
            "!DIGlobalVariable(name: \"{f}\", scope: !{d}, file: !{d}, line: {d}, type: !{d}, isLocal: false, isDefinition: true)",
            .{ escaped(g.display_name), file_node, file_node, g.span.line, try self.doxaType(g.ty) },
        );
        return self.node("!DIGlobalVariableExpression(var: !{d}, expr: !DIExpression())", .{var_node});
    }

    /// The compile unit, the module flags, and every node.
    pub fn write(self: *Debug, w: *std.Io.Writer) Error!void {
        const version = try self.node("!{{i32 2, !\"Debug Info Version\", i32 3}}", .{});
        const format = switch (self.options.format) {
            .dwarf => try self.node("!{{i32 7, !\"Dwarf Version\", i32 4}}", .{}),
            .codeview => try self.node("!{{i32 2, !\"CodeView\", i32 1}}", .{}),
        };
        try w.print("\n!llvm.dbg.cu = !{{!{d}}}\n!llvm.module.flags = !{{!{d}, !{d}}}\n", .{ self.unit, version, format });
        try w.writeAll(self.nodes.written());
    }
};

/// A metadata string's contents: LLVM spells a byte outside printable ASCII,
/// or a quote or backslash, as `\XX`.
fn escaped(bytes: []const u8) std.fmt.Alt([]const u8, writeEscaped) {
    return .{ .data = bytes };
}

fn writeEscaped(bytes: []const u8, w: *std.Io.Writer) std.Io.Writer.Error!void {
    for (bytes) |c| {
        if (c >= 0x20 and c < 0x7f and c != '"' and c != '\\') {
            try w.writeByte(c);
        } else {
            try w.print("\\{X:0>2}", .{c});
        }
    }
}
