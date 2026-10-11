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
    /// Numbered metadata nodes, written after the functions.
    nodes: std.Io.Writer.Allocating,
    next: u32 = 0,
    unit: u32,
    /// The `!DIFile` of each module record, made on first use.
    files: std.AutoHashMapUnmanaged(ir.ModuleId, u32) = .empty,
    /// The subroutine type every subprogram shares until debug types land.
    /// TODO(step 3, slice 3.3): describe each function's real signature.
    subroutine: u32,

    pub fn init(alloc: std.mem.Allocator, program: *const ir.Program, entry: ir.ModuleId, options: Options) Error!Debug {
        var self = Debug{
            .alloc = alloc,
            .options = options,
            .program = program,
            .nodes = std.Io.Writer.Allocating.init(alloc),
            .unit = undefined,
            .subroutine = undefined,
        };
        const entry_file = try self.file(entry);
        self.unit = try self.node(
            "distinct !DICompileUnit(language: DW_LANG_C, file: !{d}, producer: \"doxa\", isOptimized: {s}, runtimeVersion: 0, emissionKind: FullDebug)",
            .{ entry_file, if (options.optimized) "true" else "false" },
        );
        self.subroutine = try self.node("!DISubroutineType(types: !{{}})", .{});
        return self;
    }

    fn node(self: *Debug, comptime fmt: []const u8, args: anytype) Error!u32 {
        const id = self.next;
        self.next += 1;
        try self.nodes.writer.print("!{d} = " ++ fmt ++ "\n", .{id} ++ args);
        return id;
    }

    /// A module record's file, named by its display path: never a path on
    /// this machine.
    fn file(self: *Debug, module: ir.ModuleId) Error!u32 {
        const entry = try self.files.getOrPut(self.alloc, module);
        if (!entry.found_existing) {
            entry.value_ptr.* = try self.node("!DIFile(filename: \"{f}\", directory: \"\")", .{escaped(self.program.files[module])});
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
                self.subroutine,
                f.span.line,
                if (self.options.optimized) " | DISPFlagOptimized" else "",
                self.unit,
            },
        );
    }

    /// The location of an instruction at `span` in `scope`. A statement's
    /// key instruction sits at the statement's own start.
    pub fn location(self: *Debug, scope: u32, f: *const ir.Function, span: ir.SrcSpan, key: bool) Error!u32 {
        if (key) {
            const start = f.statements[span.stmt];
            return self.node("!DILocation(line: {d}, column: {d}, scope: !{d}, atomGroup: {d}, atomRank: 1)", .{ start.line, start.column, scope, span.stmt + 1 });
        }
        return self.node("!DILocation(line: {d}, column: {d}, scope: !{d})", .{ span.line, span.column, scope });
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
