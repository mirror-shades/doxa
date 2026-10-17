//! Emit the program's LLVM IR and its optional side files: the register HIR
//! dump, the instrumented build's site map, static complexity findings and
//! the memory map. A side file this build does not write is removed, so a
//! reader never pairs a stale one with the new binary.

const std = @import("std");
const Build = @import("context.zig").Build;
const hir = @import("../codegen/hir/register/ir.zig");
const hir_print = @import("../codegen/hir/register/print.zig");
const llvm_emit = @import("../codegen/llvm/emit.zig");
const report = @import("../instrument/report.zig");
const complexity = @import("../instrument/complexity.zig");
const memory_report = @import("../instrument/memory.zig");
const paths = @import("paths.zig");

pub fn write(build: *const Build, module: *const hir.Module) !void {
    const io = build.io;
    const profiler = build.profiler;
    profiler.begin("emit-ir");
    defer profiler.end();

    const emit_alloc = build.memory_manager.getExecutionAllocator();
    if (build.config.emit_hir) {
        const hir_path = try std.fmt.allocPrint(emit_alloc, "{s}/{s}.hir", .{ build.cache_dir, build.stem });
        defer emit_alloc.free(hir_path);
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = hir_path, .data = try writeHirText(emit_alloc, module) });
    }

    const debug_options: ?llvm_emit.debug.Options = if (build.config.debug_info) .{
        .format = if (build.target.isWindows()) .codeview else .dwarf,
        .optimized = build.config.opt.level != .o0,
    } else null;
    var emitter = try llvm_emit.Emitter.init(emit_alloc, module, debug_options, build.config.instrument, build.target.isDarwin());
    var ll = std.Io.Writer.Allocating.init(emit_alloc);
    emitter.emit(&ll.writer) catch |err| {
        if (err == error.EmitFault) build.reporter.reportInternal("the emitter rejected the register HIR: {s}. This is a compiler bug, not an error in the program", .{emitter.fault.?}, @src());
        return err;
    };
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = build.ir_path, .data = ll.written() });

    // Side files go beside the binary, where the binary writes its counts: in
    // the cache for `doxa run`, the output folder for `doxa compile`.
    const map_path = try std.mem.concat(emit_alloc, u8, &.{ build.exe_stem, ".profmap" });
    defer emit_alloc.free(map_path);
    const data_path = try std.mem.concat(emit_alloc, u8, &.{ build.exe_stem, ".profdata" });
    defer emit_alloc.free(data_path);
    const findings_path = try std.mem.concat(emit_alloc, u8, &.{ build.exe_stem, ".complexity" });
    defer emit_alloc.free(findings_path);
    const memory_path = try std.mem.concat(emit_alloc, u8, &.{ build.exe_stem, ".memory" });
    defer emit_alloc.free(memory_path);

    // Counts are the run's: the previous build's never outlive it.
    try paths.removeIfExists(io, data_path);
    if (build.config.instrument.any()) {
        var map = std.Io.Writer.Allocating.init(emit_alloc);
        try report.writeProfmap(&map.writer, build.config.opt.clangFlag(), emitter.siteTable());
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = map_path, .data = map.written() });
    } else try paths.removeIfExists(io, map_path);
    if (build.config.emit_complexity) {
        var findings = std.Io.Writer.Allocating.init(emit_alloc);
        try complexity.write(emit_alloc, &findings.writer, module);
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = findings_path, .data = findings.written() });
    } else try paths.removeIfExists(io, findings_path);
    if (build.config.emit_memory) {
        var rows = std.Io.Writer.Allocating.init(emit_alloc);
        try memory_report.write(emit_alloc, &rows.writer, module);
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = memory_path, .data = rows.written() });
    } else try paths.removeIfExists(io, memory_path);
}

/// The textual register HIR of every function (`--emit-hir`).
fn writeHirText(allocator: std.mem.Allocator, module: *const hir.Module) ![]const u8 {
    var text = std.Io.Writer.Allocating.init(allocator);
    for (module.functions) |*f| {
        try hir_print.writeFunction(&text.writer, &module.program, f, .{ .spans = true });
        try text.writer.writeAll("\n");
    }
    return text.written();
}
