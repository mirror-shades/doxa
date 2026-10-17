//! The native backend: emit IR, compile the program and runtime objects,
//! compile inline-Zig shims, link, and write the binary's debug information.

const Build = @import("context.zig").Build;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const hir = @import("../codegen/hir/register/ir.zig");
const emit_ir = @import("emit_ir.zig");
const program = @import("program.zig");
const inline_zig = @import("inline_zig.zig");
const runtime = @import("runtime.zig");
const link = @import("link.zig");
const debug_info = @import("debug_info.zig");

pub fn compile(build: *Build, semantic_analyzer: *const SemanticAnalyzer, module: *const hir.Module) !void {
    try emit_ir.write(build, module);

    const obj_path = try program.compile(build);
    defer build.allocator.free(obj_path);

    try program.emitInspections(build);

    build.profiler.begin("inline-zig");
    const inline_zig_wrapper_paths = try inline_zig.compile(build, semantic_analyzer.graph);
    build.profiler.end();
    defer {
        for (inline_zig_wrapper_paths) |p| build.memory_manager.getAllocator().free(@constCast(p));
        build.memory_manager.getAllocator().free(inline_zig_wrapper_paths);
    }

    const runtime_obj_path = try runtime.compile(build);
    defer build.allocator.free(runtime_obj_path);

    try link.exec(build, obj_path, runtime_obj_path, inline_zig_wrapper_paths);
    try debug_info.write(build);
}
