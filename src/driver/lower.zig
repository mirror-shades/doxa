//! Lower the analyzed program to the register HIR and verify every function.

const std = @import("std");
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ConstantFolder = @import("../analysis/constant_folder.zig").ConstantFolder;
const Generator = @import("../codegen/hir/generator.zig").Generator;
const Profiler = @import("../utils/profiler.zig").Profiler;
const hir = @import("../codegen/hir/register/ir.zig");
const hir_verify = @import("../codegen/hir/register/verify.zig");
const hir_arenas = @import("../codegen/hir/register/arenas.zig");

/// Lowering first fixes the program's link identities, then the generator
/// binds every global site to its link name; only then are constants folded
/// in place — folding rewrites a reference's node, so no site may still point
/// into it — and each file gets its own folder, which is syntax-local.
pub fn lowerProgram(memoryManager: *MemoryManager, semantic_analyzer: *SemanticAnalyzer, reporter: *Reporter, profiler: *Profiler) !hir.Module {
    const allocator = memoryManager.getAnalysisAllocator();

    profiler.begin("hir-lower");
    defer profiler.end();
    try semantic_analyzer.finalizeLinkIdentities();
    var generator = try Generator.init(allocator, reporter, semantic_analyzer);

    profiler.begin("constant-fold");
    var faulted = false;
    for (semantic_analyzer.graph.records.items) |record| {
        if (record.kind != .doxa or !record.status.atLeast(.Analyzed)) continue;
        var constant_folder = ConstantFolder.init(allocator, reporter, &semantic_analyzer.type_cache);
        defer constant_folder.deinit();
        for (record.statements()) |*stmt| _ = try constant_folder.foldStmt(stmt);
        faulted = faulted or constant_folder.faulted;
    }
    profiler.end();
    // A faulting constant expression has been reported where it happened.
    if (faulted) return error.ConstantExpressionFault;

    var module = try generator.generate();
    profiler.begin("hir-arenas");
    try hir_arenas.run(allocator, &module);
    profiler.end();

    // The verifier runs in every build mode: a fault is a compiler bug, and
    // it quotes the block it rejected.
    profiler.begin("hir-verify");
    defer profiler.end();
    for (module.functions) |*f| {
        const fault = try hir_verify.verify(allocator, &module.program, f) orelse continue;
        var text = std.Io.Writer.Allocating.init(allocator);
        try hir_verify.writeFault(&text.writer, &module.program, f, fault);
        reporter.reportInternal("the register HIR failed verification. This is a compiler bug, not an error in the program\n{s}", .{text.written()}, @src());
        return error.InvalidHir;
    }
    return module;
}
