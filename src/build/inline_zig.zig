//! Compile the inline-Zig shim objects for one build, matching the runtime
//! object's target, CPU and toolchain identity.

const Build = @import("context.zig").Build;
const module_graph = @import("../module/graph.zig");
const inline_zig_compiler = @import("../inline_zig/compiler.zig");

pub fn compile(build: *const Build, graph: *const module_graph.ModuleGraph) ![]const []const u8 {
    const allocator = build.allocator;
    // Match the runtime object and final link exactly: native builds also pass
    // an explicit host triple so glibc-version-dependent std declarations (e.g.
    // `arc4random_buf` at 2.36) agree across every object. Without this the
    // shim uses host detection while the link resolves Zig's default glibc, and
    // a shim that pulls `std.Io.Threaded.randomSecure` fails to link.
    const target_triple = try build.target.targetArgAlloc(allocator);
    defer allocator.free(target_triple);
    // An explicit `-target` stops auto-detecting the host CPU, so the native
    // shim must ask for it explicitly, exactly as the runtime object does.
    const cpu_arg: []const u8 = if (build.target.isCross()) "" else "-mcpu=native";
    return inline_zig_compiler.compileInlineZigObjects(
        build.io,
        build.memory_manager,
        graph,
        build.reporter,
        build.zig_exe_path,
        build.cache_dir,
        build.config.opt.zigFlag(),
        target_triple,
        build.target.os,
        cpu_arg,
        build.target.cpuKey(build.native_cpu_key.bytes()),
        build.include_dirs,
        build.toolchain,
        build.profiler,
    );
}
