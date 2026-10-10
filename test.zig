// The unit root: in-process tests only. Its result is cached on this
// executable, so nothing here may drive the installed `doxa` binary; those
// suites live under `test/suites.zig`. `build.zig`'s wiring check fails the
// build for any test file, or `src` file declaring a test, that no root
// reaches through a `test` block like this one.
test {
    _ = @import("test/test_inline_zig.zig");
    _ = @import("test/test_lazy_modules.zig");
    _ = @import("test/test_hashing.zig");
    _ = @import("test/test_lexer.zig");
    _ = @import("test/test_profiler.zig");
    _ = @import("test/test_artifact_cache.zig");
    _ = @import("test/test_floored_arith.zig");
    _ = @import("test/test_overflow.zig");
    _ = @import("test/test_register_hir.zig");
    _ = @import("test/test_stdlib_catalog.zig");
    _ = @import("test/test_module_graph.zig");
    _ = @import("src/lsp/server.zig");
    _ = @import("src/lsp/internal_methods.zig");
    _ = @import("src/analysis/consteval.zig");
    _ = @import("src/runtime/scope_arena.zig");
}
