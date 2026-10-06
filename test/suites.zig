const std = @import("std");
const testing = std.testing;

const programs = @import("programs.zig");

// Everything that drives the installed `doxa` binary. `build.zig` runs this
// executable as a plain process, after the unit and LSP roots, rather than
// through the test runner's `--listen` protocol: its suites spawn children for
// most of their runtime, and a protocol root's result is cached on its own
// executable, which knows nothing of the binary under test. `process.zig` is
// the only spawner, and the wiring check keeps it out of the protocol roots.

test "run pipeline" {
    try programs.check(testing.allocator, .run);
}

test "compile pipeline" {
    try programs.check(testing.allocator, .compile);
}

test {
    _ = @import("test_build_script.zig");
    _ = @import("test_emit.zig");
    _ = @import("test_lsp_session.zig");
}
