const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

// The emitted module pins `"tune-cpu"="generic"` on every function. Clang's C
// frontend does this when only the CPU model is given (`-mcpu=native` selects
// features, not `-mtune`); without it the `.ll` backend path inherits the
// native tune model, whose loop unroller picks a pathological unroll for some
// serial loops (the `call` benchmark's `%997` carry chain). This is a tuning
// parity guard, not a correctness one: dropping the attribute silently
// re-introduces the confound.
const probe =
    \\module std from @std()
    \\function leaf(a :: int, b :: int) returns int {
    \\    return a + b
    \\}
    \\public entry function main() {
    \\    std.io.println("{leaf(1, 2)}")
    \\}
;

test "module: every function carries the tune-cpu attribute group" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try harness.emitIrFor(allocator, &tmp, probe, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "attributes #0 = { \"tune-cpu\"=\"generic\" }") != null);

    var defines: usize = 0;
    var lines = std.mem.splitScalar(u8, ir_text, '\n');
    while (lines.next()) |line| {
        if (!std.mem.startsWith(u8, line, "define ")) continue;
        defines += 1;
        try testing.expect(std.mem.endsWith(u8, line, " #0 {"));
    }
    try testing.expect(defines > 0);
}
