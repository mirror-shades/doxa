const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

// B2/B3-lite (plan/performance-upgrades.md): a scalar-only struct that is never
// reflected and never crosses a container or function boundary is
// descriptor-free. Its construction skips `doxa_struct_register`, and a store
// that crosses a scope boundary clones with the typed scalar word copy rather
// than walking the runtime descriptor registry. Both directions are asserted
// here because `test/misc/descriptor_skip.doxa`'s struct is disqualified from
// the skip predicate (it is reflected *and* a dynamic-array element), so the
// shipped clone path was previously unexercised.

/// A local scalar struct re-homed out of a loop arena. The loop-fresh literal
/// is `Deep`, so `b is a` must clone into the function body — through the typed
/// path, since `Q` needs no descriptor.
const descriptorFreeSource =
    \\struct Q {
    \\    public x :: int,
    \\    public y :: int,
    \\}
    \\public entry function main() {
    \\    var b is $Q { x is 0, y is 0 }
    \\    for i while i < 3 do i++ {
    \\        const a is $Q { x is i, y is i + 1 }
    \\        b is a
    \\    }
    \\    @print("{b.x} {b.y}\n")
    \\}
;

test "descriptor skip: a local scalar struct clones with the typed word copy" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try harness.emitIrFor(allocator, &tmp, descriptorFreeSource, "--opt=0");
    defer allocator.free(ir_text);

    // The scope-crossing store is the typed clone, not the descriptor walk.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_clone_scalar_at") != null);
    // Construction never registers it, and no clone consults the registry.
    // Match the call form with its `(` so the `_at` variant is not confused for
    // the plain `doxa_struct_register` name.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_struct_register(") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_rehome_at") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_clone_at") == null);
}

/// The counter-case: a struct that reaches `@string` must keep its descriptor,
/// because reflection walks the registry.
const reflectedSource =
    \\struct R {
    \\    public x :: int,
    \\    public y :: int,
    \\}
    \\public entry function main() {
    \\    const r is $R { x is 7, y is 8 }
    \\    @print(@string(r) + "\n")
    \\}
;

test "descriptor skip: a reflected struct keeps its descriptor" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try harness.emitIrFor(allocator, &tmp, reflectedSource, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_struct_register(") != null);
}
