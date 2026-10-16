const std = @import("std");
const testing = std.testing;

const inline_zig = @import("../src/inline_zig/compiler.zig");
const doxa = @import("../src/main.zig");

fn baseInputs() inline_zig.ShimKeyInputs {
    return .{
        .toolchain = "0.16.0",
        .zig_path = "cache/JSON-abc.zig",
        .target_triple = "x86_64-windows.win11_dt-gnu",
        .cpu_key = "cpu-a",
        .zig_opt_flag = "-ODebug",
        .include_dirs = &.{},
    };
}

test "native cpu cache: a different CPU forces a different shim object" {
    // The regression: CI restores `.zig-cache` across runs whose
    // `windows-latest` runners are not CPU-homogeneous, and every native key
    // used to be the constant "native". A shim built for one runner was then
    // served to another that lacks its instructions, trapping on the first AVX
    // op in unrelated code. Two distinct CPUs must never share an object.
    const a = inline_zig.shimObjectKey(baseInputs());

    var other = baseInputs();
    other.cpu_key = "cpu-b";
    const b = inline_zig.shimObjectKey(other);

    try testing.expect(!std.mem.eql(u8, &a, &b));
}

test "native cpu cache: the same CPU reuses the shim object" {
    // The other half of the invariant: resolving a CPU must not defeat the
    // cache. Identical inputs must keep producing an identical key, or every
    // run would rebuild every shim.
    const a = inline_zig.shimObjectKey(baseInputs());
    const b = inline_zig.shimObjectKey(baseInputs());
    try testing.expect(std.mem.eql(u8, &a, &b));
}

test "native cpu cache: the CPU field is not the spawn arg" {
    // Guards the mistake this fix was made for: a key built from
    // "-mcpu=native" is identical on every host, which is exactly the
    // cross-machine collision above. If a future edit passes the spawn arg
    // through as the key, this catches it at the seam.
    var inputs = baseInputs();
    inputs.cpu_key = "-mcpu=native";
    const by_arg = inline_zig.shimObjectKey(inputs);

    inputs.cpu_key = "cpu-a";
    const by_identity = inline_zig.shimObjectKey(inputs);

    try testing.expect(!std.mem.eql(u8, &by_arg, &by_identity));
}

test "native cpu cache: the resolved host key is stable and not the constant" {
    // `resolveNativeCpuKey` runs host detection, so it cannot be asserted
    // against a fixed expected value — that would pin the test to the machine
    // running it. What must hold everywhere is that it is deterministic within
    // a process (so the cache still works) and is not the literal string
    // "native", which is the value that made every host share one key.
    const first = doxa.resolveNativeCpuKey(testing.io);
    const second = doxa.resolveNativeCpuKey(testing.io);

    try testing.expectEqual(first.digest, second.digest);
    try testing.expect(!std.mem.eql(u8, first.bytes(), "native"));

    // `bytes` must point into the key itself. A by-value receiver would return
    // a slice into a dead copy: the digest value stays the same, but the
    // caller reads whatever the stack holds by the time it hashes, so the key
    // varies run to run and every compile rebuilds the runtime object.
    try testing.expectEqual(@intFromPtr(&first.digest), @intFromPtr(first.bytes().ptr));
    try testing.expectEqual(first.digest.len, first.bytes().len);

    // The key must vary with the CPU it names, which is what makes a
    // cross-machine cache miss. Two different inputs are enough to show the
    // digest is derived rather than constant.
    var kb = @import("../src/utils/hashing.zig").KeyBuilder.init("doxa-native-cpu-v1");
    kb.addBytes("some-other-cpu");
    const other = kb.finish();
    try testing.expect(!std.mem.eql(u8, first.bytes(), &other));
}

test "native cpu cache: a cross build keys on no CPU" {
    // A cross build compiles for its target's baseline CPU, already fixed by
    // the triple and toolchain, so it contributes nothing. Two different
    // hosts therefore share one cross object — which is correct, since the
    // object does not depend on the host at all.
    var inputs = baseInputs();
    inputs.cpu_key = "";
    const first = inline_zig.shimObjectKey(inputs);
    const second = inline_zig.shimObjectKey(inputs);
    try testing.expect(std.mem.eql(u8, &first, &second));
}