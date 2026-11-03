//! IR-shape probes. Each compiles a small program through the installed
//! `doxa compile` and asserts on the IR the emitter chose. They drive the
//! binary, so they run with the program suites (`suites.zig`), never in a unit
//! root whose result is cached on its own executable.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const harness = @import("harness.zig");
const process = @import("process.zig");

/// Compiles `source` with `opt` and returns the emitted (unoptimized)
/// `<stem>.ll`. The unoptimized artifact is what shows the shape the emitter
/// chose rather than what LLVM made of it, so IR-shape probes assert on this.
/// The IR for `source`, without debug metadata (`--strip`): a shape probe
/// reads the code, and debug info is on by default. The debug-info tests
/// compile with `compileProbe` instead.
fn emitIrFor(
    allocator: std.mem.Allocator,
    tmp: *std.testing.TmpDir,
    source: []const u8,
    opt: []const u8,
) ![]u8 {
    return compileProbe(allocator, tmp, &.{.{ .path = "probe.doxa", .data = source }}, &.{ opt, "--strip" }, "ll");
}

const ProbeFile = struct {
    path: []const u8,
    data: []const u8,
};

/// Writes `files` into `tmp`, compiles its `probe.doxa` through the installed
/// `doxa compile` with `flags`, and returns the emitted `<stem>.<artifact>`
/// (`ll`, or `opt.ll` under `--emit-opt-ir`).
///
/// Every probe compiles into one shared cache, a `probe` `harness.Slot`. A probe
/// reads only its IR, but `doxa compile` still builds and links; sharing the
/// cache builds the program-independent runtime object once per optimizer
/// level rather than once per probe, which was nearly all of a probe's cost.
/// Persisting it across runs is sound (see `harness.Slot`), and the IR is
/// re-emitted on every compile, never served from the cache. The stem is named
/// for what the probe compiles, so distinct probes never share an IR file and a
/// rerun reuses its own program object instead of adding another.
fn compileProbe(
    allocator: std.mem.Allocator,
    tmp: *std.testing.TmpDir,
    files: []const ProbeFile,
    flags: []const []const u8,
    artifact: []const u8,
) ![]u8 {
    const io = testing.io;
    var hasher = std.hash.Wyhash.init(0);
    for (files) |file| {
        try tmp.dir.writeFile(io, .{ .sub_path = file.path, .data = file.data });
        hashFramed(&hasher, file.path);
        hashFramed(&hasher, file.data);
    }
    for (flags) |flag| hashFramed(&hasher, flag);
    var stem_buffer: [32]u8 = undefined;
    const stem = try std.fmt.bufPrint(&stem_buffer, "probe-{x:0>16}", .{hasher.final()});

    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);
    var cache = try std.Io.Dir.cwd().openDir(io, slot.path, .{});
    defer cache.close(io);
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];

    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);
    const args = try std.mem.concat(allocator, []const u8, &.{ &.{ "compile", "probe.doxa", "-o", stem, cache_flag }, flags });
    defer allocator.free(args);

    const capture = try doxa.run(allocator, io, .{ .args = args, .cwd = cwd });
    defer capture.deinit(allocator);
    if (!capture.succeeded()) {
        std.debug.print("doxa compile failed ({any}):\n{s}\n{s}\n{s}", .{ capture.end, capture.stdout, capture.stderr, capture.stuck });
        return error.ProbeDidNotCompile;
    }

    const ir_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ stem, artifact });
    defer allocator.free(ir_name);
    return cache.readFileAlloc(io, ir_name, allocator, .unlimited);
}

fn hashFramed(hasher: *std.hash.Wyhash, bytes: []const u8) void {
    hasher.update(std.mem.asBytes(&@as(u64, bytes.len)));
    hasher.update(bytes);
}

// ---------------------------------------------------------------------------
// Overflow checks: the policy reaches the emitted IR
// ---------------------------------------------------------------------------

/// An `int + int` whose operands arrive as parameters, so neither the sign nor
/// the magnitude is known and the check cannot be discharged.
const uncheckedAddSource =
    \\module std from @std()
    \\function unchecked(a :: int, b :: int) returns int {
    \\    return a + b
    \\}
    \\public entry function main() {
    \\    std.io.println("{unchecked(100, 200)}")
    \\}
;

test "emit: a checked build traps on signed overflow" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, uncheckedAddSource, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i64, i1 } @llvm.sadd.with.overflow.i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_fault(") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "ovf.trap.") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "ovf.cont.") != null);
}

test "emit: a fast build checks int and byte arithmetic" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // Overflow faults in every mode: `fast` keeps the int check and checks
    // the byte add and subtract as unsigned.
    const source =
        \\module std from @std()
        \\function sum(a :: int, b :: int) returns int {
        \\    return a + b
        \\}
        \\function byteSum(a :: byte, b :: byte) returns byte {
        \\    return a + b
        \\}
        \\function byteDiff(a :: byte, b :: byte) returns byte {
        \\    return a - b
        \\}
        \\public entry function main() {
        \\    std.io.println("{sum(100, 200)} {byteSum(0x01, 0x02)} {byteDiff(0x03, 0x02)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt-mode=fast");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i64, i1 } @llvm.sadd.with.overflow.i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i8, i1 } @llvm.uadd.with.overflow.i8") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i8, i1 } @llvm.usub.with.overflow.i8") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_fault(") != null);
}

test "emit: a range-proven byte add stays bare in a fast build" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // Both `% 16` results land in `[0, 15]`, so their sum fits a byte.
    const source =
        \\module std from @std()
        \\function bounded(a :: byte, b :: byte) returns byte {
        \\    return (a % 0x10) + (b % 0x10)
        \\}
        \\public entry function main() {
        \\    std.io.println("{bounded(0x64, 0xC8)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt-mode=fast");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i8") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "= add i8") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i8, i1 } @llvm.uadd") == null);
}

test "emit: wrapping operators lower bare and unchecked" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const source =
        \\module std from @std()
        \\function mix(h :: int, x :: int) returns int {
        \\    return (h +% x) *% 1099511628211
        \\}
        \\function step(a :: byte, b :: byte) returns byte {
        \\    return a -% b
        \\}
        \\public entry function main() {
        \\    std.io.println("{mix(1, 2)} {step(0x01, 0x02)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "= add i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "= mul i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "= sub i8") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i64, i1 } @llvm.s") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i8, i1 } @llvm.usub") == null);
}

test "emit: a loop-carried accumulator through a call reaches the bare urem" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // `sum` is loop-carried and grows by `add(i, ...) >= 1`; `add` is a real
    // call. Without both the loop fixpoint and the callee's return range the
    // dividend's sign is unknown and `% 997` keeps its correction. With them,
    // floored `%` by a positive constant over a non-negative dividend is a
    // single `urem`.
    const source =
        \\module std from @std()
        \\function add(a :: int, b :: int) returns int {
        \\    return a + b
        \\}
        \\function accumulate(iters :: int) returns int {
        \\    var sum is 0
        \\    for i while i < iters do i += 1 {
        \\        const a is add(i, (sum % 997) + 1)
        \\        sum += a
        \\    }
        \\    return sum
        \\}
        \\public entry function main() {
        \\    std.io.println("{accumulate(10)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i64") != null);
    // The correction shape is `srem` + sign test; neither may survive. The
    // loop's own bound check is `icmp slt`, so it is not a valid witness.
    try testing.expect(std.mem.indexOf(u8, ir_text, "srem i64") == null);
}

test "emit: bounded operands discharge the check even in a checked build" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // Both `% 16` results land in `[0, 15]`, so their sum is `[0, 30]` and fits
    // without a check. The fixups below are the only integer arithmetic in the
    // program, so a single surviving `with.overflow` would be the add.
    const source =
        \\module std from @std()
        \\function bounded(a :: int, b :: int) returns int {
        \\    return (a % 16) + (b % 16)
        \\}
        \\public entry function main() {
        \\    std.io.println("{bounded(100, 200)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    // The probe reached codegen: the modulo shape is there.
    const body = functionBody(ir_text, "bounded") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, body, "urem i64") != null);
    // But the add's range proved it safe, so no check was emitted. Match the
    // *call* form: the module still declares the intrinsic it may have used.
    // Only `bounded` is judged: the standard library it links has checks of
    // its own (`@byte` of an input byte).
    try testing.expect(std.mem.indexOf(u8, body, "call { i64, i1 } @llvm.s") == null);
    try testing.expect(std.mem.indexOf(u8, body, "call void @doxa_fault(") == null);
}

// ---------------------------------------------------------------------------
// Floored arithmetic: the emitter reaches the shape the plan chose
// ---------------------------------------------------------------------------

// The dividend must be something the generator cannot constant-fold. Two
// literals are folded away before codegen ever sees them, so each probe below
// hides the dividend inside an arithmetic expression over a loop induction
// variable: the operation survives into the HIR with the divisor still
// arriving as a `Const` instruction, which is exactly the operand pair
// `planModulo` reads.

/// A program whose dividend is a loop induction variable, so the expression
/// reaches codegen while the divisor stays a compile-time constant.
fn loopSource(allocator: std.mem.Allocator, expr: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator,
        \\module std from @std()
        \\public entry function main() {{
        \\    var total is 0
        \\    for i while i < 10 do i += 1 {{
        \\        total is total + ({s})
        \\    }}
        \\    std.io.println("{{total}}")
        \\}}
    , .{expr});
}

/// Counts the floored-arithmetic fixups in some emitted IR.
///
/// The correction always materialises as a `select i1 ..., i64 ..., i64 0` —
/// picking the divisor or a zero — so that line is the signature. Matching on
/// it rather than on `icmp slt` avoids catching a loop's own bound check,
/// which has the same opcode with a register second operand.
fn correctionCount(ir_text: []const u8) usize {
    var count: usize = 0;
    var lines = std.mem.splitScalar(u8, ir_text, '\n');
    while (lines.next()) |line| {
        if (std.mem.indexOf(u8, line, "select i1 ") != null and
            std.mem.indexOf(u8, line, ", i64 0") != null)
        {
            count += 1;
        }
    }
    return count;
}

test "emit: a power-of-two constant divisor lowers modulo to a single urem" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // 65536 divides 2^64, so the bit-pattern remainder *is* the magnitude
    // remainder and the shape needs no fact at all about the dividend — which
    // is why it also covers the loop induction variable here, whose range this
    // walk deliberately refuses to derive.
    const source = try loopSource(allocator, "i % 65536");
    defer allocator.free(source);
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i64") != null);
    // No sign correction may survive, and the truncating remainder must be gone
    // entirely.
    try testing.expectEqual(@as(usize, 0), correctionCount(ir_text));
    try testing.expect(std.mem.indexOf(u8, ir_text, "srem") == null);
}

test "emit: a general constant divisor keeps the remainder but drops the xor" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // 997 does *not* divide 2^64, so `urem` would be wrong for a dividend of
    // unknown sign and the signed remainder has to stay. What the constant
    // still buys is the settled divisor sign: the sign-difference `xor`
    // disappears, leaving the dividend's own sign test.
    // The dividend's sign is unknown: `i - 5` ranges below zero.
    const source = try loopSource(allocator, "(i - 5) % 997");
    defer allocator.free(source);
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "srem i64") != null);
    try testing.expectEqual(@as(usize, 1), correctionCount(ir_text));
    try testing.expect(std.mem.indexOf(u8, ir_text, "icmp slt i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "xor i64") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "urem") == null);
}

test "emit: a power-of-two constant divisor lowers floored division to a shift" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const source = try loopSource(allocator, "i // 1024");
    defer allocator.free(source);
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "ashr i64") != null);
    try testing.expectEqual(@as(usize, 0), correctionCount(ir_text));
    try testing.expect(std.mem.indexOf(u8, ir_text, "sdiv") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "srem") == null);
}

test "emit: an unconstrained divisor keeps the sign correction" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // The divisor arrives as a parameter, so nothing is known about either
    // operand and the general shape must survive — including the
    // sign-difference `xor`, which a non-negative dividend alone could not
    // replace.
    const source =
        \\module std from @std()
        \\function floor_mod(a :: int, b :: int) returns int {
        \\    return a % b
        \\}
        \\public entry function main() {
        \\    std.io.println("{floor_mod(12345, 997)}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "srem i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "xor i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "icmp slt i64") != null);
}

test "emit: a constant reached through a binding is still a constant" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // Here the divisor is named by a `const` rather than written as a literal,
    // so the range reaches the operation by variable reference instead. A
    // single-assignment binding records the initializer's exact value, so the
    // cheap shape must still be selected.
    const source =
        \\module std from @std()
        \\public entry function main() {
        \\    const d is 65536
        \\    var total is 0
        \\    for i while i < 10 do i += 1 {
        \\        const m is i * 3 + 1
        \\        total is total + (m % d)
        \\    }
        \\    std.io.println("{total}")
        \\}
    ;
    const ir_text = try emitIrFor(allocator, &tmp, source, "--opt=0");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i64") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "srem") == null);
}

// ---------------------------------------------------------------------------
// Descriptor skip
// ---------------------------------------------------------------------------

// B2/B3-lite (plan/performance-upgrades.md): a scalar-only struct that is never
// reflected and never crosses a container or function boundary is
// descriptor-free. It is allocated as its bare words, with no header naming a
// descriptor (`doxa_struct_new`), and a store that crosses a scope boundary
// clones with the typed scalar word copy rather than walking a descriptor. Both directions are asserted
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
    \\    for i while i < 3 do i += 1 {
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

    const ir_text = try emitIrFor(allocator, &tmp, descriptorFreeSource, "--opt=0");
    defer allocator.free(ir_text);

    // The scope-crossing store is the typed clone, decided statically, not
    // the descriptor walk or a runtime rehome.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_clone_scalar(") != null);
    // Construction gives it no header, and no copy reads one.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_new(") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_rehome") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_clone(") == null);
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

    const ir_text = try emitIrFor(allocator, &tmp, reflectedSource, "--opt=0");
    defer allocator.free(ir_text);

    // It is allocated behind its header, which names the descriptor.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_new(") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "doxa_struct_register") == null);
}

// ---------------------------------------------------------------------------
// Target tuning
// ---------------------------------------------------------------------------

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

// Scope elision judges what a function allocates, not what it handles: a
// function that only reads a heap parameter it was handed drops its scope
// arena, while one that mutates the parameter (and so snapshots it into its
// own arena on entry) keeps it.
const heapReaderSource =
    \\struct Board {
    \\    public cells :: int[],
    \\}
    \\function spaceAt(b :: Board, i :: int) returns int {
    \\    return b.cells[i]
    \\}
    \\function isFlankedBy(b :: Board, i :: int) returns int {
    \\    return spaceAt(b, i - 1) + spaceAt(b, i + 1)
    \\}
    \\function marked(b :: Board, i :: int) returns int {
    \\    b.cells[i] is 9
    \\    return b.cells[i]
    \\}
    \\public entry function main() {
    \\    const b is $Board { cells is [1, 2, 3] }
    \\    @print("{isFlankedBy(b, 1)} {marked(b, 0)}\n")
    \\}
;

/// The body of the function whose link name ends in `$<name>`.
fn functionBody(ir_text: []const u8, name: []const u8) ?[]const u8 {
    var lines = std.mem.splitScalar(u8, ir_text, '\n');
    while (lines.next()) |line| {
        if (!std.mem.startsWith(u8, line, "define ")) continue;
        const open = std.mem.indexOfScalar(u8, line, '(') orelse continue;
        const head = line[0..open];
        if (!std.mem.endsWith(u8, head, name) or head.len <= name.len or head[head.len - name.len - 1] != '$') continue;
        const start = lines.index orelse return null;
        const end = std.mem.indexOfPos(u8, ir_text, start, "\n}\n") orelse return null;
        return ir_text[start..end];
    }
    return null;
}

test "scope elision: a function that only reads a heap parameter drops its scope" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, heapReaderSource, "--opt=0");
    defer allocator.free(ir_text);

    for ([_][]const u8{ "spaceAt", "isFlankedBy" }) |name| {
        const body = functionBody(ir_text, name) orelse return error.FunctionNotEmitted;
        try testing.expect(std.mem.indexOf(u8, body, "@doxa_scope_enter") == null);
    }
    const mutator = functionBody(ir_text, "marked") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, mutator, "@doxa_scope_enter") != null);
}

// A method takes its receiver as a value, so lending one allocates nothing:
// a read-only method, a function calling it on a parameter, and a method
// calling it on a field all drop their scopes. A field store places its value
// in the receiver's own arena, so it needs no scope either; a method that
// builds a value of its own keeps its scope.
const methodReaderSource =
    \\struct Board {
    \\    public cells :: int[],
    \\    public label :: string,
    \\    public method spaceAt(i :: int) returns int {
    \\        return this.cells[i]
    \\    }
    \\    public method rename(name :: string) returns int {
    \\        this.label is name
    \\        return 0
    \\    }
    \\    public method shout() returns int {
    \\        const loud is this.label + "!"
    \\        return @length(loud)
    \\    }
    \\}
    \\struct Game {
    \\    public board :: Board,
    \\    public method evaluate() returns int {
    \\        return this.board.spaceAt(0) + this.board.spaceAt(1)
    \\    }
    \\    public method relabel() returns int {
    \\        return this.board.rename("b")
    \\    }
    \\}
    \\function isFlankedBy(b :: Board, i :: int) returns tetra {
    \\    return b.spaceAt(i - 1) == b.spaceAt(i + 1)
    \\}
    \\public entry function main() {
    \\    var g is $Game { board is $Board { cells is [1, 2, 1], label is "a" } }
    \\    @print("{isFlankedBy(g.board, 1)} {g.evaluate()} {g.relabel()} {g.board.shout()}\\n")
    \\}
;

test "scope elision: a method and every caller lending it a receiver drop their scopes unless they allocate" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, methodReaderSource, "--opt=0");
    defer allocator.free(ir_text);

    for ([_][]const u8{ "spaceAt", "isFlankedBy", "evaluate", "rename", "relabel" }) |name| {
        const body = functionBody(ir_text, name) orelse return error.FunctionNotEmitted;
        try testing.expect(std.mem.indexOf(u8, body, "@doxa_scope_enter") == null);
    }
    const builder = functionBody(ir_text, "shout") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, builder, "@doxa_scope_enter") != null);
}

// A by-value heap parameter is copied on entry only when analysis records a
// write through it (`analysis/semantic/effects.zig`): a read-only body binds
// the caller's value, one that writes through it, directly or by calling a
// method that writes its receiver through a helper, owns a copy.
const paramWriteSource =
    \\struct C {
    \\    public n :: int,
    \\    public method get() returns int { return this.n }
    \\    public method bump() { this.n += 1 }
    \\    public method bumpTwice() {
    \\        this.bump()
    \\        this.bump()
    \\    }
    \\}
    \\function reads(c :: C, a :: int[]) returns int {
    \\    return c.get() + a[0] + @length(a)
    \\}
    \\function writesThroughHelper(c :: C) returns int {
    \\    c.bumpTwice()
    \\    return c.n
    \\}
    \\function pushes(a :: int[]) returns int {
    \\    @push(a, 1)
    \\    return @length(a)
    \\}
    \\public entry function main() {
    \\    const c is $C { n is 1 }
    \\    const a is [1, 2]
    \\    @print("{reads(c, a)} {writesThroughHelper(c)} {pushes(a)}\n")
    \\}
;

test "param effects: a read-only heap parameter is not copied, a written one is" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, paramWriteSource, "--opt=0");
    defer allocator.free(ir_text);

    const reader = functionBody(ir_text, "reads") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, reader, "_clone") == null);
    const helper = functionBody(ir_text, "writesThroughHelper") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, helper, "@doxa_struct_clone") != null);
    const pusher = functionBody(ir_text, "pushes") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, pusher, "@doxa_array_clone") != null);
}

// Lending a by-value parameter as `^` writes it only if the callee writes
// through its `^` parameter: one that reads it, or rebinds it (replacing the
// lender's slot, not its array), leaves the parameter uncopied.
const paramLendSource =
    \\function peek(^x :: int[]) returns int { return x[0] + @length(x) }
    \\function rebinds(^x :: int[]) { x is [7] }
    \\function writes(^x :: int[]) { x[0] is 7 }
    \\function relays(^x :: int[]) { writes(^x) }
    \\function lendsToReader(p :: int[]) returns int { return peek(^p) }
    \\function lendsToRebinder(p :: int[]) returns int {
    \\    rebinds(^p)
    \\    return p[0]
    \\}
    \\function lendsToRelay(p :: int[]) returns int {
    \\    relays(^p)
    \\    return p[0]
    \\}
    \\public entry function main() {
    \\    const a is [1, 2, 3]
    \\    @print("{lendsToReader(a)} {lendsToRebinder(a)} {lendsToRelay(a)}\n")
    \\}
;

test "param effects: a parameter lent as ^ is copied only if the callee writes through it" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, paramLendSource, "--opt=0");
    defer allocator.free(ir_text);

    for ([_][]const u8{ "lendsToReader", "lendsToRebinder" }) |name| {
        const body = functionBody(ir_text, name) orelse return error.FunctionNotEmitted;
        try testing.expect(std.mem.indexOf(u8, body, "_clone") == null);
    }
    const relay = functionBody(ir_text, "lendsToRelay") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, relay, "@doxa_array_clone") != null);
}

// The element of `each n in ^path` lent on as `^` shares the element (B057):
// the call's temporary lives in the array's arena, so the write-back after
// the call keeps the element as it is, with no copy per call.
const eachLendSource =
    \\struct Item { public count :: int, public names :: string[] }
    \\function bump(^it :: Item) { it.count += 1 }
    \\function bumpAll(n :: int) returns int {
    \\    var items :: Item[] is [$Item { count is n, names is ["a"] }]
    \\    each it in ^items { bump(^it) }
    \\    return items[0].count
    \\}
    \\public entry function main() {
    \\    @print("{bumpAll(1)}\n")
    \\}
;

test "each over ^path: lending the element on copies nothing" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, eachLendSource, "--opt=0");
    defer allocator.free(ir_text);

    const body = functionBody(ir_text, "bumpAll") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, body, "_clone") == null);
    try testing.expect(std.mem.indexOf(u8, body, "rehome") == null);
}

test "module: every function carries the tune-cpu attribute group" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, probe, "--opt=0");
    defer allocator.free(ir_text);

    const plain_group = "attributes #0 = { \"tune-cpu\"=\"generic\" }";
    const darwin_group = "attributes #0 = { \"tune-cpu\"=\"generic\" \"frame-pointer\"=\"non-leaf\" }";
    try expectContains(ir_text, if (builtin.os.tag.isDarwin()) darwin_group else plain_group);

    // Darwin's ABI requires a frame record in every function: unwinders,
    // profilers and the runtime's backtrace walk the frame-pointer chain.
    // Elsewhere the code is left as it was.
    const darwin = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = probe }}, &.{ "--opt=0", "--strip", "--arch=aarch64", "--os=macos" }, "ll");
    defer allocator.free(darwin);
    try expectContains(darwin, darwin_group);
    const linux = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = probe }}, &.{ "--opt=0", "--strip", "--arch=x86_64", "--os=linux" }, "ll");
    defer allocator.free(linux);
    try expectContains(linux, plain_group);

    var defines: usize = 0;
    var lines = std.mem.splitScalar(u8, ir_text, '\n');
    while (lines.next()) |line| {
        if (!std.mem.startsWith(u8, line, "define ")) continue;
        defines += 1;
        try testing.expect(std.mem.endsWith(u8, line, " #0 {"));
    }
    try testing.expect(defines > 0);
}

// ---------------------------------------------------------------------------
// Module graph: emitted IR names no checkout
// ---------------------------------------------------------------------------

const checkout_program =
    \\module u from "./util.doxa"
    \\const v is u.helper()
    \\@print("{v}\n")
    \\
;

const checkout_util =
    \\public function helper() returns int {
    \\    return 42
    \\}
    \\
;

/// Compile the same two-file program inside `tmp` and return the optimized IR
/// from the cache. The program imports a sibling module so the emitter has a
/// real module reference to name.
fn emittedIr(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir) ![]u8 {
    return compileProbe(allocator, tmp, &.{
        .{ .path = "probe.doxa", .data = checkout_program },
        .{ .path = "util.doxa", .data = checkout_util },
    }, &.{"--emit-opt-ir"}, "opt.ll");
}

test "module graph: emitted IR is independent of the checkout directory" {
    const allocator = testing.allocator;

    var tmp_a = testing.tmpDir(.{});
    defer tmp_a.cleanup();
    var tmp_b = testing.tmpDir(.{});
    defer tmp_b.cleanup();

    // Two temp checkouts at different absolute paths emit byte-identical IR.
    const ir_a = try emittedIr(allocator, &tmp_a);
    defer allocator.free(ir_a);
    const ir_b = try emittedIr(allocator, &tmp_b);
    defer allocator.free(ir_b);

    try testing.expectEqualStrings(ir_a, ir_b);
}

// ---------------------------------------------------------------------------
// Spans: every instruction names the source it was lowered from
// ---------------------------------------------------------------------------

const spanSource =
    \\function scale(xs :: int[], k :: int) returns int {
    \\    var total is 0
    \\    each x in xs {
    \\        if x > k then {
    \\            total is total + x * k
    \\        }
    \\    }
    \\    return total + xs[0]
    \\}
    \\entry function main() {
    \\    @print("{scale([1, 2, 3], 2)}")
    \\}
;

/// The span `--emit-hir` prints for the first line holding `needle`.
fn spanOf(hir: []const u8, needle: []const u8) ![]const u8 {
    var lines = std.mem.splitScalar(u8, hir, '\n');
    while (lines.next()) |line| {
        if (std.mem.indexOf(u8, line, needle) == null) continue;
        const at = std.mem.indexOf(u8, line, "  ; ") orelse return error.NoSpan;
        return std.mem.trimEnd(u8, line[at + "  ; ".len ..], "\r");
    }
    std.debug.print("no HIR line holds `{s}`:\n{s}\n", .{ needle, hir });
    return error.NoSuchLine;
}

test "spans: an instruction carries its expression's start, a statement's first is marked" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const hir = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = spanSource }}, &.{"--emit-hir"}, "hir");
    defer allocator.free(hir);

    const expected = [_]struct { needle: []const u8, span: []const u8 }{
        // A binary expression starts at its left operand, not its operator or
        // the token after it.
        .{ .needle = "cmp.Gt", .span = "probe.doxa:4:12 (stmt)" },
        .{ .needle = "arith.Mul", .span = "probe.doxa:5:30 (stmt)" },
        .{ .needle = "%16 = arith.Add", .span = "probe.doxa:5:22" },
        // An index starts at what it indexes; a terminator has its statement.
        .{ .needle = "%13 = array.get", .span = "probe.doxa:8:20" },
        .{ .needle = "return %14", .span = "probe.doxa:8:5" },
        // An `each` loop's machinery sits at what it iterates.
        .{ .needle = "array.len", .span = "probe.doxa:3:15" },
        // A placeholder's expressions are where their text is in the file.
        .{ .needle = "array.new", .span = "probe.doxa:11:20" },
        .{ .needle = "call ", .span = "probe.doxa:11:14" },
    };
    for (expected) |e| {
        testing.expectEqualStrings(e.span, try spanOf(hir, e.needle)) catch |err| {
            std.debug.print("for `{s}`\n", .{e.needle});
            return err;
        };
    }
}

// ── Debug info (on unless `--strip`) ──

test "debug info: subprograms, files and statement-start key locations" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = spanSource }}, &.{}, "ll");
    defer allocator.free(ll);

    // One subprogram per function, named as the program spells it, linked
    // by its mangled name, stepping by key instructions.
    try expectContains(ll, "!DISubprogram(name: \"scale\", linkageName: \"__doxa_m1_");
    try expectContains(ll, "!DISubprogram(name: \"main\", linkageName: \"__doxa_m1_");
    try expectContains(ll, "keyInstructions: true");
    // Files by display path, never a path on this machine. Under DWARF the
    // directory is `.`: an empty one drops the compile unit's
    // `DW_AT_comp_dir`, and the runtime's DWARF reader then finds no line
    // table. CodeView keeps it empty, as lld-link would root a `.` here.
    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(testing.io, &cwd_buffer)];
    try expectContains(ll, if (builtin.os.tag == .windows)
        "!DIFile(filename: \"probe.doxa\", directory: \"\")"
    else
        "!DIFile(filename: \"probe.doxa\", directory: \".\")");
    try testing.expect(std.mem.indexOf(u8, ll, cwd) == null);
    const elf_ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = spanSource }}, &.{ "--arch=x86_64", "--os=linux" }, "ll");
    defer allocator.free(elf_ll);
    try expectContains(elf_ll, "!DIFile(filename: \"probe.doxa\", directory: \".\")");
    try testing.expect(std.mem.indexOf(u8, elf_ll, cwd) == null);
    // `if x > k` begins at its `if` (4:9), though its first instruction is
    // the comparison at 4:12: the key location is the statement's own start.
    try expectContains(ll, "!DILocation(line: 4, column: 9, scope: ");
    try testing.expect(std.mem.indexOf(u8, keyLocation(ll, 4, 9) orelse "", "atomRank: 1") != null);
    // Other instructions keep their expression's position and no atom.
    try expectContains(ll, "!DILocation(line: 5, column: 22, scope: ");
    try testing.expect(std.mem.indexOf(u8, keyLocation(ll, 5, 22) orelse "", "atom") == null);
}

test "debug info: under --strip the IR carries no metadata" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ll = try emitIrFor(allocator, &tmp, spanSource, "--opt=0");
    defer allocator.free(ll);
    try testing.expect(std.mem.indexOf(u8, ll, "!dbg") == null);
    try testing.expect(std.mem.indexOf(u8, ll, "!llvm.dbg.cu") == null);
}

test "debug info: the line table names the .doxa file and its statement lines" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // DWARF, read from the assembly the object is made of: the line program
    // names `probe.doxa`, and the `if` statement is a step stop at its start.
    const asm_text = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = spanSource }}, &.{ "--emit-asm", "--arch=x86_64", "--os=linux" }, "s");
    defer allocator.free(asm_text);
    try expectContains(asm_text, "\"probe.doxa\"");
    try expectContains(asm_text, " 4 9");

    // CodeView, read from the PDB a Windows link writes beside the binary.
    const pdb = try compileProbeOutput(allocator, &tmp, spanSource, &.{}, "pdb");
    defer allocator.free(pdb);
    try expectContains(pdb, "probe.doxa");
    try expectContains(pdb, "scale");
}

// ── Container stores: placed in the container's arena ──

const placementSource =
    \\struct Box {
    \\    public items :: int[],
    \\}
    \\
    \\function fill(^b :: Box, n :: int) {
    \\    const fresh is [n, n]
    \\    b.items is fresh
    \\}
    \\
    \\entry function main() {
    \\    var b is $Box { items is [0] }
    \\    fill(^b, 5)
    \\    var local is [[1]]
    \\    @push(local, [2])
    \\    @print("{b.items} {local}\n")
    \\}
;

test "placement: a value stored into a container is placed in the container's arena" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const hir = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = placementSource }}, &.{"--emit-hir"}, "hir");
    defer allocator.free(hir);

    // A caller's struct names its arena at runtime: the held copy goes there,
    // not into `fill`'s own arena, which closes before the caller reads it.
    try expectContains(hir, "= arena_of ");
    const fill_at = std.mem.indexOf(u8, hir, "fn __doxa_m1_").?;
    const main_at = std.mem.indexOfPos(u8, hir, fill_at + 1, "fn __doxa_m1_").?;
    try testing.expect(std.mem.indexOf(u8, hir[0..main_at], "= arena_of ") != null);
    // An array allocated here names its arena statically: no `arena_of`
    // survives in `main`, and the fresh literal pushed needs no copy.
    try testing.expect(std.mem.indexOf(u8, hir[main_at..], "arena_of") == null);
    try testing.expect(std.mem.indexOf(u8, hir[main_at..], "rehome") == null);
}

// ── Debug info: locals ──

const localsSource =
    \\var counter is 0
    \\
    \\struct Point {
    \\    public x :: int
    \\    public y :: int
    \\
    \\    public method norm() returns int {
    \\        var sum is this.x * this.x + this.y * this.y
    \\        return sum
    \\    }
    \\}
    \\
    \\function scale(xs :: int[], k :: int, ^out :: int) returns int {
    \\    var total is 0
    \\    each x in xs {
    \\        if x > k then {
    \\            total is total + x * k
    \\        }
    \\    }
    \\    out is total
    \\    return total
    \\}
    \\
    \\entry function main() {
    \\    var n is 2
    \\    var p is $Point { x is 3, y is 4 }
    \\    var bumps is [1, 2]
    \\    each b in ^bumps {
    \\        b is b + n
    \\    }
    \\    counter is n + bumps[0]
    \\    @print("{scale([1, 2, 3], 2, ^n) + p.norm() + counter}")
    \\}
;

test "debug info: every named binding is a local with a type" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = localsSource }}, &.{}, "ll");
    defer allocator.free(ll);

    // A method's receiver, a `^` parameter, an SSA local and an `each` item.
    try expectContains(ll, "!DILocalVariable(name: \"this\", ");
    try expectContains(ll, "!DILocalVariable(name: \"out\", ");
    try expectContains(ll, "!DILocalVariable(name: \"total\", ");
    try expectContains(ll, "!DILocalVariable(name: \"x\", ");
    // The element of `each b in ^bumps`, a place with no storage of its own.
    try expectContains(ll, "!DILocalVariable(name: \"b\", ");
    // Every local names a type; the struct is described by its fields.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_structure_type, name: \"Point\", size: 128, elements: ");
    try expectContains(ll, "!DIDerivedType(tag: DW_TAG_member, name: \"x\", baseType: ");
    // A binding's definition names its value.
    try expectContains(ll, "call void @llvm.dbg.value(metadata ");
    // A local lent as `^` is declared on its storage.
    try expectContains(ll, "call void @llvm.dbg.declare(metadata ptr ");
    // A module-level global is described where it is defined.
    try expectContains(ll, "!DIGlobalVariable(name: \"counter\", ");
    try expectContains(ll, "!DIGlobalVariableExpression(var: ");
    // No compiler temporary leaks in: the `each` index is not a name.
    try testing.expect(std.mem.indexOf(u8, ll, "__idx") == null);
}

const paramCopySource =
    \\function count(x :: int, limit :: int) returns int {
    \\    var current is x
    \\    while current <= limit {
    \\        current += 1
    \\    }
    \\    return current
    \\}
    \\
    \\entry function main() {
    \\    @print("{count(2, 5)}\n")
    \\}
;

test "debug info: a local holding a parameter's value is not that parameter" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = paramCopySource }}, &.{}, "ll");
    defer allocator.free(ll);

    // `current` starts as `x`'s value but is a local: two variables claiming
    // argument 1 make LLVM drop the whole module's debug info.
    try expectContains(ll, "!DILocalVariable(name: \"x\", ");
    const current = std.mem.indexOf(u8, ll, "!DILocalVariable(name: \"current\", ") orelse return error.TestExpectedContains;
    const line_end = std.mem.indexOfScalarPos(u8, ll, current, '\n').?;
    try testing.expect(std.mem.indexOf(u8, ll[current..line_end], "arg:") == null);
}

test "debug info: a Linux binary ships without its debug information, which a .gnu_debuglink names" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const binary = try compileProbeLinux(allocator, &tmp, localsSource, &.{}, "");
    defer allocator.free(binary);
    const debug = try firstOutput(allocator, &tmp, ".debug");
    defer allocator.free(debug);

    // The DWARF is in the debug file, not the binary.
    try testing.expect(try hasSection(debug, ".debug_info"));
    try testing.expect(!try hasSection(binary, ".debug_info"));
    try testing.expect(!try hasSection(binary, ".debug_line"));
    try testing.expect(try hasSection(binary, ".gnu_debuglink"));
    try testing.expect(try hasSection(binary, ".eh_frame"));
    try testing.expect(binary.len < debug.len);
    // `.gnu_debuglink` names the file beside the binary, then holds its
    // CRC-32, four-byte aligned after the name.
    const link = section(binary, ".gnu_debuglink").?;
    const name = link[0..std.mem.indexOfScalar(u8, link, 0).?];
    try testing.expect(std.mem.startsWith(u8, name, "probe-") and std.mem.endsWith(u8, name, ".debug"));
    try testing.expect(std.mem.indexOfScalar(u8, name, '/') == null);
    const crc_at = std.mem.alignForward(usize, name.len + 1, 4);
    try testing.expectEqual(std.hash.crc.Crc32.hash(debug), std.mem.readInt(u32, link[crc_at..][0..4], .little));

    // Both carry the same build ID, a content hash: a second build of the
    // same program in another directory gets the same one.
    const id = buildId(binary).?;
    try testing.expectEqual(20, id.len);
    try testing.expectEqualSlices(u8, id, buildId(debug).?);
    var again_tmp = testing.tmpDir(.{});
    defer again_tmp.cleanup();
    const again = try compileProbeLinux(allocator, &again_tmp, localsSource, &.{}, "");
    defer allocator.free(again);
    try testing.expectEqualSlices(u8, id, buildId(again).?);

    // `--strip` leaves no debug information anywhere, and no `.debug` file.
    var stripped_tmp = testing.tmpDir(.{});
    defer stripped_tmp.cleanup();
    const stripped = try compileProbeLinux(allocator, &stripped_tmp, localsSource, &.{"--strip"}, "");
    defer allocator.free(stripped);
    try testing.expect(!try hasSection(stripped, ".debug_info"));
    try testing.expect(!try hasSection(stripped, ".gnu_debuglink"));
    try testing.expectError(error.NoSuchOutput, firstOutput(allocator, &stripped_tmp, ".debug"));
}

test "debug info: a compiled macOS binary gets a .dSYM whose UUID matches it" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // Only Apple targets get one: a Linux build makes none on any host.
    const linux = try compileProbeLinux(allocator, &tmp, localsSource, &.{}, "");
    allocator.free(linux);
    try testing.expect(try dsymOf(allocator, &tmp) == null);

    // `dsymutil` comes with Xcode's command-line tools, on the macOS runner.
    if (builtin.os.tag != .macos) return;
    var mac_tmp = testing.tmpDir(.{});
    defer mac_tmp.cleanup();
    const ll = try compileProbe(allocator, &mac_tmp, &.{.{ .path = "probe.doxa", .data = localsSource }}, &.{}, "ll");
    allocator.free(ll);
    const dsym = (try dsymOf(allocator, &mac_tmp)) orelse return error.NoDsym;
    defer allocator.free(dsym.dwarf);
    defer allocator.free(dsym.binary);
    try testing.expectEqualSlices(u8, &(machoUuid(dsym.binary) orelse return error.NoUuid), &(machoUuid(dsym.dwarf) orelse return error.NoUuid));
    try expectContains(dsym.dwarf, "total");
}

/// The `probe-*` binary beside the invocation directory and the DWARF file of
/// the `.dSYM` beside it, if there is one.
fn dsymOf(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir) !?struct { binary: []u8, dwarf: []u8 } {
    var dir = try tmp.dir.openDir(testing.io, ".", .{ .iterate = true });
    defer dir.close(testing.io);
    var it = dir.iterate();
    while (try it.next(testing.io)) |entry| {
        if (!std.mem.startsWith(u8, entry.name, "probe-") or !std.mem.endsWith(u8, entry.name, ".dSYM")) continue;
        const stem = entry.name[0 .. entry.name.len - ".dSYM".len];
        const dwarf_path = try std.fmt.allocPrint(allocator, "{s}/Contents/Resources/DWARF/{s}", .{ entry.name, stem });
        defer allocator.free(dwarf_path);
        const dwarf = try dir.readFileAlloc(testing.io, dwarf_path, allocator, .unlimited);
        errdefer allocator.free(dwarf);
        return .{ .binary = try dir.readFileAlloc(testing.io, stem, allocator, .unlimited), .dwarf = dwarf };
    }
    return null;
}

/// The `LC_UUID` of a 64-bit little-endian Mach-O file.
fn machoUuid(file: []const u8) ?[16]u8 {
    const header_size = 32;
    if (file.len < header_size or std.mem.readInt(u32, file[0..4], .little) != std.macho.MH_MAGIC_64) return null;
    const commands = std.mem.readInt(u32, file[16..20], .little);
    var at: usize = header_size;
    for (0..commands) |_| {
        const cmd = std.mem.readInt(u32, file[at..][0..4], .little);
        const size = std.mem.readInt(u32, file[at + 4 ..][0..4], .little);
        if (cmd == @intFromEnum(std.macho.LC.UUID)) return file[at + 8 ..][0..16].*;
        at += size;
    }
    return null;
}

test "debug info: the object carries local names" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // DWARF, read from the `.debug` file a Linux build writes beside the
    // binary.
    const elf = try compileProbeLinux(allocator, &tmp, localsSource, &.{}, ".debug");
    defer allocator.free(elf);
    try expectContains(elf, "total");
    try expectContains(elf, "Point");
    try expectContains(elf, "counter");

    // CodeView, read from the PDB a Windows link writes beside the binary.
    const pdb = try compileProbeOutput(allocator, &tmp, localsSource, &.{}, "pdb");
    defer allocator.free(pdb);
    try expectContains(pdb, "total");
    try expectContains(pdb, "Point");
    try expectContains(pdb, "counter");
}

// ── Faults: a backtrace with debug info ──

const faultSource =
    \\function get(xs :: int[], i :: int) returns int {
    \\    return xs[i]
    \\}
    \\
    \\function outer(xs :: int[]) returns int {
    \\    return get(xs, 7) + 1
    \\}
    \\
    \\entry function main() {
    \\    var xs :: int[] is [1, 2, 3]
    \\    var r is outer(xs)
    \\    @print("{r}")
    \\}
;

/// A fault while a global's initializer runs.
const globalFaultSource =
    \\var xs :: int[] is [1, 2, 3]
    \\var bad is xs[5]
    \\
    \\entry function main() {
    \\    @print("{bad}")
    \\}
;

/// Recursion with no end: the call is not in tail position, so no
/// optimization turns it into a loop.
const overflowSource =
    \\function down(n :: int) returns int {
    \\    return down(n + 1) + 1
    \\}
    \\
    \\entry function main() {
    \\    @print("{down(0)}")
    \\}
;

/// Runs `source` with `flags` through `doxa run` and returns its stderr.
fn runFault(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8) ![]u8 {
    const run = try runFaultStatus(allocator, tmp, source, flags);
    return run.stderr;
}

/// `runFault`, with how the run ended.
fn runFaultStatus(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8) !struct { stderr: []u8, end: @FieldType(process.Capture, "end") } {
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "probe.doxa", .data = source });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);
    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];
    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);
    const args = try std.mem.concat(allocator, []const u8, &.{ &.{ "run", "probe.doxa", cache_flag }, flags });
    defer allocator.free(args);
    const capture = try doxa.run(allocator, io, .{ .args = args, .cwd = cwd });
    defer capture.deinit(allocator);
    return .{ .stderr = try allocator.dupe(u8, capture.stderr), .end = capture.end };
}

test "fault: with debug info a backtrace names each Doxa frame, under --strip the line alone" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const headline = "Index out of bounds at probe.doxa:2:12: index 7, length 3\n";

    const plain = try runFault(allocator, &tmp, faultSource, &.{"--strip"});
    defer allocator.free(plain);
    try testing.expectEqualStrings(headline, plain);

    // The trace ends at the entry, on every platform: the walk stops at the
    // return `__doxa_enter` recorded, so the entry's caller, the compiler's
    // `__doxa_init`, shows no frame.
    const frames = headline ++
        "  in get at probe.doxa:2:12\n" ++
        "  in outer at probe.doxa:6:5\n" ++
        "  in main at probe.doxa:11:5\n";
    const traced = try runFault(allocator, &tmp, faultSource, &.{"-O0"});
    defer allocator.free(traced);
    try testing.expectEqualStrings(frames, traced);

    // Optimized, the trace keeps the frames that survive inlining, and still
    // ends at the entry: every frame is the program's, and none is
    // `<globals>`.
    const optimized = try runFault(allocator, &tmp, faultSource, &.{"-O2"});
    defer allocator.free(optimized);
    try testing.expectStringStartsWith(optimized, headline);
    var frame_lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, optimized[headline.len..], "\n"), '\n');
    var count: usize = 0;
    while (frame_lines.next()) |frame| : (count += 1) {
        try testing.expectStringStartsWith(frame, "  in ");
        try expectContains(frame, " at probe.doxa:");
    }
    try testing.expect(count >= 1);
    try testing.expect(std.mem.indexOf(u8, optimized, "<globals>") == null);
    // The PDB reader resolves inlined frames, so on Windows each still shows,
    // innermost first; CodeView often records no column there. Zig's DWARF
    // reader does not resolve them (`Dwarf.getSymbols` ignores
    // `resolve_inline_callers`): elsewhere a function with calls inlined into
    // it shows one frame, named for the innermost inlined function at its
    // line (`in get at probe.doxa:2:12` for the whole chain).
    if (builtin.os.tag == .windows) {
        const get = std.mem.indexOf(u8, optimized, "  in get at probe.doxa:2:") orelse return error.TestExpectedContains;
        const outer = std.mem.indexOf(u8, optimized, "  in outer at probe.doxa:6:") orelse return error.TestExpectedContains;
        const main = std.mem.indexOf(u8, optimized, "  in main at probe.doxa:11:") orelse return error.TestExpectedContains;
        try testing.expect(get < outer and outer < main);
    }

    // A fault under a global's initializer names that frame `<globals>`, at
    // the initializer's line, on every platform.
    const global = try runFault(allocator, &tmp, globalFaultSource, &.{"-O0"});
    defer allocator.free(global);
    try testing.expectEqualStrings("Index out of bounds at probe.doxa:2:12: index 5, length 3\n" ++
        "  in <globals> at probe.doxa:2:12\n", global);

    // Only a program with debug info enables it, and the entry call stands
    // for no line.
    const ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = faultSource }}, &.{}, "ll");
    defer allocator.free(ll);
    try expectContains(ll, "call void @doxa_enable_backtrace()");
    try expectContains(ll, "!DILocation(line: 0, scope: !");
    // The entry is called through `__doxa_enter`, which records where the
    // walk stops and calls the entry out of line.
    try expectContains(ll, "call void @__doxa_enter(");
    try expectContains(ll, "define internal void @__doxa_enter(");
    try expectContains(ll, "store ptr %ra, ptr @doxa_entry_return");
    const bare = try emitIrFor(allocator, &tmp, faultSource, "-O0");
    defer allocator.free(bare);
    try testing.expect(std.mem.indexOf(u8, bare, "doxa_enable_backtrace") == null);
    try testing.expect(std.mem.indexOf(u8, bare, "__doxa_enter") == null);
}

test "fault: a stack overflow is a fault with a bounded, folded backtrace" {
    // Each platform traps differently: Windows raises EXCEPTION_STACK_OVERFLOW
    // to a vectored handler, Linux and macOS a SIGSEGV or SIGBUS told apart by
    // its address (`fault.handleSegfault`). The fault is the same on all of
    // them, so CI's runners check each.
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const headline = "Stack overflow\n";

    const plain = try runFaultStatus(allocator, &tmp, overflowSource, &.{"--strip"});
    defer allocator.free(plain.stderr);
    try testing.expectEqualStrings(headline, plain.stderr);
    try testing.expect(plain.end == .exited and plain.end.exited == 1);

    // In every runtime mode: the trace is the one frame, folded, cut at the
    // bound. The innermost frame may stand at the function's entry rather
    // than its call, so a second frame of `down` may print before the fold.
    for ([_][]const u8{ "--opt-mode=debug", "--opt-mode=fast" }) |mode| {
        const traced = try runFaultStatus(allocator, &tmp, overflowSource, &.{ "-O0", mode });
        defer allocator.free(traced.stderr);
        errdefer std.debug.print("{s}:\n{s}", .{ mode, traced.stderr });
        try testing.expect(traced.end == .exited and traced.end.exited == 1);
        try testing.expectStringStartsWith(traced.stderr, headline);
        try testing.expectStringEndsWith(traced.stderr, "  ... more frames not shown\n");
        var lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, traced.stderr[headline.len..], "\n"), '\n');
        var frames: usize = 0;
        var folded: usize = 0;
        while (lines.next()) |line| {
            if (std.mem.startsWith(u8, line, "  in down at probe.doxa:")) {
                frames += 1;
            } else if (std.mem.startsWith(u8, line, "  ... the frame above ")) {
                try testing.expectStringEndsWith(line, " more times");
                const count = line["  ... the frame above ".len .. line.len - " more times".len];
                folded += try std.fmt.parseInt(usize, count, 10);
            } else {
                try testing.expectEqualStrings("  ... more frames not shown", line);
            }
        }
        try testing.expect(frames >= 1 and frames <= 2);
        try testing.expectEqual(@as(usize, 64), frames + folded);
    }
}

/// A bad access in an inline `zig` block: a crash, not a fault.
const crashSource =
    \\zig Crash {
    \\    pub fn now() void {
    \\        const p: *volatile u8 = @ptrFromInt(0x10);
    \\        p.* = 1;
    \\    }
    \\}
    \\
    \\entry function main() {
    \\    Crash.now()
    \\}
;

test "fault: a crash prints its line and Doxa frames, then ends with the platform's status" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const headline = "Invalid memory access at address 0x10\n";

    // `doxa run` names how the program ended, and exits 1 for any crash.
    const ending = if (builtin.os.tag == .windows)
        "doxa: the program crashed: access violation (0xC0000005)\n"
    else
        "doxa: the program was stopped by signal ";

    const plain = try runFaultStatus(allocator, &tmp, crashSource, &.{"--strip"});
    defer allocator.free(plain.stderr);
    errdefer std.debug.print("--strip:\n{s}", .{plain.stderr});
    try testing.expect(plain.end == .exited and plain.end.exited == 1);
    try testing.expectStringStartsWith(plain.stderr, headline ++ ending);

    // The inline Zig function's frames are not the program's: the trace
    // starts at its caller.
    const traced = try runFaultStatus(allocator, &tmp, crashSource, &.{"-O0"});
    defer allocator.free(traced.stderr);
    errdefer std.debug.print("-O0:\n{s}", .{traced.stderr});
    try testing.expect(traced.end == .exited and traced.end.exited == 1);
    try testing.expectStringStartsWith(traced.stderr, headline ++ "  in main at probe.doxa:9:");
    const rest = traced.stderr[std.mem.indexOfScalar(u8, traced.stderr[headline.len..], '\n').? + headline.len + 1 ..];
    try testing.expectStringStartsWith(rest, ending);
    // A bad access ends a POSIX program with SIGSEGV, or SIGBUS on macOS for
    // some unmapped addresses.
    if (builtin.os.tag != .windows) {
        const signal = std.mem.trimEnd(u8, rest[ending.len..], "\n");
        try testing.expect(std.mem.eql(u8, signal, "SEGV") or std.mem.eql(u8, signal, "BUS"));
    }
}

// ── Instrumentation: --instrument=calls and doxa report ──

const callsSource =
    \\function fib(n :: int) returns int {
    \\    if n < 2 then return n
    \\    return fib(n - 1) + fib(n - 2)
    \\}
    \\
    \\entry function main() {
    \\    @print("{fib(10)}")
    \\}
;

/// Runs the installed `doxa` with `args` in `tmp`, in the probe cache, and
/// returns its stdout.
fn doxaStdout(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, slot: harness.Slot, args: []const []const u8) ![]u8 {
    const io = testing.io;
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{slot.path});
    defer allocator.free(cache_flag);
    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(io, &cwd_buffer)];
    const doxa: harness.Doxa = try .init(allocator, io);
    defer doxa.deinit(allocator);
    const all = try std.mem.concat(allocator, []const u8, &.{ args, &.{cache_flag} });
    defer allocator.free(all);
    const capture = try doxa.run(allocator, io, .{ .args = all, .cwd = cwd });
    defer capture.deinit(allocator);
    if (!capture.succeeded()) {
        std.debug.print("doxa {s} failed ({any}):\n{s}\n{s}\n{s}", .{ args[0], capture.end, capture.stdout, capture.stderr, capture.stuck });
        return error.DoxaFailed;
    }
    return allocator.dupe(u8, capture.stdout);
}

test "instrument: calls are counted per function and reported hottest first" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "calls.doxa", .data = callsSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "calls.doxa", "--instrument=calls,lines" });
    defer allocator.free(out);
    try testing.expectEqualStrings("55", std.mem.trim(u8, out, "\r\n"));

    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "calls" });
    defer allocator.free(text);
    try expectContains(text, "Calls\n==========================================\n  177\tfib\tcalls.doxa:1:10\n  1\tmain\tcalls.doxa:6:16\n");
    // Each statement at its own start: `fib(10)` takes 89 leaves and 88
    // inner calls.
    try expectContains(text, "Lines\n==========================================\n" ++
        "  177\tcalls.doxa:2:5\tfib\n" ++
        "  89\tcalls.doxa:2:19\tfib\n" ++
        "  88\tcalls.doxa:3:5\tfib\n" ++
        "  1\tcalls.doxa:7:5\tmain\n");
    // The runtime enters `__doxa_init`; the program never calls it.
    try testing.expect(std.mem.indexOf(u8, text, "<globals>") == null);

    // Off means absent: no counter, no registration, no extra global.
    const ll = try emitIrFor(allocator, &tmp, callsSource, "-O0");
    defer allocator.free(ll);
    try testing.expect(std.mem.indexOf(u8, ll, "doxa.counts") == null);
    try testing.expect(std.mem.indexOf(u8, ll, "doxa_instrument") == null);
}

// ── Complexity: --emit-complexity and doxa report ──

const complexitySource =
    \\function label(i :: int) returns string {
    \\    return "item {i}"
    \\}
    \\
    \\entry function main() {
    \\    var names :: string[] is []
    \\    for i while i < 3 do i += 1 {
    \\        var tmp is "t{i}"
    \\        for j while j < 2 do j += 1 {
    \\            @push(names, label(i * j))
    \\        }
    \\        @print(tmp)
    \\    }
    \\    @print("{@length(names)}")
    \\}
;

test "complexity: loop nests, where in-loop allocations land, and calls that allocate" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "loops.doxa", .data = complexitySource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "loops.doxa", "--emit-complexity" });
    defer allocator.free(out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "loops" });
    defer allocator.free(text);

    try expectContains(text, "  2\tmain\tloops.doxa:7:5 > loops.doxa:9:9\n");
    // The pushed element re-homes into `main`'s arena every inner iteration;
    // the interpolated `tmp` lives in its own loop's arena, reset each time.
    try expectContains(text, "  ! loops.doxa:10:13\tmain\tarray_push in the loop at loops.doxa:9:9: main's arena, reclaimed when it returns\n");
    try expectContains(text, "    loops.doxa:8:20\tmain\tstr_concat in the loop at loops.doxa:7:5: this loop's arena, reset every iteration\n");
    try expectContains(text, "  loops.doxa:10:26\tmain\tcalls label (allocates) in the loop at loops.doxa:9:9\n");
    // No counts were asked for, so the report has none.
    try testing.expect(std.mem.indexOf(u8, text, "Calls\n") == null);

    // The findings are a side file: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, complexitySource, "--emit-complexity");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, complexitySource, "-O0");
    defer allocator.free(without);
    try testing.expectEqualStrings(without, with);
}

const transitiveSource =
    \\function parse(n :: int) returns string {
    \\    return "n{n}"
    \\}
    \\
    \\function load(n :: int) returns int {
    \\    return @length(parse(n))
    \\}
    \\
    \\function sum(n :: int) returns int {
    \\    var t :: int is 0
    \\    for i while i < n do i += 1 {
    \\        t += i
    \\    }
    \\    return t
    \\}
    \\
    \\function wrapper(n :: int) returns int {
    \\    return sum(n)
    \\}
    \\
    \\function ping(n :: int) returns int {
    \\    if n <= 0 then return 0
    \\    return pong(n - 1)
    \\}
    \\
    \\function pong(n :: int) returns int {
    \\    return ping(n - 1)
    \\}
    \\
    \\entry function main() {
    \\    var total :: int is 0
    \\    for i while i < 3 do i += 1 {
    \\        total += load(i) + wrapper(i) + ping(i)
    \\    }
    \\    @print("{total}\n")
    \\}
;

test "complexity: a call in a loop names what its callees allocate or repeat" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "calls.doxa", .data = transitiveSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "calls.doxa", "--emit-complexity" });
    defer allocator.free(out);
    try testing.expectEqualStrings("7\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "calls" });
    defer allocator.free(text);

    // `load` allocates only through `parse`, `wrapper` loops only through
    // `sum`: each finding names the function that does it.
    try expectContains(text, "  calls.doxa:33:18\tmain\tcalls load (allocates through parse) in the loop at calls.doxa:32:5\n");
    try expectContains(text, "  calls.doxa:33:28\tmain\tcalls wrapper (loops through sum) in the loop at calls.doxa:32:5\n");
    // Mutual recursion settles, and a chain that neither allocates nor
    // loops is no finding.
    try testing.expect(std.mem.indexOf(u8, text, "calls ping") == null);
}

const rankedSource =
    \\function make(n :: int) returns string {
    \\    return "n{n}"
    \\}
    \\
    \\entry function main() {
    \\    var cold :: string[] is []
    \\    for i while i < 2 do i += 1 {
    \\        @push(cold, make(i))
    \\    }
    \\    var hot :: string[] is []
    \\    for i while i < 50 do i += 1 {
    \\        @push(hot, make(i))
    \\    }
    \\    @print("{@length(cold)} {@length(hot)}\n")
    \\}
;

test "complexity: with line counts, findings are ranked by how often they ran" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "ranked.doxa", .data = rankedSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    // Without counts, source order.
    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "ranked.doxa", "--emit-complexity" });
    defer allocator.free(out);
    try testing.expectEqualStrings("2 50\n", out);
    const plain = try doxaStdout(allocator, &tmp, slot, &.{ "report", "ranked" });
    defer allocator.free(plain);
    const cold_call = "  ranked.doxa:8:21\tmain\tcalls make (allocates) in the loop at ranked.doxa:7:5";
    const hot_call = "  ranked.doxa:12:20\tmain\tcalls make (allocates) in the loop at ranked.doxa:11:5";
    try testing.expect(std.mem.indexOf(u8, plain, cold_call).? < std.mem.indexOf(u8, plain, hot_call).?);

    // With an `--instrument=lines` run, the 50-iteration loop leads each
    // section, and every row says how often its statement ran.
    const counted_out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "ranked.doxa", "--emit-complexity", "--instrument=lines" });
    defer allocator.free(counted_out);
    const counted = try doxaStdout(allocator, &tmp, slot, &.{ "report", "ranked" });
    defer allocator.free(counted);
    const hot = std.mem.indexOf(u8, counted, hot_call ++ " (ran 50 times)\n") orelse return error.TestExpectedContains;
    const cold = std.mem.indexOf(u8, counted, cold_call ++ " (ran 2 times)\n") orelse return error.TestExpectedContains;
    try testing.expect(hot < cold);
    const hot_push = std.mem.indexOf(u8, counted, "  ! ranked.doxa:12:9\tmain\tarray_push") orelse return error.TestExpectedContains;
    const cold_push = std.mem.indexOf(u8, counted, "  ! ranked.doxa:8:9\tmain\tarray_push") orelse return error.TestExpectedContains;
    try testing.expect(hot_push < cold_push);
}

// ── Memory: --emit-memory and doxa report ──

const memorySource =
    \\function total(xs :: int[]) returns int {
    \\    var sum is 0
    \\    for i while i < @length(xs) do i += 1 {
    \\        sum += xs[i]
    \\    }
    \\    return sum
    \\}
    \\
    \\function name(i :: int) returns string {
    \\    return "n{i}"
    \\}
    \\
    \\function rename(^target :: string, n :: int) {
    \\    for j while j < n do j += 1 {
    \\        target is "r{j}"
    \\    }
    \\}
    \\
    \\entry function main() {
    \\    var last is ""
    \\    for i while i < 3 do i += 1 {
    \\        last is name(i)
    \\    }
    \\    var t is "x"
    \\    rename(^t, 2)
    \\    @print("{last} {t} {total([1, 2])}\n")
    \\    var items :: Item[] is [$Item { count is 1 }]
    \\    sweep(^items)
    \\}
    \\
    \\struct Item {
    \\    public count :: int,
    \\}
    \\
    \\function bump(^it :: Item) {
    \\    it.count += 1
    \\}
    \\
    \\function sweep(^items :: Item[]) {
    \\    each it in ^items {
    \\        bump(^it)
    \\    }
    \\}
;

test "memory: where allocations live, the copies kept and removed, the arenas dropped" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "mem.doxa", .data = memorySource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "mem.doxa", "--emit-memory", "--emit-complexity" });
    defer allocator.free(out);
    try testing.expectEqualStrings("n2 r1 3\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "mem" });
    defer allocator.free(text);

    // The template `name` returns is built where it goes, in its caller's
    // arena (`plan/strings.md`, Phase 3), so returning it copies nothing.
    try expectContains(text, "  mem.doxa:10:5\tname\tstring\tin the caller's arena; reclaimed when the caller returns\n");
    try expectContains(text, "  mem.doxa:10:5\tname\tstring\tthe value itself, which already lives there: returned\n");
    // Copies: one stored from a loop into a string variable that outlives
    // it. A template stored through a `^` target is built in the target's
    // arena, so it is no copy. An element lent on from `each … in ^items`
    // lives in an arena known only at run time, so its write-back is the
    // runtime check.
    try expectContains(text, "  mem.doxa:22:9\tmain\tstring\tcopied here: stored to a longer-lived variable\n");
    try expectContains(text, "  mem.doxa:15:9\trename\tstring\tthe value itself, which already lives there: stored to a longer-lived variable\n");
    try expectContains(text, "  mem.doxa:41:9\tsweep\tItem\tcopied at run time unless it already lives there: stored to a longer-lived variable\n");
    // `t is "x"` is a literal: its copy is the value itself.
    try expectContains(text, "\tmain\tstring\tthe value itself, which already lives there: stored to a longer-lived variable\n");
    // `total` allocates nothing: its arenas and its `@caller` are gone.
    try expectContains(text, "  mem.doxa:1:10\ttotal\tthe function's arena: nothing allocates in it\n");
    try expectContains(text, "  mem.doxa:3:5\ttotal\ta loop body's arena: nothing allocates in it\n");
    try expectContains(text, "  total\tits @caller arena: it returns nothing that lives there\n");
    try expectContains(text, " Copies: 3 kept (1 clone, 2 runtime), ");

    // The runtime copy is in a loop, so step 6's findings name it too.
    try expectContains(text, "  mem.doxa:41:9\tsweep\tin the loop at mem.doxa:40:10: a copy the arena pass left to a runtime check\n");

    // A side file only: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, memorySource, "--emit-memory");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, memorySource, "-O0");
    defer allocator.free(without);
    try testing.expectEqualStrings(without, with);
}

// ── String buffers ──

const stringPushSource =
    \\function build(n :: int) returns int {
    \\    var s is ""
    \\    for i while i < n do i += 1 {
    \\        @push(s, "a")
    \\    }
    \\    const t is s
    \\    return @length(t)
    \\}
    \\public entry function main() {
    \\    @print("{build(3)}\n")
    \\}
;

test "string buffers: a push loop appends in place, with no copy and no loop arena" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, stringPushSource, "--opt=0");
    defer allocator.free(ir_text);
    const body = functionBody(ir_text, "build") orelse return error.FunctionNotEmitted;
    // `s` is a buffer the push grows (`plan/strings.md`): nothing builds a
    // new string, nothing copies one out of the loop, and the loop's arenas
    // hold nothing, so they are gone. `const t is s` shares the bytes.
    try testing.expect(std.mem.indexOf(u8, body, "call void @doxa_str_append_string(") != null);
    try testing.expect(std.mem.indexOf(u8, body, "@doxa_str_concat") == null);
    try testing.expect(std.mem.indexOf(u8, body, "@doxa_str_clone") == null);
    try testing.expect(std.mem.indexOf(u8, body, "@doxa_scope_reset") == null);
}

const stringViewSource =
    \\var kept is ""
    \\function window(s :: string) returns int {
    \\    const head is @slice(s, 0, 3)
    \\    return @length(head) + @length(s[1])
    \\}
    \\function keep(s :: string) {
    \\    kept is @slice(s, 1, 2)
    \\}
    \\public entry function main() {
    \\    keep("abcd")
    \\    @print("{window("abcd")} {kept}\n")
    \\}
;

test "string views: a slice or an index allocates nothing; one kept longer is copied" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, stringViewSource, "--opt=0");
    defer allocator.free(ir_text);
    // `@slice` and `s[i]` are views of the parameter's bytes, so `window`
    // allocates nothing and drops its arena (`plan/strings.md`, Phase 2).
    const window = functionBody(ir_text, "window") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, window, "@doxa_substring") != null);
    try testing.expect(std.mem.indexOf(u8, window, "@doxa_str_clone") == null);
    try testing.expect(std.mem.indexOf(u8, window, "@doxa_scope_enter") == null);
    // A view stored into a global outlives the caller's bytes: it is copied
    // into the root arena.
    const keep = functionBody(ir_text, "keep") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, keep, "@doxa_str_clone") != null);
}

const stringJoinSource =
    \\function line(i :: int, name :: string) returns string {
    \\    const x is @float(i) + 0.25
    \\    return "{i}: {x} {name}|"
    \\}
    \\function fresh() returns string {
    \\    return @pack([0x68, 0x69])
    \\}
    \\function held(bytes :: byte[]) returns string {
    \\    return @pack(bytes)
    \\}
    \\public entry function main() {
    \\    @print("{line(3, "n")} {fresh()} {held([0x6F, 0x6B])}\n")
    \\}
;

test "string joins: an interpolation is one allocation; @pack of a fresh array keeps its bytes" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, stringJoinSource, "--opt=0");
    defer allocator.free(ir_text);
    // The int and the float are written into the one string the template
    // makes; no piece is a string of its own (`plan/strings.md`, Phase 3).
    const line = functionBody(ir_text, "line") orelse return error.FunctionNotEmitted;
    try testing.expectEqual(@as(usize, 1), std.mem.count(u8, line, "call void @doxa_str_concat("));
    try testing.expect(std.mem.indexOf(u8, line, "@doxa_int_to_string") == null);
    try testing.expect(std.mem.indexOf(u8, line, "@doxa_float_to_string") == null);
    // A fresh array's bytes become the string; a held array's are copied,
    // since its binding could still write them (Phase 4).
    const fresh = functionBody(ir_text, "fresh") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, fresh, "@doxa_bytes_view") != null);
    try testing.expect(std.mem.indexOf(u8, fresh, "@doxa_pack_bytes") == null);
    const held = functionBody(ir_text, "held") orelse return error.FunctionNotEmitted;
    try testing.expect(std.mem.indexOf(u8, held, "@doxa_pack_bytes") != null);
}

// ── Block arenas ──

const blockSource =
    \\function pick(id :: int) returns int {
    \\    var result :: int is 0
    \\    {
    \\        const list is [id, id + 1, id + 2]
    \\        result is list[1]
    \\    }
    \\    return result
    \\}
    \\
    \\function early(id :: int) returns string {
    \\    {
    \\        const t is "x{id}"
    \\        if id > 2 then return t
    \\    }
    \\    return "small"
    \\}
    \\
    \\function quiet(id :: int) returns int {
    \\    var r :: int is 0
    \\    {
    \\        r is id * 2
    \\    }
    \\    return r
    \\}
    \\
    \\entry function main() {
    \\    @print("{pick(7)} {early(5)} {early(1)} {quiet(4)}\n")
    \\}
;

test "block arenas: a block reclaims what it allocates at its `}` and on every edge out" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "blk.doxa", .data = blockSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "blk.doxa", "--emit-memory" });
    defer allocator.free(out);
    try testing.expectEqualStrings("8 x5 small 8\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "blk" });
    defer allocator.free(text);

    // The list lives in the block's arena, reclaimed at its `}`, not when
    // `pick` returns.
    try expectContains(text, "  blk.doxa:4:23\tpick\tint[]\tin a block's arena; reclaimed when the block ends (line 6)\n");
    // A `return` out of the block closes it too, after copying `t` out.
    try expectContains(text, "  blk.doxa:12:20\tearly\tstring\tin a block's arena; reclaimed when the block ends (lines 13, 14)\n");
    try expectContains(text, "  blk.doxa:13:24\tearly\tstring\tcopied here: returned\n");
    // A block that allocates nothing opens no arena; the bare `then return t`
    // is no block of its own.
    try expectContains(text, "  blk.doxa:20:5\tquiet\ta block's arena: nothing allocates in it\n");
    try testing.expect(std.mem.indexOf(u8, text, "early\ta block's arena: nothing allocates in it") == null);
}

// ── Retention ──

const retentionSource =
    \\var log :: string[] is []
    \\
    \\function record(n :: int) {
    \\    for i while i < n do i += 1 {
    \\        @push(log, "entry-{i}")
    \\    }
    \\}
    \\
    \\function joined(n :: int) returns string {
    \\    var s :: string is ""
    \\    for i while i < n do i += 1 {
    \\        s is s + "x"
    \\    }
    \\    return s
    \\}
    \\
    \\function scoped(n :: int) returns int {
    \\    var total :: int is 0
    \\    for i while i < n do i += 1 {
    \\        {
    \\            const t is "t{i}"
    \\            total += @length(t)
    \\        }
    \\    }
    \\    return total
    \\}
    \\
    \\entry function main() {
    \\    record(3)
    \\    @print("{@length(log)} {joined(4)} {scoped(3)}\n")
    \\}
;

test "retention: what a loop makes that no iteration reclaims, and what it grew the root by" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "ret.doxa", .data = retentionSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    // Static rows alone.
    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "ret.doxa", "--emit-memory" });
    defer allocator.free(out);
    try testing.expectEqualStrings("3 xxxx 6\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "ret" });
    defer allocator.free(text);
    // A push into a global in a loop grows the root every iteration.
    try expectContains(text, "  ret.doxa:5:9\trecord\tstring\tin the loop at ret.doxa:4:5: escapes into global `log`, kept until the program ends\n");
    // An append to a string variable grows its buffer in the function's
    // arena: an allocation, not something every iteration makes and keeps
    // (B034, `plan/strings.md`).
    try expectContains(text, "  ret.doxa:12:9\tjoined\tstring\tin joined's arena; reclaimed when it returns (line 14)\n");
    try testing.expect(std.mem.indexOf(u8, text, "\tjoined\tstring\tin the loop at") == null);
    // What a block in the loop allocates is reclaimed every iteration.
    try testing.expect(std.mem.indexOf(u8, text, "\tscoped\tstring\tin the loop at") == null);

    // With a memtrace run, each row says what it held, and the root's growth
    // is named by site: three strings of 7 bytes into `log`.
    const traced = try doxaStdout(allocator, &tmp, slot, &.{ "run", "ret.doxa", "--emit-memory", "--instrument=memtrace" });
    defer allocator.free(traced);
    const joined_text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "ret" });
    defer allocator.free(joined_text);
    try expectContains(joined_text, "escapes into global `log`, kept until the program ends (this run: 21 B in 3 allocations, 21 B into the root arena)\n");
    // The template pushed into the global's array is built in the array's
    // arena: an allocation there, with no copy after it.
    try expectContains(joined_text, "  21 B\tret.doxa:5:9\trecord\talloc\n");

    // A side file only: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, retentionSource, "--emit-memory");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, retentionSource, "-O0");
    defer allocator.free(without);
    try testing.expectEqualStrings(without, with);
}

// ── Memory trace: --instrument=memtrace ──

const memtraceSource =
    \\function fresh(^dst :: int[], n :: int) {
    \\    dst is [n, n]
    \\}
    \\
    \\function keep(^dst :: int[], src :: int[]) {
    \\    dst is src
    \\}
    \\
    \\entry function main() {
    \\    var a :: int[] is []
    \\    for i while i < 3 do i += 1 {
    \\        fresh(^a, i)
    \\    }
    \\    var b :: int[] is [7]
    \\    keep(^a, b)
    \\    var text :: string is @string(b)
    \\    @print("{@length(a)} {text}\n")
    \\    var items :: Item[] is [$Item { count is 1 }]
    \\    sweep(^items)
    \\}
    \\
    \\struct Item {
    \\    public count :: int,
    \\}
    \\
    \\function bump(^it :: Item) {
    \\    it.count += 1
    \\}
    \\
    \\function sweep(^items :: Item[]) {
    \\    each it in ^items {
    \\        bump(^it)
    \\    }
    \\}
;

test "memtrace: runtime-checked copies, arenas opened and reset, bytes by site" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "trace.doxa", .data = memtraceSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "trace.doxa", "--instrument=memtrace" });
    defer allocator.free(out);
    try testing.expectEqualStrings("1 [7]\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "trace" });
    defer allocator.free(text);

    // A store through `^` of a fresh value is a static clone: `fresh`'s new
    // array lives in an arena it opened, which the `^` argument's outlives.
    // `keep` stores a parameter, a value its caller still holds, which plain
    // stores always copy: a static clone too. Neither is checked at run time.
    // The element `sweep` lends on lives in an arena known only at run time;
    // its write-back is checked, and keeps the element where it already is.
    try expectContains(text, "\ttrace.doxa:6:5\tkeep\tclone\n");
    try testing.expect(std.mem.indexOf(u8, text, "kept\ttrace.doxa:2:5\tfresh") == null);
    try testing.expect(std.mem.indexOf(u8, text, "kept\ttrace.doxa:6:5\tkeep") == null);
    try expectContains(text, "  0 copied, 1 kept\ttrace.doxa:32:9\tsweep\n");
    // `fresh` opens its arena once a call; the loop resets its body's twice
    // an iteration (at the top and before the step).
    try expectContains(text, "\t3 opened, 0 resets\ttrace.doxa:1:10\tfresh\n");
    try expectContains(text, "\t1 opened, 6 resets\ttrace.doxa:11:5\tmain\n");
    // Every byte the copies took is counted to the copy's own site, and
    // `fresh`'s literal is gone with each call: its peak is one call's.
    try expectContains(text, "\ttrace.doxa:2:5\tfresh\tclone\n");
    try expectContains(text, "\ttrace.doxa:2:12\tfresh\talloc\n");
    // Text renders in its destination arena and is trimmed there: `[7]` is
    // three bytes, asked for by the `@string` that made it.
    const site = std.mem.indexOf(u8, text, "\ttrace.doxa:16:27\tmain\talloc\n") orelse return error.TestUnexpectedResult;
    const row = text[(std.mem.lastIndexOfScalar(u8, text[0..site], '\n') orelse 0) + 1 .. site];
    try testing.expect(std.mem.startsWith(u8, row, "  3 B\t1 alloc\t"));

    // The trace is present only when asked for.
    const traced = try emitIrFor(allocator, &tmp, memtraceSource, "--instrument=memtrace");
    defer allocator.free(traced);
    try expectContains(traced, "store i32 0, ptr @doxa_memtrace_site");
    const plain = try emitIrFor(allocator, &tmp, memtraceSource, "-O0");
    defer allocator.free(plain);
    try testing.expect(std.mem.indexOf(u8, plain, "doxa_memtrace_site") == null);
}

// ── Memory check: --instrument=memcheck ──

test "memcheck: a program that reads only live memory runs as it does unchecked" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "checked.doxa", .data = memorySource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    // Reclaimed arenas are poisoned and kept, and every read checks it is
    // not in one: the verifier's arena rules say none is, and this run
    // holds the runtime to them.
    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "checked.doxa", "--instrument=memcheck" });
    defer allocator.free(out);
    try testing.expectEqualStrings("n2 r1 3\n", out);

    const checked = try emitIrFor(allocator, &tmp, memorySource, "--instrument=memcheck");
    defer allocator.free(checked);
    try expectContains(checked, "call void @doxa_memcheck(ptr ");
    const plain = try emitIrFor(allocator, &tmp, memorySource, "-O0");
    defer allocator.free(plain);
    try testing.expect(std.mem.indexOf(u8, plain, "doxa_memcheck") == null);
}

// ── Debug info: every Doxa type is described ──

const containerSource =
    \\enum Color {
    \\    Red,
    \\    Green,
    \\    Blue,
    \\}
    \\
    \\var flag :: int | string
    \\var tint is true
    \\var c is Color.Blue
    \\
    \\entry function main() {
    \\    var dynamic is [1, 2, 3]
    \\    var fixed :: int[3] is [1, 2, 3]
    \\    c is Color.Red
    \\    flag is 1
    \\    tint is false
    \\    @print("{dynamic[0]} {fixed[1]} {@length(dynamic)}")
    \\}
;

test "debug info: arrays, unions and enumerations are described" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ll = try compileProbe(allocator, &tmp, &.{.{ .path = "probe.doxa", .data = containerSource }}, &.{}, "ll");
    defer allocator.free(ll);

    // Each function names its return and parameter types.
    try expectContains(ll, "!DISubroutineType(types: !");
    // A fixed array is an array of its element with its extent.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_array_type, baseType: !");
    try expectContains(ll, "!DISubrange(count: 3)");
    // A dynamic array is its header, with a typed data pointer and a length.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_structure_type, name: \"array\", ");
    try expectContains(ll, "!DIDerivedType(tag: DW_TAG_member, name: \"data\", ");
    // A union or group is the box, whose tag is the DoxaTag enumeration,
    // read from the runtime's own enum.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_structure_type, name: \"value\", ");
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_enumeration_type, name: \"tag\", ");
    try expectContains(ll, "!DIEnumerator(name: \"Function\", value: 9)");
    try testing.expect(std.mem.indexOf(u8, ll, "!DIEnumerator(name: \"Map\"") == null);
    // An enum and a tetra are enumerations of their values.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_enumeration_type, name: \"Color\", ");
    try expectContains(ll, "!DIEnumerator(name: \"Blue\", value: 2)");
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_enumeration_type, name: \"tetra\", ");
}

fn expectContains(haystack: []const u8, needle: []const u8) !void {
    if (std.mem.indexOf(u8, haystack, needle) != null) return;
    std.debug.print("expected `{s}` in:\n{s}\n", .{ needle, haystack });
    return error.TestExpectedContains;
}

/// The `!DILocation` node at `line:column`, through its closing parenthesis.
fn keyLocation(ll: []const u8, line: u32, column: u32) ?[]const u8 {
    var buf: [64]u8 = undefined;
    const needle = std.fmt.bufPrint(&buf, "!DILocation(line: {d}, column: {d}, ", .{ line, column }) catch return null;
    const at = std.mem.indexOf(u8, ll, needle) orelse return null;
    const end = std.mem.indexOfScalarPos(u8, ll, at, ')') orelse return null;
    return ll[at .. end + 1];
}

/// Compiles `source` for Windows with `flags` and returns the file beside
/// the output binary with extension `ext`.
fn compileProbeOutput(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8, ext: []const u8) ![]u8 {
    const all = try std.mem.concat(allocator, []const u8, &.{ flags, &.{ "--arch=x86_64", "--os=windows" } });
    defer allocator.free(all);
    const ll = try compileProbe(allocator, tmp, &.{.{ .path = "probe.doxa", .data = source }}, all, "ll");
    defer allocator.free(ll);
    return firstOutput(allocator, tmp, ext);
}

/// The first `probe-*` file with extension `ext` beside the invocation
/// directory.
fn firstOutput(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, ext: []const u8) ![]u8 {
    // `-o <stem>` names the binary, and its side files, beside the invocation
    // directory.
    var dir = try tmp.dir.openDir(testing.io, ".", .{ .iterate = true });
    defer dir.close(testing.io);
    var it = dir.iterate();
    while (try it.next(testing.io)) |entry| {
        if (std.mem.endsWith(u8, entry.name, ext) and std.mem.startsWith(u8, entry.name, "probe-")) {
            return dir.readFileAlloc(testing.io, entry.name, allocator, .unlimited);
        }
    }
    return error.NoSuchOutput;
}

/// The contents of the section named `name` in the little-endian ELF64 file
/// `file`, if it has one. The section headers decide it, not the bytes: the
/// runtime's own code holds section names and `.debug` paths as strings.
fn section(file: []const u8, name: []const u8) ?[]const u8 {
    const Shdr = std.elf.Elf64_Shdr;
    const header = std.mem.bytesToValue(std.elf.Elf64_Ehdr, file[0..@sizeOf(std.elf.Elf64_Ehdr)]);
    const headers = file[@intCast(header.e_shoff)..];
    const strings_header = std.mem.bytesToValue(Shdr, headers[header.e_shstrndx * @sizeOf(Shdr) ..][0..@sizeOf(Shdr)]);
    const strings = file[@intCast(strings_header.sh_offset)..][0..@intCast(strings_header.sh_size)];
    for (0..header.e_shnum) |i| {
        const s = std.mem.bytesToValue(Shdr, headers[i * @sizeOf(Shdr) ..][0..@sizeOf(Shdr)]);
        if (s.sh_type == std.elf.SHT_NULL) continue;
        const start = strings[s.sh_name..];
        if (!std.mem.eql(u8, start[0..std.mem.indexOfScalar(u8, start, 0).?], name)) continue;
        if (s.sh_type == std.elf.SHT_NOBITS) return &.{};
        return file[@intCast(s.sh_offset)..][0..@intCast(s.sh_size)];
    }
    return null;
}

/// The `NT_GNU_BUILD_ID` note's ID in `file`, if it has one.
fn buildId(file: []const u8) ?[]const u8 {
    const note = section(file, ".note.gnu.build-id") orelse return null;
    const name_size = std.mem.readInt(u32, note[0..4], .little);
    const id_size = std.mem.readInt(u32, note[4..8], .little);
    if (std.mem.readInt(u32, note[8..12], .little) != std.elf.NT_GNU_BUILD_ID) return null;
    const id_start = 12 + std.mem.alignForward(usize, name_size, 4);
    return note[id_start..][0..id_size];
}

fn hasSection(file: []const u8, name: []const u8) !bool {
    return section(file, name) != null;
}

/// Compiles `source` for Linux with `flags` and returns the output with
/// extension `ext`: the binary (`""`), or its debug information (`.debug`).
fn compileProbeLinux(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8, ext: []const u8) ![]u8 {
    const all = try std.mem.concat(allocator, []const u8, &.{ flags, &.{ "--arch=x86_64", "--os=linux" } });
    defer allocator.free(all);
    const ll = try compileProbe(allocator, tmp, &.{.{ .path = "probe.doxa", .data = source }}, all, "ll");
    defer allocator.free(ll);
    if (ext.len != 0) return firstOutput(allocator, tmp, ext);
    // The binary has no extension: the one `probe-*` file without a dot.
    var dir = try tmp.dir.openDir(testing.io, ".", .{ .iterate = true });
    defer dir.close(testing.io);
    var it = dir.iterate();
    while (try it.next(testing.io)) |entry| {
        if (std.mem.startsWith(u8, entry.name, "probe-") and std.mem.indexOfScalar(u8, entry.name, '.') == null) {
            return dir.readFileAlloc(testing.io, entry.name, allocator, .unlimited);
        }
    }
    return error.NoSuchOutput;
}
