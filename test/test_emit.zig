//! IR-shape probes. Each compiles a small program through the installed
//! `doxa compile` and asserts on the IR the emitter chose. They drive the
//! binary, so they run with the program suites (`suites.zig`), never in a unit
//! root whose result is cached on its own executable.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const harness = @import("harness.zig");

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
        std.debug.print("doxa compile failed ({any}):\n{s}\n{s}\n", .{ capture.end, capture.stdout, capture.stderr });
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
    try testing.expect(std.mem.indexOf(u8, ir_text, "urem i64") != null);
    // But the add's range proved it safe, so no check was emitted. Match the
    // *call* form: the module still declares the intrinsic it may have used.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call { i64, i1 } @llvm.s") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_fault(") == null);
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

test "module: every function carries the tune-cpu attribute group" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, probe, "--opt=0");
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
    \\    counter is n
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

test "debug info: the object carries local names" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    // DWARF, read from a Linux executable.
    const elf = try compileProbeLinux(allocator, &tmp, localsSource, &.{});
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

/// Runs `source` with `flags` through `doxa run` and returns its stderr.
fn runFault(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8) ![]u8 {
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
    return allocator.dupe(u8, capture.stderr);
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

    // Optimized, every Doxa frame still shows, innermost first, inlined ones
    // included. CodeView often records no column there, so columns are not
    // pinned.
    const optimized = try runFault(allocator, &tmp, faultSource, &.{"-O2"});
    defer allocator.free(optimized);
    try testing.expectStringStartsWith(optimized, headline);
    const get = std.mem.indexOf(u8, optimized, "  in get at probe.doxa:2:") orelse return error.TestExpectedContains;
    const outer = std.mem.indexOf(u8, optimized, "  in outer at probe.doxa:6:") orelse return error.TestExpectedContains;
    const main = std.mem.indexOf(u8, optimized, "  in main at probe.doxa:11:") orelse return error.TestExpectedContains;
    try testing.expect(get < outer and outer < main);
    try testing.expect(std.mem.indexOf(u8, optimized, "<globals>") == null);

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
        std.debug.print("doxa {s} failed ({any}):\n{s}\n{s}\n", .{ args[0], capture.end, capture.stdout, capture.stderr });
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

    // An explicit return closes `name`'s arena at its line; the value it
    // returns lives in its caller's.
    try expectContains(text, "  mem.doxa:10:12\tname\tstring\tin name's arena; reclaimed when it returns (line 10)\n");
    try expectContains(text, "  mem.doxa:10:5\tname\tstring\tin the caller's arena; reclaimed when the caller returns\n");
    // Copies: a static clone out to the caller, one carried out of a loop,
    // and the runtime check a `^` target needs.
    try expectContains(text, "  mem.doxa:10:5\tname\tstring\tcopied here: returned\n");
    try expectContains(text, "  mem.doxa:22:9\tmain\tstring\tcopied here: carried out of a branch or loop\n");
    try expectContains(text, "  mem.doxa:15:9\trename\tstring\tcopied at run time unless it already lives there: stored to a longer-lived variable\n");
    // `t is "x"` is a literal: its copy is the value itself.
    try expectContains(text, "\tmain\tstring\tthe value itself, which already lives there: stored to a longer-lived variable\n");
    // `total` allocates nothing: its arenas and its `@caller` are gone.
    try expectContains(text, "  mem.doxa:1:10\ttotal\tthe function's arena: nothing allocates in it\n");
    try expectContains(text, "  mem.doxa:3:5\ttotal\ta loop body's arena: nothing allocates in it\n");
    try expectContains(text, "  total\tits @caller arena: it returns nothing that lives there\n");
    try expectContains(text, " Copies: 3 kept (2 clone, 1 runtime), ");

    // The runtime copy is in a loop, so step 6's findings name it too.
    try expectContains(text, "  mem.doxa:15:9\trename\tin the loop at mem.doxa:14:5: a copy the arena pass left to a runtime check\n");

    // A side file only: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, memorySource, "--emit-memory");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, memorySource, "-O0");
    defer allocator.free(without);
    try testing.expectEqualStrings(without, with);
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
    // A value carried across iterations lands in the function's arena; a
    // block around the loop and its variable is the fix.
    try expectContains(text, "  ret.doxa:12:9\tjoined\tstring\tin the loop at ret.doxa:11:5: lands in joined's arena, reclaimed when it returns; a block around the loop and the variable that keeps it reclaims it at its `}`\n");
    // What a block in the loop allocates is reclaimed every iteration.
    try testing.expect(std.mem.indexOf(u8, text, "\tscoped\tstring\tin the loop at") == null);

    // With a memtrace run, each row says what it held, and the root's growth
    // is named by site: three strings of 7 bytes into `log`.
    const traced = try doxaStdout(allocator, &tmp, slot, &.{ "run", "ret.doxa", "--emit-memory", "--instrument=memtrace" });
    defer allocator.free(traced);
    const joined_text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "ret" });
    defer allocator.free(joined_text);
    try expectContains(joined_text, "escapes into global `log`, kept until the program ends (this run: 21 B in 3 allocations, 21 B into the root arena)\n");
    try expectContains(joined_text, "  21 B\tret.doxa:5:9\trecord\tstore\n");

    // A side file only: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, retentionSource, "--emit-memory");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, retentionSource, "-O0");
    defer allocator.free(without);
    try testing.expectEqualStrings(without, with);
}

// ── Measure B: the copies plain-store-copies would add ──

const plainSource =
    \\struct P {
    \\    public x :: int,
    \\}
    \\
    \\function fresh() returns int {
    \\    var a :: int[] is [1, 2]
    \\    var s :: string is "hi"
    \\    var t :: string is s
    \\    return @length(a) + @length(t)
    \\}
    \\
    \\function shared() returns int {
    \\    var a :: int[] is [1, 2]
    \\    var b :: int[] is a
    \\    return @length(b)
    \\}
    \\
    \\function mutatedShare() returns int {
    \\    var a :: int[] is [1, 2]
    \\    var b :: int[] is a
    \\    @push(b, 3)
    \\    return @length(a)
    \\}
    \\
    \\function fromParam(p :: P) returns int {
    \\    var q :: P is p
    \\    q.x is 5
    \\    return q.x
    \\}
    \\
    \\function intoContainer() returns int {
    \\    var rows :: int[][] is []
    \\    var row :: int[] is [1]
    \\    for i while i < 3 do i += 1 {
    \\        @push(rows, row)
    \\    }
    \\    return @length(rows)
    \\}
    \\
    \\entry function main() {
    \\    @print("{fresh()} {shared()} {mutatedShare()} {fromParam($P { x is 1 })} {intoContainer()}\n")
    \\}
;

test "measure B: stores of a value another binding holds, the mutated ones, and how often they ran" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;
    const io = testing.io;
    try tmp.dir.writeFile(io, .{ .sub_path = "b.doxa", .data = plainSource });
    const slot: harness.Slot = try .claim(allocator, io, "probe");
    defer slot.release(allocator, io);

    const out = try doxaStdout(allocator, &tmp, slot, &.{ "run", "b.doxa", "--emit-memory", "--instrument=lines" });
    defer allocator.free(out);
    try testing.expectEqualStrings("4 2 3 5 3\n", out);
    const text = try doxaStdout(allocator, &tmp, slot, &.{ "report", "b" });
    defer allocator.free(text);

    // A fresh value, and a string (immutable), would not be copied.
    try testing.expect(std.mem.indexOf(u8, text, "\tfresh\tint[]\tbound to a variable") == null);
    try testing.expect(std.mem.indexOf(u8, text, "\tfresh\tstring\tbound to a variable") == null);
    // A value another binding holds would be; one mutated after would keep
    // its copy whatever an elision did.
    try expectContains(text, "    b.doxa:14:5\tshared\tint[]\tbound to a variable\tran 1 time\n");
    try expectContains(text, "  ! b.doxa:20:5\tmutatedShare\tint[]\tbound to a variable\tran 1 time\n");
    try expectContains(text, "  ! b.doxa:26:5\tfromParam\tP\tbound to a variable\tran 1 time\n");
    // Pushed every iteration: one site, three copies made.
    try expectContains(text, "    b.doxa:35:9\tintoContainer\tint[]\tstored into a container\tran 3 times\n");
    try expectContains(text, "B adds between 2 and 4 copies at 4 sites; in this run, between 2 and 6 copies made.\n");

    // A side file only: the IR is the same without the flag.
    const with = try emitIrFor(allocator, &tmp, plainSource, "--emit-memory");
    defer allocator.free(with);
    const without = try emitIrFor(allocator, &tmp, plainSource, "-O0");
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

    // A store through `^` is decided at run time: `fresh`'s new array lives
    // in its own arena, so each of the three calls copies it out; `keep`'s
    // already lives in the caller's, so it is kept.
    try expectContains(text, "  3 copied, 0 kept\ttrace.doxa:2:5\tfresh\n");
    try expectContains(text, "  0 copied, 1 kept\ttrace.doxa:6:5\tkeep\n");
    // `fresh` opens its arena once a call; the loop resets its body's twice
    // an iteration (at the top and before the step).
    try expectContains(text, "\t3 opened, 0 resets\ttrace.doxa:1:10\tfresh\n");
    try expectContains(text, "\t1 opened, 6 resets\ttrace.doxa:11:5\tmain\n");
    // Every byte the copies took is counted to the copy's own site, and
    // `fresh`'s literal is gone with each call: its peak is one call's.
    try expectContains(text, "\ttrace.doxa:2:5\tfresh\trehome\n");
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
    \\map Ages :: int returns int {
    \\    1 then 20,
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
    \\    Ages[2] is 5
    \\    @print("{dynamic[0]} {fixed[1]} {@length(dynamic)}")
    \\}
;

test "debug info: arrays, maps, unions and enumerations are described" {
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
    // A map is its header, with its length.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_structure_type, name: \"map\", ");
    // A union or group is the box, whose tag is the DoxaTag enumeration.
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_structure_type, name: \"value\", ");
    try expectContains(ll, "!DICompositeType(tag: DW_TAG_enumeration_type, name: \"tag\", ");
    try expectContains(ll, "!DIEnumerator(name: \"Map\", value: 10)");
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

/// The first `probe-*` file with extension `ext` in the output directory.
fn firstOutput(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, ext: []const u8) ![]u8 {
    // `-o <stem>` with no directory writes the binary, and its side files,
    // into `bin/`.
    var bin = try tmp.dir.openDir(testing.io, "bin", .{ .iterate = true });
    defer bin.close(testing.io);
    var it = bin.iterate();
    while (try it.next(testing.io)) |entry| {
        if (std.mem.endsWith(u8, entry.name, ext) and std.mem.startsWith(u8, entry.name, "probe-")) {
            return bin.readFileAlloc(testing.io, entry.name, allocator, .unlimited);
        }
    }
    return error.NoSuchOutput;
}

/// Compiles `source` for Linux with `flags` and returns the linked binary.
fn compileProbeLinux(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, flags: []const []const u8) ![]u8 {
    const all = try std.mem.concat(allocator, []const u8, &.{ flags, &.{ "--arch=x86_64", "--os=linux" } });
    defer allocator.free(all);
    const ll = try compileProbe(allocator, tmp, &.{.{ .path = "probe.doxa", .data = source }}, all, "ll");
    defer allocator.free(ll);
    return firstOutput(allocator, tmp, "");
}
