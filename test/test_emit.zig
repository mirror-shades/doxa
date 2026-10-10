//! IR-shape probes. Each compiles a small program through the installed
//! `doxa compile` and asserts on the IR the emitter chose. They drive the
//! binary, so they run with the program suites (`suites.zig`), never in a unit
//! root whose result is cached on its own executable.

const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");

/// Compiles `source` with `opt` and returns the emitted (unoptimized)
/// `<stem>.ll`. The unoptimized artifact is what shows the shape the emitter
/// chose rather than what LLVM made of it, so IR-shape probes assert on this.
fn emitIrFor(
    allocator: std.mem.Allocator,
    tmp: *std.testing.TmpDir,
    source: []const u8,
    opt: []const u8,
) ![]u8 {
    return compileProbe(allocator, tmp, &.{.{ .path = "probe.doxa", .data = source }}, &.{opt}, "ll");
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
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @llvm.trap()") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "ovf.trap.") != null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "ovf.cont.") != null);
}

test "emit: a fast build wraps and carries no trap" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const allocator = testing.allocator;

    const ir_text = try emitIrFor(allocator, &tmp, uncheckedAddSource, "--opt=2");
    defer allocator.free(ir_text);

    try testing.expect(std.mem.indexOf(u8, ir_text, "with.overflow") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "llvm.trap") == null);
    try testing.expect(std.mem.indexOf(u8, ir_text, "= add i64") != null);
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
        \\    for i while i < iters do i++ {
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
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @llvm.trap()") == null);
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
        \\    for i while i < 10 do i++ {{
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
        \\    for i while i < 10 do i++ {
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

    const ir_text = try emitIrFor(allocator, &tmp, descriptorFreeSource, "--opt=0");
    defer allocator.free(ir_text);

    // The scope-crossing store is the typed clone, decided statically, not
    // the descriptor walk or a runtime rehome.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call ptr @doxa_struct_clone_scalar(") != null);
    // Construction never registers it, and no copy consults the registry.
    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_struct_register(") == null);
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

    try testing.expect(std.mem.indexOf(u8, ir_text, "call void @doxa_struct_register(") != null);
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
