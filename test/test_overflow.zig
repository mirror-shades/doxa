const std = @import("std");
const testing = std.testing;

const overflow = @import("../src/codegen/llvmir/ir_printer/overflow.zig");
const int_range = @import("../src/codegen/llvmir/ir_printer/int_range.zig");
const ArithOp = @import("../src/codegen/hir/soxa_instructions.zig").ArithOp;
const harness = @import("harness.zig");

const IntRange = int_range.IntRange;

// ---------------------------------------------------------------------------
// The prove-away predicate
// ---------------------------------------------------------------------------

test "overflow: bounded operands prove the operation safe" {
    // `[0, 255] + [0, 255]` fits, as does its subtraction and product.
    const byte = IntRange.below(256);
    try testing.expect(overflow.intArithCannotOverflow(.Add, byte, byte));
    try testing.expect(overflow.intArithCannotOverflow(.Sub, byte, byte));
    try testing.expect(overflow.intArithCannotOverflow(.Mul, byte, byte));

    // An unknown operand proves nothing.
    try testing.expect(!overflow.intArithCannotOverflow(.Add, byte, IntRange.unknown()));
    try testing.expect(!overflow.intArithCannotOverflow(.Mul, IntRange.unknown(), IntRange.unknown()));
}

test "overflow: the extreme exact values are not proved safe" {
    const max = IntRange.exact(std.math.maxInt(i64));
    const one = IntRange.exact(1);
    try testing.expect(!overflow.intArithCannotOverflow(.Add, max, one));
    try testing.expect(!overflow.intArithCannotOverflow(.Sub, IntRange.exact(std.math.minInt(i64)), one));
    try testing.expect(!overflow.intArithCannotOverflow(.Mul, max, IntRange.exact(2)));
    // The zero product is the one `max * 0` case that is safe.
    try testing.expect(overflow.intArithCannotOverflow(.Mul, max, IntRange.exact(0)));
}

// ---------------------------------------------------------------------------
// End to end: the policy reaches the emitted IR
// ---------------------------------------------------------------------------

/// Compiles a snippet and returns the emitted (unoptimized) IR. `opt` selects
/// the overflow policy through the mode axis: `--opt=0` is `debug` (trap) and
/// `--opt=2` is `fast` (wrap).
fn emitIrFor(allocator: std.mem.Allocator, tmp: *std.testing.TmpDir, source: []const u8, opt: []const u8) ![]u8 {
    const doxa = try harness.doxaExePath(allocator);
    defer allocator.free(doxa);

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "probe.doxa", .data = source });
    try tmp.dir.createDirPath(testing.io, "cache");

    var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = cwd_buffer[0..try tmp.dir.realPath(testing.io, &cwd_buffer)];

    const argv = [_][]const u8{
        doxa, "compile", "probe.doxa", "-o", "probe", opt, "--cache-dir=cache",
    };
    const result = try harness.runCommandCapture(allocator, &argv, cwd, null);
    if (result.exit_code != 0) {
        std.debug.print("doxa compile failed ({d}):\n{s}\n{s}\n", .{ result.exit_code, result.stdout, result.stderr });
        return error.CommandFailed;
    }
    allocator.free(result.stdout);
    allocator.free(result.stderr);

    return tmp.dir.readFileAlloc(testing.io, "cache/probe.ll", allocator, .unlimited);
}

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
