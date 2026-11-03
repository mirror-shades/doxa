const std = @import("std");
const testing = std.testing;

const overflow = @import("../src/codegen/llvm/int_range.zig");
const int_range = @import("../src/codegen/llvm/int_range.zig");

const IntRange = int_range.IntRange;

// ---------------------------------------------------------------------------
// The prove-away predicate
// ---------------------------------------------------------------------------

test "overflow: bounded operands prove the operation safe" {
    // `[0, 255] + [0, 255]` fits, as does its subtraction and product.
    const byte = IntRange.below(256);
    try testing.expect(overflow.arithCannotOverflow(.Add, .Int, byte, byte));
    try testing.expect(overflow.arithCannotOverflow(.Sub, .Int, byte, byte));
    try testing.expect(overflow.arithCannotOverflow(.Mul, .Int, byte, byte));

    // An operand unbounded at both ends can carry the result out of `i64`.
    try testing.expect(!overflow.arithCannotOverflow(.Add, .Int, byte, IntRange.unknown()));
    try testing.expect(!overflow.arithCannotOverflow(.Mul, .Int, IntRange.unknown(), IntRange.unknown()));
}

test "overflow: an end a range leaves open is the type's limit" {
    // A length is known only to be non-negative: `len - 1` fits, `len + 1`
    // does not.
    const len = IntRange{ .lo = 0, .lo_known = true };
    const one = IntRange.exact(1);
    try testing.expect(overflow.arithCannotOverflow(.Sub, .Int, len, one));
    try testing.expect(!overflow.arithCannotOverflow(.Add, .Int, len, one));
    // Nothing is known of `x`, yet `x * 1` and `x + 0` cannot leave `i64`;
    // `x - 1` can.
    try testing.expect(overflow.arithCannotOverflow(.Mul, .Int, IntRange.unknown(), one));
    try testing.expect(overflow.arithCannotOverflow(.Add, .Int, IntRange.unknown(), IntRange.exact(0)));
    try testing.expect(!overflow.arithCannotOverflow(.Sub, .Int, IntRange.unknown(), one));

    // A byte with an unknown range is still in `[0, 255]`: minus one can
    // underflow, but not when it is known to be at least one, and a byte at
    // most 127 plus 128 stays inside.
    try testing.expect(!overflow.arithCannotOverflow(.Sub, .Byte, IntRange.unknown(), one));
    try testing.expect(overflow.arithCannotOverflow(.Sub, .Byte, IntRange{ .lo = 1, .lo_known = true }, one));
    try testing.expect(overflow.arithCannotOverflow(.Add, .Byte, IntRange{ .hi = 127, .hi_known = true }, IntRange.exact(128)));
}

test "overflow: the extreme exact values are not proved safe" {
    const max = IntRange.exact(std.math.maxInt(i64));
    const one = IntRange.exact(1);
    try testing.expect(!overflow.arithCannotOverflow(.Add, .Int, max, one));
    try testing.expect(!overflow.arithCannotOverflow(.Sub, .Int, IntRange.exact(std.math.minInt(i64)), one));
    try testing.expect(!overflow.arithCannotOverflow(.Mul, .Int, max, IntRange.exact(2)));
    // The zero product is the one `max * 0` case that is safe.
    try testing.expect(overflow.arithCannotOverflow(.Mul, .Int, max, IntRange.exact(0)));
}

test "overflow: a byte result is proved only inside [0, 255]" {
    const small = IntRange.below(16);
    // `[0, 15] + [0, 15]` and `[0, 15] * [0, 15]` stay below 256.
    try testing.expect(overflow.arithCannotOverflow(.Add, .Byte, small, small));
    try testing.expect(overflow.arithCannotOverflow(.Mul, .Byte, small, small));
    // A full byte can overflow, and any subtraction of an unknown can underflow.
    const byte = IntRange.below(256);
    try testing.expect(!overflow.arithCannotOverflow(.Add, .Byte, byte, IntRange.exact(1)));
    try testing.expect(!overflow.arithCannotOverflow(.Sub, .Byte, IntRange.exact(0), IntRange.exact(1)));
    try testing.expect(overflow.arithCannotOverflow(.Sub, .Byte, IntRange.exact(5), small.clip(.below(6))));
}

test "overflow: a checked byte result carries its exact interval" {
    const r = int_range.arithRange(.Add, .Byte, IntRange.below(10), IntRange.exact(3));
    try testing.expect(r.lo == 3 and r.hi == 12);
    // An unproved product is still known to lie in the type.
    const full = int_range.arithRange(.Mul, .Byte, IntRange.below(256), IntRange.below(256));
    try testing.expect(full.lo == 0 and full.hi == 255);
}
