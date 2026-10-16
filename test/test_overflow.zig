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

    // An unknown operand proves nothing.
    try testing.expect(!overflow.arithCannotOverflow(.Add, .Int, byte, IntRange.unknown()));
    try testing.expect(!overflow.arithCannotOverflow(.Mul, .Int, IntRange.unknown(), IntRange.unknown()));
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
