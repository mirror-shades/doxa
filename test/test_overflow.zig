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
