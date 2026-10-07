const std = @import("std");
const testing = std.testing;

const ir = @import("../src/codegen/llvmir/ir_printer/int_range.zig");

const IntRange = ir.IntRange;
const ModShape = ir.ModShape;
const DivShape = ir.DivShape;
const SignFacts = ir.SignFacts;

// ---------------------------------------------------------------------------
// The lattice
// ---------------------------------------------------------------------------

test "range: the top element admits every sign" {
    const any = IntRange.unknown();
    try testing.expect(!any.isNonNegative());
    try testing.expect(!any.isNonPositive());
    try testing.expectEqual(@as(?i64, null), any.exactConst());
}

test "range: an exact constant knows its own sign" {
    try testing.expect(IntRange.exact(0).isNonNegative());
    try testing.expect(IntRange.exact(7).isNonNegative());
    try testing.expect(!IntRange.exact(-7).isNonNegative());
    try testing.expect(IntRange.exact(-7).isNonPositive());
    try testing.expect(IntRange.exact(0).isNonPositive());
    try testing.expectEqual(@as(?i64, -7), IntRange.exact(-7).exactConst());
}

test "range: add and sub compose bounds" {
    const a = IntRange.below(256);
    const b = IntRange.below(256);
    const sum = IntRange.add(a, b);
    try testing.expect(sum.lo_known and sum.lo == 0);
    try testing.expect(sum.hi_known and sum.hi == 510);
    try testing.expect(sum.isNonNegative());

    const difference = IntRange.sub(a, b);
    try testing.expect(difference.lo_known and difference.lo == -255);
    try testing.expect(difference.hi_known and difference.hi == 255);
    try testing.expect(!difference.isNonNegative());
}

test "range: an unbounded operand makes the whole result unbounded" {
    const bounded = IntRange.below(10);
    const any = IntRange.unknown();
    // `below(10)` is `[0, 9]`, so adding "anything" can be as low as `minInt`
    // and as high as `maxInt + 9` — neither end is a bound worth reporting.
    try testing.expectEqual(IntRange.unknown(), IntRange.add(bounded, any));
    try testing.expectEqual(IntRange.unknown(), IntRange.add(any, bounded));
    // Subtraction borrows the opposite end, but an unknown operand has no end
    // to borrow, so it costs the result both.
    try testing.expectEqual(IntRange.unknown(), IntRange.sub(bounded, any));
    try testing.expectEqual(IntRange.unknown(), IntRange.sub(any, bounded));
    // A known operand keeps both ends, which is the case a bounded carry
    // chain relies on.
    const sum = IntRange.add(bounded, IntRange.exact(5));
    try testing.expectEqual(@as(i64, 5), sum.lo);
    try testing.expectEqual(@as(i64, 14), sum.hi);
}

test "range: an endpoint computation that would overflow widens to the top" {
    const huge = IntRange.below(std.math.maxInt(i64));
    // `[0, maxInt) + [0, maxInt)` cannot be bounded, so the top wins.
    try testing.expectEqual(IntRange.unknown(), IntRange.add(huge, huge));
    // `0 - (maxInt - 1)` *is* representable, so this one keeps its bound.
    const difference = IntRange.sub(IntRange.below(1), huge);
    try testing.expect(difference.lo_known);
    try testing.expectEqual(@as(i64, -std.math.maxInt(i64) + 1), difference.lo);
    // A non-positive `below` limit describes no values at all.
    try testing.expectEqual(IntRange.unknown(), IntRange.below(0));
    try testing.expectEqual(IntRange.unknown(), IntRange.below(-5));
}

test "range: multiplication is only exact for sign-definite operands" {
    try testing.expectEqual(@as(?i64, 0), IntRange.mul(IntRange.below(10), IntRange.exact(0)).exactConst());
    // A bounded product is a range, not a constant: the operands are ranges.
    const product = IntRange.mul(IntRange.below(8), IntRange.exact(3));
    try testing.expectEqual(@as(i64, 0), product.lo);
    try testing.expectEqual(@as(i64, 21), product.hi);
    try testing.expectEqual(@as(?i64, null), product.exactConst());
    // Two negative operands still have a bounded product: the magnitudes
    // multiply, and the sign makes the whole thing non-positive.
    const both_negative = IntRange.mul(
        .{ .lo = -8, .hi = -1, .lo_known = true, .hi_known = true },
        .{ .lo = -4, .hi = -1, .lo_known = true, .hi_known = true },
    );
    try testing.expect(both_negative.lo_known and both_negative.lo == -32);
    try testing.expect(both_negative.hi_known and both_negative.hi == 0);
    // Mixed signs are still bounded, as the reflected interval.
    const mixed = IntRange.mul(IntRange.below(8), IntRange.exact(-3));
    try testing.expect(mixed.lo_known and mixed.lo == -21);
    try testing.expect(mixed.hi_known and mixed.hi == 0);
    // An operand straddling zero: the product is not an interval, so the only
    // sound answer is the whole of `i64`.
    const straddling = IntRange{ .lo = -5, .hi = 5, .lo_known = true, .hi_known = true };
    try testing.expectEqual(IntRange.unknown(), IntRange.mul(straddling, IntRange.below(4)));
    try testing.expectEqual(IntRange.unknown(), IntRange.mul(straddling, straddling));
}

test "range: negating minInt is not representable and gives up" {
    const min = IntRange.exact(std.math.minInt(i64));
    try testing.expectEqual(IntRange.unknown(), IntRange.negate(min));
    try testing.expectEqual(@as(?i64, 7), IntRange.negate(IntRange.exact(-7)).exactConst());
}

test "range: floor division by a positive constant is exact at the endpoints" {
    const span = IntRange{ .lo = -7, .hi = 9, .lo_known = true, .hi_known = true };
    const by_two = IntRange.flooredDivByConst(span, 2);
    try testing.expectEqual(@as(i64, -4), by_two.lo);
    try testing.expectEqual(@as(i64, 4), by_two.hi);
    // A negative dividend floors *away* from zero.
    try testing.expectEqual(@as(i64, -1), IntRange.flooredDivByConst(IntRange.exact(-1), 2).lo);
    try testing.expectEqual(IntRange.unknown(), IntRange.flooredDivByConst(span, 0));
}

test "range: modulo by a positive constant pins the residue" {
    const residue = IntRange.flooredModByConst(IntRange.unknown(), 65536);
    try testing.expect(residue.lo_known and residue.lo == 0);
    try testing.expect(residue.hi_known and residue.hi == 65535);
    try testing.expect(residue.isNonNegative());
    // The dividend is irrelevant, so an unbounded one still yields a bound.
    try testing.expect(IntRange.flooredModByConst(IntRange.unknown(), 997).hi_known);
    try testing.expectEqual(IntRange.unknown(), IntRange.flooredModByConst(IntRange.unknown(), 0));
}

test "range: the hull is the merge join" {
    // A phi holds one of its incoming values, so the join is a union. An
    // intersection would report the empty range here.
    const low = IntRange{ .lo = 0, .hi = 9, .lo_known = true, .hi_known = true };
    const high = IntRange{ .lo = 10, .hi = 20, .lo_known = true, .hi_known = true };
    const joined = IntRange.hull(low, high);
    try testing.expectEqual(@as(i64, 0), joined.lo);
    try testing.expectEqual(@as(i64, 20), joined.hi);
    try testing.expect(IntRange.hull(high, low).hi_known);

    // Crucially, a loop's initial value must NOT pin the loop-carried one:
    // `var sum is 0` then `sum = f(sum)` with `f` unanalyzable leaves `sum`
    // unknown, not exactly zero.
    try testing.expect(!IntRange.hull(IntRange.exact(0), IntRange.unknown()).lo_known);
    try testing.expect(!IntRange.hull(IntRange.exact(0), IntRange.unknown()).hi_known);

    // A constant survives only when every side agrees on it.
    try testing.expectEqual(@as(?i64, 5), IntRange.hull(IntRange.exact(5), IntRange.exact(5)).exactConst());
    try testing.expectEqual(@as(?i64, null), IntRange.hull(IntRange.exact(5), IntRange.exact(6)).exactConst());
}

test "range: a bounded carry chain stays bounded" {
    // The `struct` canary's shape: every link is a floored modulo by the same
    // positive constant, so the chain must close on `[0, c)` from any seed —
    // and it does so without knowing anything about the array elements feeding
    // it, which is the whole point of the constant-divisor shape.
    const modulus = 65536;
    var carry: IntRange = .exact(1);
    var step: usize = 0;
    while (step < 8) : (step += 1) {
        // The array element is out of reach, so the sum is too.
        const next = IntRange.hull(IntRange.add(carry, IntRange.unknown()), IntRange.unknown());
        carry = IntRange.flooredModByConst(next, modulus);
        try testing.expect(carry.isNonNegative());
    }
    try testing.expect(carry.hi_known and carry.hi == modulus - 1);
}

// ---------------------------------------------------------------------------
// Lowering selection
// ---------------------------------------------------------------------------

test "plan: a negative constant divisor negates the residue" {
    try testing.expectEqual(ModShape{ .urem_const_negated = 2 }, ir.planModulo(.{ .const_divisor = -2 }));
    // |minInt| is 2^63, which is a legal `urem` divisor.
    try testing.expectEqual(
        ModShape{ .urem_const_negated = 1 << 63 },
        ir.planModulo(.{ .const_divisor = std.math.minInt(i64) }),
    );
}

test "plan: a zero divisor never becomes a constant-magnitude urem" {
    // `urem x, 0` would name a division the language does not define, so a
    // zero divisor must not take the constant path whatever else is known.
    for ([_]SignFacts{
        .{ .const_divisor = 0 },
        .{ .const_divisor = 0, .lhs_nonneg = true },
        .{ .const_divisor = 0, .rhs_nonneg = true },
    }) |facts| {
        switch (ir.planModulo(facts)) {
            .urem_const, .urem_const_negated => |mag| {
                std.debug.print("zero divisor produced urem by {d}\n", .{mag});
                return error.ZeroDivisorTookConstantPath;
            },
            else => {},
        }
    }
}

test "plan: a non-negative dividend alone is not enough to drop the correction" {
    // `7 % -3` truncates to 1 and floors to -2, so a non-negative dividend
    // against a divisor of unknown sign still needs the sign difference.
    try testing.expectEqual(ModShape.fixup_general, ir.planModulo(.{ .lhs_nonneg = true }));
    try testing.expectEqual(DivShape.fixup_general, ir.planDiv(.{ .lhs_nonneg = true }));
    // Only when both signs are settled does truncation equal flooring.
    try testing.expectEqual(
        ModShape.srem_bare,
        ir.planModulo(.{ .lhs_nonneg = true, .rhs_nonneg = true }),
    );
    try testing.expectEqual(
        DivShape.sdiv_bare,
        ir.planDiv(.{ .lhs_nonneg = true, .rhs_nonneg = true }),
    );
    // A non-negative *divisor* only removes the sign-difference xor.
    try testing.expectEqual(ModShape.fixup_dividend_sign, ir.planModulo(.{ .rhs_nonneg = true }));
    try testing.expectEqual(DivShape.fixup_dividend_sign, ir.planDiv(.{ .rhs_nonneg = true }));
    try testing.expectEqual(ModShape.fixup_general, ir.planModulo(.{}));
}

test "plan: a power-of-two constant divisor shifts, anything else falls back" {
    try testing.expectEqual(DivShape{ .shift_floor = 16 }, ir.planDiv(.{ .const_divisor = 65536 }));
    try testing.expectEqual(DivShape{ .shift_floor = 0 }, ir.planDiv(.{ .const_divisor = 1 }));
    try testing.expectEqual(DivShape{ .shift_floor = 62 }, ir.planDiv(.{ .const_divisor = 1 << 62 }));
    // 997 is not a power of two, so the constant only settles the divisor's
    // sign and the correction stays.
    try testing.expectEqual(DivShape.fixup_dividend_sign, ir.planDiv(.{ .const_divisor = 997 }));
    try testing.expectEqual(
        DivShape.sdiv_bare,
        ir.planDiv(.{ .const_divisor = 997, .lhs_nonneg = true }),
    );
}

test "plan: a constant modulus only takes the urem path when it is sound" {
    // `urem x, m` equals `|x| mod m` for every dividend only when `m` divides
    // `2^64`. 65536 does; 997 does not, so an unbounded dividend has to keep
    // the signed remainder plus a correction.
    try testing.expectEqual(ModShape{ .urem_const = 65536 }, ir.planModulo(.{ .const_divisor = 65536 }));
    try testing.expectEqual(ModShape{ .urem_const = 4 }, ir.planModulo(.{ .const_divisor = 4 }));
    try testing.expectEqual(ModShape{ .urem_const = 1 }, ir.planModulo(.{ .const_divisor = 1 }));
    // 2^63 is the magnitude of minInt and still divides 2^64.
    try testing.expectEqual(
        ModShape{ .urem_const_negated = 1 << 63 },
        ir.planModulo(.{ .const_divisor = std.math.minInt(i64) }),
    );

    try testing.expectEqual(ModShape.fixup_dividend_sign, ir.planModulo(.{ .const_divisor = 997 }));
    // A non-negative dividend rescues the general constant: `urem` on a
    // non-negative bit pattern is the value's own remainder.
    try testing.expectEqual(ModShape{ .urem_const = 997 }, ir.planModulo(.{ .const_divisor = 997, .lhs_nonneg = true }));
    try testing.expectEqual(
        ModShape{ .urem_const_negated = 997 },
        ir.planModulo(.{ .const_divisor = -997, .lhs_nonneg = true }),
    );
}

test "plan: sign facts are read off a pair of ranges" {
    const facts = ir.signFacts(IntRange.exact(5), IntRange.exact(3));
    try testing.expectEqual(@as(?i64, 3), facts.const_divisor);
    try testing.expect(facts.lhs_nonneg);
    try testing.expect(facts.rhs_nonneg);

    const open = ir.signFacts(IntRange.unknown(), IntRange.unknown());
    try testing.expectEqual(@as(?i64, null), open.const_divisor);
    try testing.expect(!open.lhs_nonneg);
    try testing.expect(!open.rhs_nonneg);
}

// ---------------------------------------------------------------------------
// What each shape computes
//
// The emitter writes IR text, so it cannot be executed here. These reference
// implementations are the same arithmetic the emitted instruction sequences
// perform, checked against Zig's own floored `@mod` / `@divFloor` — which is
// exactly the semantics `docs/syntax.md` promises. If a shape's math is wrong,
// it is wrong here first.
// ---------------------------------------------------------------------------

fn shapeMod(x: i64, y: i64, shape: ModShape) i64 {
    return switch (shape) {
        .urem_const => |mag| uremMagnitude(x, mag),
        .urem_const_negated => |mag| blk: {
            const residue = uremMagnitude(x, mag);
            const mag_i: i128 = @intCast(mag);
            const residue_i: i128 = residue;
            break :blk @intCast(residue_i - (if (residue == 0) @as(i128, 0) else mag_i));
        },
        .srem_bare => @rem(x, y),
        .fixup_dividend_sign => blk: {
            const residue = @rem(x, y);
            break :blk residue + (if (residue != 0 and x < 0) y else 0);
        },
        .fixup_general => blk: {
            const residue = @rem(x, y);
            const differing = (x < 0) != (y < 0);
            break :blk residue + (if (residue != 0 and differing) y else 0);
        },
    };
}

fn shapeDiv(x: i64, y: i64, shape: DivShape) i64 {
    return switch (shape) {
        .shift_floor => |k| blk: {
            // `(x - urem(x, 1 << k)) ashr k`, in i128 so the intermediate
            // subtraction cannot wrap in the reference model.
            const magnitude = @as(u128, 1) << @intCast(k);
            const residue: i128 = @intCast(@as(u64, @bitCast(x)) % magnitude);
            break :blk @intCast((@as(i128, x) - residue) >> @intCast(k));
        },
        .sdiv_bare => @divTrunc(x, y),
        .fixup_dividend_sign => blk: {
            const quotient = @divTrunc(x, y);
            break :blk quotient - @intFromBool(@rem(x, y) != 0 and x < 0);
        },
        .fixup_general => blk: {
            const quotient = @divTrunc(x, y);
            const differing = (x < 0) != (y < 0);
            break :blk quotient - @intFromBool(@rem(x, y) != 0 and differing);
        },
    };
}

/// `urem` on bit patterns, which is `|x| mod m` for every `x` — `minInt`
/// included, because its unsigned pattern is its own magnitude.
fn uremMagnitude(x: i64, m: u64) i64 {
    return @intCast(@as(u64, @bitCast(x)) % m);
}

const edge_values = [_]i64{
    0,
    1,
    -1,
    2,
    -2,
    3,
    -3,
    7,
    -7,
    8,
    -8,
    9,
    10,
    63,
    64,
    65,
    -63,
    -64,
    -65,
    996,
    997,
    998,
    -996,
    -997,
    -998,
    65535,
    65536,
    65537,
    -65535,
    -65536,
    -65537,
    std.math.maxInt(i64),
    std.math.maxInt(i64) - 1,
    std.math.minInt(i64),
    std.math.minInt(i64) + 1,
};

/// Divisors that exercise each shape: powers of two, primes, exact divisors,
/// and the sign combinations. Zero is excluded because both `srem` and `urem`
/// by zero are undefined, and the language has no defined result for it.
const edge_divisors = [_]i64{
    1,
    -1,
    2,
    -2,
    3,
    -3,
    4,
    -4,
    8,
    -8,
    9,
    -9,
    10,
    16,
    64,
    997,
    -997,
    1024,
    65536,
    -65536,
    1 << 31,
    -(1 << 31),
    1 << 62,
    std.math.maxInt(i64),
    std.math.maxInt(i64) - 1,
    std.math.minInt(i64) + 1,
};

/// `minInt(i64)` against `-1` has no representable `i64` result in either
/// direction, and Zig's own `@rem` / `@divTrunc` trap on it. The signed shapes
/// leave it as LLVM poison, which is the pre-existing behavior; the `urem`
/// shapes actually do better, and cover it. Neither is stateable here.
fn overflowsReference(x: i64, y: i64) bool {
    return x == std.math.minInt(i64) and y == -1;
}

test "shape: every modulo shape agrees with floored modulo under its own facts" {
    for (edge_values) |x| {
        for (edge_divisors) |y| {
            if (overflowsReference(x, y)) continue;
            // No facts at all: the general shape must hold for any dividend and
            // any divisor.
            try testing.expectEqual(@mod(x, y), shapeMod(x, y, ir.planModulo(.{})));
            // The constant path claims unconditional exactness, so it is only
            // held to it where the plan actually offers it.
            const const_shape = ir.planModulo(.{ .const_divisor = y });
            switch (const_shape) {
                .urem_const, .urem_const_negated => {
                    try testing.expectEqual(@mod(x, y), shapeMod(x, y, const_shape));
                },
                else => {},
            }
            // Truncation is only flooring when the operands do not straddle
            // zero, which takes *both* signs settled.
            if (x >= 0 and y > 0) {
                try testing.expectEqual(@mod(x, y), shapeMod(x, y, ir.planModulo(.{ .lhs_nonneg = true, .rhs_nonneg = true })));
            }
            // A non-negative divisor only needs the dividend's sign.
            if (y > 0) {
                try testing.expectEqual(@mod(x, y), shapeMod(x, y, ir.planModulo(.{ .rhs_nonneg = true })));
            }
        }
    }
}

test "shape: every division shape agrees with floored division under its own facts" {
    for (edge_values) |x| {
        for (edge_divisors) |y| {
            // `minInt / -1` overflows i64 and has no representable result; the
            // emitter leaves that as LLVM poison, so the reference model
            // cannot state one either.
            if (overflowsReference(x, y)) continue;
            try testing.expectEqual(@divFloor(x, y), shapeDiv(x, y, ir.planDiv(.{})));
            try testing.expectEqual(@divFloor(x, y), shapeDiv(x, y, ir.planDiv(.{ .const_divisor = y })));
            if (x >= 0 and y > 0) {
                try testing.expectEqual(@divFloor(x, y), shapeDiv(x, y, ir.planDiv(.{ .lhs_nonneg = true, .rhs_nonneg = true })));
            }
            if (y > 0) {
                try testing.expectEqual(@divFloor(x, y), shapeDiv(x, y, ir.planDiv(.{ .rhs_nonneg = true })));
            }
        }
    }
}

test "shape: the sign facts are load-bearing, not decorative" {
    // If the sign difference were ever dropped from the general shape, a
    // negative divisor would silently truncate. Pin that the bare and
    // dividend-sign-only shapes really do differ from the floored answer.
    try testing.expect(shapeMod(-7, 3, .srem_bare) != @mod(-7, 3));
    try testing.expectEqual(@as(i64, 2), @mod(-7, 3));
    try testing.expectEqual(@as(i64, -1), shapeMod(-7, 3, .srem_bare));
    try testing.expect(shapeDiv(-7, 3, .sdiv_bare) != @divFloor(-7, 3));
    try testing.expectEqual(@as(i64, -3), @divFloor(-7, 3));
    try testing.expectEqual(@as(i64, -2), shapeDiv(-7, 3, .sdiv_bare));

    // Testing only the dividend's sign is wrong whenever the divisor is
    // negative and the dividend is not: `7 % -3` floors to -2, but the
    // dividend-sign-only shape stops at the truncated 1.
    try testing.expect(shapeMod(7, -3, .fixup_dividend_sign) != @mod(7, -3));
    try testing.expectEqual(@as(i64, -2), @mod(7, -3));
    try testing.expectEqual(@as(i64, 1), shapeMod(7, -3, .fixup_dividend_sign));
    try testing.expectEqual(@as(i64, -2), shapeMod(7, -3, .fixup_general));
}

test "shape: floored division and modulo stay mutually consistent" {
    // `x == (x // y) * y + (x % y)` and `0 <= x % y < |y|` are the defining
    // properties, and they are what a constant-modulus carry chain relies on.
    for (edge_values) |x| {
        for (edge_divisors) |y| {
            if (overflowsReference(x, y)) continue;
            const facts: SignFacts = .{ .const_divisor = y };
            const quotient = shapeDiv(x, y, ir.planDiv(facts));
            const residue = shapeMod(x, y, ir.planModulo(facts));
            try testing.expectEqual(x, @as(i128, quotient) * @as(i128, y) + @as(i128, residue));
            if (y > 0) {
                try testing.expect(residue >= 0 and residue < y);
            } else {
                try testing.expect(residue <= 0 and residue > y);
            }
        }
    }
}

test "shape: the constant-divisor modulo needs no fact about the dividend only for a power of two" {
    // The unconditional claim: a power-of-two modulus works for any dividend,
    // because `2^64` is a multiple of it.
    const pow2: SignFacts = .{ .const_divisor = 65536 };
    const shape = ir.planModulo(pow2);
    try testing.expectEqual(ModShape{ .urem_const = 65536 }, shape);
    try testing.expectEqual(@as(i64, 65535), shapeMod(-1, 65536, shape));
    try testing.expectEqual(@as(i64, 0), shapeMod(65536, 65536, shape));
    // The residue is always the valid representative, even at the extremes.
    try testing.expectEqual(@as(i64, 0), shapeMod(std.math.minInt(i64), 65536, shape));
    try testing.expectEqual(@as(i64, 65535), shapeMod(std.math.maxInt(i64), 65536, shape));

    // A general modulus only gets there with a non-negative dividend, because
    // `urem` there is `(x + 2^64) mod m`, not `|x| mod m`. `-1 % 3` is the
    // witness: floored it is 2, while the bit-pattern remainder is 0.
    const prime: SignFacts = .{ .const_divisor = 3 };
    try testing.expectEqual(ModShape.fixup_dividend_sign, ir.planModulo(prime));
    try testing.expectEqual(@as(i64, 2), @mod(-1, 3));
    const bounded = ir.planModulo(.{ .const_divisor = 3, .lhs_nonneg = true });
    try testing.expectEqual(ModShape{ .urem_const = 3 }, bounded);
    // With the dividend's sign settled the bit-pattern remainder is the value's
    // own remainder, so `-1` is no longer in scope.
    try testing.expectEqual(@as(i64, 2), shapeMod(2, 3, bounded));
    try testing.expectEqual(@as(i64, 2), @mod(2, 3));
}
