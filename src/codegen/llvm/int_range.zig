const std = @import("std");
const ir = @import("../hir/register/ir.zig");
const ArithOp = ir.ArithOp;
const HIRType = ir.HIRType;

// ---------------------------------------------------------------------------
// Phase D — value range facts, and the floored-arithmetic lowerings they pick
// ---------------------------------------------------------------------------
//
// Doxa's `//` and `%` are *floored* (`docs/syntax.md`), so they cannot be
// lowered to LLVM's truncating `sdiv` / `srem` without a sign correction. That
// correction is five or six extra instructions hanging off the division, and on
// a serial carry chain it *is* the critical path — which is why the `call` and
// `struct` benchmark canaries trail their C twins even though the C twins never
// pay it: C's `%` truncates, so clang emits a bare `srem` and stops.
//
// Two static facts retire that cost:
//
//   * A constant divisor needs no correction at all for `%`. See `planModulo`.
//   * A provably non-negative dividend makes truncating equal to flooring, for
//     both operators.
//
// The lattice below is deliberately tiny — a closed signed interval plus an
// exact-constant fact — because those are all the two consumers need. It is
// also the shape the two dormant Phase D switches will want once wired: D-1
// (overflow) needs an upper bound to prove an `add` cannot wrap, and D-3
// (bounds checks) needs an upper bound to prove an index is in range. Widening
// the lattice is then local; re-deriving it is not.
//
// Every operation is sound by construction: the unknown range is the whole of
// `i64`, and each operation either proves a subset or gives up and returns the
// whole. A producer that forgets to attach a range therefore loses an
// optimization, never correctness.

// ---------------------------------------------------------------------------
// Lattice
// ---------------------------------------------------------------------------

/// A closed signed interval over `i64`, plus an exact-constant fact.
///
/// `konst`, when present, is authoritative and the bounds are then redundant.
/// It exists because a floored `%` needs the divisor itself, not merely a
/// bound.
pub const IntRange = struct {
    lo: i64 = std.math.minInt(i64),
    hi: i64 = std.math.maxInt(i64),
    lo_known: bool = false,
    hi_known: bool = false,
    konst: ?i64 = null,

    /// The whole of `i64` — the top element, and the range of every value
    /// the pass proves nothing about.
    pub fn unknown() IntRange {
        return .{};
    }

    /// The single value `v`.
    pub fn exact(v: i64) IntRange {
        return .{ .lo = v, .hi = v, .lo_known = true, .hi_known = true, .konst = v };
    }

    /// The magnitude range `[0, limit)` — what a Doxa `byte` holds. A
    /// non-positive limit describes no values, so it yields the top element
    /// rather than an empty interval that would poison every later operation.
    pub fn below(limit: i64) IntRange {
        if (limit <= 0) return .unknown();
        return .{ .lo = 0, .hi = limit - 1, .lo_known = true, .hi_known = true };
    }

    pub fn exactConst(self: IntRange) ?i64 {
        return self.konst;
    }

    /// Structural equality, treating an exact constant as authoritative (its
    /// bounds are redundant). Used by the range fixpoint to detect stability.
    pub fn eql(a: IntRange, b: IntRange) bool {
        if (a.konst != null or b.konst != null) {
            return a.konst != null and b.konst != null and a.konst.? == b.konst.?;
        }
        return a.lo_known == b.lo_known and a.hi_known == b.hi_known and
            (!a.lo_known or a.lo == b.lo) and (!a.hi_known or a.hi == b.hi);
    }

    /// True when the value provably cannot be negative.
    pub fn isNonNegative(self: IntRange) bool {
        if (self.konst) |c| return c >= 0;
        return self.lo_known and self.lo >= 0;
    }

    /// True when the value provably cannot be zero. This is what discharges
    /// the divide-by-zero guard: a divisor that is a non-zero constant or is
    /// bounded strictly positive needs no runtime check.
    pub fn isNonZero(self: IntRange) bool {
        if (self.konst) |c| return c != 0;
        return self.lo_known and self.lo > 0;
    }

    /// True when the value provably cannot be positive.
    pub fn isNonPositive(self: IntRange) bool {
        if (self.konst) |c| return c <= 0;
        return self.hi_known and self.hi <= 0;
    }

    /// `a + b`, or the whole of `i64` if either end is open or an endpoint
    /// computation would overflow.
    pub fn add(a: IntRange, b: IntRange) IntRange {
        var out = IntRange{ .lo_known = a.lo_known and b.lo_known, .hi_known = a.hi_known and b.hi_known };
        if (out.lo_known) out.lo = std.math.add(i64, a.lo, b.lo) catch return .unknown();
        if (out.hi_known) out.hi = std.math.add(i64, a.hi, b.hi) catch return .unknown();
        if (out.lo_known and out.hi_known and out.lo > out.hi) return .unknown();
        return out;
    }

    /// `a - b`.
    pub fn sub(a: IntRange, b: IntRange) IntRange {
        var out = IntRange{ .lo_known = a.lo_known and b.hi_known, .hi_known = a.hi_known and b.lo_known };
        if (out.lo_known) out.lo = std.math.sub(i64, a.lo, b.hi) catch return .unknown();
        if (out.hi_known) out.hi = std.math.sub(i64, a.hi, b.lo) catch return .unknown();
        if (out.lo_known and out.hi_known and out.lo > out.hi) return .unknown();
        return out;
    }

    /// `a * b`. Only sign-definite cases are computed: the product of two
    /// intervals straddling zero is not an interval, so reporting its bounding
    /// rectangle would be a lie.
    pub fn mul(a: IntRange, b: IntRange) IntRange {
        if (a.konst == 0 or b.konst == 0) return .exact(0);
        if (a.isNonNegative() and b.isNonNegative()) return magnitudeProduct(a, b);
        if (a.isNonPositive() and b.isNonPositive()) return negate(magnitudeProduct(negate(a), negate(b)));
        if (a.isNonNegative() and b.isNonPositive()) return negate(magnitudeProduct(a, negate(b)));
        if (a.isNonPositive() and b.isNonNegative()) return negate(magnitudeProduct(negate(a), b));
        return .unknown();
    }

    /// The product of two non-negative ranges: `[0, a.hi * b.hi]`.
    fn magnitudeProduct(a: IntRange, b: IntRange) IntRange {
        if (!a.hi_known or !b.hi_known) return .{ .lo = 0, .lo_known = true };
        const hi = std.math.mul(i64, a.hi, b.hi) catch return .unknown();
        return .{ .lo = 0, .lo_known = true, .hi = hi, .hi_known = true };
    }

    /// `-a`.
    pub fn negate(a: IntRange) IntRange {
        if (a.konst) |c| {
            if (c == std.math.minInt(i64)) return .unknown();
            return .exact(-c);
        }
        var out = IntRange{ .lo_known = a.hi_known, .hi_known = a.lo_known };
        if (out.lo_known) out.lo = std.math.sub(i64, 0, a.hi) catch return .unknown();
        if (out.hi_known) out.hi = std.math.sub(i64, 0, a.lo) catch return .unknown();
        if (out.lo_known and out.hi_known and out.lo > out.hi) return .unknown();
        return out;
    }

    /// `a` floored-divided by the positive constant `c`. Floor division by a
    /// positive constant is monotone non-decreasing, so evaluating the two
    /// endpoints is exact rather than an over-approximation.
    pub fn flooredDivByConst(a: IntRange, c: i64) IntRange {
        if (c <= 0) return .unknown();
        var out = IntRange{ .lo_known = a.lo_known, .hi_known = a.hi_known };
        if (a.lo_known) out.lo = floorDiv(a.lo, c);
        if (a.hi_known) out.hi = floorDiv(a.hi, c);
        if (out.lo_known and out.hi_known and out.lo > out.hi) return .unknown();
        return out;
    }

    /// `a` floored-modded by the positive constant `c`. The result is the
    /// representative of the residue class in `[0, c)` — the fact that closes a
    /// bounded carry chain such as `(x + carry) % 65536`.
    pub fn flooredModByConst(a: IntRange, c: i64) IntRange {
        _ = a;
        if (c <= 0) return .unknown();
        return .below(c);
    }

    /// The join for a merge: what holds on *every* incoming value.
    ///
    /// A phi selects one of its incoming values, and a variable after a branch
    /// holds whichever branch ran — so the result is their *union*, and the
    /// sound bound is the convex hull. Intersecting would be wrong in both
    /// directions: it would report `[0, 9]` for a variable that one branch
    /// sets to `[10, 20]`, and — worse — it would let a loop's initial value
    /// pin the loop-carried one, so `var sum is 0` followed by an unanalyzable
    /// `sum = f(sum)` would still look like exactly zero.
    ///
    /// An end is reported only when every input agrees it is bounded, since the
    /// hull of a bounded and an unbounded range is unbounded.
    pub fn hull(a: IntRange, b: IntRange) IntRange {
        var out = IntRange{
            .lo_known = a.lo_known and b.lo_known,
            .hi_known = a.hi_known and b.hi_known,
        };
        if (out.lo_known) out.lo = @min(a.lo, b.lo);
        if (out.hi_known) out.hi = @max(a.hi, b.hi);
        // A constant survives only when both sides agree on it.
        if (a.konst != null and a.konst == b.konst) out.konst = a.konst;
        return out;
    }
};

/// Mathematical floor division, used only for endpoint reasoning and never
/// emitted. The divisor is asserted positive; the dividend may be
/// `minInt(i64)`, which `divFloor` cannot represent.
fn floorDiv(a: i64, b: i64) i64 {
    std.debug.assert(b > 0);
    // `@divTrunc` already rounds toward zero, so the floor is one lower
    // exactly when `a` is negative and not a multiple of `b`.
    const q = @divTrunc(a, b);
    return if (q > 0 or @rem(a, b) == 0) q else q - 1;
}

// ---------------------------------------------------------------------------
// Lowering selection
// ---------------------------------------------------------------------------

/// The static facts the floored-arithmetic lowerings are chosen from.
pub const SignFacts = struct {
    /// The divisor's exact value, when it is a compile-time constant.
    const_divisor: ?i64 = null,
    /// The dividend provably cannot be negative.
    lhs_nonneg: bool = false,
    /// The divisor provably cannot be negative.
    rhs_nonneg: bool = false,
    /// The divisor provably cannot be zero, so the division needs no guard.
    rhs_nonzero: bool = false,

    /// True when the divisor is statically known not to be zero, whether as an
    /// exact constant or from a strictly positive lower bound.
    pub fn divisorNonZero(f: SignFacts) bool {
        if (f.const_divisor) |c| return c != 0;
        return f.rhs_nonzero;
    }
};

/// Reads the facts off a pair of ranges.
pub fn signFacts(lhs: IntRange, rhs: IntRange) SignFacts {
    return .{
        .const_divisor = rhs.konst,
        .lhs_nonneg = lhs.isNonNegative(),
        .rhs_nonneg = rhs.isNonNegative(),
        .rhs_nonzero = rhs.isNonZero(),
    };
}

/// The value range of an integer `arith` result, for its type. The range
/// pass (`ranges.zig`) is its one caller. Division and modulo are only
/// compressed when the divisor is a known positive constant, matching the
/// floored lowerings the emitter selects on.
pub fn arithRange(op: ArithOp, operand_type: HIRType, lhs: IntRange, rhs: IntRange) IntRange {
    switch (operand_type) {
        .Int => switch (op) {
            .Add => return IntRange.add(lhs, rhs),
            .Sub => return IntRange.sub(lhs, rhs),
            .Mul => return IntRange.mul(lhs, rhs),
            .Div, .IntDiv => {
                if (rhs.konst) |c| {
                    if (c > 0) return IntRange.flooredDivByConst(lhs, c);
                }
                return .unknown();
            },
            .Mod => {
                if (rhs.konst) |c| {
                    if (c > 0) return IntRange.flooredModByConst(lhs, c);
                }
                return .unknown();
            },
            .Pow => return .unknown(),
        },
        .Byte => switch (op) {
            .Add, .Sub, .Mul => return IntRange.below(256),
            else => return .unknown(),
        },
        else => return .unknown(),
    }
}

/// How floored `%` is lowered. Every shape is exact; they differ in cost.
pub const ModShape = union(enum) {
    /// `urem dividend, m` for a constant magnitude `m`, where either `m` is a
    /// power of two or the dividend is provably non-negative.
    ///
    /// `urem` works on bit patterns, so the identity it computes is
    /// `(dividend + 2^64) mod m` for a negative dividend — which equals
    /// `|dividend| mod m` only when `2^64` is a multiple of `m`. That holds for
    /// a power of two, and trivially for a non-negative dividend whose own bit
    /// pattern is its magnitude (`minInt(i64)` included). Under either
    /// condition the result is the unique residue in `[0, m)` congruent to the
    /// dividend, which is what a positive divisor's floored modulo is — so
    /// this is a single `and` for a power-of-two `m` and a magic-multiply
    /// remainder otherwise.
    ///
    /// The power-of-two condition is what makes this unconditional. For a
    /// general `m` the same instruction is *wrong* for a negative dividend
    /// unless the dividend's range proves otherwise, so `planModulo` only
    /// selects it under one of the two conditions.
    urem_const: u64,
    /// The same `urem`, negated for a negative constant divisor and corrected
    /// so that an exact division yields zero rather than `-m`. Gated on the
    /// same condition as `urem_const`.
    urem_const_negated: u64,
    /// A bare `srem`. Truncating equals flooring exactly when the operands do
    /// not straddle zero — which needs *both* signs settled, not just the
    /// dividend's: a non-negative dividend against a negative divisor is
    /// `7 % -3`, truncated `1`, floored `-2`. So this shape requires
    /// `lhs_nonneg and rhs_nonneg`.
    srem_bare,
    /// `srem` plus the correction, testing the dividend's sign alone because
    /// the divisor's sign is already settled. Drops the sign-difference `xor`.
    fixup_dividend_sign,
    /// `srem` plus the correction on the sign difference of the two operands.
    fixup_general,
};

/// How floored `//` is lowered.
pub const DivShape = union(enum) {
    /// `(dividend - urem(dividend, 1 << k)) ashr k` for a positive
    /// power-of-two constant. The subtracted residue clears the low `k` bits of
    /// the dividend's unsigned pattern, leaving the largest multiple of `2^k`
    /// at or below the dividend, which the arithmetic shift then floors. Three
    /// instructions, no fixup; LLVM folds the first two into an `and`.
    shift_floor: u6,
    /// A bare `sdiv`, under the same condition as `ModShape.srem_bare`:
    /// truncating equals flooring only when the operands do not straddle zero.
    sdiv_bare,
    /// `sdiv` plus the correction, testing the dividend's sign alone.
    fixup_dividend_sign,
    /// `sdiv` plus the correction on the sign difference of the two operands.
    fixup_general,
};

/// A constant divisor's magnitude as an unsigned literal, and its sign.
/// `|minInt(i64)|` is `2^63`, which is representable as a `u64` and is a legal
/// `urem` divisor, so this is total.
fn divisorMagnitude(c: i64) struct { mag: u64, negative: bool } {
    if (c >= 0) return .{ .mag = @intCast(c), .negative = false };
    // `-(c + 1) + 1` avoids negating `minInt(i64)`.
    return .{ .mag = @as(u64, @intCast(-(c + 1))) + 1, .negative = true };
}

/// True when the constant magnitude `m` divides `2^64`, which is what makes
/// `urem` on bit patterns equal a magnitude remainder for every dividend. That
/// is exactly "m is a power of two", including `2^63` for `|minInt(i64)|`.
fn isPowerOfTwoMagnitude(m: u64) bool {
    return m != 0 and std.math.isPowerOfTwo(m);
}

/// `log2` of a positive constant divisor `c` when it is a power of two that
/// `shift_floor` can express. `2^63` is excluded because it is not a positive
/// `i64`.
fn powerOfTwoShift(c: i64) ?u6 {
    if (c <= 0) return null;
    const magnitude: u64 = @intCast(c);
    if (!std.math.isPowerOfTwo(magnitude)) return null;
    const k = @ctz(magnitude);
    if (k > 62) return null;
    return @intCast(k);
}

pub fn planModulo(f: SignFacts) ModShape {
    // A constant divisor pins the divisor's sign, which at minimum drops the
    // sign-difference `xor` from the general shape. It can also retire the
    // correction entirely — but only where `urem` computes the dividend's
    // magnitude residue, which needs `m` to divide `2^64` (a power of two) or
    // the dividend to be provably non-negative. A zero divisor is excluded
    // throughout: it has no defined result, and `urem x, 0` would name a
    // division the language does not perform.
    const constant: ?i64 = f.const_divisor;
    if (constant) |c| {
        if (c != 0) {
            const d = divisorMagnitude(c);
            if (isPowerOfTwoMagnitude(d.mag) or f.lhs_nonneg) {
                if (d.negative) return ModShape{ .urem_const_negated = d.mag };
                return ModShape{ .urem_const = d.mag };
            }
        }
    }
    // The constant is itself the sign fact, so derive the divisor's sign from
    // it rather than trusting the caller to have set both.
    const divisor_nonneg = f.rhs_nonneg or (constant != null and constant.? > 0);
    if (f.lhs_nonneg and divisor_nonneg) return .srem_bare;
    if (divisor_nonneg) return .fixup_dividend_sign;
    return .fixup_general;
}

pub fn planDiv(f: SignFacts) DivShape {
    // A power-of-two constant divisor needs no fact about the dividend: the
    // subtracted residue clears the low bits of the dividend's pattern, and
    // the arithmetic shift floors the result.
    if (f.const_divisor) |c| {
        if (powerOfTwoShift(c)) |k| return DivShape{ .shift_floor = k };
    }
    const divisor_nonneg = f.rhs_nonneg or (f.const_divisor != null and f.const_divisor.? > 0);
    if (f.lhs_nonneg and divisor_nonneg) return .sdiv_bare;
    if (divisor_nonneg) return .fixup_dividend_sign;
    return .fixup_general;
}

// ---------------------------------------------------------------------------
// Overflow (Phase D-1)
// ---------------------------------------------------------------------------
//
// Integer overflow is defined behaviour (`docs/performance.md` §1): checked
// modes trap, unchecked modes wrap. The check on a signed `add`, `sub` or
// `mul` is skipped when the operand ranges prove the result fits.

const Bounds = struct { lo: i64, hi: i64 };

/// The closed bounds a range guarantees, or `null` when an end is open. An
/// exact constant is authoritative.
fn boundsOf(r: IntRange) ?Bounds {
    if (r.konst) |k| return .{ .lo = k, .hi = k };
    if (r.lo_known and r.hi_known) return .{ .lo = r.lo, .hi = r.hi };
    return null;
}

/// True when `op` provably cannot overflow for any pair of values drawn from
/// the two ranges. Endpoints decide `add` and `sub` because both are monotone
/// in each operand; a product's extrema lie on the corners of the operand
/// rectangle, so the four corner products (computed in `i128`, which cannot
/// overflow for `i64` inputs) decide `mul`.
pub fn intArithCannotOverflow(op: ArithOp, a: IntRange, b: IntRange) bool {
    const ab = boundsOf(a) orelse return false;
    const bb = boundsOf(b) orelse return false;
    switch (op) {
        .Add => {
            _ = std.math.add(i64, ab.lo, bb.lo) catch return false;
            _ = std.math.add(i64, ab.hi, bb.hi) catch return false;
            return true;
        },
        .Sub => {
            _ = std.math.sub(i64, ab.lo, bb.hi) catch return false;
            _ = std.math.sub(i64, ab.hi, bb.lo) catch return false;
            return true;
        },
        .Mul => {
            const p0 = @as(i128, ab.lo) * @as(i128, bb.lo);
            const p1 = @as(i128, ab.lo) * @as(i128, bb.hi);
            const p2 = @as(i128, ab.hi) * @as(i128, bb.lo);
            const p3 = @as(i128, ab.hi) * @as(i128, bb.hi);
            const lo = @min(@min(p0, p1), @min(p2, p3));
            const hi = @max(@max(p0, p1), @max(p2, p3));
            return lo >= std.math.minInt(i64) and hi <= std.math.maxInt(i64);
        },
        else => return false,
    }
}
