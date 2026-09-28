const std = @import("std");
const ArithOp = @import("../../hir/soxa_instructions.zig").ArithOp;
const IntRange = @import("int_range.zig").IntRange;

// ---------------------------------------------------------------------------
// Phase D-1 — integer overflow is defined behavior
// ---------------------------------------------------------------------------
//
// `docs/performance.md` §1 states the contract: overflow is *defined*, a runtime
// trap by default, because the language never pays for UB. This module is where
// that contract is spent on signed `i64` `add` / `sub` / `mul`.
//
// The policy is a single fact chosen from the compile mode (see
// `Opt.arithOverflow`): checked modes (`debug`, `safe`) trap, unchecked modes
// (`fast`, `small`) wrap the way C does. A trap is a real `llvm.trap` on the
// intrinsic's flagged path — never `unreachable`, which would turn overflow into
// UB and let LLVM fold the check away — and the check is skipped entirely when
// the operand ranges already prove the result fits in `i64`. The range lattice
// is the same one Phase D's floored arithmetic runs on.
//
// `byte` arithmetic is deliberately absent: a Doxa `byte` is unsigned, so its
// `i8` arithmetic is mod-256 by construction and has nothing to trap.

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;

    return struct {
        /// Emits signed integer `op` on two `i64` operands and returns the SSA
        /// name of the result.
        ///
        /// Under `.Wrap`, or when the operand ranges prove no overflow, this is
        /// the bare LLVM op. Under `.Trap` otherwise it is the
        /// `llvm.s<op>.with.overflow.i64` intrinsic followed by a real trap on
        /// the flagged path. A trap arm is a self-contained basic block; the
        /// caller's `current_block` is advanced to its continuation so later phi
        /// and range bookkeeping name the correct predecessor.
        pub fn emitIntArith(
            self: *IRPrinter,
            w: anytype,
            id: *usize,
            op: ArithOp,
            lhs: []const u8,
            rhs: []const u8,
            lhs_range: IntRange,
            rhs_range: IntRange,
            current_block: *[]const u8,
        ) ![]const u8 {
            if (self.arith_overflow != .Trap or intArithCannotOverflow(op, lhs_range, rhs_range)) {
                return emitWrappingIntArith(self, w, id, op, lhs, rhs);
            }
            return emitTrappingIntArith(self, w, id, op, lhs, rhs, current_block);
        }

        /// The bare wrapping op — the `.Wrap` lowering, and also the skipped
        /// shape when the ranges have already discharged the check.
        fn emitWrappingIntArith(
            self: *IRPrinter,
            w: anytype,
            id: *usize,
            op: ArithOp,
            lhs: []const u8,
            rhs: []const u8,
        ) ![]const u8 {
            const name = try self.nextTemp(id);
            const line = switch (op) {
                .Add => try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ name, lhs, rhs }),
                .Sub => try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ name, lhs, rhs }),
                .Mul => try std.fmt.allocPrint(self.allocator, "  {s} = mul i64 {s}, {s}\n", .{ name, lhs, rhs }),
                else => unreachable,
            };
            defer self.allocator.free(line);
            try w.writeAll(line);
            return name;
        }

        /// `with.overflow` + a diamond whose taken arm is a real trap. The
        /// intrinsic computes the wrapping result and an `i1` flag in one
        /// instruction; only the flag's branch is new control flow.
        fn emitTrappingIntArith(
            self: *IRPrinter,
            w: anytype,
            id: *usize,
            op: ArithOp,
            lhs: []const u8,
            rhs: []const u8,
            current_block: *[]const u8,
        ) ![]const u8 {
            const intrinsic = switch (op) {
                .Add => "sadd",
                .Sub => "ssub",
                .Mul => "smul",
                else => unreachable,
            };

            const pair = try self.nextTemp(id);
            const pair_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call {{ i64, i1 }} @llvm.{s}.with.overflow.i64(i64 {s}, i64 {s})\n",
                .{ pair, intrinsic, lhs, rhs },
            );
            defer self.allocator.free(pair_line);
            try w.writeAll(pair_line);

            const result = try self.nextTemp(id);
            const result_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = extractvalue {{ i64, i1 }} {s}, 0\n",
                .{ result, pair },
            );
            defer self.allocator.free(result_line);
            try w.writeAll(result_line);

            const overflow = try self.nextTemp(id);
            const overflow_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = extractvalue {{ i64, i1 }} {s}, 1\n",
                .{ overflow, pair },
            );
            defer self.allocator.free(overflow_line);
            try w.writeAll(overflow_line);

            // The label names consume the shared counter only to stay unique;
            // as named values they do not participate in LLVM's unnamed-value
            // numbering.
            const trap_label = try std.fmt.allocPrint(self.allocator, "ovf.trap.{d}", .{id.*});
            id.* += 1;
            const cont_label = try std.fmt.allocPrint(self.allocator, "ovf.cont.{d}", .{id.*});
            id.* += 1;

            const branch_line = try std.fmt.allocPrint(
                self.allocator,
                "  br i1 {s}, label %{s}, label %{s}\n",
                .{ overflow, trap_label, cont_label },
            );
            defer self.allocator.free(branch_line);
            try w.writeAll(branch_line);

            // TODO(D-1): the trap carries no source location. Reporting the
            // failing expression the way `unreachable` does needs a location
            // threaded onto `Arith` from the generator, which the instruction
            // does not carry today.
            const trap_block = try std.fmt.allocPrint(
                self.allocator,
                "\n{s}:\n  call void @llvm.trap()\n  unreachable\n\n{s}:\n",
                .{ trap_label, cont_label },
            );
            defer self.allocator.free(trap_block);
            try w.writeAll(trap_block);

            // `cont_label` becomes the block the rest of this instruction
            // stream is emitted into, so it must outlive this call; the arena
            // allocator is not freed here.
            self.current_block = cont_label;
            current_block.* = cont_label;
            return result;
        }
    };
}

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
