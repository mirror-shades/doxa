const ir = @import("../../hir/register/ir.zig");
const int_range = @import("../int_range.zig");
const FunctionEmitter = @import("../function.zig").FunctionEmitter;
const Error = @import("../function.zig").Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;
const IntRange = int_range.IntRange;

// ── Numbers ──

pub fn arith(self: *FunctionEmitter, inst: *const ir.Inst, op: ir.ArithOp, lhs: ValueId, rhs: ValueId) Error!void {
    const l = self.name(lhs);
    const r = self.name(rhs);
    switch (self.doxaOf(lhs)) {
        .Int => {
            const lr = self.ranges[@intFromEnum(lhs)];
            const rr = self.ranges[@intFromEnum(rhs)];
            const result = switch (op) {
                .Add, .Sub, .Mul => try checkedArith(self, op, .Int, l, r, lr, rr),
                .WrapAdd, .WrapSub, .WrapMul => try self.tmp("{s} i64 {s}, {s}", .{ wrapping(op), l, r }),
                .IntDiv => try flooredDiv(self, l, r, int_range.signFacts(lr, rr)),
                .Mod => try flooredMod(self, l, r, int_range.signFacts(lr, rr)),
                .Div => blk: {
                    try divisorGuard(self, r, int_range.signFacts(lr, rr));
                    break :blk try self.tmp("sdiv i64 {s}, {s}", .{ l, r });
                },
                .Pow => blk: {
                    self.e.uses_int_pow = true;
                    break :blk try overflowChecked(self, "i64", try self.tmp("call {{ i64, i1 }} @doxa.int.pow(i64 {s}, i64 {s})", .{ l, r }));
                },
            };
            self.names[@intFromEnum(inst.result.?)] = result;
        },
        .Float => switch (op) {
            .Add => try self.def(inst, "fadd double {s}, {s}", .{ l, r }),
            .Sub => try self.def(inst, "fsub double {s}, {s}", .{ l, r }),
            .Mul => try self.def(inst, "fmul double {s}, {s}", .{ l, r }),
            .Div => try self.def(inst, "fdiv double {s}, {s}", .{ l, r }),
            .Mod => try self.def(inst, "frem double {s}, {s}", .{ l, r }),
            .Pow => try self.def(inst, "call double @llvm.pow.f64(double {s}, double {s})", .{ l, r }),
            .IntDiv => return self.e.faultf("floor-divides floats", .{}),
            .WrapAdd, .WrapSub, .WrapMul => return self.e.faultf("wraps float arithmetic", .{}),
        },
        // A byte is unsigned: leaving `[0, 255]` faults like `int` overflow.
        .Byte => switch (op) {
            .Add, .Sub, .Mul => self.names[@intFromEnum(inst.result.?)] = try checkedArith(self, op, .Byte, l, r, self.ranges[@intFromEnum(lhs)], self.ranges[@intFromEnum(rhs)]),
            .WrapAdd, .WrapSub, .WrapMul => try self.def(inst, "{s} i8 {s}, {s}", .{ wrapping(op), l, r }),
            .Div, .IntDiv => try self.def(inst, "udiv i8 {s}, {s}", .{ l, r }),
            .Mod => try self.def(inst, "urem i8 {s}, {s}", .{ l, r }),
            .Pow => {
                self.e.uses_byte_pow = true;
                const pair = try self.tmp("call {{ i8, i1 }} @doxa.byte.pow(i8 {s}, i8 {s})", .{ l, r });
                self.names[@intFromEnum(inst.result.?)] = try overflowChecked(self, "i8", pair);
            },
        },
        else => return self.e.faultf("arithmetic on a {s}", .{@tagName(self.doxaOf(lhs))}),
    }
}

/// The bare instruction a wrapping op lowers to: modular by definition.
pub fn wrapping(op: ir.ArithOp) []const u8 {
    return switch (op) {
        .WrapAdd => "add",
        .WrapSub => "sub",
        .WrapMul => "mul",
        else => unreachable,
    };
}

/// `add`, `sub` or `mul` on an `int` (signed `i64`) or a `byte` (unsigned
/// `i8`): bare when the ranges prove the result fits the type, else checked
/// with a fault on overflow or underflow. Wrapping is only ever explicit,
/// through `+%`, `-%` and `*%`.
pub fn checkedArith(self: *FunctionEmitter, op: ir.ArithOp, operand_type: HIRType, l: []const u8, r: []const u8, lr: IntRange, rr: IntRange) Error![]const u8 {
    const bare = switch (op) {
        .Add => "add",
        .Sub => "sub",
        .Mul => "mul",
        else => unreachable,
    };
    const sign, const width = switch (operand_type) {
        .Int => .{ "s", "i64" },
        .Byte => .{ "u", "i8" },
        else => unreachable,
    };
    if (int_range.arithCannotOverflow(op, operand_type, lr, rr)) {
        return self.tmp("{s} {s} {s}, {s}", .{ bare, width, l, r });
    }
    const pair = try self.tmp("call {{ {s}, i1 }} @llvm.{s}{s}.with.overflow.{s}({s} {s}, {s} {s})", .{ width, sign, bare, width, width, l, width, r });
    return overflowChecked(self, width, pair);
}

/// The value of a `{ width, i1 }` result-and-overflow `pair`, faulting
/// with `Integer overflow` when the flag is set.
pub fn overflowChecked(self: *FunctionEmitter, width: []const u8, pair: []const u8) Error![]const u8 {
    const result = try self.tmp("extractvalue {{ {s}, i1 }} {s}, 0", .{ width, pair });
    const overflowed = try self.tmp("extractvalue {{ {s}, i1 }} {s}, 1", .{ width, pair });
    const trap = try self.split("ovf.trap");
    const cont = try self.split("ovf.cont");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ overflowed, trap, cont });
    try self.enter(trap);
    try self.fault(.overflow, null, "0", "0");
    try self.enter(cont);
    return result;
}

/// Trap when a divisor the ranges do not prove non-zero is zero:
/// `sdiv`/`srem` by zero is undefined in LLVM.
pub fn divisorGuard(self: *FunctionEmitter, divisor: []const u8, facts: int_range.SignFacts) Error!void {
    if (facts.divisorNonZero()) return;
    const zero = try self.tmp("icmp eq i64 {s}, 0", .{divisor});
    const trap = try self.split("div0.trap");
    const cont = try self.split("div0.cont");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ zero, trap, cont });
    try self.enter(trap);
    try self.fault(.div_by_zero, null, "0", "0");
    try self.enter(cont);
}

pub fn correction(self: *FunctionEmitter, remainder: []const u8, dividend: []const u8, divisor: []const u8, dividend_sign_decided: bool) Error![]const u8 {
    const non_zero = try self.tmp("icmp ne i64 {s}, 0", .{remainder});
    const sign = if (dividend_sign_decided) dividend else try self.tmp("xor i64 {s}, {s}", .{ dividend, divisor });
    const negative = try self.tmp("icmp slt i64 {s}, 0", .{sign});
    return self.tmp("and i1 {s}, {s}", .{ non_zero, negative });
}

/// Floored `%` in the cheapest exact shape the facts allow
/// (`int_range.planModulo`).
pub fn flooredMod(self: *FunctionEmitter, dividend: []const u8, divisor: []const u8, facts: int_range.SignFacts) Error![]const u8 {
    switch (int_range.planModulo(facts)) {
        .urem_const => |mag| return self.tmp("urem i64 {s}, {d}", .{ dividend, mag }),
        .urem_const_negated => |mag| {
            const residue = try self.tmp("urem i64 {s}, {d}", .{ dividend, mag });
            const non_zero = try self.tmp("icmp ne i64 {s}, 0", .{residue});
            const adjust = try self.tmp("select i1 {s}, i64 {d}, i64 0", .{ non_zero, mag });
            return self.tmp("sub i64 {s}, {s}", .{ residue, adjust });
        },
        .srem_bare => {
            try divisorGuard(self, divisor, facts);
            return self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
        },
        .fixup_dividend_sign, .fixup_general => {
            try divisorGuard(self, divisor, facts);
            const residue = try self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
            const cond = try correction(self, residue, dividend, divisor, int_range.planModulo(facts) == .fixup_dividend_sign);
            const adjust = try self.tmp("select i1 {s}, i64 {s}, i64 0", .{ cond, divisor });
            return self.tmp("add i64 {s}, {s}", .{ residue, adjust });
        },
    }
}

/// Floored `//` (`int_range.planDiv`).
pub fn flooredDiv(self: *FunctionEmitter, dividend: []const u8, divisor: []const u8, facts: int_range.SignFacts) Error![]const u8 {
    switch (int_range.planDiv(facts)) {
        .shift_floor => |k| {
            const residue = try self.tmp("urem i64 {s}, {d}", .{ dividend, @as(u64, 1) << k });
            const cleared = try self.tmp("sub i64 {s}, {s}", .{ dividend, residue });
            return self.tmp("ashr i64 {s}, {d}", .{ cleared, k });
        },
        .sdiv_bare => {
            try divisorGuard(self, divisor, facts);
            return self.tmp("sdiv i64 {s}, {s}", .{ dividend, divisor });
        },
        .fixup_dividend_sign, .fixup_general => {
            try divisorGuard(self, divisor, facts);
            const quotient = try self.tmp("sdiv i64 {s}, {s}", .{ dividend, divisor });
            const residue = try self.tmp("srem i64 {s}, {s}", .{ dividend, divisor });
            const cond = try correction(self, residue, dividend, divisor, int_range.planDiv(facts) == .fixup_dividend_sign);
            const adjust = try self.tmp("zext i1 {s} to i64", .{cond});
            return self.tmp("sub i64 {s}, {s}", .{ quotient, adjust });
        },
    }
}

pub fn convert(self: *FunctionEmitter, inst: *const ir.Inst, operand: ValueId) Error!void {
    const from = self.doxaOf(operand);
    const to = self.f.typeOf(inst.result.?).doxa;
    const v = self.name(operand);
    switch (to) {
        .Float => switch (from) {
            .Int => try self.def(inst, "sitofp i64 {s} to double", .{v}),
            .Byte => try self.def(inst, "uitofp i8 {s} to double", .{v}),
            else => return self.e.faultf("converts a {s} to a float", .{@tagName(from)}),
        },
        .Int => switch (from) {
            .Byte => try self.def(inst, "zext i8 {s} to i64", .{v}),
            .Float => try self.def(inst, "fptosi double {s} to i64", .{v}),
            else => return self.e.faultf("converts a {s} to an int", .{@tagName(from)}),
        },
        // A value with no byte faults like byte arithmetic leaving `[0, 255]`.
        .Byte => switch (from) {
            .Int => {
                const range = self.ranges[@intFromEnum(operand)];
                const fits = range.isNonNegative() and (if (range.konst) |c| c <= 255 else range.hi_known and range.hi <= 255);
                if (!fits) try byteGuard(self, try self.tmp("icmp ugt i64 {s}, 255", .{v}));
                try self.def(inst, "trunc i64 {s} to i8", .{v});
            },
            // Truncated toward zero, so `(-1, 256)` has a byte; NaN fails
            // both ordered compares and an infinity one.
            .Float => {
                const above = try self.tmp("fcmp ogt double {s}, -1.0", .{v});
                const below = try self.tmp("fcmp olt double {s}, 256.0", .{v});
                const fits = try self.tmp("and i1 {s}, {s}", .{ above, below });
                try byteGuard(self, try self.tmp("xor i1 {s}, true", .{fits}));
                try self.def(inst, "fptoui double {s} to i8", .{v});
            },
            else => return self.e.faultf("converts a {s} to a byte", .{@tagName(from)}),
        },
        else => return self.e.faultf("converts to a {s}", .{@tagName(to)}),
    }
}

/// Fault with `Integer overflow` when `outside`, a conversion's verdict that
/// its operand has no byte, holds.
fn byteGuard(self: *FunctionEmitter, outside: []const u8) Error!void {
    const trap = try self.split("byte.trap");
    const cont = try self.split("byte.cont");
    try self.line("br i1 {s}, label %{s}, label %{s}", .{ outside, trap, cont });
    try self.enter(trap);
    try self.fault(.overflow, null, "0", "0");
    try self.enter(cont);
}

pub fn compare(self: *FunctionEmitter, inst: *const ir.Inst, op: ir.CompareOp, lhs: ValueId, rhs: ValueId) Error!void {
    const l = self.name(lhs);
    const r = self.name(rhs);
    const signed = [_][]const u8{ "eq", "ne", "slt", "sle", "sgt", "sge" };
    const unsigned = [_][]const u8{ "eq", "ne", "ult", "ule", "ugt", "uge" };
    const ordered = [_][]const u8{ "oeq", "une", "olt", "ole", "ogt", "oge" };
    const i = @intFromEnum(op);
    switch (self.doxaOf(lhs)) {
        .Int, .Enum => try self.def(inst, "icmp {s} i64 {s}, {s}", .{ signed[i], l, r }),
        .Byte => try self.def(inst, "icmp {s} i8 {s}, {s}", .{ unsigned[i], l, r }),
        .Tetra => try self.def(inst, "icmp {s} i2 {s}, {s}", .{ signed[i], l, r }),
        .Float => try self.def(inst, "fcmp {s} double {s}, {s}", .{ ordered[i], l, r }),
        .Nothing => try self.def(inst, "icmp {s} i1 0, 0", .{signed[i]}),
        .String => {
            const a = try self.stringParts(l);
            const b = try self.stringParts(r);
            switch (op) {
                .Eq => try self.def(inst, "call i1 @doxa_str_eq(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len }),
                .Ne => {
                    const eq = try self.tmp("call i1 @doxa_str_eq(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len });
                    try self.def(inst, "xor i1 {s}, true", .{eq});
                },
                // Byte-wise lexicographic (`docs/strings.md`).
                else => {
                    const order = try self.tmp("call i32 @doxa_str_cmp(ptr {s}, i64 {s}, ptr {s}, i64 {s})", .{ a.ptr, a.len, b.ptr, b.len });
                    try self.def(inst, "icmp {s} i32 {s}, 0", .{ signed[i], order });
                },
            }
        },
        else => |t| return self.e.faultf("compares {s} values", .{@tagName(t)}),
    }
}
