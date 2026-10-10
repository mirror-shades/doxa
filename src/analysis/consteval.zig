//! The compile-time evaluator: what a constant expression computes.
//!
//! A *constant expression* is defined by the language, not by what any pass
//! manages to fold: literals, `const` bindings whose initializer is itself
//! constant, and operators and conversions (`@int`, `@float`, `@byte`) over
//! those. Everything else is "not a constant" — its own state, never a
//! stand-in value — and is left to run.
//!
//! The arithmetic here is the program's arithmetic, operation for operation:
//! `int` is checked, `byte` wraps mod 256, `float` is IEEE 754, `//` and `%`
//! are floored, and the tetra operators read the same truth tables codegen
//! emits. Where the program would trap — an `int` overflow, an integer
//! division by zero, a conversion with no value, text that names no number —
//! a constant expression has no value, and that is a compile error at the
//! operator (`report`). Text is read by the runtime's own parsers.
//!
//! Two callers: the analyzer, which `evaluate`s a `const` initializer and an
//! array size, and the constant folder, which applies `binary`, `unary` and
//! `convert` to operands it has already folded to literals.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenType = @import("../types/token.zig").TokenType;
const types = @import("../types/types.zig");
const TokenLiteral = types.TokenLiteral;
const Tetra = types.Tetra;
const Reporter = @import("../utils/reporting.zig").Reporter;
const Location = @import("../utils/reporting.zig").Location;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const rt = @import("../runtime/doxa_rt.zig");

/// Why a constant expression has no value.
pub const Fault = enum {
    /// An `int` result outside 64 bits.
    overflow,
    /// `//` or `%` by an integer zero.
    division_by_zero,
    /// `@int` of a float with no `int` value (out of range, `inf`, NaN).
    int_out_of_range,
    /// `@byte` of a number outside 0–255.
    byte_out_of_range,
    /// `@int`, `@float` or `@byte` of text that names no such number.
    unparsable,
};

/// One operation's result.
pub const Outcome = union(enum) {
    value: TokenLiteral,
    /// An operand or an operator outside the constant language.
    not_constant,
    fault: Fault,
};

/// A whole expression's result. A fault names the node it happened at.
pub const Evaluation = union(enum) {
    value: TokenLiteral,
    not_constant,
    fault: struct { fault: Fault, site: *const ast.Expr },
};

/// Report `fault` at `location` as a compile error.
pub fn report(reporter: *Reporter, location: Location, fault: Fault) void {
    switch (fault) {
        .overflow => reporter.reportCompileError(location, ErrorCode.ARITHMETIC_OVERFLOW, "integer overflow in a constant expression", .{}),
        .division_by_zero => reporter.reportCompileError(location, ErrorCode.CONSTANT_DIVISION_BY_ZERO, "division by zero in a constant expression", .{}),
        .int_out_of_range => reporter.reportCompileError(location, ErrorCode.INTEGER_VALUE_OUT_OF_RANGE, "@int of a float with no int value (out of range, inf or NaN) in a constant expression", .{}),
        .byte_out_of_range => reporter.reportCompileError(location, ErrorCode.BYTE_VALUE_OUT_OF_RANGE, "byte value out of range (must be 0-255) in a constant expression", .{}),
        .unparsable => reporter.reportCompileError(location, ErrorCode.UNPARSABLE_CONVERSION, "the text names no number of the converted type in a constant expression", .{}),
    }
}

/// The value of `expr`, if it is a constant expression. `names` answers for
/// what the walk cannot see itself:
///
/// - `constantOf(*const ast.Expr) ?TokenLiteral` — the value of the binding a
///   name expression reads, when that binding is a constant;
/// - `fixedLengthOf(*const ast.Expr) ?i64` — the length of a fixed-size array
///   an expression denotes.
pub fn evaluate(expr: *const ast.Expr, names: anytype) Evaluation {
    return switch (expr.data) {
        .Literal => |literal| .{ .value = literal },
        .Grouping => |inner| if (inner) |e| evaluate(e, names) else .not_constant,
        .Variable => if (names.constantOf(expr)) |value| .{ .value = value } else .not_constant,
        .Unary => |u| switch (evaluate(u.right.?, names)) {
            .value => |v| at(expr, unary(u.operator.type, v)),
            else => |other| other,
        },
        .Binary => |b| evaluateBinary(expr, b.operator.type, b.left.?, b.right.?, names),
        .Logical => |l| evaluateBinary(expr, l.operator.type, l.left, l.right, names),
        .InternalCall => |call| if (call.method.type == .LENGTH)
            if (names.fixedLengthOf(call.receiver)) |n| .{ .value = .{ .int = n } } else .not_constant
        else switch (evaluate(call.receiver, names)) {
            .value => |v| at(expr, convert(call.method.type, v)),
            else => |other| other,
        },
        else => .not_constant,
    };
}

fn evaluateBinary(expr: *const ast.Expr, op: TokenType, left: *const ast.Expr, right: *const ast.Expr, names: anytype) Evaluation {
    const l = switch (evaluate(left, names)) {
        .value => |v| v,
        else => |other| return other,
    };
    const r = switch (evaluate(right, names)) {
        .value => |v| v,
        else => |other| return other,
    };
    return at(expr, binary(op, l, r));
}

/// `outcome` as the evaluation of `expr`: a fault happened there.
fn at(expr: *const ast.Expr, outcome: Outcome) Evaluation {
    return switch (outcome) {
        .value => |v| .{ .value = v },
        .not_constant => .not_constant,
        .fault => |fault| .{ .fault = .{ .fault = fault, .site = expr } },
    };
}

// ── Operators ──

/// `l op r` for a binary arithmetic, comparison or tetra operator.
pub fn binary(op: TokenType, l: TokenLiteral, r: TokenLiteral) Outcome {
    return switch (op) {
        .PLUS, .MINUS, .ASTERISK, .SLASH, .DOUBLE_SLASH, .MODULO, .POWER => arithmetic(op, l, r),
        .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL, .EQUALITY, .BANG_EQUAL => compare(op, l, r),
        .AND, .OR, .XOR, .IFF, .NAND, .NOR, .IMPLIES => switch (l) {
            .tetra => |a| switch (r) {
                .tetra => |b| .{ .value = .{ .tetra = tetraBinary(op, a, b) } },
                else => .not_constant,
            },
            else => .not_constant,
        },
        else => .not_constant,
    };
}

/// `op v` for unary minus and tetra `not`.
pub fn unary(op: TokenType, v: TokenLiteral) Outcome {
    return switch (op) {
        .MINUS => switch (v) {
            .int => |i| if (std.math.negate(i)) |n| .{ .value = .{ .int = n } } else |_| .{ .fault = .overflow },
            .float => |f| .{ .value = .{ .float = -f } },
            .byte => |b| .{ .value = .{ .byte = 0 -% b } },
            else => .not_constant,
        },
        // `false` and `true` swap; `both` and `neither` are their own negation.
        .NOT, .BANG => switch (v) {
            .tetra => |t| .{ .value = .{ .tetra = switch (t) {
                .true => .false,
                .false => .true,
                .both, .neither => t,
            } } },
            else => .not_constant,
        },
        else => .not_constant,
    };
}

/// A conversion intrinsic (`@int`, `@float`, `@byte`) of a number or a
/// string. Text is read by the runtime's own parsers.
pub fn convert(method: TokenType, v: TokenLiteral) Outcome {
    return switch (method) {
        .TOINT => switch (v) {
            .int => .{ .value = v },
            .byte => |b| .{ .value = .{ .int = b } },
            .float => |f| if (rt.truncateToInt(f)) |i| .{ .value = .{ .int = i } } else .{ .fault = .int_out_of_range },
            .string => |text| if (rt.parseIntText(text)) |i| .{ .value = .{ .int = i } } else .{ .fault = .unparsable },
            else => .not_constant,
        },
        .TOFLOAT => switch (v) {
            .float => .{ .value = v },
            .int => |i| .{ .value = .{ .float = @floatFromInt(i) } },
            .byte => |b| .{ .value = .{ .float = @floatFromInt(b) } },
            .string => |text| if (rt.parseFloatText(text)) |f| .{ .value = .{ .float = f } } else .{ .fault = .unparsable },
            else => .not_constant,
        },
        .TOBYTE => switch (v) {
            .byte => .{ .value = v },
            .int => |i| if (std.math.cast(u8, i)) |b| .{ .value = .{ .byte = b } } else .{ .fault = .byte_out_of_range },
            .float => |f| if (std.math.cast(u8, rt.truncateToInt(f) orelse -1)) |b| .{ .value = .{ .byte = b } } else .{ .fault = .byte_out_of_range },
            .string => |text| if (rt.parseByteText(text)) |b| .{ .value = .{ .byte = b } } else .{ .fault = .unparsable },
            else => .not_constant,
        },
        else => .not_constant,
    };
}

/// Whether a tetra condition takes its branch: `true` and `both` hold.
pub fn holds(t: Tetra) bool {
    return t == .true or t == .both;
}

/// The operand kind an arithmetic or comparison operator works in: float
/// dominates, then int; two bytes stay byte (`docs/math.md`).
const Domain = enum { int, float, byte };

fn domainOf(l: TokenLiteral, r: TokenLiteral) ?Domain {
    const numeric = struct {
        fn kind(v: TokenLiteral) ?Domain {
            return switch (v) {
                .int => .int,
                .float => .float,
                .byte => .byte,
                else => null,
            };
        }
    };
    const a = numeric.kind(l) orelse return null;
    const b = numeric.kind(r) orelse return null;
    if (a == .float or b == .float) return .float;
    if (a == .int or b == .int) return .int;
    return .byte;
}

fn asFloat(v: TokenLiteral) f64 {
    return switch (v) {
        .int => |i| @floatFromInt(i),
        .byte => |b| @floatFromInt(b),
        .float => |f| f,
        else => unreachable,
    };
}

fn asInt(v: TokenLiteral) i64 {
    return switch (v) {
        .int => |i| i,
        .byte => |b| b,
        else => unreachable,
    };
}

fn arithmetic(op: TokenType, l: TokenLiteral, r: TokenLiteral) Outcome {
    const domain = domainOf(l, r) orelse return .not_constant;
    // `/` is float division whatever its operands.
    if (op == .SLASH) return .{ .value = .{ .float = asFloat(l) / asFloat(r) } };
    return switch (domain) {
        .float => .{
            .value = .{
                .float = switch (op) {
                    .PLUS => asFloat(l) + asFloat(r),
                    .MINUS => asFloat(l) - asFloat(r),
                    .ASTERISK => asFloat(l) * asFloat(r),
                    .POWER => std.math.pow(f64, asFloat(l), asFloat(r)),
                    // `//` and `%` take integers; analysis rejects a float operand.
                    else => return .not_constant,
                },
            },
        },
        .int => intArithmetic(op, asInt(l), asInt(r)),
        .byte => byteArithmetic(op, l.byte, r.byte),
    };
}

fn intArithmetic(op: TokenType, a: i64, b: i64) Outcome {
    const result: i64 = switch (op) {
        .PLUS => std.math.add(i64, a, b) catch return .{ .fault = .overflow },
        .MINUS => std.math.sub(i64, a, b) catch return .{ .fault = .overflow },
        .ASTERISK => std.math.mul(i64, a, b) catch return .{ .fault = .overflow },
        .DOUBLE_SLASH => blk: {
            if (b == 0) return .{ .fault = .division_by_zero };
            if (a == std.math.minInt(i64) and b == -1) return .{ .fault = .overflow };
            break :blk @divFloor(a, b);
        },
        .MODULO => blk: {
            if (b == 0) return .{ .fault = .division_by_zero };
            // The floored remainder by -1 is 0, including of minInt.
            if (b == -1) break :blk 0;
            break :blk @mod(a, b);
        },
        // `int ** int` is computed in float and truncated, as at run time;
        // a power with no int value overflows.
        .POWER => rt.truncateToInt(std.math.pow(f64, @floatFromInt(a), @floatFromInt(b))) orelse return .{ .fault = .overflow },
        else => return .not_constant,
    };
    return .{ .value = .{ .int = result } };
}

fn byteArithmetic(op: TokenType, a: u8, b: u8) Outcome {
    const result: u8 = switch (op) {
        .PLUS => a +% b,
        .MINUS => a -% b,
        .ASTERISK => a *% b,
        .DOUBLE_SLASH => if (b == 0) return .{ .fault = .division_by_zero } else a / b,
        .MODULO => if (b == 0) return .{ .fault = .division_by_zero } else a % b,
        .POWER => blk: {
            // Square-and-multiply mod 256, as `@doxa.byte.pow`.
            var base = a;
            var exp = b;
            var acc: u8 = 1;
            while (exp != 0) : (exp >>= 1) {
                if (exp & 1 != 0) acc *%= base;
                base *%= base;
            }
            break :blk acc;
        },
        else => return .not_constant,
    };
    return .{ .value = .{ .byte = result } };
}

fn compare(op: TokenType, l: TokenLiteral, r: TokenLiteral) Outcome {
    const order: ?std.math.Order = if (domainOf(l, r)) |domain| switch (domain) {
        // A comparison against NaN is unordered: only `!=` holds.
        .float => if (std.math.isNan(asFloat(l)) or std.math.isNan(asFloat(r))) null else std.math.order(asFloat(l), asFloat(r)),
        .int, .byte => std.math.order(asInt(l), asInt(r)),
    } else switch (l) {
        .tetra => |a| switch (r) {
            .tetra => |b| switch (op) {
                .EQUALITY, .BANG_EQUAL => if (a == b) .eq else .lt,
                else => return .not_constant,
            },
            else => return .not_constant,
        },
        else => return .not_constant,
    };
    const result = if (order) |o| switch (op) {
        .LESS => o == .lt,
        .LESS_EQUAL => o != .gt,
        .GREATER => o == .gt,
        .GREATER_EQUAL => o != .lt,
        .EQUALITY => o == .eq,
        .BANG_EQUAL => o != .eq,
        else => unreachable,
    } else op == .BANG_EQUAL;
    return .{ .value = .{ .tetra = if (result) .true else .false } };
}

// ── Tetra truth tables ──

/// A tetra's code in the compiled program: `false` 0, `true` 1, `both` 2,
/// `neither` 3.
pub fn tetraCode(t: Tetra) u2 {
    return switch (t) {
        .false => 0,
        .true => 1,
        .both => 2,
        .neither => 3,
    };
}

fn tetraOfCode(code: u2) Tetra {
    return switch (code) {
        0 => .false,
        1 => .true,
        2 => .both,
        3 => .neither,
    };
}

/// A binary tetra operator's truth table, indexed `[lhs code][rhs code]`.
pub const TruthTable = [4][4]u2;

/// The truth table of every binary tetra operator. First-order logic yields
/// `true` or `false`: an operator reads whether each operand holds (`both`
/// does, `neither` does not) and never makes a contradiction itself
/// (`docs/tetras.md`). Codegen emits these same tables, so a folded operator
/// and a run one cannot disagree.
pub const truth_tables = struct {
    pub const @"and" = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return a and b;
        }
    }.f);
    pub const @"or" = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return a or b;
        }
    }.f);
    pub const iff = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return a == b;
        }
    }.f);
    pub const xor = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return a != b;
        }
    }.f);
    pub const nand = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return !(a and b);
        }
    }.f);
    pub const nor = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return !(a or b);
        }
    }.f);
    pub const implies = truthTable(struct {
        fn f(a: bool, b: bool) bool {
            return !a or b;
        }
    }.f);
};

fn truthTable(comptime op: fn (bool, bool) bool) TruthTable {
    var table: TruthTable = undefined;
    for (0..4) |l| for (0..4) |r| {
        table[l][r] = @intFromBool(op(holds(tetraOfCode(@intCast(l))), holds(tetraOfCode(@intCast(r)))));
    };
    return table;
}

fn tetraBinary(op: TokenType, a: Tetra, b: Tetra) Tetra {
    const table = switch (op) {
        .AND => truth_tables.@"and",
        .OR => truth_tables.@"or",
        .XOR => truth_tables.xor,
        .IFF => truth_tables.iff,
        .NAND => truth_tables.nand,
        .NOR => truth_tables.nor,
        .IMPLIES => truth_tables.implies,
        else => unreachable,
    };
    return tetraOfCode(table[tetraCode(a)][tetraCode(b)]);
}

test "int arithmetic is checked" {
    try std.testing.expectEqual(Outcome{ .fault = .overflow }, binary(.PLUS, .{ .int = std.math.maxInt(i64) }, .{ .int = 1 }));
    try std.testing.expectEqual(Outcome{ .fault = .overflow }, binary(.DOUBLE_SLASH, .{ .int = std.math.minInt(i64) }, .{ .int = -1 }));
    try std.testing.expectEqual(Outcome{ .fault = .division_by_zero }, binary(.MODULO, .{ .int = 7 }, .{ .int = 0 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = 0 } }, binary(.MODULO, .{ .int = std.math.minInt(i64) }, .{ .int = -1 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = -4 } }, binary(.DOUBLE_SLASH, .{ .int = -7 }, .{ .int = 2 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = -1 } }, binary(.MODULO, .{ .int = 7 }, .{ .int = -2 }));
    try std.testing.expectEqual(Outcome{ .fault = .overflow }, unary(.MINUS, .{ .int = std.math.minInt(i64) }));
    try std.testing.expectEqual(Outcome{ .fault = .overflow }, binary(.POWER, .{ .int = std.math.maxInt(i64) }, .{ .int = 2 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = 1024 } }, binary(.POWER, .{ .int = 2 }, .{ .int = 10 }));
}

test "byte arithmetic wraps and float division follows IEEE 754" {
    try std.testing.expectEqual(Outcome{ .value = .{ .byte = 44 } }, binary(.PLUS, .{ .byte = 200 }, .{ .byte = 100 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .byte = 0 } }, binary(.POWER, .{ .byte = 2 }, .{ .byte = 8 }));
    try std.testing.expectEqual(Outcome{ .fault = .division_by_zero }, binary(.DOUBLE_SLASH, .{ .byte = 1 }, .{ .byte = 0 }));
    const inf = binary(.SLASH, .{ .int = 1 }, .{ .int = 0 });
    try std.testing.expect(std.math.isPositiveInf(inf.value.float));
}

test "conversions without a value fault" {
    try std.testing.expectEqual(Outcome{ .fault = .int_out_of_range }, convert(.TOINT, .{ .float = 1e300 }));
    try std.testing.expectEqual(Outcome{ .fault = .int_out_of_range }, convert(.TOINT, .{ .float = std.math.nan(f64) }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = -3 } }, convert(.TOINT, .{ .float = -3.9 }));
    try std.testing.expectEqual(Outcome{ .fault = .byte_out_of_range }, convert(.TOBYTE, .{ .int = 256 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .int = 1 } }, convert(.TOINT, .{ .string = "1" }));
    try std.testing.expectEqual(Outcome{ .fault = .unparsable }, convert(.TOINT, .{ .string = "abc" }));
    try std.testing.expectEqual(Outcome{ .fault = .unparsable }, convert(.TOBYTE, .{ .string = "300" }));
}

test "first-order tetra operators yield true or false" {
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .true } }, binary(.AND, .{ .tetra = .both }, .{ .tetra = .true }));
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .false } }, binary(.AND, .{ .tetra = .both }, .{ .tetra = .neither }));
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .true } }, binary(.IMPLIES, .{ .tetra = .neither }, .{ .tetra = .false }));
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .both } }, unary(.NOT, .{ .tetra = .both }));
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .true } }, binary(.BANG_EQUAL, .{ .float = std.math.nan(f64) }, .{ .float = 1 }));
    try std.testing.expectEqual(Outcome{ .value = .{ .tetra = .false } }, binary(.EQUALITY, .{ .float = std.math.nan(f64) }, .{ .float = std.math.nan(f64) }));
}
