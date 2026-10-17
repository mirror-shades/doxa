//! Type rules for operators: binary, unary, and logical expressions.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const unifyTypes = helpers.unifyTypes;
const getLocationFromBase = helpers.getLocationFromBase;
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;
const TypeRef = ast.TypeRef;

pub fn binary(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const bin = expr.data.Binary;
    const left_type = try infer_type.inferTypeFromExpr(self, bin.left.?);
    const right_type = try infer_type.inferTypeFromExpr(self, bin.right.?);
    // A failed operand already has its error; the operation is poison too.
    if (left_type.base == .Error or right_type.base == .Error) return;
    // `x == .Red`: a shorthand compared against an enum is its variant.
    if (bin.operator.type == .EQUALITY or bin.operator.type == .BANG_EQUAL) {
        helpers.contextualizeEnumMember(self, bin.left.?, left_type, right_type);
        helpers.contextualizeEnumMember(self, bin.right.?, right_type, left_type);
        const span = ast.SourceSpan{ .location = getLocationFromBase(expr.base) };
        _ = helpers.reportUndeclaredVariant(self, bin.left.?, left_type, right_type, span);
        _ = helpers.reportUndeclaredVariant(self, bin.right.?, right_type, left_type, span);
    }

    // A compound assignment's operation (`CompoundAssign`) is a `Binary`
    // holding the *compound* lexeme (`"/="`) beside the base operator's token
    // type (`SLASH`), so dispatch on the token type, which is authoritative.
    const op: []const u8 = switch (bin.operator.type) {
        .PLUS => "+",
        .MINUS => "-",
        .ASTERISK => "*",
        .PLUS_WRAP => "+%",
        .MINUS_WRAP => "-%",
        .ASTERISK_WRAP => "*%",
        .SLASH => "/",
        .DOUBLE_SLASH => "//",
        .MODULO => "%",
        .POWER => "**",
        .LESS => "<",
        .GREATER => ">",
        .LESS_EQUAL => "<=",
        .GREATER_EQUAL => ">=",
        .EQUALITY => "==",
        .BANG_EQUAL => "!=",
        else => bin.operator.lexeme,
    };

    // Arithmetic on a union is never well typed: the operand has to be
    // narrowed first. Checked ahead of the per-operator rules, which would
    // otherwise let a union sit beside a numeric operand, infer a result, and
    // defer the failure to codegen — where the union has already vanished from
    // the diagnostic.
    const is_arithmetic = std.mem.eql(u8, op, "+") or std.mem.eql(u8, op, "-") or
        std.mem.eql(u8, op, "*") or std.mem.eql(u8, op, "/") or
        std.mem.eql(u8, op, "//") or std.mem.eql(u8, op, "%") or
        std.mem.eql(u8, op, "**") or isWrapping(op);
    if (is_arithmetic and (left_type.base == .Union or right_type.base == .Union)) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.TYPE_MISMATCH,
            "Cannot use {s} operator on union type; narrow it with 'as' or match first",
            .{op},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    // An integer literal beside a byte is a byte: `b + 1` is byte
    // arithmetic, so `b += 1` keeps `b` a byte.
    if (is_arithmetic and !std.mem.eql(u8, op, "/")) {
        if (!helpers.adoptByteLiteral(self, bin.left.?, left_type, right_type) or
            !helpers.adoptByteLiteral(self, bin.right.?, right_type, left_type))
        {
            type_info.base = .Nothing;
            return;
        }
    }

    if (std.mem.eql(u8, op, "/")) {
        if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                "Division requires numeric operands, got {s}",
                .{@tagName(left_type.base)},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                "Division requires numeric operands, got {s}",
                .{@tagName(right_type.base)},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        type_info.* = .{ .base = .Float };
    } else if (std.mem.eql(u8, op, "//")) {
        const left_is_int_or_byte = left_type.base == .Int or left_type.base == .Byte;
        const right_is_int_or_byte = right_type.base == .Int or right_type.base == .Byte;

        if (!left_is_int_or_byte or !right_is_int_or_byte) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                "Integer division requires integer or byte operands",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }

        if (left_type.base == .Int or right_type.base == .Int) {
            type_info.* = .{ .base = .Int };
        } else {
            type_info.* = .{ .base = .Byte };
        }
    } else if (isWrapping(op)) {
        if ((left_type.base != .Int and left_type.base != .Byte) or
            (right_type.base != .Int and right_type.base != .Byte))
        {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                "Wrapping {s} requires integer or byte operands",
                .{op},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        if (left_type.base == .Int or right_type.base == .Int) {
            type_info.* = .{ .base = .Int };
        } else {
            type_info.* = .{ .base = .Byte };
        }
    } else if (std.mem.eql(u8, op, "%")) {
        if ((left_type.base != .Int and left_type.base != .Byte) or
            (right_type.base != .Int and right_type.base != .Byte))
        {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                "Modulo requires integer or byte operands",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        if (left_type.base == .Int or right_type.base == .Int) {
            type_info.* = .{ .base = .Int };
        } else {
            type_info.* = .{ .base = .Byte };
        }
    } else if (std.mem.eql(u8, op, "+")) {
        if (left_type.base == .String and right_type.base == .String) {
            type_info.* = .{ .base = .String };
        } else if (left_type.base == .Array and right_type.base == .Array) {
            // `xs + []`: an empty literal is the array the other side is.
            adoptEmptyArray(bin.left.?, left_type, right_type);
            adoptEmptyArray(bin.right.?, right_type, left_type);
            // Concatenation preserves the element type, so `(a + b)[i]`
            // still resolves through the `string`/`byte[]` barrier.
            const elem_src = left_type.array_type orelse right_type.array_type;
            if (elem_src) |src| {
                const elem = try ast.TypeInfo.createDefault(self.allocator);
                elem.* = src.*;
                if (left_type.array_type != null and right_type.array_type != null) {
                    try helpers.unifyTypes(self, elem, right_type.array_type.?, .{ .location = getLocationFromBase(expr.base) });
                }
                type_info.* = .{ .base = .Array, .array_type = elem };
            } else {
                type_info.* = .{ .base = .Array };
            }
        } else if ((left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte) and
            (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte))
        {
            if (left_type.base == .Float or right_type.base == .Float) {
                type_info.* = .{ .base = .Float };
            } else if (left_type.base == .Int or right_type.base == .Int) {
                type_info.* = .{ .base = .Int };
            } else {
                type_info.* = .{ .base = .Byte };
            }
        } else {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use + operator between {s} and {s}. Both operands must be the same type.",
                .{ @tagName(left_type.base), @tagName(right_type.base) },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
    } else if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
        if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
            (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
        {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                "Arithmetic requires numeric operands",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }

        if (left_type.base == .Float or right_type.base == .Float) {
            type_info.* = .{ .base = .Float };
        } else if (left_type.base == .Int or right_type.base == .Int) {
            type_info.* = .{ .base = .Int };
        } else {
            type_info.* = .{ .base = .Byte };
        }
    } else if (std.mem.eql(u8, op, "**")) {
        if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
            (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
        {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                "Power operator requires numeric operands",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }

        if (left_type.base == .Float or right_type.base == .Float) {
            type_info.* = .{ .base = .Float };
        } else if (left_type.base == .Int or right_type.base == .Int) {
            type_info.* = .{ .base = .Int };
        } else {
            type_info.* = .{ .base = .Byte };
        }
    } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
        std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
        std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!="))
    {
        const ordering = !(std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!="));
        const left = try comparand(self, left_type);
        const right = try comparand(self, right_type);
        // Exactly the comparisons codegen lowers: numbers with numbers and
        // strings with strings in every form; tetras, `nothing` and values of
        // one enum by equality only. A union or group compares through its
        // box (`boxedCompare`), which judges the members itself.
        const left_named = left_type.base == .Custom or left_type.base == .Enum;
        const right_named = right_type.base == .Custom or right_type.base == .Enum;
        const allowed = if (left == .boxed or right == .boxed)
            left_type.base == right_type.base or (left_named and right_named)
        else switch (left) {
            .number => right == .number,
            .string => right == .string,
            .tetra => right == .tetra and !ordering,
            .nothing => right == .nothing and !ordering,
            .enumeration => right == .enumeration and !ordering and helpers.typesEqual(self, left_type, right_type),
            .boxed => unreachable,
            .other => false,
        };
        if (!allowed) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.INCOMPATIBLE_TYPES_FOR_COMPARISON,
                "Cannot compare {s} with {s} using {s}",
                .{ @tagName(left_type.base), @tagName(right_type.base), op },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        type_info.* = .{ .base = .Tetra };
    } else {
        try unifyTypes(self, left_type, right_type, .{ .location = getLocationFromBase(expr.base) });
        type_info.* = left_type.*;
    }
}

/// Type an empty array literal `[]`, inferred with no element type, as the
/// array `other` is: the operand of a concatenation it is concatenated with.
fn adoptEmptyArray(operand: *const ast.Expr, operand_type: *ast.TypeInfo, other: *const ast.TypeInfo) void {
    if (operand.data != .Array or operand.data.Array.len != 0) return;
    if (operand_type.array_type != null) return;
    operand_type.array_type = other.array_type orelse return;
}

/// What a comparison operand is, for the comparison table in `binary`.
const Comparand = union(enum) {
    number,
    string,
    tetra,
    nothing,
    /// A value of an enum; null for a `.Variant` shorthand not yet typed.
    enumeration: ?TypeRef,
    /// A union or group value, compared through its box.
    boxed,
    other,
};

fn comparand(self: *SemanticAnalyzer, t: *const ast.TypeInfo) SemanticError!Comparand {
    return switch (t.base) {
        .Int, .Float, .Byte => .number,
        .String => .string,
        .Tetra => .tetra,
        .Nothing => .nothing,
        .Union => .boxed,
        .Enum, .Custom => {
            const custom = t.custom_type orelse return if (t.base == .Enum) .{ .enumeration = null } else .other;
            const ref = custom.resolved();
            const info = (try self.customType(ref)) orelse return .other;
            return switch (info.kind) {
                .Enum => .{ .enumeration = ref },
                .Group => .boxed,
                .Struct => .other,
            };
        },
        else => .other,
    };
}

pub fn unaryExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const unary = expr.data.Unary;
    const operand_type = try infer_type.inferTypeFromExpr(self, unary.right.?);
    type_info.* = operand_type.*;
    if (operand_type.base == .Error) return;
    // Preserve comptime-literal-ness through unary minus (value negated)
    // so negative literals stay comptime constants; clear it otherwise.
    const numeric = operand_type.base == .Int or operand_type.base == .Float or operand_type.base == .Byte;
    switch (unary.operator.type) {
        .MINUS => {
            if (!numeric) return unaryOperandError(self, expr, type_info, ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS, "Negation requires a numeric operand, got {s}", operand_type);
            // Preserve comptime-literal-ness through unary minus (value
            // negated) so negative literals stay comptime constants.
            if (operand_type.comptime_int) |v| type_info.comptime_int = -v;
        },
        .MINUS_WRAP => {
            if (operand_type.base != .Int and operand_type.base != .Byte) {
                return unaryOperandError(self, expr, type_info, ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS, "Wrapping -% requires an integer or byte operand, got {s}", operand_type);
            }
            type_info.comptime_int = null;
        },
        .NOT => {
            if (operand_type.base != .Tetra) return unaryOperandError(self, expr, type_info, ErrorCode.INVALID_OPERAND_TYPE, "`not` requires a tetra operand, got {s}", operand_type);
            type_info.comptime_int = null;
        },
        else => type_info.comptime_int = null,
    }
}

fn unaryOperandError(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo, code: []const u8, comptime fmt: []const u8, operand_type: *const ast.TypeInfo) void {
    self.reporter.reportCompileError(
        getLocationFromBase(expr.base),
        code,
        fmt,
        .{@tagName(operand_type.base)},
    );
    self.fatal_error = true;
    type_info.* = .{ .base = .Nothing };
}

pub fn logicalExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const logical = expr.data.Logical;
    const left_type = try infer_type.inferTypeFromExpr(self, logical.left);
    const right_type = try infer_type.inferTypeFromExpr(self, logical.right);
    if (left_type.base == .Error or right_type.base == .Error) return;

    if (left_type.base != .Tetra or right_type.base != .Tetra) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.INVALID_OPERAND_TYPE,
            "Logical operators require tetra operands",
            .{},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return;
    }

    type_info.* = .{ .base = .Tetra };
}

/// `+%`, `-%` or `*%`: arithmetic that wraps instead of faulting.
pub fn isWrapping(op: []const u8) bool {
    return std.mem.eql(u8, op, "+%") or std.mem.eql(u8, op, "-%") or std.mem.eql(u8, op, "*%");
}
