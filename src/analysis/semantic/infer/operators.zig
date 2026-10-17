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

pub fn binary(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const bin = expr.data.Binary;
    const left_type = try infer_type.inferTypeFromExpr(self, bin.left.?);
    const right_type = try infer_type.inferTypeFromExpr(self, bin.right.?);
    // `x == .Red`: a shorthand compared against an enum is its variant.
    if (bin.operator.type == .EQUALITY or bin.operator.type == .BANG_EQUAL) {
        helpers.contextualizeEnumMember(self, bin.left.?, left_type, right_type);
        helpers.contextualizeEnumMember(self, bin.right.?, right_type, left_type);
        const span = ast.SourceSpan{ .location = getLocationFromBase(expr.base) };
        _ = helpers.reportUndeclaredVariant(self, bin.left.?, left_type, right_type, span);
        _ = helpers.reportUndeclaredVariant(self, bin.right.?, right_type, left_type, span);
    }

    // The parser rewrites a compound assignment on an index or field target into
    // an `IndexAssign`/`FieldAssignment` whose value is a `Binary` holding the
    // *compound* lexeme (`"/="`) beside the base operator's token type (`SLASH`) —
    // `precedence.zig` rewrites `.type` but copies `.lexeme` from the compound
    // token. Dispatching on the raw lexeme therefore matched none of the branches
    // below and fell through to the catch-all at the end of this switch, which
    // reports the left operand's type. So `a[i] /= 3` inferred as `Int`, and
    // storing a `float` into an `int[]` element slipped past the narrowing check
    // and truncated silently. Key off the token type, which is authoritative.
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
            // Concatenation preserves the element type, so `(a + b)[i]`
            // still resolves through the `string`/`byte[]` barrier.
            const elem_src = left_type.array_type orelse right_type.array_type;
            if (elem_src) |src| {
                const elem = try ast.TypeInfo.createDefault(self.allocator);
                elem.* = src.*;
                if (left_type.array_type != null and right_type.array_type != null) {
                    const start = self.reporter.diagnostics.items.len;
                    try helpers.unifyTypes(self, elem, right_type.array_type.?, .{ .location = getLocationFromBase(expr.base) });
                    if (self.reporter.firstErrorSince(start) != null) {
                        type_info.base = .Nothing;
                        return;
                    }
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
        const left_numeric = (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte);
        const right_numeric = (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte);

        if (left_numeric and right_numeric) {
            type_info.* = .{ .base = .Tetra };
        } else if (left_type.base == right_type.base) {
            type_info.* = .{ .base = .Tetra };
        } else if ((left_type.base == .Custom and right_type.base == .Enum) or
            (left_type.base == .Enum and right_type.base == .Custom))
        {
            type_info.* = .{ .base = .Tetra };
        } else {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.INCOMPATIBLE_TYPES_FOR_COMPARISON,
                "Cannot compare {s} with {s}",
                .{ @tagName(left_type.base), @tagName(right_type.base) },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
    } else {
        try unifyTypes(self, left_type, right_type, .{ .location = getLocationFromBase(expr.base) });
        type_info.* = left_type.*;
    }
}

pub fn unaryExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const unary = expr.data.Unary;
    const operand_type = try infer_type.inferTypeFromExpr(self, unary.right.?);
    type_info.* = operand_type.*;
    // Preserve comptime-literal-ness through unary minus (value negated)
    // so negative literals stay comptime constants; clear it otherwise.
    if (unary.operator.type == .MINUS) {
        if (operand_type.comptime_int) |v| {
            type_info.comptime_int = -v;
        }
    } else if (unary.operator.type == .MINUS_WRAP) {
        if (operand_type.base != .Int and operand_type.base != .Byte) {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                "Wrapping -% requires an integer or byte operand",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return;
        }
        type_info.comptime_int = null;
    } else {
        type_info.comptime_int = null;
    }
}

pub fn logicalExpr(self: *SemanticAnalyzer, expr: *ast.Expr, type_info: *ast.TypeInfo) SemanticError!void {
    const logical = expr.data.Logical;
    const left_type = try infer_type.inferTypeFromExpr(self, logical.left);
    const right_type = try infer_type.inferTypeFromExpr(self, logical.right);

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
