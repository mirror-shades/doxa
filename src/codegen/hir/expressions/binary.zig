const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;
const TETRA_FALSE = @import("../soxa_generator.zig").TETRA_FALSE;
const TETRA_TRUE = @import("../soxa_generator.zig").TETRA_TRUE;
const CompareOp = @import("../soxa_instructions.zig").CompareOp;
const ArithOp = @import("../soxa_instructions.zig").ArithOp;

pub const BinaryExpressionHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) BinaryExpressionHandler {
        return .{ .generator = generator };
    }

    pub fn generateBinary(self: *BinaryExpressionHandler, expr: *ast.Expr, should_pop_after_use: bool) ErrorList!void {
        const bin = expr.data.Binary;
        try self.generator.generateExpression(bin.left.?, true, should_pop_after_use);
        try self.generator.generateExpression(bin.right.?, true, should_pop_after_use);

        switch (bin.operator.type) {
            .PLUS => switch (try self.generator.typeOf(expr)) {
                .String => {
                    try self.generator.instructions.append(.Swap);
                    try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
                },
                .Array => try self.generator.instructions.append(.ArrayConcat),
                else => try self.emitArith(expr, .Add),
            },
            .MINUS => try self.emitArith(expr, .Sub),
            .ASTERISK => try self.emitArith(expr, .Mul),
            .SLASH => try self.emitArith(expr, .Div),
            .DOUBLE_SLASH => try self.emitArith(expr, .IntDiv),
            .MODULO => try self.emitArith(expr, .Mod),
            .POWER => try self.emitArith(expr, .Pow),
            .EQUALITY => try self.emitComparison(bin, .Eq),
            .BANG_EQUAL => try self.emitComparison(bin, .Ne),
            .LESS => try self.emitComparison(bin, .Lt),
            .GREATER => try self.emitComparison(bin, .Gt),
            .LESS_EQUAL => try self.emitComparison(bin, .Le),
            .GREATER_EQUAL => try self.emitComparison(bin, .Ge),
            else => {
                self.generator.reporter.reportCompileError(
                    bin.left.?.base.location(),
                    ErrorCode.UNSUPPORTED_OPERATOR,
                    "Unsupported binary operator: {}",
                    .{bin.operator.type},
                );
                return ErrorList.UnsupportedOperator;
            },
        }
    }

    pub fn generateLogical(self: *BinaryExpressionHandler, log: ast.Logical, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        if (log.operator.type == .AND) {
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.instructions.append(.Dup);

            const short_circuit_label = try self.generator.generateLabel("and_short_circuit");
            const false_handle_label = try self.generator.generateLabel("and_false_handle");
            const end_label = try self.generator.generateLabel("and_end");

            // JumpCond pops the duplicate, so after jump:
            // - If true: stack has [left_val] (original)
            // - If false: stack has [left_val] (original)
            try self.generator.instructions.append(.{
                .JumpCond = .{
                    .label_true = short_circuit_label,
                    .label_false = false_handle_label,
                    .condition_type = .Tetra,
                },
            });

            // False branch: pop left_val and push false
            try self.generator.instructions.append(.{ .Label = .{ .name = false_handle_label } });
            try self.generator.instructions.append(.Pop); // Pop the original left_val
            const false_idx = try self.generator.addConstant(HIRValue{ .tetra = TETRA_FALSE });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_FALSE }, .constant_id = false_idx } });
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

            // True branch: pop left_val, evaluate right
            try self.generator.instructions.append(.{ .Label = .{ .name = short_circuit_label } });
            try self.generator.instructions.append(.Pop); // Pop the original left_val
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

            // Merge point: stack has [false] or [right_val]
            try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
        } else if (log.operator.type == .OR) {
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.instructions.append(.Dup);

            const short_circuit_label = try self.generator.generateLabel("or_short_circuit");
            const true_handle_label = try self.generator.generateLabel("or_true_handle");
            const end_label = try self.generator.generateLabel("or_end");

            // JumpCond pops the duplicate, so after jump:
            // - If true: stack has [left_val] (original) → short circuit, result is true
            // - If false: stack has [left_val] (original) → need to evaluate right
            try self.generator.instructions.append(.{
                .JumpCond = .{
                    .label_true = true_handle_label,
                    .label_false = short_circuit_label,
                    .condition_type = .Tetra,
                },
            });

            // True branch: pop left_val and push true
            try self.generator.instructions.append(.{ .Label = .{ .name = true_handle_label } });
            try self.generator.instructions.append(.Pop);
            const true_idx = try self.generator.addConstant(HIRValue{ .tetra = TETRA_TRUE });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_TRUE }, .constant_id = true_idx } });
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

            // False branch: pop left_val, evaluate right
            try self.generator.instructions.append(.{ .Label = .{ .name = short_circuit_label } });
            try self.generator.instructions.append(.Pop);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

            // Merge point: stack has [true] or [right_val]
            try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
        } else if (log.operator.type == .IFF) {
            // IFF (if and only if): A ↔ B - true when A and B have same truth value
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Iff } });
        } else if (log.operator.type == .XOR) {
            // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Xor } });
        } else if (log.operator.type == .NAND) {
            // NAND: A ↑ B - NOT(A AND B)
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Nand } });
        } else if (log.operator.type == .NOR) {
            // NOR: A ↓ B - NOT(A OR B)
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Nor } });
        } else if (log.operator.type == .IMPLIES) {
            // IMPLIES: A → B - NOT A OR B
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Implies } });
        } else {
            self.generator.reporter.reportCompileError(
                log.left.base.location(),
                ErrorCode.UNSUPPORTED_OPERATOR,
                "Unsupported logical operator: {}",
                .{log.operator.type},
            );
            return ErrorList.UnsupportedOperator;
        }
    }

    pub fn generateUnary(self: *BinaryExpressionHandler, unary: ast.Unary) (std.mem.Allocator.Error || ErrorList)!void {
        try self.generator.generateExpression(unary.right.?, true, false);

        switch (unary.operator.type) {
            .NOT => {
                try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Not } });
            },
            .MINUS => {
                const operand_type = try self.generator.typeOf(unary.right.?);
                // Float negation subtracts from -0.0, not 0.0: IEEE 754 gives
                // `0.0 - 0.0 = +0.0`, which would lose the sign of `-0.0`,
                // while `-0.0 - x` is `-x` for every x but NaN.
                const zero_value = switch (operand_type) {
                    .Int => HIRValue{ .int = 0 },
                    .Float => HIRValue{ .float = -0.0 },
                    .Byte => HIRValue{ .byte = 0 },
                    else => HIRValue{ .int = 0 }, // fallback
                };
                const zero_idx = try self.generator.addConstant(zero_value);
                try self.generator.instructions.append(.{ .Const = .{ .value = zero_value, .constant_id = zero_idx } });

                // Stack before: [..., operand, 0]
                // Swap -> [..., 0, operand]
                // Then subtract: 0 - operand = -operand
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });
            },
            .PLUS => {
                // Unary plus: just return the operand unchanged (no-op)
                // The operand is already on the stack
            },
            else => {
                const location = Location{
                    .file = unary.operator.file,
                    .file_uri = unary.operator.file_uri,
                    .range = .{
                        .start_line = unary.operator.line,
                        .start_col = unary.operator.column,
                        .end_line = unary.operator.line,
                        .end_col = unary.operator.column + unary.operator.lexeme.len,
                    },
                };
                self.generator.reporter.reportCompileError(
                    location,
                    ErrorCode.UNSUPPORTED_OPERATOR,
                    "Unsupported unary operator: {}",
                    .{unary.operator.type},
                );
                return ErrorList.UnsupportedOperator;
            },
        }
    }

    /// Arithmetic computes in the type analysis gave the expression: the
    /// operands' common numeric type, or `float` for `/`.
    fn emitArith(self: *BinaryExpressionHandler, expr: *ast.Expr, op: ArithOp) ErrorList!void {
        try self.generator.instructions.append(.{ .Arith = .{ .op = op, .operand_type = try self.generator.typeOf(expr) } });
    }

    /// How the two operands of a comparison are compared, decided from their
    /// analyzed types and nothing else.
    const Comparison = union(enum) {
        /// Both operands already share the representation `operand_type`
        /// names (after the numeric widening the emitter applies).
        direct: HIRType,
        /// One operand is a boxed union or group value and the other is a
        /// value of one of its members.
        boxed_member: struct {
            boxed_on_left: bool,
            /// Where the member sits among the members the box can hold.
            member_index: u32,
            member_type: HIRType,
        },
    };

    fn isBoxed(t: HIRType) bool {
        return t == .Union or t == .Group;
    }

    fn sameMemberType(a: HIRType, b: HIRType) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .Struct => |id| id == b.Struct,
            .Enum => |id| id == b.Enum,
            .Group => |id| id == b.Group,
            .Array => |inner| sameMemberType(inner.*, b.Array.*),
            else => true,
        };
    }

    /// Where `member` sits among the members `boxed` can hold, or null when
    /// it is not one of them.
    fn memberIndexIn(self: *BinaryExpressionHandler, boxed: HIRType, member: HIRType) ErrorList!?u32 {
        switch (boxed) {
            .Union => |u| {
                for (u.members, 0..) |candidate, idx| {
                    if (sameMemberType(candidate.*, member)) return @intCast(idx);
                }
                return null;
            },
            .Group => |gid| {
                const members = self.generator.semantic.group_table.members(gid) orelse return null;
                for (members, 0..) |candidate, idx| {
                    if (sameMemberType(try self.generator.type_system.typeForRef(candidate.ref), member)) return @intCast(idx);
                }
                return null;
            },
            else => return null,
        }
    }

    fn classifyComparison(self: *BinaryExpressionHandler, bin: ast.Binary, left: HIRType, right: HIRType) ErrorList!Comparison {
        if (isBoxed(left) != isBoxed(right)) {
            const boxed = if (isBoxed(left)) left else right;
            const member = if (isBoxed(left)) right else left;
            if (try self.memberIndexIn(boxed, member)) |member_index| {
                return .{ .boxed_member = .{ .boxed_on_left = isBoxed(left), .member_index = member_index, .member_type = member } };
            }
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot compare {s} with {s}: the {s} can never hold that type",
                .{ @tagName(left), @tagName(right), @tagName(boxed) },
            );
            return ErrorList.TypeMismatch;
        }
        if (isBoxed(left)) {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot compare two {s} values directly; narrow one side with 'as' or match first",
                .{@tagName(left)},
            );
            return ErrorList.TypeMismatch;
        }
        if (left == .Float or right == .Float) return .{ .direct = .Float };
        if (left == .Int or right == .Int) return .{ .direct = .Int };
        // Same type on both sides: the annotation is that type.
        return .{ .direct = left };
    }

    fn emitComparison(self: *BinaryExpressionHandler, bin: ast.Binary, op: CompareOp) ErrorList!void {
        const left = try self.generator.typeOf(bin.left.?);
        const right = try self.generator.typeOf(bin.right.?);
        switch (try self.classifyComparison(bin, left, right)) {
            .direct => |operand_type| {
                try self.generator.instructions.append(.{ .Compare = .{ .op = op, .operand_type = operand_type } });
            },
            .boxed_member => |bm| {
                if (op != .Eq and op != .Ne) {
                    self.generator.reporter.reportCompileError(
                        bin.left.?.base.location(),
                        ErrorCode.TYPE_MISMATCH,
                        "Cannot order a {s} value; narrow it with 'as' or match first",
                        .{@tagName(if (bm.boxed_on_left) left else right)},
                    );
                    return ErrorList.TypeMismatch;
                }
                try self.emitBoxedMemberEquality(bin, bm.boxed_on_left, bm.member_index, bm.member_type, op);
            },
        }
    }

    /// `box == member_value` for a box whose member is an enum or an int: equal
    /// when the box holds `member_type` (at `member_index`) and its payload
    /// word is the value's. A box holding another member is unequal whatever
    /// its payload, so `IOError.Denied` never equals `ParseError.Eof` though
    /// both are variant 1. The operands are on the stack in source order.
    fn emitBoxedMemberEquality(
        self: *BinaryExpressionHandler,
        bin: ast.Binary,
        boxed_on_left: bool,
        member_index: u32,
        member_type: HIRType,
        op: CompareOp,
    ) ErrorList!void {
        const g = self.generator;
        switch (member_type) {
            .Enum, .Int => {},
            else => {
                g.reporter.reportCompileError(
                    bin.left.?.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot compare a union or group value with a {s} directly; narrow it with 'as' or match first",
                    .{@tagName(member_type)},
                );
                return ErrorList.TypeMismatch;
            },
        }
        // Bring the box to the top. Equality does not care which side each
        // operand ended up on.
        if (boxed_on_left) try g.instructions.append(.Swap);

        const holds_label = try g.generateLabel("box_holds_member");
        const other_label = try g.generateLabel("box_holds_other");
        const end_label = try g.generateLabel("box_compare_end");
        try g.instructions.append(.Dup);
        try g.instructions.append(.{ .MemberCheck = .{ .members = try g.allocator.dupe(u32, &.{member_index}) } });
        try g.instructions.append(.{ .JumpCond = .{ .label_true = holds_label, .label_false = other_label, .condition_type = .Tetra } });

        // Another member: the answer is decided without the payload.
        try g.instructions.append(.{ .Label = .{ .name = other_label } });
        try g.instructions.append(.Pop);
        try g.instructions.append(.Pop);
        const decided: HIRValue = .{ .tetra = if (op == .Eq) TETRA_FALSE else TETRA_TRUE };
        try g.instructions.append(.{ .Const = .{ .value = decided, .constant_id = try g.addConstant(decided) } });
        try g.instructions.append(.{ .Jump = .{ .label = end_label } });

        // The member: strip the box and compare two plain words.
        try g.instructions.append(.{ .Label = .{ .name = holds_label } });
        try g.instructions.append(.{ .UnboxPayload = .{} });
        try g.instructions.append(.{ .Compare = .{ .op = op, .operand_type = member_type } });
        try g.instructions.append(.{ .Jump = .{ .label = end_label } });

        try g.instructions.append(.{ .Label = .{ .name = end_label } });
    }
};
