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

pub const BinaryExpressionHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) BinaryExpressionHandler {
        return .{ .generator = generator };
    }

    pub fn generateBinary(self: *BinaryExpressionHandler, bin: ast.Binary, should_pop_after_use: bool) ErrorList!void {
        const left_type = self.generator.inferTypeFromExpression(bin.left.?);
        const right_type = self.generator.inferTypeFromExpression(bin.right.?);

        // Special handling for equality/inequality with enum members lacking context.
        // If one side is an enum-typed expression (or a field whose declared type is an enum),
        // set enum context while lowering a bare EnumMember (e.g., `.NUMBER`) on the other side.
        if (bin.operator.type == .EQUALITY or bin.operator.type == .BANG_EQUAL) {
            var enum_ctx: ?[]const u8 = null;

            // Try to derive enum context from left expression via field resolution
            if (left_type == .Enum) {
                if (self.generator.resolveFieldAccessType(bin.left.?)) |res| {
                    if (res.custom_type_name) |ctn| enum_ctx = ctn;
                }
            }
            // If not found, try right expression
            if (enum_ctx == null and right_type == .Enum) {
                if (self.generator.resolveFieldAccessType(bin.right.?)) |res| {
                    if (res.custom_type_name) |ctn| enum_ctx = ctn;
                }
            }

            // Generate left with possible enum context if it is an EnumMember literal
            if (bin.left.?.data == .EnumMember and enum_ctx != null) {
                const prev = self.generator.current_enum_type;
                const ctx = enum_ctx.?;
                self.generator.current_enum_type = ctx;
                try self.generator.generateExpression(bin.left.?, true, should_pop_after_use);
                self.generator.current_enum_type = prev;
            } else {
                try self.generator.generateExpression(bin.left.?, true, should_pop_after_use);
            }

            // Generate right with possible enum context if it is an EnumMember literal
            if (bin.right.?.data == .EnumMember and enum_ctx != null) {
                const prev2 = self.generator.current_enum_type;
                const ctx2 = enum_ctx.?;
                self.generator.current_enum_type = ctx2;
                try self.generator.generateExpression(bin.right.?, true, should_pop_after_use);
                self.generator.current_enum_type = prev2;
            } else {
                try self.generator.generateExpression(bin.right.?, true, should_pop_after_use);
            }
        } else {
            try self.generator.generateExpression(bin.left.?, true, should_pop_after_use);
            try self.generator.generateExpression(bin.right.?, true, should_pop_after_use);
        }

        switch (bin.operator.type) {
            .PLUS => try self.handlePlusOperator(left_type, right_type, bin),
            .MINUS => try self.handleMinusOperator(left_type, right_type, bin),
            .ASTERISK => try self.handleMultiplyOperator(left_type, right_type, bin),
            .SLASH => try self.handleDivideOperator(left_type, right_type, bin),
            .MODULO => try self.handleModuloOperator(left_type, right_type, bin),
            .POWER => try self.handlePowerOperator(left_type, right_type, bin),
            .EQUALITY => try self.handleEqualityOperator(bin),
            .BANG_EQUAL => try self.handleInequalityOperator(bin),
            .LESS => try self.handleLessOperator(bin),
            .GREATER => try self.handleGreaterOperator(bin),
            .LESS_EQUAL => try self.handleLessEqualOperator(bin),
            .GREATER_EQUAL => try self.handleGreaterEqualOperator(bin),
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
                    .vm_offset = 0,
                    .condition_type = .Tetra,
                },
            });

            // False branch: pop left_val and push false
            try self.generator.instructions.append(.{ .Label = .{ .name = false_handle_label, .vm_address = 0 } });
            try self.generator.instructions.append(.Pop); // Pop the original left_val
            const false_idx = try self.generator.addConstant(HIRValue{ .tetra = TETRA_FALSE });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_FALSE }, .constant_id = false_idx } });
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

            // True branch: pop left_val, evaluate right
            try self.generator.instructions.append(.{ .Label = .{ .name = short_circuit_label, .vm_address = 0 } });
            try self.generator.instructions.append(.Pop); // Pop the original left_val
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

            // Merge point: stack has [false] or [right_val]
            try self.generator.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
        } else if (log.operator.type == .OR) {
            // Similar for OR but with inverted logic
            try self.generator.generateExpression(log.left, true, should_pop_after_use);
            try self.generator.generateExpression(log.right, true, should_pop_after_use);
            try self.generator.instructions.append(.{ .LogicalOp = .{ .op = .Or } });
            // Simple OR for now - TODO: add short-circuit optimization
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
                const operand_type = self.generator.inferTypeFromExpression(unary.right.?);
                const zero_value = switch (operand_type) {
                    .Int => HIRValue{ .int = 0 },
                    .Float => HIRValue{ .float = 0.0 },
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

    fn handlePlusOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        if (left_type == .String and right_type == .String) {
            try self.generator.instructions.append(.Swap);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
        } else if (left_type == .Array and right_type == .Array) {
            try self.generator.instructions.append(.ArrayConcat);
        } else {
            const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
            if (common_type != .Unknown) {
                try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = common_type } });
            } else {
                self.generator.reporter.reportCompileError(
                    bin.left.?.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use + operator between {s} and {s}",
                    .{ @tagName(left_type), @tagName(right_type) },
                );
                return ErrorList.TypeMismatch;
            }
        }
    }

    fn handleMinusOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
        if (common_type != .Unknown) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = common_type } });
        } else {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use - operator between {s} and {s}",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleMultiplyOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
        if (common_type != .Unknown) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = common_type } });
        } else {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use * operator between {s} and {s}",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleDivideOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
        if (common_type != .Unknown) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Div, .operand_type = common_type } });
        } else {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use / operator between {s} and {s}",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleModuloOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
        if (common_type != .Unknown) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mod, .operand_type = common_type } });
        } else {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use % operator between {s} and {s}",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handlePowerOperator(self: *BinaryExpressionHandler, left_type: HIRType, right_type: HIRType, bin: ast.Binary) !void {
        const common_type = self.generator.computeNumericCommonType(left_type, right_type, bin.operator.type);
        if (common_type != .Unknown) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = common_type } });
        } else {
            self.generator.reporter.reportCompileError(
                bin.left.?.base.location(),
                ErrorCode.TYPE_MISMATCH,
                "Cannot use ** operator between {s} and {s}",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleEqualityOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = operand_type } });
    }

    fn handleInequalityOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Ne, .operand_type = operand_type } });
    }

    fn handleLessOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Lt, .operand_type = operand_type } });
    }

    fn handleGreaterOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Gt, .operand_type = operand_type } });
    }

    fn handleLessEqualOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Le, .operand_type = operand_type } });
    }

    fn handleGreaterEqualOperator(self: *BinaryExpressionHandler, bin: ast.Binary) !void {
        const operand_type = self.generator.inferComparisonOperandType(bin.left.?, bin.right.?);
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Ge, .operand_type = operand_type } });
    }
};
