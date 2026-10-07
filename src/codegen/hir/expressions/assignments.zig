const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const SoxaTypes = @import("../soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const ScopeKind = SoxaTypes.ScopeKind;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Handle assignment operations: regular assignment, compound assignment
pub const AssignmentsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) AssignmentsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for assignment expressions
    pub fn generateAssignment(self: *AssignmentsHandler, expr: *ast.Expr, preserve_result: bool) !void {
        const assign = expr.data.Assignment;
        const value = assign.value.?;
        try self.generator.generateExpression(value, true, false);

        // The value takes the type of the storage it lands in, which the
        // analyzer resolved beneath any narrowing of the name.
        const assigned_type = try self.generator.bindingTypeOf(&expr.base);
        try self.generator.convertValue(try self.generator.typeOf(value), assigned_type);

        // TODO(type-authority step 5): the name-keyed type tables below are
        // read by the generator-side inference that step deletes.
        if (self.generator.getTrackedVariableType(assign.name.lexeme) == null) {
            try self.generator.trackVariableType(assign.name.lexeme, assigned_type);
        }
        if (assigned_type == .Array) {
            var storage_kind: ArrayStorageKind = .dynamic;
            if (value.data == .Variable) {
                if (self.generator.getTrackedArrayStorageKind(value.data.Variable.lexeme)) |tracked| {
                    storage_kind = tracked;
                }
            } else if (self.generator.array_storage_override) |override_kind| {
                storage_kind = override_kind;
            }
            try self.generator.trackArrayStorageKind(assign.name.lexeme, storage_kind);
        }

        // Duplicate value to leave it on stack as assignment result
        if (preserve_result) {
            try self.generator.instructions.append(.Dup);
        }
        try self.generator.storeName(&expr.base, assign.name.lexeme, assigned_type, .rehome);
    }

    /// Generate HIR for compound assignment expressions
    pub fn generateCompoundAssign(self: *AssignmentsHandler, expr: *ast.Expr, preserve_result: bool) !void {
        const compound = expr.data.CompoundAssign;
        try self.generator.loadName(&expr.base, compound.name.lexeme);

        // Generate the value expression (e.g., the "1" in "current += 1")
        try self.generator.generateExpression(compound.value.?, true, false);

        // The operation computes in what the name reads here — a narrowed
        // member inside a view — and its result is stored as the slot's type.
        const left_type = try self.generator.bindingReadTypeOf(&expr.base);
        const right_type = try self.generator.typeOf(compound.value.?);
        switch (compound.operator.type) {
            .PLUS_EQUAL => {
                try self.handlePlusEqual(left_type, right_type, compound.name);
            },
            .MINUS_EQUAL => {
                try self.handleMinusEqual(left_type, right_type, compound.name);
            },
            .ASTERISK_EQUAL => {
                try self.handleMultiplyEqual(left_type, right_type, compound.name);
            },
            .SLASH_EQUAL => {
                try self.handleDivideEqual(left_type, right_type, compound.name);
            },
            .DOUBLE_SLASH_EQUAL => {
                try self.handleIntDivEqual(left_type, right_type, compound.name);
            },
            .MODULO_EQUAL => {
                try self.handleModuloEqual(left_type, right_type, compound.name);
            },
            .POWER_EQUAL => {
                try self.handlePowerEqual(left_type, right_type, compound.name);
            },
            else => {
                const location = Location{
                    .file = compound.operator.file,
                    .file_uri = compound.operator.file_uri,
                    .range = .{
                        .start_line = compound.operator.line,
                        .start_col = compound.operator.column,
                        .end_line = compound.operator.line,
                        .end_col = compound.operator.column + compound.operator.lexeme.len,
                    },
                };
                self.generator.reporter.reportCompileError(
                    location,
                    ErrorCode.UNSUPPORTED_OPERATOR,
                    "Unsupported compound assignment operator: {}",
                    .{compound.operator.type},
                );
                return ErrorList.UnsupportedOperator;
            },
        }

        // `/` is float division whatever the operands; every other operator
        // keeps the left operand's type.
        const result_type: HIRType = if (compound.operator.type == .SLASH_EQUAL) .Float else left_type;
        const expected_type = try self.generator.bindingTypeOf(&expr.base);
        try self.generator.convertValue(result_type, expected_type);

        // Duplicate the result to leave it on stack as the expression result
        if (preserve_result) {
            try self.generator.instructions.append(.Dup);
        }

        try self.generator.storeName(&expr.base, compound.name.lexeme, expected_type, .rehome);
    }

    // Private helper methods for each compound operator type
    fn handlePlusEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Int } });
        } else if (left_type == .Float and right_type == .Float) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Float } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Byte } });
        } else if (left_type == .Byte and right_type == .Int) {
            // Implicitly convert RHS Int to Byte for byte arithmetic
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = .Int, .to_type = .Byte } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Byte } });
        } else if (left_type == .Float and (right_type == .Int or right_type == .Byte)) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Float } });
        } else if (left_type == .String and right_type == .String) {
            try self.generator.instructions.append(.Swap);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
        } else if (left_type == .Array and right_type == .Array) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Unknown } });
        } else {
            const location = Location{
                .file = name.file,
                .file_uri = name.file_uri,
                .range = .{
                    .start_line = name.line,
                    .start_col = name.column,
                    .end_line = name.line,
                    .end_col = name.column + name.lexeme.len,
                },
            };
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "Cannot use += operator between {s} and {s}. Both operands must be the same type.",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleMinusEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Int } });
        } else if (left_type == .Float and right_type == .Float) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Float } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Byte } });
        } else if (left_type == .Float and (right_type == .Int or right_type == .Byte)) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Float } });
        } else {
            const location = Location{
                .file = name.file,
                .file_uri = name.file_uri,
                .range = .{
                    .start_line = name.line,
                    .start_col = name.column,
                    .end_line = name.line,
                    .end_col = name.column + name.lexeme.len,
                },
            };
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "Cannot use -= operator between {s} and {s}. Both operands must be the same type.",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleMultiplyEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Int } });
        } else if (left_type == .Float and right_type == .Float) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Float } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Byte } });
        } else if (left_type == .Float and (right_type == .Int or right_type == .Byte)) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Float } });
        } else {
            const location = Location{
                .file = name.file,
                .file_uri = name.file_uri,
                .range = .{
                    .start_line = name.line,
                    .start_col = name.column,
                    .end_line = name.line,
                    .end_col = name.column + name.lexeme.len,
                },
            };
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "Cannot use *= operator between {s} and {s}. Both operands must be the same type.",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    fn handleDivideEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        _ = name; // Unused parameter
        // Stack: [..., left, right]
        // Convert left to Float
        try self.generator.instructions.append(.Swap);
        if (left_type != .Float) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = left_type, .to_type = .Float } });
        }
        // Convert right to Float
        try self.generator.instructions.append(.Swap);
        if (right_type != .Float) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
        }
        // Now do float division
        try self.generator.instructions.append(.{ .Arith = .{ .op = .Div, .operand_type = .Float } });
    }

    fn handlePowerEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Int } });
        } else if (left_type == .Float and right_type == .Float) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Float } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Byte } });
        } else if (left_type == .Float and (right_type == .Int or right_type == .Byte)) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Float } });
        } else {
            const location = Location{
                .file = name.file,
                .file_uri = name.file_uri,
                .range = .{
                    .start_line = name.line,
                    .start_col = name.column,
                    .end_line = name.line,
                    .end_col = name.column + name.lexeme.len,
                },
            };
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "Cannot use **= operator between {s} and {s}. Both operands must be the same type.",
                .{ @tagName(left_type), @tagName(right_type) },
            );
            return ErrorList.TypeMismatch;
        }
    }

    /// `//=`. Integer-only, matching the binary `//`: `semantic.zig` rejects a
    /// float operand there with `E1006`, so a float target is a type error here
    /// rather than a silent conversion.
    fn handleIntDivEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .IntDiv, .operand_type = .Int } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .IntDiv, .operand_type = .Byte } });
        } else if (left_type == .Byte and right_type == .Int) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = .Int, .to_type = .Byte } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .IntDiv, .operand_type = .Byte } });
        } else {
            try self.reportCompoundOperandMismatch("//=", left_type, right_type, name);
            return ErrorList.TypeMismatch;
        }
    }

    /// `%=`. Integer-only, matching the binary `%`.
    fn handleModuloEqual(self: *AssignmentsHandler, left_type: HIRType, right_type: HIRType, name: ast.Token) !void {
        if (left_type == .Int and right_type == .Int) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mod, .operand_type = .Int } });
        } else if (left_type == .Byte and right_type == .Byte) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mod, .operand_type = .Byte } });
        } else if (left_type == .Byte and right_type == .Int) {
            try self.generator.instructions.append(.{ .Convert = .{ .from_type = .Int, .to_type = .Byte } });
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Mod, .operand_type = .Byte } });
        } else {
            try self.reportCompoundOperandMismatch("%=", left_type, right_type, name);
            return ErrorList.TypeMismatch;
        }
    }

    /// The shared operand-mismatch diagnostic for the integer-only compound
    /// operators. `op` is spelled out in the message so the error names the
    /// operator the programmer actually wrote.
    fn reportCompoundOperandMismatch(
        self: *AssignmentsHandler,
        op: []const u8,
        left_type: HIRType,
        right_type: HIRType,
        name: ast.Token,
    ) !void {
        const location = Location{
            .file = name.file,
            .file_uri = name.file_uri,
            .range = .{
                .start_line = name.line,
                .start_col = name.column,
                .end_line = name.line,
                .end_col = name.column + name.lexeme.len,
            },
        };
        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.TYPE_MISMATCH,
            "Cannot use {s} operator between {s} and {s}. Both operands must be integers or bytes.",
            .{ op, @tagName(left_type), @tagName(right_type) },
        );
    }
};
