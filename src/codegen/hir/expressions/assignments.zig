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

/// Handle assignment operations: regular assignment, compound assignment
pub const AssignmentsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) AssignmentsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for assignment expressions
    pub fn generateAssignment(self: *AssignmentsHandler, assign: ast.Assignment, preserve_result: bool) !void {
        // Special case: namespace aliasing like `const rl is g.raylib`
        // If RHS is a module namespace field (graphics.raylib / graphics.doxa),
        // register LHS name as a module namespace alias and emit no runtime code.
        if (assign.value) |rhs| {
            if (rhs.data == .FieldAccess) {
                const fa = rhs.data.FieldAccess;
                if (fa.object.data == .Variable) {
                    const root_alias = fa.object.data.Variable.lexeme;
                    if (self.generator.isModuleNamespace(root_alias)) {
                        if (std.mem.eql(u8, fa.field.lexeme, "raylib") or std.mem.eql(u8, fa.field.lexeme, "doxa")) {
                            if (self.generator.module_namespaces.get(root_alias)) |_| {
                                // Create a minimal nested ModuleInfo to tag this alias as graphics.<sub>
                                const nested_name = if (std.mem.eql(u8, fa.field.lexeme, "raylib")) "graphics.raylib" else "graphics.doxa";
                                const nested_info: @import("../../../ast/ast.zig").ModuleInfo = .{
                                    .name = nested_name,
                                    .imports = &[_]@import("../../../ast/ast.zig").ImportInfo{},
                                    .ast = null,
                                    .file_path = nested_name,
                                    .symbols = null,
                                };
                                try self.generator.module_namespaces.put(assign.name.lexeme, nested_info);
                                // Do not generate any runtime instructions for this aliasing assignment
                                return;
                            }
                        }
                    }
                }
            }
        }

        // Generate the value expression
        try self.generator.generateExpression(assign.value.?, true, false);

        // NEW: Track the variable's type from the assigned value
        const assigned_type = self.generator.inferTypeFromExpression(assign.value.?);
        
        // Only update the variable's type if it wasn't already explicitly declared
        // This preserves the original type annotation (e.g., "var ip :: int")
        const existing_type = self.generator.getTrackedVariableType(assign.name.lexeme);
        if (existing_type == null or existing_type.? == .Unknown) {
            try self.generator.trackVariableType(assign.name.lexeme, assigned_type);
        }

        // NEW: Track array element type for array literals
        if (assigned_type == .Array and assign.value.?.data == .Array) {
            const elements = assign.value.?.data.Array;
            if (elements.len > 0) {
                const element_type: HIRType = switch (elements[0].data) {
                    .Literal => |lit| switch (lit) {
                        .int => .Int,
                        .float => .Float,
                        .string => .String,
                        .tetra => .Tetra,
                        .byte => .Byte,
                        else => .Unknown,
                    },
                    .Array => HIRType.Unknown, // Handle nested arrays
                    else => .Unknown,
                };
                if (element_type != .Unknown) {
                    try self.generator.trackArrayElementType(assign.name.lexeme, element_type);
                }
            }
        }

        // If assigning result of builtin returning unions, track members
        if (assign.value) |rhs| {
            if (rhs.data == .BuiltinCall) {
                const bc = rhs.data.BuiltinCall;
                if (std.mem.eql(u8, bc.function.lexeme, "int")) {
                    if (self.generator.getOrCreateVariable(assign.name.lexeme)) |var_idx| {
                        const members = try self.generator.allocator.alloc([]const u8, 2);
                        members[0] = "int";
                        members[1] = "ValueError";
                        try self.generator.trackVariableUnionMembersByIndex(var_idx, members);
                    } else |_| {}
                } else if (std.mem.eql(u8, bc.function.lexeme, "slice")) {
                    if (self.generator.getOrCreateVariable(assign.name.lexeme)) |var_idx| {
                        const members = try self.generator.allocator.alloc([]const u8, 2);
                        const base_t = if (bc.arguments.len > 0) self.generator.inferTypeFromExpression(bc.arguments[0]) else .Unknown;
                        members[0] = if (base_t == .String) "string" else "array";
                        members[1] = "ValueError";
                        try self.generator.trackVariableUnionMembersByIndex(var_idx, members);
                    } else |_| {}
                }
            }
        }

        // Get or create variable index
        const var_idx = try self.generator.getOrCreateVariable(assign.name.lexeme);

        // Duplicate value to leave it on stack as assignment result
        if (preserve_result) {
            try self.generator.instructions.append(.Dup);
        }

        // Store to variable
        try self.generator.instructions.append(.{ .StoreVar = .{
            .var_index = var_idx,
            .var_name = assign.name.lexeme,
            .scope_kind = .Local,
            .module_context = null,
            .expected_type = assigned_type,
        } });
    }

    /// Generate HIR for compound assignment expressions
    pub fn generateCompoundAssign(self: *AssignmentsHandler, compound: ast.CompoundAssignment, preserve_result: bool) !void {
        // Check if this is an alias parameter
        if (self.generator.symbol_table.isAliasParameter(compound.name.lexeme)) {
            // For alias parameters, we need to load from the alias, not the local variable
            // The alias should be bound to the original variable's storage
            // We'll use a special instruction to load from the alias
            try self.generator.instructions.append(.{
                .LoadAlias = .{
                    .var_name = compound.name.lexeme,
                    .slot_index = 1, // TODO: Get the correct slot index from the alias binding
                },
            });
        } else {
            // Regular variable - load from local storage
            const var_idx = try self.generator.getOrCreateVariable(compound.name.lexeme);
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx,
                    .var_name = compound.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                },
            });
        }

        // Generate the value expression (e.g., the "1" in "current += 1")
        try self.generator.generateExpression(compound.value.?, true, false);

        const left_type = self.generator.getTrackedVariableType(compound.name.lexeme) orelse .Unknown;
        const right_type = self.generator.inferTypeFromExpression(compound.value.?);
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
            .POWER_EQUAL => {
                try self.handlePowerEqual(left_type, right_type, compound.name);
            },
            else => {
                const location = Location{
                    .file = compound.operator.file,
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

        // Duplicate the result to leave it on stack as the expression result
        if (preserve_result) {
            try self.generator.instructions.append(.Dup);
        }

        // Store the result back to the variable
        const expected_type = self.generator.getTrackedVariableType(compound.name.lexeme) orelse .Unknown;
        if (self.generator.symbol_table.isAliasParameter(compound.name.lexeme)) {
            // For alias parameters, store to the alias, not the local variable
            try self.generator.instructions.append(.{
                .StoreAlias = .{
                    .var_name = compound.name.lexeme,
                    .slot_index = 1, // TODO: Get the correct slot index from the alias binding
                    .expected_type = expected_type,
                },
            });
        } else {
            // Regular variable - store to local storage
            const var_idx = try self.generator.getOrCreateVariable(compound.name.lexeme);
            try self.generator.instructions.append(.{ .StoreVar = .{
                .var_index = var_idx,
                .var_name = compound.name.lexeme,
                .scope_kind = .Local,
                .module_context = null,
                .expected_type = expected_type,
            } });
        }
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
        } else if (left_type == .String and right_type == .String) {
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
        } else if (left_type == .Array and right_type == .Array) {
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Unknown } });
        } else {
            const location = Location{
                .file = name.file,
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
        } else {
            const location = Location{
                .file = name.file,
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
        } else {
            const location = Location{
                .file = name.file,
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
        } else {
            const location = Location{
                .file = name.file,
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
};
