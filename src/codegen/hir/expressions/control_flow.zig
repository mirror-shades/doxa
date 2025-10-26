const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;
const TETRA_TRUE = @import("../soxa_generator.zig").TETRA_TRUE;
const generateStatement = @import("../soxa_statements.zig").generateStatement;

/// Handle control flow expressions: if, match, loops, blocks
pub const ControlFlowHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) ControlFlowHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for if expressions
    pub fn generateIf(self: *ControlFlowHandler, if_expr: ast.If, preserve_result: bool, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        // Special-case: if inside a loop and the then/else branch is a pure break/continue block,
        // emit a direct conditional jump to the loop label (so control flow skips subsequent body code).
        var handled_as_loop_control = false;
        const lc_opt = self.generator.currentLoopContext();

        // Helper lambdas for detection
        const isControlOnlyBlock = struct {
            fn run(_: *HIRGenerator, node: *ast.Expr, want_break: bool, want_continue: bool) bool {
                switch (node.data) {
                    .Block => |blk| {
                        if (blk.statements.len == 0) return false;
                        // Require all statements to be the desired control kind(s)
                        for (blk.statements) |s| {
                            const d = s.data;
                            if (want_break and d == .Break) continue;
                            if (want_continue and d == .Continue) continue;
                            // Allow empty expression statements as no-ops
                            if (d == .Expression and s.data.Expression == null) continue;
                            return false;
                        }
                        return true;
                    },
                    else => return false,
                }
            }
        };

        if (lc_opt) |lc| {
            const then_is_continue = isControlOnlyBlock.run(self.generator, if_expr.then_branch.?, false, true);
            const then_is_break = isControlOnlyBlock.run(self.generator, if_expr.then_branch.?, true, false);
            const else_is_continue = if (if_expr.else_branch) |eb| isControlOnlyBlock.run(self.generator, eb, false, true) else false;
            const else_is_break = if (if_expr.else_branch) |eb| isControlOnlyBlock.run(self.generator, eb, true, false) else false;

            if (then_is_continue and !else_is_break and !else_is_continue and !then_is_break) {
                // If TRUE -> continue label, else fall-through
                try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                const end_if = try self.generator.generateLabel("end_if");
                try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = lc.continue_label, .label_false = end_if, .vm_offset = 0, .condition_type = .Tetra } });
                try self.generator.instructions.append(.{ .Label = .{ .name = end_if, .vm_address = 0 } });
                handled_as_loop_control = true;
            } else if (then_is_break and !else_is_break and !else_is_continue and !then_is_continue) {
                // If TRUE -> break label, else fall-through
                try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                const end_if = try self.generator.generateLabel("end_if");
                try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = lc.break_label, .label_false = end_if, .vm_offset = 0, .condition_type = .Tetra } });
                try self.generator.instructions.append(.{ .Label = .{ .name = end_if, .vm_address = 0 } });
                handled_as_loop_control = true;
            } else if (!then_is_break and !then_is_continue and (else_is_break or else_is_continue)) {
                // DISABLED: This optimization can skip important semantics like debugging output
                // or proper execution flow. It's safer to use the standard if-then-else codegen.
            }
        }

        if (!handled_as_loop_control) {
            // Standard if codegen
            try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);

            const end_label = try self.generator.generateLabel("end_if");
            const then_label = try self.generator.generateLabel("then");

            if (if_expr.else_branch) |else_branch| {
                // Check if else branch is just a nothing literal (implicit else)
                const is_implicit_nothing = switch (else_branch.data) {
                    .Literal => |lit| switch (lit) {
                        .nothing => true,
                        else => false,
                    },
                    else => false,
                };

                if (is_implicit_nothing) {
                    // No real else branch - only generate then branch
                    try self.generator.instructions.append(.{
                        .JumpCond = .{
                            .label_true = then_label,
                            .label_false = end_label,
                            .vm_offset = 0,
                            .condition_type = .Tetra,
                        },
                    });

                    // THEN branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = then_label, .vm_address = 0 } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    } else {
                        // Statement context: do not produce a value
                        try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                } else {
                    // Has real else branch - generate both branches
                    const else_label = try self.generator.generateLabel("else");
                    try self.generator.instructions.append(.{
                        .JumpCond = .{
                            .label_true = then_label,
                            .label_false = else_label,
                            .vm_offset = 0,
                            .condition_type = .Tetra,
                        },
                    });

                    // THEN branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = then_label, .vm_address = 0 } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    } else {
                        // Statement context: do not produce a value
                        try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                    try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                    // ELSE branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.else_branch.?, true, should_pop_after_use);
                    } else {
                        try self.generator.generateExpression(if_expr.else_branch.?, false, should_pop_after_use);
                    }
                }
            } else {
                // No else branch - only generate then branch
                try self.generator.instructions.append(.{
                    .JumpCond = .{
                        .label_true = then_label,
                        .label_false = end_label,
                        .vm_offset = 0,
                        .condition_type = .Tetra,
                    },
                });

                // THEN branch
                try self.generator.instructions.append(.{ .Label = .{ .name = then_label, .vm_address = 0 } });
                if (preserve_result) {
                    // If we need to preserve result but there's no else branch,
                    // we need to generate a nothing value for the else case
                    try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    // Jump to end to skip the nothing value generation
                    try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });
                } else {
                    // Statement context: do not produce a value
                    try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                }
            }
            try self.generator.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
        }
    }

    /// Generate HIR for match expressions
    pub fn generateMatch(self: *ControlFlowHandler, match_expr: ast.MatchExpr) ErrorList!void {
        // Extract enum type context from the match value
        var match_enum_type: ?[]const u8 = null;
        switch (match_expr.value.data) {
            .Variable => |v| {
                const var_name = v.lexeme;
                if (self.generator.symbol_table.getTrackedVariableType(var_name)) |var_type| {
                    if (var_type == .Enum) {
                        match_enum_type = self.generator.symbol_table.getVariableCustomType(var_name);
                    }
                }
                // Fallback: if the variable name itself is a registered enum type, use it
                if (match_enum_type == null) {
                    if (self.generator.type_system.custom_types.get(var_name)) |ct| {
                        if (ct.kind == .Enum) {
                            match_enum_type = var_name;
                        }
                    }
                }
            },
            .FieldAccess => |fa| {
                // Handle matching on enum member container like GameState.Start (unlikely here but safe)
                if (fa.object.data == .Variable) {
                    const base_name = fa.object.data.Variable.lexeme;
                    if (self.generator.type_system.custom_types.get(base_name)) |ct| {
                        if (ct.kind == .Enum) match_enum_type = base_name;
                    }
                }
            },
            else => {},
        }

        try self.generator.generateExpression(match_expr.value, true, false);

        // Create labels for each case body and the end
        const end_label = try self.generator.generateLabel("match_end");
        var case_labels = std.array_list.Managed([]const u8).init(self.generator.allocator);
        defer case_labels.deinit();
        var check_labels = std.array_list.Managed([]const u8).init(self.generator.allocator);
        defer check_labels.deinit();

        // Generate labels for each case body and case check
        for (match_expr.cases, 0..) |_, i| {
            const case_label = try self.generator.generateLabel("match_case");
            try case_labels.append(case_label);

            // Create check labels for all but the first case (first case starts immediately)
            if (i > 0) {
                const check_label = try self.generator.generateLabel("match_check");
                try check_labels.append(check_label);
            }
        }

        for (match_expr.cases, 0..) |case, i| {
            // Add check label for cases after the first
            if (i > 0) {
                try self.generator.instructions.append(.{ .Label = .{ .name = check_labels.items[i - 1], .vm_address = 0 } });
            }

            // Handle multiple patterns for this case
            var pattern_matched = false;

            for (case.patterns, 0..) |pattern, pattern_idx| {
                // Duplicate the match value for comparison (each pattern needs its own copy)
                try self.generator.instructions.append(.Dup);

                // Treat both token type .ELSE and identifier "else" as the else-case
                const is_else_case = pattern.type == .ELSE or
                    (pattern.type == .IDENTIFIER and std.mem.eql(u8, pattern.lexeme, "else"));

                if (is_else_case) {
                    // Else case - always matches, pop the duplicated value
                    try self.generator.instructions.append(.Pop);
                    try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i], .vm_offset = 0 } });
                    pattern_matched = true;
                    break;
                } else {
                    // Check if this is a type pattern for union matching
                    const is_type_pattern = switch (pattern.type) {
                        .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE => true,
                        else => false,
                    };

                    if (is_type_pattern) {
                        // This is a type pattern - use TypeCheck instruction
                        const type_name = pattern.lexeme;
                        try self.generator.instructions.append(.{ .TypeCheck = .{ .target_type = type_name } });
                    } else if (match_enum_type) |enum_type_name| {
                        // Generate the pattern value (enum member with proper context)
                        const variant_index = if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type|
                            custom_type.getEnumVariantIndex(pattern.lexeme) orelse 0
                        else
                            0;

                        const pattern_value = HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = enum_type_name,
                                .variant_name = pattern.lexeme,
                                .variant_index = variant_index,
                                .path = null,
                            },
                        };

                        const pattern_value_idx = try self.generator.addConstant(pattern_value);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_value_idx } });

                        // Compare and jump if equal (use Enum operand type)
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = 0 } } });
                    } else if (self.generator.current_enum_type) |enum_type_from_context| {
                        // Fallback: if we are in an enum context (e.g., inside var decl init), use it
                        const variant_index2 = if (self.generator.type_system.custom_types.get(enum_type_from_context)) |custom_type|
                            custom_type.getEnumVariantIndex(pattern.lexeme) orelse 0
                        else
                            0;

                        const pattern_value2 = HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = enum_type_from_context,
                                .variant_name = pattern.lexeme,
                                .variant_index = variant_index2,
                                .path = null,
                            },
                        };
                        const pattern_idx2 = try self.generator.addConstant(pattern_value2);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value2, .constant_id = pattern_idx2 } });
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = 0 } } });
                    } else {
                        // Regular string literal pattern
                        const pattern_value = HIRValue{ .string = pattern.literal.string };
                        const pattern_constant_idx = try self.generator.addConstant(pattern_value);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_constant_idx } });

                        // Compare and jump if equal (use String operand type)
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .String } });
                    }

                    // For the last pattern in this case, determine where to jump if no match
                    if (pattern_idx == case.patterns.len - 1) {
                        // This is the last pattern for this case
                        const false_label = if (i < match_expr.cases.len - 1)
                            check_labels.items[i] // Jump to next case check
                        else
                            end_label; // Last case - jump to end if no match
                        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = false_label, .vm_offset = 0, .condition_type = .Tetra } });
                    } else {
                        // Not the last pattern - if this doesn't match, continue to next pattern
                        const next_pattern_label = try self.generator.generateLabel("next_pattern");
                        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = next_pattern_label, .vm_offset = 0, .condition_type = .Tetra } });
                        try self.generator.instructions.append(.{ .Label = .{ .name = next_pattern_label, .vm_address = 0 } });
                    }
                }
            }
        }

        // Generate case bodies with enum context
        for (match_expr.cases, 0..) |case, i| {
            try self.generator.instructions.append(.{ .Label = .{ .name = case_labels.items[i], .vm_address = 0 } });

            // Set enum context for case body generation if needed
            const old_enum_context = self.generator.current_enum_type;
            if (match_enum_type) |enum_type_name| {
                self.generator.current_enum_type = enum_type_name;
            }

            // Check if the case body is a block (statements) or an expression
            const is_block = case.body.data == .Block;

            if (is_block) {
                // For blocks, we don't need to pop the match value since blocks don't return values
                // and we don't need to preserve the result
                try self.generator.generateExpression(case.body, false, false);
            } else {
                // For expressions, drop the original match value before producing the case body result
                // to keep the stack balanced and ensure the case body value is on top.
                try self.generator.instructions.append(.Pop);
                try self.generator.generateExpression(case.body, true, false);
            }

            // Restore previous enum context
            self.generator.current_enum_type = old_enum_context;

            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });
        }

        // End label - the stack should now contain the result from whichever case was taken
        try self.generator.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });

        // The match statement result is now on the stack and will be handled by the PHI node logic
    }

    /// Generate HIR for loop expressions
    pub fn generateLoop(self: *ControlFlowHandler, loop: ast.Loop, preserve_result: bool) !void {
        _ = preserve_result; // Unused parameter
        const loop_start_label = try self.generator.generateLabel("loop_start");
        const loop_body_label = try self.generator.generateLabel("loop_body");
        const loop_step_label = try self.generator.generateLabel("loop_step");
        const loop_end_label = try self.generator.generateLabel("loop_end");

        // continue should jump to step if present, otherwise to start
        const continue_target = if (loop.step != null) loop_step_label else loop_start_label;
        try self.generator.pushLoopContext(loop_end_label, continue_target);

        // Initializer (var decl or expression statement)
        if (loop.var_decl) |initializer| {
            try generateStatement(self.generator, initializer.*);
        }

        // Loop start - condition check
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_start_label, .vm_address = 0 } });

        if (loop.condition) |condition| {
            try self.generator.generateExpression(condition, true, false);
        } else {
            const true_idx = try self.generator.addConstant(HIRValue{ .tetra = TETRA_TRUE });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_TRUE }, .constant_id = true_idx } });
        }

        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = loop_body_label, .label_false = loop_end_label, .vm_offset = 0, .condition_type = .Tetra } });

        // Body
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_body_label, .vm_address = 0 } });

        // Enter per-iteration scope to ensure locals/consts do not leak across iterations
        var iteration_scope_id: u32 = 0;
        if (self.generator.current_function != null) {
            iteration_scope_id = self.generator.label_generator.label_count + 2000;
            try self.generator.instructions.append(.{ .EnterScope = .{ .scope_id = iteration_scope_id, .var_count = 0 } });
        }

        try self.generator.generateExpression(loop.body, false, false);

        // Step
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_step_label, .vm_address = 0 } });

        // Exit per-iteration scope before executing the step
        if (self.generator.current_function != null) {
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = iteration_scope_id } });
        }
        if (loop.step) |step_expr| {
            try self.generator.generateExpression(step_expr, false, false);
        }

        try self.generator.instructions.append(.{ .Jump = .{ .label = loop_start_label, .vm_offset = 0 } });
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_end_label, .vm_address = 0 } }); // Add end label
        self.generator.popLoopContext();
    }

    /// Generate HIR for block expressions
    pub fn generateBlock(self: *ControlFlowHandler, block: ast.Expr.Data, preserve_result: bool) !void {
        const block_data = block.Block;

        // Generate all block statements without creating scopes for simple blocks
        for (block_data.statements) |stmt| {
            try generateStatement(self.generator, stmt);
        }

        // Generate final value if present
        if (block_data.value) |value_expr| {
            // Evaluate the final value expression
            if (preserve_result) {
                try self.generator.generateExpression(value_expr, true, false);
            } else {
                try self.generator.generateExpression(value_expr, false, false);
            }
        } else if (preserve_result) {
            // Block without value: only push 'nothing' when a value is expected
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }
    }

    /// Generate HIR for return expressions
    pub fn generateReturn(self: *ControlFlowHandler, return_expr: ast.Expr.Data) !void {
        const return_data = return_expr.ReturnExpr;

        // TAIL CALL OPTIMIZATION: Check if return value is a direct function call
        if (return_data.value) |value| {
            if (self.generator.tryGenerateTailCall(value)) {
                return; // Tail call replaces both Call and Return
            } else {
                // Regular return with value
                try self.generator.generateExpression(value, true, false);
            }
        } else {
            // No value - push nothing
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }

        // Generate Return instruction (only if not tail call)
        try self.generator.instructions.append(.{ .Return = .{ .has_value = return_data.value != null, .return_type = self.generator.current_function_return_type } });
    }

    /// Generate HIR for cast expressions
    pub fn generateCast(self: *ControlFlowHandler, cast_expr: ast.Expr.Data, preserve_result: bool) !void {
        const cast_data = cast_expr.Cast;

        // Generate the value to cast
        try self.generator.generateExpression(cast_data.value, true, false);

        // Duplicate it so we can keep original value on success path
        try self.generator.instructions.append(.Dup);

        // Map target type to a runtime name string compatible with VM getTypeString
        const target_name: []const u8 = blk: {
            switch (cast_data.target_type.data) {
                .Basic => |basic| switch (basic) {
                    .Integer => break :blk "int",
                    .Byte => break :blk "byte",
                    .Float => break :blk "float",
                    .String => break :blk "string",
                    .Tetra => break :blk "tetra",
                    .Nothing => break :blk "nothing",
                },
                .Custom => |tok| break :blk tok.lexeme,
                .Array => |arr_type| {
                    // Map array element types to the strings produced by VM.getTypeString
                    switch (arr_type.element_type.data) {
                        .Basic => |elem_basic| switch (elem_basic) {
                            .Integer => break :blk "int[]",
                            .Byte => break :blk "byte[]",
                            .Float => break :blk "float[]",
                            .String => break :blk "string[]",
                            .Tetra => break :blk "tetra[]",
                            .Nothing => break :blk "array[]", // VM uses array[] for unknown/nothing
                        },
                        // Arrays of custom/struct types appear as struct[] at runtime
                        .Custom => break :blk "struct[]",
                        .Struct => break :blk "struct[]",
                        // Other element kinds (enum, union, map, function, auto) default to array[]
                        else => break :blk "array[]",
                    }
                },
                .Struct => break :blk "struct",
                .Enum => break :blk "enum",
                .Union => break :blk "union",
            }
        };

        // Check runtime type against target type using dedicated TypeCheck instruction
        try self.generator.instructions.append(.{ .TypeCheck = .{ .target_type = target_name } });

        // Branch based on comparison
        const ok_label = try self.generator.generateLabel("cast_ok");
        const else_label = try self.generator.generateLabel("cast_else");
        const end_label = try self.generator.generateLabel("cast_end");
        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = ok_label, .label_false = else_label, .vm_offset = 0, .condition_type = .Tetra } });

        // Else branch: drop original value and evaluate else expression
        try self.generator.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });
        try self.generator.instructions.append(.Pop);
        if (cast_data.else_branch) |else_expr| {
            // Preserve result only if requested by parent
            try self.generator.generateExpression(else_expr, preserve_result, false);
        } else {
            // No else branch: cast must fail -> halt program
            try self.generator.instructions.append(.Halt);
        }
        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

        // Success branch
        try self.generator.instructions.append(.{ .Label = .{ .name = ok_label, .vm_address = 0 } });
        if (cast_data.then_branch) |then_expr| {
            // On success, drop original and evaluate then-branch
            try self.generator.instructions.append(.Pop);
            try self.generator.generateExpression(then_expr, preserve_result, false);
        } else {
            // No then branch: keep original value if result is needed, drop if not
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        }

        // End merge point
        try self.generator.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
    }
};
