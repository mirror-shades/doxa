const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;

/// Struct info for peek operations
const StructPeekInfo = struct {
    name: []const u8,
    field_count: u32,
    field_names: [][]const u8,
    field_types: []HIRType,
};

/// Handle I/O and debugging operations: print, peek, input
pub const IOHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) IOHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for print expressions
    pub fn generatePrint(self: *IOHandler, print: ast.PrintExpr, preserve_result: bool) !void {
        if (print.expr) |print_expr| {
            // Legacy simple printing: lower as streaming sequence
            try self.generator.instructions.append(.PrintBegin);
            try self.generator.generateExpression(print_expr, true, false);
            try self.generator.instructions.append(.PrintVal);
            try self.generator.instructions.append(.PrintEnd);
        } else if (print.format_template) |template| {
            // Generate code for each template part and build correct placeholder mapping
            var arg_count: u32 = 0;
            var format_parts = std.array_list.Managed([]const u8).init(self.generator.allocator);
            // VM expects placeholder_indices to be argument indices (0..N-1) by placeholder order.
            // We will later interleave using the format part positions, so here we only record arg order.
            var placeholder_indices = std.array_list.Managed(u32).init(self.generator.allocator);
            var expressions = std.array_list.Managed(*ast.Expr).init(self.generator.allocator);
            defer format_parts.deinit();
            defer placeholder_indices.deinit();
            defer expressions.deinit();

            // First pass: collect all expressions and build format parts
            for (template.parts) |part| {
                switch (part) {
                    .String => |str| {
                        // Add string literal to format parts
                        try format_parts.append(str);
                    },
                    .Expression => |part_expr| {
                        // Store expression for later evaluation
                        try expressions.append(part_expr);
                        // Map placeholder to its argument index by encounter order
                        try placeholder_indices.append(arg_count);
                        arg_count += 1;
                    },
                }
            }

            // If there are no placeholders, emit a simple PrintBegin → PrintStr(full) → PrintEnd
            if (arg_count == 0) {
                var total_len: usize = 0;
                for (format_parts.items) |part| total_len += part.len;

                var full = try self.generator.allocator.alloc(u8, total_len);
                var offset: usize = 0;
                for (format_parts.items) |part| {
                    @memcpy(full[offset..(offset + part.len)], part);
                    offset += part.len;
                }

                const const_id = try self.generator.addConstant(.{ .string = full });
                try self.generator.instructions.append(.PrintBegin);
                try self.generator.instructions.append(.{ .PrintStr = .{ .const_id = const_id } });
                // If literal ends with \n, emit explicit newline and strip responsibility lies with frontend; here we just emit as-is
                try self.generator.instructions.append(.PrintEnd);
            } else {
                // Second pass: evaluate expressions in REVERSE order so PrintVal pops in correct left-to-right order
                var expr_idx: usize = expressions.items.len;
                while (expr_idx > 0) {
                    expr_idx -= 1;
                    try self.generator.generateExpression(expressions.items[expr_idx], true, false);
                }

                // Store format parts as constants and get their IDs
                var format_part_ids = try self.generator.allocator.alloc(u32, format_parts.items.len);
                for (format_parts.items, 0..) |part, i| {
                    const constant_id = try self.generator.addConstant(.{ .string = part });
                    format_part_ids[i] = constant_id;
                }

                // Expand interpolated print into streaming sequence
                try self.generator.instructions.append(.PrintBegin);
                // Interleave string parts and PrintVal for expressions
                var expr_index: usize = 0;
                for (format_part_ids, 0..) |part_id, i| {
                    if (i < format_part_ids.len and part_id != 0xFFFFFFFF) {
                        try self.generator.instructions.append(.{ .PrintStr = .{ .const_id = part_id } });
                    }
                    if (i < placeholder_indices.items.len) {
                        // Top-of-stack currently has all expressions pushed in order; we'll print values now in order by popping later at VM level via PrintVal
                        try self.generator.instructions.append(.PrintVal);
                        expr_index += 1;
                    }
                }
                try self.generator.instructions.append(.PrintEnd);
            }
        } else if (print.arguments) |args| {
            if (args.len == 0) {
                // No interpolation - emit PrintBegin → PrintStr(original) → PrintEnd
                if (print.format_parts) |parts| {
                    if (parts.len > 0) {
                        // Add the string to the constant pool first
                        const constant_id = try self.generator.addConstant(.{ .string = parts[0] });
                        try self.generator.instructions.append(.PrintBegin);
                        try self.generator.instructions.append(.{ .PrintStr = .{ .const_id = constant_id } });
                        try self.generator.instructions.append(.PrintEnd);
                    }
                }
            } else {
                // Generate code for each argument in REVERSE order so PrintVal pops in correct left-to-right order
                var arg_idx: usize = args.len;
                while (arg_idx > 0) {
                    arg_idx -= 1;
                    try self.generator.generateExpression(args[arg_idx], true, false);
                }

                // Store format parts as constants and get their IDs
                const format_parts = print.format_parts orelse return error.MissingFormatParts;
                const placeholder_indices = print.placeholder_indices orelse return error.MissingPlaceholderIndices;

                var format_part_ids = try self.generator.allocator.alloc(u32, format_parts.len);
                for (format_parts, 0..) |part, i| {
                    const constant_id = try self.generator.addConstant(.{ .string = part });
                    format_part_ids[i] = constant_id;
                }

                // Expand interpolated print into streaming sequence
                try self.generator.instructions.append(.PrintBegin);
                var expr_index2: usize = 0;
                for (format_part_ids, 0..) |pid, i| {
                    try self.generator.instructions.append(.{ .PrintStr = .{ .const_id = pid } });
                    if (i < placeholder_indices.len) {
                        try self.generator.instructions.append(.PrintVal);
                        expr_index2 += 1;
                    }
                }
                try self.generator.instructions.append(.PrintEnd);
            }
        } else {
            self.generator.reporter.reportCompileError(null, ErrorCode.INVALID_PRINT_EXPRESSION, "No expr and no arguments - InvalidPrintExpression", .{});
            return error.InvalidPrintExpression;
        }

        // If the caller does not need the result (statement context), drop it now
        // Note: For interpolated printing, PrintInterpolated already consumes its arguments
        if (!preserve_result and print.expr != null) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for peek expressions
    pub fn generatePeek(self: *IOHandler, peek: ast.PeekExpr, preserve_result: bool) !void {
        // Set current peek expression for field access tracking
        self.generator.current_peek_expr = peek.expr;
        defer self.generator.current_peek_expr = null;

        // Generate the expression to peek (leaves value on stack)
        try self.generator.generateExpression(peek.expr, true, false);

        // Build the full path for the peek expression (handles field access)
        // Special case: Don't show variable name for enum member access like Color.Red
        const peek_path = if (peek.expr.data == .FieldAccess) blk: {
            const field = peek.expr.data.FieldAccess;
            const obj_type = self.generator.inferTypeFromExpression(field.object);
            // If this is enum member access (Color.Red), don't show variable name
            if (obj_type == .Enum and field.object.data == .Variable) {
                break :blk null; // No variable name for enum member access
            } else {
                break :blk try self.generator.buildPeekPath(peek.expr);
            }
        } else try self.generator.buildPeekPath(peek.expr);

        // NEW: Prefer expression inference; refine for array indexing
        var inferred_type: HIRType = self.generator.inferTypeFromExpression(peek.expr);
        var enum_type_name: ?[]const u8 = null;
        if (peek.expr.data == .Index and peek.expr.data.Index.array.data == .Variable) {
            if (self.generator.getTrackedArrayElementType(peek.expr.data.Index.array.data.Variable.lexeme)) |elem_type| {
                inferred_type = elem_type;
            }
        } else if (peek.expr.data == .Variable) {
            if (self.generator.getTrackedVariableType(peek.expr.data.Variable.lexeme)) |tracked_type| {
                inferred_type = tracked_type;
            }
        } else if (peek.expr.data == .FieldAccess) {
            // For field accesses, try to recover the concrete enum type name
            // (e.g., "Species" for zoo[0].animal_type) so the LLVM backend
            // can print the enum nicely even when the value on the stack is
            // just an i64 discriminant.
            if (inferred_type == .Enum) {
                if (self.generator.resolveFieldAccessType(peek.expr)) |res| {
                    enum_type_name = res.custom_type_name;
                }
            }
        }

        // New: include union member list for variables declared as unions or expressions that return unions
        var union_members: ?[][]const u8 = null;
        // Attach inline union info for selected builtins/internal calls
        if (peek.expr.data == .Variable) {
            const var_name = peek.expr.data.Variable.lexeme;
            // Prefer index-based lookup to avoid name collisions; do this for all scopes
            if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                // FIXED: Only use union member information if we're looking at the correct scope
                // When inside a function, only use union members for local variables to avoid
                // global union variables affecting function parameters with the same name
                var should_use_union_members = true;
                if (self.generator.symbol_table.current_function != null) {
                    // Inside function: only use union members if this is a local variable
                    if (self.generator.symbol_table.local_variables.get(var_name) == null) {
                        // This is a global variable accessed from inside a function
                        // Don't use union member information to avoid scope confusion
                        should_use_union_members = false;
                    }
                }

                if (should_use_union_members) {
                    if (self.generator.symbol_table.getUnionMembersByIndex(var_index)) |members2| {
                        union_members = members2;
                    }
                }
            }
        }

        // Generate peek instruction with full path and correct type
        try self.generator.instructions.append(.{ .Peek = .{
            .name = peek_path,
            .value_type = inferred_type,
            .location = peek.location,
            .union_members = union_members,
            .enum_type_name = enum_type_name,
        } });

        // IMPORTANT: Peek pops the value, prints, then pushes it back.
        // In statement context (!preserve_result), we don't need to do anything else
        // because the Peek instruction already handles the stack properly.
        // The value is pushed back and will be consumed by the next operation or
        // cleaned up at the end of the statement.
        _ = preserve_result; // Acknowledge the parameter is intentionally unused
    }

    /// Generate HIR for struct peek expressions
    pub fn generatePeekStruct(self: *IOHandler, peek: ast.Expr.Data, preserve_result: bool) !void {
        const peek_data = peek.PeekStruct;

        // Generate the expression to peek
        try self.generator.generateExpression(peek_data.expr, true, false);

        // Get struct info from the expression
        const struct_info: StructPeekInfo = switch (peek_data.expr.data) {
            .StructLiteral => |struct_lit| blk: {
                const field_count: u32 = @truncate(struct_lit.fields.len);
                const field_names = try self.generator.allocator.alloc([]const u8, struct_lit.fields.len);
                const field_types = try self.generator.allocator.alloc(HIRType, struct_lit.fields.len);
                break :blk StructPeekInfo{
                    .name = struct_lit.name.lexeme,
                    .field_count = field_count,
                    .field_names = field_names,
                    .field_types = field_types,
                };
            },
            .Variable => |var_token| if (self.generator.getTrackedVariableType(var_token.lexeme)) |var_type| blk: {
                if (var_type != .Struct) {
                    return error.ExpectedStructType;
                }
                const field_names = try self.generator.allocator.alloc([]const u8, 0);
                const field_types = try self.generator.allocator.alloc(HIRType, 0);
                break :blk StructPeekInfo{
                    .name = var_token.lexeme,
                    .field_count = 0,
                    .field_names = field_names,
                    .field_types = field_types,
                };
            } else {
                return error.UnknownVariableType;
            },
            .FieldAccess => |field| blk: {
                // For field access, we need to generate the field access code first
                try self.generator.generateExpression(field.object, true, false);
                try self.generator.instructions.append(.{
                    .StoreFieldName = .{
                        .field_name = field.field.lexeme,
                    },
                });

                // Generate GetField instruction to access the field
                try self.generator.instructions.append(.{
                    .GetField = .{
                        .field_name = field.field.lexeme,
                        .container_type = HIRType{ .Struct = 0 },
                        .struct_id = 0,
                        .field_index = 0,
                        .field_type = .Unknown,
                        .field_for_peek = true,
                        .nested_struct_id = null,
                    },
                });

                // Create a single-field struct info
                const field_names = try self.generator.allocator.alloc([]const u8, 1);
                const field_types = try self.generator.allocator.alloc(HIRType, 1);
                field_names[0] = field.field.lexeme;
                field_types[0] = self.generator.inferTypeFromExpression(peek_data.expr);

                break :blk StructPeekInfo{
                    .name = field.field.lexeme,
                    .field_count = 1,
                    .field_names = field_names,
                    .field_types = field_types,
                };
            },
            else => {
                return error.ExpectedStructType;
            },
        };

        // Add the PeekStruct instruction with the gathered info
        try self.generator.instructions.append(.{ .PeekStruct = .{
            .type_name = struct_info.name,
            .struct_id = 0,
            .field_count = struct_info.field_count,
            .field_names = struct_info.field_names,
            .field_types = struct_info.field_types,
            .location = peek_data.location,
            .should_pop_after_peek = !preserve_result,
        } });
    }

    /// Generate HIR for input expressions
    pub fn generateInput(self: *IOHandler, input: ast.Expr.Data) !void {
        const input_data = input.Input;

        // Check if we have a non-empty prompt
        const prompt_str = input_data.prompt.literal.string;
        if (prompt_str.len > 0) {
            // Generate the prompt as a constant first
            const prompt_value = HIRValue{ .string = prompt_str };
            const prompt_idx = try self.generator.addConstant(prompt_value);

            // Push the prompt onto the stack as an argument
            try self.generator.instructions.append(.{ .Const = .{ .value = prompt_value, .constant_id = prompt_idx } });

            // Generate input call with the prompt as argument
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = 0,
                    .qualified_name = "input",
                    .arg_count = 1, // Has 1 argument (the prompt)
                    .call_kind = .BuiltinFunction,
                    .target_module = null,
                    .return_type = .String,
                },
            });
        } else {
            // No prompt - call input with no arguments
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = 0,
                    .qualified_name = "input",
                    .arg_count = 0, // No arguments
                    .call_kind = .BuiltinFunction,
                    .target_module = null,
                    .return_type = .String,
                },
            });
        }
    }
};
