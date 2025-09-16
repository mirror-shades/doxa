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
            // Simple printing case
            try self.generator.generateExpression(print_expr, true, false);
            try self.generator.instructions.append(.{ .Print = .{} });
        } else if (print.format_template) |template| {
            // Generate code for each template part and build correct placeholder mapping
            var arg_count: u32 = 0;
            var format_parts = std.ArrayList([]const u8).init(self.generator.allocator);
            // VM expects placeholder_indices to be argument indices (0..N-1) by placeholder order.
            // We will later interleave using the format part positions, so here we only record arg order.
            var placeholder_indices = std.ArrayList(u32).init(self.generator.allocator);
            var expressions = std.ArrayList(*ast.Expr).init(self.generator.allocator);
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

            // Second pass: evaluate expressions in encounter order (left-to-right)
            // VM will reverse-pop to restore original order
            for (expressions.items) |expr_item| {
                try self.generator.generateExpression(expr_item, true, false);
            }

            // Store format parts as constants and get their IDs
            var format_part_ids = try self.generator.allocator.alloc(u32, format_parts.items.len);
            for (format_parts.items, 0..) |part, i| {
                const constant_id = try self.generator.addConstant(.{ .string = part });
                format_part_ids[i] = constant_id;
            }

            // Generate interpolated print instruction
            try self.generator.instructions.append(.{ .PrintInterpolated = .{
                .format_parts = try format_parts.toOwnedSlice(),
                .placeholder_indices = try placeholder_indices.toOwnedSlice(),
                .argument_count = arg_count,
                .format_part_ids = format_part_ids,
            } });
        } else if (print.arguments) |args| {
            if (args.len == 0) {
                // No interpolation - need to push the format string literal
                // The format_parts should contain the original string
                if (print.format_parts) |parts| {
                    if (parts.len > 0) {
                        // Add the string to the constant pool first
                        const constant_id = try self.generator.addConstant(.{ .string = parts[0] });

                        // Push the constant by ID
                        try self.generator.instructions.append(.{
                            .Const = .{
                                .value = .{ .string = parts[0] }, // Use the actual string value
                                .constant_id = constant_id,
                            },
                        });
                    }
                }
                try self.generator.instructions.append(.{ .Print = .{} });
            } else {
                // Generate code for each argument
                for (args) |arg| {
                    try self.generator.generateExpression(arg, true, false);
                }

                // Store format parts as constants and get their IDs
                const format_parts = print.format_parts orelse return error.MissingFormatParts;
                const placeholder_indices = print.placeholder_indices orelse return error.MissingPlaceholderIndices;

                var format_part_ids = try self.generator.allocator.alloc(u32, format_parts.len);
                for (format_parts, 0..) |part, i| {
                    const constant_id = try self.generator.addConstant(.{ .string = part });
                    format_part_ids[i] = constant_id;
                }

                // Generate interpolated print instruction
                try self.generator.instructions.append(.{ .PrintInterpolated = .{
                    .format_parts = format_parts,
                    .placeholder_indices = placeholder_indices,
                    .argument_count = @intCast(args.len),
                    .format_part_ids = format_part_ids,
                } });
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
        if (peek.expr.data == .Index and peek.expr.data.Index.array.data == .Variable) {
            if (self.generator.getTrackedArrayElementType(peek.expr.data.Index.array.data.Variable.lexeme)) |elem_type| {
                inferred_type = elem_type;
            }
        } else if (peek.expr.data == .Variable) {
            if (self.generator.getTrackedVariableType(peek.expr.data.Variable.lexeme)) |tracked_type| {
                inferred_type = tracked_type;
            }
        }

        // New: include union member list for variables declared as unions
        var union_members: ?[][]const u8 = null;
        // Attach inline union info for selected builtins/internal calls
        if (peek.expr.data == .BuiltinCall) {
            const bc = peek.expr.data.BuiltinCall;
            if (std.mem.eql(u8, bc.function.lexeme, "int")) {
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "int";
                members[1] = "ValueError";
                union_members = members;
            } else if (std.mem.eql(u8, bc.function.lexeme, "float")) {
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "float";
                members[1] = "ValueError";
                union_members = members;
            } else if (std.mem.eql(u8, bc.function.lexeme, "byte")) {
                // @byte now returns byte | ValueError for all inputs (strings, numerics)
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "byte";
                members[1] = "ValueError";
                union_members = members;
            }
        } else if (peek.expr.data == .InternalCall) {
            const ic = peek.expr.data.InternalCall;
            if (std.mem.eql(u8, ic.method.lexeme, "int")) {
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "int";
                members[1] = "ValueError";
                union_members = members;
            } else if (std.mem.eql(u8, ic.method.lexeme, "float")) {
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "float";
                members[1] = "ValueError";
                union_members = members;
            } else if (std.mem.eql(u8, ic.method.lexeme, "byte")) {
                const members = try self.generator.allocator.alloc([]const u8, 2);
                members[0] = "byte";
                members[1] = "ValueError";
                union_members = members;
            }
        }
        if (peek.expr.data == .Variable) {
            const var_name = peek.expr.data.Variable.lexeme;
            // Prefer index-based lookup to avoid name collisions; do this for all scopes
            if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                if (self.generator.symbol_table.getUnionMembersByIndex(var_index)) |members2| {
                    union_members = members2;
                }
            }
        }

        // Generate peek instruction with full path and correct type
        try self.generator.instructions.append(.{ .Peek = .{
            .name = peek_path,
            .value_type = inferred_type,
            .location = peek.location,
            .union_members = union_members,
        } });

        // IMPORTANT: Peek pops the value, prints, then pushes it back.
        // If the caller does not need the result (statement context), drop it now
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
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
                        .container_type = .Struct,
                        .field_index = 0,
                        .field_for_peek = true,
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
