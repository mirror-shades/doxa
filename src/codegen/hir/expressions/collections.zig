const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const SoxaTypes = @import("../soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;
const HIRMapEntry = @import("../soxa_values.zig").HIRMapEntry;
const ArithOp = @import("../soxa_instructions.zig").ArithOp;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Collect dimension sizes from nested array literals.
/// For [[[0;3];3];3] this yields sizes=[3,3,3], depth=3.
fn collectLiteralNestedSizes(
    elements: []const *ast.Expr,
    sizes: *[4]u32,
    depth: *u3,
) void {
    if (elements.len == 0 or depth.* >= 4) return;
    if (elements[0].data != .Array) return;
    const inner = elements[0].data.Array;
    const inner_len = inner.len;
    for (elements[1..]) |el| {
        if (el.data != .Array or el.data.Array.len != inner_len) return;
    }
    if (inner_len == 0) return;
    sizes[depth.*] = @intCast(inner_len);
    depth.* += 1;
    collectLiteralNestedSizes(inner, sizes, depth);
}

/// Handle collection operations: arrays, maps, indexing
pub const CollectionsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) CollectionsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for an array literal. Its element type and storage are
    /// the analyzer's: the array its context expects, or else its elements'
    /// promoted type, with every element already of that type.
    pub fn generateArray(self: *CollectionsHandler, array_expr: *ast.Expr, preserve_result: bool) !void {
        const elements = array_expr.data.Array;
        // A3: consume a pending return-placement intent. Clearing it before the
        // elements are lowered keeps nested arrays/literals in the callee arena;
        // the element-store path re-homes them into this array's arena.
        const place_intent = self.generator.place_return_value;
        self.generator.place_return_value = false;

        const element_type = (try self.generator.typeOf(array_expr)).Array.*;
        const storage_kind = self.generator.storageKindFromTypeInfo((try self.generator.typeInfoOf(array_expr)).*);

        var nested_sizes: [4]u32 = [_]u32{0} ** 4;
        var nested_depth: u3 = 0;

        // TODO: nested_sizes currently walks only one literal level deep.
        // Triple-nested literals (e.g. int[3][3][3]) need recursive size
        // collection across all dimensions.
        if (storage_kind == .fixed or storage_kind == .const_literal) {
            collectLiteralNestedSizes(elements, &nested_sizes, &nested_depth);
        }

        try self.generator.instructions.append(.{ .ArrayNew = .{
            .element_type = element_type,
            .size = @intCast(elements.len),
            // Nested element typing is handled by the per-element recursion below.
            .nested_element_type = null,
            .storage_kind = storage_kind,
            .nested_sizes = nested_sizes,
            .nested_depth = nested_depth,
            .place_in_caller = place_intent and storage_kind == .dynamic and nested_depth == 0,
            .element_struct_field_types = self.generator.elementStructFieldTypes(element_type),
            .element_struct_type_name = self.generator.elementStructTypeName(element_type),
        } });

        for (elements, 0..) |element, i| {
            const index_value = HIRValue{ .int = @intCast(i) };
            const index_const = try self.generator.addConstant(index_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = index_value, .constant_id = index_const } });

            try self.generator.generateExpression(element, true, false);
            try self.generator.convertValue(try self.generator.typeOf(element), element_type);

            // ArraySet pops: value, index, array; and pushes updated array back
            try self.generator.instructions.append(.{ .ArraySet = .{ .bounds_check = false } }); // No bounds check for initialization
        }

        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for range expressions (e.g., 1 to 6)
    pub fn generateRange(self: *CollectionsHandler, range: struct { start: *ast.Expr, end: *ast.Expr }, preserve_result: bool) !void {
        _ = preserve_result;

        try self.generator.generateExpression(range.start, true, false);
        try self.generator.generateExpression(range.end, true, false);

        const int_type_ptr = try self.generator.allocator.create(HIRType);
        int_type_ptr.* = .Int;
        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = null,
                .qualified_name = "range",
                .arg_count = 2,
                .call_kind = .BuiltinFunction,
                .return_type = HIRType{ .Array = int_type_ptr },
            },
        });
    }

    /// Generate HIR for map literals
    pub fn generateMap(self: *CollectionsHandler, map_expr: *ast.Expr, entries: []*ast.MapEntry, else_expr: ?*ast.Expr) !void {
        // The map's key and value types, as analysis typed the literal.
        const map_type = (try self.generator.typeOf(map_expr)).Map;
        if (else_expr) |expr| {
            try self.generator.generateExpression(expr, true, false);
            try self.generator.convertValue(try self.generator.typeOf(expr), map_type.value.*);
        }

        // Generate key-value pairs in reverse order so the VM pops in source order
        var reverse_i: usize = entries.len;
        while (reverse_i > 0) {
            reverse_i -= 1;
            const entry = entries[reverse_i];
            try self.generator.generateExpression(entry.key, true, false);
            try self.generator.convertValue(try self.generator.typeOf(entry.key), map_type.key.*);
            try self.generator.generateExpression(entry.value, true, false);
            try self.generator.convertValue(try self.generator.typeOf(entry.value), map_type.value.*);
        }

        // Prepare dummy HIRMapEntry slice; VM will read actual values from stack
        const dummy_entries = try self.generator.allocator.alloc(HIRMapEntry, entries.len);
        for (dummy_entries) |*e| {
            const nothing_key = try self.generator.allocator.create(HIRValue);
            nothing_key.* = .{ .nothing = .{} };
            const nothing_value = try self.generator.allocator.create(HIRValue);
            nothing_value.* = .{ .nothing = .{} };
            e.* = HIRMapEntry{ .key = nothing_key, .value = nothing_value };
        }

        const map_instruction = HIRInstruction{
            .Map = .{
                .entries = dummy_entries,
                .key_type = map_type.key.*,
                .value_type = map_type.value.*,
                .has_else_value = else_expr != null,
            },
        };
        try self.generator.instructions.append(map_instruction);
    }

    /// Generate HIR for index access expressions
    pub fn generateIndex(self: *CollectionsHandler, expr: *ast.Expr, preserve_result: bool, should_pop_after_use: bool) !void {
        const index = expr.data.Index;

        // Generate array/map expression
        try self.generator.generateExpression(index.array, true, false);

        // The container as analysis typed it; a narrowed name has already
        // been read as its member.
        switch (try self.generator.typeOf(index.array)) {
            .Map => |map_type| {
                // Generate index expression
                try self.generator.generateExpression(index.index, true, false);
                try self.generator.convertValue(try self.generator.typeOf(index.index), map_type.key.*);

                // The read is the map's value, or `value | nothing` for a map
                // without an `else`.
                try self.generator.instructions.append(.{ .MapGet = .{
                    .key_type = map_type.key.*,
                    .value_type = try self.generator.typeOf(expr),
                } });
            },
            .Array, .String => {
                // Generate index expression
                try self.generator.generateExpression(index.index, true, false);

                // Array or string access - use ArrayGet
                try self.generator.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
            },
            else => unreachable, // analysis indexes only arrays, strings and maps
        }
        // If result is not preserved AND needs to be popped, do so now
        if (!preserve_result and should_pop_after_use) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for index assignment expressions
    pub fn generateIndexAssign(self: *CollectionsHandler, assign: ast.Expr.Data, preserve_result: bool) !void {
        const assign_data = assign.IndexAssign;

        // Check if this is a compound assignment (e.g., tape[tp] += 1)
        // by checking if the value expression is a binary expression that references the same array access
        const is_compound_assignment = switch (assign_data.value.data) {
            .Binary => |binary| blk: {
                // Check if the left side of the binary expression is the same array access
                if (binary.left) |left| {
                    switch (left.data) {
                        .Index => |left_index| {
                            // Check if it's the same array and index
                            const same_array = switch (assign_data.array.data) {
                                .Variable => |arr_var| switch (left_index.array.data) {
                                    .Variable => |left_var| std.mem.eql(u8, arr_var.lexeme, left_var.lexeme),
                                    else => false,
                                },
                                else => false,
                            };
                            const same_index = switch (assign_data.index.data) {
                                .Variable => |idx_var| switch (left_index.index.data) {
                                    .Variable => |left_idx_var| std.mem.eql(u8, idx_var.lexeme, left_idx_var.lexeme),
                                    else => false,
                                },
                                else => false,
                            };
                            break :blk same_array and same_index;
                        },
                        else => break :blk false,
                    }
                } else break :blk false;
            },
            else => false,
        };

        if (is_compound_assignment) {
            // Handle compound assignment: array[index] += value

            // Generate array expression
            try self.generator.generateExpression(assign_data.array, true, false);

            // Generate index expression
            try self.generator.generateExpression(assign_data.index, true, false);

            // Generate the right-hand side of the binary expression
            const binary = assign_data.value.data.Binary;
            if (binary.right) |right| {
                try self.generator.generateExpression(right, true, false);
            }

            // Generate the appropriate compound assignment instruction.
            // Every operator the parser can produce here is listed; anything
            // else means the desugaring in `precedence.zig` grew an arm this
            // table never learned. That used to fall through to `ArithOp.Add`,
            // which silently compiled the wrong arithmetic — `arr[i] //= 3`
            // returned 13 (10 + 3) because `DOUBLE_SLASH` was missing. A
            // missing arm must stop the build, not invent arithmetic.
            const arith_op = switch (binary.operator.type) {
                .PLUS => ArithOp.Add,
                .MINUS => ArithOp.Sub,
                .ASTERISK => ArithOp.Mul,
                .SLASH => ArithOp.Div,
                .DOUBLE_SLASH => ArithOp.IntDiv,
                .MODULO => ArithOp.Mod,
                .POWER => ArithOp.Pow,
                else => return ErrorList.InvalidOperator,
            };
            try self.generator.instructions.append(.{ .ArrayCompoundAssign = .{ .bounds_check = true, .op = arith_op } });

            // Keep stack balanced across control-flow merges for statement-style
            // compound assignments (the result value is unused).
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else {
            // Regular assignment: array[index] = value
            // Generate array expression
            try self.generator.generateExpression(assign_data.array, true, false);

            // The receiver is a map (MapSet) or an array (ArraySet); either
            // way the key and the value are converted to the slot they fill.
            const receiver_type = try self.generator.typeOf(assign_data.array);
            const key_type: HIRType = if (receiver_type == .Map) receiver_type.Map.key.* else .Int;
            const value_type: HIRType = if (receiver_type == .Map) receiver_type.Map.value.* else receiver_type.Array.*;

            try self.generator.generateExpression(assign_data.index, true, false);
            try self.generator.convertValue(try self.generator.typeOf(assign_data.index), key_type);
            try self.generator.generateExpression(assign_data.value, true, false);
            try self.generator.convertValue(try self.generator.typeOf(assign_data.value), value_type);

            if (receiver_type == .Map) {
                try self.generator.instructions.append(.{ .MapSet = .{ .key_type = key_type, .value_type = value_type } });
            } else {
                // Generate ArraySet instruction
                // Stack order expected by VM (top to bottom): value, index, array
                try self.generator.instructions.append(.{ .ArraySet = .{ .bounds_check = true } });
            }
        }

        // Store the modified array back to the variable
        // Skip this for compound assignments since they are atomic operations
        if (!is_compound_assignment and assign_data.array.data == .Variable) {
            const var_name = assign_data.array.data.Variable.lexeme;
            const expected_type = try self.generator.bindingTypeOf(&assign_data.array.base);
            try self.generator.convertValue(try self.generator.typeOf(assign_data.array), expected_type);

            // Duplicate the result to leave it on stack as the expression result
            if (preserve_result) {
                try self.generator.instructions.append(.Dup);
            }

            try self.generator.storeName(&assign_data.array.base, var_name, expected_type, .keep);
        }
    }

    /// Generate HIR for quantifier expressions (ForAll, Exists)
    pub fn generateForAll(self: *CollectionsHandler, forall: ast.Expr.Data) !void {
        const forall_data = forall.ForAll;

        // ForAll quantifier: ∀x ∈ array : condition
        // Implementation: iterate through array, return false if any element fails condition

        // Generate array expression
        try self.generator.generateExpression(forall_data.array, true, false);

        const bound_var_name = forall_data.variable.lexeme;

        // Check if the condition is a simple binary comparison
        if (forall_data.condition.data == .Binary) {
            const binary = forall_data.condition.data.Binary;

            // Check if bound variable is on the left side (e == something)
            if (binary.left) |left| {
                if (left.data == .Variable and std.mem.eql(u8, left.data.Variable.lexeme, bound_var_name)) {
                    // Handle case: bound_var == something
                    if (binary.right) |right| {
                        switch (right.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "e == 3"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.generator.addConstant(comparison_value);
                                try self.generator.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "e == checkAgainst"
                                try self.generator.loadName(&right.base, var_token.lexeme);
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generator.generateExpression(right, true, false);
                            },
                        }
                    }
                }
            }

            // Check if bound variable is on the right side (something == e)
            if (binary.right) |right| {
                if (right.data == .Variable and std.mem.eql(u8, right.data.Variable.lexeme, bound_var_name)) {
                    // Handle case: something == bound_var
                    if (binary.left) |left| {
                        switch (left.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "3 == e"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.generator.addConstant(comparison_value);
                                try self.generator.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "checkAgainst == e"
                                try self.generator.loadName(&left.base, var_token.lexeme);
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generator.generateExpression(left, true, false);
                            },
                        }
                    }
                }
            }

            // If neither side matches the bound variable, generate the condition as-is
            if (!((binary.left != null and binary.left.?.data == .Variable and std.mem.eql(u8, binary.left.?.data.Variable.lexeme, bound_var_name)) or
                (binary.right != null and binary.right.?.data == .Variable and std.mem.eql(u8, binary.right.?.data.Variable.lexeme, bound_var_name))))
            {
                try self.generator.generateExpression(forall_data.condition, true, false);
            }
        } else {
            // Complex condition - generate the expression as-is
            try self.generator.generateExpression(forall_data.condition, true, false);
        }

        // Use builtin function call with proper predicate
        const operator_name = if (forall_data.condition.data == .Binary)
            if (std.mem.eql(u8, forall_data.condition.data.Binary.operator.lexeme, "==")) "forall_quantifier_eq" else "forall_quantifier_gt"
        else
            "forall_quantifier_gt";

        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = null,
                .qualified_name = operator_name,
                .arg_count = 2, // array + comparison value
                .call_kind = .BuiltinFunction,
                .return_type = .Tetra,
            },
        });
    }

    /// Generate HIR for exists quantifier expressions
    pub fn generateExists(self: *CollectionsHandler, exists: ast.Expr.Data) !void {
        const exists_data = exists.Exists;

        // Exists quantifier: ∃x ∈ array : condition
        // Implementation: iterate through array, return true if any element satisfies condition

        // Generate array expression
        try self.generator.generateExpression(exists_data.array, true, false);

        const bound_var_name = exists_data.variable.lexeme;

        // Check if the condition is a simple binary comparison
        if (exists_data.condition.data == .Binary) {
            const binary = exists_data.condition.data.Binary;

            // Check if bound variable is on the left side (e == something)
            if (binary.left) |left| {
                if (left.data == .Variable and std.mem.eql(u8, left.data.Variable.lexeme, bound_var_name)) {
                    // Handle case: bound_var == something
                    if (binary.right) |right| {
                        switch (right.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "e == 3"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.generator.addConstant(comparison_value);
                                try self.generator.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "e == checkAgainst"
                                try self.generator.loadName(&right.base, var_token.lexeme);
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generator.generateExpression(right, true, false);
                            },
                        }
                    }
                }
            }

            // Check if bound variable is on the right side (something == e)
            if (binary.right) |right| {
                if (right.data == .Variable and std.mem.eql(u8, right.data.Variable.lexeme, bound_var_name)) {
                    // Handle case: something == bound_var
                    if (binary.left) |left| {
                        switch (left.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "3 == e"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.generator.addConstant(comparison_value);
                                try self.generator.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "checkAgainst == e"
                                try self.generator.loadName(&left.base, var_token.lexeme);
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generator.generateExpression(left, true, false);
                            },
                        }
                    }
                }
            }

            // If neither side matches the bound variable, generate the condition as-is
            if (!((binary.left != null and binary.left.?.data == .Variable and std.mem.eql(u8, binary.left.?.data.Variable.lexeme, bound_var_name)) or
                (binary.right != null and binary.right.?.data == .Variable and std.mem.eql(u8, binary.right.?.data.Variable.lexeme, bound_var_name))))
            {
                try self.generator.generateExpression(exists_data.condition, true, false);
            }
        } else {
            // Complex condition - generate the expression as-is
            try self.generator.generateExpression(exists_data.condition, true, false);
        }

        // Use builtin function call with proper predicate
        const operator_name = if (exists_data.condition.data == .Binary)
            if (std.mem.eql(u8, exists_data.condition.data.Binary.operator.lexeme, "==")) "exists_quantifier_eq" else "exists_quantifier_gt"
        else
            "exists_quantifier_gt";

        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = null,
                .qualified_name = operator_name,
                .arg_count = 2, // array + comparison value
                .call_kind = .BuiltinFunction,
                .return_type = .Tetra,
            },
        });
    }

    /// Generate HIR for increment operations
    pub fn generateIncrement(self: *CollectionsHandler, operand: *ast.Expr) !void {
        if (operand.data == .Variable) {
            const var_name = operand.data.Variable.lexeme;

            // Load current value
            try self.generator.loadName(&operand.base, var_name);

            // Add 1 (create constant 1)
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Add the values
            const operand_type = try self.generator.typeOf(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });

            // Duplicate result so we can both return it and store it
            try self.generator.instructions.append(.Dup);
            const slot_type = try self.generator.bindingTypeOf(&operand.base);
            try self.generator.convertValue(operand_type, slot_type);

            // Store back to variable
            try self.generator.storeName(&operand.base, var_name, slot_type, .rehome);
        } else {
            // For non-variable expressions, generate the expression and add 1
            try self.generator.generateExpression(operand, true, false);

            // Add 1
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Add the values
            const operand_type = try self.generator.typeOf(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });
        }
    }

    /// Generate HIR for decrement operations
    pub fn generateDecrement(self: *CollectionsHandler, operand: *ast.Expr) !void {
        // Generate decrement operation: load variable, subtract 1, store back
        // First, check if this is a variable reference
        if (operand.data == .Variable) {
            const var_name = operand.data.Variable.lexeme;

            // Load current value
            try self.generator.loadName(&operand.base, var_name);

            // Add 1 (create constant 1)
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Subtract the values
            const operand_type = try self.generator.typeOf(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });

            // Duplicate result so we can both return it and store it
            try self.generator.instructions.append(.Dup);
            const slot_type = try self.generator.bindingTypeOf(&operand.base);
            try self.generator.convertValue(operand_type, slot_type);

            // Store back to variable
            try self.generator.storeName(&operand.base, var_name, slot_type, .rehome);
        } else {
            // For non-variable expressions, generate the expression and subtract 1
            try self.generator.generateExpression(operand, true, false);

            // Add 1
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Subtract the values
            const operand_type = try self.generator.typeOf(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });
        }
    }
};
