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
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Handle collection operations: arrays, maps, indexing
pub const CollectionsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) CollectionsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for array literals
    pub fn generateArray(self: *CollectionsHandler, elements: []const *ast.Expr, preserve_result: bool) !void {
        return self.generateArrayInternal(elements, preserve_result);
    }

    /// Internal array generation with nesting control
    pub fn generateArrayInternal(self: *CollectionsHandler, elements: []const *ast.Expr, preserve_result: bool) !void {
        var element_type: HIRType = .Unknown;
        var nested_element_type: ?HIRType = null;

        if (elements.len > 0) {
            // Try to infer type from first element
            const first = elements[0];
            switch (first.data) {
                .Literal => |lit| element_type = switch (lit) {
                    .int => .Int,
                    .float => .Float,
                    .string => .String,
                    .tetra => .Tetra,
                    .byte => .Byte,
                    else => .Unknown,
                },
                .Array => |nested_elements| {
                    // For nested arrays, determine the element type of the nested array
                    if (nested_elements.len > 0) {
                        const inner_element_type = self.generator.inferTypeFromExpression(nested_elements[0]);
                        // For nested arrays like [["hello"]], the element_type should be Array(String)
                        const inner_type_ptr = self.generator.allocator.create(HIRType) catch return ErrorList.OutOfMemory;
                        inner_type_ptr.* = inner_element_type;
                        element_type = HIRType{ .Array = inner_type_ptr };
                    } else {
                        // Empty nested array - element type is Array with unknown inner type
                        const unknown_ptr = self.generator.allocator.create(HIRType) catch return ErrorList.OutOfMemory;
                        unknown_ptr.* = .Unknown;
                        element_type = HIRType{ .Array = unknown_ptr };
                    }
                    // For nested arrays, the nested_element_type is not used in the same way
                    nested_element_type = null;
                },
                else => {
                    // Use the general type system to infer the element type.
                    // This lets us correctly detect structs/enums/functions, so
                    // arrays like Animal[] carry HIRType.Struct instead of Unknown.
                    element_type = self.generator.inferTypeFromExpression(first);
                },
            }
        }

        const storage_kind = self.generator.array_storage_override orelse ArrayStorageKind.dynamic;

        // Generate ArrayNew instruction with nested type info
        try self.generator.instructions.append(.{ .ArrayNew = .{
            .element_type = element_type,
            .size = @intCast(elements.len),
            .nested_element_type = nested_element_type,
            .storage_kind = storage_kind,
        } });

        // Generate each element and ArraySet
        for (elements, 0..) |element, i| {
            // Push index
            const index_value = HIRValue{ .int = @intCast(i) };
            const index_const = try self.generator.addConstant(index_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = index_value, .constant_id = index_const } });

            // Generate the element value
            if (element.data == .Array) {
                // Set nested context flag before generating nested array
                const prev_nested_flag = self.generator.is_generating_nested_array;
                self.generator.is_generating_nested_array = true;
                defer self.generator.is_generating_nested_array = prev_nested_flag;

                try self.generator.generateExpression(element, true, false);
            } else {
                try self.generator.generateExpression(element, true, false);
            }

            // ArraySet pops: value, index, array; and pushes updated array back
            try self.generator.instructions.append(.{ .ArraySet = .{ .bounds_check = false } }); // No bounds check for initialization
        }

        // Handle preserve_result parameter
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for range expressions (e.g., 1 to 6)
    pub fn generateRange(self: *CollectionsHandler, range: struct { start: *ast.Expr, end: *ast.Expr }, preserve_result: bool) !void {
        _ = preserve_result; // Unused parameter

        // Generate the start value
        try self.generator.generateExpression(range.start, true, false);

        // Generate the end value
        try self.generator.generateExpression(range.end, true, false);

        // Generate Range instruction that creates an array from start to end
        try self.generator.instructions.append(.{
            .Range = .{
                .element_type = .Int, // Ranges always produce integer arrays
            },
        });
    }

    /// Generate HIR for map literals
    pub fn generateMap(self: *CollectionsHandler, entries: []*ast.MapEntry, else_expr: ?*ast.Expr) !void {
        if (else_expr) |expr| {
            try self.generator.generateExpression(expr, true, false);
        }

        // Generate key-value pairs in reverse order so the VM pops in source order
        var reverse_i: usize = entries.len;
        while (reverse_i > 0) {
            reverse_i -= 1;
            const entry = entries[reverse_i];
            try self.generator.generateExpression(entry.key, true, false);
            try self.generator.generateExpression(entry.value, true, false);
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

        // Infer key/value types from the first entry when possible so that
        // downstream stages (LLVM + peek) know what the map actually stores.
        var inferred_key_type: HIRType = .String;
        var inferred_value_type: HIRType = .Unknown;
        if (entries.len > 0) {
            const first = entries[0];
            const key_t = self.generator.inferTypeFromExpression(first.key);
            const val_t = self.generator.inferTypeFromExpression(first.value);

            inferred_key_type = switch (key_t) {
                // Enums are represented as integer discriminants at runtime.
                .Enum => .Int,
                else => key_t,
            };
            inferred_value_type = switch (val_t) {
                .Unknown, .Nothing => .Unknown,
                else => val_t,
            };
        }

        const map_instruction = HIRInstruction{
            .Map = .{
                .entries = dummy_entries,
                .key_type = inferred_key_type,
                .value_type = inferred_value_type,
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

        // Determine if we're accessing an array, map
        const container_type = self.generator.inferTypeFromExpression(index.array);
        switch (container_type) {
            .Map => {
                // Generate index expression
                try self.generator.generateExpression(index.index, true, false);

                // Infer the result type of this index expression (e.g., float | nothing)
                const result_type = self.generator.inferTypeFromExpression(expr);

                // Map access - choose a compatible key type for the runtime map (i64 storage).
                // Treat enums/bytes/tetra/etc as Int keys; only String stays String.
                const raw_key_type = self.generator.inferTypeFromExpression(index.index);
                const key_type: HIRType = switch (raw_key_type) {
                    .String => .String,
                    .Enum => .Int,
                    .Int, .Byte, .Tetra, .Float, .Nothing, .Unknown, .Array, .Map, .Struct, .Function, .Union, .Poison => .Int,
                };
                try self.generator.instructions.append(.{ .MapGet = .{
                    .key_type = key_type,
                    .value_type = result_type,
                } });
            },
            .Array, .String => {
                // Generate index expression
                try self.generator.generateExpression(index.index, true, false);

                // Array or string access - use ArrayGet
                try self.generator.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });

                // Record element type for index expressions into variables
                if (index.array.data == .Variable) {
                    if (self.generator.getTrackedArrayElementType(index.array.data.Variable.lexeme)) |elem_type| {
                        try self.generator.trackVariableType("__index_tmp__", elem_type);
                    }
                }
            },
            else => {
                // Generate index expression
                try self.generator.generateExpression(index.index, true, false);

                // Default unknown container to ArrayGet (covers strings/arrays); Map is detected explicitly above
                try self.generator.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
            },
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
            // Generate: array, index, value, ArrayGetAndAdd (atomic operation)

            // Generate array expression
            try self.generator.generateExpression(assign_data.array, true, false);

            // Generate index expression
            try self.generator.generateExpression(assign_data.index, true, false);

            // Generate the right-hand side of the binary expression
            const binary = assign_data.value.data.Binary;
            if (binary.right) |right| {
                try self.generator.generateExpression(right, true, false);
            }

            // Generate the appropriate compound assignment instruction
            switch (binary.operator.type) {
                .PLUS => {
                    try self.generator.instructions.append(.{ .ArrayGetAndAdd = .{ .bounds_check = true } });
                },
                .MINUS => {
                    try self.generator.instructions.append(.{ .ArrayGetAndSub = .{ .bounds_check = true } });
                },
                .ASTERISK => {
                    try self.generator.instructions.append(.{ .ArrayGetAndMul = .{ .bounds_check = true } });
                },
                .SLASH => {
                    try self.generator.instructions.append(.{ .ArrayGetAndDiv = .{ .bounds_check = true } });
                },
                .MODULO => {
                    try self.generator.instructions.append(.{ .ArrayGetAndMod = .{ .bounds_check = true } });
                },
                .POWER => {
                    try self.generator.instructions.append(.{ .ArrayGetAndPow = .{ .bounds_check = true } });
                },
                else => {
                    // Fallback to addition for unknown operators
                    try self.generator.instructions.append(.{ .ArrayGetAndAdd = .{ .bounds_check = true } });
                },
            }

            // Keep stack balanced across control-flow merges for statement-style
            // compound assignments (the result value is unused).
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else {
            // Regular assignment: array[index] = value
            // Generate array expression
            try self.generator.generateExpression(assign_data.array, true, false);

            // Generate index expression
            try self.generator.generateExpression(assign_data.index, true, false);

            // Generate value expression
            try self.generator.generateExpression(assign_data.value, true, false);

            // If the receiver is a map, emit MapSet; otherwise ArraySet
            const container_type = self.generator.inferTypeFromExpression(assign_data.array);
            if (container_type == .Map) {
                const idx_type = self.generator.inferTypeFromExpression(assign_data.index);
                const key_type: HIRType = switch (idx_type) {
                    .String => .String,
                    .Enum => .Int,
                    .Int, .Byte, .Tetra, .Float, .Nothing, .Unknown, .Array, .Map, .Struct, .Function, .Union, .Poison => .Int,
                };
                try self.generator.instructions.append(.{ .MapSet = .{ .key_type = key_type } });
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

            const var_idx = try self.generator.getOrCreateVariable(var_name);
            const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;

            // Duplicate the result to leave it on stack as the expression result
            if (preserve_result) {
                try self.generator.instructions.append(.Dup);
            }

            try self.generator.instructions.append(.{ .StoreVar = .{
                .var_index = var_idx,
                .var_name = var_name,
                .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                .module_context = null,
                .expected_type = expected_type,
            } });
        }
    }

    /// Generate HIR for quantifier expressions (ForAll, Exists)
    pub fn generateForAll(self: *CollectionsHandler, forall: ast.Expr.Data) !void {
        const forall_data = forall.ForAll;

        // ForAll quantifier: ∀x ∈ array : condition
        // Implementation: iterate through array, return false if any element fails condition

        // Generate array expression
        try self.generator.generateExpression(forall_data.array, true, false);

        // Add bound variable to symbol table so it can be referenced in the condition
        const bound_var_name = forall_data.variable.lexeme;
        _ = try self.generator.symbol_table.getOrCreateVariable(bound_var_name);

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
                                const var_name = var_token.lexeme;
                                if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                                    try self.generator.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .file_uri = var_token.file_uri,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.generator.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
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
                                const var_name = var_token.lexeme;
                                if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                                    try self.generator.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .file_uri = var_token.file_uri,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.generator.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
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
                .function_index = 0,
                .qualified_name = operator_name,
                .arg_count = 2, // array + comparison value
                .call_kind = .BuiltinFunction,
                .target_module = null,
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

        // Add bound variable to symbol table so it can be referenced in the condition
        const bound_var_name = exists_data.variable.lexeme;
        _ = try self.generator.symbol_table.getOrCreateVariable(bound_var_name);

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
                                const var_name = var_token.lexeme;
                                if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                                    try self.generator.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .file_uri = var_token.file_uri,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.generator.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
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
                                const var_name = var_token.lexeme;
                                if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                                    try self.generator.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .file_uri = var_token.file_uri,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.generator.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
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
                .function_index = 0,
                .qualified_name = operator_name,
                .arg_count = 2, // array + comparison value
                .call_kind = .BuiltinFunction,
                .target_module = null,
                .return_type = .Tetra,
            },
        });
    }

    /// Generate HIR for increment operations
    pub fn generateIncrement(self: *CollectionsHandler, operand: *ast.Expr) !void {
        if (operand.data == .Variable) {
            const var_name = operand.data.Variable.lexeme;
            const var_idx = try self.generator.getOrCreateVariable(var_name);

            // Load current value
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx,
                    .var_name = var_name,
                    .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                    .module_context = null,
                },
            });

            // Add 1 (create constant 1)
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Add the values
            const operand_type = self.generator.getTrackedVariableType(var_name) orelse .Int;
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });

            // Duplicate result so we can both return it and store it
            try self.generator.instructions.append(.Dup);

            // Store back to variable
            try self.generator.instructions.append(.{
                .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = var_name,
                    .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                    .module_context = null,
                    .expected_type = operand_type,
                },
            });
        } else {
            // For non-variable expressions, generate the expression and add 1
            try self.generator.generateExpression(operand, true, false);

            // Add 1
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Add the values
            const operand_type = self.generator.inferTypeFromExpression(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });
        }
    }

    /// Generate HIR for decrement operations
    pub fn generateDecrement(self: *CollectionsHandler, operand: *ast.Expr) !void {
        // Generate decrement operation: load variable, subtract 1, store back
        // First, check if this is a variable reference
        if (operand.data == .Variable) {
            const var_name = operand.data.Variable.lexeme;
            const var_idx = try self.generator.getOrCreateVariable(var_name);

            // Load current value
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx,
                    .var_name = var_name,
                    .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                    .module_context = null,
                },
            });

            // Add 1 (create constant 1)
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Subtract the values
            const operand_type = self.generator.getTrackedVariableType(var_name) orelse .Int;
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });

            // Duplicate result so we can both return it and store it
            try self.generator.instructions.append(.Dup);

            // Store back to variable
            try self.generator.instructions.append(.{
                .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = var_name,
                    .scope_kind = self.generator.symbol_table.determineVariableScope(var_name),
                    .module_context = null,
                    .expected_type = operand_type,
                },
            });
        } else {
            // For non-variable expressions, generate the expression and subtract 1
            try self.generator.generateExpression(operand, true, false);

            // Add 1
            const one_value = HIRValue{ .int = 1 };
            const one_idx = try self.generator.addConstant(one_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

            // Subtract the values
            const operand_type = self.generator.inferTypeFromExpression(operand);
            try self.generator.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });
        }
    }
};
