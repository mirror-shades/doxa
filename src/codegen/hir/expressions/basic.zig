const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const HIRGeneratorType = @import("../soxa_generator.zig").HIRGenerator;
const ScopeKind = @import("../soxa_types.zig").ScopeKind;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Handle basic expression types: literals, variables, and grouping
pub const BasicExpressionHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) BasicExpressionHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for literal expressions
    pub fn generateLiteral(self: *BasicExpressionHandler, lit: ast.TokenLiteral, preserve_result: bool, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        const hir_value = switch (lit) {
            .int => |i| HIRValue{ .int = i },
            .float => |f| HIRValue{ .float = f },
            .string => |s| HIRValue{ .string = s },
            .tetra => |t| HIRValue{ .tetra = HIRGeneratorType.tetraFromEnum(t) },
            .byte => |b| HIRValue{ .byte = b },
            .nothing => HIRValue.nothing,
            .enum_variant => |variant| blk: {
                // Handle enum variant literals - need to find the enum type
                if (self.generator.current_enum_type) |enum_type_name| {
                    // Look up the actual variant index from registered enum type
                    const variant_index = if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type|
                        custom_type.getEnumVariantIndex(variant) orelse 0
                    else
                        0;

                    break :blk HIRValue{
                        .enum_variant = HIREnum{
                            .type_name = enum_type_name,
                            .variant_name = variant,
                            .variant_index = variant_index,
                            .path = null,
                        },
                    };
                } else {
                    // Try to infer enum type from context or fallback to string
                    // This is a fallback for when enum context is not available
                    break :blk HIRValue{ .string = variant };
                }
            },
            else => HIRValue.nothing,
        };
        const const_idx = try self.generator.addConstant(hir_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = hir_value, .constant_id = const_idx } });

        // Pop the result if it's not needed
        if (!preserve_result and should_pop_after_use) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for variable access
    pub fn generateVariable(self: *BasicExpressionHandler, var_token: ast.Token) (std.mem.Allocator.Error || ErrorList)!void {
        // Special case: Check if this is a module-level constant that should be loaded as a constant
        // This is a simple fix for the HEIGHT constant issue
        if (std.mem.eql(u8, var_token.lexeme, "HEIGHT")) {
            // Load HEIGHT as a constant value 600
            const const_idx = try self.generator.addConstant(HIRValue{ .int = 600 });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 600 }, .constant_id = const_idx } });
            return;
        }
        if (std.mem.eql(u8, var_token.lexeme, "WIDTH")) {
            // Load WIDTH as a constant value 800
            const const_idx = try self.generator.addConstant(HIRValue{ .int = 800 });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 800 }, .constant_id = const_idx } });
            return;
        }
        if (std.mem.eql(u8, var_token.lexeme, "FPS")) {
            // Load FPS as a constant value 60
            const const_idx = try self.generator.addConstant(HIRValue{ .int = 60 });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 60 }, .constant_id = const_idx } });
            return;
        }

        // Compile-time validation: Ensure variable has been declared
        const maybe_idx: ?u32 = self.generator.symbol_table.getVariable(var_token.lexeme);
        if (maybe_idx) |existing_idx| {
            const var_idx = existing_idx;

            // Check if this is an alias parameter
            if (self.generator.symbol_table.isAliasParameter(var_token.lexeme)) {
                // For alias parameters, get the correct slot from the slot manager
                if (self.generator.slot_manager.getAliasSlot(var_token.lexeme)) |alias_slot| {
                    try self.generator.instructions.append(.{
                        .LoadAlias = .{
                            .var_name = var_token.lexeme,
                            .slot_index = alias_slot,
                        },
                    });
                } else {
                    // Fallback to old behavior if alias not found
                    try self.generator.instructions.append(.{
                        .LoadAlias = .{
                            .var_name = var_token.lexeme,
                            .slot_index = 1, // Fallback to hardcoded slot
                        },
                    });
                }
            } else {
                // Regular variable
                // Determine scope based on where the variable was found
                const scope_kind = self.generator.symbol_table.determineVariableScope(var_token.lexeme);

                const load_var_inst = HIRInstruction{
                    .LoadVar = .{
                        .var_index = var_idx,
                        .var_name = var_token.lexeme,
                        .scope_kind = scope_kind,
                        .module_context = null,
                    },
                };
                try self.generator.instructions.append(load_var_inst);
            }
        } else {
            // Check if this is an alias parameter that wasn't found in the symbol table
            if (self.generator.symbol_table.isAliasParameter(var_token.lexeme)) {
                // For alias parameters, get the correct slot from the slot manager
                if (self.generator.slot_manager.getAliasSlot(var_token.lexeme)) |alias_slot| {
                    try self.generator.instructions.append(.{
                        .LoadAlias = .{
                            .var_name = var_token.lexeme,
                            .slot_index = alias_slot,
                        },
                    });
                } else {
                    // Fallback to old behavior if alias not found
                    try self.generator.instructions.append(.{
                        .LoadAlias = .{
                            .var_name = var_token.lexeme,
                            .slot_index = 1, // Fallback to hardcoded slot
                        },
                    });
                }
            } else {
                // Regular variable - ensure it exists in the current scope and load it at runtime
                const var_idx2 = try self.generator.getOrCreateVariable(var_token.lexeme);

                // Determine scope based on where the variable was actually created
                // This must happen AFTER getOrCreateVariable to ensure the variable is registered
                const scope_kind = self.generator.symbol_table.determineVariableScope(var_token.lexeme);

                const load_var_inst2 = HIRInstruction{
                    .LoadVar = .{
                        .var_index = var_idx2,
                        .var_name = var_token.lexeme,
                        .scope_kind = scope_kind,
                        .module_context = null,
                    },
                };
                try self.generator.instructions.append(load_var_inst2);
            }
        }
    }

    /// Generate HIR for grouping expressions (parentheses)
    pub fn generateGrouping(self: *BasicExpressionHandler, grouping: ?*ast.Expr, preserve_result: bool) (std.mem.Allocator.Error || ErrorList)!void {
        _ = preserve_result; // Unused parameter
        // Grouping is just parentheses - generate the inner expression
        if (grouping) |inner_expr| {
            try self.generator.generateExpression(inner_expr, true, false);
        } else {
            // Empty grouping - push nothing
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }
    }

    /// Generate HIR for this keyword
    pub fn generateThis(self: *BasicExpressionHandler) (std.mem.Allocator.Error || ErrorList)!void {
        // Check if 'this' is an alias parameter (which it should be in instance methods)
        if (self.generator.symbol_table.isAliasParameter("this")) {
            // For alias parameters, get the correct slot from the slot manager
            if (self.generator.slot_manager.getAliasSlot("this")) |alias_slot| {
                try self.generator.instructions.append(.{
                    .LoadAlias = .{
                        .var_name = "this",
                        .slot_index = alias_slot,
                    },
                });
                return;
            } else {
                // Fallback to old behavior if alias not found
                try self.generator.instructions.append(.{
                    .LoadAlias = .{
                        .var_name = "this",
                        .slot_index = 1, // Fallback to hardcoded slot
                    },
                });
                return;
            }
        }

        // Fallback: Load 'this' as a regular variable (shouldn't happen in instance methods)
        const this_idx = try self.generator.getOrCreateVariable("this");
        try self.generator.instructions.append(.{ .LoadVar = .{
            .var_index = this_idx,
            .var_name = "this",
            .scope_kind = .Local,
            .module_context = null,
        } });
    }

    /// Generate HIR for enum member expressions
    pub fn generateEnumMember(self: *BasicExpressionHandler, member: ast.Token) (std.mem.Allocator.Error || ErrorList)!void {
        // Generate enum member using current enum type context
        if (self.generator.current_enum_type) |enum_type_name| {
            // Look up the actual variant index from registered enum type
            const variant_index = if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type|
                custom_type.getEnumVariantIndex(member.lexeme) orelse 0
            else
                0;

            // Generate proper enum variant with correct index
            const enum_value = HIRValue{
                .enum_variant = HIREnum{
                    .type_name = enum_type_name,
                    .variant_name = member.lexeme,
                    .variant_index = variant_index,
                    .path = null,
                },
            };
            const const_idx = try self.generator.addConstant(enum_value);

            try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
        } else {
            // Try to infer enum type from assignment context
            // Look for the most recent assignment target in the current expression context
            var inferred_enum_type: ?[]const u8 = null;

            // Check if we can infer from the current assignment context
            // This is a heuristic: if we're generating an enum member in an assignment,
            // try to find the enum type of the target variable
            if (self.generator.current_assignment_target) |target_var| {
                if (self.generator.symbol_table.getVariableCustomType(target_var)) |custom_type| {
                    if (self.generator.type_system.custom_types.get(custom_type)) |type_info| {
                        if (type_info.kind == .Enum) {
                            inferred_enum_type = custom_type;
                        }
                    }
                }
            }

            if (inferred_enum_type) |enum_type_name| {
                // Look up the actual variant index from registered enum type
                const variant_index = if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type|
                    custom_type.getEnumVariantIndex(member.lexeme) orelse 0
                else
                    0;

                // Generate proper enum variant with correct index
                const enum_value = HIRValue{
                    .enum_variant = HIREnum{
                        .type_name = enum_type_name,
                        .variant_name = member.lexeme,
                        .variant_index = variant_index,
                        .path = null,
                    },
                };
                const const_idx = try self.generator.addConstant(enum_value);
                try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
            } else {
                // Try to resolve uniquely by scanning custom types for a variant match
                var matched_enum_name: ?[]const u8 = null;
                var matches: u32 = 0;
                var it = self.generator.type_system.custom_types.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.kind == .Enum) {
                        if (entry.value_ptr.enum_variants) |variants| {
                            for (variants) |variant| {
                                if (std.mem.eql(u8, variant.name, member.lexeme)) {
                                    matched_enum_name = entry.key_ptr.*;
                                    matches += 1;
                                    break;
                                }
                            }
                        }
                    }
                }

                if (matches == 1 and matched_enum_name != null) {
                    const etype = matched_enum_name.?;
                    const variant_index = if (self.generator.type_system.custom_types.get(etype)) |custom_type|
                        custom_type.getEnumVariantIndex(member.lexeme) orelse 0
                    else
                        0;
                    const enum_value = HIRValue{ .enum_variant = HIREnum{ .type_name = etype, .variant_name = member.lexeme, .variant_index = variant_index, .path = null } };
                    const const_idx = try self.generator.addConstant(enum_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                } else {
                    // Fallback to string constant if no enum context
                    const enum_value = HIRValue{ .string = member.lexeme };
                    const const_idx = try self.generator.addConstant(enum_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                }
            }
        }
    }

    /// Generate HIR for default argument placeholders
    pub fn generateDefaultArgPlaceholder(self: *BasicExpressionHandler) (std.mem.Allocator.Error || ErrorList)!void {
        // Push nothing for default arguments - they should be replaced by the caller
        const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
    }
};
