const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const HIRGeneratorType = @import("../soxa_generator.zig").HIRGenerator;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Handle basic expression types: literals, variables, and grouping
pub const BasicExpressionHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) BasicExpressionHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for literal expressions
    pub fn generateLiteral(self: *BasicExpressionHandler, lit: ast.TokenLiteral) (std.mem.Allocator.Error || ErrorList)!void {
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
    }

    /// Generate HIR for variable access
    pub fn generateVariable(self: *BasicExpressionHandler, var_token: ast.Token) (std.mem.Allocator.Error || ErrorList)!void {
        // Compile-time validation: Ensure variable has been declared
        const maybe_idx: ?u32 = self.generator.symbol_table.getVariable(var_token.lexeme);
        if (maybe_idx) |existing_idx| {
            const var_idx = existing_idx;
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx,
                    .var_name = var_token.lexeme,
                    .scope_kind = .Local, // TODO: determine actual scope
                    .module_context = null,
                },
            });
        } else {
            // Ensure the variable exists in the current scope and load it at runtime
            const var_idx2 = try self.generator.getOrCreateVariable(var_token.lexeme);
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx2,
                    .var_name = var_token.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                },
            });
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
        // Load implicit receiver 'this' from local variable
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
            // Fallback to string constant if no enum context
            const enum_value = HIRValue{ .string = member.lexeme };
            const const_idx = try self.generator.addConstant(enum_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
        }
    }

    /// Generate HIR for default argument placeholders
    pub fn generateDefaultArgPlaceholder(self: *BasicExpressionHandler) (std.mem.Allocator.Error || ErrorList)!void {
        // Push nothing for default arguments - they should be replaced by the caller
        const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
    }
};
