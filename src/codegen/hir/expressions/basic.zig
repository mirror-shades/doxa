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
const Location = @import("../../../utils/reporting.zig").Location;
const graph = @import("../../../module/graph.zig");

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
            else => HIRValue.nothing,
        };
        const const_idx = try self.generator.addConstant(hir_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = hir_value, .constant_id = const_idx } });

        // Pop the result if it's not needed
        if (!preserve_result and should_pop_after_use) {
            try self.generator.instructions.append(.Pop);
        }
    }

    pub fn generateInterpolatedString(self: *BasicExpressionHandler, template: *ast.FormatTemplate, preserve_result: bool, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        var has_value = false;

        for (template.parts) |part| {
            switch (part) {
                .String => |text| {
                    if (text.len == 0 and has_value) continue;
                    const value = HIRValue{ .string = text };
                    const const_idx = try self.generator.addConstant(value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = value, .constant_id = const_idx } });
                },
                .Expression => |expr| {
                    // B2: a struct interpolated into a string is printed, so its
                    // descriptor must stay registered.
                    const value_type = self.generator.inferTypeFromExpression(expr);
                    self.generator.markReflectedType(value_type);
                    try self.generator.generateExpression(expr, true, false);
                    try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString, .value_type = value_type } });
                },
            }

            if (has_value) {
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
            } else {
                has_value = true;
            }
        }

        if (!has_value) {
            const value = HIRValue{ .string = "" };
            const const_idx = try self.generator.addConstant(value);
            try self.generator.instructions.append(.{ .Const = .{ .value = value, .constant_id = const_idx } });
        }

        if (!preserve_result and should_pop_after_use) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Generate HIR for variable access
    pub fn generateVariable(self: *BasicExpressionHandler, var_token: ast.Token) (std.mem.Allocator.Error || ErrorList)!void {
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
                    return ErrorList.InvalidAliasArgument;
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
                    return ErrorList.InvalidAliasArgument;
                }
                return;
            }

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
                return ErrorList.InvalidAliasArgument;
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

    /// Generate HIR for a `.Variant` shorthand: a variant of the enum the
    /// analyzer typed it as, from what its position expects.
    pub fn generateEnumMember(self: *BasicExpressionHandler, expr: *ast.Expr) (std.mem.Allocator.Error || ErrorList)!void {
        const member = expr.data.EnumMember;
        const location = ast.SourceSpan.fromToken(member).location;
        const type_info = self.generator.semantic.getCachedExprType(expr) orelse return ErrorList.MissingExpressionType;
        // Analysis rejects a shorthand its context left untyped.
        const key = self.generator.typeKeyOf(type_info.*).?;

        const enum_value = HIRValue{
            .enum_variant = HIREnum{
                .type_name = key,
                .variant_name = member.lexeme,
                .variant_index = try self.resolveEnumVariantIndex(key, member.lexeme, location),
                .path = null,
            },
        };
        const const_idx = try self.generator.addConstant(enum_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
    }

    fn resolveEnumVariantIndex(self: *BasicExpressionHandler, enum_key: []const u8, variant_name: []const u8, location: Location) ErrorList!u32 {
        const custom_type = self.generator.type_system.custom_types.get(enum_key).?;
        if (custom_type.kind != .Enum) {
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "'{s}' is not an enum type",
                .{graph.displayName(enum_key)},
            );
            return ErrorList.TypeMismatch;
        }
        if (custom_type.getEnumVariantIndex(variant_name)) |index| return index;
        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.VARIABLE_NOT_FOUND,
            "Unknown enum variant '{s}' for enum '{s}'",
            .{ variant_name, graph.displayName(enum_key) },
        );
        return ErrorList.InvalidEnumVariant;
    }

    /// Generate HIR for default argument placeholders
    pub fn generateDefaultArgPlaceholder(self: *BasicExpressionHandler) (std.mem.Allocator.Error || ErrorList)!void {
        // Push nothing for default arguments - they should be replaced by the caller
        const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
    }
};
