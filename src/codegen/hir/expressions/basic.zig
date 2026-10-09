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
                    const value_type = try self.generator.typeOf(expr);
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

    /// Generate HIR for `this`: the receiver, an alias parameter of the method
    /// being lowered.
    pub fn generateThis(self: *BasicExpressionHandler) (std.mem.Allocator.Error || ErrorList)!void {
        const this_slot = self.generator.this_slot orelse return ErrorList.InvalidAliasArgument;
        try self.generator.instructions.append(.{ .LoadAlias = .{
            .slot = this_slot,
            .var_name = "this",
            .slot_index = self.generator.alias_params.get(this_slot).?,
        } });
    }

    /// Generate HIR for a `.Variant` shorthand: a variant of the enum the
    /// analyzer typed it as, from what its position expects.
    pub fn generateEnumMember(self: *BasicExpressionHandler, expr: *ast.Expr) (std.mem.Allocator.Error || ErrorList)!void {
        const member = expr.data.EnumMember;
        const location = ast.SourceSpan.fromToken(member).location;
        const type_info = try self.generator.typeInfoOf(expr);
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
