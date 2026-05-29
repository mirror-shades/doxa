const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const parser_types = @import("parser_types.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const Parser = @import("parser_types.zig").Parser;
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("statement_parser.zig");
const expression_parser = @import("expression_parser.zig");
const declaration_parser = @import("declaration_parser.zig");

pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
    return parser_types.execute(self);
}

fn array(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    var elements = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer elements.deinit();

    if (self.peek().type != .RIGHT_BRACKET) {
        const first_element = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
        try elements.append(first_element);

        const first_type = literalTokenType(first_element.*);

        while (self.peek().type == .COMMA) {
            self.advance();
            if (self.peek().type == .RIGHT_BRACKET) break;

            const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            const element_type = literalTokenType(element.*);

            if (element_type != first_type) {
                element.deinit(self.allocator);
                self.allocator.destroy(element);
                return error.HeterogeneousArray;
            }

            try elements.append(element);
        }
    }

    if (self.peek().type != .RIGHT_BRACKET) {
        return error.ExpectedRightBracket;
    }
    self.advance();

    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{ .Array = try elements.toOwnedSlice() };
    return array_expr;
}

fn literalTokenType(expr: ast.Expr) token.TokenType {
    return switch (expr) {
        .Literal => |lit| switch (lit) {
            .int => .INT_TYPE,
            .float => .FLOAT_TYPE,
            .string => .STRING_TYPE,
            .tetra => .TETRA_TYPE,
            .nothing => .NOTHING,
            .array => .ARRAY_TYPE,
            .struct_value => .STRUCT_TYPE,
            .function => .FUNCTION_KEYWORD,
            .enum_variant => .ENUM_TYPE,
            .map => .MAP_TYPE,
        },
        else => .IDENTIFIER,
    };
}

fn map(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    var entries = std.array_list.Managed(ast.MapEntry).init(self.allocator);
    errdefer {
        for (entries.items) |*entry| {
            entry.key.deinit(self.allocator);
            self.allocator.destroy(entry.key);
            entry.value.deinit(self.allocator);
            self.allocator.destroy(entry.value);
        }
        entries.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const key = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.peek().type != .WHERE_SYMBOL) {
            if (entries.items.len == 0) {
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                return self.block(null, .NONE);
            }
            return error.ExpectedColon;
        }
        self.advance();

        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        try entries.append(.{
            .key = key,
            .value = value,
        });

        if (self.peek().type == .COMMA) {
            self.advance();
            if (self.peek().type == .RIGHT_BRACE) break;
        } else if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedCommaOrBrace;
        }
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance();

    return error.MapLiteralsMustBeStatements;
}
