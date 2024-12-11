const std = @import("std");
const Parser = @import("parser_types.zig").Parser;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const ErrorList = @import("../reporting.zig").ErrorList;
const Precedence = @import("precedence.zig").Precedence;
const expression_parser = @import("./expression_parser.zig");
const precedence = @import("./precedence.zig");

pub fn existentialQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing existential quantifier...\n", .{});
    }

    self.advance(); // consume 'exists'

    // Parse the bound variable
    if (self.peek().type != .IDENTIFIER) {
        if (self.debug_enabled) {
            std.debug.print("Expected identifier, found: {s}\n", .{@tagName(self.peek().type)});
        }
        return error.ExpectedIdentifier;
    }
    const bound_variable = self.peek();
    self.advance();

    // Parse 'in' keyword
    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    // Parse array expression (can be identifier or array type)
    if (self.peek().type == .ARRAY_TYPE) {
        // Create array type expression
        self.advance(); // consume ARRAY_TYPE
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{ .Variable = .{
            .lexeme = "array",
            .type = .IDENTIFIER,
            .literal = .{ .nothing = {} },
            .line = 0,
            .column = 0,
        } };

        if (self.debug_enabled) {
            std.debug.print("After array type, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
        }

        // Parse 'where' keyword
        if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        // Parse the condition
        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const exists_expr = try self.allocator.create(ast.Expr);
        exists_expr.* = .{ .Exists = .{
            .variable = bound_variable,
            .array = array_expr,
            .condition = condition,
        } };
        return exists_expr;
    } else {
        // Parse regular expression for array
        const array_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.debug_enabled) {
            std.debug.print("After array expression, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
        }

        // Parse 'where' keyword
        if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        // Parse the condition
        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const exists_expr = try self.allocator.create(ast.Expr);
        exists_expr.* = .{ .Exists = .{
            .variable = bound_variable,
            .array = array_expr,
            .condition = condition,
        } };
        return exists_expr;
    }
}

pub fn universalQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing universal quantifier...\n", .{});
    }

    self.advance(); // consume 'forall'

    // Parse the bound variable
    if (self.peek().type != .IDENTIFIER) {
        if (self.debug_enabled) {
            std.debug.print("Expected identifier, found: {s}\n", .{@tagName(self.peek().type)});
        }
        return error.ExpectedIdentifier;
    }
    const bound_variable = self.peek();
    self.advance();

    // Parse 'in' keyword
    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    // Parse array expression (can be identifier or array type)
    if (self.peek().type == .ARRAY_TYPE) {
        // Create array type expression
        self.advance(); // consume ARRAY_TYPE
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{ .Variable = .{
            .lexeme = "array",
            .type = .IDENTIFIER,
            .literal = .{ .nothing = {} },
            .line = 0,
            .column = 0,
        } };

        if (self.debug_enabled) {
            std.debug.print("After array type, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
        }

        // Parse 'where' keyword
        if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        // Parse the condition
        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const forall_expr = try self.allocator.create(ast.Expr);
        forall_expr.* = .{ .ForAll = .{
            .variable = bound_variable,
            .array = array_expr,
            .condition = condition,
        } };
        return forall_expr;
    } else {
        // Parse regular expression for array
        const array_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.debug_enabled) {
            std.debug.print("After array expression, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
        }

        // Parse 'where' keyword
        if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        // Parse the condition
        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const forall_expr = try self.allocator.create(ast.Expr);
        forall_expr.* = .{ .ForAll = .{
            .variable = bound_variable,
            .array = array_expr,
            .condition = condition,
        } };
        return forall_expr;
    }
}

pub fn inOperator(self: *Parser, left: ?*ast.Expr, prec: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    // Parse the array expression
    const array_expr = try precedence.parsePrecedence(self, @enumFromInt(@intFromEnum(prec) + 1)) orelse
        return error.ExpectedArrayExpression;

    const in_expr = try self.allocator.create(ast.Expr);
    in_expr.* = .{ .Binary = .{
        .left = left,
        .operator = self.tokens[self.current - 1],
        .right = array_expr,
    } };
    return in_expr;
}