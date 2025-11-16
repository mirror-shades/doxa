const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const Reporting = @import("../utils/reporting.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const Precedence = @import("./precedence.zig").Precedence;
const expression_parser = @import("./expression_parser.zig");
const precedence = @import("./precedence.zig");

pub fn existentialQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const bound_variable = self.peek();
    self.advance();

    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    if (self.peek().type == .ARRAY_TYPE) {
        self.advance();
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .Variable = token.Token{
                    .type = .IDENTIFIER,
                    .lexeme = "array",
                    .literal = .{ .nothing = {} },
                    .line = 0,
                    .column = 0,
                    .file = "",
                    .file_uri = "",
                },
            },
        };

        if (self.peek().type != .WHERE) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const exists_expr = try self.allocator.create(ast.Expr);
        exists_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .Exists = .{
                    .variable = bound_variable,
                    .array = array_expr,
                    .condition = condition,
                },
            },
        };
        return exists_expr;
    } else {
        const array_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
        if (self.peek().type != .WHERE) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const exists_expr = try self.allocator.create(ast.Expr);
        exists_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .Exists = .{
                    .variable = bound_variable,
                    .array = array_expr,
                    .condition = condition,
                },
            },
        };
        return exists_expr;
    }
}

pub fn universalQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const bound_variable = self.peek();
    self.advance();

    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    if (self.peek().type == .ARRAY_TYPE) {
        self.advance();
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .Variable = token.Token{
                    .type = .IDENTIFIER,
                    .lexeme = "array",
                    .literal = .{ .nothing = {} },
                    .line = 0,
                    .column = 0,
                    .file = "",
                    .file_uri = "",
                },
            },
        };

        if (self.peek().type != .WHERE) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const forall_expr = try self.allocator.create(ast.Expr);
        forall_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .ForAll = .{
                    .variable = bound_variable,
                    .array = array_expr,
                    .condition = condition,
                },
            },
        };
        return forall_expr;
    } else {
        const array_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.peek().type != .WHERE) {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedWhereKeyword;
        }
        self.advance();

        const condition = try expression_parser.parseExpression(self) orelse {
            array_expr.deinit(self.allocator);
            self.allocator.destroy(array_expr);
            return error.ExpectedExpression;
        };

        const forall_expr = try self.allocator.create(ast.Expr);
        forall_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(bound_variable),
            },
            .data = .{
                .ForAll = .{
                    .variable = bound_variable,
                    .array = array_expr,
                    .condition = condition,
                },
            },
        };
        return forall_expr;
    }
}

pub fn inOperator(self: *Parser, left: ?*ast.Expr, prec: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    const array_expr = try precedence.parsePrecedence(self, @enumFromInt(@intFromEnum(prec) + 1)) orelse {
        return error.ExpectedArrayExpression;
    };
    errdefer {
        array_expr.deinit(self.allocator);
        self.allocator.destroy(array_expr);
    }

    if (self.peek().type == .WHERE) {
        self.advance();

        const condition = try expression_parser.parseExpression(self) orelse {
            return error.ExpectedExpression;
        };

        const exists_expr = try self.allocator.create(ast.Expr);
        exists_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(left.?.data.Variable),
            },
            .data = .{
                .Exists = .{
                    .variable = left.?.data.Variable, // We know left is a Variable
                    .array = array_expr,
                    .condition = condition,
                },
            },
        };
        return exists_expr;
    }

    const in_expr = try self.allocator.create(ast.Expr);
    in_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(left.?.data.Variable),
        },
        .data = .{
            .Binary = .{
                .left = left,
                .operator = self.tokens[self.current - 1],
                .right = array_expr,
            },
        },
    };
    return in_expr;
}
