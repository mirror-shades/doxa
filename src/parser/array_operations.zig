const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");
const Precedence = @import("./precedence.zig").Precedence;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("core_parser.zig").Parser;

pub fn arrayPush(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (array == null) return error.ExpectedExpression;

    const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    const push_expr = try self.allocator.create(ast.Expr);
    push_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayPush = .{
                .array = array.?,
                .element = element,
            },
        },
    };

    return push_expr;
}

pub fn arrayIsEmpty(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (array == null) return error.ExpectedExpression;

    const empty_expr = try self.allocator.create(ast.Expr);
    empty_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayIsEmpty = .{ .array = array.? },
        },
    };
    return empty_expr;
}

pub fn arrayPop(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (array == null) return error.ExpectedExpression;

    const pop_expr = try self.allocator.create(ast.Expr);
    pop_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayPop = .{
                .array = array.?,
            },
        },
    };

    return pop_expr;
}

pub fn arrayConcat(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (array == null) return error.ExpectedExpression;

    const array2 = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    const concat_expr = try self.allocator.create(ast.Expr);
    concat_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayConcat = .{
                .array = array.?,
                .array2 = array2,
            },
        },
    };
    return concat_expr;
}

pub fn arrayLength(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (array == null) return error.ExpectedExpression;

    const length_expr = try self.allocator.create(ast.Expr);
    length_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayLength = .{
                .array = array.?,
            },
        },
    };

    return length_expr;
}
