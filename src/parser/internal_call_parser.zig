const std = @import("std");
const ast = @import("../ast/ast.zig");
const Precedence = @import("./precedence.zig").Precedence;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

pub fn internalCallExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const method_tok = self.peek();

    self.advance();
    if (self.peek().type != .LEFT_PAREN) return error.ExpectedLeftParen;
    self.advance();

    while (self.peek().type == .NEWLINE) self.advance();

    var args = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer {
        for (args.items) |arg| {
            arg.deinit(self.allocator);
            self.allocator.destroy(arg);
        }
        args.deinit();
    }

    if (self.peek().type != .RIGHT_PAREN) {
        while (true) {
            while (self.peek().type == .NEWLINE) self.advance();
            const expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            try args.append(expr);
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .COMMA) {
                self.advance();
                while (self.peek().type == .NEWLINE) self.advance();
                if (self.peek().type == .RIGHT_PAREN) {
                    break;
                }
                continue;
            }
            break;
        }
    }

    while (self.peek().type == .NEWLINE) self.advance();
    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance();

    var receiver_expr: *ast.Expr = undefined;
    var call_args = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer call_args.deinit();

    if (args.items.len > 0) {
        receiver_expr = args.items[0];
        var i: usize = 1;
        while (i < args.items.len) : (i += 1) {
            try call_args.append(args.items[i]);
        }
    } else {
        receiver_expr = try self.allocator.create(ast.Expr);
        receiver_expr.* = .{
            .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
            .data = .{ .Literal = .{ .nothing = {} } },
        };
    }

    const method_expr = try self.allocator.create(ast.Expr);
    method_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .InternalCall = .{
            .receiver = receiver_expr,
            .method = method_tok,
            .arguments = try call_args.toOwnedSlice(),
        } },
    };

    return method_expr;
}

