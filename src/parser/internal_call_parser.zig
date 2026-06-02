const std = @import("std");
const ast = @import("../ast/ast.zig");
const Precedence = @import("./precedence.zig").Precedence;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

pub fn internalCallExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const method_tok = self.peek();

    if (method_tok.type == .PRINT) {
        return try parsePrintMethod(self);
    }

    if (method_tok.type == .EXIT) {
        return try parseExitMethod(self);
    }

    if (method_tok.type == .STD) {
        return try parseStdMethod(self);
    }

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

pub fn parsePrintMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const format_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const args = try self.allocator.alloc(*ast.Expr, 1);
    args[0] = format_expr;

    const print_expr = try self.allocator.create(ast.Expr);
    print_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(method_tok),
        },
        .data = .{
            .BuiltinCall = .{
                .function = method_tok,
                .arguments = args,
            },
        },
    };

    return print_expr;
}

pub fn parseExitMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const exit_code_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const arguments = try self.allocator.alloc(*ast.Expr, 1);
    arguments[0] = exit_code_expr;

    const exit_expr = try self.allocator.create(ast.Expr);
    exit_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = arguments,
        } },
    };

    return exit_expr;
}

pub fn parseStdMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const std_expr = try self.allocator.create(ast.Expr);
    std_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = &.{},
        } },
    };

    return std_expr;
}

