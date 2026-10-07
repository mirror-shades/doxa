const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const precedence = @import("./precedence.zig");
const Precedence = precedence.Precedence;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

/// `@name(value, args...)`: an intrinsic call. Its first argument is the value
/// it acts on (the receiver); an intrinsic called with none receives `nothing`.
pub fn internalCallExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();
    const args = try parseArguments(self);

    if (args.len == 0) {
        const nothing = try self.allocator.create(ast.Expr);
        nothing.* = .{
            .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
            .data = .{ .Literal = .{ .nothing = {} } },
        };
        return try internalCall(self, method_tok, nothing, args);
    }
    defer self.allocator.free(args);
    return try internalCall(self, method_tok, args[0], try self.allocator.dupe(*ast.Expr, args[1..]));
}

/// Whether `token_type` names an intrinsic that acts on a value, and so may
/// be called postfix. `@std()` takes no value.
pub fn isPostfixIntrinsic(token_type: token.TokenType) bool {
    if (token_type == .STD) return false;
    const prefix = precedence.getRule(token_type).prefix orelse return false;
    return prefix == &Parser.internalCallExpr;
}

/// `value.@name(args...)`: the same call `@name(value, args...)` spells, with
/// `value` as the receiver (docs/methods.md). The parser stands on the `@name`
/// token.
pub fn postfixInternalCall(self: *Parser, receiver: *ast.Expr) ErrorList!*ast.Expr {
    const method_tok = self.peek();
    self.advance();
    return internalCall(self, method_tok, receiver, try parseArguments(self));
}

/// A parenthesized, comma-separated argument list, newlines allowed between
/// arguments and around the parentheses' contents.
fn parseArguments(self: *Parser) ErrorList![]*ast.Expr {
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
                if (self.peek().type == .RIGHT_PAREN) break;
                continue;
            }
            break;
        }
    }

    while (self.peek().type == .NEWLINE) self.advance();
    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance();
    return args.toOwnedSlice();
}

fn internalCall(self: *Parser, method_tok: token.Token, receiver: *ast.Expr, arguments: []*ast.Expr) ErrorList!*ast.Expr {
    const call = try self.allocator.create(ast.Expr);
    call.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .InternalCall = .{
            .receiver = receiver,
            .method = method_tok,
            .arguments = arguments,
        } },
    };
    return call;
}
