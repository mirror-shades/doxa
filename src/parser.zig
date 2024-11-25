const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const ReportingModule = @import("reporting.zig");
const Reporting = ReportingModule.Reporting;
const ErrorList = ReportingModule.ErrorList;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const token.Token,
    current: usize,
    debug_enabled: bool,

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, debug_enabled: bool) !Parser {
        if (tokens.len == 0) return error.EmptyTokenList;
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .debug_enabled = debug_enabled,
        };
    }

    pub fn deinit(_: *Parser) void {}

    fn peek(self: *Parser) token.Token {
        // Ensure we have tokens to look at
        if (self.tokens.len == 0) {
            return token.Token{
                .type = .EOF,
                .lexeme = "",
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            };
        }

        // If we're at or past the end, return the last token (which should be EOF)
        if (self.current >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }

        return self.tokens[self.current];
    }

    fn advance(self: *Parser) void {
        // Only advance if we're not at the end
        if (self.current < self.tokens.len - 1) {
            self.current += 1;
        }
    }

    pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        defer statements.deinit(); // In case of error

        while (true) {
            const current = self.peek();
            if (current.type == .EOF) break;

            const stmt = try self.parseVarDecl();
            try statements.append(stmt);
        }

        return statements.toOwnedSlice();
    }

    fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nAttempting to parse var declaration...\n", .{});
        }

        // var
        const tok = self.peek();
        if (tok.type != .VAR) {
            if (self.debug_enabled) {
                std.debug.print("Expected VAR, got {s}\n", .{@tagName(tok.type)});
            }
            return error.UnexpectedToken;
        }
        if (self.debug_enabled) std.debug.print("Found VAR token\n", .{});
        self.advance();

        // identifier
        const id_tok = self.peek();
        if (id_tok.type != .IDENTIFIER) {
            if (self.debug_enabled) {
                std.debug.print("Expected IDENTIFIER, got {s}\n", .{@tagName(id_tok.type)});
            }
            return error.ExpectedIdentifier;
        }
        if (self.debug_enabled) std.debug.print("Found identifier: '{s}'\n", .{id_tok.lexeme});
        self.advance();

        // =
        const eq_tok = self.peek();
        if (eq_tok.type != .ASSIGN) {
            if (self.debug_enabled) {
                std.debug.print("Expected assignment operator, got {s}\n", .{@tagName(eq_tok.type)});
            }
            return error.ExpectedAssignmentOperator;
        }
        if (self.debug_enabled) std.debug.print("Found assignment operator\n", .{});
        self.advance();

        // Parse initializer expression
        const initializer = try self.parseExpression();

        // ;
        const semi_tok = self.peek();
        if (semi_tok.type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected SEMICOLON, got {s}\n", .{@tagName(semi_tok.type)});
            }
            return error.ExpectedSemicolon;
        }
        if (self.debug_enabled) std.debug.print("Found semicolon\n", .{});
        self.advance();

        if (self.debug_enabled) {
            std.debug.print("\nSuccessfully parsed var declaration\n", .{});
        }

        const name = id_tok;
        return ast.Stmt{ .VarDecl = .{
            .name = name,
            .initializer = initializer,
        }};
    }

    fn parseExpression(self: *Parser) ErrorList!*ast.Expr {
        return try self.parseEquality();
    }

    fn parseEquality(self: *Parser) ErrorList!*ast.Expr {
        if (self.debug_enabled) std.debug.print("Parsing equality expression...\n", .{});
        
        var expr = try self.parseComparison();

        while (self.peek().type == .EQUALITY or self.peek().type == .BANG_EQUAL) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseComparison();

            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = operator,
                .right = right,
            }};
            expr = binary;
            
            if (self.debug_enabled) {
                std.debug.print("Found equality expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }

        return expr;
    }

    fn parseComparison(self: *Parser) ErrorList!*ast.Expr {
        var expr = try self.parseTerm();

        while (self.peek().type == .GREATER or 
               self.peek().type == .GREATER_EQUAL or 
               self.peek().type == .LESS or 
               self.peek().type == .LESS_EQUAL) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseTerm();
            
            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = operator,
                .right = right,
            }};
            expr = binary;
            
            if (self.debug_enabled) {
                std.debug.print("Found comparison expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }

        return expr;
    }

    fn parseTerm(self: *Parser) ErrorList!*ast.Expr {
        var expr = try self.parseFactor();

        while (self.peek().type == .PLUS or self.peek().type == .MINUS) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseFactor();
            
            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = operator,
                .right = right,
            }};
            expr = binary;
            
            if (self.debug_enabled) {
                std.debug.print("Found term expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }

        return expr;
    }

    fn parseFactor(self: *Parser) ErrorList!*ast.Expr {
        var expr = try self.parseUnary();

        while (self.peek().type == .ASTERISK or self.peek().type == .SLASH) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseUnary();
            
            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = operator,
                .right = right,
            }};
            expr = binary;
            
            if (self.debug_enabled) {
                std.debug.print("Found factor expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }

        return expr;
    }

    fn parseUnary(self: *Parser) ErrorList!*ast.Expr {
        if (self.peek().type == .MINUS or self.peek().type == .BANG) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseUnary();
            
            const unary = try self.allocator.create(ast.Expr);
            unary.* = ast.Expr{ .Unary = .{
                .operator = operator,
                .right = right,
            }};
            
            if (self.debug_enabled) {
                std.debug.print("Found unary expression with operator: {s}\n", .{@tagName(operator.type)});
            }
            return unary;
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ErrorList!*ast.Expr {
        if (self.debug_enabled) std.debug.print("Parsing primary expression...\n", .{});
        
        const tok = self.peek();
        const expr = try self.allocator.create(ast.Expr);

        switch (tok.type) {
            .INT, .FLOAT, .STRING, .BOOL => {
                expr.* = ast.Expr{ .Literal = tok.literal };
                if (self.debug_enabled) {
                    std.debug.print("Found literal: {any}\n", .{tok.literal});
                }
                self.advance();
            },
            .IDENTIFIER => {
                expr.* = ast.Expr{ .Variable = tok };
                if (self.debug_enabled) {
                    std.debug.print("Found variable reference: {s}\n", .{tok.lexeme});
                }
                self.advance();
            },
            .LEFT_PAREN => {
                self.advance();
                const subexpr = try self.parseExpression();
                if (self.peek().type != .RIGHT_PAREN) {
                    // Clean up allocated memory before returning error
                    subexpr.deinit(self.allocator);
                    self.allocator.destroy(subexpr);
                    return error.ExpectedRightParen;
                }
                self.advance();
                return subexpr;
            },
            else => return error.ExpectedExpression,
        }

        return expr;
    }
};