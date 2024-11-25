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
    current: u32,
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

            const stmt = if (current.type == .VAR)
                try self.parseVarDecl()
            else
                try self.parseExpressionStmt();

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
        } };
    }

    fn parseExpressionStmt(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing expression statement...\n", .{});
        }

        const expr: ?*ast.Expr = self.parseExpression() catch |err| {
            return err;
        };

        // Expect semicolon
        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected SEMICOLON, got {s}\n", .{@tagName(self.peek().type)});
            }
            if (expr) |e| {
                e.deinit(self.allocator);
                self.allocator.destroy(e);
            }
            return error.ExpectedSemicolon;
        }
        self.advance();

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed expression statement\n", .{});
        }

        return ast.Stmt{ .Expression = expr };
    }

    fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {
        return try self.parseComparison();
    }

    fn parseComparison(self: *Parser) ErrorList!?*ast.Expr {
        var expr = (try self.parseTerm()) orelse return null;
        errdefer expr.deinit(self.allocator);

        while (true) {
            const current = self.peek();
            if (current.type != .GREATER and
                current.type != .GREATER_EQUAL and
                current.type != .LESS and
                current.type != .LESS_EQUAL and
                current.type != .EQUALITY and
                current.type != .BANG_EQUAL) break;

            const next = self.peek_next();
            if (next.type == .SEMICOLON or next.type == .EOF) break;

            self.advance();
            const right = try self.parseTerm();
            if (right == null) return error.ExpectedExpression;

            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = current,
                .right = right,
            } };
            expr = binary;

            if (self.debug_enabled) {
                std.debug.print("Found comparison expression with operator: {s}\n", .{@tagName(current.type)});
            }
        }

        return expr;
    }

    fn parseTerm(self: *Parser) ErrorList!?*ast.Expr {
        var expr = try self.parseFactor();
        errdefer if (expr) |e| {
            e.deinit(self.allocator);
            self.allocator.destroy(e);
        };

        while (true) {
            const current = self.peek();
            if (current.type != .PLUS and current.type != .MINUS) break;

            // Save the operator and advance
            const operator = current;
            self.advance();

            // Try to parse the right side
            const right = try self.parseFactor();
            if (right == null) {
                // If we can't parse the right side after seeing an operator,
                // that's an error
                return error.ExpectedExpression;
            }

            if (expr) |left| {
                const binary = try self.allocator.create(ast.Expr);
                binary.* = ast.Expr{ .Binary = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                } };
                expr = binary;

                if (self.debug_enabled) {
                    std.debug.print("Found term expression with operator: {s}\n", .{@tagName(operator.type)});
                }
            }
        }

        return expr;
    }

    fn parseFactor(self: *Parser) ErrorList!?*ast.Expr {
        var expr = try self.parseUnary();
        errdefer if (expr) |e| {
            e.deinit(self.allocator);
            self.allocator.destroy(e);
        };

        while (true) {
            const current = self.peek();
            if (current.type != .ASTERISK and current.type != .SLASH) break;

            // Save the operator and advance
            const operator = current;
            self.advance();

            // Try to parse the right side
            const right = try self.parseUnary();
            if (right == null) {
                // If we can't parse the right side after seeing an operator,
                // that's an error
                return error.ExpectedExpression;
            }

            if (expr) |left| {
                const binary = try self.allocator.create(ast.Expr);
                binary.* = ast.Expr{ .Binary = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                } };
                expr = binary;

                if (self.debug_enabled) {
                    std.debug.print("Found factor expression with operator: {s}\n", .{@tagName(operator.type)});
                }
            }
        }

        return expr;
    }

    fn parseUnary(self: *Parser) ErrorList!?*ast.Expr {
        if (self.peek().type == .MINUS or self.peek().type == .BANG) {
            const operator = self.peek();
            self.advance();
            const right = try self.parseUnary();
            if (right == null) return error.ExpectedExpression;

            const unary = try self.allocator.create(ast.Expr);
            unary.* = ast.Expr{ .Unary = .{
                .operator = operator,
                .right = right,
            } };

            if (self.debug_enabled) {
                std.debug.print("Found unary expression with operator: {s}\n", .{@tagName(operator.type)});
            }
            return unary;
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) std.debug.print("Parsing primary expression...\n", .{});

        const tok = self.peek();
        const expr = try self.allocator.create(ast.Expr);

        var result: ?*ast.Expr = null;

        switch (tok.type) {
            .INT, .FLOAT, .STRING, .BOOL => {
                expr.* = ast.Expr{ .Literal = tok.literal };
                if (self.debug_enabled) {
                    std.debug.print("Found literal: {any}\n", .{tok.literal});
                }
                self.advance();
                result = expr;
            },
            .IDENTIFIER => {
                expr.* = ast.Expr{ .Variable = tok };
                if (self.debug_enabled) {
                    std.debug.print("Found variable reference: {s}\n", .{tok.lexeme});
                }
                self.advance();
                result = expr;
            },
            .LEFT_PAREN => {
                self.advance();
                const subexpr = (try self.parseExpression()) orelse {
                    self.allocator.destroy(expr);
                    return error.ExpectedExpression;
                };
                if (self.peek().type != .RIGHT_PAREN) {
                    // Clean up allocated memory before returning error
                    subexpr.deinit(self.allocator);
                    self.allocator.destroy(subexpr);
                    self.allocator.destroy(expr);
                    return error.ExpectedRightParen;
                }
                self.advance();
                expr.* = ast.Expr{ .Grouping = subexpr };
                result = expr;
            },
            else => {
                self.allocator.destroy(expr);
                return null;
            },
        }

        return result;
    }

    fn peek_next(self: *Parser) token.Token {
        if (self.current + 1 >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[self.current + 1];
    }
};
