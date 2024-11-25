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

    pub fn parse(self: *Parser) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("ASTERISKting parse with {} tokens\n", .{self.tokens.len});
        }

        while (true) {
            const current = self.peek();
            if (current.type == .EOF) break;

            if (self.debug_enabled) {
                std.debug.print("\nToken: {s} '{s}'\n", .{ @tagName(current.type), current.lexeme });
                std.debug.print("Current position: {}\n", .{self.current});
            }

            try self.parseVarDecl();
        }
    }

    fn parseVarDecl(self: *Parser) ErrorList!void {
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

        // Parse expression instead of just a literal
        try self.parseExpression();

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
    }

    fn parseExpression(self: *Parser) ErrorList!void {
        try self.parseEquality();
    }

    fn parseEquality(self: *Parser) ErrorList!void {
        try self.parseComparison();

        while (self.peek().type == .EQUALITY or self.peek().type == .BANG_EQUAL) {
            const operator = self.peek();
            self.advance();
            try self.parseComparison();
            
            if (self.debug_enabled) {
                std.debug.print("Found equality expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }
    }

    fn parseComparison(self: *Parser) ErrorList!void {
        try self.parseTerm();

        while (self.peek().type == .GREATER or 
               self.peek().type == .GREATER_EQUAL or 
               self.peek().type == .LESS or 
               self.peek().type == .LESS_EQUAL) {
            const operator = self.peek();
            self.advance();
            try self.parseTerm();
            
            if (self.debug_enabled) {
                std.debug.print("Found comparison expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }
    }

    fn parseTerm(self: *Parser) ErrorList!void {
        try self.parseFactor();

        while (self.peek().type == .PLUS or self.peek().type == .MINUS) {
            const operator = self.peek();
            self.advance();
            try self.parseFactor();
            
            if (self.debug_enabled) {
                std.debug.print("Found term expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }
    }

    fn parseFactor(self: *Parser) ErrorList!void {
        try self.parseUnary();

        while (self.peek().type == .ASTERISK or self.peek().type == .SLASH) {
            const operator = self.peek();
            self.advance();
            try self.parseUnary();
            
            if (self.debug_enabled) {
                std.debug.print("Found factor expression with operator: {s}\n", .{@tagName(operator.type)});
            }
        }
    }

    fn parseUnary(self: *Parser) ErrorList!void {
        if (self.peek().type == .MINUS or self.peek().type == .BANG) {
            const operator = self.peek();
            self.advance();
            try self.parseUnary();
            
            if (self.debug_enabled) {
                std.debug.print("Found unary expression with operator: {s}\n", .{@tagName(operator.type)});
            }
            return;
        }

        try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ErrorList!void {
        const tok = self.peek();
        switch (tok.type) {
            .INT, .FLOAT, .STRING, .BOOL => {
                if (self.debug_enabled) {
                    std.debug.print("Found literal: {s}\n", .{@tagName(tok.type)});
                }
                self.advance();
            },
            .LEFT_PAREN => {
                self.advance();
                try self.parseExpression();
                
                if (self.peek().type != .RIGHT_PAREN) {
                    return error.ExpectedRightParen;
                }
                self.advance();
            },
            else => return error.ExpectedExpression,
        }
    }
};