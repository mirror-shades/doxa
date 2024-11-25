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
            std.debug.print("Starting parse with {} tokens\n", .{self.tokens.len});
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

        // literal
        const lit_tok = self.peek();
        if (lit_tok.type != .INT and lit_tok.type != .FLOAT and lit_tok.type != .STRING) {
            if (self.debug_enabled) {
                std.debug.print("Expected literal, got {s}\n", .{@tagName(lit_tok.type)});
            }
            return error.ExpectedLiteral;
        }
        if (self.debug_enabled) {
            std.debug.print("Found literal: {s} = ", .{@tagName(lit_tok.type)});
            switch (lit_tok.literal) {
                .int => |i| std.debug.print("{}\n", .{i}),
                .float => |f| std.debug.print("{d}\n", .{f}),
                .string => |s| std.debug.print("\"{s}\"\n", .{s}),
                else => std.debug.print("(unknown literal type)\n", .{}),
            }
        }
        self.advance();

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
};