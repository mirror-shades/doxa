const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Node = @import("ast.zig").Node;
const NodeType = @import("ast.zig").NodeType;

pub const ParserError = error{
    UnexpectedToken,
    ExpectedExpression,
};

pub const Parser = struct {
    tokens: []Token,  
    current: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        const mutable_tokens = allocator.dupe(Token, tokens) catch unreachable;
        return Parser{
            .tokens = mutable_tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.free(self.tokens);
    }

    pub fn parse(self: *Parser) !Node {
        var statements = std.ArrayList(Node).init(self.allocator);
        defer statements.deinit();

        while (!self.isAtEnd()) {
            const stmt = try self.declaration();
            try statements.append(stmt);
        }

        return Node{
            .node_type = NodeType.Program,
            .children = try statements.toOwnedSlice(),
            .token = Token.init(.NOTHING, "", .nothing, 0),
        };
    }

    fn declaration(self: *Parser) !Node {
        if (self.match(&[_]TokenType{.VAR})) {
            return try self.variableDeclaration();
        } else if (self.match(&[_]TokenType{.FUNCTION})) {
            return try self.functionDeclaration();
        } else {
            return try self.statement();
        }
    }

    fn variableDeclaration(self: *Parser) !Node {
        const identifier = try self.consume(.IDENTIFIER, "Expected variable name");

        var initializer: ?Node = null;
        if (self.match(&[_]TokenType{.EQUAL})) {
            initializer = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expected ';' after variable declaration");

        var children = std.ArrayList(Node).init(self.allocator);
        try children.append(Node{ 
            .node_type = NodeType.Identifier, 
            .children = &[_]Node{}, 
            .token = identifier 
        });
        if (initializer) |expr| {
            try children.append(expr);
        }

        return Node{
            .node_type = NodeType.VariableDeclaration,
            .children = try children.toOwnedSlice(),
            .token = Token.init(.NOTHING, "", .nothing, 0),
        };
    }

    fn functionDeclaration(self: *Parser) !Node {
        _ = self;
        // Implement function declaration parsing
        // ...
        return error.NotImplemented;
    }

    fn statement(self: *Parser) !Node {
        _ = self;
        // Implement statement parsing (expression statements, control flow, etc.)
        // ...
        return error.NotImplemented;
    }

    fn expression(self: *Parser) !Node {
        _ = self;
        // Implement expression parsing
        // ...
        return error.NotImplemented;
    }

    // Utility functions
    fn match(self: *Parser, token_types: []const TokenType) bool {
        for (token_types) |tt| {
            if (self.check(tt)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        std.debug.print("{s} Got: {any}\n", .{message, self.peek()});
        return error.UnexpectedToken;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
};