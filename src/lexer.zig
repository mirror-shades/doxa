const std = @import("std");

pub const TokenKind = enum {
    // Single-character tokens
    Plus, // '+'
    Minus, // '-'
    Star, // '*'
    Slash, // '/'
    LeftParen, // '('
    RightParen, // ')'
    Equal, // '='
    Colon, // ':'
    Semicolon, // ';'
    LeftBrace, // '{'
    RightBrace, // '}'
    Comma, // ','
    Dot, // '.'
    Quote, // '"'

    // Keywords and identifiers
    Identifier, // [a-zA-Z_][a-zA-Z0-9_]*
    Var, // 'var'
    Const, // 'const'
    Print, // 'print'
    IntType, // 'int'
    FloatType, // 'float'
    StringType, // 'string'
    BoolType, // 'bool'

    // Literals
    Number,
    String,
    Nothing,

    // End of file
    EOF,

};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
};

pub const Lexer = struct {
    source: []const u8,
    current: usize,

    pub fn init(source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .current = 0,
        };
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return Token{ .kind = .EOF, .lexeme = "" };
        }

        const c = self.advance();

        // Handle single-character tokens
        switch (c) {
            '+' => return Token{ .kind = .Plus, .lexeme = "+" },
            '-' => return Token{ .kind = .Minus, .lexeme = "-" },
            '*' => return Token{ .kind = .Star, .lexeme = "*" },
            '/' => return Token{ .kind = .Slash, .lexeme = "/" },
            '(' => return Token{ .kind = .LeftParen, .lexeme = "(" },
            ')' => return Token{ .kind = .RightParen, .lexeme = ")" },
            '=' => return Token{ .kind = .Equal, .lexeme = "=" },
            ':' => return Token{ .kind = .Colon, .lexeme = ":" },
            ';' => return Token{ .kind = .Semicolon, .lexeme = ";" },
            '{' => return Token{ .kind = .LeftBrace, .lexeme = "{" },
            '}' => return Token{ .kind = .RightBrace, .lexeme = "}" },
            ',' => return Token{ .kind = .Comma, .lexeme = "," },
            '.' => return Token{ .kind = .Dot, .lexeme = "." },
            '"' => return self.string(),
            else => {},
        }

        if (std.ascii.isDigit(c)) {
            self.current -= 1; // Step back to include the digit
            return self.int();
        } else if (std.ascii.isAlphabetic(c) or c == '_') {
            self.current -= 1; // Step back to include the character
            return self.identifier();
        } else {
            // Handle unexpected character
            std.debug.print("Unexpected character: {c}\n", .{c});
            return Token{ .kind = .EOF, .lexeme = "" };
        }
    }

    pub fn peek(self: *Lexer) TokenKind {
        const saved_current = self.current;
        const token = self.nextToken();
        self.current = saved_current;
        return token.kind;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            switch (self.source[self.current]) {
                ' ', '\r', '\t', '\n' => self.current += 1,
                else => return,
            }
        }
    }

    fn identifier(self: *Lexer) Token {
        const start = self.current;
        while (!self.isAtEnd() and self.isIdentifierChar(self.source[self.current])) {
            self.current += 1;
        }
        const lexeme = self.source[start..self.current];

        // Replace string switch with if-else comparisons
        const token_kind = if (std.mem.eql(u8, lexeme, "var"))
            TokenKind.Var
        else if (std.mem.eql(u8, lexeme, "const"))
            TokenKind.Const
        else if (std.mem.eql(u8, lexeme, "print"))
            TokenKind.Print
        else if (std.mem.eql(u8, lexeme, "int"))
            TokenKind.IntType
        else if (std.mem.eql(u8, lexeme, "float"))
            TokenKind.FloatType
        else if (std.mem.eql(u8, lexeme, "string"))
            TokenKind.StringType
        else if (std.mem.eql(u8, lexeme, "bool"))
            TokenKind.BoolType
        else
            TokenKind.Identifier;

        return Token{ .kind = token_kind, .lexeme = lexeme };
    }

    fn isIdentifierChar(_: *Lexer, c: u8) bool {
        return std.ascii.isAlphabetic(c) or std.ascii.isDigit(c) or c == '_';
    }

    fn int(self: *Lexer) Token {
        const start = self.current;

        // Process digits before decimal point
        while (!self.isAtEnd() and std.ascii.isDigit(self.source[self.current])) {
            self.current += 1;
        }

        // Look for decimal point
        if (!self.isAtEnd() and self.source[self.current] == '.') {
            // Consume the decimal point
            self.current += 1;

            // Process digits after decimal point
            while (!self.isAtEnd() and std.ascii.isDigit(self.source[self.current])) {
                self.current += 1;
            }
        }

        const lexeme = self.source[start..self.current];
        return Token{ .kind = .IntType, .lexeme = lexeme };
    }

    fn string(self: *Lexer) Token {
        const start = self.current;
        while (!self.isAtEnd() and self.source[self.current] != '"') {
            self.current += 1;
        }
        const lexeme = self.source[start..self.current];

        if (!self.isAtEnd()) {
            self.current += 1; // consume closing quote
        }

        return Token{ .kind = .String, .lexeme = lexeme };
    }
};
