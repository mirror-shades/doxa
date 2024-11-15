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
    Bang, // '!'
    Colon, // ':'
    Semicolon, // ';'
    LeftBrace, // '{'
    RightBrace, // '}'
    Comma, // ','
    Dot, // '.'
    Quote, // '"'
    Newline, // '\n'
    True, // 'true'
    False, // 'false'
    LeftBracket, // '['
    RightBracket, // ']'
    Ampersand, // '&'
    Pipe, // '|'

    // Comparison operators
    EqualEqual, // '=='
    NotEqual, // '!='
    And, // 'and'
    Or, // 'or'
    If, // 'if'
    Then, // 'then'
    Else, // 'else'

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
            '/' => {
                if (!self.isAtEnd() and self.peekChar() == '/') {
                    _ = self.advance(); // Consume the second '/'
                    self.skipComment(); // Skip the comment
                    return self.nextToken(); // Get the next token after the comment
                } else {
                    return Token{ .kind = .Slash, .lexeme = "/" };
                }
            },
            '&' => {
                if (!self.isAtEnd() and self.peekChar() == '&') {
                    _ = self.advance(); // Consume the second '&'
                    return Token{ .kind = .And, .lexeme = "and" };
                } else {
                    return Token{ .kind = .Ampersand, .lexeme = "&" };
                }
            },
            '|' => {
                if (!self.isAtEnd() and self.peekChar() == '|') {
                    _ = self.advance(); // Consume the second '|'
                    return Token{ .kind = .Or, .lexeme = "or" };
                } else {
                    return Token{ .kind = .Pipe, .lexeme = "|" };
                }
            },
            '(' => return Token{ .kind = .LeftParen, .lexeme = "(" },
            ')' => return Token{ .kind = .RightParen, .lexeme = ")" },
            '[' => return Token{ .kind = .LeftBracket, .lexeme = "[" },
            ']' => return Token{ .kind = .RightBracket, .lexeme = "]" },
            '{' => return Token{ .kind = .LeftBrace, .lexeme = "{" },
            '}' => return Token{ .kind = .RightBrace, .lexeme = "}" },
            '=' => {
                if (!self.isAtEnd() and self.peekChar() == '=') {
                    _ = self.advance();
                    return Token{ .kind = .EqualEqual, .lexeme = "==" };
                } else {
                    return Token{ .kind = .Equal, .lexeme = "=" };
                }
            },
            '!' => {
                if (!self.isAtEnd() and self.peekChar() == '=') {
                    _ = self.advance(); 
                    return Token{ .kind = .NotEqual, .lexeme = "!=" };
                } else {
                    return Token{ .kind = .Bang, .lexeme = "!" };
                }
            },
            ':' => return Token{ .kind = .Colon, .lexeme = ":" },
            ';' => return Token{ .kind = .Semicolon, .lexeme = ";" },
            ',' => return Token{ .kind = .Comma, .lexeme = "," },
            '.' => return Token{ .kind = .Dot, .lexeme = "." },
            '"' => return self.string(),
            else => {},
        }

        if (std.ascii.isDigit(c)) {
            self.current -= 1; // Step back to include the digit
            return self.number();
        } else if (std.ascii.isAlphabetic(c) or c == '_') {
            self.current -= 1; // Step back to include the character
            return self.identifier();
        } else {
            // Handle unexpected character
            std.debug.print("Unexpected character: {c}\n", .{c});
            return Token{ .kind = .EOF, .lexeme = "" };
        }
    }

    fn skipComment(self: *Lexer) void {
        while (!self.isAtEnd() and self.peekChar() != '\n') {
            _ = self.advance();
        }
        if (!self.isAtEnd() and self.peekChar() == '\n') {
            _ = self.advance();
        }
    }

    pub fn peek(self: *Lexer) TokenKind {
        const saved_current = self.current;
        const token = self.nextToken();
        self.current = saved_current;
        return token.kind;
    }

    fn peekChar(self: *Lexer) u8 {
        return self.source[self.current];
    }

    fn peekWord(self: *Lexer) []const u8 {
        var end = self.current;
        while (end < self.source.len and self.isIdentifierChar(self.source[end])) {
            end += 1;
        }
        return self.source[self.current..end];
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

        // Include all keywords in the checks
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
        else if (std.mem.eql(u8, lexeme, "and"))
            TokenKind.And
        else if (std.mem.eql(u8, lexeme, "or"))
            TokenKind.Or
        else if (std.mem.eql(u8, lexeme, "if"))
            TokenKind.If
        else if (std.mem.eql(u8, lexeme, "then"))
            TokenKind.Then
        else if (std.mem.eql(u8, lexeme, "else"))
            TokenKind.Else
        else if (std.mem.eql(u8, lexeme, "true"))
            TokenKind.True
        else if (std.mem.eql(u8, lexeme, "false"))
            TokenKind.False
        else if (std.mem.eql(u8, lexeme, "nothing"))
            TokenKind.Nothing
        else
            TokenKind.Identifier;

        return Token{ .kind = token_kind, .lexeme = lexeme };
    }

    fn isIdentifierChar(_: *Lexer, c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    fn number(self: *Lexer) Token {
        const start = self.current;
        while (!self.isAtEnd() and std.ascii.isDigit(self.source[self.current])) {
            self.current += 1;
        }

        if (!self.isAtEnd() and self.source[self.current] == '.') {
            self.current += 1; // Consume the '.'
            while (!self.isAtEnd() and std.ascii.isDigit(self.source[self.current])) {
                self.current += 1;
            }
        }

        const lexeme = self.source[start..self.current];
        return Token{ .kind = .Number, .lexeme = lexeme };
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
