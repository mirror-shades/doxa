// TODO:
// - add support for unicode identifiers
// - add support for scientific notation in numbers
// - add support for hexadecimal numbers (0x...)
// - add support for binary numbers (0b...)
// - add support for underscores in numbers (1_000_000)
const std = @import("std");

const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const TokenLiteral = token.TokenLiteral;

pub const LexerError = error{
    UnterminatedString,
    UnterminatedArray,
    UnterminatedParenthesis,
    UnterminatedMultilineComment,
    ExpectedCommaOrClosingBracket,
    ExpectedCommaOrClosingParenthesis,
    InvalidNumber,
    InvalidEscapeSequence,
    UnexpectedCharacter,
    Overflow,
    InvalidCharacter,
};

pub const Lexer = struct {

    keywords: std.StringHashMap(TokenType),

    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {

        return Lexer{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .allocator = allocator,
            .tokens = std.ArrayList(Token).init(allocator),
            .keywords = std.StringHashMap(TokenType).init(allocator),
        };
    }
    
    pub fn deinit(self: *Lexer) void {
        // Free the memory for each token's string literal
        for (self.tokens.items) |item| {
            item.deinit(self.allocator);
        }
        self.tokens.deinit();
        self.keywords.deinit();
    }

    pub fn initKeywords(self: *Lexer) !void {
        try self.keywords.put("true", .BOOL);
        try self.keywords.put("false", .BOOL);
        try self.keywords.put("if", .IF);
        try self.keywords.put("then", .THEN);
        try self.keywords.put("else", .ELSE);
        try self.keywords.put("while", .WHILE);
        try self.keywords.put("for", .FOR);
        try self.keywords.put("foreach", .FOREACH);
        try self.keywords.put("in", .IN);
        try self.keywords.put("fn", .FN_KEYWORD);
        try self.keywords.put("function", .FUNCTION_KEYWORD);
        try self.keywords.put("return", .RETURN);
        try self.keywords.put("const", .CONST);
        try self.keywords.put("var", .VAR);
        try self.keywords.put("struct", .STRUCT);
        try self.keywords.put("break", .BREAK);
        try self.keywords.put("continue", .CONTINUE);
        try self.keywords.put("throw", .THROW);
        try self.keywords.put("try", .TRY);
        try self.keywords.put("catch", .CATCH);
        try self.keywords.put("and", .AND_KEYWORD);
        try self.keywords.put("or", .OR_KEYWORD);
        try self.keywords.put("nothing", .NOTHING);
        try self.keywords.put("import", .IMPORT);
        try self.keywords.put("public", .PUBLIC);
        try self.keywords.put("import", .IMPORT);
        try self.keywords.put("assert", .ASSERT);
        try self.keywords.put("match", .MATCH);
        try self.keywords.put("enum", .ENUM);
        try self.keywords.put("async", .ASYNC);
        try self.keywords.put("await", .AWAIT);
    }

    // ========add token========
    fn addMinimalToken(self: *Lexer, token_type: TokenType) !void {
        try self.addToken(token_type, .nothing);
    }

    fn addToken(self: *Lexer, token_type: TokenType, literal: TokenLiteral) !void {
        try self.tokens.append(Token.init(token_type, self.source[self.start..self.current], literal, self.line));
    }

    fn addLongToken(self: *Lexer, token_type: TokenType, literal: TokenLiteral, lexeme: []const u8) !void {
        try self.tokens.append(Token.init(token_type, lexeme, literal, self.line));
    }

    // ========peek========
    
    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }
    fn peekBack(self: *Lexer) u8 {
        if(self.current == 0) return 0;
        return self.source[self.current - 1];
    }

    // ========helpers========
    pub fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    } 
    
    fn advance(self: *Lexer) void {
        self.current += 1;
    }

    // ========lex tokens========
    pub fn lexTokens(self: *Lexer) !std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            try self.getNextToken();
        }
        try self.tokens.append(Token.init(.EOF, "", .auto, self.line));
        return self.tokens;
    }

    // lexes the next token
    pub fn getNextToken(self: *Lexer) (LexerError || std.mem.Allocator.Error)!void {
        // Skip whitespace
        while (!self.isAtEnd() and (self.peek() == ' ' or self.peek() == '\r' or self.peek() == '\t' or self.peek() == '\n')) {
            if (self.peek() == '\n') self.line += 1;
            self.advance();
        }
        
        if (self.isAtEnd()) return;

        self.start = self.current;
        const c = self.source[self.current];
        self.advance();

        switch (c) {

            //strings
            'r' => {
                if (self.peek() == '"') {
                    self.current = self.start;  // Reset current to include 'r'
                    try self.rawString();
                } else {
                    // This is just an identifier starting with 'r'
                    try self.identifier();
                }
            },

            '/' => {                
                if (!self.isAtEnd() and self.source[self.current] == '/') {
                    self.advance();  // consume the second slash
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        self.advance();
                    }
                    if (!self.isAtEnd() and self.peek() == '\n') {
                        self.line += 1;
                        self.advance();
                    }
                } else if (self.match('*')) {
                    var nesting: usize = 1;
                    
                    while (nesting > 0 and !self.isAtEnd()) {
                        if (self.peek() == '/' and self.peekNext() == '*') {
                            self.advance();  // consume /
                            self.advance();  // consume *
                            nesting += 1;
                        } else if (self.peek() == '*' and self.peekNext() == '/') {
                            self.advance();  // consume *
                            self.advance();  // consume /
                            nesting -= 1;
                        } else {
                            if (self.peek() == '\n') {
                                self.line += 1;
                            }
                            self.advance();
                        }
                    }

                    if (nesting > 0) {
                        return error.UnterminatedMultilineComment;
                    }
                } else if (self.match('=')) {
                    try self.addMinimalToken(.SLASH_EQUAL);
                } else {
                    try self.addMinimalToken(.SLASH);
                    // Skip any whitespace after the slash
                    while (!self.isAtEnd() and (self.peek() == ' ' or self.peek() == '\r' or self.peek() == '\t' or self.peek() == '\n')) {
                        if (self.peek() == '\n') self.line += 1;
                        self.advance();
                    }
                }
            },

            '(' => try self.parenthesis(),
            ')' => try self.addMinimalToken(.RIGHT_PAREN),
            '{' => try self.addMinimalToken(.LEFT_BRACE),
            '}' => try self.addMinimalToken(.RIGHT_BRACE),
            ',' => try self.addMinimalToken(.COMMA),
            '.' => try self.addMinimalToken(.DOT),
            ':' => try self.addMinimalToken(.COLON),
            ';' => try self.addMinimalToken(.SEMICOLON),
            '%' => try self.addMinimalToken(.MODULO),
            '#' => try self.addMinimalToken(.HASH),

            '&' => {
                if (self.match('&')) {
                    try self.addMinimalToken(.AND_SYMBOL);
                } else {
                    try self.addMinimalToken(.AMPERSAND);
                }
            },
            '|' => {
                if (self.match('|')) {
                    try self.addMinimalToken(.OR_SYMBOL);
                } else {
                    try self.addMinimalToken(.PIPE);
                }
            },
            '^' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.POWER_EQUAL);
                } else {
                    try self.addMinimalToken(.POWER);
                }
            },
            '*' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.ASTERISK_EQUAL);
                } else {
                    try self.addMinimalToken(.ASTERISK);
                }
            },
            '!' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.BANG_EQUAL);
                } else {
                    try self.addMinimalToken(.BANG);
                }
            },
            '=' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.EQUAL_EQUAL);
                } else {
                    try self.addMinimalToken(.EQUAL);
                }
            },
            '<' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.LESS_EQUAL);
                } else {
                    try self.addMinimalToken(.LESS);
                }
            },
            '>' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.GREATER_EQUAL);
                } else {
                    try self.addMinimalToken(.GREATER);
                }
            },
            '+' => {
                if (self.match('+')) {
                    try self.addMinimalToken(.PLUS_PLUS);
                } else if (self.match('=')) {
                    try self.addMinimalToken(.PLUS_EQUAL);
                } else {
                    try self.addMinimalToken(.PLUS);
                }
            },
            '-' => {
                if (self.match('-')) {
                    try self.addMinimalToken(.MINUS_MINUS);
                } else if (self.match('=')) {
                    try self.addMinimalToken(.MINUS_EQUAL);
                } else {
                    try self.addMinimalToken(.MINUS);
                }
            },
            //numbers
            '0'...'9' => try self.number(),

            '"' => try self.string(),
            //arrays
            '[' => try self.array(),
            ']' => try self.addMinimalToken(.RIGHT_BRACKET),
            //whitespace
            ' ', '\r', '\t' => {}, // Skip whitespace without creating tokens
            '\n' => {
                self.line += 1;
                // Don't create a token for newlines
            },
            //identifier or keyword
            else => {
                if (isAlpha(c)) {
                    try self.identifier();
                } else {
                    std.debug.print("Unexpected character: {c}\n", .{c});
                    return error.UnexpectedCharacter;
                }
            },
        }
    }

    fn identifier(self: *Lexer) !void {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
            self.advance();
        }
        const text = self.source[self.start..self.current];
        
        // Check if it's a keyword
        if (self.keywords.get(text)) |keyword_type| {
            switch (keyword_type) {
                .BOOL => try self.addToken(.BOOL, .{ .boolean = std.mem.eql(u8, text, "true") }),
                else => try self.addMinimalToken(keyword_type),
            }
        } else {
            try self.addToken(.IDENTIFIER, .{ .string = text });
        }
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
               (c >= 'A' and c <= 'Z') or
               c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.advance();
        return true;
    }

    fn boolean(self: *Lexer) !void {
        try self.addToken(.BOOLEAN, .{ .boolean = self.source[self.start..self.current] });
    }

    fn parenthesis(self: *Lexer) !void {
        try self.addMinimalToken(.LEFT_PAREN);
    }

    fn array(self: *Lexer) !void {
        try self.addMinimalToken(.LEFT_BRACKET);
        
        while (!self.isAtEnd() and self.peek() != ']') {
            // Skip whitespace
            while (!self.isAtEnd() and (self.peek() == ' ' or self.peek() == '\r' or self.peek() == '\t' or self.peek() == '\n')) {
                if (self.peek() == '\n') self.line += 1;
                self.advance();
            }
            
            if (self.peek() == ']') break;
            
            try self.getNextToken();
            
            // Skip whitespace after element
            while (!self.isAtEnd() and (self.peek() == ' ' or self.peek() == '\r' or self.peek() == '\t' or self.peek() == '\n')) {
                if (self.peek() == '\n') self.line += 1;
                self.advance();
            }
            
            // Check for comma or end of array
            if (self.peek() == ',') {
                self.start = self.current; // Set start position
                self.advance();  // Advance past the comma
                try self.addMinimalToken(.COMMA);  // Add token after advancing
            } else if (self.peek() != ']') {
                return error.ExpectedCommaOrClosingBracket;
            }
        }

        if (self.isAtEnd()) {
            return error.UnterminatedArray;
        }

        self.start = self.current; // Set start position to current before consuming right bracket
        self.advance(); // consume closing bracket
        try self.addMinimalToken(.RIGHT_BRACKET);
    }

    fn rawString(self: *Lexer) !void {        
        self.advance(); // Consume the 'r'
        self.advance(); // Consume the opening quote
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        // For raw strings, consume everything literally until an unescaped closing quote
        while (!self.isAtEnd()) {
            const ch = self.peek();            
            if (ch == '"' and (self.current == 0 or self.source[self.current - 1] != '\\')) {
                break;
            }
            try result.append(ch);
            self.advance();
        }

        if (self.isAtEnd()) {
            return error.UnterminatedString;
        }

        self.advance(); // consume closing quote
        const lexeme = self.source[self.start..self.current];        
        const string_content = try result.toOwnedSlice();
        try self.addLongToken(.STRING, .{ .string = string_content }, lexeme);
    }

    fn string(self: *Lexer) !void {
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        while (!self.isAtEnd() and self.peek() != '"') {
            const c = self.peek();
            self.advance();
            
            if (c == '\\') {
                if (self.isAtEnd()) {
                    return error.UnterminatedString;
                }
                
                const escaped = self.peek();
                self.advance();
                switch (escaped) {
                    '"' => try result.append('"'),
                    '\\' => try result.append('\\'),
                    'n' => try result.append('\n'),
                    't' => try result.append('\t'),
                    else => return error.InvalidEscapeSequence,
                }
            } else {
                try result.append(c);
            }
        }

        if (self.isAtEnd()) {
            return error.UnterminatedString;
        }

        self.advance(); // consume closing quote
        
        const lexeme = self.source[self.start..self.current];
        const string_content = try result.toOwnedSlice();
        
        try self.addLongToken(.STRING, .{ .string = string_content }, lexeme);
    }

    fn number(self: *Lexer) !void {
        while (isDigit(self.peek())) self.advance();

        // Look for decimal
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            self.advance(); // Consume the dot
            while (isDigit(self.peek())) self.advance();
            
            const num_str = self.source[self.start..self.current];
            const float_val = try std.fmt.parseFloat(f64, num_str);
            try self.addToken(.FLOAT, .{ .float = float_val });
        } else {
            const num_str = self.source[self.start..self.current];
            const int_val = try std.fmt.parseInt(i64, num_str, 10);
            try self.addToken(.INT, .{ .int = int_val });
        }
    }


};
