const std = @import("std");

const token = @import("token.zig");
pub const TokenType = token.TokenType;
pub const Token = token.Token;
pub const TokenLiteral = token.TokenLiteral;

///==========================================================================
/// Types & Errors
///==========================================================================

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
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8InvalidCodepoint,
    Utf8CodepointTooLarge,
    InvalidUnicodeEscape,
    CodepointTooLarge,
    Utf8CannotEncodeSurrogateHalf,
    LeadingZeros,
    MultipleExponents,
    InvalidExponent,
};

pub const Lexer = struct {
    //======================================================================
    // Fields
    //======================================================================
    
    keywords: std.StringHashMap(TokenType),
    tokens: std.ArrayList(Token),
    source: []const u8,
    start: u32,
    current: u32,
    line: u32,
    allocator: std.mem.Allocator,

    //======================================================================
    // Initialization
    //======================================================================

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
        try self.keywords.put("typeof", .TYPEOF);
        try self.keywords.put("is", .IS);
        try self.keywords.put("as", .AS);
        try self.keywords.put("from", .FROM);
        try self.keywords.put("auto", .AUTO);
    }

    //======================================================================
    // Public Interface
    //======================================================================

    pub fn lexTokens(self: *Lexer) !std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            try self.getNextToken();
        }
        try self.tokens.append(Token.init(.EOF, "", .nothing, self.line, self.current));
        return self.tokens;
    }

    // lexes the next token
    fn getNextToken(self: *Lexer) (LexerError || std.mem.Allocator.Error)!void {
        // Skip whitespace
        while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t' or self.peekAt(0) == '\n')) {
            if (self.peekAt(0) == '\n') self.line += 1;
            self.advance();
        }
        
        if (self.isAtEnd()) return;

        self.start = self.current;
        const c = self.source[self.current];
        self.advance();

        switch (c) {

            //strings
            'r' => {
                if (self.peekAt(0) == '"') {
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
                    while (!self.isAtEnd() and self.peekAt(0) != '\n') {
                        self.advance();
                    }
                    if (!self.isAtEnd() and self.peekAt(0) == '\n') {
                        self.line += 1;
                        self.advance();
                    }
                } else if (self.match('*')) {
                    var nesting: u32 = 1;
                    
                    while (nesting > 0 and !self.isAtEnd()) {
                        if (self.peekAt(0) == '/' and self.peekAt(1) == '*') {
                            self.advance();  // consume /
                            self.advance();  // consume *
                            nesting += 1;
                        } else if (self.peekAt(0) == '*' and self.peekAt(1) == '/') {
                            self.advance();  // consume *
                            self.advance();  // consume /
                            nesting -= 1;
                        } else {
                            if (self.peekAt(0) == '\n') {
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
                    while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t' or self.peekAt(0) == '\n')) {
                        if (self.peekAt(0) == '\n') self.line += 1;
                        self.advance();
                    }
                }
            },

            '(' => try self.parenthesis(),
            ')' => try self.addMinimalToken(.RIGHT_PAREN),
            '{' => try self.addMinimalToken(.LEFT_BRACE),
            '}' => try self.addMinimalToken(.RIGHT_BRACE),
            ',' => try self.addMinimalToken(.COMMA),
            '.' => {
                if (self.peekAt(0) == '.') {
                    self.advance(); // consume second dot
                    if (self.peekAt(0) == '.') {
                        self.advance(); // consume third dot
                        try self.addMinimalToken(.SPREAD);
                    } else {
                        try self.addMinimalToken(.DOT_DOT);
                    }
                } else if (isDigit(self.peekAt(0))) {
                    self.current -= 1; // back up to include the dot
                    self.start = self.current;
                    try self.number();
                } else {
                    try self.addMinimalToken(.DOT);
                }
            },
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
                } else if (isDigit(self.peekAt(0)) or self.peekAt(0) == '.' or 
                          (self.peekAt(0) == '0' and 
                           (self.peekAt(1) == 'x' or 
                            self.peekAt(1) == 'b' or 
                            self.peekAt(1) == 'o'))) {
                    self.current -= 1;
                    self.start = self.current;
                    try self.number();
                } else {
                    try self.addMinimalToken(.PLUS);
                }
            },
            '-' => {
                if (self.match('>')) {
                    try self.addMinimalToken(.ARROW);
                } else if (self.match('-')) {
                    try self.addMinimalToken(.MINUS_MINUS);
                } else if (self.match('=')) {
                    try self.addMinimalToken(.MINUS_EQUAL);
                } else if (isDigit(self.peekAt(0)) or self.peekAt(0) == '.' or 
                          (self.peekAt(0) == '0' and (self.peekAt(1) == 'x' or self.peekAt(1) == 'b'))) {
                    // This is a negative number - include the minus sign in the token
                    self.current -= 1;  // Back up to include the minus sign
                    self.start = self.current;  // Start from the minus sign
                    try self.number();
                } else {
                    try self.addMinimalToken(.MINUS);
                }
            },
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
            '~' => try self.addMinimalToken(.TILDE),
            '?' => try self.addMinimalToken(.QUESTION),
        }
    }

    //======================================================================
    // Token Generation
    //======================================================================

    fn addMinimalToken(self: *Lexer, token_type: TokenType) !void {
        try self.addToken(token_type, .nothing);
    }

    fn addToken(self: *Lexer, token_type: TokenType, literal: TokenLiteral) !void {
        try self.addLongToken(token_type, literal, self.source[self.start..self.current]);
    }

    fn addLongToken(self: *Lexer, token_type: TokenType, literal: TokenLiteral, lexeme: []const u8) !void {
        try self.tokens.append(Token.init(token_type, lexeme, literal, self.line, self.current));
    }

    //======================================================================
    // Lexer State Management
    //======================================================================

    fn peekAt(self: *Lexer, offset: i32) u8 {
        // For negative offsets, check if we'd go before start of string
        if (offset < 0) {
            const abs_offset = @abs(offset);
            if (abs_offset > self.current) return 0;
            return self.source[self.current - @as(u32, abs_offset)];
        }
        
        // For zero or positive offsets
        if (offset >= 0) {
            const pos = self.current + @as(u32, @intCast(offset));
            if (pos >= self.source.len) return 0;
            return self.source[pos];
        }
        
        return 0;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    } 
    
    fn advance(self: *Lexer) void {
        self.current += 1;
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.advance();
        return true;
    }

    //======================================================================
    // Character Classification
    //======================================================================

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
               (c >= 'A' and c <= 'Z') or
               c == '_' or
               c > 127;  // Accept all Unicode characters above ASCII
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isOperator(c: u8) bool {
     return c == '+' or c == '-' or c == '*' or c == '/' or c == '%' or c == '^' or c == '&' or c == '|' or c == '!' or c == '=' or c == '<' or c == '>';
    }

    fn isWhitespace(c: u8) bool {
        return c == ' ' or c == '\r' or c == '\t' or c == '\n';
    }

    fn isHexDigit(c: u8) bool {
        return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    fn isBinaryDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctalDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    //======================================================================
    // Token Handlers
    //======================================================================

    fn identifier(self: *Lexer) !void {
        while (!self.isAtEnd()) {
            const remaining = self.source[self.current..];
            // First check if we have a valid UTF-8 sequence
            const sequence_length = try std.unicode.utf8ByteSequenceLength(remaining[0]);
            if (sequence_length > remaining.len) break;
            
            // Try to decode the codepoint
            const view = std.unicode.Utf8View.init(remaining[0..sequence_length]) catch break;
            var iterator = view.iterator();
            if (iterator.nextCodepoint()) |codepoint| {
                // Check if it's a letter, number, or underscore
                if ((codepoint >= 'a' and codepoint <= 'z') or
                    (codepoint >= 'A' and codepoint <= 'Z') or
                    (codepoint >= '0' and codepoint <= '9') or
                    codepoint == '_' or
                    codepoint > 127) {  // Accept all Unicode characters above ASCII
                    var i: u32 = 0;
                    while (i < sequence_length) : (i += 1) {
                        self.advance();
                    }
                    continue;
                }
            }
            break;
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

    fn boolean(self: *Lexer) !void {
        try self.addToken(.BOOLEAN, .{ .boolean = self.source[self.start..self.current] });
    }

    fn parenthesis(self: *Lexer) !void {
        try self.addMinimalToken(.LEFT_PAREN);
    }

    fn array(self: *Lexer) !void {
        try self.addMinimalToken(.LEFT_BRACKET);
        
        while (!self.isAtEnd() and self.peekAt(0) != ']') {
            // Skip whitespace
            while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t' or self.peekAt(0) == '\n')) {
                if (self.peekAt(0) == '\n') self.line += 1;
                self.advance();
            }
            
            if (self.peekAt(0) == ']') break;
            
            try self.getNextToken();
            
            // Skip whitespace after element
            while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t' or self.peekAt(0) == '\n')) {
                if (self.peekAt(0) == '\n') self.line += 1;
                self.advance();
            }
            
            // Check for comma or end of array
            if (self.peekAt(0) == ',') {
                self.start = self.current; // Set start position
                self.advance();  // Advance past the comma
                try self.addMinimalToken(.COMMA);  // Add token after advancing
            } else if (self.peekAt(0) != ']') {
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

    //======================================================================
    // String Handling
    //======================================================================

    fn string(self: *Lexer) !void {
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        while (!self.isAtEnd() and self.peekAt(0) != '"') {
            const c = self.peekAt(0);
            self.advance();
            
            if (c == '\\') {
                if (self.isAtEnd()) {
                    return error.UnterminatedString;
                }
                
                const escaped = self.peekAt(0);
                self.advance();
                switch (escaped) {
                    '"' => try result.append('"'),
                    '\\' => try result.append('\\'),
                    'n' => try result.append('\n'),
                    't' => try result.append('\t'),
                    'u' => {
                        // Handle Unicode escape sequences
                        if (self.peekAt(0) != '{') return error.InvalidUnicodeEscape;
                        self.advance(); // consume '{'
                        
                        // Read hex digits until '}'
                        var codepoint: u21 = 0;
                        var digit_count: u8 = 0;
                        while (!self.isAtEnd() and self.peekAt(0) != '}' and digit_count < 6) {
                            const digit = switch (self.peekAt(0)) {
                                '0'...'9' => self.peekAt(0) - '0',
                                'a'...'f' => self.peekAt(0) - 'a' + 10,
                                'A'...'F' => self.peekAt(0) - 'A' + 10,
                                else => return error.InvalidUnicodeEscape,
                            };
                            codepoint = (codepoint << 4) | digit;
                            digit_count += 1;
                            self.advance();
                        }
                        
                        if (self.peekAt(0) != '}') return error.InvalidUnicodeEscape;
                        self.advance(); // consume '}'
                        
                        // Validate and encode the codepoint
                        if (codepoint > 0x10FFFF) return error.CodepointTooLarge;
                        
                        var buf: [4]u8 = undefined;
                        const len = try std.unicode.utf8Encode(codepoint, &buf);
                        try result.appendSlice(buf[0..len]);
                        continue;
                    },
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

    fn rawString(self: *Lexer) !void {        
        self.advance(); // Consume the 'r'
        self.advance(); // Consume the opening quote
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        // For raw strings, consume everything literally until an unescaped closing quote
        while (!self.isAtEnd()) {
            const ch = self.peekAt(0);            
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

    //======================================================================
    // Number Handling
    //======================================================================

    fn number(self: *Lexer) !void {
        var has_digits = false;
        var has_decimal = false;
        var has_exponent = false;
        var is_negative = false;
        var has_sign = false;

        // Check if this number starts with a negative sign
        if (self.source[self.start] == '-') {
            is_negative = true;
        }
        if (self.source[self.start] == '+' or self.source[self.start] == '-') {
            has_sign = true;
            self.advance();
        }
        // Determine the first actual character, accounting for negative sign
        const first_char = if (has_sign and self.start + 1 < self.source.len) 
            self.source[self.start + 1]  // Skip the sign
        else 
            self.source[self.start];
        
        // Mark if we start with a valid digit
        if (isDigit(first_char)) {
            has_digits = true;
        }

        // Handle special bases (hex, binary, octal)
        if (self.current < self.source.len - 1 and 
            ((has_sign and self.source[self.current] == '0') or 
             self.source[self.current - 1] == '0')) {
            const next_char = if (has_sign) self.peekAt(1) else self.peekAt(0);
            switch (next_char) {
                'x' => { try self.handleHex(is_negative); return; },
                'b' => { try self.handleBinary(is_negative); return; },
                'o' => { try self.handleOctal(is_negative); return; },
                else => {},
            }
        }

        // Process digits before decimal point
        while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            if (isDigit(self.peekAt(0))) {
                // Check for leading zeros
                if (has_digits == false and self.peekAt(0) == '0' and 
                    isDigit(self.peekAt(1))) {
                    return error.LeadingZeros;
                }
                has_digits = true;
            }
            self.advance();
        }

        // Look for decimal point
        if (self.peekAt(0) == '.') {
            has_decimal = true;
            self.advance(); // consume the decimal point

            // Process digits after decimal point
            while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
                if (isDigit(self.peekAt(0))) has_digits = true;
                self.advance();
            }
        }

        // Look for decimal point
        if (self.peekAt(0) == '.') {
            has_decimal = true;
            self.advance();

            // Must have at least one digit after decimal
            if (!isDigit(self.peekAt(0)) or self.peekAt(0) != '.') return error.InvalidNumber;

            while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
                if (isDigit(self.peekAt(0))) has_digits = true;
                self.advance();
            }
        }

        // Look for scientific notation
        if (self.peekAt(0) == 'e' or self.peekAt(0) == 'E') {
            if (has_exponent) return error.MultipleExponents;
            has_exponent = true;
            has_decimal = true;  // Treat as float
            self.advance();

            if (self.peekAt(0) == '+' or self.peekAt(0) == '-') {
                self.advance();
            }

            // Must have at least one digit in exponent
            if (!isDigit(self.peekAt(0))) return error.InvalidExponent;

            while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
                self.advance();
            }
        }

        if (!has_digits) return error.InvalidNumber;

        const raw_str = self.source[self.start..self.current];
        const num_str = try self.removeUnderscores(raw_str);
        defer self.allocator.free(num_str);

        if (has_decimal) {
            // Handle special floating point values like infinity and NaN
            const num_str_buf = try self.allocator.alloc(u8, num_str.len);
            defer self.allocator.free(num_str_buf);
            const num_str_lower = std.ascii.lowerString(num_str_buf, num_str);
            
            if (std.mem.eql(u8, num_str_lower, "inf") or std.mem.eql(u8, num_str_lower, "infinity")) {
                const float_val = if (is_negative) -std.math.inf(f32) else std.math.inf(f32);
                try self.addToken(.FLOAT, .{ .float = float_val });
                return;
            }
            if (std.mem.eql(u8, num_str_lower, "nan")) {
                try self.addToken(.FLOAT, .{ .float = std.math.nan(f32) });
                return;
            }

            // Parse the float value
            const float_val = std.fmt.parseFloat(f32, num_str) catch {
                return error.InvalidNumber;
            };
            
            // Check for underflow/overflow
            if (std.math.isInf(float_val) or std.math.isNan(float_val)) {
                return error.InvalidNumber;
            }
            
            try self.addToken(.FLOAT, .{ .float = float_val });
        } else {
            const int_val = std.fmt.parseInt(i32, num_str, 10) catch |err| switch (err) {
                error.InvalidCharacter => return error.InvalidNumber,
                else => return err,
            };
            try self.addToken(.INT, .{ .int = int_val });
        }
    }

    fn handleHex(self: *Lexer, is_negative: bool) !void {
        var digits_start: u32 = self.current;
        // If we're at 'x', back up to include the '0'
        if (self.source[self.current] == 'x') {
            self.current -= 1;
        }
        self.advance(); // Move past '0'
        self.advance(); // Move past 'x'
        
        // Must have at least one hex digit after 0x
        if (!isHexDigit(self.peekAt(0))) return error.InvalidNumber;
        
        // Mark where the actual hex digits begin (after 0x)
        digits_start = self.current;  // Update digits_start to after the 0x prefix
        
        // Consume all hex digits and underscores
        while (isHexDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }
        
        // Extract just the hex digits portion
        const hex_digits = self.source[digits_start..self.current];
        
        // Clean up the hex string by removing underscores
        const clean_hex = try self.removeUnderscores(hex_digits);
        defer self.allocator.free(clean_hex);

        // Special handling for MIN_INT (-0x80000000)
        if (is_negative and std.mem.eql(u8, clean_hex, "80000000")) {
            try self.addToken(.INT, .{ .int = std.math.minInt(i32) });
            return;
        }

        // Convert the hex string to an integer
        var int_val = std.fmt.parseInt(i32, clean_hex, 16) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            else => return err,
        };
        
        // Apply negative sign if needed
        if (is_negative) int_val = -int_val;
        
        // Create the final token and return
        try self.addToken(.INT, .{ .int = int_val });
    }

    fn handleBinary(self: *Lexer, is_negative: bool) !void {
        // If we're at 'b', back up to include the '0'
        if (self.source[self.current] == 'b') {
            self.current -= 1;
        }
        self.advance(); // consume '0'
        self.advance(); // consume 'b'
        
        // Must have at least one binary digit after 0b
        if (!isBinaryDigit(self.peekAt(0))) return error.InvalidNumber;
        
        // Mark where the actual binary digits begin (after 0b)
        const digits_start = self.current;
        
        // Consume all binary digits and underscores
        while (isBinaryDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }
        
        // Extract just the binary digits portion
        const bin_digits = self.source[digits_start..self.current];
        
        // Clean up the binary string by removing underscores
        const clean_bin = try self.removeUnderscores(bin_digits);
        defer self.allocator.free(clean_bin);

        // Special handling for MIN_INT (-0b10000000000000000000000000000000)
        if (is_negative and std.mem.eql(u8, clean_bin, "10000000000000000000000000000000")) {
            try self.addToken(.INT, .{ .int = std.math.minInt(i32) });
            return;
        }

        // Convert the binary string to an integer
        var int_val = std.fmt.parseInt(i32, clean_bin, 2) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            else => return err,
        };
        
        if (is_negative) int_val = -int_val;
        try self.addToken(.INT, .{ .int = int_val });
    }

    fn handleOctal(self: *Lexer, is_negative: bool) !void {
        // If we're at 'o', back up to include the '0'
        if (self.source[self.current] == 'o') {
            self.current -= 1;
        }
        self.advance(); // consume '0'
        self.advance(); // consume 'o'
        
        // Must have at least one octal digit after 0o
        if (!isOctalDigit(self.peekAt(0))) return error.InvalidNumber;
        
        // Mark where the actual octal digits begin (after 0o)
        const digits_start = self.current;
        
        // Consume all octal digits and underscores
        while (isOctalDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }
        
        // Extract just the octal digits portion
        const oct_digits = self.source[digits_start..self.current];
        
        // Clean up the octal string by removing underscores
        const clean_oct = try self.removeUnderscores(oct_digits);
        defer self.allocator.free(clean_oct);

        // Convert the octal string to an integer
        var int_val = std.fmt.parseInt(i32, clean_oct, 8) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            else => return err,
        };
        
        if (is_negative) int_val = -int_val;
        try self.addToken(.INT, .{ .int = int_val });
    }

    //======================================================================
    // Utilities
    //======================================================================

    fn removeUnderscores(self: *Lexer, input: []const u8) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();
        
        // Handle negative sign if present
        var i: u32 = 0;
        if (input.len > 0 and input[0] == '-') {
            try result.append('-');
            i = 1;
        }
        
        // Handle prefixes (0x, 0b)
        if (input.len - i >= 2 and input[i] == '0') {
            try result.append('0');
            const prefix = input[i + 1];
            if (prefix == 'x' or prefix == 'b') {
                try result.append(prefix);
                // Skip the prefix in the input
                i += 2;
                // Process the rest of the digits
                while (i < input.len) : (i += 1) {
                    if (input[i] != '_') {
                        try result.append(input[i]);
                    }
                }
            } else {
                // Regular number starting with 0
                i += 1;
                while (i < input.len) : (i += 1) {
                    if (input[i] != '_') {
                        try result.append(input[i]);
                    }
                }
            }
        } else {
            // Regular number not starting with 0
            while (i < input.len) : (i += 1) {
                if (input[i] != '_') {
                    try result.append(input[i]);
                }
            }
        }
        
        return result.toOwnedSlice();
    }

    fn validateUnderscores(input: []const u8) !void {
        var last_was_underscore = false;
        var has_digit = false;
        
        for (input, 0..) |c, i| {
            if (c == '_') {
                if (last_was_underscore) return error.InvalidNumber; // Double underscore
                if (i == 0 or i == input.len - 1) return error.InvalidNumber; // Leading/trailing underscore
                last_was_underscore = true;
            } else {
                last_was_underscore = false;
                if (std.ascii.isDigit(c)) has_digit = true;
            }
        }
        
        if (!has_digit) return error.InvalidNumber;
    }

    fn handleExponent(self: *Lexer) !void {
        self.advance(); // consume 'e' or 'E'
        
        // Handle optional sign
        var exp_is_negative = false;
        if (self.peekAt(0) == '+' or self.peekAt(0) == '-') {
            exp_is_negative = self.peekAt(0) == '-';
            self.advance();
        }
        
        // Must have at least one digit
        if (!isDigit(self.peekAt(0))) return error.InvalidExponent;
        
        var exp_value: i32 = 0;
        var has_digit = false;
        
        while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            if (isDigit(self.peekAt(0))) {
                exp_value = exp_value * 10 + (self.peekAt(0) - '0');
                has_digit = true;
                
                // Check for exponent overflow
                if (exp_value > 308) return error.Overflow; // Max double exponent
            }
            self.advance();
        }
        
        if (!has_digit) return error.InvalidExponent;
        if (exp_is_negative) exp_value = -exp_value;
        
        return exp_value;
    }

    fn validateBasePrefix(self: *Lexer) !enum { Decimal, Hex, Binary, Octal } {
        if (self.peekAt(0) != '0') return .Decimal;
        
        const next_char = self.peekAt(1);
        switch (next_char) {
            'x', 'X' => {
                if (!isHexDigit(self.peekAt(2))) return error.InvalidNumber;
                return .Hex;
            },
            'b', 'B' => {
                if (!isBinaryDigit(self.peekAt(2))) return error.InvalidNumber;
                return .Binary;
            },
            'o', 'O' => {
                if (!isOctalDigit(self.peekAt(2))) return error.InvalidNumber;
                return .Octal;
            },
            '0'...'9' => return error.LeadingZeros,
            else => return .Decimal,
        }
    }
};