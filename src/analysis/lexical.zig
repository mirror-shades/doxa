const std = @import("std");
const Reporting = @import("../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const TokenImport = @import("../types/token.zig");
const Token = TokenImport.Token;
const TokenType = TokenImport.TokenType;

const TypesImport = @import("../types/types.zig");
const Tetra = TypesImport.Tetra;
const TokenLiteral = TypesImport.TokenLiteral;

pub const LexicalAnalyzer = struct {
    keywords: std.StringHashMap(TokenType),
    tokens: std.array_list.Managed(Token),
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    allocator: std.mem.Allocator,
    allocated_strings: std.array_list.Managed([]const u8),
    allocated_arrays: std.array_list.Managed([]const TokenLiteral),
    line_start: usize,
    file_path: []const u8,
    file_uri: []const u8,
    token_line: usize,
    reporter: *Reporter,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8, reporter: *Reporter) !LexicalAnalyzer {
        const file_uri = try reporter.ensureFileUri(file_path);
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .column = 1,
            .line_start = 0,
            .allocator = allocator,
            .tokens = std.array_list.Managed(Token).init(allocator),
            .keywords = std.StringHashMap(TokenType).init(allocator),
            .allocated_strings = std.array_list.Managed([]const u8).init(allocator),
            .allocated_arrays = std.array_list.Managed([]const TokenLiteral).init(allocator),
            .file_path = file_path,
            .file_uri = file_uri,
            .token_line = 1,
            .reporter = reporter,
        };
    }

    pub fn deinit(self: *LexicalAnalyzer) void {
        for (self.allocated_strings.items) |str| {
            self.allocator.free(str);
        }
        self.allocated_strings.deinit();

        for (self.allocated_arrays.items) |arr| {
            self.allocator.free(arr);
        }
        self.allocated_arrays.deinit();

        self.tokens.deinit();
        self.keywords.deinit();
    }

    pub fn initKeywords(self: *LexicalAnalyzer) !void {
        try self.keywords.put("pub", .PUBLIC);
        try self.keywords.put("public", .PUBLIC);
        try self.keywords.put("true", .LOGIC);
        try self.keywords.put("false", .LOGIC);
        try self.keywords.put("both", .LOGIC);
        try self.keywords.put("neither", .LOGIC);
        try self.keywords.put("if", .IF);
        try self.keywords.put("then", .THEN);
        try self.keywords.put("else", .ELSE);
        try self.keywords.put("while", .WHILE);
        try self.keywords.put("do", .DO);
        try self.keywords.put("for", .FOR);
        try self.keywords.put("each", .EACH);
        try self.keywords.put("function", .FUNCTION);
        try self.keywords.put("method", .METHOD);
        try self.keywords.put("returns", .RETURNS);
        try self.keywords.put("return", .RETURN);
        try self.keywords.put("unreachable", .UNREACHABLE);
        try self.keywords.put("this", .THIS);
        try self.keywords.put("const", .CONST);
        try self.keywords.put("var", .VAR);
        try self.keywords.put("struct", .STRUCT);
        try self.keywords.put("break", .BREAK);
        try self.keywords.put("continue", .CONTINUE);
        try self.keywords.put("and", .AND);
        try self.keywords.put("or", .OR);
        try self.keywords.put("nothing", .NOTHING);
        try self.keywords.put("import", .IMPORT);
        try self.keywords.put("module", .MODULE);
        try self.keywords.put("match", .MATCH);
        try self.keywords.put("enum", .ENUM);
        try self.keywords.put("async", .ASYNC);
        try self.keywords.put("await", .AWAIT);
        try self.keywords.put("entry", .ENTRY);
        try self.keywords.put("is", .ASSIGN);
        try self.keywords.put("equals", .EQUALITY);
        try self.keywords.put("as", .AS);
        try self.keywords.put("from", .FROM);
        try self.keywords.put("to", .TO);
        try self.keywords.put("int", .INT_TYPE);
        try self.keywords.put("byte", .BYTE_TYPE);
        try self.keywords.put("float", .FLOAT_TYPE);
        try self.keywords.put("string", .STRING_TYPE);
        try self.keywords.put("tetra", .TETRA_TYPE);
        try self.keywords.put("array", .ARRAY_TYPE);
        try self.keywords.put("struct", .STRUCT_TYPE);
        try self.keywords.put("enum", .ENUM_TYPE);
        try self.keywords.put("map", .MAP_TYPE);
        try self.keywords.put("xor", .XOR);
        try self.keywords.put("exists", .EXISTS);
        try self.keywords.put("forall", .FORALL);
        try self.keywords.put("where", .WHERE);
        try self.keywords.put("iff", .IFF);
        try self.keywords.put("implies", .IMPLIES);
        try self.keywords.put("nand", .NAND);
        try self.keywords.put("nor", .NOR);
        try self.keywords.put("not", .NOT);
        try self.keywords.put("in", .IN);
        try self.keywords.put("at", .AT);
        try self.keywords.put("to", .RANGE);
        try self.keywords.put("∃", .EXISTS);

        try self.keywords.put("∀", .FORALL);
        try self.keywords.put("∈", .IN);
        try self.keywords.put("¬", .NOT);
        try self.keywords.put("⊕", .XOR);
        try self.keywords.put("↔", .IFF);
        try self.keywords.put("∧", .AND);
        try self.keywords.put("∨", .OR);
        try self.keywords.put("↑", .NAND);
        try self.keywords.put("↓", .NOR);
        try self.keywords.put("→", .IMPLIES);
        //try self.keywords.put("⊞", .AND_PARADOXICAL); // TODO: let you add true and false to get both
        try self.keywords.put("⊟", .NOT_PARADOXICAL);
    }

    pub fn lexTokens(self: *LexicalAnalyzer) !std.array_list.Managed(Token) {
        while (!self.isAtEnd()) {
            try self.getNextToken();
        }

        try self.tokens.append(Token.init(.EOF, "", .nothing, self.line, self.column));
        return self.tokens;
    }

    fn getNextToken(self: *LexicalAnalyzer) (ErrorList || std.mem.Allocator.Error)!void {
        while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t')) {
            self.advance();
        }

        if (self.isAtEnd()) return;

        self.start = self.current;
        self.token_line = self.line;
        const c = self.peekAt(0);

        if (c >= 0x80) {
            const sequence_length = try std.unicode.utf8ByteSequenceLength(c);
            if (sequence_length <= self.source[self.current..].len) {
                const symbol = self.source[self.current .. self.current + sequence_length];
                if (self.keywords.get(symbol)) |keyword_type| {
                    var i: usize = 0;
                    while (i < sequence_length) : (i += 1) {
                        self.advance();
                    }
                    try self.addMinimalToken(keyword_type);
                    return;
                }
            }
            try self.identifier();
            return;
        }

        self.advance();

        switch (c) {
            'r' => {
                if (self.peekAt(0) == '"') {
                    self.current = self.start;
                    try self.rawString();
                } else {
                    try self.identifier();
                }
            },

            '/' => {
                if (self.match('*')) {
                    var nesting: usize = 1;

                    while (nesting > 0 and !self.isAtEnd()) {
                        if (self.peekAt(0) == '/' and self.peekAt(1) == '*') {
                            self.advance();
                            self.advance();
                            nesting += 1;
                        } else if (self.peekAt(0) == '*' and self.peekAt(1) == '/') {
                            self.advance();
                            self.advance();
                            nesting -= 1;
                        } else {
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
                    while (!self.isAtEnd() and (self.peekAt(0) == ' ' or self.peekAt(0) == '\r' or self.peekAt(0) == '\t' or self.peekAt(0) == '\n')) {
                        self.advance();
                    }
                }
            },

            '(' => {
                try self.addMinimalToken(.LEFT_PAREN);
            },
            ':' => {
                if (self.match(':')) {
                    try self.addMinimalToken(.TYPE_SYMBOL);
                } else {
                    try self.addMinimalToken(.WHERE);
                }
            },
            ')' => try self.addMinimalToken(.RIGHT_PAREN),
            '{' => try self.addMinimalToken(.LEFT_BRACE),
            '}' => try self.addMinimalToken(.RIGHT_BRACE),
            ',' => try self.addMinimalToken(.COMMA),
            '.' => {
                if (self.peekAt(0) == '.') {
                    self.advance();
                    if (self.peekAt(0) == '.') {
                        self.advance();
                        if (self.tokens.items.len > 0 and self.tokens.items[self.tokens.items.len - 1].type == .NEWLINE) {
                            try self.removeLastToken();
                        } else {
                            return error.EllipsisWithoutNewline;
                        }
                    } else {
                        try self.addMinimalToken(.DOT_DOT);
                    }
                } else if (isDigit(self.peekAt(0))) {
                    self.current -= 1;
                    self.start = self.current;
                    try self.number();
                } else if (isAlpha(self.peekAt(0))) {
                    try self.addMinimalToken(.DOT);
                    self.start = self.current;
                    while (!self.isAtEnd() and (isAlpha(self.peekAt(0)) or isDigit(self.peekAt(0)) or self.peekAt(0) == '_')) {
                        self.advance();
                    }
                    const lexeme = self.source[self.start..self.current];
                    try self.addToken(.IDENTIFIER, .{ .string = lexeme });
                } else {
                    try self.addMinimalToken(.DOT);
                }
            },
            ';' => {
                try self.addMinimalToken(.SEMICOLON);
                try self.tokens.append(Token.initWithFile(.NEWLINE, "", .nothing, self.line, 1, self.file_path, self.file_uri));
            },
            '%' => try self.addMinimalToken(.MODULO),
            '#' => {
                if (!self.isAtEnd()) {
                    while (!self.isAtEnd() and self.peekAt(0) != '\n') {
                        self.advance();
                    }
                    if (!self.isAtEnd() and self.peekAt(0) == '\n') {
                        self.advance();
                        if (self.tokens.items.len == 0 or self.tokens.items[self.tokens.items.len - 1].type != .NEWLINE) {
                            const tok_line: usize = if (self.line > 0) self.line - 1 else 0;
                            try self.tokens.append(Token.initWithFile(.NEWLINE, "", .nothing, tok_line, 1, self.file_path, self.file_uri));
                        }
                    }
                }
            },
            '^' => try self.addMinimalToken(.CARET),
            '*' => {
                if (self.match('*')) {
                    if (self.match('=')) {
                        try self.addMinimalToken(.POWER_EQUAL);
                    } else {
                        try self.addMinimalToken(.POWER);
                    }
                } else if (self.match('=')) {
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
            '|' => try self.addMinimalToken(.PIPE),
            '=' => {
                if (self.match('=')) {
                    try self.addMinimalToken(.EQUALITY);
                } else if (self.match(' ')) {
                    const location = Location{
                        .file = self.file_path,
                        .file_uri = self.file_uri,
                        .range = .{
                            .start_line = self.line,
                            .start_col = self.column,
                            .end_line = self.line,
                            .end_col = self.column,
                        },
                    };
                    self.reporter.reportCompileError(location, ErrorCode.USE_IS_FOR_ASSIGNMENT, "equals sign '=' is not used for variable declarations, use 'is' instead", .{});
                    return error.UseIsForAssignment;
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
                    try self.addMinimalToken(.INCREMENT);
                } else if (self.match('=')) {
                    try self.addMinimalToken(.PLUS_EQUAL);
                } else {
                    try self.addMinimalToken(.PLUS);
                }
            },
            '-' => {
                if (self.match('-')) {
                    try self.addMinimalToken(.DECREMENT);
                } else if (self.match('=')) {
                    try self.addMinimalToken(.MINUS_EQUAL);
                } else if (isDigit(self.peekAt(0)) or self.peekAt(0) == '.' or
                    (self.peekAt(0) == '0' and (self.peekAt(1) == 'x' or self.peekAt(1) == 'b')))
                {
                    // This is a negative number - Back up to include the minus sign
                    self.current -= 1;
                    self.start = self.current;
                    try self.number();
                } else {
                    try self.addMinimalToken(.MINUS);
                }
            },
            '@' => {
                try self.internalMethod();
            },

            '$' => {
                try self.structInstance();
            },
            '0'...'9' => try self.number(),

            '"' => try self.string(),
            '[' => try self.addMinimalToken(.LEFT_BRACKET),
            ']' => try self.addMinimalToken(.RIGHT_BRACKET),
            ' ', '\r', '\t' => {},
            '\n' => {
                const length = self.tokens.items.len;
                var should_add_newline = false;
                if (length == 0) {
                    should_add_newline = true;
                } else if (self.tokens.items[length - 1].type != .NEWLINE) {
                    should_add_newline = true;
                } else if (length > 1) {
                    should_add_newline = self.tokens.items[length - 2].type == .SEMICOLON;
                }

                if (should_add_newline) {
                    const tok_line: usize = if (self.line > 0) self.line - 1 else 0;
                    try self.tokens.append(Token.initWithFile(.NEWLINE, "", .nothing, tok_line, 1, self.file_path, self.file_uri));
                }
            },
            else => {
                if (isAlpha(c)) {
                    try self.identifier();
                } else {
                    std.debug.print("Unexpected character: {c}\n", .{c});
                    return error.UnexpectedCharacter;
                }
            },
            '~' => try self.addMinimalToken(.TILDE),
            '?' => try self.addMinimalToken(.PEEK),
        }
    }

    fn addMinimalToken(self: *LexicalAnalyzer, token_type: TokenType) !void {
        try self.addToken(token_type, .nothing);
    }

    fn addToken(self: *LexicalAnalyzer, token_type: TokenType, literal: TokenLiteral) !void {
        try self.addLongToken(token_type, literal, self.source[self.start..self.current]);
    }

    fn addLongToken(self: *LexicalAnalyzer, token_type: TokenType, literal: TokenLiteral, lexeme: []const u8) !void {
        var tracked_literal = literal;
        if (literal == .string) {
            const owned_str = try self.addString(literal.string);
            tracked_literal = .{ .string = owned_str };
        }

        var tracked_lexeme = lexeme;
        const lexeme_start = @intFromPtr(lexeme.ptr);
        const source_start = @intFromPtr(self.source.ptr);
        const source_end = source_start + self.source.len;

        if (lexeme_start < source_start or lexeme_start >= source_end) {
            tracked_lexeme = try self.addString(lexeme);
        }

        const token_line = self.token_line;
        // Calculate column position relative to the line where the token started
        // We need to find the line start for the token's line
        var token_line_start: usize = 0;
        var current_line: usize = 1;
        var i: usize = 0;
        while (i < self.start and current_line < token_line) {
            if (self.source[i] == '\n') {
                current_line += 1;
                token_line_start = i + 1;
            }
            i += 1;
        }
        const token_column = self.start - token_line_start + 1;

        try self.tokens.append(Token.initWithFile(token_type, tracked_lexeme, tracked_literal, token_line, token_column, self.file_path, self.file_uri));
    }

    fn peekAt(self: *LexicalAnalyzer, offset: i32) u8 {
        if (offset < 0) {
            const abs_offset = @abs(offset);
            if (abs_offset > self.current) return 0;
            return self.source[self.current - @as(usize, abs_offset)];
        }

        if (offset >= 0) {
            const pos = self.current + @as(usize, @intCast(offset));
            if (pos >= self.source.len) return 0;
            return self.source[pos];
        }

        return 0;
    }

    fn isAtEnd(self: *LexicalAnalyzer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *LexicalAnalyzer) void {
        if (self.current < self.source.len) {
            if (self.source[self.current] == '\n') {
                self.line += 1;
                self.line_start = self.current + 1;
                self.column = 1;
            } else {
                self.column = self.current - self.line_start + 1;
            }
            self.current += 1;
        }
    }

    fn match(self: *LexicalAnalyzer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.advance();
        return true;
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isOperator(c: u8) bool {
        return c == '+' or c == '-' or c == '*' or c == '/' or c == '%' or c == '!' or c == '=' or c == '<' or c == '>';
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

    fn identifier(self: *LexicalAnalyzer) !void {
        while (!self.isAtEnd()) {
            const remaining = self.source[self.current..];

            if (remaining[0] >= 0x80) {
                std.debug.print("Unexpected character in identifier: {c}\n", .{remaining[0]});
                return error.UnexpectedCharacter;
            }

            if (isAlpha(remaining[0]) or isDigit(remaining[0]) or remaining[0] == '_') {
                self.advance();
                continue;
            }
            break;
        }

        const text = self.source[self.start..self.current];
        if (self.keywords.get(text)) |keyword_type| {
            switch (keyword_type) {
                .LOGIC => {
                    if (std.mem.eql(u8, text, "true") or std.mem.eql(u8, text, "false")) {
                        const value = if (std.mem.eql(u8, text, "true"))
                            Tetra.true
                        else
                            Tetra.false;
                        try self.addToken(.LOGIC, .{ .tetra = value });
                    } else if (std.mem.eql(u8, text, "both") or std.mem.eql(u8, text, "neither")) {
                        const value: Tetra = if (std.mem.eql(u8, text, "both"))
                            .both
                        else
                            .neither;
                        try self.addToken(.LOGIC, .{ .tetra = value });
                    }
                },
                else => try self.addMinimalToken(keyword_type),
            }
        } else {
            try self.addToken(.IDENTIFIER, .{ .string = text });
        }
    }

    fn tetra(self: *LexicalAnalyzer) !void {
        try self.addToken(.TETRA, .{ .tetra = self.source[self.start..self.current] });
    }

    fn parenthesis(self: *LexicalAnalyzer) !void {
        try self.addMinimalToken(.LEFT_PAREN);
    }

    fn string(self: *LexicalAnalyzer) !void {
        var result = std.array_list.Managed(u8).init(self.allocator);
        errdefer result.deinit();

        while (!self.isAtEnd() and self.peekAt(0) != '"') {
            const c = self.peekAt(0);

            if (c == '\n' or c == '\r') {
                // Look ahead to see if there's an ellipsis continuation
                var lookahead_pos = self.current + 1; // Skip the newline/carriage return
                var found_ellipsis = false;

                // Skip whitespace and any remaining carriage returns/newlines
                while (lookahead_pos < self.source.len and
                    (self.source[lookahead_pos] == ' ' or
                        self.source[lookahead_pos] == '\t' or
                        self.source[lookahead_pos] == '\r' or
                        self.source[lookahead_pos] == '\n'))
                {
                    lookahead_pos += 1;
                }

                // Check for ellipsis
                if (lookahead_pos + 2 < self.source.len and
                    self.source[lookahead_pos] == '.' and
                    self.source[lookahead_pos + 1] == '.' and
                    self.source[lookahead_pos + 2] == '.')
                {
                    found_ellipsis = true;
                    // Skip the ellipsis
                    lookahead_pos += 3;
                }

                if (found_ellipsis) {
                    // Skip the newline/carriage return and ellipsis, continue processing
                    self.current = lookahead_pos;
                    // Don't update line tracking - self.start points to the token start
                    continue; // Don't add the newline/carriage return to the result
                } else {
                    // Add the newline/carriage return as a literal character
                    self.advance(); // Advance past the newline/carriage return
                    try result.append(c);
                }
            } else {
                self.advance();
            }

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
                    '{' => try result.append('{'),
                    '}' => try result.append('}'),
                    'u' => {
                        if (self.peekAt(0) != '{') return error.InvalidUnicodeEscape;
                        self.advance();
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
                        self.advance();
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

        self.advance();

        const lexeme = self.source[self.start..self.current];
        const string_content = try result.toOwnedSlice();

        try self.addLongToken(.STRING, .{ .string = string_content }, lexeme);
    }

    fn rawString(self: *LexicalAnalyzer) !void {
        self.advance();
        self.advance();
        var result = std.array_list.Managed(u8).init(self.allocator);
        errdefer result.deinit();

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

        self.advance();
        const lexeme = self.source[self.start..self.current];
        const string_content = try result.toOwnedSlice();
        try self.addLongToken(.STRING, .{ .string = string_content }, lexeme);
    }

    fn internalMethod(self: *LexicalAnalyzer) !void {
        const name_start = self.current;
        const method_name = self.getMethodName();
        self.start = name_start;

        if (std.mem.eql(u8, method_name, "type")) {
            try self.addToken(.TYPE, .nothing);
        } else if (std.mem.eql(u8, method_name, "length")) {
            try self.addToken(.LENGTH, .nothing);
        } else if (std.mem.eql(u8, method_name, "slice")) {
            try self.addToken(.SLICE, .nothing);
        } else if (std.mem.eql(u8, method_name, "push")) {
            try self.addToken(.PUSH, .nothing);
        } else if (std.mem.eql(u8, method_name, "pop")) {
            try self.addToken(.POP, .nothing);
        } else if (std.mem.eql(u8, method_name, "insert")) {
            try self.addToken(.INSERT, .nothing);
        } else if (std.mem.eql(u8, method_name, "remove")) {
            try self.addToken(.REMOVE, .nothing);
        } else if (std.mem.eql(u8, method_name, "clear")) {
            try self.addToken(.CLEAR, .nothing);
        } else if (std.mem.eql(u8, method_name, "find")) {
            try self.addToken(.FIND, .nothing);
        } else if (std.mem.eql(u8, method_name, "string")) {
            try self.addToken(.TOSTRING, .nothing);
        } else if (std.mem.eql(u8, method_name, "int")) {
            try self.addToken(.TOINT, .nothing);
        } else if (std.mem.eql(u8, method_name, "float")) {
            try self.addToken(.TOFLOAT, .nothing);
        } else if (std.mem.eql(u8, method_name, "byte")) {
            try self.addToken(.TOBYTE, .nothing);
        } else if (std.mem.eql(u8, method_name, "print")) {
            try self.addToken(.PRINT, .nothing);
        } else if (std.mem.eql(u8, method_name, "assert")) {
            try self.addToken(.ASSERT, .nothing);
        } else if (std.mem.eql(u8, method_name, "panic")) {
            try self.addToken(.PANIC, .nothing);
        } else if (std.mem.eql(u8, method_name, "input")) {
            try self.addToken(.INPUT, .nothing);
        } else if (std.mem.eql(u8, method_name, "os")) {
            try self.addToken(.OS, .nothing);
        } else if (std.mem.eql(u8, method_name, "arch")) {
            try self.addToken(.ARCH, .nothing);
        } else if (std.mem.eql(u8, method_name, "abi")) {
            try self.addToken(.ABI, .nothing);
        } else if (std.mem.eql(u8, method_name, "time")) {
            try self.addToken(.TIME, .nothing);
        } else if (std.mem.eql(u8, method_name, "tick")) {
            try self.addToken(.TICK, .nothing);
        } else if (std.mem.eql(u8, method_name, "exit")) {
            try self.addToken(.EXIT, .nothing);
        } else if (std.mem.eql(u8, method_name, "sleep")) {
            try self.addToken(.SLEEP, .nothing);
        } else if (std.mem.eql(u8, method_name, "random")) {
            try self.addToken(.RANDOM, .nothing);
        } else if (std.mem.eql(u8, method_name, "build")) {
            try self.addToken(.BUILD, .nothing);
        } else if (std.mem.eql(u8, method_name, "read")) {
            try self.addToken(.READ, .nothing);
        } else {
            return error.InvalidInternalMethod;
        }
    }

    fn structInstance(self: *LexicalAnalyzer) !void {
        try self.addMinimalToken(.STRUCT_INSTANCE);
    }

    fn removeLastToken(self: *LexicalAnalyzer) !void {
        if (self.tokens.items.len > 0) {
            _ = self.tokens.pop();
        }
    }

    fn getMethodName(self: *LexicalAnalyzer) []const u8 {
        var buffer: [20]u8 = undefined;
        var i: usize = 0;

        while (!self.isAtEnd() and self.peekAt(0) != '(' and self.peekAt(0) != ' ' and i < buffer.len) {
            buffer[i] = self.peekAt(0);
            self.advance();
            i += 1;
        }

        const result = self.source[self.current - i .. self.current];
        return result;
    }

    fn number(self: *LexicalAnalyzer) !void {
        var has_digits = false;
        var has_decimal = false;
        var has_exponent = false;
        var is_negative = false;
        var has_sign = false;

        if (self.source[self.start] == '-') {
            is_negative = true;
        }
        if (self.source[self.start] == '+' or self.source[self.start] == '-') {
            has_sign = true;
            self.advance();
        }
        const first_char = if (has_sign and self.start + 1 < self.source.len)
            self.source[self.start + 1]
        else
            self.source[self.start];

        if (isDigit(first_char)) {
            has_digits = true;
        }

        if (self.current < self.source.len - 1 and
            ((has_sign and self.source[self.current] == '0') or
                self.source[self.current - 1] == '0'))
        {
            const next_char = if (has_sign) self.peekAt(1) else self.peekAt(0);
            switch (next_char) {
                'x' => {
                    try self.handleHex(is_negative);
                    return;
                },
                'b' => {
                    try self.handleBinary(is_negative);
                    return;
                },
                'o' => {
                    try self.handleOctal(is_negative);
                    return;
                },
                else => {},
            }
        }

        while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            if (isDigit(self.peekAt(0))) {
                if (has_digits == false and self.peekAt(0) == '0' and
                    isDigit(self.peekAt(1)))
                {
                    return error.LeadingZeros;
                }
                has_digits = true;
            }
            self.advance();
        }

        if (self.peekAt(0) == '.') {
            has_decimal = true;
            self.advance();
            while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
                if (isDigit(self.peekAt(0))) has_digits = true;
                self.advance();
            }
        }

        if (self.peekAt(0) == '.') {
            has_decimal = true;
            self.advance();
            if (!isDigit(self.peekAt(0)) or self.peekAt(0) != '.') return error.InvalidNumber;

            while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
                if (isDigit(self.peekAt(0))) has_digits = true;
                self.advance();
            }
        }

        if (self.peekAt(0) == 'e' or self.peekAt(0) == 'E') {
            if (has_exponent) return error.MultipleExponents;
            has_exponent = true;
            has_decimal = true;
            self.advance();

            if (self.peekAt(0) == '+' or self.peekAt(0) == '-') {
                self.advance();
            }

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
            const num_str_buf = try self.allocator.alloc(u8, num_str.len);
            defer self.allocator.free(num_str_buf);
            const num_str_lower = std.ascii.lowerString(num_str_buf, num_str);

            if (std.mem.eql(u8, num_str_lower, "inf") or std.mem.eql(u8, num_str_lower, "infinity")) {
                const float_val = if (is_negative) -std.math.inf(f64) else std.math.inf(f64);
                try self.addToken(.FLOAT, .{ .float = float_val });
                return;
            }
            if (std.mem.eql(u8, num_str_lower, "nan")) {
                try self.addToken(.FLOAT, .{ .float = std.math.nan(f64) });
                return;
            }

            const float_val = std.fmt.parseFloat(f64, num_str) catch {
                return error.InvalidNumber;
            };

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

    fn handleHex(self: *LexicalAnalyzer, is_negative: bool) !void {
        if (is_negative) return error.InvalidNumber;

        var digits_start: usize = self.current;
        if (self.source[self.current] == 'x') {
            self.current -= 1;
        }
        self.advance();
        self.advance();

        if (!isHexDigit(self.peekAt(0))) return error.InvalidNumber;

        digits_start = self.current;

        while (isHexDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }

        const hex_digits = self.source[digits_start..self.current];

        const clean_hex = try self.removeUnderscores(hex_digits);
        defer self.allocator.free(clean_hex);

        if (clean_hex.len > 2) {
            return error.ByteValueTooLarge;
        }

        const byte_val = std.fmt.parseInt(u8, clean_hex, 16) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            error.Overflow => return error.ByteValueTooLarge,
            else => return err,
        };

        try self.addToken(.BYTE, .{ .byte = byte_val });
    }

    fn handleBinary(self: *LexicalAnalyzer, is_negative: bool) !void {
        if (self.source[self.current] == 'b') {
            self.current -= 1;
        }
        self.advance();
        self.advance();

        if (!isBinaryDigit(self.peekAt(0))) return error.InvalidNumber;

        const digits_start = self.current;

        while (isBinaryDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }

        const bin_digits = self.source[digits_start..self.current];

        const clean_bin = try self.removeUnderscores(bin_digits);
        defer self.allocator.free(clean_bin);

        if (is_negative and std.mem.eql(u8, clean_bin, "10000000000000000000000000000000")) {
            try self.addToken(.INT, .{ .int = std.math.minInt(i32) });
            return;
        }

        var int_val = std.fmt.parseInt(i32, clean_bin, 2) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            else => return err,
        };

        if (is_negative) int_val = -int_val;
        try self.addToken(.INT, .{ .int = int_val });
    }

    fn handleOctal(self: *LexicalAnalyzer, is_negative: bool) !void {
        if (self.source[self.current] == 'o') {
            self.current -= 1;
        }
        self.advance();
        self.advance();

        if (!isOctalDigit(self.peekAt(0))) return error.InvalidNumber;

        const digits_start = self.current;

        while (isOctalDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            self.advance();
        }

        const oct_digits = self.source[digits_start..self.current];

        const clean_oct = try self.removeUnderscores(oct_digits);
        defer self.allocator.free(clean_oct);

        var int_val = std.fmt.parseInt(i32, clean_oct, 8) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidNumber,
            else => return err,
        };

        if (is_negative) int_val = -int_val;
        try self.addToken(.INT, .{ .int = int_val });
    }

    fn removeUnderscores(self: *LexicalAnalyzer, input: []const u8) ![]const u8 {
        var result = std.array_list.Managed(u8).init(self.allocator);
        errdefer result.deinit();

        var i: usize = 0;
        if (input.len > 0 and input[0] == '-') {
            try result.append('-');
            i = 1;
        }

        if (input.len - i >= 2 and input[i] == '0') {
            try result.append('0');
            const prefix = input[i + 1];
            if (prefix == 'x' or prefix == 'b') {
                try result.append(prefix);
                i += 2;
                while (i < input.len) : (i += 1) {
                    if (input[i] != '_') {
                        try result.append(input[i]);
                    }
                }
            } else {
                i += 1;
                while (i < input.len) : (i += 1) {
                    if (input[i] != '_') {
                        try result.append(input[i]);
                    }
                }
            }
        } else {
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
                if (last_was_underscore) return error.InvalidNumber;
                if (i == 0 or i == input.len - 1) return error.InvalidNumber;
                last_was_underscore = true;
            } else {
                last_was_underscore = false;
                if (std.ascii.isDigit(c)) has_digit = true;
            }
        }

        if (!has_digit) return error.InvalidNumber;
    }

    fn handleExponent(self: *LexicalAnalyzer) !void {
        self.advance();

        var exp_is_negative = false;
        if (self.peekAt(0) == '+' or self.peekAt(0) == '-') {
            exp_is_negative = self.peekAt(0) == '-';
            self.advance();
        }

        if (!isDigit(self.peekAt(0))) return error.InvalidExponent;

        var exp_value: i32 = 0;
        var has_digit = false;

        while (isDigit(self.peekAt(0)) or self.peekAt(0) == '_') {
            if (isDigit(self.peekAt(0))) {
                exp_value = exp_value * 10 + (self.peekAt(0) - '0');
                has_digit = true;

                if (exp_value > 308) return error.Overflow;
            }
            self.advance();
        }

        if (!has_digit) return error.InvalidExponent;
        if (exp_is_negative) exp_value = -exp_value;

        return exp_value;
    }

    fn validateBasePrefix(self: *LexicalAnalyzer) !enum { Decimal, Hex, Binary, Octal } {
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

    pub fn addString(self: *LexicalAnalyzer, str: []const u8) ![]const u8 {
        const owned = try self.allocator.dupe(u8, str);
        try self.allocated_strings.append(owned);
        return owned;
    }

    pub fn addArray(self: *LexicalAnalyzer, arr: []const TokenLiteral) ![]const TokenLiteral {
        const owned = try self.allocator.dupe(TokenLiteral, arr);
        try self.allocated_arrays.append(owned);
        return owned;
    }

    fn countLines(self: *LexicalAnalyzer, start: usize, end: usize) struct { line: i32, column: usize } {
        var line: i32 = 1;
        var last_newline: usize = 0;

        var i: usize = 0;
        while (i < end) : (i += 1) {
            if (self.source[i] == '\n') {
                line += 1;
                last_newline = i + 1;
            }
            if (i == start) {
                break;
            }
        }

        const result = .{
            .line = line,
            .column = start - last_newline + 1,
        };

        return result;
    }
};
