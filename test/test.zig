const std = @import("std");
const lexer_mod = @import("lexer");
const TokenType = lexer_mod.TokenType;

// Basic operators and delimiters
test "lexer - operators and delimiters" {
    const allocator = std.testing.allocator;
    
    {
        var lex = lexer_mod.Lexer.init(allocator, "+ - * / = == != < <= > >= ( ) { } [ ] , ; .");
        defer lex.deinit();
        try lex.initKeywords();
        
        const tokens = try lex.lexTokens();
        const expected_types = [_]TokenType{
            .PLUS, .MINUS, .ASTERISK, .SLASH, 
            .EQUAL, .EQUAL_EQUAL, .BANG_EQUAL,
            .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL,
            .LEFT_PAREN, .RIGHT_PAREN,
            .LEFT_BRACE, .RIGHT_BRACE,
            .LEFT_BRACKET, .RIGHT_BRACKET,
            .COMMA, .SEMICOLON, .DOT,
            .EOF,
        };
        
        try std.testing.expectEqual(expected_types.len, tokens.items.len);
        for (tokens.items, 0..) |token, i| {
            try std.testing.expectEqual(expected_types[i], token.type);
        }
    }
}

// Number formats
test "lexer - number formats" {
    const allocator = std.testing.allocator;
    
    {
        var lex = lexer_mod.Lexer.init(allocator, 
            \\0 123 1_234_567                   // integers
            \\0.0 3.14159 1_234.567_89          // decimals
            \\1e5 1.5e-3 1_234.567_89e+12       // scientific notation
            \\0x0 0xff 0xFF 0xDEAD_BEEF         // hex
            \\0x1.5p3 0x1.4p-2 0xA.Bp+2         // hex float
            \\0b0 0b1010 0b1111_0000            // binary
        );
        defer lex.deinit();
        try lex.initKeywords();
        
        const tokens = try lex.lexTokens();
        
        // Debug print all tokens
        std.debug.print("\nAll tokens:\n", .{});
        for (tokens.items, 0..) |token, i| {
            std.debug.print("Token {d}: type={s}, lexeme='{s}', literal={any}\n", .{
                i,
                @tagName(token.type),
                token.lexeme,
                token.literal,
            });
        }
    }
}

// String variants
test "lexer - string formats" {
    const allocator = std.testing.allocator;
    
    {
        var lex = lexer_mod.Lexer.init(allocator,
            \\"" "hello" "hello\nworld" "escaped\"quote"
            \\r"raw\string\here" r"multiple
            \\lines"
        );
        defer lex.deinit();
        try lex.initKeywords();
        
        const tokens = try lex.lexTokens();
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[0].type);
        try std.testing.expectEqualStrings("", tokens.items[0].literal.string);
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[1].type);
        try std.testing.expectEqualStrings("hello", tokens.items[1].literal.string);
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[2].type);
        try std.testing.expectEqualStrings("hello\nworld", tokens.items[2].literal.string);
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[3].type);
        try std.testing.expectEqualStrings("escaped\"quote", tokens.items[3].literal.string);
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[4].type);
        try std.testing.expectEqualStrings("raw\\string\\here", tokens.items[4].literal.string);
        
        try std.testing.expectEqual(TokenType.STRING, tokens.items[5].type);
        try std.testing.expectEqualStrings("multiple\nlines", tokens.items[5].literal.string);
    }
}

// Error cases
test "lexer - error handling" {
    const allocator = std.testing.allocator;
    
    // Test invalid numbers
    {
        const test_cases = [_][]const u8{
            "0x",      // Hex with no digits
            "0b",      // Binary with no digits
            "0o",      // Octal with no digits
            "0xZ",     // Invalid hex digit
            "0b2",     // Invalid binary digit
            "1.2.3",   // Multiple decimal points
            "1e",      // Incomplete scientific notation
            "1e+",     // Incomplete scientific notation with sign
        };
        
        for (test_cases) |test_case| {
            var lex = lexer_mod.Lexer.init(allocator, test_case);
            defer lex.deinit();
            try lex.initKeywords();
            try std.testing.expectError(error.InvalidNumber, lex.lexTokens());
        }
    }
}

// Keywords and identifiers
test "lexer - keywords and identifiers" {
    const allocator = std.testing.allocator;
    
    {
        var lex = lexer_mod.Lexer.init(allocator,
            \\if else while for function return
            \\true false nothing
            \\myVar _test test123 UPPERCASE
        );
        defer lex.deinit();
        try lex.initKeywords();
        
        const tokens = try lex.lexTokens();
        
        // Keywords
        try std.testing.expectEqual(TokenType.IF, tokens.items[0].type);
        try std.testing.expectEqual(TokenType.ELSE, tokens.items[1].type);
        try std.testing.expectEqual(TokenType.WHILE, tokens.items[2].type);
        try std.testing.expectEqual(TokenType.FOR, tokens.items[3].type);
        try std.testing.expectEqual(TokenType.FUNCTION_KEYWORD, tokens.items[4].type);
        try std.testing.expectEqual(TokenType.RETURN, tokens.items[5].type);
        
        // Boolean and nothing literals
        try std.testing.expectEqual(TokenType.BOOL, tokens.items[6].type);
        try std.testing.expectEqual(true, tokens.items[6].literal.boolean);
        try std.testing.expectEqual(TokenType.BOOL, tokens.items[7].type);
        try std.testing.expectEqual(false, tokens.items[7].literal.boolean);
        try std.testing.expectEqual(TokenType.NOTHING, tokens.items[8].type);
        try std.testing.expectEqual(.nothing, tokens.items[8].literal);
        
        // Identifiers
        try std.testing.expectEqual(TokenType.IDENTIFIER, tokens.items[9].type);
        try std.testing.expectEqualStrings("myVar", tokens.items[9].literal.string);
        try std.testing.expectEqual(TokenType.IDENTIFIER, tokens.items[10].type);
        try std.testing.expectEqualStrings("_test", tokens.items[10].literal.string);
        try std.testing.expectEqual(TokenType.IDENTIFIER, tokens.items[11].type);
        try std.testing.expectEqualStrings("test123", tokens.items[11].literal.string);
        try std.testing.expectEqual(TokenType.IDENTIFIER, tokens.items[12].type);
        try std.testing.expectEqualStrings("UPPERCASE", tokens.items[12].literal.string);
    }
}

test {
    std.testing.refAllDecls(@This());
    std.debug.print("\nAll tests completed successfully!\n", .{});
}