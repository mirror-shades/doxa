const std = @import("std");
const testing = std.testing;

const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Reporting = @import("../src/utils/reporting.zig");
const TokenImport = @import("../src/types/token.zig");
const Token = TokenImport.Token;
const TokenType = TokenImport.TokenType;

/// Keeps the lexer alive for as long as its tokens are read: `LexicalAnalyzer`
/// owns the token list, so the two have to be torn down together.
const Lexed = struct {
    lexer: LexicalAnalyzer,
    tokens: std.array_list.Managed(Token),

    fn deinit(self: *Lexed) void {
        self.lexer.deinit();
    }
};

fn lex(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, source: []const u8) !Lexed {
    var lexer = try LexicalAnalyzer.init(testing.io, allocator, source, "test/lexer_int_literals.doxa", reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    return .{ .lexer = lexer, .tokens = tokens };
}

fn expectInt(token: Token, expected: i64) !void {
    try testing.expectEqual(TokenType.INT, token.type);
    try testing.expectEqual(expected, token.literal.int);
}

test "lexer: int literals use the full 64-bit range" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexed = try lex(allocator, &reporter, "2147483647");
    defer lexed.deinit();

    try testing.expectEqual(@as(usize, 2), lexed.tokens.items.len);
    try expectInt(lexed.tokens.items[0], 2147483647);
    try testing.expectEqual(TokenType.EOF, lexed.tokens.items[1].type);
}

test "lexer: int literal above i32 max does not overflow" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexed = try lex(allocator, &reporter, "2147483648");
    defer lexed.deinit();

    try testing.expectEqual(@as(usize, 2), lexed.tokens.items.len);
    try expectInt(lexed.tokens.items[0], 2147483648);
    try testing.expectEqual(TokenType.EOF, lexed.tokens.items[1].type);
}

test "lexer: -2147483648 lexes as unary minus over the magnitude" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexed = try lex(allocator, &reporter, "-2147483648");
    defer lexed.deinit();

    try testing.expectEqual(@as(usize, 3), lexed.tokens.items.len);
    try testing.expectEqual(TokenType.MINUS, lexed.tokens.items[0].type);
    try expectInt(lexed.tokens.items[1], 2147483648);
    try testing.expectEqual(TokenType.EOF, lexed.tokens.items[2].type);
}

test "lexer: int literal at i64 max lexes" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexed = try lex(allocator, &reporter, "9223372036854775807");
    defer lexed.deinit();

    try testing.expectEqual(@as(usize, 2), lexed.tokens.items.len);
    try expectInt(lexed.tokens.items[0], std.math.maxInt(i64));
    try testing.expectEqual(TokenType.EOF, lexed.tokens.items[1].type);
}

test "lexer: int literal above i64 max reports a compile error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexer = try LexicalAnalyzer.init(testing.io, allocator, "9223372036854775808", "test/lexer_int_literals.doxa", &reporter);
    defer lexer.deinit();
    try lexer.initKeywords();

    try testing.expectError(error.IntegerOverflow, lexer.lexTokens());
    try testing.expect(reporter.hasCompileErrors());
}
