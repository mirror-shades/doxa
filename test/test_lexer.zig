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

// ── String literals: one scanner (plan/front-end-invariants.md Step 3) ──

const StringSegment = @import("../src/types/types.zig").StringSegment;

/// A literal's segments written back as source: text with every byte the
/// escape table decodes escaped again, placeholders in their braces.
fn encodeLiteral(allocator: std.mem.Allocator, segments: []const StringSegment) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    try out.append(allocator, '"');
    for (segments) |segment| switch (segment) {
        .text => |text| for (text) |byte| switch (byte) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '{' => try out.appendSlice(allocator, "\\{"),
            '}' => try out.appendSlice(allocator, "\\}"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            else => try out.append(allocator, byte),
        },
        .placeholder => |placeholder| {
            try out.append(allocator, '{');
            try out.appendSlice(allocator, placeholder.source);
            try out.append(allocator, '}');
        },
    };
    try out.append(allocator, '"');
    return out.items;
}

/// A string token's segments: a plain literal is one text segment.
fn segmentsOf(token: Token) []const StringSegment {
    return switch (token.literal) {
        .template => |segments| segments,
        .string => |text| &.{.{ .text = text }},
        else => unreachable,
    };
}

fn expectSameSegments(expected: []const StringSegment, actual: []const StringSegment) !void {
    // A plain literal's empty text and a literal with no segments agree.
    const want = if (expected.len == 1 and expected[0] == .text and expected[0].text.len == 0) expected[0..0] else expected;
    const got = if (actual.len == 1 and actual[0] == .text and actual[0].text.len == 0) actual[0..0] else actual;
    try testing.expectEqual(want.len, got.len);
    for (want, got) |a, b| {
        try testing.expectEqual(std.meta.activeTag(a), std.meta.activeTag(b));
        switch (a) {
            .text => |text| try testing.expectEqualStrings(text, b.text),
            .placeholder => |placeholder| try testing.expectEqualStrings(placeholder.source, b.placeholder.source),
        }
    }
}

/// Every string literal in `source`: its placeholders' positions index
/// their own source text, and its segments, written back as source and lexed
/// again, come back the same.
fn checkStringLiterals(allocator: std.mem.Allocator, path: []const u8, source: []const u8) !void {
    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var lexer = try LexicalAnalyzer.init(testing.io, allocator, source, path, &reporter);
    try lexer.initKeywords();
    // A program that does not lex is a test of its own error.
    const tokens = lexer.lexTokens() catch return;

    var line_starts: std.ArrayListUnmanaged(usize) = .empty;
    try line_starts.append(allocator, 0);
    for (source, 0..) |byte, i| if (byte == '\n') try line_starts.append(allocator, i + 1);

    for (tokens.items) |token| {
        if (token.type != .STRING) continue;
        const segments = segmentsOf(token);
        for (segments) |segment| switch (segment) {
            .text => {},
            .placeholder => |placeholder| {
                const at = line_starts.items[placeholder.line - 1] + placeholder.column - 1;
                testing.expectEqualStrings(placeholder.source, source[at..][0..placeholder.source.len]) catch |err| {
                    std.debug.print("{s}:{d}:{d}: a placeholder's position does not index its source\n", .{ path, placeholder.line, placeholder.column });
                    return err;
                };
            },
        };

        const encoded = try encodeLiteral(allocator, segments);
        var again = try LexicalAnalyzer.init(testing.io, allocator, encoded, path, &reporter);
        try again.initKeywords();
        const relexed = try again.lexTokens();
        try testing.expectEqual(@as(usize, 2), relexed.items.len);
        expectSameSegments(segments, segmentsOf(relexed.items[0])) catch |err| {
            std.debug.print("{s}:{d}:{d}: the literal does not round-trip through `{s}`\n", .{ path, token.line, token.column, encoded });
            return err;
        };
    }
}

test "lexer: every string literal's segments round-trip and index their source" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var files: usize = 0;
    for ([_][]const u8{ "test", "std" }) |root| {
        var dir = try std.Io.Dir.cwd().openDir(testing.io, root, .{ .iterate = true });
        defer dir.close(testing.io);
        var walker = try dir.walk(allocator);
        defer walker.deinit();
        while (try walker.next(testing.io)) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.basename, ".doxa")) continue;
            const source = try entry.dir.readFileAlloc(testing.io, entry.basename, allocator, .unlimited);
            const path = try std.fs.path.join(allocator, &.{ root, entry.path });
            try checkStringLiterals(allocator, path, source);
            files += 1;
        }
    }
    // The walk found the corpus.
    try testing.expect(files > 100);
}

test "lexer: a decoded or escaped brace is text, never a placeholder" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();

    var lexed = try lex(allocator, &reporter, "\"\\{a\\} \\u{7b}b\\u{7d} {c} \\r\"");
    defer lexed.deinit();
    const segments = segmentsOf(lexed.tokens.items[0]);
    try testing.expectEqual(@as(usize, 3), segments.len);
    try testing.expectEqualStrings("{a} {b} ", segments[0].text);
    try testing.expectEqualStrings("c", segments[1].placeholder.source);
    try testing.expectEqualStrings(" \r", segments[2].text);
}
