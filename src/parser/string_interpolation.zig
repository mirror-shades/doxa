const std = @import("std");
const ast = @import("../ast/ast.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Token = @import("../types/token.zig").Token;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

/// `content` is `literal`'s decoded text.
pub fn buildStringLiteralExpr(
    self: *Parser,
    content: []const u8,
    literal: Token,
    interpolated: bool,
) ErrorList!*ast.Expr {
    const span = ast.SourceSpan.fromToken(literal);
    if (interpolated and std.mem.indexOfScalar(u8, content, '{') != null) {
        const template = try parseFormatTemplate(self, content, literal);
        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = span,
            },
            .data = .{ .InterpolatedString = template },
        };
        return expr;
    }

    const string_copy = try self.allocator.dupe(u8, content);
    const expr = try self.allocator.create(ast.Expr);
    expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = span,
        },
        .data = .{ .Literal = .{ .string = string_copy } },
    };
    return expr;
}

fn parseFormatTemplate(self: *Parser, format_string: []const u8, literal: Token) ErrorList!*ast.FormatTemplate {
    const span = ast.SourceSpan.fromToken(literal);
    var source = SourceCursor.init(literal);
    var template_parts = std.array_list.Managed(ast.FormatPart).init(self.allocator);
    errdefer {
        for (template_parts.items) |*part| {
            part.deinit(self.allocator);
        }
        template_parts.deinit();
    }

    var i: usize = 0;
    var current_part_start: usize = 0;

    while (i < format_string.len) {
        if (format_string[i] == '{') {
            const part = format_string[current_part_start..i];
            try template_parts.append(try ast.createStringPart(self.allocator, part));

            const j = placeholderEnd(format_string, i) orelse return error.UnmatchedOpenBrace;
            // Placeholder text is not escape-decoded, so it reads the same in
            // the source: where it starts there is where its tokens are.
            const origin = source.nextPlaceholder() orelse return error.UnmatchedOpenBrace;

            const placeholder_content = format_string[i + 1 .. j];
            const placeholder_expr = try parsePlaceholderExpression(self, placeholder_content, span, origin);
            try template_parts.append(ast.createExpressionPart(placeholder_expr));

            i = j + 1;
            current_part_start = i;
        } else {
            i += 1;
        }
    }

    const final_part = format_string[current_part_start..];
    try template_parts.append(try ast.createStringPart(self.allocator, final_part));

    return try ast.createFormatTemplate(self.allocator, try template_parts.toOwnedSlice());
}

/// The index of the `}` closing the placeholder whose `{` is at `open`.
fn placeholderEnd(text: []const u8, open: usize) ?usize {
    var j = open + 1;
    var depth: usize = 1;
    while (j < text.len) {
        switch (text[j]) {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if (depth == 0) return j;
            },
            '"' => {
                j += 1;
                while (j < text.len and text[j] != '"') j += if (text[j] == '\\') 2 else 1;
            },
            else => {},
        }
        j += 1;
    }
    return null;
}

/// A line and column in the source.
const Position = struct { line: usize, column: usize };

/// Walks a string literal's source text (its lexeme, quotes and escapes
/// included) to find where each placeholder's text begins.
const SourceCursor = struct {
    text: []const u8,
    index: usize,
    at: Position,

    fn init(literal: Token) SourceCursor {
        return .{ .text = literal.lexeme, .index = 0, .at = .{ .line = literal.line, .column = literal.column } };
    }

    /// The position just inside the next placeholder's `{`, past its `}`
    /// afterwards; null when the source has no further placeholder.
    fn nextPlaceholder(self: *SourceCursor) ?Position {
        while (self.index < self.text.len) {
            switch (self.text[self.index]) {
                '\\' => {
                    const unicode = self.index + 1 < self.text.len and self.text[self.index + 1] == 'u';
                    self.step(2);
                    if (unicode) {
                        while (self.index < self.text.len and self.text[self.index] != '}') self.step(1);
                        self.step(1);
                    }
                },
                '{' => {
                    const end = placeholderEnd(self.text, self.index) orelse return null;
                    self.step(1);
                    const start = self.at;
                    self.step(end + 1 - self.index);
                    return start;
                },
                else => self.step(1),
            }
        }
        return null;
    }

    fn step(self: *SourceCursor, count: usize) void {
        for (0..count) |_| {
            if (self.index >= self.text.len) return;
            if (self.text[self.index] == '\n') {
                self.at = .{ .line = self.at.line + 1, .column = 1 };
            } else {
                self.at.column += 1;
            }
            self.index += 1;
        }
    }
};

fn parsePlaceholderExpression(self: *Parser, content: []const u8, outer_span: ast.SourceSpan, origin: Position) ErrorList!*ast.Expr {
    // The placeholder content is a slice into the enclosing string literal's
    // buffer, which may be owned by a lexer whose lifetime ends before the AST
    // is consumed. Duplicate it so every token lexeme produced below (and the
    // bare-variable fallback) points into parser-owned memory that outlives the
    // whole compile.
    const owned_content = try self.allocator.dupe(u8, content);

    var temp_lexer = try LexicalAnalyzer.init(self.io, self.allocator, owned_content, self.current_file, self.reporter);
    defer {
        // Keep string literals nested inside the placeholder alive (e.g. a
        // `{"hi"}` argument): the parsed AST borrows them, so they must outlive
        // this lexer just like the outer string's buffers do.
        temp_lexer.takeOwnershipOfStrings();
        temp_lexer.deinit();
    }

    try temp_lexer.initKeywords();
    const tokens = try temp_lexer.lexTokens();
    defer tokens.deinit();
    // The lexer counts from the placeholder's own start: move every token to
    // where its text is in the file.
    for (tokens.items) |*tok| {
        if (tok.line == 1) tok.column += origin.column - 1;
        tok.line += origin.line - 1;
    }

    var temp_parser = Parser.init(self.io, self.allocator, tokens.items, self.current_file, self.current_file_uri, self.reporter);

    const expr = try expression_parser.parseExpression(&temp_parser) orelse {
        self.reporter.reportCompileError(
            outer_span.location,
            Errors.ErrorCode.INVALID_PLACEHOLDER_EXPRESSION,
            "invalid interpolation placeholder '{s}'; built-in operations require '@' (e.g. `{{@string(value)}}`)",
            .{content},
        );
        return error.InvalidExpression;
    };

    return expr;
}
