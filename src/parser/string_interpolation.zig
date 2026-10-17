const std = @import("std");
const ast = @import("../ast/ast.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Token = @import("../types/token.zig").Token;
const StringSegment = @import("../types/types.zig").StringSegment;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

/// A `STRING` token as an expression: a literal, or, when the lexer split it
/// into segments, an interpolation of them. The segments are final: their
/// text is decoded, and each placeholder's source is parsed as it stands.
pub fn buildStringLiteralExpr(self: *Parser, literal: Token) ErrorList!*ast.Expr {
    const expr = try self.allocator.create(ast.Expr);
    expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(literal) },
        .data = switch (literal.literal) {
            .string => |text| .{ .Literal = .{ .string = try self.allocator.dupe(u8, text) } },
            .template => |segments| .{ .InterpolatedString = try template(self, segments, ast.SourceSpan.fromToken(literal)) },
            else => unreachable, // the lexer gives a string token one of these
        },
    };
    return expr;
}

fn template(self: *Parser, segments: []const StringSegment, span: ast.SourceSpan) ErrorList!*ast.FormatTemplate {
    var parts = std.array_list.Managed(ast.FormatPart).init(self.allocator);
    errdefer {
        for (parts.items) |*part| part.deinit(self.allocator);
        parts.deinit();
    }
    for (segments) |segment| switch (segment) {
        .text => |text| try parts.append(try ast.createStringPart(self.allocator, text)),
        .placeholder => |placeholder| try parts.append(ast.createExpressionPart(try parsePlaceholderExpression(self, placeholder, span))),
    };
    return try ast.createFormatTemplate(self.allocator, try parts.toOwnedSlice());
}

fn parsePlaceholderExpression(self: *Parser, placeholder: StringSegment.Placeholder, outer_span: ast.SourceSpan) ErrorList!*ast.Expr {
    const content = placeholder.source;
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
        if (tok.line == 1) tok.column += placeholder.column - 1;
        tok.line += placeholder.line - 1;
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
    // A placeholder holds one expression: `{x y}` is an error, never `{x}`.
    try temp_parser.expectEnd("the interpolated expression", &.{.EOF});

    return expr;
}
