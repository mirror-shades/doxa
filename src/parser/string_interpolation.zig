const std = @import("std");
const ast = @import("../ast/ast.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const expression_parser = @import("expression_parser.zig");

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const Parser = @import("parser_types.zig").Parser;

pub fn buildStringLiteralExpr(
    self: *Parser,
    content: []const u8,
    span: ast.SourceSpan,
    interpolated: bool,
) ErrorList!*ast.Expr {
    if (interpolated and std.mem.indexOfScalar(u8, content, '{') != null) {
        const template = try parseFormatTemplate(self, content, span);
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

fn parseFormatTemplate(self: *Parser, format_string: []const u8, span: ast.SourceSpan) ErrorList!*ast.FormatTemplate {
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

            var j = i + 1;
            var depth: usize = 1;
            while (j < format_string.len and depth > 0) {
                switch (format_string[j]) {
                    '{' => {
                        depth += 1;
                        j += 1;
                    },
                    '}' => {
                        depth -= 1;
                        if (depth > 0) j += 1;
                    },
                    '"' => {
                        j += 1;
                        while (j < format_string.len and format_string[j] != '"') {
                            if (format_string[j] == '\\') {
                                j += 2;
                            } else {
                                j += 1;
                            }
                        }
                        if (j < format_string.len) j += 1;
                    },
                    else => j += 1,
                }
            }

            if (depth > 0) {
                return error.UnmatchedOpenBrace;
            }

            const placeholder_content = format_string[i + 1 .. j];
            const placeholder_expr = try parsePlaceholderExpression(self, placeholder_content, span);
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

fn parsePlaceholderExpression(self: *Parser, content: []const u8, outer_span: ast.SourceSpan) ErrorList!*ast.Expr {
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

    var temp_parser = Parser.init(self.io, self.allocator, tokens.items, self.current_file, self.current_file_uri, self.reporter, self.graph);
    defer temp_parser.deinit();
    temp_parser.owner_record = self.owner_record;

    const expr = try expression_parser.parseExpression(&temp_parser) orelse {
        self.reporter.reportCompileError(
            outer_span.location,
            Errors.ErrorCode.INVALID_PLACEHOLDER_EXPRESSION,
            "invalid interpolation placeholder '{s}'; built-in operations require '@' (e.g. `{{@string(value)}}`)",
            .{content},
        );
        return error.InvalidExpression;
    };

    expr.base.span = outer_span;
    return expr;
}
