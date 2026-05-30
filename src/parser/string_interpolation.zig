const std = @import("std");
const token = @import("../types/token.zig");
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
        const template = try parseFormatTemplate(self, content);
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

fn parseFormatTemplate(self: *Parser, format_string: []const u8) ErrorList!*ast.FormatTemplate {
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
            const placeholder_expr = try parsePlaceholderExpression(self, placeholder_content);
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

fn parsePlaceholderExpression(self: *Parser, content: []const u8) ErrorList!*ast.Expr {
    var temp_lexer = try LexicalAnalyzer.init(self.allocator, content, self.current_file, self.reporter);
    defer temp_lexer.deinit();

    try temp_lexer.initKeywords();
    const tokens = try temp_lexer.lexTokens();
    defer tokens.deinit();

    var temp_parser = Parser.init(self.allocator, tokens.items, self.current_file, self.current_file_uri, self.reporter);
    defer temp_parser.deinit();

    const expr = try expression_parser.parseExpression(&temp_parser) orelse {
        const var_token = token.Token{
            .type = .IDENTIFIER,
            .lexeme = content,
            .literal = .{ .nothing = {} },
            .line = 0,
            .column = 0,
            .file = self.current_file,
            .file_uri = self.current_file_uri,
        };

        const var_expr = try self.allocator.create(ast.Expr);
        var_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(var_token),
            },
            .data = .{ .Variable = var_token },
        };
        return var_expr;
    };

    return expr;
}
