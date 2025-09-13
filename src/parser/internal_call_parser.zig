const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");
const Precedence = @import("./precedence.zig").Precedence;
const expression_parser = @import("expression_parser.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const Parser = @import("parser_types.zig").Parser;

/// Parse a generic @method(...) call into a InternalCall expression.
/// Expect current token to be the method token (e.g., PUSH, POP, SLICE, ...)
pub fn internalCallExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const method_tok = self.peek();

    // Special handling for @print method with string interpolation
    if (method_tok.type == .PRINT) {
        return try parsePrintMethod(self);
    }

    // consume method token
    self.advance();
    // expect '('
    if (self.peek().type != .LEFT_PAREN) return error.ExpectedLeftParen;
    self.advance();

    // Allow newlines after '('
    while (self.peek().type == .NEWLINE) self.advance();

    var args = std.ArrayList(*ast.Expr).init(self.allocator);
    errdefer {
        for (args.items) |arg| {
            arg.deinit(self.allocator);
            self.allocator.destroy(arg);
        }
        args.deinit();
    }

    // Parse zero or more comma-separated expressions until ')'
    if (self.peek().type != .RIGHT_PAREN) {
        while (true) {
            // Allow leading newlines before each argument
            while (self.peek().type == .NEWLINE) self.advance();
            const expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            try args.append(expr);
            // Allow newlines before comma
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow newlines after comma
                while (self.peek().type == .NEWLINE) self.advance();
                // Allow trailing comma: if next is ')', stop parsing args
                if (self.peek().type == .RIGHT_PAREN) {
                    break;
                }
                continue;
            }
            break;
        }
    }

    // Allow trailing newlines before ')'
    while (self.peek().type == .NEWLINE) self.advance();
    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance();

    // For @methods that act on a receiver, we use first argument as receiver
    // and keep remaining as method arguments. If no args, receiver is a dummy.
    var receiver_expr: *ast.Expr = undefined;
    var call_args = std.ArrayList(*ast.Expr).init(self.allocator);
    errdefer call_args.deinit();

    if (args.items.len > 0) {
        receiver_expr = args.items[0];
        // move remaining args to call_args
        var i: usize = 1;
        while (i < args.items.len) : (i += 1) {
            try call_args.append(args.items[i]);
        }
    } else {
        // Create a nothing literal as placeholder receiver to keep AST consistent
        receiver_expr = try self.allocator.create(ast.Expr);
        receiver_expr.* = .{
            .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
            .data = .{ .Literal = .{ .nothing = {} } },
        };
    }

    const method_expr = try self.allocator.create(ast.Expr);
    method_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .InternalCall = .{
            .receiver = receiver_expr,
            .method = method_tok,
            .arguments = try call_args.toOwnedSlice(),
        } },
    };

    return method_expr;
}

/// Parse @print method with string interpolation support
/// Supports: @print("Hello {name}!", name) with escaped braces \{ \}
pub fn parsePrintMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance(); // consume PRINT_METHOD token

    // Expect opening parenthesis
    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    // Parse format string
    const format_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    // Expect closing parenthesis
    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    // Parse string interpolation to get format template and expressions
    const interp_info = try parseStringInterpolation(self, format_expr);

    // Use the placeholder expressions directly as arguments (for legacy compatibility)
    var arguments = std.ArrayList(*ast.Expr).init(self.allocator);
    errdefer {
        for (arguments.items) |arg| {
            arg.deinit(self.allocator);
            self.allocator.destroy(arg);
        }
        arguments.deinit();
    }

    for (interp_info.placeholder_expressions) |placeholder_expr| {
        // Use the placeholder expression directly - it's already parsed as an expression
        try arguments.append(placeholder_expr);
    }

    // Create placeholder indices (each placeholder maps to its argument index)
    const placeholder_indices = try self.allocator.alloc(u32, interp_info.placeholder_expressions.len);
    for (0..interp_info.placeholder_expressions.len) |idx| {
        placeholder_indices[idx] = @intCast(idx);
    }

    // Create the print expression with new FormatTemplate structure
    const print_expr = try self.allocator.create(ast.Expr);
    print_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(method_tok),
        },
        .data = .{
            .Print = .{
                .expr = null, // No simple expr for method calls
                .format_template = interp_info.format_template, // NEW: Use structured format template
                .format_parts = interp_info.format_parts, // Legacy compatibility
                .arguments = try arguments.toOwnedSlice(), // Legacy compatibility
                .placeholder_indices = placeholder_indices, // Legacy compatibility
            },
        },
    };

    return print_expr;
}

/// Parse string interpolation from format string
/// Returns a structured FormatTemplate with both strings and expressions
const InterpolationInfo = struct {
    format_template: *ast.FormatTemplate, // NEW: Structured format template
    // Legacy fields for backward compatibility during transition
    format_parts: []const []const u8, // String parts between placeholders
    placeholder_expressions: []*ast.Expr, // Expressions to interpolate
};

fn parseStringInterpolation(self: *Parser, format_expr: *ast.Expr) ErrorList!InterpolationInfo {
    // Extract string literal from format expression
    const format_string = switch (format_expr.data) {
        .Literal => |lit| switch (lit) {
            .string => |s| s,
            else => return error.ExpectedStringLiteral,
        },
        else => return error.ExpectedStringLiteral,
    };

    // Build both new FormatTemplate structure and legacy compatibility fields
    var template_parts = std.ArrayList(ast.FormatPart).init(self.allocator);
    var legacy_format_parts = std.ArrayList([]const u8).init(self.allocator);
    var placeholder_expressions = std.ArrayList(*ast.Expr).init(self.allocator);

    errdefer {
        // Clean up template parts
        for (template_parts.items) |*part| {
            part.deinit(self.allocator);
        }
        template_parts.deinit();

        // Clean up legacy parts
        for (legacy_format_parts.items) |part| {
            self.allocator.free(part);
        }
        legacy_format_parts.deinit();

        // Clean up expressions
        for (placeholder_expressions.items) |expr| {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
        }
        placeholder_expressions.deinit();
    }

    var i: usize = 0;
    var current_part_start: usize = 0;

    while (i < format_string.len) {
        if (format_string[i] == '\\' and i + 1 < format_string.len) {
            // Handle escaped characters
            if (format_string[i + 1] == '{' or format_string[i + 1] == '}') {
                i += 2; // Skip escaped brace
                continue;
            }
        } else if (format_string[i] == '{') {
            // Found start of placeholder
            // Add the string part before this placeholder
            const part = format_string[current_part_start..i];

            // Add to new template structure
            const string_part = try ast.createStringPart(self.allocator, part);
            try template_parts.append(string_part);

            // Add to legacy structure for backward compatibility
            try legacy_format_parts.append(try self.allocator.dupe(u8, part));

            // Find the end of the placeholder
            var j = i + 1;
            while (j < format_string.len and format_string[j] != '}') {
                j += 1;
            }

            if (j >= format_string.len) {
                return error.UnmatchedOpenBrace;
            }

            // Extract placeholder content and parse it as an expression
            const placeholder_content = format_string[i + 1 .. j];

            // Parse the placeholder content as an expression
            const placeholder_expr = try parsePlaceholderExpression(self, placeholder_content);

            // Add to new template structure
            const expr_part = ast.createExpressionPart(placeholder_expr);
            try template_parts.append(expr_part);

            // Add to legacy structure for backward compatibility
            try placeholder_expressions.append(placeholder_expr);

            i = j + 1; // Move past the '}'
            current_part_start = i;
        } else {
            i += 1;
        }
    }

    // Add the final string part
    const final_part = format_string[current_part_start..];

    // Add to new template structure
    const final_string_part = try ast.createStringPart(self.allocator, final_part);
    try template_parts.append(final_string_part);

    // Add to legacy structure
    try legacy_format_parts.append(try self.allocator.dupe(u8, final_part));

    // Create the FormatTemplate
    const format_template = try ast.createFormatTemplate(self.allocator, try template_parts.toOwnedSlice());

    return InterpolationInfo{
        .format_template = format_template,
        .format_parts = try legacy_format_parts.toOwnedSlice(),
        .placeholder_expressions = try placeholder_expressions.toOwnedSlice(),
    };
}

/// Parse a placeholder expression from string content
/// This handles cases like @string(number), @length(variable_name), etc.
fn parsePlaceholderExpression(self: *Parser, content: []const u8) ErrorList!*ast.Expr {
    // Create a temporary lexer for the placeholder content
    var temp_lexer = LexicalAnalyzer.init(self.allocator, content, self.current_file, self.reporter);
    defer temp_lexer.deinit();

    // Initialize keywords and lex the content into tokens
    try temp_lexer.initKeywords();
    const tokens = try temp_lexer.lexTokens();
    defer tokens.deinit();

    // Create a temporary parser for the tokens
    var temp_parser = Parser.init(self.allocator, tokens.items, self.current_file, self.reporter);
    defer temp_parser.deinit();

    // Parse the expression
    const expr = try expression_parser.parseExpression(&temp_parser) orelse {
        // If parsing fails, fall back to treating it as a variable name
        const var_token = token.Token{
            .type = .IDENTIFIER,
            .lexeme = content,
            .literal = .{ .nothing = {} },
            .line = 0,
            .column = 0,
            .file = self.current_file,
        };

        const var_expr = try self.allocator.create(ast.Expr);
        var_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(var_token),
            },
            .data = .{
                .Variable = var_token,
            },
        };
        return var_expr;
    };

    return expr;
}
