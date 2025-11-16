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

pub fn internalCallExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const method_tok = self.peek();

    if (method_tok.type == .PRINT) {
        return try parsePrintMethod(self);
    }

    if (method_tok.type == .EXIT) {
        return try parseExitMethod(self);
    }

    if (method_tok.type == .SLEEP) {
        return try parseSleepMethod(self);
    }

    if (method_tok.type == .RANDOM) {
        return try parseRandomMethod(self);
    }

    if (method_tok.type == .TIME) {
        return try parseTimeMethod(self);
    }

    if (method_tok.type == .TICK) {
        return try parseTickMethod(self);
    }

    if (method_tok.type == .BUILD) {
        return try parseBuildMethod(self);
    }

    self.advance();
    if (self.peek().type != .LEFT_PAREN) return error.ExpectedLeftParen;
    self.advance();

    while (self.peek().type == .NEWLINE) self.advance();

    var args = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer {
        for (args.items) |arg| {
            arg.deinit(self.allocator);
            self.allocator.destroy(arg);
        }
        args.deinit();
    }

    if (self.peek().type != .RIGHT_PAREN) {
        while (true) {
            while (self.peek().type == .NEWLINE) self.advance();
            const expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            try args.append(expr);
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .COMMA) {
                self.advance();
                while (self.peek().type == .NEWLINE) self.advance();
                if (self.peek().type == .RIGHT_PAREN) {
                    break;
                }
                continue;
            }
            break;
        }
    }

    while (self.peek().type == .NEWLINE) self.advance();
    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance();

    var receiver_expr: *ast.Expr = undefined;
    var call_args = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer call_args.deinit();

    if (args.items.len > 0) {
        receiver_expr = args.items[0];
        var i: usize = 1;
        while (i < args.items.len) : (i += 1) {
            try call_args.append(args.items[i]);
        }
    } else {
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

pub fn parsePrintMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const format_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const interp_info = try parseStringInterpolation(self, format_expr);

    var arguments = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer {
        for (arguments.items) |arg| {
            arg.deinit(self.allocator);
            self.allocator.destroy(arg);
        }
        arguments.deinit();
    }

    for (interp_info.placeholder_expressions) |placeholder_expr| {
        try arguments.append(placeholder_expr);
    }

    const placeholder_indices = try self.allocator.alloc(u32, interp_info.placeholder_expressions.len);
    for (0..interp_info.placeholder_expressions.len) |idx| {
        placeholder_indices[idx] = @intCast(idx);
    }

    const print_expr = try self.allocator.create(ast.Expr);
    print_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(method_tok),
        },
        .data = .{
            .Print = .{
                .expr = null,
                .format_template = interp_info.format_template,
                .format_parts = interp_info.format_parts,
                .arguments = try arguments.toOwnedSlice(),
                .placeholder_indices = placeholder_indices,
            },
        },
    };

    return print_expr;
}

const InterpolationInfo = struct {
    format_template: *ast.FormatTemplate,
    format_parts: []const []const u8,
    placeholder_expressions: []*ast.Expr,
};

fn parseStringInterpolation(self: *Parser, format_expr: *ast.Expr) ErrorList!InterpolationInfo {
    const format_string = switch (format_expr.data) {
        .Literal => |lit| switch (lit) {
            .string => |s| s,
            else => return error.ExpectedStringLiteral,
        },
        else => return error.ExpectedStringLiteral,
    };

    var template_parts = std.array_list.Managed(ast.FormatPart).init(self.allocator);
    var legacy_format_parts = std.array_list.Managed([]const u8).init(self.allocator);
    var placeholder_expressions = std.array_list.Managed(*ast.Expr).init(self.allocator);

    errdefer {
        for (template_parts.items) |*part| {
            part.deinit(self.allocator);
        }
        template_parts.deinit();

        for (legacy_format_parts.items) |part| {
            self.allocator.free(part);
        }
        legacy_format_parts.deinit();

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
            if (format_string[i + 1] == '{' or format_string[i + 1] == '}') {
                i += 2;
                continue;
            }
        } else if (format_string[i] == '{') {
            const part = format_string[current_part_start..i];
            const string_part = try ast.createStringPart(self.allocator, part);
            try template_parts.append(string_part);

            try legacy_format_parts.append(try self.allocator.dupe(u8, part));

            var j = i + 1;
            while (j < format_string.len and format_string[j] != '}') {
                j += 1;
            }

            if (j >= format_string.len) {
                return error.UnmatchedOpenBrace;
            }

            const placeholder_content = format_string[i + 1 .. j];

            const placeholder_expr = try parsePlaceholderExpression(self, placeholder_content);

            const expr_part = ast.createExpressionPart(placeholder_expr);
            try template_parts.append(expr_part);

            try placeholder_expressions.append(placeholder_expr);

            i = j + 1;
            current_part_start = i;
        } else {
            i += 1;
        }
    }

    const final_part = format_string[current_part_start..];
    const final_string_part = try ast.createStringPart(self.allocator, final_part);
    try template_parts.append(final_string_part);

    try legacy_format_parts.append(try self.allocator.dupe(u8, final_part));

    const format_template = try ast.createFormatTemplate(self.allocator, try template_parts.toOwnedSlice());

    return InterpolationInfo{
        .format_template = format_template,
        .format_parts = try legacy_format_parts.toOwnedSlice(),
        .placeholder_expressions = try placeholder_expressions.toOwnedSlice(),
    };
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
            .data = .{
                .Variable = var_token,
            },
        };
        return var_expr;
    };

    return expr;
}

pub fn parseExitMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const exit_code_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const arguments = try self.allocator.alloc(*ast.Expr, 1);
    arguments[0] = exit_code_expr;

    const exit_expr = try self.allocator.create(ast.Expr);
    exit_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = arguments,
        } },
    };

    return exit_expr;
}

pub fn parseSleepMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const duration_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const arguments = try self.allocator.alloc(*ast.Expr, 1);
    arguments[0] = duration_expr;

    const sleep_expr = try self.allocator.create(ast.Expr);
    sleep_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = arguments,
        } },
    };

    return sleep_expr;
}

pub fn parseRandomMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const random_expr = try self.allocator.create(ast.Expr);
    random_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = &[_]*ast.Expr{},
        } },
    };

    return random_expr;
}

pub fn parseTimeMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const time_expr = try self.allocator.create(ast.Expr);
    time_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = &[_]*ast.Expr{},
        } },
    };

    return time_expr;
}

pub fn parseTickMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    const tick_expr = try self.allocator.create(ast.Expr);
    tick_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = &[_]*ast.Expr{},
        } },
    };

    return tick_expr;
}

pub fn parseBuildMethod(self: *Parser) ErrorList!?*ast.Expr {
    const method_tok = self.peek();
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    // Require exactly 5 args: src, out, arch, os, debug
    const src_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type == .RIGHT_PAREN) return error.TooFewArguments;
    if (self.peek().type != .COMMA) return error.ExpectedComma;
    self.advance();
    const out_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type == .RIGHT_PAREN) return error.TooFewArguments;
    if (self.peek().type != .COMMA) return error.ExpectedComma;
    self.advance();
    const arch_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type == .RIGHT_PAREN) return error.TooFewArguments;
    if (self.peek().type != .COMMA) return error.ExpectedComma;
    self.advance();
    const os_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type == .RIGHT_PAREN) return error.TooFewArguments;
    if (self.peek().type != .COMMA) return error.ExpectedComma;
    self.advance();
    const debug_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance();

    const arguments = try self.allocator.alloc(*ast.Expr, 5);
    arguments[0] = src_expr;
    arguments[1] = out_expr;
    arguments[2] = arch_expr;
    arguments[3] = os_expr;
    arguments[4] = debug_expr;

    const build_expr = try self.allocator.create(ast.Expr);
    build_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(method_tok) },
        .data = .{ .BuiltinCall = .{
            .function = method_tok,
            .arguments = arguments,
        } },
    };

    return build_expr;
}
