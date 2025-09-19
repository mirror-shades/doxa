const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const parser_types = @import("parser_types.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const Parser = @import("parser_types.zig").Parser;
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("statement_parser.zig");
const expression_parser = @import("expression_parser.zig");
const declaration_parser = @import("declaration_parser.zig");

pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
    return parser_types.execute(self);
}

fn isAlternateToken(token_type: token.TokenType) bool {
    return switch (token_type) {
        .ASSIGN_SYMBOL, .ASSIGN_KEYWORD, .EQUALITY_SYMBOL, .EQUALITY_KEYWORD, .AND_SYMBOL, .AND_KEYWORD, .OR_SYMBOL, .OR_KEYWORD, .WHERE_SYMBOL, .WHERE_KEYWORD, .FN_KEYWORD, .FUNCTION_KEYWORD => true,
        else => false,
    };
}

fn hasAllBlockBranches(e: *ast.Expr) bool {
    return switch (e.*) {
        .If => |if_expr| switch (if_expr.then_branch.?.*) {
            .Block => switch (if_expr.else_branch.?.*) {
                .Block => true,
                .If => hasAllBlockBranches(if_expr.else_branch.?),
                else => false,
            },
            else => false,
        },
        else => false,
    };
}

fn infix_call(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
    self.advance();
    return self.call(left, precedence);
}

fn array(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // First, consume the LEFT_BRACKET token!

    var elements = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer elements.deinit();

    // Parse first element to establish the type
    if (self.peek().type != .RIGHT_BRACKET) {
        const first_element = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
        try elements.append(first_element);

        // Get the type of the first element
        const first_type = switch (first_element.*) {
            .Literal => |lit| @as(token.TokenType, switch (lit) {
                .int => .INT_TYPE,
                .float => .FLOAT_TYPE,
                .string => .STRING_TYPE,
                .tetra => .TETRA_TYPE,
                .nothing => .NOTHING,
                .array => .ARRAY_TYPE,
                .struct_value => .STRUCT_TYPE,
                .function => .FUNCTION_KEYWORD,
                .enum_variant => .ENUM_TYPE,
                .map => .MAP_TYPE,
            }),
            else => .IDENTIFIER, // Handle other expression types
        };

        // Parse remaining elements
        while (self.peek().type == .COMMA) {
            self.advance(); // consume comma
            if (self.peek().type == .RIGHT_BRACKET) break;

            const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            // Check if the new element matches the type of the first element
            const element_type = switch (element.*) {
                .Literal => |lit| @as(token.TokenType, switch (lit) {
                    .int => .INT_TYPE,
                    .float => .FLOAT_TYPE,
                    .string => .STRING_TYPE,
                    .tetra => .TETRA_TYPE,
                    .nothing => .NOTHING,
                    .array => .ARRAY_TYPE,
                    .struct_value => .STRUCT_TYPE,
                    .function => .FUNCTION_KEYWORD,
                    .enum_variant => .ENUM_TYPE,
                    .map => .MAP_TYPE,
                }),
                else => .IDENTIFIER,
            };

            if (element_type != first_type) {
                // Clean up the element we just created
                element.deinit(self.allocator);
                self.allocator.destroy(element);
                return error.HeterogeneousArray;
            }

            try elements.append(element);
        }
    }

    if (self.peek().type != .RIGHT_BRACKET) {
        return error.ExpectedRightBracket;
    }
    self.advance(); // consume ']'

    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{ .Array = try elements.toOwnedSlice() };
    return array_expr;
}

fn parseForEach(self: *Parser) ErrorList!*ast.Expr {
    self.advance(); // consume 'foreach'

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    // Parse item name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    _ = self.peek();
    self.advance();

    // Parse 'in' keyword
    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    // Parse array expression
    _ = try expression_parser.parseExpression(self);

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    // Parse body as a block
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    _ = try self.parseBlockStmt();

    // Desugar to Loop handled in statement_parser (keep here for compatibility if needed)
    const foreach_expr = try self.allocator.create(ast.Expr);
    foreach_expr.* = .{ .DefaultArgPlaceholder = {} };
    return foreach_expr;
}

fn parseReturn(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'return'

    var value: ?*ast.Expr = null;
    const type_info = ast.TypeInfo{ .base = .Nothing };

    if (self.peek().type != .NEWLINE) {
        value = try expression_parser.parseExpression(self);
        return error.ExpectedNewline;
    }
    self.advance();

    return ast.Stmt{ .Return = .{
        .value = value,
        .type_info = type_info,
    } };
}

fn map(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {

    // Consume the left brace
    self.advance();

    var entries = std.array_list.Managed(ast.MapEntry).init(self.allocator); // Changed from *ast.MapEntry
    errdefer {
        for (entries.items) |*entry| { // Changed to use pointer to entry
            entry.key.deinit(self.allocator);
            self.allocator.destroy(entry.key);
            entry.value.deinit(self.allocator);
            self.allocator.destroy(entry.value);
        }
        entries.deinit();
    }

    // Parse entries until we hit a right brace
    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        // Parse key
        const key = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Expect :
        if (self.peek().type != .WHERE_SYMBOL) {
            // If we don't see a :, this might be a block instead
            if (entries.items.len == 0) {
                // Clean up the key we just parsed
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                // Fall back to block parsing
                return self.block(null, .NONE);
            }
            return error.ExpectedColon;
        }
        self.advance(); // consume colon

        // Parse value
        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Create and append entry directly
        try entries.append(.{
            .key = key,
            .value = value,
        });

        // Handle comma
        if (self.peek().type == .COMMA) {
            self.advance();
            // Allow trailing comma
            if (self.peek().type == .RIGHT_BRACE) break;
        } else if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedCommaOrBrace;
        }
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance();

    // Map literals are now statements, not expressions
    // This function should not be called for map literals anymore
    return error.MapLiteralsMustBeStatements;
}
