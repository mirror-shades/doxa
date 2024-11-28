const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const MemoryManager = @import("memory.zig").MemoryManager;
const Reporting = @import("reporting.zig").Reporting;
const ErrorList = @import("reporting.zig").ErrorList;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const token.Token,
    current: usize, // array index is usize by default
    debug_enabled: bool,

    pub fn init(memory: *MemoryManager, tokens: []const token.Token, debug_enabled: bool) !Parser {
        if (tokens.len == 0) return error.EmptyTokenList;
        return Parser{
            .allocator = memory.getAllocator(),
            .tokens = tokens,
            .current = 0,
            .debug_enabled = debug_enabled,
        };
    }

    pub fn deinit(_: *Parser) void {}

    fn peek(self: *Parser) token.Token {
        // Ensure we have tokens to look at
        if (self.tokens.len == 0) {
            return token.Token{
                .type = .EOF,
                .lexeme = "",
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            };
        }

        // If we're at or past the end, return the last token (which should be EOF)
        if (self.current >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }

        return self.tokens[self.current];
    }

    fn advance(self: *Parser) void {
        if (self.debug_enabled) {
            std.debug.print("Advancing from position {} to {}\n", .{
                self.current,
                self.current + 1,
            });
        }
        // Only advance if we're not at the end
        if (self.current < self.tokens.len - 1) {
            self.current += 1;
        }
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

    pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        while (true) {
            const current = self.peek();
            if (current.type == .EOF) break;

            const stmt = if (current.type == .VAR)
                try self.parseVarDecl()
            else if (current.type == .FN_KEYWORD or current.type == .FUNCTION_KEYWORD)
                try self.parseFunctionDecl()
            else if (current.type == .LEFT_BRACE)
                try self.parseBlock()
            else if (current.type == .RETURN)
                try self.parseReturnStmt()
            else
                try self.parseExpressionStmt();

            try statements.append(stmt);
        }

        return statements.toOwnedSlice();
    }

    fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nAttempting to parse var declaration...\n", .{});
        }

        // var
        const tok = self.peek();
        if (tok.type != .VAR) {
            if (self.debug_enabled) {
                std.debug.print("Expected VAR, got {s}\n", .{@tagName(tok.type)});
            }
            return error.UnexpectedToken;
        }
        if (self.debug_enabled) std.debug.print("Found VAR token\n", .{});
        self.advance();

        // identifier
        const id_tok = self.peek();
        if (id_tok.type != .IDENTIFIER) {
            if (self.debug_enabled) {
                std.debug.print("Expected IDENTIFIER, got {s}\n", .{@tagName(id_tok.type)});
            }
            return error.ExpectedIdentifier;
        }
        if (self.debug_enabled) std.debug.print("Found identifier: '{s}'\n", .{id_tok.lexeme});
        self.advance();

        // : type (optional)
        var type_expr: ?*ast.TypeExpr = null;
        if (self.peek().type == .COLON) {
            self.advance();
            type_expr = try self.parseTypeExpr();
        }

        // = (optional)
        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();
            initializer = try self.parseExpression();
        }

        // ;
        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected SEMICOLON, got {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedSemicolon;
        }
        if (self.debug_enabled) std.debug.print("Found semicolon\n", .{});
        self.advance();

        if (self.debug_enabled) {
            std.debug.print("\nSuccessfully parsed var declaration\n", .{});
        }

        return ast.Stmt{ .VarDecl = .{
            .name = id_tok,
            .type_expr = type_expr,
            .initializer = initializer,
        } };
    }

    fn parseExpressionStmt(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing expression statement...\n", .{});
            std.debug.print("Current token: {s} at position {}\n", .{
                @tagName(self.peek().type),
                self.current,
            });
        }

        if (self.peek().type == .RETURN) {
            return try self.parseReturnStmt();
        }

        const expr = try self.parseExpression();

        // If we didn't get an expression and we're at a semicolon, just skip it
        if (expr == null and self.peek().type == .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Skipping standalone semicolon\n", .{});
            }
            self.advance();
            return ast.Stmt{ .Expression = null };
        }

        // Expect semicolon unless the expression is a block or if-chain with all blocks
        if (expr) |e| {
            const needs_semicolon = switch (e.*) {
                .Block => false,
                .If => !hasAllBlockBranches(e),
                else => true,
            };

            if (self.debug_enabled) {
                std.debug.print("Expression needs semicolon: {}\n", .{needs_semicolon});
                std.debug.print("Next token: {s}\n", .{@tagName(self.peek().type)});
            }

            if (needs_semicolon) {
                if (self.peek().type != .SEMICOLON) {
                    if (self.debug_enabled) {
                        std.debug.print("Expected SEMICOLON, got {s}\n", .{@tagName(self.peek().type)});
                    }
                    return error.ExpectedSemicolon;
                }
                self.advance(); // consume the semicolon
            }
        }

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed expression statement\n", .{});
            std.debug.print("Next token: {s} at position {}\n", .{
                @tagName(self.peek().type),
                self.current,
            });
        }

        return ast.Stmt{ .Expression = expr };
    }

    fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {
        if (self.peek().type == .IF) {
            return try self.parseIfExpr();
        } else if (self.peek().type == .LEFT_BRACE) {
            // Convert block statement to expression
            const block_stmt = try self.parseBlock();
            const block_expr = try self.allocator.create(ast.Expr);
            block_expr.* = ast.Expr{ .Block = block_stmt.Block };
            return block_expr;
        }
        return try self.parseAssignment();
    }

    fn parseIfExpr(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing if expression...\n", .{});
        }

        self.advance(); // consume 'if'

        // Optional parentheses around condition
        const has_parens = self.peek().type == .LEFT_PAREN;
        if (has_parens) {
            self.advance();
        }

        const condition = (try self.parseExpression()) orelse return error.ExpectedExpression;

        if (has_parens) {
            if (self.peek().type != .RIGHT_PAREN) {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                return error.ExpectedRightParen;
            }
            self.advance();
        }

        if (self.peek().type != .THEN) {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            return error.ExpectedThen;
        }
        self.advance();

        const then_expr = (try self.parseExpression()) orelse {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            return error.ExpectedExpression;
        };

        if (self.peek().type != .ELSE) {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            then_expr.deinit(self.allocator);
            self.allocator.destroy(then_expr);
            return error.ExpectedElse;
        }
        self.advance();

        // Here's where we handle else-if chains
        var else_expr: *ast.Expr = undefined;
        if (self.peek().type == .IF) {
            // If we see an 'if' after 'else', recursively parse it as another if expression
            else_expr = (try self.parseIfExpr()) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                then_expr.deinit(self.allocator);
                self.allocator.destroy(then_expr);
                return error.ExpectedExpression;
            };
        } else {
            // Regular else branch
            const else_branch = (try self.parseExpression()) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                then_expr.deinit(self.allocator);
                self.allocator.destroy(then_expr);
                return error.ExpectedExpression;
            };
            else_expr = else_branch;
        }

        const if_expr = try self.allocator.create(ast.Expr);
        if_expr.* = ast.Expr{ .If = .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_expr,
        } };

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed if expression\n", .{});
        }

        return if_expr;
    }

    fn parseAssignment(self: *Parser) ErrorList!?*ast.Expr {
        const expr = (try self.parseComparison()) orelse return null;
        errdefer {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
        }

        if (self.check(.ASSIGN)) {
            self.advance(); // consume the equals sign
            const value = (try self.parseAssignment()) orelse {
                return error.InvalidAssignment;
            };
            errdefer {
                value.deinit(self.allocator);
                self.allocator.destroy(value);
            }

            // Handle array index assignment
            if (expr.* == .Index) {
                const index_expr = expr;
                const assign_expr = try self.allocator.create(ast.Expr);
                assign_expr.* = .{ .IndexAssign = .{
                    .array = index_expr.Index.array,
                    .index = index_expr.Index.index,
                    .value = value,
                } };
                return assign_expr;
            }

            // Handle regular variable assignment
            if (expr.* == .Variable) {
                const name = expr.Variable;
                const assign_expr = try self.allocator.create(ast.Expr);
                assign_expr.* = .{ .Assignment = .{
                    .name = name,
                    .value = value,
                } };
                return assign_expr;
            }

            value.deinit(self.allocator);
            self.allocator.destroy(value);
            return error.InvalidAssignmentTarget;
        }
        return expr;
    }

    fn parseComparison(self: *Parser) ErrorList!?*ast.Expr {
        var expr = (try self.parseTerm()) orelse return null;
        errdefer {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
        }

        while (true) {
            const current = self.peek();
            if (current.type != .GREATER and
                current.type != .GREATER_EQUAL and
                current.type != .LESS and
                current.type != .LESS_EQUAL and
                current.type != .EQUALITY and
                current.type != .BANG_EQUAL) break;

            const next = self.peek_next();
            if (next.type == .SEMICOLON or next.type == .EOF) break;

            self.advance();
            const right = (try self.parseTerm()) orelse {
                return error.ExpectedExpression;
            };
            errdefer {
                right.deinit(self.allocator);
                self.allocator.destroy(right);
            }

            const binary = try self.allocator.create(ast.Expr);
            binary.* = ast.Expr{ .Binary = .{
                .left = expr,
                .operator = current,
                .right = right,
            } };
            expr = binary;
        }

        return expr;
    }

    fn parseTerm(self: *Parser) ErrorList!?*ast.Expr {
        var expr = try self.parseFactor();
        errdefer if (expr) |e| {
            e.deinit(self.allocator);
            self.allocator.destroy(e);
        };

        while (true) {
            const current = self.peek();
            if (current.type != .PLUS and current.type != .MINUS) break;

            const operator = current;
            self.advance();

            const right = try self.parseFactor() orelse {
                return error.ExpectedExpression;
            };
            errdefer {
                right.deinit(self.allocator);
                self.allocator.destroy(right);
            }

            if (expr) |left| {
                const binary = try self.allocator.create(ast.Expr);
                errdefer self.allocator.destroy(binary);
                binary.* = ast.Expr{ .Binary = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                } };
                expr = binary;
            }
        }

        return expr;
    }

    fn parseFactor(self: *Parser) ErrorList!?*ast.Expr {
        var expr = try self.parseUnary();
        errdefer if (expr) |e| {
            e.deinit(self.allocator);
            self.allocator.destroy(e);
        };

        while (true) {
            const current = self.peek();
            if (current.type != .ASTERISK and current.type != .SLASH) break;

            const operator = current;
            self.advance();

            const right = try self.parseUnary() orelse {
                return error.ExpectedExpression;
            };
            errdefer {
                right.deinit(self.allocator);
                self.allocator.destroy(right);
            }

            if (expr) |left| {
                const binary = try self.allocator.create(ast.Expr);
                errdefer self.allocator.destroy(binary);
                binary.* = ast.Expr{ .Binary = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                } };
                expr = binary;
            }
        }

        return expr;
    }

    fn parseUnary(self: *Parser) ErrorList!?*ast.Expr {
        if (self.peek().type == .MINUS or self.peek().type == .BANG) {
            const operator = self.peek();
            self.advance();
            const right = (try self.parseUnary()) orelse {
                return error.ExpectedExpression;
            };

            const unary = try self.allocator.create(ast.Expr);
            unary.* = ast.Expr{ .Unary = .{
                .operator = operator,
                .right = right,
            } };

            if (self.debug_enabled) {
                std.debug.print("Found unary expression with operator: {s}\n", .{@tagName(operator.type)});
            }
            return unary;
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) std.debug.print("Parsing primary expression...\n", .{});

        const expr = try self.allocator.create(ast.Expr);
        errdefer self.allocator.destroy(expr);

        const tok = self.peek();
        switch (tok.type) {
            .INT, .FLOAT, .STRING, .BOOL => {
                expr.* = ast.Expr{ .Literal = tok.literal };
                if (self.debug_enabled) {
                    std.debug.print("Found literal: {any}\n", .{tok.literal});
                }
                self.advance();
                return expr;
            },
            .IDENTIFIER => {
                expr.* = ast.Expr{ .Variable = tok };
                if (self.debug_enabled) {
                    std.debug.print("Found variable reference: {s}\n", .{tok.lexeme});
                }
                self.advance();

                // Handle function calls
                if (self.peek().type == .LEFT_PAREN) {
                    self.advance(); // consume (
                    var args = std.ArrayList(*ast.Expr).init(self.allocator);
                    errdefer {
                        for (args.items) |arg| {
                            arg.deinit(self.allocator);
                            self.allocator.destroy(arg);
                        }
                        args.deinit();
                    }

                    while (self.peek().type != .RIGHT_PAREN) {
                        const arg = (try self.parseExpression()) orelse {
                            expr.deinit(self.allocator);
                            self.allocator.destroy(expr);
                            return error.ExpectedExpression;
                        };
                        try args.append(arg);

                        if (self.peek().type == .COMMA) {
                            self.advance();
                        } else if (self.peek().type != .RIGHT_PAREN) {
                            expr.deinit(self.allocator);
                            self.allocator.destroy(expr);
                            return error.ExpectedCommaOrParen;
                        }
                    }
                    self.advance(); // consume )

                    const call_expr = try self.allocator.create(ast.Expr);
                    errdefer self.allocator.destroy(call_expr);
                    call_expr.* = .{ .Call = .{
                        .callee = expr,
                        .arguments = try args.toOwnedSlice(),
                    } };
                    return call_expr;
                }

                // Handle array indexing
                if (self.peek().type == .LEFT_BRACKET) {
                    self.advance(); // consume [
                    const index = (try self.parseExpression()) orelse return error.ExpectedExpression;
                    errdefer {
                        index.deinit(self.allocator);
                        self.allocator.destroy(index);
                    }

                    if (self.peek().type != .RIGHT_BRACKET) {
                        index.deinit(self.allocator);
                        self.allocator.destroy(index);
                        return error.ExpectedRightBracket;
                    }
                    self.advance(); // consume ]

                    // If this is an assignment target
                    if (self.peek().type == .ASSIGN) {
                        self.advance(); // consume =
                        const value = (try self.parseExpression()) orelse {
                            index.deinit(self.allocator);
                            self.allocator.destroy(index);
                            return error.ExpectedExpression;
                        };
                        errdefer {
                            value.deinit(self.allocator);
                            self.allocator.destroy(value);
                        }

                        const index_expr = try self.allocator.create(ast.Expr);
                        index_expr.* = .{ .IndexAssign = .{
                            .array = expr,
                            .index = index,
                            .value = value,
                        } };
                        return index_expr;
                    }

                    const index_expr = try self.allocator.create(ast.Expr);
                    errdefer self.allocator.destroy(index_expr);
                    index_expr.* = .{ .Index = .{
                        .array = expr,
                        .index = index,
                    } };
                    return index_expr;
                }

                return expr;
            },
            .LEFT_PAREN => {
                self.advance();
                const subexpr = (try self.parseExpression()) orelse {
                    self.allocator.destroy(expr);
                    return error.ExpectedExpression;
                };
                if (self.peek().type != .RIGHT_PAREN) {
                    // Clean up allocated memory before returning error
                    subexpr.deinit(self.allocator);
                    self.allocator.destroy(subexpr);
                    self.allocator.destroy(expr);
                    return error.ExpectedRightParen;
                }
                self.advance();
                expr.* = ast.Expr{ .Grouping = subexpr };
                return expr;
            },
            .LEFT_BRACKET => {
                // Array literal
                self.advance();
                var elements = std.ArrayList(*ast.Expr).init(self.allocator);
                errdefer {
                    for (elements.items) |elem| {
                        elem.deinit(self.allocator);
                    }
                    elements.deinit();
                }

                while (self.peek().type != .RIGHT_BRACKET) {
                    const element = (try self.parseExpression()) orelse return error.ExpectedExpression;
                    try elements.append(element);

                    if (self.peek().type == .COMMA) {
                        self.advance();
                    } else if (self.peek().type != .RIGHT_BRACKET) {
                        return error.ExpectedCommaOrBracket;
                    }
                }
                self.advance(); // consume ]

                expr.* = ast.Expr{ .Array = try elements.toOwnedSlice() };
                return expr;
            },
            .LEFT_BRACE => {
                self.advance();
                var fields = std.ArrayList(*ast.StructLiteralField).init(self.allocator);
                errdefer {
                    for (fields.items) |field| {
                        field.deinit(self.allocator);
                        self.allocator.destroy(field);
                    }
                    fields.deinit();
                }

                while (self.peek().type != .RIGHT_BRACE) {
                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    const field_name = self.peek();
                    self.advance();

                    if (self.peek().type != .COLON) {
                        return error.ExpectedColon;
                    }
                    self.advance();

                    const field_value = (try self.parseExpression()) orelse {
                        return error.ExpectedExpression;
                    };

                    const new_field = try self.allocator.create(ast.StructLiteralField);
                    new_field.* = .{
                        .name = field_name,
                        .value = field_value,
                    };
                    try fields.append(new_field);

                    if (self.peek().type == .COMMA) {
                        self.advance();
                    } else if (self.peek().type != .RIGHT_BRACE) {
                        return error.ExpectedCommaOrBrace;
                    }
                }
                self.advance(); // consume }

                expr.* = ast.Expr{ .Struct = try fields.toOwnedSlice() };
                return expr;
            },
            else => {
                self.allocator.destroy(expr);
                return null;
            },
        }
    }

    fn peek_next(self: *Parser) token.Token {
        if (self.current + 1 >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[self.current + 1];
    }

    fn parseBlock(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing block...\n", .{});
        }

        // Consume {
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            var stmt = stmt: {
                if (self.peek().type == .VAR) {
                    break :stmt try self.parseVarDecl();
                } else if (self.peek().type == .LEFT_BRACE) {
                    break :stmt try self.parseBlock();
                } else if (self.peek().type == .RETURN) {
                    break :stmt try self.parseReturnStmt();
                } else {
                    break :stmt try self.parseExpressionStmt();
                }
            };
            errdefer stmt.deinit(self.allocator);
            try statements.append(stmt);
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        return ast.Stmt{ .Block = try statements.toOwnedSlice() };
    }

    fn parseTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing type expression...\n", .{});
            std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
        }

        const type_expr = try self.allocator.create(ast.TypeExpr);
        errdefer self.allocator.destroy(type_expr);

        switch (self.peek().type) {
            .IDENTIFIER => {
                const type_name = self.peek().lexeme;
                // Match built-in type names
                if (std.mem.eql(u8, type_name, "int")) {
                    type_expr.* = .{ .Basic = .Integer };
                } else if (std.mem.eql(u8, type_name, "float")) {
                    type_expr.* = .{ .Basic = .Float };
                } else if (std.mem.eql(u8, type_name, "string")) {
                    type_expr.* = .{ .Basic = .String };
                } else if (std.mem.eql(u8, type_name, "bool")) {
                    type_expr.* = .{ .Basic = .Boolean };
                } else {
                    // Handle custom types here in the future
                    self.allocator.destroy(type_expr);
                    return null;
                }
                self.advance();
            },
            .ARRAY => {
                self.advance();
                if (self.peek().type != .LEFT_BRACKET) {
                    return error.ExpectedLeftBracket;
                }
                self.advance();

                // Parse element type
                const elem_type = (try self.parseTypeExpr()) orelse {
                    return error.ExpectedExpression; // Using existing error
                };

                if (self.peek().type != .RIGHT_BRACKET) {
                    return error.ExpectedRightBracket;
                }
                self.advance();

                type_expr.* = .{ .Array = .{
                    .element_type = elem_type,
                } };
            },
            .STRUCT => {
                self.advance();
                if (self.peek().type != .LEFT_BRACE) {
                    return error.ExpectedLeftBrace;
                }
                self.advance();

                var fields = std.ArrayList(*ast.StructField).init(self.allocator);
                errdefer {
                    for (fields.items) |field| {
                        field.deinit(self.allocator);
                        self.allocator.destroy(field);
                    }
                    fields.deinit();
                }

                while (self.peek().type != .RIGHT_BRACE) {
                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    const field_name = self.peek();
                    self.advance();

                    if (self.peek().type != .COLON) {
                        return error.ExpectedColon;
                    }
                    self.advance();

                    const field_type = (try self.parseTypeExpr()) orelse {
                        return error.ExpectedExpression;
                    };

                    const new_field = try self.allocator.create(ast.StructField);
                    new_field.* = .{
                        .name = field_name,
                        .type_expr = field_type,
                    };
                    try fields.append(new_field);

                    if (self.peek().type == .COMMA) {
                        self.advance();
                    } else if (self.peek().type != .RIGHT_BRACE) {
                        return error.ExpectedCommaOrBrace;
                    }
                }
                self.advance(); // consume }

                type_expr.* = .{ .Struct = try fields.toOwnedSlice() };
            },
            .ENUM => {
                self.advance();
                if (self.peek().type != .LEFT_BRACE) {
                    return error.ExpectedLeftBrace;
                }
                self.advance();

                var variants = std.ArrayList([]const u8).init(self.allocator);
                errdefer variants.deinit();

                while (self.peek().type != .RIGHT_BRACE) {
                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    try variants.append(self.peek().lexeme);
                    self.advance();

                    if (self.peek().type == .COMMA) {
                        self.advance();
                    } else if (self.peek().type != .RIGHT_BRACE) {
                        return error.ExpectedCommaOrBrace;
                    }
                }
                self.advance(); // consume }

                type_expr.* = .{ .Enum = try variants.toOwnedSlice() };
            },
            else => {
                if (self.debug_enabled) {
                    std.debug.print("Unexpected token type for type expression: {s}\n", .{@tagName(self.peek().type)});
                }
                self.allocator.destroy(type_expr);
                return null;
            },
        }

        return type_expr;
    }

    fn check(self: *Parser, token_type: token.TokenType) bool {
        return self.peek().type == token_type;
    }

    fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing function declaration...\n", .{});
        }

        // Consume 'fn' or 'function'
        const current_token = self.peek();
        if (current_token.type != .FN_KEYWORD and current_token.type != .FUNCTION_KEYWORD) {
            return error.UnexpectedToken;
        }
        self.advance();

        // Parse function name
        const name_token = self.peek();
        if (name_token.type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        self.advance();

        // Parse parameters
        if (self.peek().type != .LEFT_PAREN) {
            return error.ExpectedLeftParen;
        }
        self.advance();

        var params = std.ArrayList(ast.FunctionParam).init(self.allocator);
        errdefer {
            for (params.items) |*param| {
                param.deinit(self.allocator);
            }
            params.deinit();
        }

        while (self.peek().type != .RIGHT_PAREN) {
            // Parameter name
            const param_name = self.peek();
            if (param_name.type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            self.advance();

            // Parameter type
            if (self.peek().type != .COLON) {
                return error.ExpectedColon;
            }
            self.advance();

            const param_type = (try self.parseTypeExpr()) orelse {
                return error.ExpectedType;
            };

            try params.append(.{
                .name = param_name,
                .type_expr = param_type,
            });

            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow trailing comma
                if (self.peek().type == .RIGHT_PAREN) {
                    break;
                }
            } else if (self.peek().type != .RIGHT_PAREN) {
                return error.ExpectedCommaOrParen;
            }
        }
        self.advance(); // consume )

        // Parse return type (optional)
        var return_type: ?*ast.TypeExpr = null;
        if (self.peek().type != .LEFT_BRACE) {
            if (self.peek().type == .COLON) {
                self.advance(); // Consume the colon
                return_type = try self.parseTypeExpr();
                if (return_type == null) {
                    return error.ExpectedType;
                }
            }
        }

        // Parse body
        const body_stmt = try self.parseBlock();
        const body = switch (body_stmt) {
            .Block => |b| b,
            else => unreachable,
        };

        return ast.Stmt{ .Function = .{
            .name = name_token,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
        } };
    }

    fn parseReturnStmt(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing return statement...\n", .{});
        }

        // Consume 'return'
        if (self.peek().type != .RETURN) {
            return error.UnexpectedToken;
        }
        self.advance();

        // Parse the return value (optional)
        var value: ?*ast.Expr = null;
        errdefer if (value) |v| {
            v.deinit(self.allocator);
            self.allocator.destroy(v);
        };

        if (self.peek().type != .SEMICOLON) {
            value = try self.parseExpression();
        }

        // Expect semicolon
        if (self.peek().type != .SEMICOLON) {
            if (value) |v| {
                v.deinit(self.allocator);
                self.allocator.destroy(v);
            }
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{ .Return = .{ .value = value } };
    }
};
