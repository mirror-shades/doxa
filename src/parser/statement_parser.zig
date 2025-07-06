const std = @import("std");
const ast = @import("../ast/ast.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("./expression_parser.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const Reporter = Reporting.Reporter;
const token = @import("../lexer/token.zig");
const declaration_parser = @import("./declaration_parser.zig");
const expression_parser = @import("./expression_parser.zig");
const HIRType = @import("../codegen/soxa_types.zig").HIRType;

pub fn parse(self: *Parser, reporter: *Reporter) ErrorList![]ast.Stmt {
    reporter.debug("Token stream:", .{});
    for (self.tokens, 0..) |t, i| {
        reporter.debug("{}: {s} ({s})\n", .{ i, @tagName(t.type), t.lexeme });
    }
    reporter.debug("", .{});

    // First pass: collect all function declarations and create them
    var function_table = std.StringHashMap(*ast.Stmt).init(self.allocator);
    defer function_table.deinit();

    // Store current position
    const original_pos = self.current;

    // First pass to find and store all function declarations
    while (self.peek().type != .EOF) {
        if (self.peek().type == .FUNCTION) {
            // Don't advance here - let parseFunctionDecl handle it
            const fn_stmt = try declaration_parser.parseFunctionDecl(self);
            const fn_ptr = try self.allocator.create(ast.Stmt);
            fn_ptr.* = fn_stmt;

            // Get the function name from the statement
            if (fn_stmt == .Function) {
                try function_table.put(fn_stmt.Function.name.lexeme, fn_ptr);
            }
        } else {
            self.advance();
        }
    }

    // Reset position for main parse
    self.current = original_pos;

    // Create statements array with function declarations first
    var statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        statements.deinit();
    }

    // Add all function declarations first
    var it = function_table.iterator();
    while (it.next()) |entry| {
        try statements.append(entry.value_ptr.*.*);
    }

    // Now parse the rest of the statements normally
    try self.parseDirective();

    while (true) {
        const current = self.peek();
        if (current.type == .EOF) break;

        const stmt = blk: {
            if (self.peek().type == .VAR or self.peek().type == .CONST)
                break :blk try self.parseVarDecl()
            else if (self.peek().type == .LEFT_BRACE) {
                const block_stmt = if (try self.block(null, .NONE)) |expr|
                    ast.Stmt{
                        .base = .{
                            .id = ast.generateNodeId(),
                            .span = ast.SourceSpan.fromToken(self.previous()),
                        },
                        .data = .{
                            .Expression = expr,
                        },
                    }
                else
                    ast.Stmt{
                        .base = .{
                            .id = ast.generateNodeId(),
                            .span = ast.SourceSpan.fromToken(self.previous()),
                        },
                        .data = .{
                            .Expression = null,
                        },
                    };
                break :blk block_stmt;
            } else if (self.peek().type == .FUNCTION) {
                // Skip the entire function declaration since we already processed it
                self.advance(); // consume 'function'
                if (self.peek().type == .IDENTIFIER) {
                    self.advance(); // consume function name
                }
                if (self.peek().type == .LEFT_PAREN) {
                    self.advance(); // consume '('
                    // Skip parameters
                    var paren_count: usize = 1;
                    while (paren_count > 0 and self.peek().type != .EOF) {
                        if (self.peek().type == .LEFT_PAREN) paren_count += 1;
                        if (self.peek().type == .RIGHT_PAREN) paren_count -= 1;
                        self.advance();
                    }
                }
                // Skip return type if present
                if (self.peek().type == .RETURNS) {
                    self.advance(); // consume 'returns'
                    if (self.peek().type == .LEFT_TUPLE) {
                        self.advance(); // consume '('
                        var tuple_count: usize = 1;
                        while (tuple_count > 0 and self.peek().type != .EOF) {
                            if (self.peek().type == .LEFT_TUPLE) tuple_count += 1;
                            if (self.peek().type == .RIGHT_TUPLE) tuple_count -= 1;
                            self.advance();
                        }
                    }
                }
                // Skip function body
                if (self.peek().type == .LEFT_BRACE) {
                    self.advance(); // consume '{'
                    var brace_count: usize = 1;
                    while (brace_count > 0 and self.peek().type != .EOF) {
                        if (self.peek().type == .LEFT_BRACE) brace_count += 1;
                        if (self.peek().type == .RIGHT_BRACE) brace_count -= 1;
                        self.advance();
                    }
                }
                break :blk ast.Stmt{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.previous()),
                    },
                    .data = .{
                        .Expression = null,
                    },
                };
            } else if (self.peek().type == .STRUCT_TYPE) {
                break :blk try self.parseStructDeclStmt();
            } else if (self.peek().type == .ENUM_TYPE) {
                break :blk try self.parseEnumDecl();
            } else if (self.peek().type == .TRY) {
                break :blk try self.parseTryStmt();
            } else if (self.peek().type == .ASSERT) {
                break :blk try self.parseAssertStmt();
            } else break :blk try self.parseExpressionStmt();
        };

        // Only append non-null expression statements
        switch (stmt.data) {
            .Expression => |expr| {
                if (expr != null) {
                    try statements.append(stmt);
                }
            },
            else => try statements.append(stmt),
        }
    }

    // After parsing everything, check if we need an entry point in safe mode
    if (self.mode == .Safe and !self.has_entry_point) {
        return error.MissingEntryPoint;
    }

    return statements.toOwnedSlice();
}

pub fn parseExpressionStmt(self: *Parser) ErrorList!ast.Stmt {
    // Special handling for variable declarations
    if (self.peek().type == .VAR) {
        return declaration_parser.parseVarDecl(self); // Handle var declarations separately
    }

    const expr = try expression_parser.parseExpression(self);

    // Check if we need a semicolon
    const needs_semicolon = if (expr) |e| switch (e.data) {
        .Block => false,
        .If => false, // If expressions don't need semicolons
        .While => false,
        .For => false,
        .ForEach => false,
        .Peek => true,
        .Match => false,
        .Index => true,
        .Assignment => true,
        .Assert => true,
        else => true,
    } else true;

    // Only check for semicolon if we need one
    if (needs_semicolon) {
        if (self.peek().type != .SEMICOLON) {
            if (expr) |e| {
                e.deinit(self.allocator);
                self.allocator.destroy(e);
            }
            return error.ExpectedSemicolon;
        }
        self.advance(); // Consume the semicolon
    }

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Expression = expr,
        },
    };
}

pub fn parseBlockStmt(self: *Parser) ErrorList![]ast.Stmt {
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance(); // consume {

    var statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        statements.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try parseStatement(self);
        try statements.append(stmt);
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance(); // consume }

    return statements.toOwnedSlice();
}

pub fn parseReturnStmt(self: *Parser) ErrorList!ast.Stmt {

    // Consume 'return'
    if (self.peek().type != .RETURN) {
        return error.UnexpectedToken;
    }
    self.advance();

    var value: ?*ast.Expr = null;
    const type_info = ast.TypeInfo{ .base = .Nothing }; // Default to Nothing type

    if (self.peek().type != .SEMICOLON) {
        value = try expression_parser.parseExpression(self);
    }

    if (self.peek().type != .SEMICOLON) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected semicolon", .{});
        return error.ExpectedSemicolon;
    }
    self.advance();

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Return = .{
                .value = value,
                .type_info = type_info,
            },
        },
    };
}

pub fn parseStatement(self: *Parser) ErrorList!ast.Stmt {
    return switch (self.peek().type) {
        .VAR, .CONST => declaration_parser.parseVarDecl(self),
        .FUNCTION => declaration_parser.parseFunctionDecl(self),
        .RETURN => parseReturnStmt(self),
        .CONTINUE => parseContinueStmt(self),
        .BREAK => parseBreakStmt(self),
        .LEFT_BRACE => blk: {
            const block_expr = if (try Parser.block(self, null, .NONE)) |expr|
                ast.Stmt{ .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(self.peek()),
                }, .data = .{ .Expression = expr } }
            else
                ast.Stmt{ .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(self.peek()),
                }, .data = .{ .Expression = null } };
            break :blk block_expr;
        },
        .STRUCT_TYPE => if (self.peekAhead(1).type == .LEFT_BRACE)
            try parseStructDeclStmt(self)
        else
            try parseExpressionStmt(self),
        .ENUM_TYPE => declaration_parser.parseEnumDecl(self),
        .TRY => try parseTryStmt(self),
        .ASSERT => try parseAssertStmt(self),
        else => try parseExpressionStmt(self),
    };
}

pub fn parseAssertStmt(self: *Parser) ErrorList!ast.Stmt {
    if (self.peek().type != .ASSERT) {
        return error.UnexpectedToken;
    }

    // Store location information before advancing
    const assert_token = self.peek();
    const location = Reporter.Location{
        .file = self.current_file,
        .line = assert_token.line,
        .column = assert_token.column,
    };

    self.advance();

    // Expect opening parenthesis
    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const condition = try expression_parser.parseExpression(self);
    if (condition == null) {
        return error.ExpectedExpression;
    }

    // Check for optional message parameter
    var message: ?*ast.Expr = null;
    if (self.peek().type == .COMMA) {
        self.advance(); // consume comma

        message = try expression_parser.parseExpression(self);
        if (message == null) {
            return error.ExpectedExpression;
        }
    }

    // Expect closing parenthesis
    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(assert_token),
        },
        .data = .{
            .Assert = .{
                .condition = condition.?,
                .location = location,
                .message = message,
            },
        },
    };
}

pub fn parseTryStmt(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'try'

    // Parse the try block
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }

    // Parse try block statements
    self.advance(); // consume {
    var try_statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (try_statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        try_statements.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try parseStatement(self);
        try try_statements.append(stmt);
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance(); // consume }

    // Parse catch block
    if (self.peek().type != .CATCH) {
        return error.ExpectedCatch;
    }
    self.advance(); // consume 'catch'

    // Parse optional error variable
    var error_var: ?token.Token = null;
    if (self.peek().type == .IDENTIFIER) {
        error_var = self.peek();
        self.advance();
    }

    // Parse catch block
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }

    self.advance(); // consume {
    var catch_statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (catch_statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        catch_statements.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try parseStatement(self);
        try catch_statements.append(stmt);
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance(); // consume }

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Try = .{
                .try_body = try try_statements.toOwnedSlice(),
                .catch_body = try catch_statements.toOwnedSlice(),
                .error_var = null,
            },
        },
    };
}

pub fn parseStructDeclStmt(self: *Parser) ErrorList!ast.Stmt {
    const expr = try declaration_parser.parseStructDecl(self, null, .NONE) orelse return error.InvalidExpression;
    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Expression = expr,
        },
    };
}

pub fn parseContinueStmt(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'continue' keyword

    if (self.peek().type != .SEMICOLON) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected semicolon", .{});
        return error.ExpectedSemicolon;
    }
    self.advance(); // consume ';'

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Continue = {},
        },
    };
}

pub fn parseBreakStmt(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'break' keyword

    if (self.peek().type != .SEMICOLON) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected semicolon", .{});
        return error.ExpectedSemicolon;
    }
    self.advance(); // consume ';'

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Break = {},
        },
    };
}
