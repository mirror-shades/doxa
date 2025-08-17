const std = @import("std");
const ast = @import("../ast/ast.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("./expression_parser.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const Reporter = Reporting.Reporter;
const token = @import("../types/token.zig");
const declaration_parser = @import("./declaration_parser.zig");
const expression_parser = @import("./expression_parser.zig");
const import_parser = @import("./import_parser.zig");
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;

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

    // Check if we need a newline
    const needs_newline = if (expr) |e| switch (e.data) {
        .Block => false,
        .If => false, // If expressions don't need newlines
        .While => false,
        .For => false,
        .ForEach => false, // Make sure this is false
        .Peek => true,
        .Match => false,
        .Index => true,
        .Assignment => true,
        .Cast => |c| blk: {
            // If either branch is a block, allow omitting the newline
            const then_is_block = c.then_branch != null and c.then_branch.?.data == .Block;
            const else_is_block = c.else_branch != null and c.else_branch.?.data == .Block;
            break :blk !(then_is_block or else_is_block);
        },
        .Assert => true,
        else => true,
    } else true;

    // Only check for newline if we need one
    if (needs_newline) {
        const next_type = self.peek().type;
        if (next_type == .NEWLINE) {
            self.advance(); // Consume the newline
        } else if (next_type == .EOF or next_type == .RIGHT_BRACE) {
            // EOF or '}' terminates the statement
        } else {
            if (expr) |e| {
                e.deinit(self.allocator);
                self.allocator.destroy(e);
            }
            return error.ExpectedNewline;
        }
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

    if (self.peek().type != .NEWLINE) {
        value = try expression_parser.parseExpression(self);
    }

    if (self.peek().type != .NEWLINE) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected newline", .{});
        return error.ExpectedNewline;
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
        .EACH => parseEachStmt(self), // Add this line
        .IMPORT => blk: {
            // Allow imports inside blocks; perform side-effects and emit empty expression stmt
            _ = try import_parser.parseImportStmt(self);
            break :blk ast.Stmt{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(self.peek()) }, .data = .{ .Expression = null } };
        },
        .MODULE => blk: {
            _ = try import_parser.parseModuleStmt(self);
            break :blk ast.Stmt{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(self.peek()) }, .data = .{ .Expression = null } };
        },
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

    if (self.peek().type != .NEWLINE) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected newline", .{});
        return error.ExpectedNewline;
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

    if (self.peek().type != .NEWLINE) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Expected newline", .{});
        return error.ExpectedNewline;
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

pub fn parseEachStmt(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'each'

    // Parse item name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const item_name = self.peek();
    self.advance();

    // Check for optional index variable
    var index_name: ?token.Token = null;
    if (self.peek().type == .AT) {
        self.advance(); // consume 'at'

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        index_name = self.peek();
        self.advance();
    }

    // Parse 'in' keyword
    if (self.peek().type != .IN) {
        return error.ExpectedInKeyword;
    }
    self.advance();

    // Parse array expression. Special-case IDENTIFIER { to avoid consuming the loop body as a struct literal.
    var array_expr: ?*ast.Expr = null;
    if (self.peek().type == .IDENTIFIER and self.peekAhead(1).type == .LEFT_BRACE) {
        const name = self.peek();
        self.advance(); // consume identifier only; leave '{' for the loop body

        const var_expr = try self.allocator.create(ast.Expr);
        var_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(name),
            },
            .data = .{ .Variable = name },
        };
        array_expr = var_expr;
    } else {
        array_expr = try expression_parser.parseExpression(self);
        if (array_expr == null) {
            return error.ExpectedExpression;
        }
    }

    // Parse body as a block
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    const body = try parseBlockStmt(self);

    // Create ForEach expression
    const foreach_expr = try self.allocator.create(ast.Expr);
    foreach_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .ForEach = .{
                .item_name = item_name,
                .index_name = index_name,
                .array = array_expr.?,
                .body = body,
            },
        },
    };

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Expression = foreach_expr,
        },
    };
}
