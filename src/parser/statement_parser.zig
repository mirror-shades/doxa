const std = @import("std");
const ast = @import("../ast/ast.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("./expression_parser.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
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
    var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
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

    // Always allow block-like expressions without newline
    const is_block_like = if (expr) |e| switch (e.data) {
        .Block, .If, .Loop, .Peek, .Match => true,
        .Cast => |c| (c.then_branch != null and c.then_branch.?.data == .Block) or
            (c.else_branch != null and c.else_branch.?.data == .Block),
        else => false,
    } else false;

    if (!is_block_like) {
        // Only require newline for non-block-like expressions
        if (self.peek().type == .SEMICOLON) {
            self.advance();
        }
        const next_type = self.peek().type;
        if (next_type == .NEWLINE) {
            self.advance();
        } else if (next_type == .EOF or next_type == .RIGHT_BRACE) {
            // OK: statement terminated by end-of-file or closing brace
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

    var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
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

    // Only parse a value if the next token can start an expression
    if (self.peek().type != .NEWLINE and
        self.peek().type != .ELSE and
        self.peek().type != .RIGHT_BRACE and
        self.peek().type != .EOF)
    {
        value = try expression_parser.parseExpression(self);
    }

    // Accept optional semicolon, then newline, '}', or EOF as statement terminators
    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    const next_type = self.peek().type;
    if (next_type == .NEWLINE) {
        self.advance();
    } else if (next_type == .RIGHT_BRACE or next_type == .EOF) {
        // do not consume here; caller (block parser) will handle
    } else {
        const location = Reporting.Location{
            .file = self.current_file,
            .range = .{
                .start_line = @intCast(self.peek().line),
                .start_col = self.peek().column,
                .end_line = @intCast(self.peek().line),
                .end_col = self.peek().column + self.peek().lexeme.len,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_NEWLINE, "Expected newline or closing brace after return", .{});
        return error.ExpectedNewline;
    }

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
        .MAP => try parseMapStatement(self),
        else => try parseExpressionStmt(self),
    };
}

pub fn parseAssertStmt(self: *Parser) ErrorList!ast.Stmt {
    if (self.peek().type != .ASSERT) {
        return error.UnexpectedToken;
    }

    // Store location information before advancing
    const assert_token = self.peek();
    const location = Location{
        .file = self.current_file,
        .range = .{
            .start_line = @intCast(assert_token.line),
            .start_col = assert_token.column,
            .end_line = @intCast(assert_token.line),
            .end_col = assert_token.column + assert_token.lexeme.len,
        },
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

    // Allow optional semicolon before required newline or accept '}'/EOF terminators
    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const location = Reporting.Location{
            .file = self.current_file,
            .range = .{
                .start_line = @intCast(self.peek().line),
                .start_col = self.peek().column,
                .end_line = @intCast(self.peek().line),
                .end_col = self.peek().column + self.peek().lexeme.len,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_NEWLINE, "Expected newline", .{});
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

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

    // Allow optional semicolon before required newline or accept '}'/EOF terminators
    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const location = Reporting.Location{
            .file = self.current_file,
            .range = .{
                .start_line = @intCast(self.peek().line),
                .start_col = self.peek().column,
                .end_line = @intCast(self.peek().line),
                .end_col = self.peek().column + self.peek().lexeme.len,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_NEWLINE, "Expected newline", .{});
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

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

    // Check if the array expression is a range (e.g., 1 to limit)
    // If so, generate a direct loop instead of creating an array first
    if (array_expr.?.data == .Range) {
        const range = array_expr.?.data.Range;

        // Create a direct loop: for item is start; while item <= end; step item += 1 { <body> }

        // Build initializer: var item is start
        const item_type = ast.TypeInfo{ .base = .Int };
        const init_stmt = try self.allocator.create(ast.Stmt);
        init_stmt.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .VarDecl = .{ .name = item_name, .type_info = item_type, .initializer = range.start, .is_public = false } } };

        // Build condition: item <= end
        const load_item = try self.allocator.create(ast.Expr);
        load_item.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Variable = item_name } };

        const cond_expr = try self.allocator.create(ast.Expr);
        cond_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Binary = .{ .left = load_item, .operator = token.Token.initWithFile(.LESS_EQUAL, "<=", .{ .nothing = {} }, item_name.line, item_name.column, self.current_file), .right = range.end } } };

        // Build step: item = item + 1
        const one_expr = try self.allocator.create(ast.Expr);
        one_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Literal = .{ .int = 1 } } };
        const left_item = try self.allocator.create(ast.Expr);
        left_item.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Variable = item_name } };
        const add_expr = try self.allocator.create(ast.Expr);
        add_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Binary = .{ .left = left_item, .operator = token.Token.initWithFile(.PLUS, "+", .{ .nothing = {} }, item_name.line, item_name.column, self.current_file), .right = one_expr } } };
        const step_expr = try self.allocator.create(ast.Expr);
        step_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Assignment = .{ .name = item_name, .value = add_expr, .target_context = null } } };

        // Create the loop expression
        const loop_expr = try self.allocator.create(ast.Expr);
        loop_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Loop = .{ .var_decl = init_stmt, .condition = cond_expr, .body = try self.allocator.create(ast.Expr), .step = step_expr } } };

        // Set the body as a block expression
        loop_expr.data.Loop.body.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Block = .{ .statements = body, .value = null } } };

        // Return as an expression statement
        return ast.Stmt{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Expression = loop_expr } };
    }

    // Create ForEach expression
    // Desugar foreach into Loop (init; cond; body; step)
    // for idx is 0; while idx < length(array); step idx += 1 { item is array[idx]; <body> }
    const idx_name = if (index_name) |tok| tok else token.Token.initWithFile(.IDENTIFIER, "__idx", .{ .nothing = {} }, self.peek().line, self.peek().column, self.current_file);

    // Build initializer: var idx is 0
    const idx_type = ast.TypeInfo{ .base = .Int };
    const zero_expr = try self.allocator.create(ast.Expr);
    zero_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Literal = .{ .int = 0 } } };
    const init_stmt = try self.allocator.create(ast.Stmt);
    init_stmt.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .VarDecl = .{ .name = idx_name, .type_info = idx_type, .initializer = zero_expr, .is_public = false } } };

    // Build condition: idx < @length(array)
    const load_idx = try self.allocator.create(ast.Expr);
    load_idx.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Variable = idx_name } };

    // Create @length method call
    const length_method = token.Token.initWithFile(.LENGTH, "length", .{ .nothing = {} }, idx_name.line, idx_name.column, self.current_file);
    const len_expr = try self.allocator.create(ast.Expr);
    len_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .InternalCall = .{ .receiver = array_expr.?, .method = length_method, .arguments = &[_]*ast.Expr{} } } };

    const cond_expr = try self.allocator.create(ast.Expr);
    cond_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Binary = .{ .left = load_idx, .operator = token.Token.initWithFile(.LESS, "<", .{ .nothing = {} }, idx_name.line, idx_name.column, self.current_file), .right = len_expr } } };

    // Build step: idx = idx + 1
    const one_expr = try self.allocator.create(ast.Expr);
    one_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Literal = .{ .int = 1 } } };
    const left_idx2 = try self.allocator.create(ast.Expr);
    left_idx2.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Variable = idx_name } };
    const add_expr = try self.allocator.create(ast.Expr);
    add_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Binary = .{ .left = left_idx2, .operator = token.Token.initWithFile(.PLUS, "+", .{ .nothing = {} }, idx_name.line, idx_name.column, self.current_file), .right = one_expr } } };
    const step_expr = try self.allocator.create(ast.Expr);
    step_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(idx_name) }, .data = .{ .Assignment = .{ .name = idx_name, .value = add_expr, .target_context = null } } };

    // Build item assignment at loop body head: item_name is array[idx]
    const arr_index_left = try self.allocator.create(ast.Expr);
    arr_index_left.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Index = .{ .array = array_expr.?, .index = load_idx } } };
    const item_assign = try self.allocator.create(ast.Stmt);

    // Let type inference handle the array element type
    // The type will be inferred from the array[index] expression during semantic analysis
    const item_type = ast.TypeInfo{ .base = .Nothing, .is_mutable = true };

    item_assign.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .VarDecl = .{ .name = item_name, .type_info = item_type, .initializer = arr_index_left, .is_public = false } } };

    // Prepend item assignment to body
    var new_body = try self.allocator.alloc(ast.Stmt, body.len + 1);
    new_body[0] = item_assign.*;
    @memcpy(new_body[1..], body);

    // Wrap new_body into a block expression
    const block_expr = try self.allocator.create(ast.Expr);
    block_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Block = .{ .statements = new_body, .value = null } } };

    // Create Loop expression
    const loop_expr = try self.allocator.create(ast.Expr);
    loop_expr.* = .{ .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(item_name) }, .data = .{ .Loop = .{ .var_decl = init_stmt, .condition = cond_expr, .step = step_expr, .body = block_expr } } };

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Expression = loop_expr,
        },
    };
}

pub fn parseMapStatement(self: *Parser) ErrorList!ast.Stmt {
    if (self.peek().type != .MAP) {
        return error.ExpectedMapKeyword;
    }
    const map_token = self.peek();
    self.advance();

    // Parse map entries
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();
    var entries = std.array_list.Managed(*ast.MapEntry).init(self.allocator);
    errdefer {
        for (entries.items) |entry| {
            entry.key.deinit(self.allocator);
            self.allocator.destroy(entry.key);
            entry.value.deinit(self.allocator);
            self.allocator.destroy(entry.value);
            self.allocator.destroy(entry);
        }
        entries.deinit();
    }

    // Parse entries until we hit a right brace
    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        // Parse key
        const key = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Expect 'is' as separator between key and value
        if (self.peek().type != .ASSIGN) {
            key.deinit(self.allocator);
            self.allocator.destroy(key);
            return error.UseIsForAssignment;
        }
        self.advance(); // consume 'is'

        // Parse value
        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Create and append entry
        const entry = try self.allocator.create(ast.MapEntry);
        entry.* = .{
            .key = key,
            .value = value,
        };
        try entries.append(entry);

        // Handle separators: comma and/or newline(s)
        if (self.peek().type == .COMMA) {
            self.advance();
        }
        while (self.peek().type == .NEWLINE) self.advance();
        // Allow trailing comma/newlines
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance();

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(map_token),
        },
        .data = .{
            .MapLiteral = try entries.toOwnedSlice(),
        },
    };
}
