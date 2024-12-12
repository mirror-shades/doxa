const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("parser_types.zig").Parser;
const expr_parser = @import("expression_parser.zig");
const ErrorList = @import("../reporting.zig").ErrorList;
const token = @import("../token.zig");
const declaration_parser = @import("./declaration_parser.zig");
const expression_parser = @import("./expression_parser.zig");
const module_parser = @import("./module_parser.zig");
pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
    if (self.debug_enabled) {
        std.debug.print("\nToken stream:\n", .{});
        for (self.tokens, 0..) |t, i| {
            std.debug.print("{}: {s} ({s})\n", .{ i, @tagName(t.type), t.lexeme });
        }
        std.debug.print("\n", .{});
    }

    // First pass: collect all function declarations and create them
    var function_table = std.StringHashMap(*ast.Stmt).init(self.allocator);
    defer function_table.deinit();

    // Store current position
    const original_pos = self.current;

    // First pass to find and store all function declarations
    while (self.peek().type != .EOF) {
        if (self.peek().type == .FN_KEYWORD or
            self.peek().type == .FUNCTION_KEYWORD)
        {
            // Don't advance here - let parseFunctionDecl handle it
            const fn_stmt = try self.parseFunctionDecl();
            const fn_ptr = try self.allocator.create(ast.Stmt);
            fn_ptr.* = fn_stmt;

            // Get the function name from the statement
            if (fn_stmt == .Function) {
                try function_table.put(fn_stmt.Function.name.lexeme, fn_ptr);
            }
        }
        self.advance();
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
                    ast.Stmt{ .Expression = expr }
                else
                    ast.Stmt{ .Expression = null };
                break :blk block_stmt;
            } else if (self.peek().type == .FN_KEYWORD or
                self.peek().type == .FUNCTION_KEYWORD or
                self.peek().type == .MAIN)
            {
                // Skip the entire function declaration
                var brace_count: usize = 0;
                while (self.peek().type != .EOF) {
                    if (self.peek().type == .LEFT_BRACE) {
                        brace_count += 1;
                    } else if (self.peek().type == .RIGHT_BRACE) {
                        brace_count -= 1;
                        if (brace_count == 0) {
                            self.advance(); // consume the final }
                            break;
                        }
                    }
                    self.advance();
                }
                break :blk ast.Stmt{ .Expression = null };
            } else if (self.peek().type == .STRUCT_TYPE) {
                break :blk try self.parseStructDeclStmt();
            } else if (self.peek().type == .ENUM_TYPE) {
                break :blk try self.parseEnumDecl();
            } else if (self.peek().type == .TRY) {
                break :blk try self.parseTryStmt();
            } else break :blk try self.parseExpressionStmt();
        };

        // Only append non-null expression statements
        switch (stmt) {
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
    if (self.debug_enabled) {
        std.debug.print("\nParsing expression statement...\n", .{});
    }

    // Special handling for variable declarations
    if (self.peek().type == .VAR) {
        return declaration_parser.parseVarDecl(self); // Handle var declarations separately
    }

    const expr = try expression_parser.parseExpression(self);

    // Check if we need a semicolon
    const needs_semicolon = if (expr) |e| switch (e.*) {
        .Block => false,
        .If => false,
        .While => false,
        .For => false,
        .ForEach => false,
        .Print => true,
        .Match => false,
        .Index => true,
        .Assignment => true, // Added this case
        else => true,
    } else true;

    // Handle question mark operator
    var final_expr = expr;
    if (expr != null and self.peek().type == .QUESTION) {
        self.advance(); // consume ?
        const print_expr = try self.allocator.create(ast.Expr);
        print_expr.* = .{
            .Print = .{
                .expr = expr.?,
                .location = .{
                    .file = self.current_file,
                    // I have no idea why this is necessary, but it is.
                    .line = @divTrunc(self.peek().line + 1, 2),
                    .column = self.peek().column,
                },
            },
        };
        final_expr = print_expr;
    }

    if (needs_semicolon or (final_expr != null and final_expr.?.* == .Print)) {
        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected semicolon but found: {s} at position {}\n", .{
                    @tagName(self.peek().type),
                    self.current,
                });
                if (final_expr != null) {
                    std.debug.print("Expression type: {s}\n", .{@tagName(@as(std.meta.Tag(ast.Expr), final_expr.?.*))});
                }
            }
            if (final_expr) |e| {
                e.deinit(self.allocator);
                self.allocator.destroy(e);
            }
            return error.ExpectedSemicolon;
        }
        self.advance(); // Consume the semicolon
    }

    return ast.Stmt{ .Expression = final_expr };
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
    if (self.debug_enabled) {
        std.debug.print("\nParsing return statement...\n", .{});
    }

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
        return error.ExpectedSemicolon;
    }
    self.advance();

    return ast.Stmt{ .Return = .{
        .value = value,
        .type_info = type_info,
    } };
}

pub fn parseStatement(self: *Parser) ErrorList!ast.Stmt {
    if (self.debug_enabled) {
        std.debug.print("\nParsing statement...\n", .{});
        std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
    }

    return switch (self.peek().type) {
        .VAR => declaration_parser.parseVarDecl(self),
        .FN_KEYWORD, .FUNCTION_KEYWORD => declaration_parser.parseFunctionDecl(self),
        .RETURN => parseReturnStmt(self),
        .LEFT_BRACE => blk: {
            const block_expr = if (try Parser.block(self, null, .NONE)) |expr|
                ast.Stmt{ .Expression = expr }
            else
                ast.Stmt{ .Expression = null };
            break :blk block_expr;
        },
        .STRUCT_TYPE => if (self.peekAhead(1).type == .LEFT_BRACE)
            try parseStructDeclStmt(self)
        else
            try parseExpressionStmt(self),
        .ENUM_TYPE => declaration_parser.parseEnumDecl(self),
        .TRY => try parseTryStmt(self),
        .MODULE => try module_parser.parseModuleDecl(self),
        .IMPORT => try module_parser.parseImport(self, self.peek()),
        else => parseExpressionStmt(self),
    };
}

pub fn parseTryStmt(self: *Parser) ErrorList!ast.Stmt {
    if (self.debug_enabled) {
        std.debug.print("\nParsing try statement...\n", .{});
    }

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

    return ast.Stmt{ .Try = .{
        .try_body = try try_statements.toOwnedSlice(),
        .catch_body = try catch_statements.toOwnedSlice(),
        .error_var = error_var,
    } };
}

pub fn parseStructDeclStmt(self: *Parser) ErrorList!ast.Stmt {
    const expr = try declaration_parser.parseStructDecl(self, null, .NONE) orelse return error.InvalidExpression;
    return ast.Stmt{ .Expression = expr };
}
