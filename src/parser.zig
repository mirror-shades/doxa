const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const MemoryManager = @import("memory.zig").MemoryManager;
const Reporting = @import("reporting.zig").Reporting;
const ErrorList = @import("reporting.zig").ErrorList;

// for pratt parsing
const Precedence = enum(u8) {
    NONE = 0,
    ASSIGNMENT = 1, // =
    OR = 2, // or
    AND = 3, // and
    EQUALITY = 4, // == !=
    COMPARISON = 5, // < > <= >=
    TERM = 6, // + -
    FACTOR = 7, // * /
    UNARY = 8, // ! -
    CALL = 9, // . () []
    PRIMARY = 10,
};

const ParseFn = *const fn (*Parser, ?*ast.Expr, Precedence) ErrorList!?*ast.Expr;

const Associativity = enum {
    LEFT,
    RIGHT,
    NONE,
};

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
    associativity: Associativity = .LEFT,
};

pub const Mode = enum {
    Normal,
    Strict,
    Warn,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const token.Token,
    current: usize, // array index is usize by default
    debug_enabled: bool,
    mode: Mode,
    is_repl: bool,

    pub fn init(memory: *MemoryManager, tokens: []const token.Token, debug_enabled: bool, is_repl: bool, is_strict_repl: bool) !Parser {
        if (tokens.len == 0) return error.EmptyTokenList;

        const mode = if (is_repl)
            if (is_strict_repl) Mode.Strict else Mode.Normal // Use provided REPL mode or default
        else blk: {
            // File mode detection logic
            if (tokens.len >= 2 and tokens[0].type == .HASH and tokens[1].type == .IDENTIFIER) {
                if (std.mem.eql(u8, tokens[1].lexeme, "strict")) {
                    break :blk Mode.Strict;
                } else if (std.mem.eql(u8, tokens[1].lexeme, "warn")) {
                    break :blk Mode.Warn;
                } else break :blk Mode.Normal;
            } else break :blk Mode.Normal;
        };

        return Parser{
            .allocator = memory.getAllocator(),
            .tokens = tokens,
            .current = if (!is_repl and mode != .Normal) 2 else 0,
            .debug_enabled = debug_enabled,
            .mode = mode,
            .is_repl = is_repl,
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
        if (self.debug_enabled) {
            std.debug.print("\nToken stream:\n", .{});
            for (self.tokens, 0..) |t, i| {
                std.debug.print("{}: {s} ({s})\n", .{ i, @tagName(t.type), t.lexeme });
            }
            std.debug.print("\n", .{});
        }

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

        // Check for const/var keyword
        const tok = self.peek();
        const is_mutable = switch (tok.type) {
            .VAR => true,
            .CONST => false,
            else => return error.UnexpectedToken,
        };
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
        const is_dynamic = blk: {
            if (self.peek().type == .COLON) {
                self.advance();
                type_expr = try self.parseTypeExpr();
                break :blk false; // If type is explicitly declared, it's not dynamic
            } else {
                break :blk !is_mutable and self.mode != .Strict; // Only allow dynamic if not const and not in strict mode
            }
        };

        // = or 'is'
        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();

            // Parse the expression
            const expr = try self.parseExpression();
            if (expr == null) {
                if (self.debug_enabled) {
                    std.debug.print("Failed to parse initializer expression\n", .{});
                }
                return error.ExpectedExpression;
            }
            initializer = expr;

            if (self.debug_enabled) {
                std.debug.print("Successfully parsed initializer\n", .{});
            }
        }

        // Add debug print before semicolon check
        if (self.debug_enabled) {
            std.debug.print("Checking for semicolon, current token: {s}\n", .{@tagName(self.peek().type)});
        }

        // ;
        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected SEMICOLON, got {s} at position {}\n", .{
                    @tagName(self.peek().type),
                    self.current,
                });
            }
            return error.ExpectedSemicolon;
        }
        self.advance(); // consume semicolon

        if (self.debug_enabled) {
            std.debug.print("\nSuccessfully parsed var declaration\n", .{});
        }

        // Type checking based on mode
        switch (self.mode) {
            .Strict => {
                if (type_expr == null) {
                    return error.StrictModeRequiresType;
                }
                if (is_dynamic) {
                    return error.StrictModeNoDynamicTypes;
                }
            },
            .Warn => {
                if (type_expr == null) {
                    // Log warning but continue
                    if (self.debug_enabled) {
                        std.debug.print("Warning: Variable declaration without type in warn mode\n", .{});
                    }
                }
            },
            .Normal => {
                // Allow both typed and untyped declarations
            },
        }

        return ast.Stmt{ .VarDecl = .{
            .name = id_tok,
            .type_expr = type_expr,
            .initializer = initializer,
            .is_mutable = is_mutable,
            .is_dynamic = is_dynamic,
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

        return ast.Stmt{ .Expression = expr };
    }

    fn getRule(token_type: token.TokenType) ParseRule {
        return switch (token_type) {
            .LEFT_PAREN => .{ .prefix = Parser.grouping, .infix = Parser.infix_call, .precedence = .CALL },
            .LEFT_BRACKET => .{ .prefix = Parser.array, .infix = Parser.index, .precedence = .CALL },
            .MINUS => .{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM },
            .PLUS => .{ .infix = Parser.binary, .precedence = .TERM },
            .SLASH => .{ .infix = Parser.binary, .precedence = .FACTOR },
            .ASTERISK => .{ .infix = Parser.binary, .precedence = .FACTOR },
            .BANG => .{ .prefix = Parser.unary },
            .ASSIGN => .{
                .infix = Parser.assignment, // Make sure this is properly set
                .precedence = .ASSIGNMENT, // This should be the lowest precedence
                .associativity = .RIGHT,
            },
            .BANG_EQUAL => .{ .infix = Parser.binary, .precedence = .EQUALITY },
            .EQUALITY => .{ .infix = Parser.binary, .precedence = .EQUALITY },
            .GREATER => .{ .infix = Parser.binary, .precedence = .COMPARISON },
            .GREATER_EQUAL => .{ .infix = Parser.binary, .precedence = .COMPARISON },
            .LESS => .{ .infix = Parser.binary, .precedence = .COMPARISON },
            .LESS_EQUAL => .{ .infix = Parser.binary, .precedence = .COMPARISON },
            .IDENTIFIER => .{
                .prefix = Parser.variable,
                .precedence = .NONE, // Important: identifiers should have lowest precedence
            },
            .STRING => .{ .prefix = Parser.literal },
            .INT => .{ .prefix = Parser.literal },
            .FLOAT => .{ .prefix = Parser.literal },
            .BOOL => .{ .prefix = Parser.literal },
            .IF => .{ .prefix = Parser.parseIfExpr },
            .LEFT_BRACE => .{ .prefix = Parser.block },
            .AND_KEYWORD, .AND_SYMBOL => .{
                .infix = Parser.logical,
                .precedence = .AND,
                .associativity = .LEFT,
            },
            .OR_KEYWORD, .OR_SYMBOL => .{
                .infix = Parser.logical,
                .precedence = .OR,
                .associativity = .LEFT,
            },
            else => .{},
        };
    }

    fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {
        // Start parsing at lowest precedence
        return try self.parsePrecedence(.ASSIGNMENT);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing with precedence: {}\n", .{@intFromEnum(precedence)});
            std.debug.print("Current token: {s} at position {}\n", .{
                @tagName(self.peek().type),
                self.current,
            });
        }

        // Get the prefix rule for current token
        const current = self.peek();
        const rule = getRule(current.type);
        const prefix_rule = rule.prefix orelse {
            if (self.debug_enabled) {
                std.debug.print("No prefix parse rule for {s}\n", .{@tagName(current.type)});
            }
            return null;
        };

        // Parse prefix expression
        if (self.debug_enabled) {
            std.debug.print("Applying prefix rule for {s}\n", .{@tagName(current.type)});
        }

        var left = try prefix_rule(self, null, precedence);
        if (left == null) {
            if (self.debug_enabled) {
                std.debug.print("Prefix rule returned null\n", .{});
            }
            return null;
        }

        // Keep parsing infix expressions while the next operator has higher or equal precedence
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.peek().type).precedence)) {
            const operator = self.peek();
            const next_rule = getRule(operator.type);
            const infix_rule = next_rule.infix orelse break;

            if (self.debug_enabled) {
                std.debug.print("Found infix operator: {s}\n", .{@tagName(operator.type)});
            }

            self.advance(); // Consume the operator

            // Adjust precedence based on associativity
            const next_precedence = switch (next_rule.associativity) {
                .LEFT => @as(Precedence, @enumFromInt(@intFromEnum(next_rule.precedence))),
                .RIGHT => @as(Precedence, @enumFromInt(@intFromEnum(next_rule.precedence) - 1)),
                .NONE => .PRIMARY, // Highest precedence to prevent chaining
            };

            left = try infix_rule(self, left, next_precedence);
            if (left == null) break;
        }

        return left;
    }

    fn literal(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const current = self.peek();

        if (self.debug_enabled) {
            std.debug.print("Attempting to parse literal, token: {s}\n", .{@tagName(current.type)});
        }

        // Don't try to parse tokens that aren't literals
        const expr = switch (current.type) {
            .INT, .FLOAT, .BOOL => blk: {
                const new_expr = try self.allocator.create(ast.Expr);
                new_expr.* = .{ .Literal = current.literal };
                self.advance();
                break :blk new_expr;
            },
            .STRING => blk: {
                // Make a copy of the string data
                const string_copy = try self.allocator.dupe(u8, current.literal.string);
                const new_expr = try self.allocator.create(ast.Expr);
                new_expr.* = .{ .Literal = .{ .string = string_copy } };
                self.advance();
                break :blk new_expr;
            },
            else => {
                if (self.debug_enabled) {
                    std.debug.print("Not a literal token: {s}\n", .{@tagName(current.type)});
                }
                return null;
            },
        };

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed literal, next token: {s}\n", .{@tagName(self.peek().type)});
        }

        return expr;
    }

    fn binary(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        const operator = self.tokens[self.current - 1];

        // Use the passed-in precedence instead of calculating it here
        const right = try self.parsePrecedence(precedence) orelse return error.ExpectedExpression;

        const binary_expr = try self.allocator.create(ast.Expr);
        binary_expr.* = .{ .Binary = .{
            .left = left.?,
            .operator = operator,
            .right = right,
        } };
        return binary_expr;
    }

    fn grouping(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance();
        const expr = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
        if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
        self.advance();
        return expr;
    }

    fn infix_call(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance();
        return self.call(left.?);
    }

    fn call(self: *Parser, callee: *ast.Expr) ErrorList!?*ast.Expr {
        var args = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (args.items) |arg| {
                arg.deinit(self.allocator);
                self.allocator.destroy(arg);
            }
            args.deinit();
        }

        while (self.peek().type != .RIGHT_PAREN) {
            const arg = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
            try args.append(arg);

            if (self.peek().type != .COMMA) break;
            self.advance(); // consume comma
        }

        if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
        self.advance(); // consume right paren

        const call_expr = try self.allocator.create(ast.Expr);
        call_expr.* = .{ .Call = .{
            .callee = callee,
            .arguments = try args.toOwnedSlice(),
        } };
        return call_expr;
    }

    fn parseIfExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
            else_expr = (try self.parseIfExpr(null, .PRIMARY)) orelse {
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

    fn assignment(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("Parsing assignment expression\n", .{});
        }

        const value = try self.parsePrecedence(precedence) orelse return error.ExpectedExpression;
        errdefer {
            value.deinit(self.allocator);
            self.allocator.destroy(value);
        }

        // Handle different assignment targets
        if (left) |target| {
            switch (target.*) {
                .Index => |index_expr| {
                    const assign_expr = try self.allocator.create(ast.Expr);
                    assign_expr.* = .{ .IndexAssign = .{
                        .array = index_expr.array,
                        .index = index_expr.index,
                        .value = value,
                    } };
                    return assign_expr;
                },
                .Variable => |name| {
                    if (self.debug_enabled) {
                        std.debug.print("Creating assignment expression\n", .{});
                    }
                    const assign_expr = try self.allocator.create(ast.Expr);
                    assign_expr.* = .{ .Assignment = .{
                        .name = name,
                        .value = value,
                    } };
                    return assign_expr;
                },
                else => {
                    value.deinit(self.allocator);
                    self.allocator.destroy(value);
                    return error.InvalidAssignmentTarget;
                },
            }
        }
        return error.InvalidAssignment;
    }

    fn unary(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const operator = self.peek();
        self.advance();

        const right = try self.parsePrecedence(.UNARY) orelse return error.ExpectedExpression;
        errdefer {
            right.deinit(self.allocator);
            self.allocator.destroy(right);
        }

        const unary_expr = try self.allocator.create(ast.Expr);
        unary_expr.* = .{ .Unary = .{
            .operator = operator,
            .right = right,
        } };
        return unary_expr;
    }

    fn variable(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const var_token = self.peek();
        self.advance();

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{ .Variable = var_token };
        return expr;
    }

    fn block(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const block_stmt = try self.parseBlock();
        const block_expr = try self.allocator.create(ast.Expr);

        // Extract the statements from the Block variant
        const statements = switch (block_stmt) {
            .Block => |stmts| stmts,
            else => unreachable, // parseBlock always returns a Block variant
        };

        block_expr.* = .{ .Block = statements };
        return block_expr;
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
                    // Instead of returning null, return an error
                    self.allocator.destroy(type_expr);
                    return error.UnknownType;
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

    fn array(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        var elements = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (elements.items) |elem| {
                elem.deinit(self.allocator);
                self.allocator.destroy(elem);
            }
            elements.deinit();
        }

        // Parse array elements until right bracket
        while (self.peek().type != .RIGHT_BRACKET) {
            const element = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
            try elements.append(element);

            if (self.peek().type == .COMMA) {
                self.advance();
            } else if (self.peek().type != .RIGHT_BRACKET) {
                return error.ExpectedCommaOrBracket;
            }
        }
        self.advance(); // consume ]

        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{ .Array = try elements.toOwnedSlice() };
        return array_expr;
    }

    fn index(self: *Parser, array_expr: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const index_expr = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;

        if (self.peek().type != .RIGHT_BRACKET) {
            index_expr.deinit(self.allocator);
            self.allocator.destroy(index_expr);
            return error.ExpectedRightBracket;
        }
        self.advance(); // consume ]

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{ .Index = .{
            .array = array_expr.?,
            .index = index_expr,
        } };
        return expr;
    }

    fn allocExpr(self: *Parser, expr: ast.Expr) ErrorList!*ast.Expr {
        const new_expr = try self.allocator.create(ast.Expr);
        new_expr.* = expr;
        return new_expr;
    }

    fn logical(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("Parsing logical expression\n", .{});
        }

        const operator = self.tokens[self.current - 1]; // Get the operator token (AND/OR)
        const right = try self.parsePrecedence(precedence) orelse return error.ExpectedExpression;

        const logical_expr = try self.allocator.create(ast.Expr);
        logical_expr.* = .{ .Logical = .{
            .left = left.?,
            .operator = operator,
            .right = right,
        } };
        return logical_expr;
    }
};
