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

    const rules = blk: {
        var r = std.EnumArray(token.TokenType, ParseRule).initFill(ParseRule{});

        // Binary operators
        r.set(.PLUS, .{ .infix = binary, .precedence = .TERM });
        r.set(.MINUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
        r.set(.ASTERISK, .{ .infix = binary, .precedence = .FACTOR });
        r.set(.SLASH, .{ .infix = binary, .precedence = .FACTOR });

        // Comparison operators
        r.set(.EQUALITY, .{ .infix = binary, .precedence = .EQUALITY });
        r.set(.BANG_EQUAL, .{ .infix = binary, .precedence = .EQUALITY });
        r.set(.LESS, .{ .infix = binary, .precedence = .COMPARISON });
        r.set(.LESS_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });
        r.set(.GREATER, .{ .infix = binary, .precedence = .COMPARISON });
        r.set(.GREATER_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });

        // Logical operators
        r.set(.AND_KEYWORD, .{ .infix = logical, .precedence = .AND });
        r.set(.OR_KEYWORD, .{ .infix = logical, .precedence = .OR });
        r.set(.AND_SYMBOL, .{ .infix = logical, .precedence = .AND });
        r.set(.OR_SYMBOL, .{ .infix = logical, .precedence = .OR });

        // Unary operators
        r.set(.BANG, .{ .prefix = unary });

        // Literals
        r.set(.INT, .{ .prefix = literal });
        r.set(.FLOAT, .{ .prefix = literal });
        r.set(.STRING, .{ .prefix = literal });
        r.set(.BOOL, .{ .prefix = literal });
        r.set(.NOTHING, .{ .prefix = literal });

        // Grouping
        r.set(.LEFT_PAREN, .{ .prefix = grouping, .infix = infix_call, .precedence = .CALL });
        r.set(.LEFT_BRACKET, .{ .prefix = array, .infix = index, .precedence = .CALL });

        // Variables and assignment
        r.set(.VAR, .{ .prefix = variable });
        r.set(.CONST, .{ .prefix = variable });
        r.set(.IDENTIFIER, .{ .prefix = variable });
        r.set(.ASSIGN, .{ .infix = assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });

        // Control flow
        r.set(.IF, .{ .prefix = parseIfExpr });

        // Blocks
        r.set(.LEFT_BRACE, .{ .prefix = block });

        break :blk r;
    };

    fn getRule(token_type: token.TokenType) ParseRule {
        return rules.get(token_type);
    }

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

            const stmt = blk: {
                if (self.peek().type == .VAR)
                    break :blk try self.parseVarDecl()
                else if (self.peek().type == .LEFT_BRACE) {
                    const block_stmt = if (try self.block(null, .NONE)) |expr|
                        ast.Stmt{ .Expression = expr }
                    else
                        ast.Stmt{ .Expression = null };
                    break :blk block_stmt;
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

        // If we're at a semicolon with no preceding expression, just skip it
        if (self.peek().type == .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Skipping standalone semicolon\n", .{});
            }
            self.advance();
            return ast.Stmt{ .Expression = null };
        }

        const expr = try self.parseExpression();

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

        // Get the prefix rule for the current token
        const prefix_rule = Parser.getRule(self.peek().type).prefix;
        if (prefix_rule == null) {
            if (self.debug_enabled) {
                std.debug.print("No prefix rule for token: {s}\n", .{@tagName(self.peek().type)});
            }
            return null;
        }

        // Parse prefix expression
        var left = try prefix_rule.?(self, null, precedence);
        if (left == null) return null;

        // Keep parsing infix expressions as long as we have higher precedence
        while (@intFromEnum(precedence) < @intFromEnum(Parser.getRule(self.peek().type).precedence)) {
            const infix_rule = Parser.getRule(self.peek().type).infix;
            if (infix_rule == null) break;

            if (self.debug_enabled) {
                std.debug.print("Found infix operator: {s}\n", .{@tagName(self.peek().type)});
            }

            // Don't advance here for function calls - let the call handler do it
            if (self.peek().type != .LEFT_PAREN) {
                self.advance();
            }

            left = try infix_rule.?(self, left, Parser.getRule(self.peek().type).precedence);
            if (left == null) return null;
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
                self.advance(); // Advance past the literal token
                break :blk new_expr;
            },
            .STRING => blk: {
                // Make a copy of the string data
                const string_copy = try self.allocator.dupe(u8, current.literal.string);
                const new_expr = try self.allocator.create(ast.Expr);
                new_expr.* = .{ .Literal = .{ .string = string_copy } };
                self.advance(); // Advance past the literal token
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

    fn binary(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;
        const operator = self.tokens[self.current - 1];
        const right = try self.parsePrecedence(@enumFromInt(@intFromEnum(Parser.getRule(operator.type).precedence) + 1)) orelse return error.ExpectedRightOperand;

        const binary_expr = try self.allocator.create(ast.Expr);
        binary_expr.* = .{ .Binary = .{
            .left = left,
            .operator = operator,
            .right = right,
        } };
        return binary_expr;
    }

    fn grouping(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const expr = try self.parseExpression() orelse return error.ExpectedExpression;

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();

        const grouping_expr = try self.allocator.create(ast.Expr);
        grouping_expr.* = .{ .Grouping = expr };
        return grouping_expr;
    }

    fn infix_call(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        self.advance();
        return self.call(left, precedence);
    }

    fn call(self: *Parser, callee: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing function call...\n", .{});
        }

        if (callee == null) return error.ExpectedCallable;

        var args = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (args.items) |arg| {
                arg.deinit(self.allocator);
                self.allocator.destroy(arg);
            }
            args.deinit();
        }

        // Parse arguments
        while (self.peek().type != .RIGHT_PAREN) {
            const arg = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
            try args.append(arg);

            if (self.debug_enabled) {
                std.debug.print("Parsed argument {}\n", .{args.items.len});
            }

            if (self.peek().type == .RIGHT_PAREN) break;

            if (self.peek().type != .COMMA) return error.ExpectedCommaOrParen;
            self.advance(); // consume comma
        }
        self.advance(); // consume right paren

        if (self.debug_enabled) {
            std.debug.print("Finished parsing function call with {} arguments\n", .{args.items.len});
        }

        // Create the call expression
        const call_expr = try self.allocator.create(ast.Expr);
        call_expr.* = .{ .Call = .{
            .callee = callee.?,
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

        // Handle else branch
        const else_expr = (try self.parseExpression()) orelse {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            then_expr.deinit(self.allocator);
            self.allocator.destroy(then_expr);
            return error.ExpectedExpression;
        };

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

    fn assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.InvalidAssignmentTarget;

        // Verify left side is a valid assignment target
        const is_valid_target = switch (left.?.*) {
            .Variable => true,
            .Index => true,
            else => false,
        };

        if (!is_valid_target) {
            return error.InvalidAssignmentTarget;
        }

        // Parse the value being assigned
        const value = try self.parsePrecedence(.ASSIGNMENT) orelse return error.ExpectedExpression;

        const assign_expr = try self.allocator.create(ast.Expr);
        assign_expr.* = switch (left.?.*) {
            .Variable => |v| .{ .Assignment = .{
                .name = v,
                .value = value,
            } },
            .Index => |idx| .{ .IndexAssign = .{
                .array = idx.array,
                .index = idx.index,
                .value = value,
            } },
            else => unreachable,
        };

        return assign_expr;
    }

    fn unary(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const operator = self.tokens[self.current - 1];
        const right = try self.parsePrecedence(.UNARY) orelse return error.ExpectedOperand;

        const unary_expr = try self.allocator.create(ast.Expr);
        unary_expr.* = .{ .Unary = .{
            .operator = operator,
            .right = right,
        } };
        return unary_expr;
    }

    fn variable(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const name = self.peek();
        self.advance(); // Advance past the identifier
        const var_expr = try self.allocator.create(ast.Expr);
        var_expr.* = .{ .Variable = name };
        return var_expr;
    }

    fn block(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing block expression...\n", .{});
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

        var last_expr: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Check if this is the last statement/expression
            const next_is_brace = self.peekAhead(1).type == .RIGHT_BRACE;

            if (next_is_brace) {
                // For the last expression, make semicolon optional
                if (self.peek().type == .SEMICOLON) {
                    self.advance(); // Skip trailing semicolon
                    break;
                }
                last_expr = try self.parseExpression();
                break;
            } else {
                // Parse regular statement
                const stmt = try self.parseExpressionStmt();
                try statements.append(stmt);
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{ .Block = .{
            .statements = try statements.toOwnedSlice(),
            .value = last_expr,
        } };

        return block_expr;
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

        if (self.debug_enabled) {
            std.debug.print("Parsing function parameters...\n", .{});
        }

        while (self.peek().type != .RIGHT_PAREN) {
            // Parameter name
            const param_name = self.peek();
            if (param_name.type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            self.advance();

            // Create a default type if none is specified
            const param_type = if (self.peek().type == .COLON) blk: {
                self.advance();
                break :blk (try self.parseTypeExpr()) orelse return error.ExpectedType;
            } else if (self.mode == .Strict) {
                return error.StrictModeRequiresType;
            } else blk: {
                // Create a default dynamic type for parameters without type annotations
                const type_expr = try self.allocator.create(ast.TypeExpr);
                type_expr.* = .{ .Basic = .Auto };
                break :blk type_expr;
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

        if (self.debug_enabled) {
            std.debug.print("Parsed {} parameters\n", .{params.items.len});
        }

        // Parse return type (required in strict mode, optional otherwise)
        var return_type: ?*ast.TypeExpr = null;
        if (self.peek().type == .COLON) {
            self.advance();
            return_type = try self.parseTypeExpr();
            if (return_type == null) {
                return error.ExpectedType;
            }
        } else if (self.mode == .Strict and self.peek().type != .LEFT_BRACE) {
            return error.StrictModeRequiresType;
        }

        // Parse body
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

        // Parse statements until we hit the closing brace
        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Look ahead to see if this is the last expression before the closing brace
            var is_last = false;
            var needs_semicolon = true;
            var pos = self.current;
            var brace_count: usize = 0;

            while (pos < self.tokens.len) : (pos += 1) {
                const tok = self.tokens[pos];
                if (tok.type == .LEFT_BRACE) {
                    brace_count += 1;
                } else if (tok.type == .RIGHT_BRACE) {
                    if (brace_count == 0) {
                        is_last = true;
                        break;
                    }
                    brace_count -= 1;
                } else if (tok.type == .SEMICOLON and brace_count == 0) {
                    needs_semicolon = true;
                    is_last = false;
                    break;
                }
            }

            if (is_last) {
                // Parse as expression and wrap in return
                if (try self.parseExpression()) |expr| {
                    try statements.append(ast.Stmt{ .Return = .{ .value = expr } });
                }
                if (needs_semicolon and self.peek().type == .SEMICOLON) {
                    self.advance(); // consume semicolon if present
                }
                break;
            }

            const stmt = try self.parseExpressionStmt();
            try statements.append(stmt);
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        return ast.Stmt{ .Function = .{
            .name = name_token,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = try statements.toOwnedSlice(),
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

    fn peekAhead(self: *Parser, offset: usize) token.Token {
        const pos = self.current + offset;
        if (pos >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[pos];
    }
};
