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
    current_file: []const u8,

    const rules = blk: {
        var r = std.EnumArray(token.TokenType, ParseRule).initFill(ParseRule{});

        // Binary operators
        r.set(.PLUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
        r.set(.MINUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
        r.set(.ASTERISK, .{ .infix = binary, .precedence = .FACTOR });
        r.set(.SLASH, .{ .infix = binary, .precedence = .FACTOR });
        r.set(.MODULO, .{ .infix = binary, .precedence = .FACTOR });

        // Add compound assignment operators
        r.set(.PLUS_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });

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
        r.set(.LEFT_PAREN, .{ .prefix = grouping, .infix = call, .precedence = .CALL });
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

        // Add function declaration support
        r.set(.FN_KEYWORD, .{ .prefix = functionExpr });
        r.set(.FUNCTION_KEYWORD, .{ .prefix = functionExpr });

        // Add rule for the ? operator with lower precedence
        r.set(.QUESTION, .{ .infix = print, .precedence = .UNARY });

        // Add loop support
        r.set(.WHILE, .{ .prefix = whileExpr });
        r.set(.FOR, .{ .prefix = forExpr });

        break :blk r;
    };

    fn getRule(token_type: token.TokenType) ParseRule {
        return rules.get(token_type);
    }

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, debug_enabled: bool, mode: Mode, is_repl: bool) Parser {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .debug_enabled = debug_enabled,
            .mode = mode,
            .is_repl = is_repl,
            .current_file = "main.dx",
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
                } else if (self.peek().type == .FN_KEYWORD or self.peek().type == .FUNCTION_KEYWORD) {
                    break :blk try self.parseFunctionDecl();
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
        self.advance(); // consume 'var'

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();
        self.advance();

        // Parse type annotation if present
        var type_info: ast.TypeInfo = .{ .base = .Dynamic };
        if (self.peek().type == .COLON) {
            self.advance();
            const type_token = self.peek();
            type_info.base = switch (type_token.type) {
                .INT_TYPE => .Int,
                .FLOAT_TYPE => .Float,
                .STRING_TYPE => .String,
                .BOOLEAN_TYPE => .Boolean,
                .AUTO => .Auto,
                else => return error.InvalidType,
            };
            self.advance();

            // Handle multiple array dimensions
            var array_dimensions: u8 = 0;
            while (self.peek().type == .LEFT_BRACKET) {
                self.advance();
                if (self.peek().type != .RIGHT_BRACKET) {
                    return error.ExpectedRightBracket;
                }
                self.advance();
                array_dimensions += 1;
            }

            // If we found array brackets, create nested array types
            if (array_dimensions > 0) {
                var current_type = type_info.base;
                var i: u8 = 0;
                while (i < array_dimensions) : (i += 1) {
                    type_info = .{
                        .base = .Array,
                        .element_type = current_type,
                    };
                    current_type = .Array;
                }
            }
        } else if (self.mode == .Strict) {
            return error.ExpectedTypeAnnotation;
        }

        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();
            initializer = try self.parseExpression();
        }

        if (self.peek().type != .SEMICOLON) {
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{ .VarDecl = .{
            .name = name,
            .initializer = initializer,
            .type_info = type_info,
        } };
    }

    fn parseExpressionStmt(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing expression statement...\n", .{});
        }

        const expr = try self.parseExpression();

        // Check if we need a semicolon
        const needs_semicolon = if (expr) |e| switch (e.*) {
            .Block => false, // Blocks don't need semicolons
            .If => false, // If expressions don't need semicolons
            .While => false, // While loops don't need semicolons
            .For => false, // For loops don't need semicolons
            .ForEach => false, // ForEach loops don't need semicolons
            .Print => true, // Print expressions need semicolons
            else => true, // All other expressions need semicolons
        } else true;

        if (needs_semicolon) {
            if (self.peek().type != .SEMICOLON) {
                if (self.debug_enabled) {
                    std.debug.print("Expected semicolon but found: {s} at position {}\n", .{
                        @tagName(self.peek().type),
                        self.current,
                    });
                }
                if (expr) |e| {
                    e.deinit(self.allocator);
                    self.allocator.destroy(e);
                }
                return error.ExpectedSemicolon;
            }
            self.advance(); // Consume the semicolon
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
        while (@intFromEnum(precedence) <= @intFromEnum(Parser.getRule(self.peek().type).precedence)) {
            const infix_rule = Parser.getRule(self.peek().type).infix;
            if (infix_rule == null) break;

            if (self.debug_enabled) {
                std.debug.print("Found infix operator: {s}\n", .{@tagName(self.peek().type)});
            }

            // Don't advance here for function calls - let the call handler do it
            if (self.peek().type != .LEFT_PAREN) {
                self.advance();
            }

            left = try infix_rule.?(self, left, precedence);
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

    fn grouping(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance(); // consume (

        // If we have a left expression, this is a function call
        if (left != null) {
            return self.call(left, .NONE);
        }

        // Otherwise it's just a grouped expression
        const expr = try self.parseExpression() orelse return error.ExpectedExpression;

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance(); // consume )

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

        // We're already at the opening parenthesis, advance past it
        self.advance(); // consume (

        var arguments = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (arguments.items) |arg| {
                arg.deinit(self.allocator);
                self.allocator.destroy(arg);
            }
            arguments.deinit();
        }

        // Handle empty argument list
        if (self.peek().type == .RIGHT_PAREN) {
            self.advance(); // consume )
            const call_expr = try self.allocator.create(ast.Expr);
            call_expr.* = .{ .Call = .{
                .callee = callee.?,
                .arguments = try arguments.toOwnedSlice(),
            } };
            return call_expr;
        }

        // Parse first argument
        const arg = (try self.parseExpression()) orelse return error.ExpectedExpression;
        try arguments.append(arg);

        // Parse any additional arguments
        while (self.peek().type == .COMMA) {
            self.advance(); // consume comma
            const next_arg = (try self.parseExpression()) orelse return error.ExpectedExpression;
            try arguments.append(next_arg);
        }

        if (self.debug_enabled) {
            std.debug.print("Looking for closing paren, current token: {s}\n", .{@tagName(self.peek().type)});
        }

        // Expect closing parenthesis
        if (self.peek().type != .RIGHT_PAREN) {
            if (self.debug_enabled) {
                std.debug.print("Expected ), got {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedRightParen;
        }
        self.advance(); // consume )

        // Create call expression
        const call_expr = try self.allocator.create(ast.Expr);
        call_expr.* = .{ .Call = .{
            .callee = callee.?,
            .arguments = try arguments.toOwnedSlice(),
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

        // Check for semicolon after then branch if it's a print expression
        if (then_expr.* == .Print) {
            if (self.peek().type != .SEMICOLON) {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                then_expr.deinit(self.allocator);
                self.allocator.destroy(then_expr);
                return error.ExpectedSemicolon;
            }
            self.advance(); // consume semicolon
        }

        // Handle else branch
        var else_expr: ?*ast.Expr = null;
        if (self.peek().type == .ELSE) {
            self.advance(); // consume 'else'

            // Check if this is an else-if
            if (self.peek().type == .IF) {
                else_expr = try self.parseIfExpr(null, .NONE);
            } else {
                else_expr = try self.parsePrecedence(.NONE);

                // Check for semicolon after else branch if it's a print expression
                if (else_expr != null and else_expr.?.* == .Print) {
                    if (self.peek().type != .SEMICOLON) {
                        condition.deinit(self.allocator);
                        self.allocator.destroy(condition);
                        then_expr.deinit(self.allocator);
                        self.allocator.destroy(then_expr);
                        else_expr.?.deinit(self.allocator);
                        self.allocator.destroy(else_expr.?);
                        return error.ExpectedSemicolon;
                    }
                    self.advance(); // consume semicolon
                }
            }

            if (else_expr == null) {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                then_expr.deinit(self.allocator);
                self.allocator.destroy(then_expr);
                return error.ExpectedExpression;
            }
        } else {
            // Create implicit nothing for else branch
            else_expr = try self.allocator.create(ast.Expr);
            else_expr.?.* = .{ .Literal = .{ .nothing = {} } };
        }

        const if_expr = try self.allocator.create(ast.Expr);
        if_expr.* = .{ .If = .{
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
        if (self.debug_enabled) {
            std.debug.print("Parsing unary expression, current token: {s}\n", .{
                @tagName(self.peek().type),
            });
        }

        const operator = self.peek(); // Get the current token as operator
        self.advance(); // Move past the operator

        const right = try self.parsePrecedence(.UNARY) orelse return error.ExpectedExpression;

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
            // Special handling for return statements
            if (self.peek().type == .RETURN) {
                const return_stmt = try self.parseReturnStmt();
                try statements.append(return_stmt);
                break; // Exit after return statement
            }

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

        // Parse base type
        const base_type = switch (self.peek().type) {
            .IDENTIFIER => blk: {
                const type_name = self.peek().lexeme;
                // Match built-in type names
                const base = if (std.mem.eql(u8, type_name, "int"))
                    .Integer
                else if (std.mem.eql(u8, type_name, "float"))
                    .Float
                else if (std.mem.eql(u8, type_name, "string"))
                    .String
                else if (std.mem.eql(u8, type_name, "bool"))
                    .Boolean
                else {
                    self.allocator.destroy(type_expr);
                    return error.UnknownType;
                };
                self.advance();
                break :blk base;
            },
            else => {
                self.allocator.destroy(type_expr);
                return null;
            },
        };

        // Check for array type notation (e.g., int[])
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance();
            if (self.peek().type != .RIGHT_BRACKET) {
                self.allocator.destroy(type_expr);
                return error.ExpectedRightBracket;
            }
            self.advance();

            // Create array type expression
            type_expr.* = .{ .Array = .{
                .element_type = try self.allocator.create(ast.TypeExpr),
            } };
            type_expr.Array.element_type.* = .{ .Basic = base_type };
        } else {
            // Create basic type expression
            type_expr.* = .{ .Basic = base_type };
        }

        return type_expr;
    }

    fn check(self: *Parser, token_type: token.TokenType) bool {
        return self.peek().type == token_type;
    }

    fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
        self.advance(); // consume 'fn' or 'function'

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();
        self.advance();

        // Parse parameter list
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

        if (self.peek().type != .RIGHT_PAREN) {
            while (true) {
                if (self.peek().type != .IDENTIFIER) {
                    return error.ExpectedIdentifier;
                }
                const param_name = self.peek();
                self.advance();

                // Parse type annotation if present
                var param_type = ast.TypeInfo{ .base = .Dynamic };
                if (self.peek().type == .COLON) {
                    self.advance();
                    param_type.base = switch (self.peek().type) {
                        .INT_TYPE => .Int,
                        .FLOAT_TYPE => .Float,
                        .STRING_TYPE => .String,
                        .BOOLEAN_TYPE => .Boolean,
                        else => return error.InvalidType,
                    };
                    self.advance();
                }

                try params.append(.{
                    .name = param_name,
                    .type_info = param_type,
                });

                if (self.peek().type == .RIGHT_PAREN) break;
                if (self.peek().type != .COMMA) {
                    return error.ExpectedComma;
                }
                self.advance();
            }
        }
        self.advance();

        // Parse return type
        var return_type_info: ast.TypeInfo = .{ .base = .Dynamic };
        if (self.peek().type == .COLON) {
            self.advance();
            if (self.peek().type == .AUTO) {
                self.advance();
                return_type_info.base = .Auto;
            } else {
                const type_name = self.peek();
                self.advance();
                return_type_info.base = switch (type_name.type) {
                    .INT => .Int,
                    .FLOAT => .Float,
                    .STRING => .String,
                    .BOOL => .Boolean,
                    else => return error.InvalidType,
                };
            }
        }

        // Parse function body
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }

        const body = try self.parseBlockStmt();

        return ast.Stmt{ .Function = .{
            .name = name,
            .params = try params.toOwnedSlice(),
            .return_type_info = return_type_info,
            .body = body,
        } };
    }

    fn parseBlockStmt(self: *Parser) ErrorList![]ast.Stmt {
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
            const stmt = try self.parseStatement();
            try statements.append(stmt);
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance(); // consume }

        return statements.toOwnedSlice();
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

        var value: ?*ast.Expr = null;
        const type_info = ast.TypeInfo{ .base = .Nothing }; // Default to Nothing type

        if (self.peek().type != .SEMICOLON) {
            value = try self.parseExpression();
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

    fn array(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance(); // First, consume the LEFT_BRACKET token!

        var elements = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer elements.deinit();

        // Parse first element to establish the type
        if (self.peek().type != .RIGHT_BRACKET) {
            const first_element = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
            try elements.append(first_element);

            // Parse remaining elements
            while (self.peek().type == .COMMA) {
                self.advance(); // consume comma
                const element = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;
                try elements.append(element);
            }
        }

        if (self.peek().type != .RIGHT_BRACKET) {
            return error.ExpectedRightBracket;
        }
        self.advance(); // consume ]

        // Type check all elements after parsing
        if (elements.items.len > 1) {
            const first = elements.items[0];
            const is_int = first.* == .Literal and first.Literal == .int;
            const is_float = first.* == .Literal and first.Literal == .float;
            const is_string = first.* == .Literal and first.Literal == .string;
            const is_boolean = first.* == .Literal and first.Literal == .boolean;

            for (elements.items[1..]) |element| {
                const matches = if (is_int)
                    element.* == .Literal and element.Literal == .int
                else if (is_float)
                    element.* == .Literal and element.Literal == .float
                else if (is_string)
                    element.* == .Literal and element.Literal == .string
                else if (is_boolean)
                    element.* == .Literal and element.Literal == .boolean
                else
                    true;

                if (!matches) {
                    // Clean up all elements
                    for (elements.items) |elem| {
                        elem.deinit(self.allocator);
                        self.allocator.destroy(elem);
                    }
                    elements.deinit();
                    return error.ArrayTypeMismatch;
                }
            }
        }

        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{ .Array = try elements.toOwnedSlice() };
        return array_expr;
    }

    fn index(self: *Parser, array_expr: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("Parsing array index\n", .{});
        }

        // We're already at the LEFT_BRACKET when this function is called
        // No need to check for it or advance - we were called because we found it
        const index_expr = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;

        // Check for and consume the RIGHT_BRACKET
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

    fn functionExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        // Convert the function declaration into an expression
        const fn_stmt = try self.parseFunctionDecl();

        // Create and return a function expression
        const fn_expr = try self.allocator.create(ast.Expr);
        fn_expr.* = switch (fn_stmt) {
            .Function => |f| .{ .Function = .{
                .name = f.name,
                .params = f.params,
                .return_type_info = f.return_type_info,
                .body = f.body,
            } },
            else => unreachable,
        };

        return fn_expr;
    }

    // Add the print parsing function
    fn print(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        const print_expr = try self.allocator.create(ast.Expr);
        print_expr.* = .{
            .Print = .{
                .expr = left.?,
                .location = .{
                    .file = self.current_file,
                    .line = self.peek().line,
                    .column = self.peek().column,
                },
            },
        };

        return print_expr;
    }

    // Add new parsing function for compound assignments
    fn compound_assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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

        const operator = self.tokens[self.current - 1];
        const value = try self.parsePrecedence(.ASSIGNMENT) orelse return error.ExpectedExpression;

        // Create binary expression for the operation
        const binary_expr = try self.allocator.create(ast.Expr);
        binary_expr.* = .{
            .Binary = .{
                .left = left,
                .operator = .{ // Convert += to + for the operation
                    .type = switch (operator.type) {
                        .PLUS_EQUAL => .PLUS,
                        else => return error.UnsupportedCompoundOperator,
                    },
                    .lexeme = operator.lexeme,
                    .line = operator.line,
                    .column = operator.column,
                    .literal = operator.literal,
                },
                .right = value,
            },
        };

        // Create assignment expression
        const assign_expr = try self.allocator.create(ast.Expr);
        assign_expr.* = switch (left.?.*) {
            .Variable => |v| .{ .Assignment = .{
                .name = v,
                .value = binary_expr,
            } },
            .Index => |idx| .{ .IndexAssign = .{
                .array = idx.array,
                .index = idx.index,
                .value = binary_expr,
            } },
            else => unreachable,
        };

        return assign_expr;
    }

    fn parseParameters(self: *Parser, params: *std.ArrayList(ast.FunctionParam)) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("Parsing function parameters...\n", .{});
        }

        while (true) {
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }

            const param_name = self.peek();
            self.advance();

            var param_type: ?*ast.TypeExpr = null;
            if (self.peek().type == .COLON) {
                self.advance();
                param_type = try self.parseTypeExpr() orelse return error.ExpectedType;
            }

            try params.append(.{
                .name = param_name,
                .type_expr = param_type,
            });

            if (self.peek().type == .COMMA) {
                self.advance();
                continue;
            }

            break;
        }

        if (self.debug_enabled) {
            std.debug.print("Parsed {} parameters\n", .{params.items.len});
        }
    }

    fn parseStatement(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing statement...\n", .{});
            std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
        }

        return switch (self.peek().type) {
            .VAR => self.parseVarDecl(),
            .FN_KEYWORD, .FUNCTION_KEYWORD => self.parseFunctionDecl(),
            .RETURN => self.parseReturnStmt(),
            .LEFT_BRACE => blk: {
                const block_expr = if (try self.block(null, .NONE)) |expr|
                    ast.Stmt{ .Expression = expr }
                else
                    ast.Stmt{ .Expression = null };
                break :blk block_expr;
            },
            else => self.parseExpressionStmt(),
        };
    }

    fn whileExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing while expression...\n", .{});
        }

        self.advance(); // consume 'while'

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

        // Parse the loop body as a block statement if we see a left brace
        var body: *ast.Expr = undefined;
        if (self.peek().type == .LEFT_BRACE) {
            const block_stmts = try self.parseBlockStmt();
            const block_expr = try self.allocator.create(ast.Expr);
            block_expr.* = .{
                .Block = .{
                    .statements = block_stmts,
                    .value = null, // No final expression value for the block
                },
            };
            body = block_expr;
        } else {
            body = (try self.parseExpression()) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                return error.ExpectedExpression;
            };
        }

        const while_expr = try self.allocator.create(ast.Expr);
        while_expr.* = .{ .While = .{
            .condition = condition,
            .body = body,
        } };

        return while_expr;
    }

    fn forExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing for expression...\n", .{});
        }

        self.advance(); // consume 'for'

        if (self.peek().type != .LEFT_PAREN) {
            return error.ExpectedLeftParen;
        }
        self.advance();

        // Parse initializer
        var initializer: ?*ast.Stmt = null;
        if (self.peek().type != .SEMICOLON) {
            if (self.peek().type == .VAR) {
                const init_stmt = try self.parseVarDecl();
                const stmt_ptr = try self.allocator.create(ast.Stmt);
                stmt_ptr.* = init_stmt;
                initializer = stmt_ptr;
            } else {
                const expr_stmt = try self.parseExpressionStmt();
                const stmt_ptr = try self.allocator.create(ast.Stmt);
                stmt_ptr.* = expr_stmt;
                initializer = stmt_ptr;
            }
        }

        // Parse condition
        var condition: ?*ast.Expr = null;
        if (self.peek().type != .SEMICOLON) {
            condition = try self.parseExpression();
        }
        if (self.peek().type != .SEMICOLON) {
            return error.ExpectedSemicolon;
        }
        self.advance();

        // Parse increment
        var increment: ?*ast.Expr = null;
        if (self.peek().type != .RIGHT_PAREN) {
            increment = try self.parseExpression();
        }
        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();

        // Parse body
        const body = (try self.parseExpression()) orelse return error.ExpectedExpression;

        const for_expr = try self.allocator.create(ast.Expr);
        for_expr.* = .{ .For = .{
            .initializer = initializer,
            .condition = condition,
            .increment = increment,
            .body = body,
        } };

        return for_expr;
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
        const item_name = self.peek();
        self.advance();

        // Parse 'in' keyword
        if (self.peek().type != .IN) {
            return error.ExpectedInKeyword;
        }
        self.advance();

        // Parse array expression
        const array_expr = try self.parseExpression();

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();

        // Parse body as a block
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        const body = try self.parseBlockStmt();

        const foreach = try self.allocator.create(ast.Expr);
        foreach.* = .{ .ForEach = .{
            .item_name = item_name,
            .array = array_expr,
            .body = body,
        } };

        return foreach;
    }

    fn parseReturn(self: *Parser) ErrorList!ast.Stmt {
        self.advance(); // consume 'return'

        var value: ?*ast.Expr = null;
        const type_info = ast.TypeInfo{ .base = .Nothing };

        if (self.peek().type != .SEMICOLON) {
            value = try self.parseExpression();
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
};
