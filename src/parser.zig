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
    TRY = 1,
    ASSIGNMENT = 2, // =
    OR = 3, // or
    AND = 4, // and
    XOR = 5, // xor
    EQUALITY = 6, // == !=
    COMPARISON = 7, // < > <= >=
    QUANTIFIER = 8, // ∃ ∀
    TERM = 9, // + -
    FACTOR = 10, // * /
    UNARY = 11, // ! -
    CALL = 12, // . () []
    PRIMARY = 13,
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

const TokenStyle = enum {
    Keyword,
    Symbol,
    Undefined,
};

pub const Parser = struct {
    pub const Mode = enum { Safe, Normal }; // Declare enum once at struct level

    tokens: []const token.Token,
    current: usize,
    allocator: std.mem.Allocator,
    debug_enabled: bool,
    mode: Mode = .Normal,
    current_file: []const u8,

    // tracking style conflicts
    fn_style: TokenStyle = .Undefined,
    assign_style: TokenStyle = .Undefined,
    equality_style: TokenStyle = .Undefined,
    and_style: TokenStyle = .Undefined,
    or_style: TokenStyle = .Undefined,
    where_style: TokenStyle = .Undefined,

    fn_style_warning_shown: bool = false,
    assign_style_warning_shown: bool = false,
    equality_style_warning_shown: bool = false,
    and_style_warning_shown: bool = false,
    or_style_warning_shown: bool = false,
    where_style_warning_shown: bool = false,

    // Add these fields to track entry points
    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,

    const rules = blk: {
        var r = std.EnumArray(token.TokenType, ParseRule).initFill(ParseRule{});

        // Add rule for entry point arrow
        r.set(.MAIN, .{ .prefix = functionExpr });

        // Binary operators
        r.set(.PLUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
        r.set(.MINUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
        r.set(.ASTERISK, .{ .infix = binary, .precedence = .FACTOR });
        r.set(.SLASH, .{ .infix = binary, .precedence = .FACTOR });
        r.set(.MODULO, .{ .infix = binary, .precedence = .FACTOR });

        // Add compound assignment operators
        r.set(.PLUS_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });

        // Comparison operators
        r.set(.EQUALITY_SYMBOL, .{ .infix = binary, .precedence = .EQUALITY });
        r.set(.EQUALITY_KEYWORD, .{ .infix = binary, .precedence = .EQUALITY });
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
        r.set(.XOR, .{ .infix = logical, .precedence = .XOR });
        r.set(.NOT, .{ .prefix = unary, .precedence = .UNARY });

        // Unary operators
        //r.set(.BANG, .{ .prefix = unary });

        // Literals
        r.set(.INT, .{ .prefix = literal });
        r.set(.FLOAT, .{ .prefix = literal });
        r.set(.STRING, .{ .prefix = literal });
        r.set(.BOOL, .{ .prefix = literal });
        r.set(.NOTHING, .{ .prefix = literal });

        // Grouping
        r.set(.LEFT_PAREN, .{ .prefix = grouping, .infix = call, .precedence = .CALL });
        r.set(.LEFT_BRACKET, .{ .prefix = parseArrayLiteral, .infix = index, .precedence = .CALL });

        // Variables and assignment
        r.set(.VAR, .{ .prefix = variable });
        r.set(.CONST, .{ .prefix = variable });
        r.set(.IDENTIFIER, .{ .prefix = variable });
        r.set(.ASSIGN_SYMBOL, .{ .infix = assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
        r.set(.ASSIGN_KEYWORD, .{ .infix = assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
        r.set(.ARRAY_TYPE, .{ .prefix = variable, .infix = fieldAccess });

        // Control flow
        r.set(.IF, .{ .prefix = parseIfExpr });

        // Blocks
        r.set(.LEFT_BRACE, .{
            .prefix = braceExpr, // New function that handles both blocks and maps
            .precedence = .NONE,
        });

        // Add function declaration support
        r.set(.FN_KEYWORD, .{ .prefix = functionExpr });
        r.set(.FUNCTION_KEYWORD, .{ .prefix = functionExpr });

        // Add rule for the ? operator with lower precedence
        r.set(.QUESTION, .{ .infix = print, .precedence = .UNARY });

        // Add loop support
        r.set(.WHILE, .{ .prefix = whileExpr });
        r.set(.FOR, .{ .prefix = forExpr });

        // Add struct support
        r.set(.STRUCT, .{ .prefix = parseStructDecl });
        r.set(.DOT, .{ .infix = fieldAccess, .precedence = .CALL });

        // Add struct declaration rule in the rules block
        r.set(.STRUCT_TYPE, .{ .prefix = parseStructDecl });

        // Add struct instantiation support
        r.set(.IDENTIFIER, .{ .prefix = variable, .infix = fieldAccess });
        r.set(.DOT, .{ .infix = fieldAccess, .precedence = .CALL });

        // Add quantifier operators
        r.set(.EXISTS, .{ .prefix = existentialQuantifier, .precedence = .QUANTIFIER });
        r.set(.FORALL, .{ .prefix = universalQuantifier, .precedence = .QUANTIFIER });

        // Add array type support
        r.set(.ARRAY_TYPE, .{ .prefix = arrayType });

        // Add 'in' keyword support for quantifiers
        r.set(.IN, .{ .infix = inOperator, .precedence = .TERM });

        // Add array literal support
        r.set(.LEFT_BRACKET, .{ .prefix = parseArrayLiteral });

        // Add enum declaration support using the wrapper
        r.set(.ENUM_TYPE, .{ .prefix = enumDeclPrefix });

        // Add dot prefix rule for enum member access
        r.set(.DOT, .{ .prefix = enumMember, .infix = fieldAccess, .precedence = .CALL });

        // Add match expression support
        r.set(.MATCH, .{ .prefix = parseMatchExpr });

        // Add typeof support
        r.set(.TYPEOF, .{ .prefix = typeofExpr, .precedence = .CALL }); // Added precedence

        // Add tuple support
        r.set(.LEFT_PAREN, .{ .prefix = parseTuple, .infix = call, .precedence = .CALL });

        break :blk r;
    };

    fn getRule(token_type: token.TokenType) ParseRule {
        return rules.get(token_type);
    }

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, current_file: []const u8, debug_enabled: bool) Parser {
        const parser = Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .debug_enabled = debug_enabled,
            .current_file = current_file,
        };

        return parser;
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

    fn peekAhead(self: *Parser, offset: usize) token.Token {
        const pos = self.current + offset;
        if (pos >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[pos];
    }

    fn advance(self: *Parser) void {
        if (self.debug_enabled) {
            std.debug.print("Advancing from position {} to {}\n", .{
                self.current,
                self.current + 1,
            });
        }

        // Check for alternate tokens before advancing
        const current = self.peek();
        if (isAlternateToken(current.type)) {
            if (self.debug_enabled) {
                std.debug.print("Checking alternate token: {s}\n", .{@tagName(current.type)});
            }
            self.updateTokenStyle(current.type);
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

    fn parseDirective(self: *Parser) ErrorList!void {
        const current = self.peek();
        if (current.type == .HASH) {
            self.advance(); // consume #
            if (self.peek().type == .IDENTIFIER) {
                // if identifer, do something for custom doxas, ect. ect.
                // this wont be added for a long time so don't wait around
            }
            if (self.peek().type == .SAFE) {
                self.mode = .Safe;
                if (self.debug_enabled) {
                    std.debug.print("Safe mode enabled\n", .{});
                }
                self.advance(); // consume directive name
                return;
            }
            return error.InvalidDirective;
        }
    }

    fn isAlternateToken(token_type: token.TokenType) bool {
        return switch (token_type) {
            .ASSIGN_SYMBOL, .ASSIGN_KEYWORD, .EQUALITY_SYMBOL, .EQUALITY_KEYWORD, .AND_SYMBOL, .AND_KEYWORD, .OR_SYMBOL, .OR_KEYWORD, .WHERE_SYMBOL, .WHERE_KEYWORD, .FN_KEYWORD, .FUNCTION_KEYWORD => true,
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

    fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
        const is_const = self.peek().type == .CONST;
        self.advance(); // consume 'var' or 'const'

        if (self.debug_enabled) {
            std.debug.print("\nParsing var declaration\n", .{});
            std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
        }

        var type_info: ast.TypeInfo = .{
            .base = .Dynamic,
            .is_dynamic = true, // Start as dynamic
            .is_mutable = !is_const, // const variables can't change value
        };

        // Special handling for array type declarations
        if (self.peek().type == .ARRAY_TYPE) {
            type_info.base = .Array;
            type_info.is_dynamic = false;
            self.advance(); // consume 'array'

            // Create implicit name for array
            const name = token.Token{
                .type = .IDENTIFIER,
                .lexeme = "array",
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            };

            var initializer: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
                self.advance();
                if (self.debug_enabled) {
                    std.debug.print("\nParsing array initializer at position {}, token: {s}\n", .{
                        self.current,
                        @tagName(self.peek().type),
                    });
                }

                initializer = try self.parseExpression();
                if (initializer == null) return error.ExpectedExpression;
            }

            if (self.peek().type != .SEMICOLON) {
                if (self.debug_enabled) {
                    std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
                }
                return error.ExpectedSemicolon;
            }
            self.advance();

            return ast.Stmt{ .VarDecl = .{
                .name = name,
                .type_info = type_info,
                .initializer = initializer,
            } };
        }

        // Original variable declaration parsing logic
        const name = if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        } else blk: {
            const n = self.peek();
            self.advance();
            break :blk n;
        };

        if (self.debug_enabled) {
            std.debug.print("After name, current token: {s}\n", .{@tagName(self.peek().type)});
        }

        // Handle type annotation
        if (self.peek().type == .COLON) {
            self.advance(); // consume ':'
            type_info.is_dynamic = false; // Explicitly typed variables are not dynamic

            type_info.base = switch (self.peek().type) {
                .INT_TYPE => .Int,
                .FLOAT_TYPE => .Float,
                .STRING_TYPE => .String,
                .BOOLEAN_TYPE => .Boolean,
                .IDENTIFIER => blk: {
                    type_info.custom_type = self.peek().lexeme;
                    break :blk .Custom;
                },
                else => return error.ExpectedType,
            };
            self.advance(); // consume type identifier

            // Check for array type with [] syntax
            while (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume '['
                if (self.peek().type != .RIGHT_BRACKET) {
                    return error.ExpectedRightBracket;
                }
                self.advance(); // consume ']'

                // Wrap the current type in an array type
                type_info = ast.TypeInfo{
                    .base = .Array,
                    .element_type = type_info.base,
                    .is_dynamic = false,
                    .is_mutable = !is_const,
                };
            }
        } else if (self.mode == .Safe) {
            return error.MissingTypeAnnotation;
        }

        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
            self.advance();
            if (self.debug_enabled) {
                std.debug.print("\nParsing var initializer at position {}, token: {s}\n", .{
                    self.current,
                    @tagName(self.peek().type),
                });
            }

            // Try parsing struct initialization first
            if (self.peek().type == .IDENTIFIER) {
                // First try struct initialization
                if (try self.parseStructInit()) |struct_init| {
                    initializer = struct_init;
                } else {
                    // If not a struct init, try regular expression
                    initializer = try self.parseExpression();
                }
            } else {
                initializer = try self.parseExpression();
            }
            if (initializer == null) return error.ExpectedExpression;
        }

        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{ .VarDecl = .{
            .name = name,
            .type_info = type_info,
            .initializer = initializer,
        } };
    }

    fn parseArrayLiteral(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing array literal\n", .{});
        }

        self.advance(); // consume '['

        var elements = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (elements.items) |element| {
                element.deinit(self.allocator);
                self.allocator.destroy(element);
            }
            elements.deinit();
        }

        // Parse first element to establish the type
        if (self.peek().type != .RIGHT_BRACKET) {
            const first_element = try self.parseExpression() orelse return error.ExpectedExpression;
            try elements.append(first_element);

            // Parse remaining elements without type checking
            while (self.peek().type == .COMMA) {
                self.advance(); // consume comma
                if (self.peek().type == .RIGHT_BRACKET) break;

                const element = try self.parseExpression() orelse return error.ExpectedExpression;
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

    fn parseExpressionStmt(self: *Parser) ErrorList!ast.Stmt {
        if (self.debug_enabled) {
            std.debug.print("\nParsing expression statement...\n", .{});
        }

        // Special handling for variable declarations
        if (self.peek().type == .VAR) {
            return self.parseVarDecl(); // Handle var declarations separately
        }

        const expr = try self.parseExpression();

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

    fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing expression...\n", .{});
        }

        // Special handling for array type expressions
        if (self.peek().type == .ARRAY_TYPE) {
            self.advance(); // consume 'array'

            // Create array variable expression
            const array_expr = try self.allocator.create(ast.Expr);
            array_expr.* = .{ .Variable = .{
                .type = .IDENTIFIER,
                .lexeme = "array",
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            } };

            // Check if this is an array indexing operation
            if (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume '['
                return self.index(array_expr, .NONE);
            }

            return array_expr;
        }

        // Start parsing at lowest precedence for other expressions
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

        // Add specific check for BANG token
        if (self.peek().type == .BANG) {
            return error.BangNegationNotSupported; // New error type
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

            // Don't advance here for function calls or indexing operations
            if (self.peek().type != .LEFT_PAREN and self.peek().type != .LEFT_BRACKET) {
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

        const expr = switch (current.type) {
            .INT, .FLOAT, .BOOL, .NOTHING => blk: {
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

        // Parse arguments
        while (true) {
            var arg: *ast.Expr = undefined;
            if (self.peek().type == .TILDE) {
                // Handle default argument placeholder
                self.advance(); // consume ~
                const placeholder = try self.allocator.create(ast.Expr);
                placeholder.* = .DefaultArgPlaceholder; // Added semicolon here
                arg = placeholder;
            } else {
                arg = try self.parseExpression() orelse return error.ExpectedExpression;
            }
            try arguments.append(arg);

            if (self.peek().type == .RIGHT_PAREN) break;
            if (self.peek().type != .COMMA) return error.ExpectedComma;
            self.advance(); // consume comma
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
        if (self.debug_enabled) {
            std.debug.print("\nParsing assignment...\n", .{});
            std.debug.print("Current token: {s} at position {}\n", .{
                @tagName(self.peek().type),
                self.current,
            });
        }

        if (left == null) return error.ExpectedExpression;

        // Handle other expressions
        const value = try self.parseExpression() orelse return error.ExpectedExpression;

        // Check if left side is a valid assignment target
        switch (left.?.*) {
            .Variable => |name| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .Assignment = .{
                    .name = name,
                    .value = value,
                } };
                return assign;
            },
            .FieldAccess => |field_access| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .FieldAssignment = .{
                    .object = field_access.object,
                    .field = field_access.field,
                    .value = value,
                } };
                return assign;
            },
            .Index => |index_expr| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .IndexAssign = .{
                    .array = index_expr.array,
                    .index = index_expr.index,
                    .value = value,
                } };
                return assign;
            },
            else => return error.InvalidAssignmentTarget,
        }
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
        self.advance();

        // Create variable expression
        const var_expr = try self.allocator.create(ast.Expr);
        var_expr.* = .{ .Variable = name };

        // Check for indexing operation
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume '['
            return self.index(var_expr, .NONE);
        }

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

        const last_expr: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Special handling for return statements
            if (self.peek().type == .RETURN) {
                const return_stmt = try self.parseReturnStmt();
                try statements.append(return_stmt);
                break; // Exit after return statement
            }

            // Parse regular statement
            const stmt = try self.parseExpressionStmt();

            // Only append non-null expression statements
            switch (stmt) {
                .Expression => |expr| {
                    if (expr != null) {
                        try statements.append(stmt);
                    }
                },
                else => try statements.append(stmt),
            }

            // Don't break on semicolon before right brace
            if (self.peek().type == .RIGHT_BRACE) {
                break;
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
        const type_name = self.peek().lexeme;
        const type_token = self.peek();

        const basic_type = if (std.mem.eql(u8, type_name, "int"))
            ast.BasicType.Integer
        else if (std.mem.eql(u8, type_name, "float"))
            ast.BasicType.Float
        else if (std.mem.eql(u8, type_name, "string"))
            ast.BasicType.String
        else if (std.mem.eql(u8, type_name, "bool"))
            ast.BasicType.Boolean
        else if (std.mem.eql(u8, type_name, "auto"))
            ast.BasicType.Auto
        else
            null;

        self.advance(); // Always advance past the type token

        const type_expr = try self.allocator.create(ast.TypeExpr);
        if (basic_type) |basic| {
            type_expr.* = .{ .Basic = basic };
        } else {
            // Handle custom type (like struct names)
            type_expr.* = .{ .Custom = type_token };
        }

        return type_expr;
    }

    fn check(self: *Parser, token_type: token.TokenType) bool {
        return self.peek().type == token_type;
    }

    fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
        var is_entry = false;

        // Check for entry point marker
        if (self.peek().type == .MAIN) {
            is_entry = true;

            // Check if we already have an entry point
            if (self.has_entry_point) {
                return error.MultipleEntryPoints;
            }

            self.has_entry_point = true;
            self.entry_point_location = self.peek();
            self.advance(); // consume ->
        }

        // Expect fn or function keyword
        if (self.peek().type != .FN_KEYWORD and self.peek().type != .FUNCTION_KEYWORD) {
            return error.ExpectedFunction;
        }
        self.advance(); // consume fn/function keyword

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();

        // If this is an entry point, verify it's named 'main'
        if (is_entry and !std.mem.eql(u8, name.lexeme, "main")) {
            return error.EntryPointMustBeMain;
        }

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
            try self.parseParameters(&params);
        }

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();

        // Parse return type
        var return_type = ast.TypeInfo{ .base = .Nothing }; // Default to nothing
        var has_return_type = false;

        if (self.peek().type == .RETURNS) {
            has_return_type = true;
            self.advance();
            if (self.peek().type == .LEFT_PAREN) {
                self.advance(); // consume (

                // Parse return type
                const type_name = self.peek();
                self.advance();
                return_type.base = switch (type_name.type) {
                    .INT_TYPE => .Int,
                    .FLOAT_TYPE => .Float,
                    .STRING_TYPE => .String,
                    .BOOLEAN_TYPE => .Boolean,
                    else => return error.InvalidType,
                };

                if (self.peek().type != .RIGHT_PAREN) {
                    return error.ExpectedRightParen;
                }
                self.advance();
            } else {
                return error.ExpectedLeftParen; // In Safe Mode, must use returns(type) syntax
            }
        }

        if (self.mode == .Safe) {
            // In safe mode:
            // 1. All parameters must have types
            for (params.items) |param| {
                if (param.type_expr == null) {
                    return error.MissingParameterType;
                }
            }

            // 2. If function has any return statements with values, must use returns(type)
            if ((try self.hasReturnWithValue()) and !has_return_type) {
                return error.MissingReturnType;
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
            .return_type_info = return_type,
            .body = body,
            .is_entry = is_entry,
        } };
    }

    fn hasReturnWithValue(self: *Parser) !bool {
        // Find the opening brace of the function body
        var pos = self.current;
        while (pos < self.tokens.len and self.tokens[pos].type != .LEFT_BRACE) {
            pos += 1;
        }
        if (pos >= self.tokens.len) return false;

        // Start scanning after the opening brace
        pos += 1;
        var brace_count: usize = 1;
        var found_return_value = false;

        if (self.debug_enabled) {
            std.debug.print("Scanning for return values starting at position {}\n", .{pos});
        }

        while (pos < self.tokens.len) {
            const current_token = self.tokens[pos];

            if (self.debug_enabled) {
                std.debug.print("Scanning token: {s} at position {}\n", .{ @tagName(current_token.type), pos });
            }

            switch (current_token.type) {
                .LEFT_BRACE => {
                    brace_count += 1;
                },
                .RIGHT_BRACE => {
                    brace_count -= 1;
                    if (brace_count == 0) break;
                },
                .RETURN => {
                    // Check next token
                    if (pos + 1 < self.tokens.len) {
                        const next_token = self.tokens[pos + 1];
                        if (next_token.type != .SEMICOLON) {
                            found_return_value = true;
                            break;
                        }
                    }
                },
                else => {},
            }

            pos += 1;
        }

        if (self.debug_enabled) {
            std.debug.print("Scan complete. Found return value: {}\n", .{found_return_value});
        }

        return found_return_value;
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

            // Get the type of the first element
            const first_type = switch (first_element.*) {
                .Literal => |lit| @as(token.TokenType, switch (lit) {
                    .int => .INT_TYPE,
                    .float => .FLOAT_TYPE,
                    .string => .STRING_TYPE,
                    .boolean => .BOOLEAN_TYPE,
                    .nothing => .NOTHING,
                    .array => .ARRAY_TYPE,
                    .tuple => .TUPLE_TYPE,
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

                const element = try self.parseExpression() orelse return error.ExpectedExpression;

                // Check if the new element matches the type of the first element
                const element_type = switch (element.*) {
                    .Literal => |lit| @as(token.TokenType, switch (lit) {
                        .int => .INT_TYPE,
                        .float => .FLOAT_TYPE,
                        .string => .STRING_TYPE,
                        .boolean => .BOOLEAN_TYPE,
                        .nothing => .NOTHING,
                        .array => .ARRAY_TYPE,
                        .tuple => .TUPLE_TYPE,
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

    fn index(self: *Parser, array_expr: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("Parsing array/tuple index\n", .{});
        }

        // Parse the index expression
        const index_expr = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;

        // Check for and consume the RIGHT_BRACKET
        if (self.peek().type != .RIGHT_BRACKET) {
            index_expr.deinit(self.allocator);
            self.allocator.destroy(index_expr);
            return error.ExpectedRightBracket;
        }
        self.advance(); // consume ]

        // Check if this is an assignment
        if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
            self.advance(); // consume =
            const value = try self.parsePrecedence(.NONE) orelse return error.ExpectedExpression;

            const expr = try self.allocator.create(ast.Expr);
            expr.* = .{ .IndexAssign = .{
                .array = array_expr.?,
                .index = index_expr,
                .value = value,
            } };
            return expr;
        }

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{ .Index = .{
            .array = array_expr.?,
            .index = index_expr,
        } };

        // Check for another index operation (for nested access)
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume [
            return self.index(expr, .NONE);
        }

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

    fn functionExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        var is_entry = false;

        // Check if this is an entry point
        if (self.peek().type == .MAIN) {
            is_entry = true;

            // Check if we already have an entry point
            if (self.has_entry_point) {
                return error.MultipleEntryPoints;
            }

            self.has_entry_point = true;
            self.entry_point_location = self.peek();
            self.advance(); // consume ->
        }

        // Expect fn or function keyword
        if (self.peek().type != .FN_KEYWORD and self.peek().type != .FUNCTION_KEYWORD) {
            return error.ExpectedFunction;
        }
        self.advance(); // consume fn/function keyword

        // Parse function name
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();

        // If this is an entry point, verify it's named 'main'
        if (is_entry and !std.mem.eql(u8, name.lexeme, "main")) {
            return error.EntryPointMustBeMain;
        }

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
            try self.parseParameters(&params);
        }

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();

        // Parse return type
        var return_type = ast.TypeInfo{ .base = .Nothing }; // Default to nothing
        var has_return_type = false;

        if (self.peek().type == .RETURNS) {
            has_return_type = true;
            self.advance();
            if (self.peek().type == .LEFT_PAREN) {
                self.advance(); // consume (

                // Parse return type
                const type_name = self.peek();
                self.advance();
                return_type.base = switch (type_name.type) {
                    .INT_TYPE => .Int,
                    .FLOAT_TYPE => .Float,
                    .STRING_TYPE => .String,
                    .BOOLEAN_TYPE => .Boolean,
                    else => return error.InvalidType,
                };

                if (self.peek().type != .RIGHT_PAREN) {
                    return error.ExpectedRightParen;
                }
                self.advance();
            } else {
                return error.ExpectedLeftParen; // In Safe Mode, must use returns(type) syntax
            }
        }

        if (self.mode == .Safe) {
            // In safe mode:
            // 1. All parameters must have types
            for (params.items) |param| {
                if (param.type_expr == null) {
                    return error.MissingParameterType;
                }
            }

            // 2. If function has any return statements with values, must use returns(type)
            if ((try self.hasReturnWithValue()) and !has_return_type) {
                return error.MissingReturnType;
            }
        }

        // Parse function body
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }

        const body = try self.parseBlockStmt();

        const function = try self.allocator.create(ast.Expr);
        function.* = .{ .Function = .{
            .name = name,
            .params = try params.toOwnedSlice(),
            .return_type_info = return_type,
            .body = body,
            .is_entry = is_entry,
        } };
        return function;
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
                    // I have no idea why this is necessary, but it is.
                    .line = @divTrunc(self.peek().line + 1, 2),
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

            // Parse type annotation if present
            var type_expr: ?*ast.TypeExpr = null;
            if (self.peek().type == .COLON) {
                self.advance(); // consume :
                type_expr = try self.parseTypeExpr();
            }

            // Handle default value
            var default_value: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
                self.advance(); // consume =
                const value_expr = try self.parseExpression() orelse return error.ExpectedExpression;
                const default_literal = try self.allocator.create(ast.Expr);
                default_literal.* = .{ .Literal = value_expr.Literal };
                default_value = default_literal;
            }

            try params.append(.{
                .name = param_name,
                .type_expr = type_expr,
                .default_value = default_value,
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
            .STRUCT_TYPE => if (self.peekAhead(1).type == .LEFT_BRACE)
                try self.parseStructDeclStmt()
            else
                try self.parseExpressionStmt(),
            .ENUM_TYPE => self.parseEnumDecl(),
            .TRY => try self.parseTryStmt(),
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

    fn parseStructDeclStmt(self: *Parser) ErrorList!ast.Stmt {
        const expr = try self.parseStructDecl(null, .NONE) orelse return error.InvalidExpression;
        return ast.Stmt{ .Expression = expr };
    }

    fn parseStructDecl(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        // Consume 'struct' keyword
        self.advance();

        // Parse struct name
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();
        self.advance();

        // Expect opening brace
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        // Parse fields
        var fields = std.ArrayList(*ast.StructField).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                field.deinit(self.allocator);
                self.allocator.destroy(field);
            }
            fields.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE) {
            // Parse field name
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const field_name = self.peek();
            self.advance();

            // Expect colon
            if (self.peek().type != .COLON) {
                return error.ExpectedColon;
            }
            self.advance();

            // Parse field type
            const type_expr = try self.parseTypeExpr() orelse return error.ExpectedType;

            // Create field
            const field = try self.allocator.create(ast.StructField);
            field.* = .{
                .name = field_name,
                .type_expr = type_expr,
            };
            try fields.append(field);

            // Handle field separator (comma)
            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow trailing comma by checking for closing brace
                if (self.peek().type == .RIGHT_BRACE) {
                    break;
                }
            } else if (self.peek().type != .RIGHT_BRACE) {
                return error.ExpectedCommaOrBrace;
            }
        }

        self.advance(); // consume right brace

        const struct_expr = try self.allocator.create(ast.Expr);
        struct_expr.* = .{ .StructDecl = .{
            .name = name,
            .fields = try fields.toOwnedSlice(),
        } };

        return struct_expr;
    }

    fn fieldAccess(self: *Parser, object: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (object == null) return error.ExpectedExpression;

        // The dot has already been consumed by the time we get here
        // Don't advance again

        // Parse the field name
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const field = self.peek();
        self.advance();

        const field_access = try self.allocator.create(ast.Expr);
        field_access.* = .{ .FieldAccess = .{
            .object = object.?,
            .field = field,
        } };
        return field_access;
    }

    fn parseStructInit(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nTrying to parse struct init at position {}, token: {s}\n", .{
                self.current,
                @tagName(self.peek().type),
            });
        }

        const struct_name = self.peek();
        const start_pos = self.current;
        self.advance();

        if (self.peek().type != .LEFT_BRACE) {
            if (self.debug_enabled) {
                std.debug.print("No left brace found, resetting to position {}\n", .{start_pos});
            }
            // Reset position if this isn't a struct initialization
            self.current = start_pos;
            return null;
        }
        self.advance(); // consume {

        var fields = std.ArrayList(*ast.StructInstanceField).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                field.deinit(self.allocator);
                self.allocator.destroy(field);
            }
            fields.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE) {
            if (self.debug_enabled) {
                std.debug.print("Parsing field at position {}, token: {s}\n", .{
                    self.current,
                    @tagName(self.peek().type),
                });
            }

            // Parse field name
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const field_name = self.peek();
            self.advance();

            // Expect equals
            if (self.peek().type != .ASSIGN_SYMBOL and self.peek().type != .ASSIGN_KEYWORD) {
                return error.ExpectedEquals;
            }
            self.advance();

            // Parse field value - try struct init first, then fall back to regular expression
            var value: *ast.Expr = undefined;
            if (self.peek().type == .IDENTIFIER) {
                if (self.debug_enabled) {
                    std.debug.print("Attempting nested struct init for identifier: {s}\n", .{
                        self.peek().lexeme,
                    });
                }
                if (try self.parseStructInit()) |struct_init| {
                    value = struct_init;
                } else {
                    if (self.debug_enabled) {
                        std.debug.print("Not a struct init, falling back to expression\n", .{});
                    }
                    value = try self.parseExpression() orelse return error.ExpectedExpression;
                }
            } else {
                value = try self.parseExpression() orelse return error.ExpectedExpression;
            }

            // Create field
            const field = try self.allocator.create(ast.StructInstanceField);
            field.* = .{
                .name = field_name,
                .value = value,
            };
            try fields.append(field);

            // Handle comma
            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow trailing comma by checking for closing brace
                if (self.peek().type == .RIGHT_BRACE) {
                    break;
                }
            } else if (self.peek().type != .RIGHT_BRACE) {
                if (self.debug_enabled) {
                    std.debug.print("Expected comma or brace, got: {s}\n", .{
                        @tagName(self.peek().type),
                    });
                }
                return error.ExpectedCommaOrBrace;
            }
        }

        self.advance(); // consume }

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed struct init for {s}\n", .{struct_name.lexeme});
        }

        const struct_init = try self.allocator.create(ast.Expr);
        struct_init.* = .{ .StructLiteral = .{
            .name = struct_name,
            .fields = try fields.toOwnedSlice(),
        } };
        return struct_init;
    }

    fn existentialQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing existential quantifier...\n", .{});
        }

        self.advance(); // consume 'exists'

        // Parse the bound variable
        if (self.peek().type != .IDENTIFIER) {
            if (self.debug_enabled) {
                std.debug.print("Expected identifier, found: {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedIdentifier;
        }
        const bound_variable = self.peek();
        self.advance();

        // Parse 'in' keyword
        if (self.peek().type != .IN) {
            return error.ExpectedInKeyword;
        }
        self.advance();

        // Parse array expression (can be identifier or array type)
        if (self.peek().type == .ARRAY_TYPE) {
            // Create array type expression
            self.advance(); // consume ARRAY_TYPE
            const array_expr = try self.allocator.create(ast.Expr);
            array_expr.* = .{ .Variable = .{
                .lexeme = "array",
                .type = .IDENTIFIER,
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            } };

            if (self.debug_enabled) {
                std.debug.print("After array type, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
            }

            // Parse 'where' keyword
            if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedWhereKeyword;
            }
            self.advance();

            // Parse the condition
            const condition = try self.parseExpression() orelse {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedExpression;
            };

            const exists_expr = try self.allocator.create(ast.Expr);
            exists_expr.* = .{ .Exists = .{
                .variable = bound_variable,
                .array = array_expr,
                .condition = condition,
            } };
            return exists_expr;
        } else {
            // Parse regular expression for array
            const array_expr = try self.parseExpression() orelse return error.ExpectedExpression;

            if (self.debug_enabled) {
                std.debug.print("After array expression, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
            }

            // Parse 'where' keyword
            if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedWhereKeyword;
            }
            self.advance();

            // Parse the condition
            const condition = try self.parseExpression() orelse {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedExpression;
            };

            const exists_expr = try self.allocator.create(ast.Expr);
            exists_expr.* = .{ .Exists = .{
                .variable = bound_variable,
                .array = array_expr,
                .condition = condition,
            } };
            return exists_expr;
        }
    }

    fn universalQuantifier(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing universal quantifier...\n", .{});
        }

        self.advance(); // consume 'forall'

        // Parse the bound variable
        if (self.peek().type != .IDENTIFIER) {
            if (self.debug_enabled) {
                std.debug.print("Expected identifier, found: {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedIdentifier;
        }
        const bound_variable = self.peek();
        self.advance();

        // Parse 'in' keyword
        if (self.peek().type != .IN) {
            return error.ExpectedInKeyword;
        }
        self.advance();

        // Parse array expression (can be identifier or array type)
        if (self.peek().type == .ARRAY_TYPE) {
            // Create array type expression
            self.advance(); // consume ARRAY_TYPE
            const array_expr = try self.allocator.create(ast.Expr);
            array_expr.* = .{ .Variable = .{
                .lexeme = "array",
                .type = .IDENTIFIER,
                .literal = .{ .nothing = {} },
                .line = 0,
                .column = 0,
            } };

            if (self.debug_enabled) {
                std.debug.print("After array type, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
            }

            // Parse 'where' keyword
            if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedWhereKeyword;
            }
            self.advance();

            // Parse the condition
            const condition = try self.parseExpression() orelse {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedExpression;
            };

            const forall_expr = try self.allocator.create(ast.Expr);
            forall_expr.* = .{ .ForAll = .{
                .variable = bound_variable,
                .array = array_expr,
                .condition = condition,
            } };
            return forall_expr;
        } else {
            // Parse regular expression for array
            const array_expr = try self.parseExpression() orelse return error.ExpectedExpression;

            if (self.debug_enabled) {
                std.debug.print("After array expression, expecting WHERE, found: {s}\n", .{@tagName(self.peek().type)});
            }

            // Parse 'where' keyword
            if (self.peek().type != .WHERE_KEYWORD and self.peek().type != .WHERE_SYMBOL) {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedWhereKeyword;
            }
            self.advance();

            // Parse the condition
            const condition = try self.parseExpression() orelse {
                array_expr.deinit(self.allocator);
                self.allocator.destroy(array_expr);
                return error.ExpectedExpression;
            };

            const forall_expr = try self.allocator.create(ast.Expr);
            forall_expr.* = .{ .ForAll = .{
                .variable = bound_variable,
                .array = array_expr,
                .condition = condition,
            } };
            return forall_expr;
        }
    }

    fn inOperator(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;

        // Parse the array expression
        const array_expr = try self.parsePrecedence(@enumFromInt(@intFromEnum(precedence) + 1)) orelse
            return error.ExpectedArrayExpression;

        const in_expr = try self.allocator.create(ast.Expr);
        in_expr.* = .{ .Binary = .{
            .left = left,
            .operator = self.tokens[self.current - 1],
            .right = array_expr,
        } };
        return in_expr;
    }

    fn arrayType(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{ .ArrayType = .{
            .element_type = null,
        } };
        return array_expr;
    }

    fn parseEnumDecl(self: *Parser) ErrorList!ast.Stmt {
        self.advance(); // consume 'enum' keyword

        // Parse enum name
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const name = self.peek();
        self.advance();

        // Parse opening brace
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        // Parse variants
        var variants = std.ArrayList(token.Token).init(self.allocator);
        errdefer variants.deinit();

        while (self.peek().type != .RIGHT_BRACE) {
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            try variants.append(self.peek());
            self.advance();

            if (self.peek().type == .COMMA) {
                self.advance();
            }
        }
        self.advance(); // consume right brace

        return ast.Stmt{ .EnumDecl = .{
            .name = name,
            .variants = try variants.toOwnedSlice(),
        } };
    }

    fn parseMatchExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing match expression...\n", .{});
        }

        self.advance(); // consume 'match' keyword

        // Parse the value to match on
        const value = try self.parseExpression() orelse return error.ExpectedExpression;

        // Expect opening brace
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        // Parse match cases
        var cases = std.ArrayList(ast.MatchCase).init(self.allocator);
        errdefer cases.deinit();

        var has_else_case = false;

        while (self.peek().type != .RIGHT_BRACE) {
            if (self.peek().type == .ELSE) {
                // Handle else case
                self.advance(); // consume 'else'

                // Parse arrow
                if (self.peek().type != .ARROW) {
                    return error.ExpectedArrow;
                }
                self.advance();

                // Parse body expression
                const body = try self.parseExpression() orelse return error.ExpectedExpression;

                try cases.append(.{
                    .pattern = token.Token{
                        .type = .ELSE,
                        .lexeme = "else",
                        .literal = .{ .nothing = {} },
                        .line = 0,
                        .column = 0,
                    },
                    .body = body,
                });

                has_else_case = true;

                // Handle optional comma
                if (self.peek().type == .COMMA) {
                    self.advance();
                }
                continue;
            }

            // Parse pattern (enum variant)
            if (self.peek().type != .DOT) {
                return error.ExpectedDot;
            }
            self.advance();

            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const pattern = self.peek();
            self.advance();

            // Parse arrow
            if (self.peek().type != .ARROW) {
                return error.ExpectedArrow;
            }
            self.advance();

            // Parse body expression
            const body = try self.parseExpression() orelse return error.ExpectedExpression;

            try cases.append(.{
                .pattern = pattern,
                .body = body,
            });

            // Handle optional comma
            if (self.peek().type == .COMMA) {
                self.advance();
            }
        }
        self.advance(); // consume right brace

        // If there's no else case, we need to verify that all variants are covered
        if (!has_else_case) {
            // Get the enum type from the value expression
            const enum_type = switch (value.*) {
                .Variable => |v| v.lexeme,
                .EnumMember => |m| m.lexeme,
                else => return error.ExpectedEnumValue,
            };

            _ = enum_type;

            // Find the enum declaration in the current scope
            // This would require tracking declarations in scope, which might need additional infrastructure
            // For now, we'll just enforce that the number of cases matches the number of variants
            // A more complete solution would verify each specific variant is covered

            // TODO: Add proper enum variant tracking
            // For now, just ensure we have at least one case
            if (cases.items.len == 0) {
                return error.EmptyMatch;
            }
        }

        const match_expr = try self.allocator.create(ast.Expr);
        match_expr.* = .{ .Match = .{
            .value = value,
            .cases = try cases.toOwnedSlice(),
        } };
        return match_expr;
    }

    // Update the wrapper function to properly construct the enum declaration
    fn enumDeclPrefix(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        const enum_stmt = try self.parseEnumDecl();
        const expr = try self.allocator.create(ast.Expr);

        // Create a mutable copy of the variants array
        const variants = try self.allocator.alloc(token.Token, enum_stmt.EnumDecl.variants.len);
        @memcpy(variants, enum_stmt.EnumDecl.variants);

        expr.* = .{ .EnumDecl = .{
            .name = enum_stmt.EnumDecl.name,
            .variants = variants,
        } };
        return expr;
    }

    fn enumMember(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing enum member access...\n", .{});
        }

        // We're already at the dot, so advance past it
        self.advance();

        // Expect identifier after dot
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }

        const member = self.peek();
        self.advance();

        // Debug output for token scanning
        if (self.debug_enabled) {
            std.debug.print("\nLooking for enum variant '{s}'...\n", .{member.lexeme});
        }

        // Look backwards through tokens to find the enum declaration
        var found_valid_variant = false;
        var i: usize = 0; // Start from the beginning
        while (i < self.tokens.len) : (i += 1) {
            if (self.debug_enabled) {
                std.debug.print("Checking token at {}: {s} ({s})\n", .{ i, @tagName(self.tokens[i].type), self.tokens[i].lexeme });
            }

            if (self.tokens[i].type == .ENUM_TYPE) {
                if (self.debug_enabled) {
                    std.debug.print("Found enum declaration at {}\n", .{i});
                }

                // Skip enum keyword and name
                i += 2;

                // Verify we're at the opening brace
                if (self.tokens[i].type != .LEFT_BRACE) continue;
                i += 1;

                // Scan through variants
                while (i < self.tokens.len and self.tokens[i].type != .RIGHT_BRACE) : (i += 1) {
                    if (self.tokens[i].type == .IDENTIFIER) {
                        if (self.debug_enabled) {
                            std.debug.print("Checking variant: {s}\n", .{self.tokens[i].lexeme});
                        }
                        if (std.mem.eql(u8, self.tokens[i].lexeme, member.lexeme)) {
                            found_valid_variant = true;
                            if (self.debug_enabled) {
                                std.debug.print("Found matching variant!\n", .{});
                            }
                            break;
                        }
                    }
                }
                if (found_valid_variant) break;
            }
        }

        if (!found_valid_variant) {
            if (self.debug_enabled) {
                std.debug.print("Invalid enum variant: {s}\n", .{member.lexeme});
            }
            return error.InvalidEnumVariant;
        }

        const enum_member = try self.allocator.create(ast.Expr);
        enum_member.* = .{ .EnumMember = member };
        return enum_member;
    }

    fn typeofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing typeof expression...\n", .{});
        }

        // We're already at 'typeof', advance to the next token
        self.advance();

        // Expect opening parenthesis
        if (self.peek().type != .LEFT_PAREN) {
            return error.ExpectedLeftParen;
        }
        self.advance();

        // Parse the expression whose type we want to check
        const expr = try self.parseExpression() orelse return error.ExpectedExpression;

        // Expect closing parenthesis
        if (self.peek().type != .RIGHT_PAREN) {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
            return error.ExpectedRightParen;
        }
        self.advance();

        const typeof_expr = try self.allocator.create(ast.Expr);
        typeof_expr.* = .{ .TypeOf = expr };
        return typeof_expr;
    }

    fn parseTuple(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing tuple...\n", .{});
        }

        self.advance(); // consume '('

        var elements = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (elements.items) |element| {
                element.deinit(self.allocator);
                self.allocator.destroy(element);
            }
            elements.deinit();
        }

        // Parse first element
        const first = try self.parseExpression() orelse return error.ExpectedExpression;
        try elements.append(first);

        // If there's a comma, this is a tuple. Otherwise, it's just a grouped expression
        if (self.peek().type == .COMMA) {
            // Parse remaining elements
            while (true) {
                if (self.peek().type == .COMMA) {
                    self.advance(); // consume comma

                    // Allow trailing comma
                    if (self.peek().type == .RIGHT_PAREN) break;

                    const element = try self.parseExpression() orelse return error.ExpectedExpression;
                    try elements.append(element);
                } else if (self.peek().type == .RIGHT_PAREN) {
                    break;
                } else {
                    return error.ExpectedCommaOrParen;
                }
            }

            self.advance(); // consume ')'

            const tuple_expr = try self.allocator.create(ast.Expr);
            tuple_expr.* = .{ .Tuple = try elements.toOwnedSlice() };
            return tuple_expr;
        } else {
            // Not a tuple, just a grouped expression
            elements.deinit();

            if (self.peek().type != .RIGHT_PAREN) {
                first.deinit(self.allocator);
                self.allocator.destroy(first);
                return error.ExpectedRightParen;
            }
            self.advance(); // consume ')'

            const grouping_expr = try self.allocator.create(ast.Expr);
            grouping_expr.* = .{ .Grouping = first };
            return grouping_expr;
        }
    }

    fn map(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing map literal...\n", .{});
        }

        // Consume the left brace
        self.advance();

        var entries = std.ArrayList(ast.MapEntry).init(self.allocator); // Changed from *ast.MapEntry
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
            const key = try self.parseExpression() orelse return error.ExpectedExpression;

            // Expect colon
            if (self.peek().type != .COLON) {
                // If we don't see a colon, this might be a block instead
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
            const value = try self.parseExpression() orelse return error.ExpectedExpression;

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

        const map_expr = try self.allocator.create(ast.Expr);
        map_expr.* = .{ .Map = try entries.toOwnedSlice() };
        return map_expr;
    }

    fn parseTryStmt(self: *Parser) ErrorList!ast.Stmt {
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
            const stmt = try self.parseStatement();
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
            const stmt = try self.parseExpressionStmt();
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

    fn reportWarning(self: *Parser, message: []const u8) void {
        _ = self;
        var reporting = Reporting.init();
        reporting.reportWarning("{s}", .{message});
    }

    fn updateTokenStyle(self: *Parser, token_type: token.TokenType) void {
        // Runtime style determination
        var new_style: TokenStyle = .Undefined;
        if (token_type == .FUNCTION_KEYWORD or token_type == .AND_KEYWORD or token_type == .OR_KEYWORD or token_type == .ASSIGN_KEYWORD or token_type == .EQUALITY_KEYWORD or token_type == .WHERE_KEYWORD) {
            new_style = .Keyword;
        } else if (token_type == .FN_KEYWORD or token_type == .AND_SYMBOL or token_type == .OR_SYMBOL or token_type == .ASSIGN_SYMBOL or token_type == .EQUALITY_SYMBOL or token_type == .WHERE_SYMBOL) {
            new_style = .Symbol;
        } else {
            return;
        }
        if (self.debug_enabled) {
            std.debug.print("Updating token style for: {s}\n", .{@tagName(token_type)});
        }
        // Check and update styles
        switch (token_type) {
            .WHERE_KEYWORD, .WHERE_SYMBOL => {
                if (self.where_style != .Undefined and self.where_style != new_style and !self.where_style_warning_shown) {
                    self.reportWarning("Style conflict for 'where' keyword and '|' symbol");
                    self.where_style_warning_shown = true;
                    return;
                }
                if (self.where_style == .Undefined) {
                    self.where_style = new_style;
                }
            },
            .ASSIGN_KEYWORD, .ASSIGN_SYMBOL => {
                if (self.assign_style != .Undefined and self.assign_style != new_style and !self.assign_style_warning_shown) {
                    self.reportWarning("Style conflict for 'assignment' keyword and '=' symbol");
                    self.assign_style_warning_shown = true;
                    return;
                }
                if (self.assign_style == .Undefined) {
                    self.assign_style = new_style;
                }
            },
            .EQUALITY_KEYWORD, .EQUALITY_SYMBOL => {
                if (self.equality_style != .Undefined and self.equality_style != new_style and !self.equality_style_warning_shown) {
                    self.reportWarning("Style conflict for 'equality' keyword and '==' symbol");
                    self.equality_style_warning_shown = true;
                    return;
                }
                if (self.equality_style == .Undefined) {
                    self.equality_style = new_style;
                }
            },
            .FN_KEYWORD, .FUNCTION_KEYWORD => {
                if (self.fn_style != .Undefined and self.fn_style != new_style and !self.fn_style_warning_shown) {
                    self.reportWarning("Style conflict for 'fn' and 'function' keywords");
                    self.fn_style_warning_shown = true;
                    return;
                }
                if (self.fn_style == .Undefined) {
                    self.fn_style = new_style;
                }
            },
            .AND_KEYWORD, .AND_SYMBOL => {
                if (self.and_style != .Undefined and self.and_style != new_style and !self.and_style_warning_shown) {
                    self.reportWarning("Style conflict for 'and' keyword and '&&' symbol");
                    self.and_style_warning_shown = true;
                    return;
                }
                if (self.and_style == .Undefined) {
                    self.and_style = new_style;
                }
            },
            .OR_KEYWORD, .OR_SYMBOL => {
                if (self.or_style != .Undefined and self.or_style != new_style and !self.or_style_warning_shown) {
                    self.reportWarning("Style conflict for 'or' keyword and '||' symbol");
                    self.or_style_warning_shown = true;
                    return;
                }
                if (self.or_style == .Undefined) {
                    self.or_style = new_style;
                }
            },
            else => {},
        }
    }

    // Add new braceExpr function
    fn braceExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing brace expression...\n", .{});
        }

        // Consume the left brace
        self.advance();

        // Look ahead to see if this might be a map
        // A map must have a key followed by a colon
        if (self.peek().type != .RIGHT_BRACE and self.peekAhead(1).type == .COLON) {
            return self.parseMap();
        }

        // Otherwise, treat it as a block
        return self.parseBlock();
    }

    fn parseMap(self: *Parser) ErrorList!?*ast.Expr {
        var entries = std.ArrayList(ast.MapEntry).init(self.allocator);
        errdefer {
            for (entries.items) |*entry| {
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
            const key = try self.parseExpression() orelse return error.ExpectedExpression;

            // Expect colon
            if (self.peek().type != .COLON) {
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                return error.ExpectedColon;
            }
            self.advance(); // consume colon

            // Parse value
            const value = try self.parseExpression() orelse return error.ExpectedExpression;

            // Create and append entry
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

        const map_expr = try self.allocator.create(ast.Expr);
        map_expr.* = .{ .Map = try entries.toOwnedSlice() };
        return map_expr;
    }

    fn parseBlock(self: *Parser) ErrorList!?*ast.Expr {
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            const stmt = try self.parseExpressionStmt();
            try statements.append(stmt);

            // Don't break on semicolon before right brace
            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{
            .Block = .{
                .statements = try statements.toOwnedSlice(),
                .value = null, // Last expression value
            },
        };

        return block_expr;
    }
};
