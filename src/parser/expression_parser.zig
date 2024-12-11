const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("parser_types.zig").Parser;
const token = @import("../token.zig");
const precedence = @import("precedence.zig");
const Precedence = @import("precedence.zig").Precedence;
const ErrorList = @import("../reporting.zig").ErrorList;
const statement_parser = @import("./statement_parser.zig");
const declaration_parser = @import("./declaration_parser.zig");

pub fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {
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
            return Parser.index(self, array_expr, .NONE);
        }

        return array_expr;
    }

    // Start parsing at lowest precedence for other expressions
    return try precedence.parsePrecedence(self, Precedence.ASSIGNMENT);
}

pub fn binary(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;
    const operator = self.tokens[self.current - 1];
    const right = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedRightOperand;

    const binary_expr = try self.allocator.create(ast.Expr);
    binary_expr.* = .{ .Binary = .{
        .left = left,
        .operator = operator,
        .right = right,
    } };
    return binary_expr;
}

pub fn braceExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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

pub fn typeofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
    const expr = try parseExpression(self) orelse return error.ExpectedExpression;

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

pub fn parseMatchExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing match expression...\n", .{});
    }

    self.advance(); // consume 'match' keyword

    // Parse the value to match on
    const value = try parseExpression(self) orelse return error.ExpectedExpression;

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
            const body = try parseExpression(self) orelse return error.ExpectedExpression;

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
        const body = try parseExpression(self) orelse return error.ExpectedExpression;

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

pub fn whileExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing while expression...\n", .{});
    }

    self.advance(); // consume 'while'

    // Optional parentheses around condition
    const has_parens = self.peek().type == .LEFT_PAREN;
    if (has_parens) {
        self.advance();
    }

    const condition = (try parseExpression(self)) orelse return error.ExpectedExpression;

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
        const block_stmts = try statement_parser.parseBlockStmt(self);
        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{
            .Block = .{
                .statements = block_stmts,
                .value = null, // No final expression value for the block
            },
        };
        body = block_expr;
    } else {
        body = (try parseExpression(self)) orelse {
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

pub fn forExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
            const init_stmt = try declaration_parser.parseVarDecl(self);
            const stmt_ptr = try self.allocator.create(ast.Stmt);
            stmt_ptr.* = init_stmt;
            initializer = stmt_ptr;
        } else {
            const expr_stmt = try statement_parser.parseExpressionStmt(self);
            const stmt_ptr = try self.allocator.create(ast.Stmt);
            stmt_ptr.* = expr_stmt;
            initializer = stmt_ptr;
        }
    }

    // Parse condition
    var condition: ?*ast.Expr = null;
    if (self.peek().type != .SEMICOLON) {
        condition = try parseExpression(self);
    }
    if (self.peek().type != .SEMICOLON) {
        return error.ExpectedSemicolon;
    }
    self.advance();

    // Parse increment
    var increment: ?*ast.Expr = null;
    if (self.peek().type != .RIGHT_PAREN) {
        increment = try parseExpression(self);
    }
    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    // Parse body
    const body = (try parseExpression(self)) orelse return error.ExpectedExpression;

    const for_expr = try self.allocator.create(ast.Expr);
    for_expr.* = .{ .For = .{
        .initializer = initializer,
        .condition = condition,
        .increment = increment,
        .body = body,
    } };

    return for_expr;
}

pub fn parseTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
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

pub fn allocExpr(self: *Parser, expr: ast.Expr) ErrorList!*ast.Expr {
    const new_expr = try self.allocator.create(ast.Expr);
    new_expr.* = expr;
    return new_expr;
}

pub fn parseIfExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing if expression...\n", .{});
    }

    self.advance(); // consume 'if'

    // Optional parentheses around condition
    const has_parens = self.peek().type == .LEFT_PAREN;
    if (has_parens) {
        self.advance();
    }

    const condition = (try parseExpression(self)) orelse return error.ExpectedExpression;

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

    const then_expr = (try parseExpression(self)) orelse {
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
            else_expr = try parseIfExpr(self, null, .NONE);
        } else {
            else_expr = try precedence.parsePrecedence(self, .NONE);

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

pub fn functionExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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

    const body = try statement_parser.parseBlockStmt(self);

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

pub fn literal(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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

pub fn grouping(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume (

    // If we have a left expression, this is a function call
    if (left != null) {
        return self.call(left, .NONE);
    }

    // Otherwise it's just a grouped expression
    const expr = try parseExpression(self) orelse return error.ExpectedExpression;

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance(); // consume )

    const grouping_expr = try self.allocator.create(ast.Expr);
    grouping_expr.* = .{ .Grouping = expr };
    return grouping_expr;
}

pub fn unary(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("Parsing unary expression, current token: {s}\n", .{
            @tagName(self.peek().type),
        });
    }

    const operator = self.peek(); // Get the current token as operator
    self.advance(); // Move past the operator

    const right = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedExpression;

    const unary_expr = try self.allocator.create(ast.Expr);
    unary_expr.* = .{ .Unary = .{
        .operator = operator,
        .right = right,
    } };
    return unary_expr;
}

pub fn variable(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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

pub fn arrayType(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{ .ArrayType = .{
        .element_type = null,
    } };
    return array_expr;
}

pub fn parseArrayLiteral(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
        const first_element = try parseExpression(self) orelse return error.ExpectedExpression;
        try elements.append(first_element);

        // Parse remaining elements without type checking
        while (self.peek().type == .COMMA) {
            self.advance(); // consume comma
            if (self.peek().type == .RIGHT_BRACKET) break;

            const element = try parseExpression(self) orelse return error.ExpectedExpression;
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