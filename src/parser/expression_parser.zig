const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("parser_types.zig").Parser;
const token = @import("../token.zig");
const precedence = @import("precedence.zig");
const Precedence = @import("precedence.zig").Precedence;
const statement_parser = @import("./statement_parser.zig");
const declaration_parser = @import("./declaration_parser.zig");
const Reporting = @import("../reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorList = Reporting.ErrorList;

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
    // A map must have a key followed by a :
    if (self.peek().type != .RIGHT_BRACE and self.peekAhead(1).type == .WHERE) {
        return self.parseMap();
    }

    // Parse block statements
    var statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        statements.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try statement_parser.parseStatement(self);
        try statements.append(stmt);

        // Break after return statement
        if (stmt == .Return) break;
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance();

    const block_expr = try self.allocator.create(ast.Expr);
    block_expr.* = .{ .Block = .{
        .statements = try statements.toOwnedSlice(),
        .value = null,
    } };

    return block_expr;
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
            const var_decl = try declaration_parser.parseVarDecl(self);
            const stmt = try self.allocator.create(ast.Stmt);
            stmt.* = var_decl;
            initializer = stmt;
        } else {
            const expr_stmt = try statement_parser.parseExpressionStmt(self);
            const stmt = try self.allocator.create(ast.Stmt);
            stmt.* = expr_stmt;
            initializer = stmt;
        }
    } else {
        self.advance(); // consume ';'
    }

    // Parse condition
    var condition: ?*ast.Expr = null;
    if (self.peek().type != .SEMICOLON) {
        condition = try parseExpression(self);
    }
    if (self.peek().type != .SEMICOLON) {
        return error.ExpectedSemicolon;
    }
    self.advance(); // consume ';'

    // Parse increment
    var increment: ?*ast.Expr = null;
    if (self.peek().type != .RIGHT_PAREN) {
        increment = try parseExpression(self);
        // Handle ++ operator specially
        if (self.peek().type == .PLUS_PLUS) {
            if (increment) |var_expr| {
                self.advance(); // consume '++'
                // Create assignment expression: i = i + 1
                const one = try self.allocator.create(ast.Expr);
                one.* = .{ .Literal = .{ .int = 1 } };

                const add = try self.allocator.create(ast.Expr);
                add.* = .{ .Binary = .{
                    .left = var_expr,
                    .operator = .{ .type = .PLUS, .lexeme = "+", .line = self.peek().line, .column = self.peek().column, .literal = .nothing },
                    .right = one,
                } };

                const new_increment = try self.allocator.create(ast.Expr);
                new_increment.* = .{ .Assignment = .{
                    .name = var_expr.Variable,
                    .value = add,
                } };
                increment = new_increment;
            }
        } else if (self.peek().type == .MINUS_MINUS) {
            if (increment) |var_expr| {
                self.advance(); // consume '--'
                // Create assignment expression: i = i - 1
                const one = try self.allocator.create(ast.Expr);
                one.* = .{ .Literal = .{ .int = 1 } };

                const sub = try self.allocator.create(ast.Expr);
                sub.* = .{ .Binary = .{
                    .left = var_expr,
                    .operator = .{ .type = .MINUS, .lexeme = "-", .line = self.peek().line, .column = self.peek().column, .literal = .nothing },
                    .right = one,
                } };

                const new_increment = try self.allocator.create(ast.Expr);
                new_increment.* = .{ .Assignment = .{
                    .name = var_expr.Variable,
                    .value = sub,
                } };
                increment = new_increment;
            }
        }
    }

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    // Parse body
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance(); // consume '{'

    var body = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer body.deinit();

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try statement_parser.parseStatement(self);
        try body.append(stmt);
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance(); // consume '}'

    // Create block expression for body
    const block_expr = try self.allocator.create(ast.Expr);
    block_expr.* = .{ .Block = .{
        .statements = try body.toOwnedSlice(),
        .value = null,
    } };

    const for_expr = try self.allocator.create(ast.Expr);
    for_expr.* = .{
        .For = .{
            .initializer = initializer,
            .condition = condition,
            .increment = increment,
            .body = block_expr,
        },
    };

    return for_expr;
}

pub fn parseTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
    const type_token = self.peek();
    const type_name = type_token.lexeme;

    if (self.debug_enabled) {
        std.debug.print("Parsing type expression, current token: {s} ({s})\n", .{ @tagName(type_token.type), type_name });
    }

    var base_type_expr: ?*ast.TypeExpr = null;
    var consumed_token = false;

    // 1. Check for Basic Types
    const maybe_basic_type: ?ast.BasicType = blk: {
        if (std.mem.eql(u8, type_name, "int")) break :blk ast.BasicType.Integer;
        if (std.mem.eql(u8, type_name, "u8")) break :blk ast.BasicType.U8;
        if (std.mem.eql(u8, type_name, "float")) break :blk ast.BasicType.Float;
        if (std.mem.eql(u8, type_name, "string")) break :blk ast.BasicType.String;
        if (std.mem.eql(u8, type_name, "bool")) break :blk ast.BasicType.Boolean;
        if (std.mem.eql(u8, type_name, "tetra")) break :blk ast.BasicType.Tetra;
        if (std.mem.eql(u8, type_name, "auto") or std.mem.eql(u8, type_name, "")) break :blk ast.BasicType.Auto;
        // Not a basic type keyword
        break :blk null;
    };

    if (maybe_basic_type) |basic| {
        // It's a basic type
        if (self.debug_enabled) {
            std.debug.print("Recognized basic type: {s}\n", .{@tagName(basic)});
        }
        self.advance(); // Consume the basic type keyword token
        consumed_token = true;
        base_type_expr = try self.allocator.create(ast.TypeExpr);
        base_type_expr.?.* = .{ .Basic = basic };
    } else if (type_token.type == .STRUCT_TYPE) {
        // 2. Check for Struct Type Syntax
        if (self.debug_enabled) {
            std.debug.print("Recognized struct type syntax\n", .{});
        }
        self.advance(); // consume 'struct'
        consumed_token = true;

        if (self.peek().type != .LEFT_BRACE) return error.ExpectedLeftBrace;
        self.advance(); // consume {

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
            if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
            const field_name = self.peek();
            self.advance();
            // Expect :
            if (self.peek().type != .WHERE) return error.ExpectedColon;
            self.advance();
            // Parse field type
            const field_type = try parseTypeExpr(self) orelse return error.ExpectedType;
            // Create field
            const field = try self.allocator.create(ast.StructField);
            field.* = .{ .name = field_name, .type_expr = field_type };
            try fields.append(field);
            // Handle separator
            if (self.peek().type == .COMMA) {
                self.advance();
                if (self.peek().type == .RIGHT_BRACE) break;
            } else if (self.peek().type != .RIGHT_BRACE) {
                return error.ExpectedCommaOrBrace;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) return error.ExpectedRightBrace; // Should be caught by loop condition
        self.advance(); // consume }

        base_type_expr = try self.allocator.create(ast.TypeExpr);
        base_type_expr.?.* = .{ .Struct = try fields.toOwnedSlice() };
    } else if (type_token.type == .IDENTIFIER) {
        // 3. Check for Declared or Imported Custom Types
        var is_known_custom = false;
        if (self.declared_types.contains(type_name)) {
            is_known_custom = true;
            if (self.debug_enabled) {
                std.debug.print("Recognized locally declared type: {s}\n", .{type_name});
            }
        } else if (self.imported_symbols) |symbols| {
            var it = symbols.iterator();
            while (it.next()) |entry| {
                const symbol = entry.value_ptr.*;
                if ((symbol.kind == .Enum or symbol.kind == .Struct or symbol.kind == .Type) and
                    std.mem.eql(u8, symbol.name, type_name))
                {
                    is_known_custom = true;
                    if (self.debug_enabled) {
                        std.debug.print("Recognized imported type: {s}\n", .{type_name});
                    }
                    break;
                }
            }
        }

        if (is_known_custom) {
            self.advance(); // Consume the identifier token
            consumed_token = true;
            base_type_expr = try self.allocator.create(ast.TypeExpr);
            base_type_expr.?.* = .{ .Custom = type_token };
        } else {
            // 4. Unknown Identifier -> Error
            if (self.debug_enabled) {
                std.debug.print("Unknown type identifier: {s}\n", .{type_name});
            }
            var reporter = Reporter.init();
            reporter.reportCompileError(.{ .file = self.current_file, .line = type_token.line, .column = type_token.column }, "Unknown type: '{s}'", .{type_name});
            return error.UnknownType;
        }
    } else {
        // 5. Not a basic type, not struct syntax, not an identifier -> Error
        if (self.debug_enabled) {
            std.debug.print("Invalid token for type expression: {s}\n", .{@tagName(type_token.type)});
        }
        var reporter = Reporter.init();
        reporter.reportCompileError(.{ .file = self.current_file, .line = type_token.line, .column = type_token.column }, "Expected type identifier, found {s}", .{@tagName(type_token.type)});
        return error.ExpectedType;
    }

    // Make sure we consumed a token if we successfully identified a base type
    if (!consumed_token and base_type_expr != null) {
        // This case should ideally not happen with the logic above
        return error.InternalParserError;
    }
    // If base_type_expr is still null here, something went wrong or an error should have been returned
    if (base_type_expr == null) return error.ExpectedType;

    // --- Check for Array Type ---
    if (self.peek().type == .LEFT_BRACKET) {
        if (self.debug_enabled) {
            std.debug.print("Found array brackets after base type\n", .{});
        }
        self.advance(); // consume [

        var size: ?*ast.Expr = null;
        if (self.peek().type == .INT) {
            const size_expr = try self.allocator.create(ast.Expr);
            size_expr.* = .{ .Literal = self.peek().literal };
            size = size_expr;
            self.advance(); // consume the integer
        }

        if (self.peek().type != .RIGHT_BRACKET) {
            // Cleanup size expr if allocated
            if (size) |s| {
                s.deinit(self.allocator);
                self.allocator.destroy(s);
            }
            // Cleanup base_type_expr
            base_type_expr.?.deinit(self.allocator);
            self.allocator.destroy(base_type_expr.?);
            return error.ExpectedRightBracket;
        }
        self.advance(); // consume ]

        const array_type_expr = try self.allocator.create(ast.TypeExpr);
        array_type_expr.* = .{
            .Array = .{
                .element_type = base_type_expr.?, // We know it's non-null here
                .size = size,
            },
        };
        if (self.debug_enabled) {
            std.debug.print("Successfully parsed array type\n", .{});
        }
        return array_type_expr;
    }

    // If no array brackets, return the base type expression
    if (self.debug_enabled) {
        std.debug.print("Successfully parsed non-array type\n", .{});
    }

    return base_type_expr;
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
        var reporter = Reporter.init();
        const location: Reporter.Location = .{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        reporter.reportCompileError(location, "If is an expression and must be followed by a then. (if (condition) then (expr))", .{});
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
            var reporter = Reporter.init();
            const location = Reporter.Location{
                .file = self.current_file,
                .line = self.peek().line,
                .column = self.peek().column,
            };
            reporter.reportCompileError(location, "Expected semicolon", .{});

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
        } else if (self.peek().type == .LEFT_BRACE) {
            // Special handling for block expressions
            else_expr = try braceExpr(self, null, .NONE);
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
                    var reporter = Reporter.init();
                    const location = Reporter.Location{
                        .file = self.current_file,
                        .line = self.peek().line,
                        .column = self.peek().column,
                    };
                    reporter.reportCompileError(location, "Expected semicolon", .{});

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
    if (self.peek().type != .FUNCTION) {
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

    for (params.items) |param| {
        if (param.type_expr == null) {
            return error.MissingParameterType;
        }
    }

    // 2. If function has any return statements with values, must use returns(type)
    if ((try self.hasReturnWithValue()) and !has_return_type) {
        return error.MissingReturnType;
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
        .INT, .FLOAT, .LOGIC, .NOTHING, .TETRA => blk: {
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

    // Check if this is an imported symbol with namespace prefix
    if (self.peek().type == .DOT) {
        // This could be a namespace access (module.symbol)
        const namespace = name.lexeme;

        // If we have a namespace registered
        if (self.module_namespaces.contains(namespace)) {
            // This is a module access - consume the dot
            self.advance();

            // Get the symbol name
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }

            const symbol_name = self.peek();
            self.advance();

            // Create a namespace object as variable
            const namespace_var = try self.allocator.create(ast.Expr);
            namespace_var.* = .{ .Variable = name };

            // Create a FieldAccess expression instead of just Variable
            const field_access = try self.allocator.create(ast.Expr);
            field_access.* = .{ .FieldAccess = .{
                .object = namespace_var,
                .field = symbol_name,
            } };

            // Check for indexing operation
            if (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume '['
                return self.index(field_access, .NONE);
            }

            return field_access;
        }
    }

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
    // Create a basic type expression for the element type
    const element_type = try self.allocator.create(ast.TypeExpr);
    element_type.* = .{ .Basic = .Auto }; // Default to auto type

    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{ .ArrayType = .{
        .element_type = element_type,
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
