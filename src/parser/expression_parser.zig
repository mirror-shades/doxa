const std = @import("std");
const ast = @import("../ast/ast.zig");
const Parser = @import("./parser_types.zig").Parser;
const token = @import("../types/token.zig");
const precedence = @import("./precedence.zig");
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("./statement_parser.zig");
const declaration_parser = @import("./declaration_parser.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const printTemp = std.debug.print;

pub fn parseExpression(self: *Parser) ErrorList!?*ast.Expr {

    // Special handling for array type expressions
    if (self.peek().type == .ARRAY_TYPE) {
        self.advance(); // consume 'array'

        // Create array variable expression
        const array_expr = try self.allocator.create(ast.Expr);
        array_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Variable = self.peek(),
            },
        };

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

    // Get the precedence of the current operator
    const rule = precedence.getRule(operator.type);

    // For left-associative operators, we want to bind more tightly to the right
    const precedence_level = if (rule.associativity == .RIGHT)
        rule.precedence
    else if (rule.precedence == .PRIMARY)
        .PRIMARY
    else
        @as(Precedence, @enumFromInt(@intFromEnum(rule.precedence) + 2));

    // Parse the right-hand side with the appropriate precedence
    const right = try precedence.parsePrecedence(self, precedence_level) orelse
        return error.ExpectedRightOperand;

    const binary_expr = try self.allocator.create(ast.Expr);
    binary_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(operator),
        },
        .data = .{
            .Binary = .{
                .left = left,
                .operator = operator,
                .right = right,
            },
        },
    };
    return binary_expr;
}

pub fn braceExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {

    // Consume the left brace
    self.advance();

    // Look ahead to see if this might be a map
    // A map must have a key followed by a :
    // Skip any leading newlines after '{' before deciding
    var skipped: usize = 0;
    while (self.peek().type == .NEWLINE) : (skipped += 1) self.advance();
    const is_map = (self.peek().type != .RIGHT_BRACE and self.peekAhead(1).type == .WHERE) or
        (self.peekAhead(1).type != .EOF and self.peekAhead(2).type == .WHERE);
    if (is_map) {
        return self.parseMap();
    }

    // Delegate to unified block parser so it can populate the last-expression value.
    // Rewind tokens we may have skipped plus the '{' so Parser.block sees it to consume.
    var to_rewind: usize = skipped + 1;
    while (to_rewind > 0 and self.current > 0) : (to_rewind -= 1) {
        self.current -= 1;
    }
    return Parser.block(self, null, .NONE);
}

pub fn typeofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
    typeof_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .TypeOf = expr,
        },
    };
    return typeof_expr;
}

pub fn lengthofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {

    // We're already at 'lengthof', advance to the next token
    self.advance();

    // Allow optional parentheses around the target expression
    const has_parens = self.peek().type == .LEFT_PAREN;
    if (has_parens) {
        self.advance(); // consume '('
    }

    // Parse the expression whose length we want to check
    const expr = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedExpression;

    if (has_parens) {
        if (self.peek().type != .RIGHT_PAREN) {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
            return error.ExpectedRightParen;
        }
        self.advance(); // consume ')'
    }

    const lengthof_expr = try self.allocator.create(ast.Expr);
    lengthof_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .LengthOf = expr,
        },
    };
    return lengthof_expr;
}

pub fn bytesofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {

    // We're already at 'bytesof', advance to the next token
    self.advance();

    // Allow optional parentheses around the target expression
    const has_parens = self.peek().type == .LEFT_PAREN;
    if (has_parens) {
        self.advance(); // consume '('
    }

    // Parse the expression whose bytes we want to get
    const expr = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedExpression;

    if (has_parens) {
        if (self.peek().type != .RIGHT_PAREN) {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
            return error.ExpectedRightParen;
        }
        self.advance(); // consume ')'
    }

    const bytesof_expr = try self.allocator.create(ast.Expr);
    bytesof_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .BytesOf = expr,
        },
    };
    return bytesof_expr;
}

pub fn parseMatchExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume 'match' keyword

    // Parse the value to match on - parse as a simple variable expression to avoid recursion
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }

    const match_value = self.peek();
    self.advance(); // consume identifier

    const value = try self.allocator.create(ast.Expr);
    value.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(match_value),
        },
        .data = .{
            .Variable = match_value,
        },
    };

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
        // Allow blank lines between match arms
        while (self.peek().type == .NEWLINE) self.advance();
        // Tolerate stray commas between arms (e.g., trailing commas and blank lines)
        while (self.peek().type == .COMMA) self.advance();
        // If skipping moved us to the closing brace, stop parsing arms
        if (self.peek().type == .RIGHT_BRACE) break;
        if (self.peek().type == .ELSE) {
            // Handle else case
            self.advance();
            const else_token = self.previous();

            // Parse body per hybrid rule: block (no required comma) or expression (requires comma)
            var body: *ast.Expr = undefined;
            if (self.peek().type == .LEFT_BRACE) {
                const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                body = block_expr;
                // Optional comma after a block arm
                if (self.peek().type == .COMMA) self.advance();
            } else {
                body = try parseExpression(self) orelse return error.ExpectedExpression;
                // Expression arms must be followed by a comma (trailing comma allowed)
                if (self.peek().type != .COMMA) {
                    return error.ExpectedCommaOrBrace;
                }
                self.advance();
            }

            try cases.append(.{
                .pattern = else_token, // ensure codegen sees a real ELSE token
                .body = body,
            });

            has_else_case = true;

            // Handle optional comma
            if (self.peek().type == .COMMA) {
                self.advance();
            }
            continue;
        }

        // Parse pattern (enum variant, type, or custom type)
        const pattern = try parseMatchPattern(self) orelse return error.ExpectedPattern;

        // Parse then
        if (self.peek().type != .THEN) {
            return error.ExpectedThen;
        }
        self.advance();

        // Parse body per hybrid rule: block (no required comma) or expression (requires comma)
        var body: *ast.Expr = undefined;
        if (self.peek().type == .LEFT_BRACE) {
            const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
            body = block_expr;
            // Optional comma after a block arm
            if (self.peek().type == .COMMA) self.advance();
        } else {
            body = try parseExpression(self) orelse return error.ExpectedExpression;
            // After an expression arm, tolerate newlines and require either a comma or a closing brace.
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .COMMA) {
                self.advance();
            } else if (self.peek().type == .RIGHT_BRACE) {
                // No trailing comma before closing brace is allowed
            } else {
                return error.ExpectedCommaOrBrace;
            }
        }

        try cases.append(.{ .pattern = pattern, .body = body });
    }
    self.advance(); // consume right brace

    // For enums, you may want to check all variants are covered. For unions, partial is fine.
    if (!has_else_case) {
        if (cases.items.len == 0) {
            return error.EmptyMatch;
        }
    }

    const match_expr = try self.allocator.create(ast.Expr);
    match_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Match = .{
                .value = value,
                .cases = try cases.toOwnedSlice(),
            },
        },
    };
    return match_expr;
}

pub fn whileExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.previous()),
            },
            .data = .{
                .Block = .{
                    .statements = block_stmts,
                    .value = null, // No final expression value for the block
                },
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
    while_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .While = .{
                .condition = condition,
                .body = body,
            },
        },
    };
    return while_expr;
}

pub fn forExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume 'for'

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    // Parse initializer
    var initializer: ?*ast.Stmt = null;
    if (self.peek().type != .NEWLINE) {
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
    if (self.peek().type != .NEWLINE) {
        condition = try parseExpression(self);
    }
    if (self.peek().type != .NEWLINE) {
        return error.ExpectedNewline;
    }
    self.advance(); // consume newline

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
                one.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Literal = .{ .int = 1 },
                    },
                };

                const add = try self.allocator.create(ast.Expr);
                add.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Binary = .{
                            .left = var_expr,
                            .operator = token.Token.initWithFile(.PLUS, "+", .{ .nothing = {} }, self.peek().line, self.peek().column, self.current_file),
                            .right = one,
                        },
                    },
                };

                const new_increment = try self.allocator.create(ast.Expr);
                new_increment.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Assignment = .{
                            .name = var_expr.data.Variable,
                            .value = add,
                        },
                    },
                };
                increment = new_increment;
            }
        } else if (self.peek().type == .MINUS_MINUS) {
            if (increment) |var_expr| {
                self.advance(); // consume '--'
                // Create assignment expression: i = i - 1
                const one = try self.allocator.create(ast.Expr);
                one.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Literal = .{ .int = 1 },
                    },
                };

                const sub = try self.allocator.create(ast.Expr);
                sub.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Binary = .{
                            .left = var_expr,
                            .operator = token.Token.initWithFile(.MINUS, "-", .{ .nothing = {} }, self.peek().line, self.peek().column, self.current_file),
                            .right = one,
                        },
                    },
                };

                const new_increment = try self.allocator.create(ast.Expr);
                new_increment.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Assignment = .{
                            .name = var_expr.data.Variable,
                            .value = sub,
                        },
                    },
                };
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
    block_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{ .Block = .{
            .statements = try body.toOwnedSlice(),
            .value = null,
        } },
    };

    const for_expr = try self.allocator.create(ast.Expr);
    for_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .For = .{
                .initializer = initializer,
                .condition = condition,
                .increment = increment,
                .body = block_expr,
            },
        },
    };

    return for_expr;
}

pub fn parseTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
    const type_token = self.peek();
    const type_name = type_token.lexeme;

    var base_type_expr: ?*ast.TypeExpr = null;
    var consumed_token = false;

    // 1. Check for Basic Types
    const maybe_basic_type: ?ast.BasicType = blk: {
        switch (type_token.type) {
            .INT_TYPE => break :blk ast.BasicType.Integer,
            .BYTE_TYPE => break :blk ast.BasicType.Byte,
            .FLOAT_TYPE => break :blk ast.BasicType.Float,
            .STRING_TYPE => break :blk ast.BasicType.String,
            .TETRA_TYPE => break :blk ast.BasicType.Tetra,
            .NOTHING_TYPE => break :blk ast.BasicType.Nothing,
            else => {
                // For backward compatibility, also check lexemes
                if (std.mem.eql(u8, type_name, "int")) break :blk ast.BasicType.Integer;
                if (std.mem.eql(u8, type_name, "byte")) break :blk ast.BasicType.Byte;
                if (std.mem.eql(u8, type_name, "float")) break :blk ast.BasicType.Float;
                if (std.mem.eql(u8, type_name, "string")) break :blk ast.BasicType.String;
                if (std.mem.eql(u8, type_name, "tetra")) break :blk ast.BasicType.Tetra;
                if (std.mem.eql(u8, type_name, "nothing")) break :blk ast.BasicType.Nothing;
                break :blk null;
            },
        }
    };

    if (maybe_basic_type) |basic| {
        // It's a basic type
        self.advance(); // Consume the basic type keyword token
        consumed_token = true;
        base_type_expr = try self.allocator.create(ast.TypeExpr);
        base_type_expr.?.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Basic = basic,
            },
        };
    } else if (type_token.type == .ARRAY_TYPE) {
        // 2. Check for Array Type Syntax
        self.advance(); // consume 'array'
        consumed_token = true;

        if (self.peek().type != .LEFT_BRACKET) return error.ExpectedLeftBracket;
        self.advance(); // consume [

        // Check for optional size specification
        var size: ?*ast.Expr = null;
        if (self.peek().type == .INT) {
            const size_expr = try self.allocator.create(ast.Expr);
            size_expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(self.peek()),
                },
                .data = .{
                    .Literal = self.peek().literal,
                },
            };
            size = size_expr;
            self.advance(); // consume the integer
        }

        if (self.peek().type != .RIGHT_BRACKET) return error.ExpectedRightBracket;
        self.advance(); // consume ]

        // Parse element type (comes after the brackets)
        const element_type = try parseTypeExpr(self) orelse return error.ExpectedType;

        base_type_expr = try self.allocator.create(ast.TypeExpr);
        base_type_expr.?.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Array = .{
                    .element_type = element_type,
                    .size = size, // Use the parsed size (null for dynamic arrays)
                },
            },
        };
    } else if (type_token.type == .STRUCT_TYPE) {
        // 3. Check for Struct Type Syntax
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
        base_type_expr.?.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Struct = try fields.toOwnedSlice(),
            },
        };
    } else if (type_token.type == .IDENTIFIER) {
        // 4. Check for Declared or Imported Custom Types
        var is_known_custom = false;
        if (self.declared_types.contains(type_name)) {
            is_known_custom = true;
        } else if (self.imported_symbols) |symbols| {
            var it = symbols.iterator();
            while (it.next()) |entry| {
                const symbol = entry.value_ptr.*;
                if ((symbol.kind == .Enum or symbol.kind == .Struct or symbol.kind == .Type) and
                    std.mem.eql(u8, symbol.name, type_name))
                {
                    is_known_custom = true;
                    break;
                }
            }
        }

        if (is_known_custom) {
            self.advance(); // Consume the identifier token
            consumed_token = true;
            base_type_expr = try self.allocator.create(ast.TypeExpr);
            base_type_expr.?.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(type_token),
                },
                .data = .{
                    .Custom = type_token,
                },
            };
        } else {
            return error.UnknownType;
        }
    } else {
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
    while (self.peek().type == .LEFT_BRACKET) {
        self.advance(); // consume [

        var size: ?*ast.Expr = null;
        if (self.peek().type == .INT) {
            const size_expr = try self.allocator.create(ast.Expr);
            size_expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(self.peek()),
                },
                .data = .{
                    .Literal = self.peek().literal,
                },
            };
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
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Array = .{
                    .element_type = base_type_expr.?, // We know it's non-null here
                    .size = size,
                },
            },
        };
        base_type_expr = array_type_expr;
    }

    // If no array brackets, return the base type expression

    // --- Check for Union Type ---
    if (self.peek().type == .PIPE) {
        var types = std.ArrayList(*ast.TypeExpr).init(self.allocator);
        errdefer {
            for (types.items) |type_expr| {
                type_expr.deinit(self.allocator);
                self.allocator.destroy(type_expr);
            }
            types.deinit();
        }

        // Add the first type we already parsed
        try types.append(base_type_expr.?);

        // Parse additional types separated by pipes
        while (self.peek().type == .PIPE) {
            self.advance(); // consume |
            const next_type = try parseTypeExpr(self) orelse return error.ExpectedType;
            try types.append(next_type);
        }

        const union_type_expr = try self.allocator.create(ast.TypeExpr);
        union_type_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Union = try types.toOwnedSlice(),
            },
        };
        base_type_expr = union_type_expr;
    }

    return base_type_expr;
}

pub fn allocExpr(self: *Parser, expr: ast.Expr) ErrorList!*ast.Expr {
    const new_expr = try self.allocator.create(ast.Expr);
    new_expr.* = expr;
    return new_expr;
}

pub fn parseIfExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume 'if'

    // Parse the condition expression without assuming parentheses wrap the whole thing
    const condition = (try parseExpression(self)) orelse return error.ExpectedExpression;

    var then_expr: *ast.Expr = undefined;

    if (self.peek().type == .THEN) {
        // Traditional if-then-else expression syntax
        self.advance(); // consume 'then'

        then_expr = (try parseExpression(self)) orelse {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            return error.ExpectedExpression;
        };
    } else if (self.peek().type == .LEFT_BRACE) {
        // Block-style if statement syntax: if condition { statements }
        then_expr = (try braceExpr(self, null, .NONE)) orelse {
            condition.deinit(self.allocator);
            self.allocator.destroy(condition);
            return error.ExpectedExpression;
        };
    } else {
        condition.deinit(self.allocator);
        self.allocator.destroy(condition);
        const location: Reporting.Location = .{
            .file = self.current_file,
            .range = .{
                .start_line = self.peek().line,
                .start_col = self.peek().column,
                .end_line = self.peek().line,
                .end_col = self.peek().column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_THEN, "Expected then", .{});
        return error.ExpectedThen;
    }

    // Handle else branch
    var else_expr: ?*ast.Expr = null;

    // Check for else after newline (if-else-if chain syntax)
    if (self.peek().type == .NEWLINE and self.peekAhead(1).type == .ELSE) {
        self.advance(); // consume newline
    }

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
        else_expr.?.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Literal = .{ .nothing = {} },
            },
        };
    }

    const if_expr = try self.allocator.create(ast.Expr);
    if_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .If = .{
                .condition = condition,
                .then_branch = then_expr,
                .else_branch = else_expr,
            },
        },
    };
    return if_expr;
}

pub fn returnExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume 'return'

    var value: ?*ast.Expr = null;

    // Check if there's a value to return
    if (self.peek().type != .NEWLINE and
        self.peek().type != .ELSE and
        self.peek().type != .RIGHT_BRACE and
        self.peek().type != .EOF)
    {
        value = try parseExpression(self);
    }

    const return_expr = try self.allocator.create(ast.Expr);
    return_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .ReturnExpr = .{
                .value = value,
            },
        },
    };
    return return_expr;
}

pub fn parseBreakExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume 'break'

    const break_expr = try self.allocator.create(ast.Expr);
    break_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Break = {},
        },
    };
    return break_expr;
}

pub fn functionExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    var is_entry = false;

    // Check if this is an entry point
    if (self.peek().type == .ENTRY) {
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
        try self.parseParameters(&params, self.reporter);
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
                .TETRA_TYPE => .Tetra,
                .ARRAY_TYPE => .Array,
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
    function.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .FunctionExpr = .{
                .name = name,
                .params = try params.toOwnedSlice(),
                .return_type_info = return_type,
                .body = body,
                .is_entry = is_entry,
                .is_public = false,
            },
        },
    };
    return function;
}

pub fn literal(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const current = self.peek();
    const expr = switch (current.type) {
        .INT, .FLOAT, .LOGIC, .NOTHING, .TETRA, .BYTE => blk: {
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(current),
                },
                .data = .{
                    .Literal = current.literal,
                },
            };
            self.advance();
            break :blk new_expr;
        },
        .STRING => blk: {
            // Make a copy of the string data
            const string_copy = try self.allocator.dupe(u8, current.literal.string);
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(current),
                },
                .data = .{
                    .Literal = .{ .string = string_copy },
                },
            };
            self.advance();
            break :blk new_expr;
        },
        else => {
            return null;
        },
    };

    return expr;
}

pub fn castExpr(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    // The 'as' token has already been consumed by the precedence parser
    // We should now be at the target type token

    // Parse the target type
    const target_type = try parseTypeExpr(self) orelse return error.ExpectedType;

    // Optional then-branch: "as Type then <expr|{...}>"
    var then_branch: ?*ast.Expr = null;
    if (self.peek().type == .THEN) {
        self.advance(); // consume 'then'
        if (self.peek().type == .LEFT_BRACE) {
            // Block-style then
            const block_expr = try braceExpr(self, null, .NONE) orelse return error.ExpectedLeftBrace;
            then_branch = block_expr;
        } else {
            then_branch = try parseExpression(self) orelse return error.ExpectedExpression;
        }
    }

    // Optional else-branch: "else <expr|{...}>"
    var else_branch: ?*ast.Expr = null;

    // Allow newlines between end of then-branch and else keyword
    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type == .ELSE) {
        self.advance(); // consume 'else'

        // Allow newlines before else-branch body
        while (self.peek().type == .NEWLINE) self.advance();

        if (self.peek().type == .LEFT_BRACE) {
            // Block-style else
            const block_expr = try braceExpr(self, null, .NONE) orelse return error.ExpectedLeftBrace;
            else_branch = block_expr;
        } else {
            else_branch = try parseExpression(self) orelse return error.ExpectedExpression;
        }
    }

    const cast_expr = try self.allocator.create(ast.Expr);
    cast_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{
            .Cast = .{
                .value = left.?,
                .target_type = target_type,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        },
    };
    return cast_expr;
}

pub fn grouping(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume (

    // If we have a left expression, this is a function call
    if (left != null) {
        return self.call(left, .NONE);
    }

    // Allow optional newlines after '('
    while (self.peek().type == .NEWLINE) self.advance();

    // Otherwise it's just a grouped expression
    const expr = try parseExpression(self) orelse return error.ExpectedExpression;

    // Allow optional newlines before ')'
    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance(); // consume )

    const grouping_expr = try self.allocator.create(ast.Expr);
    grouping_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Grouping = expr,
        },
    };
    return grouping_expr;
}

pub fn unary(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const operator = self.peek(); // Get the current token as operator
    self.advance(); // Move past the operator

    const right = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedExpression;

    const unary_expr = try self.allocator.create(ast.Expr);
    unary_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(operator),
        },
        .data = .{
            .Unary = .{
                .operator = operator,
                .right = right,
            },
        },
    };
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
            namespace_var.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(name),
                },
                .data = .{
                    .Variable = name,
                },
            };

            // Create a FieldAccess expression instead of just Variable
            const field_access = try self.allocator.create(ast.Expr);
            field_access.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(symbol_name),
                },
                .data = .{
                    .FieldAccess = .{
                        .object = namespace_var,
                        .field = symbol_name,
                    },
                },
            };

            // Check for indexing operation
            if (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume '['
                return self.index(field_access, .NONE);
            }

            return field_access;
        }
    }

    // Note: Struct literal parsing is handled separately to avoid conflicts with match expressions

    // Create variable expression or enum member expression
    const var_expr = try self.allocator.create(ast.Expr);
    if (name.type == .FIELD_ACCESS) {
        // This is an enum member (e.g., .Red, .Green)
        var_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(name),
            },
            .data = .{
                .EnumMember = name,
            },
        };
    } else {
        // This is a regular variable
        var_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(name),
            },
            .data = .{
                .Variable = name,
            },
        };
    }

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
    element_type.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Basic = .Nothing,
        },
    };

    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .ArrayType = .{
                .element_type = element_type,
            },
        },
    };
    return array_expr;
}

pub fn parseArrayLiteral(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance(); // consume '['

    // Skip optional newlines after '['
    while (self.peek().type == .NEWLINE) self.advance();

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
            // Skip optional newlines after comma
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .RIGHT_BRACKET) break;

            const element = try parseExpression(self) orelse return error.ExpectedExpression;
            try elements.append(element);
        }
    }

    // Allow trailing newlines before closing bracket
    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type != .RIGHT_BRACKET) {
        return error.ExpectedRightBracket;
    }
    self.advance(); // consume ']'

    const array_expr = try self.allocator.create(ast.Expr);
    array_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Array = try elements.toOwnedSlice(),
        },
    };
    return array_expr;
}

fn parseBasicType(self: *Parser) ErrorList!?*ast.TypeExpr {
    const type_token = self.peek();
    const basic_type = switch (type_token.type) {
        .INT_TYPE => ast.BasicType.Integer,
        .BYTE_TYPE => ast.BasicType.Byte,
        .FLOAT_TYPE => ast.BasicType.Float,
        .STRING_TYPE => ast.BasicType.String,
        .TETRA_TYPE => ast.BasicType.Tetra,
        .NOTHING_TYPE => ast.BasicType.Nothing,
        else => return null,
    };

    self.advance(); // consume type token

    const type_expr = try self.allocator.create(ast.TypeExpr);
    type_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(type_token),
        },
        .data = .{
            .Basic = basic_type,
        },
    };
    return type_expr;
}

pub fn inferType(expr: *ast.Expr) !ast.TypeInfo {
    switch (expr.data) {
        .Literal => |lit| {
            switch (lit) {
                .int => return .{ .base = .Int, .is_mutable = false },
                .byte => return .{ .base = .Byte, .is_mutable = false },
                .float => return .{ .base = .Float, .is_mutable = false },
                .string => return .{ .base = .String, .is_mutable = false },
                .tetra => return .{ .base = .Tetra, .is_mutable = false },
                .nothing => return .{ .base = .Nothing, .is_mutable = false },
                .array => return .{ .base = .Array, .is_mutable = false },
                .map => return .{ .base = .Map, .is_mutable = false },
                .enum_variant => return .{ .base = .Enum, .is_mutable = false },
                .struct_value => return .{ .base = .Struct, .is_mutable = false },
                .function => return .{ .base = .Function, .is_mutable = false },
            }
        },
        .Array => return .{ .base = .Array, .is_mutable = false },
        .Map => return .{ .base = .Map, .is_mutable = false },
        .StructLiteral => |struct_lit| return .{ .base = .Custom, .custom_type = struct_lit.name.lexeme, .is_mutable = false },
        .Cast => return .{ .base = .Nothing, .is_mutable = false },
        else => return .{ .base = .Nothing, .is_mutable = false },
    }
}

pub fn parseStructOrMatch(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    // We should be at an identifier
    if (self.peek().type != .IDENTIFIER) {
        return null;
    }

    const start_pos = self.current;
    self.advance(); // consume identifier

    // Check if next token is a brace
    if (self.peek().type != .LEFT_BRACE) {
        // Not a struct literal or match expression, reset and fall back to variable parser
        self.current = start_pos;
        return variable(self, null, .NONE);
    }

    self.advance(); // consume '{'

    // Skip any NEWLINEs immediately after '{' so decisions aren't tripped by formatting
    while (self.peek().type == .NEWLINE) self.advance();

    // Check the first token after the brace (after skipping newlines)
    const first_token_after_brace = self.peek();

    // Check if this looks like a match expression pattern
    const is_match_pattern = (first_token_after_brace.type == .DOT) or // .Red
        (first_token_after_brace.type == .INT_TYPE) or // int
        (first_token_after_brace.type == .FLOAT_TYPE) or // float
        (first_token_after_brace.type == .STRING_TYPE) or // string
        (first_token_after_brace.type == .BYTE_TYPE) or // byte
        (first_token_after_brace.type == .TETRA_TYPE) or // tetra
        (first_token_after_brace.type == .NOTHING_TYPE) or // nothing
        (first_token_after_brace.type == .INT) or // 0, 1, 2, etc.
        (first_token_after_brace.type == .ELSE) or // else
        (first_token_after_brace.type == .IDENTIFIER and self.declared_types.contains(first_token_after_brace.lexeme));

    if (is_match_pattern) {
        // This is a match expression - parse it directly without recursion
        self.current = start_pos; // Reset to before identifier

        // We should be at an identifier (the match value)
        if (self.peek().type != .IDENTIFIER) {
            return null;
        }

        const match_value = self.peek();
        self.advance(); // consume identifier

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
            // Allow blank lines between match arms
            while (self.peek().type == .NEWLINE) self.advance();
            // Tolerate stray commas between arms
            while (self.peek().type == .COMMA) self.advance();
            if (self.peek().type == .RIGHT_BRACE) break;
            if (self.peek().type == .ELSE) {
                // Handle else case
                self.advance();
                const else_token = self.previous();

                // Parse body per hybrid rule: block (no required comma) or expression (requires comma)
                var body: *ast.Expr = undefined;
                if (self.peek().type == .LEFT_BRACE) {
                    const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                    body = block_expr;
                    // Optional comma after a block arm
                    if (self.peek().type == .COMMA) {
                        self.advance();
                    }
                } else {
                    body = try parseExpression(self) orelse return error.ExpectedExpression;
                    if (self.peek().type != .COMMA) {
                        return error.ExpectedCommaOrBrace;
                    }
                    self.advance();
                }

                try cases.append(.{
                    .pattern = else_token,
                    .body = body,
                });

                has_else_case = true;
                continue;
            }

            // Parse pattern (enum variant, type, or custom type)
            const pattern = try parseMatchPattern(self) orelse return error.ExpectedPattern;

            // Parse then
            if (self.peek().type != .THEN) {
                return error.ExpectedThen;
            }
            self.advance();

            // Parse body per hybrid rule: block (no required comma) or expression (requires comma)
            var body: *ast.Expr = undefined;
            if (self.peek().type == .LEFT_BRACE) {
                const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                body = block_expr;
                // Optional comma after a block arm
                if (self.peek().type == .COMMA) {
                    self.advance();
                }
            } else {
                body = try parseExpression(self) orelse return error.ExpectedExpression;
                if (self.peek().type != .COMMA) {
                    return error.ExpectedCommaOrBrace;
                }
                self.advance();
            }

            try cases.append(.{
                .pattern = pattern,
                .body = body,
            });
        }
        self.advance(); // consume right brace

        // For enums, you may want to check all variants are covered. For unions, partial is fine.
        if (!has_else_case) {
            if (cases.items.len == 0) {
                return error.EmptyMatch;
            }
        }

        // Create variable expression for the match value
        const value_expr = try self.allocator.create(ast.Expr);
        value_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(match_value),
            },
            .data = .{
                .Variable = match_value,
            },
        };

        const match_expr = try self.allocator.create(ast.Expr);
        match_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.previous()),
            },
            .data = .{
                .Match = .{
                    .value = value_expr,
                    .cases = try cases.toOwnedSlice(),
                },
            },
        };
        return match_expr;
    } else if (first_token_after_brace.type == .IDENTIFIER) {
        // This is likely a struct literal (starts with field name)
        self.current = start_pos; // Reset to before identifier
        return Parser.parseStructInit(self);
    } else {
        // This might be something else, reset and fall back to variable parser
        self.current = start_pos;
        return variable(self, null, .NONE);
    }
}

fn parseMatchPattern(self: *Parser) ErrorList!?token.Token {
    // Skip any leading newlines before a pattern
    while (self.peek().type == .NEWLINE or self.peek().type == .COMMA) self.advance();
    const current = self.peek();

    switch (current.type) {
        .FIELD_ACCESS => {
            // Allow tokens that represent a field/enum member (e.g., .Red) directly
            const fa = self.peek();
            self.advance();
            return fa;
        },
        .DOT => {
            // Enum variant pattern: .Red
            self.advance(); // consume '.'
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const variant = self.peek();
            self.advance();
            return variant;
        },
        .INT => {
            // Integer literal pattern: 0, 1, 2, etc.
            const int_token = self.peek();
            self.advance();
            return int_token;
        },
        .STRING, .FLOAT, .BYTE, .TETRA, .LOGIC => {
            // Support literal patterns for common literal token types
            const lit_token = self.peek();
            self.advance();
            return lit_token;
        },
        .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE, .NOTHING => {
            // Type patterns for union matching: int, float, string, etc.
            // Also handle .NOTHING for the literal 'nothing' when used as a type pattern
            const type_token = self.peek();
            self.advance();
            return type_token;
        },
        .IDENTIFIER => {
            // Treat any identifier (except 'else') as a valid pattern token.
            if (std.mem.eql(u8, current.lexeme, "else")) {
                self.advance();
                return current;
            }
            // Handle qualified enum variant: Type.Identifier
            if (self.peekAhead(1).type == .DOT and self.peekAhead(2).type == .IDENTIFIER) {
                self.advance(); // consume type identifier
                self.advance(); // dot
                const variant_ident = self.peek();
                self.advance(); // variant
                // Return the variant identifier as the pattern token (codegen/semantics can use both)
                return variant_ident;
            }
            const ident_tok = self.peek();
            self.advance();
            return ident_tok;
        },
        else => {
            // Permissive fallback: accept any non-separator token as a pattern token
            if (current.type == .THEN or current.type == .RIGHT_BRACE or current.type == .COMMA or current.type == .NEWLINE) {
                return null;
            }
            const tok = self.peek();
            self.advance();
            return tok;
        },
    }
}
