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
    if (self.peek().type == .ARRAY_TYPE) {
        self.advance();

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

        if (self.peek().type == .LEFT_BRACKET) {
            self.advance();
            return Parser.index(self, array_expr, .NONE);
        }

        return array_expr;
    }

    return try precedence.parsePrecedence(self, Precedence.ASSIGNMENT);
}

pub fn binary(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;
    const operator = self.tokens[self.current - 1];
    const rule = precedence.getRule(operator.type);

    const precedence_level = if (rule.associativity == .RIGHT)
        rule.precedence
    else if (rule.precedence == .PRIMARY)
        .PRIMARY
    else
        @as(Precedence, @enumFromInt(@intFromEnum(rule.precedence) + 2));

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
    self.advance();

    if (self.current >= 2) {
        const prev_prev_tok = self.tokens[self.current - 2];
        const force_block = prev_prev_tok.type == .THEN or prev_prev_tok.type == .ELSE or prev_prev_tok.type == .DO or prev_prev_tok.type == .WHILE or prev_prev_tok.type == .FOR or prev_prev_tok.type == .FUNCTION;
        if (force_block) {
            self.current -= 1;
            return Parser.block(self, null, .NONE);
        }
    }

    var skipped: usize = 0;
    while (self.peek().type == .NEWLINE) : (skipped += 1) self.advance();
    const first_tok = self.peek().type;
    const sep1 = self.peekAhead(1).type;
    const starts_like_statement = first_tok == .VAR or first_tok == .CONST or first_tok == .FUNCTION or
        first_tok == .IF or first_tok == .WHILE or first_tok == .FOR or first_tok == .RETURN or
        first_tok == .BREAK or first_tok == .CONTINUE or first_tok == .IMPORT;
    const literal_key_start = first_tok == .STRING or first_tok == .INT or first_tok == .FLOAT or first_tok == .BYTE or first_tok == .TETRA or first_tok == .DOT;
    const is_map = !starts_like_statement and literal_key_start and (first_tok != .RIGHT_BRACE and sep1 == .ASSIGN);
    if (is_map) {
        return self.parseMap();
    }

    var to_rewind: usize = skipped + 1;
    while (to_rewind > 0 and self.current > 0) : (to_rewind -= 1) {
        self.current -= 1;
    }
    return Parser.block(self, null, .NONE);
}

pub fn typeofExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    const expr = try parseExpression(self) orelse return error.ExpectedExpression;

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
    self.advance();

    const has_parens = self.peek().type == .LEFT_PAREN;
    if (has_parens) {
        self.advance();
    }

    const expr = try precedence.parsePrecedence(self, .UNARY) orelse return error.ExpectedExpression;

    if (has_parens) {
        if (self.peek().type != .RIGHT_PAREN) {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
            return error.ExpectedRightParen;
        }
        self.advance();
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

pub fn parseMatchExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }

    const match_value = self.peek();
    self.advance();

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

    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    var cases = std.array_list.Managed(ast.MatchCase).init(self.allocator);
    errdefer cases.deinit();

    var has_else_case = false;

    while (self.peek().type != .RIGHT_BRACE) {
        while (self.peek().type == .NEWLINE) self.advance();
        while (self.peek().type == .COMMA) self.advance();
        if (self.peek().type == .RIGHT_BRACE) break;
        if (self.peek().type == .ELSE) {
            self.advance();
            const else_token = self.previous();

            var body: *ast.Expr = undefined;
            if (self.peek().type == .LEFT_BRACE) {
                const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                body = block_expr;
                if (self.peek().type == .COMMA) self.advance();
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

            if (self.peek().type == .COMMA) {
                self.advance();
            }
            continue;
        }

        const pattern = try parseMatchPattern(self) orelse return error.ExpectedPattern;

        if (self.peek().type != .THEN) {
            return error.ExpectedThen;
        }
        self.advance();

        var body: *ast.Expr = undefined;
        if (self.peek().type == .LEFT_BRACE) {
            const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
            body = block_expr;
            if (self.peek().type == .COMMA) self.advance();
        } else {
            body = try parseExpression(self) orelse return error.ExpectedExpression;
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .COMMA) {
                self.advance();
            } else if (self.peek().type != .RIGHT_BRACE) {
                return error.ExpectedCommaOrBrace;
            }
        }

        try cases.append(.{ .pattern = pattern, .body = body });
    }
    self.advance();

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

pub fn doExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    var step_expr: ?*ast.Expr = null;
    var condition: ?*ast.Expr = null;
    var body: *ast.Expr = undefined;

    if (self.peek().type == .LEFT_BRACE) {
        const first_block = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;

        if (self.peek().type == .WHILE) {
            step_expr = first_block;
            self.advance();

            const has_parens = self.peek().type == .LEFT_PAREN;
            if (has_parens) self.advance();

            condition = try parseExpression(self);
            if (has_parens) {
                if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
                self.advance();
            }

            if (self.peek().type == .LEFT_BRACE) {
                body = (try Parser.block(self, null, .NONE)) orelse return error.ExpectedLeftBrace;
            } else {
                body = (try parseExpression(self)) orelse return error.ExpectedExpression;
            }
        } else {
            body = first_block;
        }
    } else if (self.peek().type == .WHILE) {
        self.advance();

        const has_parens = self.peek().type == .LEFT_PAREN;
        if (has_parens) self.advance();

        condition = try parseExpression(self);
        if (has_parens) {
            if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
            self.advance();
        }

        if (self.peek().type == .LEFT_BRACE) {
            body = (try Parser.block(self, null, .NONE)) orelse return error.ExpectedLeftBrace;
        } else {
            body = (try parseExpression(self)) orelse return error.ExpectedExpression;
        }
    } else {
        step_expr = try parseExpression(self) orelse return error.ExpectedExpression;

        if (self.peek().type == .WHILE) {
            self.advance();

            const has_parens = self.peek().type == .LEFT_PAREN;
            if (has_parens) self.advance();

            condition = try parseExpression(self);
            if (has_parens) {
                if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
                self.advance();
            }
        }

        if (self.peek().type == .LEFT_BRACE) {
            body = (try Parser.block(self, null, .NONE)) orelse return error.ExpectedLeftBrace;
        } else {
            body = (try parseExpression(self)) orelse return error.ExpectedExpression;
        }
    }

    const loop_expr = try self.allocator.create(ast.Expr);
    loop_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{ .Loop = .{
            .var_decl = null,
            .condition = condition,
            .step = step_expr,
            .body = body,
        } },
    };
    return loop_expr;
}

pub fn whileExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();
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

    var step_expr: ?*ast.Expr = null;
    var body: *ast.Expr = undefined;

    if (self.peek().type == .DO) {
        self.advance();

        if (self.peek().type == .LEFT_BRACE) {
            step_expr = (try Parser.block(self, null, .NONE)) orelse return error.ExpectedLeftBrace;
        } else {
            step_expr = (try parseExpression(self)) orelse return error.ExpectedExpression;
        }

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
                        .value = null,
                    },
                },
            };
            body = block_expr;
        } else {
            body = (try parseExpression(self)) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                if (step_expr) |step| {
                    step.deinit(self.allocator);
                    self.allocator.destroy(step);
                }
                return error.ExpectedExpression;
            };
        }
    } else {
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
                        .value = null,
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
    }

    const loop_expr = try self.allocator.create(ast.Expr);
    loop_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{ .Loop = .{
            .var_decl = null,
            .condition = condition,
            .step = step_expr,
            .body = body,
        } },
    };
    return loop_expr;
}

pub fn forExpr(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    self.advance();

    var var_decl: ?*ast.Stmt = null;
    var condition: ?*ast.Expr = null;
    var step_expr: ?*ast.Expr = null;
    var body: *ast.Expr = undefined;

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const var_name = self.peek();
    self.advance();

    if (self.peek().type == .ASSIGN) {
        self.advance();
        const initializer = (try parseExpression(self)) orelse return error.ExpectedExpression;

        const var_decl_stmt = try self.allocator.create(ast.Stmt);
        var_decl_stmt.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(var_name),
            },
            .data = .{
                .VarDecl = .{
                    .name = var_name,
                    .type_info = ast.TypeInfo{ .base = .Int, .is_mutable = true },
                    .initializer = initializer,
                    .is_public = false,
                },
            },
        };
        var_decl = var_decl_stmt;
    } else {
        const zero_expr = try self.allocator.create(ast.Expr);
        zero_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(var_name),
            },
            .data = .{ .Literal = .{ .int = 0 } },
        };

        const var_decl_stmt = try self.allocator.create(ast.Stmt);
        var_decl_stmt.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(var_name),
            },
            .data = .{
                .VarDecl = .{
                    .name = var_name,
                    .type_info = ast.TypeInfo{ .base = .Int, .is_mutable = true },
                    .initializer = zero_expr,
                    .is_public = false,
                },
            },
        };
        var_decl = var_decl_stmt;
    }

    if (self.peek().type == .WHILE) {
        self.advance();
        condition = (try parseExpression(self)) orelse return error.ExpectedExpression;
    }

    if (self.peek().type == .DO) {
        self.advance();
        if (self.peek().type == .LEFT_BRACE) {
            step_expr = (try Parser.block(self, null, .NONE)) orelse return error.ExpectedLeftBrace;
        } else {
            step_expr = (try parseExpression(self)) orelse return error.ExpectedExpression;
        }
    }

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
                    .value = null,
                },
            },
        };
        body = block_expr;
    } else {
        body = (try parseExpression(self)) orelse return error.ExpectedExpression;
    }

    const loop_expr = try self.allocator.create(ast.Expr);
    loop_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.previous()),
        },
        .data = .{ .Loop = .{
            .var_decl = var_decl,
            .condition = condition,
            .step = step_expr,
            .body = body,
        } },
    };
    return loop_expr;
}

pub fn prefixIncrement(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const operator = self.peek();
    const is_decrement = (operator.type == .DECREMENT);

    self.advance();
    const expr = try parseExpression(self);

    const increment_expr = try self.allocator.create(ast.Expr);
    increment_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(operator) },
        .data = if (is_decrement)
            .{ .Decrement = expr.? }
        else
            .{ .Increment = expr.? },
    };
    return increment_expr;
}

pub fn postfixIncrement(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const operator = self.previous();
    const is_decrement = (operator.type == .DECREMENT);

    const increment_expr = try self.allocator.create(ast.Expr);
    increment_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(operator) },
        .data = if (is_decrement)
            .{ .Decrement = left.? }
        else
            .{ .Increment = left.? },
    };
    return increment_expr;
}

pub fn prefixDecrement(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const operator = self.peek();

    self.advance();
    const expr = try parseExpression(self);

    const decrement_expr = try self.allocator.create(ast.Expr);
    decrement_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(operator) },
        .data = .{ .Decrement = expr.? },
    };
    return decrement_expr;
}

pub fn postfixDecrement(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const operator = self.previous();

    const decrement_expr = try self.allocator.create(ast.Expr);
    decrement_expr.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(operator) },
        .data = .{ .Decrement = left.? },
    };
    return decrement_expr;
}

pub fn parseTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
    const type_token = self.peek();
    const type_name = type_token.lexeme;

    var base_type_expr: ?*ast.TypeExpr = null;
    var consumed_token = false;

    const maybe_basic_type: ?ast.BasicType = blk: {
        switch (type_token.type) {
            .INT_TYPE => break :blk ast.BasicType.Integer,
            .BYTE_TYPE => break :blk ast.BasicType.Byte,
            .FLOAT_TYPE => break :blk ast.BasicType.Float,
            .STRING_TYPE => break :blk ast.BasicType.String,
            .TETRA_TYPE => break :blk ast.BasicType.Tetra,
            .NOTHING_TYPE => break :blk ast.BasicType.Nothing,
            else => {
                if (std.mem.eql(u8, type_name, "int")) break :blk ast.BasicType.Integer;
                if (std.mem.eql(u8, type_name, "Int")) break :blk ast.BasicType.Integer;
                if (std.mem.eql(u8, type_name, "byte")) break :blk ast.BasicType.Byte;
                if (std.mem.eql(u8, type_name, "Byte")) break :blk ast.BasicType.Byte;
                if (std.mem.eql(u8, type_name, "float")) break :blk ast.BasicType.Float;
                if (std.mem.eql(u8, type_name, "Float")) break :blk ast.BasicType.Float;
                if (std.mem.eql(u8, type_name, "string")) break :blk ast.BasicType.String;
                if (std.mem.eql(u8, type_name, "String")) break :blk ast.BasicType.String;
                if (std.mem.eql(u8, type_name, "tetra")) break :blk ast.BasicType.Tetra;
                if (std.mem.eql(u8, type_name, "Tetra")) break :blk ast.BasicType.Tetra;
                if (std.mem.eql(u8, type_name, "nothing")) break :blk ast.BasicType.Nothing;
                if (std.mem.eql(u8, type_name, "Nothing")) break :blk ast.BasicType.Nothing;
                break :blk null;
            },
        }
    };

    if (maybe_basic_type) |basic| {
        self.advance();
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
        self.advance();
        consumed_token = true;

        if (self.peek().type != .LEFT_BRACKET) return error.ExpectedLeftBracket;
        self.advance();

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
            self.advance();
        }

        if (self.peek().type != .RIGHT_BRACKET) return error.ExpectedRightBracket;
        self.advance();

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
                    .size = size,
                },
            },
        };
    } else if (type_token.type == .STRUCT_TYPE) {
        self.advance();
        consumed_token = true;

        if (self.peek().type != .LEFT_BRACE) return error.ExpectedLeftBrace;
        self.advance();

        var fields = std.array_list.Managed(*ast.StructField).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                field.deinit(self.allocator);
                self.allocator.destroy(field);
            }
            fields.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE) {
            if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
            const field_name = self.peek();
            self.advance();
            if (self.peek().type != .WHERE) return error.ExpectedColon;
            self.advance();
            const field_type = try parseTypeExpr(self) orelse return error.ExpectedType;
            const field = try self.allocator.create(ast.StructField);
            field.* = .{ .name = field_name, .type_expr = field_type };
            try fields.append(field);
            if (self.peek().type == .COMMA) {
                self.advance();
                if (self.peek().type == .RIGHT_BRACE) break;
            } else if (self.peek().type != .RIGHT_BRACE) {
                return error.ExpectedCommaOrBrace;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) return error.ExpectedRightBrace;
        self.advance();

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
            self.advance();
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

    if (!consumed_token and base_type_expr != null) {
        return error.InternalParserError;
    }
    if (base_type_expr == null) return error.ExpectedType;

    while (self.peek().type == .LEFT_BRACKET) {
        self.advance();

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
            self.advance();
        }

        if (self.peek().type != .RIGHT_BRACKET) {
            if (size) |s| {
                s.deinit(self.allocator);
                self.allocator.destroy(s);
            }
            base_type_expr.?.deinit(self.allocator);
            self.allocator.destroy(base_type_expr.?);
            return error.ExpectedRightBracket;
        }
        self.advance();

        const array_type_expr = try self.allocator.create(ast.TypeExpr);
        array_type_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Array = .{
                    .element_type = base_type_expr.?,
                    .size = size,
                },
            },
        };
        base_type_expr = array_type_expr;
    }

    if (self.peek().type == .PIPE) {
        var types = std.array_list.Managed(*ast.TypeExpr).init(self.allocator);
        errdefer {
            for (types.items) |type_expr| {
                type_expr.deinit(self.allocator);
                self.allocator.destroy(type_expr);
            }
            types.deinit();
        }

        try types.append(base_type_expr.?);

        while (self.peek().type == .PIPE) {
            self.advance();
            const next_type = try parseNonUnionTypeExpr(self) orelse return error.ExpectedType;
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
    self.advance();

    const condition = (try parseExpression(self)) orelse return error.ExpectedExpression;

    var then_expr: *ast.Expr = undefined;

    if (self.peek().type == .THEN) {
        self.advance();

        if (self.peek().type == .CONTINUE or self.peek().type == .BREAK or self.peek().type == .RETURN) {
            const stmt: ast.Stmt = switch (self.peek().type) {
                .CONTINUE => try statement_parser.parseContinueStmt(self),
                .BREAK => try statement_parser.parseBreakStmt(self),
                .RETURN => try statement_parser.parseReturnStmt(self),
                else => unreachable,
            };
            var stmts = try self.allocator.alloc(ast.Stmt, 1);
            stmts[0] = stmt;
            const block_expr = try self.allocator.create(ast.Expr);
            block_expr.* = .{
                .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(self.previous()) },
                .data = .{ .Block = .{ .statements = stmts, .value = null } },
            };
            then_expr = block_expr;
        } else if (self.peek().type == .LEFT_BRACE) {
            then_expr = (try braceExpr(self, null, .NONE)) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                return error.ExpectedExpression;
            };
        } else {
            then_expr = (try parseExpression(self)) orelse {
                condition.deinit(self.allocator);
                self.allocator.destroy(condition);
                return error.ExpectedExpression;
            };
        }
    } else if (self.peek().type == .LEFT_BRACE) {
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

    var else_expr: ?*ast.Expr = null;

    if (self.peek().type == .NEWLINE and self.peekAhead(1).type == .ELSE) {
        self.advance();
    }

    if (self.peek().type == .ELSE) {
        self.advance();
        if (self.peek().type == .IF) {
            else_expr = try parseIfExpr(self, null, .NONE);
        } else if (self.peek().type == .LEFT_BRACE) {
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
    self.advance();

    var value: ?*ast.Expr = null;

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
    self.advance();

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

    const target_type = try parseTypeExpr(self) orelse return error.ExpectedType;

    var then_branch: ?*ast.Expr = null;
    if (self.peek().type == .THEN) {
        self.advance();
        if (self.peek().type == .LEFT_BRACE) {
            const block_expr = try braceExpr(self, null, .NONE) orelse return error.ExpectedLeftBrace;
            then_branch = block_expr;
        } else {
            then_branch = try parseExpression(self) orelse return error.ExpectedExpression;
        }
    }

    var else_branch: ?*ast.Expr = null;

    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type == .ELSE) {
        self.advance();

        while (self.peek().type == .NEWLINE) self.advance();

        if (self.peek().type == .LEFT_BRACE) {
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
    self.advance();

    if (left != null) {
        return self.call(left, .NONE);
    }

    while (self.peek().type == .NEWLINE) self.advance();

    const expr = try parseExpression(self) orelse return error.ExpectedExpression;

    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

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
    const operator = self.peek();
    self.advance();

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

    if (name.type == .THIS) {
        const this_expr = try self.allocator.create(ast.Expr);
        this_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(name),
            },
            .data = .{ .This = {} },
        };
        return this_expr;
    }

    if (self.peek().type == .DOT) {
        const namespace = name.lexeme;

        if (self.module_namespaces.contains(namespace)) {
            self.advance();

            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }

            const symbol_name = self.peek();
            self.advance();

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

            if (self.peek().type == .LEFT_BRACKET) {
                self.advance();
                return self.index(field_access, .NONE);
            }

            return field_access;
        }
    }

    const var_expr = try self.allocator.create(ast.Expr);
    if (name.type == .FIELD_ACCESS) {
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

    if (self.peek().type == .LEFT_BRACKET) {
        self.advance();
        return self.index(var_expr, .NONE);
    }

    return var_expr;
}

pub fn arrayType(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
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
    self.advance();

    while (self.peek().type == .NEWLINE) self.advance();

    var elements = std.array_list.Managed(*ast.Expr).init(self.allocator);
    errdefer {
        for (elements.items) |element| {
            element.deinit(self.allocator);
            self.allocator.destroy(element);
        }
        elements.deinit();
    }

    if (self.peek().type != .RIGHT_BRACKET) {
        const first_element = try parseExpression(self) orelse return error.ExpectedExpression;
        try elements.append(first_element);

        while (self.peek().type == .COMMA) {
            self.advance();
            while (self.peek().type == .NEWLINE) self.advance();
            if (self.peek().type == .RIGHT_BRACKET) break;

            const element = try parseExpression(self) orelse return error.ExpectedExpression;
            try elements.append(element);
        }
    }

    while (self.peek().type == .NEWLINE) self.advance();

    if (self.peek().type != .RIGHT_BRACKET) {
        return error.ExpectedRightBracket;
    }
    self.advance();

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

fn parseNonUnionTypeExpr(self: *Parser) ErrorList!?*ast.TypeExpr {
    const type_token = self.peek();
    const type_name = type_token.lexeme;

    var base_type_expr: ?*ast.TypeExpr = null;
    var consumed_token = false;

    const maybe_basic_type: ?ast.BasicType = blk: {
        switch (type_token.type) {
            .INT_TYPE => break :blk ast.BasicType.Integer,
            .BYTE_TYPE => break :blk ast.BasicType.Byte,
            .FLOAT_TYPE => break :blk ast.BasicType.Float,
            .STRING_TYPE => break :blk ast.BasicType.String,
            .TETRA_TYPE => break :blk ast.BasicType.Tetra,
            .NOTHING_TYPE => break :blk ast.BasicType.Nothing,
            else => {
                if (std.mem.eql(u8, type_name, "int")) break :blk ast.BasicType.Integer;
                if (std.mem.eql(u8, type_name, "Int")) break :blk ast.BasicType.Integer;
                if (std.mem.eql(u8, type_name, "byte")) break :blk ast.BasicType.Byte;
                if (std.mem.eql(u8, type_name, "Byte")) break :blk ast.BasicType.Byte;
                if (std.mem.eql(u8, type_name, "float")) break :blk ast.BasicType.Float;
                if (std.mem.eql(u8, type_name, "Float")) break :blk ast.BasicType.Float;
                if (std.mem.eql(u8, type_name, "string")) break :blk ast.BasicType.String;
                if (std.mem.eql(u8, type_name, "String")) break :blk ast.BasicType.String;
                if (std.mem.eql(u8, type_name, "tetra")) break :blk ast.BasicType.Tetra;
                if (std.mem.eql(u8, type_name, "Tetra")) break :blk ast.BasicType.Tetra;
                if (std.mem.eql(u8, type_name, "nothing")) break :blk ast.BasicType.Nothing;
                if (std.mem.eql(u8, type_name, "Nothing")) break :blk ast.BasicType.Nothing;
                break :blk null;
            },
        }
    };

    if (maybe_basic_type) |basic_type| {
        self.advance();
        consumed_token = true;

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
        base_type_expr = type_expr;
    } else {
        if (type_token.type == .IDENTIFIER) {
            self.advance();
            consumed_token = true;

            const type_expr = try self.allocator.create(ast.TypeExpr);
            type_expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(type_token),
                },
                .data = .{
                    .Custom = type_token,
                },
            };
            base_type_expr = type_expr;
        }
    }

    if (base_type_expr == null) {
        return null;
    }

    if (self.peek().type == .LEFT_BRACKET) {
        self.advance();
        consumed_token = true;

        var array_size: ?*ast.Expr = null;
        if (self.peek().type != .RIGHT_BRACKET) {
            array_size = try parseExpression(self) orelse return error.ExpectedExpression;
        }

        if (self.peek().type != .RIGHT_BRACKET) {
            return error.ExpectedRightBracket;
        }
        self.advance();

        const element_type = try parseNonUnionTypeExpr(self) orelse return error.ExpectedType;

        const array_type_expr = try self.allocator.create(ast.TypeExpr);
        array_type_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(type_token),
            },
            .data = .{
                .Array = ast.ArrayType{
                    .element_type = element_type,
                    .size = array_size,
                },
            },
        };
        base_type_expr = array_type_expr;
    }

    return base_type_expr;
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

    self.advance();

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
        .StructLiteral => |struct_lit| return .{
            .base = .Custom,
            .custom_type = struct_lit.name.lexeme,
            .is_mutable = false,
        },
        .Cast => return .{ .base = .Nothing, .is_mutable = false },
        .Print => |print| {
            if (print.arguments) |args| {
                for (args) |arg| {
                    _ = try inferType(arg);
                }
            }
            return .{ .base = .Nothing, .is_mutable = false };
        },
        .Increment => |operand| {
            return try inferType(operand);
        },
        .Decrement => |operand| {
            return try inferType(operand);
        },
        else => return .{ .base = .Nothing, .is_mutable = false },
    }
}

pub fn identifierOrStructLiteral(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const start_pos = self.current;

    if (self.peek().type != .IDENTIFIER) return null;
    self.advance();

    if (self.peek().type == .LEFT_BRACE) {
        self.current = start_pos;
        return parseStructOrMatch(self, null, .NONE);
    }

    self.current = start_pos;
    return variable(self, null, .NONE);
}

pub fn parseStructOrMatch(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (self.peek().type != .IDENTIFIER) {
        return null;
    }

    const start_pos = self.current;
    self.advance();

    if (self.peek().type != .LEFT_BRACE) {
        self.current = start_pos;
        return variable(self, null, .NONE);
    }

    self.advance();

    while (self.peek().type == .NEWLINE) self.advance();

    const first_token_after_brace = self.peek();

    const is_match_pattern = (first_token_after_brace.type == .DOT) or
        (first_token_after_brace.type == .INT_TYPE) or
        (first_token_after_brace.type == .FLOAT_TYPE) or
        (first_token_after_brace.type == .STRING_TYPE) or
        (first_token_after_brace.type == .BYTE_TYPE) or
        (first_token_after_brace.type == .TETRA_TYPE) or
        (first_token_after_brace.type == .NOTHING_TYPE) or
        (first_token_after_brace.type == .INT) or
        (first_token_after_brace.type == .ELSE) or
        (first_token_after_brace.type == .IDENTIFIER and self.declared_types.contains(first_token_after_brace.lexeme));

    if (is_match_pattern) {
        self.current = start_pos;

        if (self.peek().type != .IDENTIFIER) {
            return null;
        }

        const match_value = self.peek();
        self.advance();

        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        var cases = std.array_list.Managed(ast.MatchCase).init(self.allocator);
        errdefer cases.deinit();

        var has_else_case = false;

        while (self.peek().type != .RIGHT_BRACE) {
            while (self.peek().type == .NEWLINE) self.advance();
            while (self.peek().type == .COMMA) self.advance();
            if (self.peek().type == .RIGHT_BRACE) break;
            if (self.peek().type == .ELSE) {
                self.advance();
                const else_token = self.previous();
                var body: *ast.Expr = undefined;
                if (self.peek().type == .LEFT_BRACE) {
                    const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                    body = block_expr;
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

            const pattern = try parseMatchPattern(self) orelse return error.ExpectedPattern;

            if (self.peek().type != .THEN) {
                return error.ExpectedThen;
            }
            self.advance();

            var body: *ast.Expr = undefined;
            if (self.peek().type == .LEFT_BRACE) {
                const block_expr = try Parser.block(self, null, .NONE) orelse return error.ExpectedLeftBrace;
                body = block_expr;
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
        self.advance();

        if (!has_else_case) {
            if (cases.items.len == 0) {
                return error.EmptyMatch;
            }
        }

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
    } else if (first_token_after_brace.type == .IDENTIFIER and self.peekAhead(1).type == .ASSIGN) {
        self.current = start_pos;
        return Parser.parseStructInit(self);
    } else {
        self.current = start_pos;
        return variable(self, null, .NONE);
    }
}

fn parseMatchPattern(self: *Parser) ErrorList!?token.Token {
    while (self.peek().type == .NEWLINE or self.peek().type == .COMMA) self.advance();
    const current = self.peek();

    switch (current.type) {
        .FIELD_ACCESS => {
            const fa = self.peek();
            self.advance();
            return fa;
        },
        .DOT => {
            self.advance();
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const variant = self.peek();
            self.advance();
            return variant;
        },
        .INT => {
            const int_token = self.peek();
            self.advance();
            return int_token;
        },
        .STRING, .FLOAT, .BYTE, .TETRA, .LOGIC => {
            const lit_token = self.peek();
            self.advance();
            return lit_token;
        },
        .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE, .NOTHING => {
            const type_token = self.peek();
            self.advance();
            return type_token;
        },
        .IDENTIFIER => {
            if (std.mem.eql(u8, current.lexeme, "else")) {
                self.advance();
                return current;
            }
            if (self.peekAhead(1).type == .DOT and self.peekAhead(2).type == .IDENTIFIER) {
                self.advance();
                self.advance();
                const variant_ident = self.peek();
                self.advance();
                return variant_ident;
            }
            const ident_tok = self.peek();
            self.advance();
            return ident_tok;
        },
        else => {
            if (current.type == .THEN or current.type == .RIGHT_BRACE or current.type == .COMMA or current.type == .NEWLINE) {
                return null;
            }
            const tok = self.peek();
            self.advance();
            return tok;
        },
    }
}
