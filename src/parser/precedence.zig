const std = @import("std");
const token = @import("../types/token.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("./expression_parser.zig");
const decl_parser = @import("./declaration_parser.zig");
const quantifer_parser = @import("./quantifer_parser.zig");
const ast = @import("../ast/ast.zig");
const Reporting = @import("../utils/reporting.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

// Import all the parsing functions used in rules
const unary = expr_parser.unary;
const binary = expr_parser.binary;
const literal = expr_parser.literal;
const grouping = expr_parser.grouping;
const parseArrayLiteral = expr_parser.parseArrayLiteral;
const call = Parser.call;
const index = Parser.index;
const variable = expr_parser.variable;
const assignment = Parser.assignment;
const fieldAccess = Parser.fieldAccess;
const print = Parser.print;
const braceExpr = expr_parser.braceExpr;

const parseStructDecl = decl_parser.parseStructDecl;
const enumDeclPrefix = decl_parser.enumDeclPrefix;
const enumMember = Parser.enumMember;

const parseIfExpr = expr_parser.parseIfExpr;
const whileExpr = expr_parser.whileExpr;
const forExpr = expr_parser.forExpr;
const parseMatchExpr = expr_parser.parseMatchExpr;
const parseStructOrMatch = expr_parser.parseStructOrMatch;
const returnExpr = expr_parser.returnExpr;
const parseBreakExpr = expr_parser.parseBreakExpr;

const arrayType = expr_parser.arrayType;
const typeofExpr = expr_parser.typeofExpr;
const lengthofExpr = expr_parser.lengthofExpr;
const internalCallExpr = Parser.internalCallExpr;

const existentialQuantifier = quantifer_parser.existentialQuantifier;
const universalQuantifier = quantifer_parser.universalQuantifier;
const inOperator = quantifer_parser.inOperator;

const arrayPush = Parser.arrayPush;
const arrayLength = Parser.arrayLength;
const arrayPop = Parser.arrayPop;
const arrayIsEmpty = Parser.arrayIsEmpty;
const arrayConcat = Parser.arrayConcat;

pub const ParseFn = *const fn (*Parser, ?*ast.Expr, Precedence) ErrorList!?*ast.Expr;

pub const Associativity = enum {
    LEFT,
    RIGHT,
    NONE,
};

pub const Precedence = enum(u8) {
    NONE = 0,
    ASSIGNMENT = 2, // =
    OR = 4, // or
    AND = 6, // and
    XOR = 8, // xor
    NAND = 10, // ↑
    NOR = 12, // ↓
    IFF = 14, // iff
    IMPLIES = 16, // →
    EQUALITY = 18, // == !=
    COMPARISON = 20, // < > <= >=
    QUANTIFIER = 22, // ∃ ∀
    TERM = 24, // + -
    FACTOR = 26, // * / % (higher than TERM)
    EXPONENT = 28, // ** (higher than FACTOR)
    UNARY = 30, // ! -
    CALL = 32, // . () []
    PRIMARY = 34,
};

pub const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
    associativity: ?Associativity = .LEFT,
};

pub const rules = blk: {
    var r = std.EnumArray(token.TokenType, ParseRule).initFill(ParseRule{});

    // Binary operators
    r.set(.PLUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
    r.set(.MINUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
    r.set(.ASTERISK, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.SLASH, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.MODULO, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.POWER, .{ .infix = binary, .precedence = .EXPONENT, .associativity = .RIGHT }); // Right associative for exponentiation

    // Add compound assignment operators
    r.set(.PLUS_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.MINUS_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.POWER_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.ASTERISK_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.SLASH_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    // r.set(.MODULO_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });

    // Comparison operators
    r.set(.EQUALITY, .{ .infix = binary, .precedence = .EQUALITY });
    r.set(.BANG_EQUAL, .{ .infix = binary, .precedence = .EQUALITY });
    r.set(.LESS, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.LESS_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.GREATER, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.GREATER_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });

    // Logical operators
    r.set(.AND, .{ .infix = logical, .precedence = .AND });
    r.set(.OR, .{ .infix = logical, .precedence = .OR });
    r.set(.NAND, .{ .infix = logical, .precedence = .NAND });
    r.set(.NOR, .{ .infix = logical, .precedence = .NOR });
    r.set(.XOR, .{ .infix = logical, .precedence = .XOR });
    r.set(.IFF, .{ .infix = logical, .precedence = .IFF });
    r.set(.NOT, .{ .prefix = unary, .precedence = .UNARY });
    r.set(.NOT_PARADOXICAL, .{ .prefix = unary, .precedence = .UNARY });
    r.set(.BANG, .{ .prefix = unary, .precedence = .UNARY });
    r.set(.IMPLIES, .{ .infix = logical, .precedence = .IMPLIES });

    // Literals
    r.set(.INT, .{ .prefix = literal });
    r.set(.BYTE, .{ .prefix = literal });
    r.set(.FLOAT, .{ .prefix = literal });
    r.set(.STRING, .{ .prefix = literal });
    r.set(.LOGIC, .{ .prefix = literal });
    r.set(.NOTHING, .{ .prefix = literal });

    // Grouping
    r.set(.LEFT_PAREN, .{ .prefix = grouping, .infix = call, .precedence = .CALL });
    r.set(.LEFT_BRACKET, .{ .prefix = parseArrayLiteral, .infix = index, .precedence = .CALL });

    // Variables and assignment
    r.set(.VAR, .{ .prefix = variable });
    r.set(.CONST, .{ .prefix = variable });
    r.set(.IDENTIFIER, .{ .prefix = variable });
    r.set(.THIS, .{ .prefix = variable });
    r.set(.ASSIGN, .{ .infix = assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.ARRAY_TYPE, .{ .prefix = variable, .infix = fieldAccess });

    // Control flow
    r.set(.IF, .{ .prefix = parseIfExpr });
    r.set(.RETURN, .{ .prefix = returnExpr });
    r.set(.BREAK, .{ .prefix = parseBreakExpr });

    // Blocks
    r.set(.LEFT_BRACE, .{
        .prefix = braceExpr, // New function that handles both blocks and maps
        .precedence = .NONE,
    });

    // Add function declaration support
    r.set(.FUNCTION, .{ .prefix = null }); // Function declarations are handled as statements, not expressions

    r.set(.PEEK, .{ .infix = peekValue, .precedence = .CALL });

    // Add loop support (unified Loop is produced by these parsers)
    r.set(.WHILE, .{ .prefix = whileExpr });
    r.set(.DO, .{ .prefix = expr_parser.doExpr });
    r.set(.FOR, .{ .prefix = forExpr });

    // Add struct support
    r.set(.STRUCT, .{ .prefix = parseStructDecl });
    r.set(.DOT, .{ .infix = fieldAccess, .precedence = .CALL });

    // Add struct declaration rule in the rules block
    r.set(.STRUCT_TYPE, .{ .prefix = parseStructDecl });

    // Add struct instantiation or match lookahead support
    r.set(.IDENTIFIER, .{ .prefix = parseStructOrMatch });
    r.set(.DOT, .{ .infix = fieldAccess, .precedence = .CALL });

    // Add quantifier operators
    r.set(.EXISTS, .{ .prefix = existentialQuantifier, .precedence = .QUANTIFIER });
    r.set(.FORALL, .{ .prefix = universalQuantifier, .precedence = .QUANTIFIER });

    // Add array type support
    r.set(.ARRAY_TYPE, .{ .prefix = arrayType });

    // Add 'in' keyword support for quantifiers
    r.set(.IN, .{ .infix = inOperator, .precedence = .TERM });

    // Add range operator support
    r.set(.RANGE, .{ .infix = rangeExpr, .precedence = .TERM });

    // Add enum declaration support using the wrapper
    r.set(.ENUM_TYPE, .{ .prefix = enumDeclPrefix });

    // Add dot prefix rule for enum member access
    r.set(.DOT, .{ .prefix = enumMember, .infix = fieldAccess, .precedence = .CALL });

    // Add match expression support
    r.set(.MATCH, .{ .prefix = parseMatchExpr });

    // Add @methods generic handler
    r.set(.TYPE, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.LENGTH, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.SLICE, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // Array ops
    r.set(.PUSH, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.POP, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.INSERT, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.REMOVE, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.CLEAR, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.FIND, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // Type conversions
    r.set(.TOSTRING, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.TOINT, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.TOFLOAT, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.TOBYTE, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // I/O
    r.set(.READ, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.WRITE, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.PRINT, .{ .prefix = internalCallExpr, .precedence = .CALL });
    r.set(.SYSCALL, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // Control flow
    r.set(.PANIC, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // Copy/clone
    r.set(.SHALLOW, .{ .prefix = internalCallExpr, .precedence = .CALL });

    // Add input support
    r.set(.INPUT, .{ .prefix = Parser.input, .precedence = .PRIMARY });

    // Add cast operator support
    r.set(.AS, .{ .infix = expr_parser.castExpr, .precedence = .CALL });

    // Add field access support
    r.set(.FIELD_ACCESS, .{ .prefix = variable, .precedence = .PRIMARY });

    // Add field access support
    r.set(.FIELD_ACCESS, .{ .prefix = variable, .precedence = .PRIMARY });

    // Add increment/decrement operators
    r.set(.INCREMENT, .{ .prefix = expr_parser.prefixIncrement, .infix = expr_parser.postfixIncrement, .precedence = .UNARY, .associativity = .RIGHT });
    r.set(.DECREMENT, .{
        .prefix = expr_parser.prefixDecrement,
        .infix = expr_parser.postfixDecrement,
        .precedence = .UNARY,
        .associativity = .RIGHT,
    });

    break :blk r;
};

pub fn getRule(token_type: token.TokenType) ParseRule {
    return rules.get(token_type);
}

pub fn parsePrecedence(self: *Parser, precedence_level: Precedence) ErrorList!?*ast.Expr {
    const prefix_rule = getRule(self.peek().type).prefix;
    if (prefix_rule == null) {
        return null;
    }

    var left = try prefix_rule.?(self, null, precedence_level) orelse return null;
    errdefer {
        left.deinit(self.allocator);
        self.allocator.destroy(left);
    }

    while (true) {
        const next_rule = getRule(self.peek().type);

        if (next_rule.infix == null) {
            break;
        }

        const rule_precedence = next_rule.precedence;

        if (@intFromEnum(rule_precedence) < @intFromEnum(precedence_level)) {
            break;
        }

        if (self.peek().type != .LEFT_PAREN and self.peek().type != .LEFT_BRACKET) {
            self.advance();
        }

        var new_precedence = rule_precedence;
        if (next_rule.associativity != .RIGHT and rule_precedence != .PRIMARY) {
            const current_level = @intFromEnum(rule_precedence);
            const next_level = current_level + 2;
            if (next_level > @intFromEnum(Precedence.PRIMARY)) {
                new_precedence = Precedence.PRIMARY;
            } else {
                new_precedence = @as(Precedence, @enumFromInt(next_level));
            }
        }

        left = try next_rule.infix.?(self, left, new_precedence) orelse {
            left.deinit(self.allocator);
            self.allocator.destroy(left);
            return null;
        };
    }

    return left;
}

fn compound_assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.InvalidAssignmentTarget;

    // Verify left side is a valid assignment target
    const is_valid_target = switch (left.?.data) {
        .Variable => true,
        .Index => true,
        else => false,
    };

    if (!is_valid_target) {
        return error.InvalidAssignmentTarget;
    }

    const operator = self.tokens[self.current - 1];
    const value = try parsePrecedence(self, .ASSIGNMENT) orelse return error.ExpectedExpression;

    // Create compound assignment expression
    const compound_expr = try self.allocator.create(ast.Expr);

    compound_expr.* = switch (left.?.data) {
        .Variable => |v| .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(operator),
            },
            .data = .{ .CompoundAssign = .{
                .name = v,
                .operator = operator,
                .value = value,
            } },
        },
        .Index => |idx| .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(operator),
            },
            .data = .{
                .IndexAssign = .{
                    .array = idx.array,
                    .index = idx.index,
                    .value = blk: {
                        // For compound assignments, create the proper binary expression
                        // Convert += to + etc. (only for enabled compound operators)
                        const binary_op = switch (operator.type) {
                            .PLUS_EQUAL => token.TokenType.PLUS,
                            .MINUS_EQUAL => token.TokenType.MINUS,
                            .POWER_EQUAL => token.TokenType.POWER,
                            else => unreachable,
                        };

                        // Create array[index] expression for the left side
                        const array_access = try self.allocator.create(ast.Expr);
                        array_access.* = .{
                            .base = .{
                                .id = ast.generateNodeId(),
                                .span = ast.SourceSpan.fromToken(operator),
                            },
                            .data = .{ .Index = .{
                                .array = idx.array,
                                .index = idx.index,
                            } },
                        };

                        // Create binary expression: array[index] + value
                        const binary_expr = try self.allocator.create(ast.Expr);
                        binary_expr.* = .{
                            .base = .{
                                .id = ast.generateNodeId(),
                                .span = ast.SourceSpan.fromToken(operator),
                            },
                            .data = .{ .Binary = .{
                                .left = array_access,
                                .operator = .{
                                    .type = binary_op,
                                    .lexeme = operator.lexeme,
                                    .literal = operator.literal,
                                    .line = operator.line,
                                    .column = operator.column,
                                    .file = operator.file,
                                },
                                .right = value,
                            } },
                        };

                        break :blk binary_expr;
                    },
                },
            },
        },
        else => unreachable,
    };

    return compound_expr;
}

fn logical(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
    const operator = self.tokens[self.current - 1]; // Get the operator token (AND/OR)
    const right = try parsePrecedence(self, precedence) orelse return error.ExpectedExpression;
    const logical_expr = try self.allocator.create(ast.Expr);
    logical_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(operator),
        },
        .data = .{
            .Logical = .{
                .left = left.?,
                .operator = operator,
                .right = right,
            },
        },
    };
    return logical_expr;
}

fn peekValue(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    // Get the variable name if this is a variable expression
    var name_token: ?[]const u8 = null;
    if (left.?.data == .Variable) {
        name_token = left.?.data.Variable.lexeme;
    }

    // Use the '?' token (previous) for accurate location info. After infix dispatch,
    // parsePrecedence has already advanced past '?', so peek() would point at the next token
    // (possibly EOF/newline) which may have empty file information.
    const qm_token = self.previous();

    // Create the peekion expression
    const peek_expr = try self.allocator.create(ast.Expr);
    peek_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = .{
                .location = .{
                    .file = qm_token.file,
                    .range = .{
                        .start_line = @intCast(qm_token.line),
                        .start_col = qm_token.column,
                        .end_line = @intCast(qm_token.line),
                        .end_col = qm_token.column + qm_token.lexeme.len,
                    },
                },
            },
        },
        .data = .{
            // Use regular Peek for non-struct values
            .Peek = .{
                .expr = left.?,
                .location = .{
                    .file = qm_token.file,
                    .range = .{
                        .start_line = @intCast(qm_token.line),
                        .start_col = qm_token.column,
                        .end_line = @intCast(qm_token.line),
                        .end_col = qm_token.column + qm_token.lexeme.len,
                    },
                },
                .variable_name = name_token,
            },
        },
    };

    return peek_expr;
}

fn printValue(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    // Get the variable name if this is a variable expression
    var name_token: ?[]const u8 = null;
    if (left.?.data == .Variable) {
        name_token = left.?.data.Variable.lexeme;
    }

    // Use the '?' token (previous) for accurate location info. After infix dispatch,
    // parsePrecedence has already advanced past '?', so peek() would point at the next token
    // (possibly EOF/newline) which may have empty file information.
    const qm_token = self.previous();

    // Create the print expression
    const print_expr = try self.allocator.create(ast.Expr);
    print_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = .{
                .location = .{
                    .file = qm_token.file,
                    .range = .{
                        .start_line = @intCast(qm_token.line),
                        .start_col = qm_token.column,
                        .end_line = @intCast(qm_token.line),
                        .end_col = qm_token.column + qm_token.lexeme.len,
                    },
                },
            },
        },
        .data = .{
            // Use regular Print for non-struct values
            .Print = .{
                .expr = left.?,
                .format_template = null,
                .format_parts = null,
                .arguments = null,
                .placeholder_indices = null,
            },
        },
    };

    return print_expr;
}

fn rangeExpr(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    if (left == null) return error.ExpectedLeftOperand;

    const operator = self.tokens[self.current - 1]; // Get the 'to' token
    const right = try parsePrecedence(self, .TERM) orelse return error.ExpectedRightOperand;

    const range_expr = try self.allocator.create(ast.Expr);
    range_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(operator),
        },
        .data = .{
            .Range = .{
                .start = left.?,
                .end = right,
            },
        },
    };

    return range_expr;
}
