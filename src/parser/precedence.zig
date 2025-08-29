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

const functionExpr = expr_parser.functionExpr;
const parseStructDecl = decl_parser.parseStructDecl;
const enumDeclPrefix = decl_parser.enumDeclPrefix;
const enumMember = Parser.enumMember;

const parseIfExpr = expr_parser.parseIfExpr;
const whileExpr = expr_parser.whileExpr;
const forExpr = expr_parser.forExpr;
const parseMatchExpr = expr_parser.parseMatchExpr;
const parseStructOrMatch = expr_parser.parseStructOrMatch;
const returnExpr = expr_parser.returnExpr;

const arrayType = expr_parser.arrayType;
const typeofExpr = expr_parser.typeofExpr;
const lengthofExpr = expr_parser.lengthofExpr;
const bytesofExpr = expr_parser.bytesofExpr;
const methodCallExpr = Parser.methodCallExpr;

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
    FACTOR = 26, // * / (higher than TERM)
    UNARY = 28, // ! -
    CALL = 30, // . () []
    PRIMARY = 32,
};

pub const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
    associativity: ?Associativity = .LEFT,
};

pub const rules = blk: {
    var r = std.EnumArray(token.TokenType, ParseRule).initFill(ParseRule{});

    // Add rule for entry point keyword
    r.set(.ENTRY, .{ .prefix = functionExpr });

    // Binary operators
    r.set(.PLUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
    r.set(.MINUS, .{ .prefix = unary, .infix = binary, .precedence = .TERM });
    r.set(.ASTERISK, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.SLASH, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.MODULO, .{ .infix = binary, .precedence = .FACTOR });
    r.set(.POWER, .{ .infix = binary, .precedence = .UNARY, .associativity = .RIGHT }); // Right associative for exponentiation

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
    r.set(.ASSIGN, .{ .infix = assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    r.set(.ARRAY_TYPE, .{ .prefix = variable, .infix = fieldAccess });

    // Control flow
    r.set(.IF, .{ .prefix = parseIfExpr });
    r.set(.RETURN, .{ .prefix = returnExpr });

    // Blocks
    r.set(.LEFT_BRACE, .{
        .prefix = braceExpr, // New function that handles both blocks and maps
        .precedence = .NONE,
    });

    // Add function declaration support
    r.set(.FUNCTION, .{ .prefix = null }); // Function declarations are handled as statements, not expressions

    // Add rule for the ? operator with lower precedence
    r.set(.PEEK, .{ .infix = peekValue, .precedence = .CALL });

    // Add loop support
    r.set(.WHILE, .{ .prefix = whileExpr });
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

    // Add enum declaration support using the wrapper
    r.set(.ENUM_TYPE, .{ .prefix = enumDeclPrefix });

    // Add dot prefix rule for enum member access
    r.set(.DOT, .{ .prefix = enumMember, .infix = fieldAccess, .precedence = .CALL });

    // Add match expression support
    r.set(.MATCH, .{ .prefix = parseMatchExpr });

    // Add @methods generic handler
    r.set(.TYPE, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.LENGTH, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.BYTES, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.SLICE, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.SUBSTRING, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Array ops
    r.set(.PUSH, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.POP, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.INSERT, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.REMOVE, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.CLEAR, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.INDEX, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Type conversions
    r.set(.TOSTRING, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.PARSEINT, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.PARSEFLOAT, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.PARSEBYTE, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // String
    r.set(.SPLIT, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.JOIN, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.TRIM, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.LOWER, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.UPPER, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Math
    r.set(.ABS, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.MIN, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.MAX, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.ROUND, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.FLOOR, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.CEIL, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // I/O
    r.set(.READ, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.WRITE, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.EXEC, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.SPAWN, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Control flow
    r.set(.PANIC, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Copy/clone
    r.set(.CLONE, .{ .prefix = methodCallExpr, .precedence = .CALL });
    r.set(.COPY, .{ .prefix = methodCallExpr, .precedence = .CALL });

    // Add cast operator support
    r.set(.AS, .{ .infix = expr_parser.castExpr, .precedence = .CALL });

    // Add input support
    r.set(.INPUT, .{ .prefix = Parser.input, .precedence = .PRIMARY });

    // Add field access support
    r.set(.FIELD_ACCESS, .{ .prefix = variable, .precedence = .PRIMARY });

    // Add field access support
    r.set(.FIELD_ACCESS, .{ .prefix = variable, .precedence = .PRIMARY });

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
