const std = @import("std");
const token = @import("../token.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("expression_parser.zig");
const decl_parser = @import("declaration_parser.zig");
const quantifer_parser = @import("quantifer_parser.zig");
const ast = @import("../ast.zig");
const ErrorList = @import("../reporting.zig").ErrorList;

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
const parseTuple = Parser.parseTuple;

const functionExpr = expr_parser.functionExpr;
const parseStructDecl = decl_parser.parseStructDecl;
const enumDeclPrefix = decl_parser.enumDeclPrefix;
const enumMember = Parser.enumMember;

const parseIfExpr = expr_parser.parseIfExpr;
const whileExpr = expr_parser.whileExpr;
const forExpr = expr_parser.forExpr;
const parseMatchExpr = expr_parser.parseMatchExpr;

const arrayType = expr_parser.arrayType;
const typeofExpr = expr_parser.typeofExpr;

const existentialQuantifier = quantifer_parser.existentialQuantifier;
const universalQuantifier = quantifer_parser.universalQuantifier;
const inOperator = quantifer_parser.inOperator;

pub const ParseFn = *const fn (*Parser, ?*ast.Expr, Precedence) ErrorList!?*ast.Expr;

pub const Associativity = enum {
    LEFT,
    RIGHT,
    NONE,
};

pub const Precedence = enum(u8) {
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

pub const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
    associativity: ?Associativity = .LEFT,
};

pub const rules = blk: {
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

pub fn getRule(token_type: token.TokenType) ParseRule {
    return rules.get(token_type);
}

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
    const value = try parsePrecedence(self, .ASSIGNMENT) orelse return error.ExpectedExpression;

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

fn logical(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("Parsing logical expression\n", .{});
    }

    const operator = self.tokens[self.current - 1]; // Get the operator token (AND/OR)
    const right = try parsePrecedence(self, precedence) orelse return error.ExpectedExpression;
    const logical_expr = try self.allocator.create(ast.Expr);
    logical_expr.* = .{ .Logical = .{
        .left = left.?,
        .operator = operator,
        .right = right,
    } };
    return logical_expr;
}

pub fn parsePrecedence(self: *Parser, prec: Precedence) ErrorList!?*ast.Expr {
    if (self.debug_enabled) {
        std.debug.print("\nParsing with precedence: {}\n", .{@intFromEnum(prec)});
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
    const prefix_rule = getRule(self.peek().type).prefix;
    if (prefix_rule == null) {
        if (self.debug_enabled) {
            std.debug.print("No prefix rule for token: {s}\n", .{@tagName(self.peek().type)});
        }
        return null;
    }

    // Parse prefix expression
    var left = try prefix_rule.?(self, null, prec);
    if (left == null) return null;

    // Keep parsing infix expressions as long as we have higher precedence
    while (@intFromEnum(prec) <= @intFromEnum(getRule(self.peek().type).precedence)) {
        const infix_rule = getRule(self.peek().type).infix;
        if (infix_rule == null) break;

        if (self.debug_enabled) {
            std.debug.print("Found infix operator: {s}\n", .{@tagName(self.peek().type)});
        }

        // Don't advance here for function calls or indexing operations
        if (self.peek().type != .LEFT_PAREN and self.peek().type != .LEFT_BRACKET) {
            self.advance();
        }

        left = try infix_rule.?(self, left, prec);
    }

    return left;
}