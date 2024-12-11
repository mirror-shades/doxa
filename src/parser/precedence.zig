const std = @import("std");
const token = @import("../token.zig");
const Parser = @import("./parser.zig").Parser;
const expr_parser = @import("expression_parser.zig");
const decl_parser = @import("declaration_parser.zig");
const control_parser = @import("control_parser.zig");
const type_parser = @import("type_parser.zig");
const quantifier_parser = @import("quantifier_parser.zig");

// Import all the parsing functions used in rules
const unary = expr_parser.unary;
const binary = expr_parser.binary;
const compound_assignment = expr_parser.compound_assignment;
const logical = expr_parser.logical;
const literal = expr_parser.literal;
const grouping = expr_parser.grouping;
const parseArrayLiteral = expr_parser.parseArrayLiteral;
const call = expr_parser.call;
const index = expr_parser.index;
const variable = expr_parser.variable;
const assignment = expr_parser.assignment;
const fieldAccess = expr_parser.fieldAccess;
const print = expr_parser.print;
const braceExpr = expr_parser.braceExpr;
const parseTuple = expr_parser.parseTuple;

const functionExpr = decl_parser.functionExpr;
const parseStructDecl = decl_parser.parseStructDecl;
const enumDeclPrefix = decl_parser.enumDeclPrefix;
const enumMember = decl_parser.enumMember;

const parseIfExpr = control_parser.parseIfExpr;
const whileExpr = control_parser.whileExpr;
const forExpr = control_parser.forExpr;
const parseMatchExpr = control_parser.parseMatchExpr;

const arrayType = type_parser.arrayType;
const typeofExpr = type_parser.typeofExpr;

const existentialQuantifier = quantifier_parser.existentialQuantifier;
const universalQuantifier = quantifier_parser.universalQuantifier;
const inOperator = quantifier_parser.inOperator;

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
    prefix: ?Parser.ParseFn = null,
    infix: ?Parser.ParseFn = null,
    precedence: Precedence = .NONE,
    associativity: ?Parser.Associativity = .LEFT,
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
