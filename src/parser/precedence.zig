const std = @import("std");
const token = @import("../lexer/token.zig");
const Parser = @import("./parser_types.zig").Parser;
const expr_parser = @import("./expression_parser.zig");
const decl_parser = @import("./declaration_parser.zig");
const quantifer_parser = @import("./quantifer_parser.zig");
const ast = @import("../ast/ast.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;

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
    TRY = 1,
    ASSIGNMENT = 2, // =
    OR = 3, // or
    AND = 4, // and
    XOR = 5, // xor
    NAND = 6, // ↑
    NOR = 7, // ↓
    IFF = 8, // iff
    EQUALITY = 9, // == !=
    COMPARISON = 10, // < > <= >=
    QUANTIFIER = 11, // ∃ ∀
    TERM = 12, // + -
    FACTOR = 13, // * /
    UNARY = 14, // ! -
    CALL = 15, // . () []
    PRIMARY = 16,
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
    r.set(.MINUS_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    // r.set(.ASTERISK_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    // r.set(.SLASH_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });
    // r.set(.MODULO_EQUAL, .{ .infix = compound_assignment, .precedence = .ASSIGNMENT, .associativity = .RIGHT });

    // Comparison operators
    r.set(.EQUALITY, .{ .infix = binary, .precedence = .EQUALITY });
    r.set(.BANG_EQUAL, .{ .infix = binary, .precedence = .EQUALITY });
    r.set(.LESS, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.LESS_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.GREATER, .{ .infix = binary, .precedence = .COMPARISON });
    r.set(.GREATER_EQUAL, .{ .infix = binary, .precedence = .COMPARISON });

    // Logical operators
    r.set(.AND_KEYWORD, .{ .infix = logical, .precedence = .AND });
    r.set(.AND_LOGICAL, .{ .infix = logical, .precedence = .AND });
    r.set(.OR_KEYWORD, .{ .infix = logical, .precedence = .OR });
    r.set(.OR_LOGICAL, .{ .infix = logical, .precedence = .OR });
    r.set(.NAND, .{ .infix = logical, .precedence = .NAND });
    r.set(.NOR, .{ .infix = logical, .precedence = .NOR });
    r.set(.XOR, .{ .infix = logical, .precedence = .XOR });
    r.set(.IFF, .{ .infix = logical, .precedence = .IFF });
    r.set(.NOT_LOGICAL, .{ .prefix = unary, .precedence = .UNARY });
    r.set(.NOT_KEYWORD, .{ .prefix = unary, .precedence = .UNARY });
    r.set(.NOT_TRANCENDENTAL, .{ .prefix = unary, .precedence = .UNARY });

    // Literals
    r.set(.INT, .{ .prefix = literal });
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

    // Blocks
    r.set(.LEFT_BRACE, .{
        .prefix = braceExpr, // New function that handles both blocks and maps
        .precedence = .NONE,
    });

    // Add function declaration support
    r.set(.FUNCTION, .{ .prefix = functionExpr });

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

    // Add input support
    r.set(.INPUT, .{ .prefix = Parser.input, .precedence = .PRIMARY });

    // Add tuple support
    r.set(.LEFT_PAREN, .{ .prefix = parseTuple, .infix = call, .precedence = .CALL });

    break :blk r;
};

pub fn getRule(token_type: token.TokenType) ParseRule {
    return rules.get(token_type);
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
        return error.BangNegationNotSupported;
    }

    // Get the prefix rule for the current token
    const prefix_rule = getRule(self.peek().type).prefix;
    if (prefix_rule == null) {
        if (self.debug_enabled) {
            std.debug.print("No prefix rule for token: {s}\n", .{@tagName(self.peek().type)});
        }
        return null;
    }

    // Parse prefix expression with error handling
    var left = try prefix_rule.?(self, null, prec) orelse return null;
    errdefer {
        if (self.debug_enabled) {
            std.debug.print("Cleaning up left expression in parsePrecedence\n", .{});
        }
        left.deinit(self.allocator);
        self.allocator.destroy(left);
    }

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

        const new_left = try infix_rule.?(self, left, prec);
        if (new_left == null) {
            // Clean up left if infix parsing fails
            left.deinit(self.allocator);
            self.allocator.destroy(left);
            return null;
        }
        left = new_left.?;
    }

    return left;
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

    // Create compound assignment expression
    const compound_expr = try self.allocator.create(ast.Expr);

    // Create the compound assignment value
    const compound_value = try self.allocator.create(ast.Expr);
    compound_value.* = .{
        .CompoundAssign = .{
            .name = self.tokens[self.current - 2], // Use the token before the operator
            .operator = operator,
            .value = value,
        },
    };

    compound_expr.* = switch (left.?.*) {
        .Variable => |v| .{ .CompoundAssign = .{
            .name = v,
            .operator = operator,
            .value = value,
        } },
        .Index => |idx| .{ .IndexAssign = .{
            .array = idx.array,
            .index = idx.index,
            .value = compound_value,
        } },
        else => unreachable,
    };

    return compound_expr;
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
