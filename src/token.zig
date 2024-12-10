const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("interpreter.zig").Environment;
const FunctionParam = ast.FunctionParam;

pub const TokenType = enum {
    // single-character tokens
    LEFT_PAREN, // (
    RIGHT_PAREN, // )
    LEFT_BRACE, // {
    RIGHT_BRACE, // }
    LEFT_BRACKET, // [
    RIGHT_BRACKET, // ]
    COMMA, // ,
    COLON, // :
    SEMICOLON, // ;
    MODULO, // %
    HASH, // #
    TILDE, // ~
    QUESTION, // ?

    //one or two character tokens
    DOT, // .
    DOT_DOT, // ..
    AMPERSAND, // &
    ARROW, // =>
    SLASH, // /
    SLASH_EQUAL, // /=
    ASTERISK, // *
    ASTERISK_EQUAL, // *=
    POWER, // ^
    POWER_EQUAL, // ^=
    PLUS, // +
    PLUS_PLUS, // ++
    PLUS_EQUAL, // +=
    MINUS, // -
    MINUS_MINUS, // --
    MINUS_EQUAL, // -=
    MAIN, // ->
    BANG, // !
    BANG_EQUAL, // !=
    GREATER, // >
    GREATER_EQUAL, // >=
    LESS, // <
    LESS_EQUAL, // <=

    // keywords
    VAR, // var
    CONST, // const
    IMPORT, // import
    PUBLIC, // public
    ASSERT, // assert
    RETURN, // return
    RETURNS, // assign an expected return type
    BREAK, // break
    CONTINUE, // continue
    MATCH, // match
    THROW, // throw
    TRY, // try
    CATCH, // catch
    WHILE, // while
    FOR, // for
    FOREACH, // foreach
    XOR, // xor
    EXISTS, // exists ∃
    FORALL, // forall ∀
    FROM, // from
    IN, // in
    AS, // as
    ASYNC, // async
    AWAIT, // await
    TYPEOF, // typeof
    GUIDE, // guide
    SAFE, // safe
    NORMAL, // normal

    // keywords with alternate tokens
    AND_KEYWORD, // and
    AND_SYMBOL, // &&
    OR_KEYWORD, // or
    OR_SYMBOL, // ||
    FN_KEYWORD, // fn
    FUNCTION_KEYWORD, // function
    WHERE_KEYWORD, // where
    WHERE_SYMBOL, // |
    EQUALITY_KEYWORD, // equals
    EQUALITY_SYMBOL, // ==
    ASSIGN_KEYWORD, // is
    ASSIGN_SYMBOL, // =

    // logical operators
    IF, // if
    THEN, // then
    ELSE, // else

    // literals
    IDENTIFIER, // identifier
    SPREAD, // ...
    INT, // integer
    FLOAT, // float
    STRING, // string
    ARRAY, // array
    BOOL, // boolean
    STRUCT, // struct
    ENUM, // enum
    AUTO, // auto
    TUPLE, // tuple
    MAP, // map

    // Type keywords
    INT_TYPE, // int type declaration
    FLOAT_TYPE, // float type declaration
    STRING_TYPE, // string type declaration
    BOOLEAN_TYPE, // bool type declaration
    ARRAY_TYPE, // array type declaration
    STRUCT_TYPE, // struct type declaration
    ENUM_TYPE, // enum type declaration
    AUTO_TYPE, // auto type declaration
    TUPLE_TYPE, // tuple type declaration
    MAP_TYPE, // map type declaration
    NOTHING, // nothing

    EOF, // end of file
};

pub const StructField = struct {
    name: []const u8,
    value: TokenLiteral,
};

pub const TokenLiteral = union(enum) {
    int: i32,
    float: f64,
    string: []const u8,
    boolean: bool,
    nothing: void,
    array: []TokenLiteral,
    tuple: []TokenLiteral,
    struct_value: struct {
        type_name: []const u8,
        fields: []StructField,
    },
    function: struct {
        params: []FunctionParam,
        body: []ast.Stmt,
        closure: *Environment, // Capture the environment where the function was defined
    },
    enum_variant: []const u8,
    map: std.StringHashMap(TokenLiteral),
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: i32,
    column: usize, // array index is usize by default

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: i32, column: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column, // array index is usize by default
        };
    }
};

pub fn deinit(self: *TokenLiteral, allocator: std.mem.Allocator) void {
    switch (self.*) {
        .map => |*m| {
            var iter = m.iterator();
            while (iter.next()) |entry| {
                var value = entry.value_ptr.*;
                value.deinit(allocator);
            }
            m.deinit();
        },
        else => {},
    }
}
