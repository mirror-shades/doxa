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
    POWER, // **
    POWER_EQUAL, // **=
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
    TYPE_SYMBOL, // ::

    // keywords
    VAR, // var
    CONST, // const
    IMPORT, // import
    INPUT, // input
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
    NOT_TRANCENDENTAL, // ⊖
    EXISTS, // exists ∃
    FORALL, // forall ∀
    FROM, // from
    IN, // in
    AS, // as
    NOT_KEYWORD, // not
    NOT_LOGICAL, // ¬
    ASYNC, // async
    AWAIT, // await
    TYPEOF, // typeof
    GUIDE, // guide
    SAFE, // safe
    NORMAL, // normal

    // keywords with alternate tokens
    AND_KEYWORD, // and
    AND_LOGICAL, // ∧
    OR_KEYWORD, // or
    OR_LOGICAL, // ∨
    FUNCTION, // fn
    WHERE, // where
    EQUALITY, // equals
    ASSIGN, // is

    // logical operators
    IF, // if
    IFF, // iff
    THEN, // then
    ELSE, // else
    NAND, // ↑
    NOR, // ↓

    // literals
    IDENTIFIER, // identifier
    SPREAD, // ...
    INT, // integer
    U8, // 8-bit unsigned integer
    FLOAT, // float
    STRING, // string
    ARRAY, // array
    BOOL, // boolean
    TETRA, // tetra
    LOGIC, // logic
    STRUCT, // struct
    ENUM, // enum
    AUTO, // auto
    TUPLE, // tuple
    MAP, // map

    // Type keywords
    INT_TYPE, // int type declaration
    U8_TYPE, // 8-bit unsigned integer type declaration
    FLOAT_TYPE, // float type declaration
    STRING_TYPE, // string type declaration
    BOOLEAN_TYPE, // bool type declaration
    TETRA_TYPE, // tetra type declaration
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

pub const Tetra = enum {
    true,
    false,
    both,
    neither,
};

pub const Boolean = enum {
    true,
    false,
};

pub const TokenLiteral = union(enum) {
    int: i32,
    u8: u8,
    float: f64,
    string: []const u8,
    boolean: Boolean,
    tetra: Tetra,
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
