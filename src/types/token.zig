const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenLiteral = @import("./types.zig").TokenLiteral;

pub const TokenType = enum {
    // single-character tokens
    LEFT_BRACE, // {
    RIGHT_BRACE, // }
    LEFT_BRACKET, // [
    RIGHT_BRACKET, // ]
    COMMA, // ,
    MODULO, // %
    HASH, // #
    TILDE, // ~
    PIPE, // |
    SEMICOLON, // ;
    PEEK, // ?
    CARET, // ^

    //one or two character tokens
    DOT, // .
    DOT_DOT, // ..
    AMPERSAND, // &
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
    RETURNS, // returns
    BANG, // !
    BANG_EQUAL, // !=
    GREATER, // >
    GREATER_EQUAL, // >=
    LESS, // <
    LESS_EQUAL, // <=
    TYPE_SYMBOL, // ::
    LEFT_PAREN, // (
    RIGHT_PAREN, // )

    // keywords
    VAR, // var
    CONST, // const
    IMPORT, // import
    MODULE, // module
    PUBLIC, // public
    INPUT, // input
    ASSERT, // assert
    RETURN, // return
    BREAK, // break
    CONTINUE, // continue
    MATCH, // match
    WHILE, // while
    DO, // do
    FOR, // for
    EACH, // each
    IN, // in
    AT, // at
    XOR, // xor
    NOT_PARADOXICAL, // ⊖
    EXISTS, // exists ∃
    FORALL, // forall ∀
    FROM, // from
    AS, // as
    NOT, // not ¬
    IMPLIES, // →
    ASYNC, // async
    AWAIT, // await
    ENTRY, // entry
    RANGE, // to

    // compiler level methods (@methods)
    TYPE, // typeof / type
    LENGTH, // length
    BYTES, // bytes
    SLICE, // slice
    SUBSTRING, // substring
    PUSH, // push
    POP, // pop
    INSERT, // insert
    REMOVE, // remove
    CLEAR, // clear
    INDEX, // indexOf / index
    TOSTRING, // toString / string
    PARSEINT, // parseInt / int
    PARSEFLOAT, // parseFloat / float
    PARSEBYTE, // parseByte (legacy)
    SPLIT, // split
    JOIN, // join
    TRIM, // trim
    LOWER, // lower
    UPPER, // upper
    ABS, // abs
    MIN, // min
    MAX, // max
    ROUND, // round
    FLOOR, // floor
    CEIL, // ceil
    READ, // read
    WRITE, // write
    EXEC, // exec
    SPAWN, // spawn
    PANIC, // panic
    CLONE, // clone
    COPY, // copy
    PRINT, // print

    // keywords with alternate tokens
    AND, // and ∧
    OR, // or ∨
    WHERE, // where
    EQUALITY, // ==
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
    NEWLINE, // newline
    CONTINUE_LINE, // continue line

    // atomic types
    INT, // integer
    BYTE, // hex literal u8
    FLOAT, // float
    STRING, // string
    TETRA, // tetra
    ALIAS, // alias
    NOTHING, // nothing
    INT_TYPE, // int type declaration
    BYTE_TYPE, // 8-bit unsigned integer type declaration
    FLOAT_TYPE, // float type declaration
    STRING_TYPE, // string type declaration
    TETRA_TYPE, // tetra type declaration
    ALIAS_TYPE, // alias type declaration
    NOTHING_TYPE, // nothing type declaration

    // molecular types
    STRUCT, // struct
    ENUM, // enum
    MAP, // map
    UNION, // union
    ARRAY, // array
    FUNCTION, // function
    INTRINSIC, // intrinsic
    ARRAY_TYPE, // array type declaration
    STRUCT_TYPE, // struct type declaration
    ENUM_TYPE, // enum type declaration
    MAP_TYPE, // map type declaration
    UNION_TYPE, // union type declaration
    FUNCTION_TYPE, // function type declaration
    INTRINSIC_TYPE, // intrinsic type declaration

    // type helpers
    FIELD_ACCESS, // field access
    LOGIC, // logic
    CUSTOM, // custom

    // Type keywords

    EOF, // end of file
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: usize,
    column: usize,
    file: []const u8, // Add file path for better error reporting

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize, column: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column,
            .file = "", // Default to empty string, will be set by lexer
        };
    }

    pub fn initWithFile(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize, column: usize, file: []const u8) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column,
            .file = file,
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

// Add a helper function to convert TokenType to TypeInfo
pub fn convertTokenTypeToTypeInfo(token_type: TokenType) ast.TypeInfo {
    return switch (token_type) {
        .INT => ast.TypeInfo{ .base = .Int, .is_mutable = true },
        .BYTE => ast.TypeInfo{ .base = .Byte, .is_mutable = true },
        .FLOAT => ast.TypeInfo{ .base = .Float, .is_mutable = true },
        .STRING => ast.TypeInfo{ .base = .String, .is_mutable = true },
        .TETRA => ast.TypeInfo{ .base = .Tetra, .is_mutable = true },
        .ARRAY => ast.TypeInfo{ .base = .Array, .is_mutable = true },
    };
}
