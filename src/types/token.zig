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
    INCREMENT, // ++
    PLUS_EQUAL, // +=
    MINUS, // -
    DECREMENT, // --
    MINUS_EQUAL, // -=
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
    RETURN, // return
    BREAK, // break
    FUNCTION, // function
    METHOD, // method
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
    FROM, // from import
    AS, // as narrow
    TO, // to spread
    NOT, // not ¬
    IMPLIES, // →
    ASYNC, // async
    AWAIT, // await
    ENTRY, // entry
    RANGE, // to
    RETURNS, // returns
    THIS, // this

    // compiler level methods (@methods)
    TYPE, // @type        - get type name as string
    INPUT, // @input       - read from stdin
    LENGTH, // @length      - get length of string/array
    SLICE, // @slice       - get substring/subarray
    PUSH, // @push        - add to end of array
    POP, // @pop         - remove from end of array
    INSERT, // @insert      - insert into array at index
    REMOVE, // @remove      - remove from array at index
    CLEAR, // @clear       - clear array
    FIND, // @find        - find index of value
    ASSERT, // @assert      - assert condition with message
    PANIC, // @panic       - panic with message
    PRINT, // @print       - print to stdout
    SYSCALL, // @syscall     - raw system call
    SHALLOW, // @shallow     - shallow copy
    READ, // @read        - read file
    WRITE, // @write       - write file
    TOSTRING, // @string      - convert to string
    TOINT, // @int         - convert to int (returns int | ParseError)
    TOFLOAT, // @float       - convert to float (returns float | ParseError)
    TOBYTE, // @bytes       - get bytes of string/array
    BYTES, // bytes we should move this to STD library when it is implemented
    TIME, // @time       - get current time
    RANDOM, // @random       - get random number

    // SPLIT, // split
    // JOIN, // join
    // SUBSTRING, // substring
    // TRIM, // trim
    // LOWER, // lower
    // UPPER, // upper
    // ABS, // abs
    // MIN, // min
    // MAX, // max
    // ROUND, // round
    // FLOOR, // floor
    // CEIL, // ceil
    // EXEC, // exec
    // SPAWN, // spawn

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
    NOTHING_TYPE, // nothing type declaration

    // molecular types
    STRUCT, // struct
    ENUM, // enum
    MAP, // map
    UNION, // union
    ARRAY, // array
    ARRAY_TYPE, // array type declaration
    STRUCT_TYPE, // struct type declaration
    ENUM_TYPE, // enum type declaration
    MAP_TYPE, // map type declaration
    UNION_TYPE, // union type declaration

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
