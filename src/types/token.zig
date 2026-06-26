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
    TILDE, // ~
    PIPE, // |
    SEMICOLON, // ;
    PEEK, // ?
    CARET, // ^

    //one or two character tokens
    DOT, // .
    DOT_DOT, // ..
    SLASH, // /
    SLASH_EQUAL, // /=
    DOUBLE_SLASH, // //
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
    ZIG, // zig
    ZIG_BLOCK, // zig { ... } payload (raw zig source)
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
    UNREACHABLE, // unreachable
    DEFER, // defer
    LIFT, // lift

    // built-in methods
    TYPE, // @type        - get type name as string
    LENGTH, // @length      - get length of string/array
    PUSH, // @push        - add to end of string/array
    POP, // @pop         - remove from end of string/array
    PRINT, // @print       - print to stdout
    FIND, // @find        - find first occurrence of substring/element in string/array
    CLEAR, // @clear       - clear string/array
    TOSTRING, // @string      - convert to string
    TOINT, // @int         - convert to int (panics on failure)
    TOFLOAT, // @float       - convert to float (panics on failure)
    TOBYTE, // @byte       - convert to byte (panics on failure)
    PACK, // @pack       - pack byte[] → string
    UNPACK, // @unpack     - unpack string → byte[]
    INSERT, // @insert      - insert into string/array at index
    REMOVE, // @remove      - remove from string/array at index
    SLICE, // @slice       - get substring/subarray
    ASSERT, // @assert      - assert condition with message
    PANIC, // @panic       - panic with message
    EXIT, // @exit     - exit program
    STD, // @std        - get path to standard library

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

    // scalar types
    INT, // integer
    BYTE, // hex literal u8
    FLOAT, // float
    STRING, // double-quoted string
    TETRA, // tetra
    ENUM, // enum
    NOTHING, // nothing
    INT_TYPE, // int type declaration
    BYTE_TYPE, // 8-bit unsigned integer type declaration
    FLOAT_TYPE, // float type declaration
    STRING_TYPE, // string type declaration
    TETRA_TYPE, // tetra type declaration
    ENUM_KEYWORD, // 'enum' keyword
    NOTHING_TYPE, // nothing type declaration

    // composite types
    STRUCT, // struct
    MAP, // map
    UNION, // union
    ARRAY, // array
    ARRAY_KEYWORD, // 'array' keyword
    STRUCT_KEYWORD, // 'struct' keyword
    STRUCT_INSTANCE, // struct instance
    GROUP_KEYWORD, // 'group' keyword
    MAP_KEYWORD, // 'map' keyword
    UNION_KEYWORD, // 'union' keyword

    // type helpers
    FIELD_ACCESS, // field access
    LOGIC, // logic
    CUSTOM, // custom

    EOF, // end of file
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: usize,
    column: usize,
    file: []const u8,
    file_uri: []const u8,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize, column: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column,
            .file = "",
            .file_uri = "",
        };
    }

    pub fn initWithFile(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize, column: usize, file: []const u8, file_uri: []const u8) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column,
            .file = file,
            .file_uri = file_uri,
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

pub fn methodNameToTokenType(name: []const u8) ?TokenType {
    if (std.mem.eql(u8, name, "type")) return .TYPE;
    if (std.mem.eql(u8, name, "length")) return .LENGTH;
    if (std.mem.eql(u8, name, "slice")) return .SLICE;
    if (std.mem.eql(u8, name, "push")) return .PUSH;
    if (std.mem.eql(u8, name, "pop")) return .POP;
    if (std.mem.eql(u8, name, "insert")) return .INSERT;
    if (std.mem.eql(u8, name, "remove")) return .REMOVE;
    if (std.mem.eql(u8, name, "clear")) return .CLEAR;
    if (std.mem.eql(u8, name, "find")) return .FIND;
    if (std.mem.eql(u8, name, "string")) return .TOSTRING;
    if (std.mem.eql(u8, name, "int")) return .TOINT;
    if (std.mem.eql(u8, name, "float")) return .TOFLOAT;
    if (std.mem.eql(u8, name, "byte")) return .TOBYTE;
    if (std.mem.eql(u8, name, "pack")) return .PACK;
    if (std.mem.eql(u8, name, "unpack")) return .UNPACK;
    if (std.mem.eql(u8, name, "print")) return .PRINT;
    if (std.mem.eql(u8, name, "assert")) return .ASSERT;
    if (std.mem.eql(u8, name, "panic")) return .PANIC;
    if (std.mem.eql(u8, name, "exit")) return .EXIT;
    if (std.mem.eql(u8, name, "std")) return .STD;

    return null;
}
