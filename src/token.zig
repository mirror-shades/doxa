const std = @import("std");

pub const TokenType = enum {
    // single-character tokens
    LEFT_PAREN,     // (
    RIGHT_PAREN,    // )
    LEFT_BRACE,     // {
    RIGHT_BRACE,    // }
    LEFT_BRACKET,   // [
    RIGHT_BRACKET,  // ]
    COMMA,          // ,
    DOT,            // .
    SEMICOLON,      // ;
    MODULO,         // %
    HASH,           // #

    //one or two character tokens
    SLASH,          // /
    SLASH_SLASH,    // //
    SLASH_EQUAL,    // /=
    ASTERISK,       // *
    ASTERISK_EQUAL, // *=
    POWER,          // ^
    POWER_EQUAL,    // ^=
    PLUS,           // +
    PLUS_PLUS,      // ++
    PLUS_EQUAL,     // +=   
    MINUS,          // -
    MINUS_MINUS,    // --
    MINUS_EQUAL,    // -=
    EQUAL,          // =
    EQUAL_EQUAL,    // ==
    BANG,           // !
    BANG_EQUAL,     // !=
    GREATER,        // >
    GREATER_EQUAL,  // >=
    LESS,           // <
    LESS_EQUAL,     // <=

    // keywords
    VAR,            // var
    CONST,          // const
    STRUCT,         // struct
    FUNCTION,       // function
    IMPORT,         // import
    PUBLIC,         // public
    ASSERT,         // assert
    RETURN,         // return
    BREAK,          // break
    CONTINUE,       // continue
    MATCH,          // match
    THROW,          // throw
    TRY,            // try
    CATCH,          // catch
    WHILE,          // while
    FOR,            // for
    FOREACH,        // foreach
    IN,             // in

    // logical operators
    IF,             // if
    THEN,           // then
    ELSE,           // else
    AND,            // and
    OR,             // or

    // literals
    ASSIGN,         // =
    IDENTIFIER,  // identifier
    INT,         // integer
    FLOAT,       // float
    STRING,      // string
    ARRAY,       // array
    BOOL,          // boolean
    AUTO,          // auto
    NOTHING,       // nothing

    EOF
};

pub const TokenLiteral = union(enum) {
    int: i64,                     // For integer literals like 123
    float: f64,                   // For numeric literals like "123.45"
    string: []const u8,           // For string literals like "hello"
    boolean: bool,                // For boolean literals like true or false
    array: []const TokenLiteral,  // Changed from []const u8 to allow nested arrays and mixed types
    auto,                         // For tokens that don't need a literal value
    nothing,                      // For tokens that don't need a literal value
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn toString(self: Token) ![]const u8 {
        return std.fmt.allocPrint(
            "{} {} {}",
            .{ self.type, self.lexeme, self.literal }
        );
    }
};
