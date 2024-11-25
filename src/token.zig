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
    COLON,          // :
    SEMICOLON,      // ;
    MODULO,         // %
    HASH,           // #
    TILDE,          // ~
    QUESTION,       // ?

    //one or two character tokens
    AMPERSAND,      // &
    PIPE,           // |
    ARROW,          // ->
    SLASH,          // /
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
    IMPORT,         // import
    PUBLIC,         // public
    ASSERT,         // assert
    RETURN,         // return
    RETURNS,        // assign an expected return type
    BREAK,          // break
    CONTINUE,       // continue
    MATCH,          // match
    THROW,          // throw
    TRY,            // try
    CATCH,          // catch
    WHILE,          // while
    FOR,            // for
    FOREACH,        // foreach
    FROM,           // from
    IN,             // in
    IS,             // is
    AS,             // as
    ASYNC,          // async
    AWAIT,          // await
    TYPEOF,         // typeof
    DOT_DOT,        // ..

    // keywords with alternate tokens
    AND_KEYWORD,    // and
    AND_SYMBOL,     // &&
    OR_KEYWORD,     // or
    OR_SYMBOL,      // ||
    FN_KEYWORD,     // fn
    FUNCTION_KEYWORD, // function

    // logical operators
    IF,             // if
    THEN,           // then
    ELSE,           // else

    // literals
    IDENTIFIER,    // identifier
    SPREAD,        // ...
    INT,           // integer
    FLOAT,         // float
    STRING,        // string
    ARRAY,         // array
    BOOL,          // boolean
    ENUM,          // enum
    AUTO,          // auto
    NOTHING,       // nothing
    EQUALITY,      // == or equals
    ASSIGN,        // = or is

    EOF,            // end of file
};

pub const TokenLiteral = union(enum) {
    int: i32,                     // For integer literals like 123
    float: f32,                   // For numeric literals like "123.45"
    string: []const u8,           // For string literals like "hello"
    boolean: bool,                // For boolean literals like true or false
    array: []const TokenLiteral,  // Changed from []const u8 to allow nested arrays and mixed types
    auto,                         // For identifiers that are not explicitly typed
    nothing,                      // For tokens that don't have a literal value
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: TokenLiteral,
    line: u32,
    column: u32,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: TokenLiteral, line: u32, column: u32) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
            .column = column,
        };
    }

    // Remove deinit - lexer will handle cleanup
};
