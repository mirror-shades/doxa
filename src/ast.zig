const TokenKind = @import("lexer.zig").TokenKind;

pub const Type = enum {
    Unknown,
    Number,
    Float,
    String,
    Bool,
};

pub const Node = union(enum) {
    Number: struct {
        value: f64,
        typ: Type,
    },
    Binary: struct {
        left: *Node,
        operator: TokenKind,
        right: *Node,
        typ: Type,
    },
    Assignment: struct {
        name: []const u8,
        value: *Node,
        typ: Type,
    },
    Variable: struct {
        name: []const u8,
        typ: Type,
    },
    Declaration: struct {
        name: []const u8,
        value: *Node,
        typ: Type,
        is_mutable: bool,
    },
    Print: PrintStatement,
};

pub const PrintStatement = struct {
    expression: *Node,
};