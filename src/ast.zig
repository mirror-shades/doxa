const TokenKind = @import("lexer.zig").TokenKind;

pub const Type = enum {
    Auto,
    Number,
    Float,
    String,
    Bool,
    Array,
};

pub const Node = union(enum) {
    Number: struct {
        value: f64,
        typ: Type,
    },
    String: struct {
        value: []const u8,
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
    Block: struct {
        statements: []*Node,
    },

    pub fn typ(self: *const Node) Type {
        return switch (self.*) {
            .Number => |n| n.typ,
            .String => |s| s.typ,
            .Binary => |b| b.typ,
            .Assignment => |a| a.typ,
            .Variable => |v| v.typ,
            .Declaration => |d| d.typ,
            .Print => .Auto, // or whatever default type makes sense
            .Block => .Auto, // or whatever default type makes sense
        };
    }
};

pub const PrintStatement = struct {
    expression: *Node,
};
