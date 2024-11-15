const TokenKind = @import("lexer.zig").TokenKind;

pub const Type = enum {
    Auto,
    Int,
    Float,
    String,
    Bool,
    Array,
    Nothing,
};

pub const Node = union(enum) {
    Int: struct {
        value: i64,
        typ: Type,
    },
    Float: struct {
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
    Nothing: struct {},
    Print: PrintStatement,
    Block: struct {
        statements: []*Node,
    },
    True: struct {},
    False: struct {},
    Array: struct {
        elements: []*Node,
        typ: Type,
    },
    And: struct {
        left: *Node,
        right: *Node,
    },
    Or: struct {
        left: *Node,
        right: *Node,
    },
    Unary: struct {
        operator: TokenKind,
        operand: *Node,
        typ: Type,
    },
    If: struct {
        condition: *Node,
        then_branch: *Node,
        else_branch: *Node,
    },

    pub fn typ(self: *const Node) Type {
        return switch (self.*) {
            .Int => |i| i.typ,
            .Float => |f| f.typ,
            .String => |s| s.typ,
            .Binary => |b| b.typ,
            .Assignment => |a| a.typ,
            .Variable => |v| v.typ,
            .Declaration => |d| d.typ,
            .Nothing => .Nothing,
            .Print => .Auto, // or whatever default type makes sense
            .Block => .Auto, // or whatever default type makes sense
            .True => .Bool,
            .False => .Bool,
            .Array => |arr| arr.typ,
            .And => .Bool,
            .Or => .Bool,
            .Unary => |u| u.typ,
            .If => |ifnode| {
                const then_branch = ifnode.then_branch.typ();
                const else_branch = ifnode.else_branch.typ();
                if (then_branch == else_branch) {
                    return then_branch;
                }
                return .Nothing;
            },
        };
    }
};

pub const PrintStatement = struct {
    expression: *Node,
};
