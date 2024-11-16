const std = @import("std");
const Token = @import("token.zig").Token;

pub const NodeType = enum {
    Program,
    Identifier,
    FunctionDeclaration,
    VariableDeclaration,
    Expression,
    BlockStatement,
    IfStatement,
    WhileStatement,
    ForStatement,
    ForeachStatement,   
    BreakStatement,
    ContinueStatement,
    ReturnStatement,
    ThrowStatement,
    TryStatement,
    CatchStatement,
    PrintStatement,
};

pub const Node = struct {
    node_type: NodeType,
    children: []Node,
    token: Token = Token.init( .NOTHING, "", .nothing, 0),
};
