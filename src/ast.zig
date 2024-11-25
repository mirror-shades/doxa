const std = @import("std");
const token = @import("token.zig");

pub const Binary = struct {
    left: ?*Expr,
    operator: token.Token,
    right: ?*Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: ?*Expr,
};

pub const If = struct {
    condition: ?*Expr,
    then_branch: ?*Expr,
    else_branch: ?*Expr,
};

pub const Expr = union(enum) {
    Literal: token.TokenLiteral,
    Binary: Binary,
    Unary: Unary,
    Variable: token.Token,
    Assignment: Assignment,
    Grouping: ?*Expr,
    If: If,
    Block: []Stmt,

    pub fn deinit(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal, .Variable => {}, // Nothing to free
            .Binary => |*bin| {
                if (bin.left) |left| {
                    left.deinit(allocator);
                    allocator.destroy(left);
                }
                if (bin.right) |right| {
                    right.deinit(allocator);
                    allocator.destroy(right);
                }
            },
            .Unary => |*un| {
                if (un.right) |right| {
                    right.deinit(allocator);
                    allocator.destroy(right);
                }
            },
            .Assignment => |*assign| {
                if (assign.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Grouping => |maybe_expr| if (maybe_expr) |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
            .If => |*i| {
                if (i.condition) |condition| {
                    condition.deinit(allocator);
                    allocator.destroy(condition);
                }
                if (i.then_branch) |then_branch| {
                    then_branch.deinit(allocator);
                    allocator.destroy(then_branch);
                }
                if (i.else_branch) |else_branch| {
                    else_branch.deinit(allocator);
                    allocator.destroy(else_branch);
                }
            },
            .Block => |statements| {
                for (statements) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(statements);
            },
        }
    }
};

pub const Assignment = struct {
    name: token.Token,
    value: ?*Expr,
};

pub const VarDecl = struct {
    name: token.Token,
    initializer: ?*Expr,
};

pub const Stmt = union(enum) {
    VarDecl: struct {
        name: token.Token,
        initializer: ?*Expr,
    },
    Expression: ?*Expr,
    Block: []Stmt,

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
            },
            .VarDecl => |*decl| {
                if (decl.initializer) |init| {
                    init.deinit(allocator);
                    allocator.destroy(init);
                }
            },
            .Block => |statements| {
                for (statements) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(statements);
            },
        }
    }
};
