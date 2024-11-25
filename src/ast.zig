const std = @import("std");
const token = @import("token.zig");

pub const Expr = union(enum) {
    Binary: struct {
        left: *Expr,
        operator: token.Token,
        right: *Expr,
    },
    Unary: struct {
        operator: token.Token,
        right: *Expr,
    },
    Literal: token.TokenLiteral,
    Variable: token.Token,

    pub fn deinit(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Binary => |*b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b.left);
                allocator.destroy(b.right);
            },
            .Unary => |*u| {
                u.right.deinit(allocator);
                allocator.destroy(u.right);
            },
            .Literal, .Variable => {},
        }
    }
};

pub const Stmt = union(enum) {
    VarDecl: struct {
        name: token.Token,
        initializer: ?*Expr,
    },
    ExprStmt: struct {
        expr: *Expr,
    },

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .VarDecl => |*v| {
                if (v.initializer) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
            },
            .ExprStmt => |*e| {
                e.expr.deinit(allocator);
                allocator.destroy(e.expr);
            },
        }
    }
};