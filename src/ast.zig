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

pub const Expr = union(enum) {
    Literal: token.TokenLiteral,
    Binary: Binary,
    Unary: Unary,
    Variable: token.Token,
    Assignment: Assignment,

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
    Expression: ?*Expr,
    VarDecl: VarDecl,
    
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
        }
    }
};