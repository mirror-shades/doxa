//! The AST is a tree: no expression or type node is reachable from two
//! places, and no two expressions share a node id. A desugaring that needs a
//! value twice computes it once in an owner (a `Loop`'s `over`, a
//! `CompoundAssign`'s target) and reads it again through a `Held` of that
//! owner, which must enclose the read.
//!
//! Every later stage assumes this: a shared node is analysed, lowered and run
//! once per place it occurs, and the analyzer's per-node tables (keyed by
//! `base.id`) would merge its occurrences. `Parser.execute` checks every file
//! it parses in a debug build, so every program the test suite compiles is
//! checked, whether or not a shared node has an effect.

const std = @import("std");
const ast = @import("ast.zig");

pub const Violation = struct {
    /// What is shared, or the `Held` whose owner does not enclose it.
    what: []const u8,
    location: ?@import("../utils/reporting.zig").Location,
};

const Walk = struct {
    nodes: std.AutoHashMapUnmanaged(usize, void) = .empty,
    ids: std.AutoHashMapUnmanaged(ast.NodeId, void) = .empty,
    /// The owners enclosing the node being walked, innermost last.
    owners: std.ArrayListUnmanaged(ast.NodeId) = .empty,
    allocator: std.mem.Allocator,
    violation: ?Violation = null,

    fn fail(self: *Walk, what: []const u8, base: ?ast.Base) void {
        if (self.violation == null) self.violation = .{ .what = what, .location = if (base) |b| b.location() else null };
    }

    fn once(self: *Walk, address: usize, what: []const u8, base: ?ast.Base) std.mem.Allocator.Error!bool {
        const seen = try self.nodes.getOrPut(self.allocator, address);
        if (seen.found_existing) {
            self.fail(what, base);
            return false;
        }
        return true;
    }

    fn statements(self: *Walk, list: []const ast.Stmt) std.mem.Allocator.Error!void {
        for (list) |*s| try self.stmt(s);
    }

    fn stmt(self: *Walk, s: *const ast.Stmt) std.mem.Allocator.Error!void {
        switch (s.data) {
            .Expression => |e| try self.opt(e),
            .VarDecl => |decl| {
                try self.opt(decl.initializer);
                if (decl.type_expr) |t| try self.typeExpr(t);
            },
            .FunctionDecl => |f| {
                try self.params(f.params);
                try self.statements(f.body);
            },
            .Return => |r| try self.opt(r.value),
            .Assert => |a| {
                try self.expr(a.condition);
                try self.opt(a.message);
            },
            .Defer => |e| try self.expr(e),
            .Lift => |lift| try self.expr(lift.value),
            .ZigDecl, .EnumDecl, .GroupDecl, .Import, .Continue, .Break => {},
        }
    }

    fn params(self: *Walk, list: []const ast.FunctionParam) std.mem.Allocator.Error!void {
        for (list) |p| {
            if (p.type_expr) |t| try self.typeExpr(t);
            try self.opt(p.default_value);
        }
    }

    fn opt(self: *Walk, e: ?*const ast.Expr) std.mem.Allocator.Error!void {
        if (e) |x| try self.expr(x);
    }

    fn owned(self: *Walk, owner: ast.NodeId, e: ?*const ast.Expr) std.mem.Allocator.Error!void {
        try self.owners.append(self.allocator, owner);
        defer _ = self.owners.pop();
        try self.opt(e);
    }

    fn expr(self: *Walk, e: *const ast.Expr) std.mem.Allocator.Error!void {
        if (!try self.once(@intFromPtr(e), "an expression node is reachable from two places", e.base)) return;
        const id = try self.ids.getOrPut(self.allocator, e.base.id);
        if (id.found_existing) self.fail("two expressions share a node id", e.base);

        switch (e.data) {
            .Literal, .Variable, .EnumMember, .DefaultArgPlaceholder, .Unreachable, .Break, .This, .EnumDecl, .GroupDecl => {},
            .Held => |owner| {
                if (std.mem.indexOfScalar(ast.NodeId, self.owners.items, owner) == null) {
                    self.fail("a held value is read outside the node that holds it", e.base);
                }
            },
            .InterpolatedString => |template| for (template.parts) |part| switch (part) {
                .String => {},
                .Expression => |x| try self.expr(x),
            },
            .Binary => |b| {
                try self.opt(b.left);
                try self.opt(b.right);
            },
            .Unary => |u| try self.opt(u.right),
            .Peek => |p| try self.expr(p.expr),
            .Assignment => |a| try self.opt(a.value),
            .Grouping => |g| try self.opt(g),
            .If => |i| {
                try self.opt(i.condition);
                try self.opt(i.then_branch);
                try self.opt(i.else_branch);
            },
            .Block => |b| {
                try self.statements(b.statements);
                try self.opt(b.value);
            },
            .Array => |elements| for (elements) |x| try self.expr(x),
            .Index => |i| {
                try self.expr(i.array);
                try self.expr(i.index);
            },
            .IndexAssign => |a| {
                try self.expr(a.array);
                try self.expr(a.index);
                try self.expr(a.value);
            },
            .FunctionCall => |c| {
                try self.expr(c.callee);
                for (c.arguments) |arg| try self.expr(arg.expr);
            },
            .Logical => |l| {
                try self.expr(l.left);
                try self.expr(l.right);
            },
            .FieldAccess => |f| try self.expr(f.object),
            .StructDecl => |d| {
                for (d.fields) |field| try self.typeExpr(field.type_expr);
                for (d.methods) |method| {
                    try self.params(method.params);
                    try self.statements(method.body);
                }
            },
            .StructLiteral => |l| for (l.fields) |field| try self.expr(field.value),
            .FieldAssignment => |a| {
                try self.expr(a.object);
                try self.expr(a.value);
            },
            .Exists => |q| {
                try self.expr(q.array);
                try self.expr(q.condition);
            },
            .ForAll => |q| {
                try self.expr(q.array);
                try self.expr(q.condition);
            },
            .ArrayType => |a| {
                try self.typeExpr(a.element_type);
                try self.opt(a.size);
            },
            .Match => |m| {
                try self.expr(m.value);
                for (m.cases) |case| try self.expr(case.body);
            },
            .InternalCall => |c| {
                try self.expr(c.receiver);
                for (c.arguments) |x| try self.expr(x);
            },
            .Assert => |a| {
                try self.expr(a.condition);
                try self.opt(a.message);
            },
            .Cast => |c| {
                try self.expr(c.value);
                try self.typeExpr(c.target_type);
                try self.opt(c.then_branch);
                try self.opt(c.else_branch);
            },
            .ReturnExpr => |r| try self.opt(r.value),
            .TypeExpr => |t| try self.typeExpr(t),
            .Loop => |l| {
                if (l.var_decl) |vd| try self.stmt(vd);
                // `over` belongs to the scope around the loop: it cannot
                // read the loop's own held value.
                try self.opt(l.over);
                try self.owned(e.base.id, l.condition);
                try self.owned(e.base.id, l.body);
                try self.owned(e.base.id, l.step);
            },
            .Range => |r| {
                try self.expr(r.start);
                try self.expr(r.end);
            },
            .CompoundAssign => |c| {
                try self.expr(c.target);
                try self.owned(e.base.id, c.operation);
            },
        }
    }

    fn typeExpr(self: *Walk, t: *const ast.TypeExpr) std.mem.Allocator.Error!void {
        if (!try self.once(@intFromPtr(t), "a type node is reachable from two places", t.base)) return;
        switch (t.data) {
            .Basic, .Custom => {},
            .Array => |a| {
                try self.typeExpr(a.element_type);
                try self.opt(a.size);
            },
            .Struct => |fields| for (fields) |field| try self.typeExpr(field.type_expr),
            .Union => |types| for (types) |member| try self.typeExpr(member),
        }
    }
};

/// The first way `statements` fails to be a tree, or null when it is one.
pub fn check(allocator: std.mem.Allocator, statements: []const ast.Stmt) std.mem.Allocator.Error!?Violation {
    var walk: Walk = .{ .allocator = allocator };
    defer {
        walk.nodes.deinit(allocator);
        walk.ids.deinit(allocator);
        walk.owners.deinit(allocator);
    }
    try walk.statements(statements);
    return walk.violation;
}
