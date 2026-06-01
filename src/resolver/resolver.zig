const std = @import("std");
const ast = @import("../ast/ast.zig");
const Parser = @import("../parser/parser_types.zig").Parser;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const WalkContext = enum { Convert, Collect };

pub const Resolver = struct {
    allocator: std.mem.Allocator,
    parser: *Parser,
    enum_group_names: std.StringHashMap(void),

    pub fn init(allocator: std.mem.Allocator, parser: *Parser) Resolver {
        return .{
            .allocator = allocator,
            .parser = parser,
            .enum_group_names = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *Resolver) void {
        self.enum_group_names.deinit();
    }

    pub fn resolve(self: *Resolver, statements: []const ast.Stmt) ErrorList!void {
        try self.loadInitialModules();
        try self.parser.ensureSpecificImports();
        try self.parser.ensureReachableModuleDependencies();

        self.collectImportedEnumGroupNames();

        for (statements) |stmt| {
            self.walkStmt(stmt, .Collect);
        }

        for (statements) |stmt| {
            self.walkStmt(stmt, .Convert);
        }
    }

    fn loadInitialModules(self: *Resolver) ErrorList!void {
        var keys = std.StringHashMap(void).init(self.allocator);
        defer keys.deinit();

        var it = self.parser.module_namespaces.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.ast == null) {
                try keys.put(entry.key_ptr.*, {});
            }
        }

        var key_it = keys.iterator();
        while (key_it.next()) |key_entry| {
            const namespace = key_entry.key_ptr.*;
            if (self.parser.module_namespaces.get(namespace)) |existing| {
                if (existing.ast == null) {
                    _ = try self.parser.loadAndRegisterModule(existing.file_path, namespace, null);
                }
            }
        }
    }

    fn collectImportedEnumGroupNames(self: *Resolver) void {
        if (self.parser.imported_symbols) |symbols| {
            var it = symbols.iterator();
            while (it.next()) |entry| {
                const sym = entry.value_ptr.*;
                switch (sym.kind) {
                    .Enum, .Group => {
                        if (sym.name.len > 0) {
                            self.enum_group_names.put(sym.name, {}) catch {};
                        }
                    },
                    else => {},
                }
            }
        }
    }

    fn walkStmt(self: *Resolver, stmt: ast.Stmt, ctx: WalkContext) void {
        switch (ctx) {
            .Collect => {
                switch (stmt.data) {
                    .EnumDecl => |ed| self.enum_group_names.put(ed.name.lexeme, {}) catch {},
                    .GroupDecl => |gd| self.enum_group_names.put(gd.name.lexeme, {}) catch {},
                    else => {},
                }
            },
            .Convert => {},
        }

        switch (stmt.data) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| self.walkExpr(expr, ctx);
            },
            .Block => |block_stmts| {
                for (block_stmts) |nested| self.walkStmt(nested, ctx);
            },
            .FunctionDecl => |func| {
                for (func.body) |body_stmt| self.walkStmt(body_stmt, ctx);
            },
            .VarDecl => |var_decl| {
                if (var_decl.initializer) |initializer| self.walkExpr(initializer, ctx);
            },
            else => {},
        }
    }

    fn walkExpr(self: *Resolver, expr: *ast.Expr, ctx: WalkContext) void {
        switch (ctx) {
            .Convert => {
                if (expr.data == .FieldAccess) {
                    const fa = &expr.data.FieldAccess;
                    self.walkExpr(fa.object, ctx);
                    if (fa.object.data == .Variable) {
                        const var_name = fa.object.data.Variable.lexeme;
                        if (self.enum_group_names.contains(var_name)) {
                            const member_token = fa.field;
                            fa.object.deinit(self.allocator);
                            self.allocator.destroy(fa.object);
                            expr.data = .{ .EnumMember = member_token };
                            return;
                        }
                    }
                    return;
                }
            },
            .Collect => {
                switch (expr.data) {
                    .EnumDecl => |ed| self.enum_group_names.put(ed.name.lexeme, {}) catch {},
                    .GroupDecl => |gd| self.enum_group_names.put(gd.name.lexeme, {}) catch {},
                    else => {},
                }
            },
        }

        switch (expr.data) {
            .FieldAccess => |fa| self.walkExpr(fa.object, ctx),
            .Binary => |bin| {
                if (bin.left) |l| self.walkExpr(l, ctx);
                if (bin.right) |r| self.walkExpr(r, ctx);
            },
            .Unary => |un| {
                if (un.right) |r| self.walkExpr(r, ctx);
            },
            .Assignment => |assign| {
                if (assign.value) |v| self.walkExpr(v, ctx);
            },
            .CompoundAssign => |ca| {
                if (ca.value) |v| self.walkExpr(v, ctx);
            },
            .FunctionCall => |fc| {
                self.walkExpr(fc.callee, ctx);
                for (fc.arguments) |arg| self.walkExpr(arg.expr, ctx);
            },
            .Index => |idx| {
                self.walkExpr(idx.array, ctx);
                self.walkExpr(idx.index, ctx);
            },
            .IndexAssign => |ia| {
                self.walkExpr(ia.array, ctx);
                self.walkExpr(ia.index, ctx);
                self.walkExpr(ia.value, ctx);
            },
            .Logical => |log| {
                self.walkExpr(log.left, ctx);
                self.walkExpr(log.right, ctx);
            },
            .StructLiteral => |sl| {
                for (sl.fields) |field| self.walkExpr(field.value, ctx);
            },
            .Struct => |fields| {
                for (fields) |field| self.walkExpr(field.value, ctx);
            },
            .Array => |elements| {
                for (elements) |elem| self.walkExpr(elem, ctx);
            },
            .Block => |block| {
                for (block.statements) |s| self.walkStmt(s, ctx);
                if (block.value) |v| self.walkExpr(v, ctx);
            },
            .If => |if_expr| {
                if (if_expr.condition) |cond| self.walkExpr(cond, ctx);
                if (if_expr.then_branch) |b| self.walkExpr(b, ctx);
                if (if_expr.else_branch) |b| self.walkExpr(b, ctx);
            },
            .Loop => |loop| {
                if (loop.condition) |cond| self.walkExpr(cond, ctx);
                self.walkExpr(loop.body, ctx);
                if (loop.step) |step| self.walkExpr(step, ctx);
            },
            .Match => |m| {
                self.walkExpr(m.value, ctx);
                for (m.cases) |case| self.walkExpr(case.body, ctx);
            },
            .Cast => |c| {
                self.walkExpr(c.value, ctx);
                if (c.then_branch) |b| self.walkExpr(b, ctx);
                if (c.else_branch) |b| self.walkExpr(b, ctx);
            },
            .ReturnExpr => |ret| {
                if (ret.value) |v| self.walkExpr(v, ctx);
            },
            .StructDecl => |sd| {
                for (sd.methods) |method| {
                    for (method.body) |s| self.walkStmt(s, ctx);
                }
            },
            .InterpolatedString => |is| {
                for (is.parts) |*part| {
                    switch (part.*) {
                        .Expression => |e| self.walkExpr(e, ctx),
                        else => {},
                    }
                }
            },
            .Grouping => |g| {
                if (g) |inner| self.walkExpr(inner, ctx);
            },
            .MapLiteral => |ml| {
                for (ml.entries) |entry| {
                    self.walkExpr(entry.key, ctx);
                    self.walkExpr(entry.value, ctx);
                }
            },
            .Map => |m| {
                for (m.entries) |entry| {
                    self.walkExpr(entry.key, ctx);
                    self.walkExpr(entry.value, ctx);
                }
            },
            .Range => |r| {
                self.walkExpr(r.start, ctx);
                self.walkExpr(r.end, ctx);
            },
            .Peek => |p| self.walkExpr(p.expr, ctx),
            .Print => |p| self.walkExpr(p.expr, ctx),
            .PeekStruct => |ps| self.walkExpr(ps.expr, ctx),
            .BuiltinCall => |bc| {
                for (bc.arguments) |arg| self.walkExpr(arg, ctx);
            },
            .InternalCall => |ic| {
                self.walkExpr(ic.receiver, ctx);
                for (ic.arguments) |arg| self.walkExpr(arg, ctx);
            },
            .Exists => |e| {
                self.walkExpr(e.array, ctx);
                self.walkExpr(e.condition, ctx);
            },
            .ForAll => |forall| {
                self.walkExpr(forall.array, ctx);
                self.walkExpr(forall.condition, ctx);
            },
            .FieldAssignment => |fa2| {
                self.walkExpr(fa2.object, ctx);
                self.walkExpr(fa2.value, ctx);
            },
            .Increment => |inner| self.walkExpr(inner, ctx),
            .Decrement => |inner| self.walkExpr(inner, ctx),
            .Assert => |a| {
                self.walkExpr(a.condition, ctx);
                if (a.message) |m| self.walkExpr(m, ctx);
            },
            .ArrayType => |at| {
                if (at.size) |s| self.walkExpr(s, ctx);
            },
            .Literal,
            .Variable,
            .EnumMember,
            .EnumDecl,
            .GroupDecl,
            .Unreachable,
            .Break,
            .This,
            .DefaultArgPlaceholder,
            .Input,
            .TypeExpr,
            => {},
        }
    }
};
