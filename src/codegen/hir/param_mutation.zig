const std = @import("std");
const ast = @import("../../ast/ast.zig");
const builtin_methods = @import("../../runtime/builtin_methods.zig");

/// Whether a function body can mutate the object one of its by-value heap
/// parameters points at.
///
/// Heap parameters are snapshotted on entry (`HeapCopyKind.snapshot`) so a
/// callee cannot write through to the caller's array, struct, or map — `^`
/// alias parameters are the explicit opt-in for that (see `docs/alias.md`).
/// The snapshot is a deep copy, so it costs O(n) plus an arena allocation on
/// every call. When the body provably never writes through the parameter the
/// copy is unobservable, and the parameter can bind the incoming pointer
/// directly (`HeapCopyKind.keep`); a call site boxing a fixed-size array
/// argument for such a parameter can borrow the flat buffer instead of
/// copying it.
///
/// The verdict is computed once per parameter when function metadata is built
/// and recorded as `FunctionInfo.param_is_readonly`, beside `param_is_alias`,
/// so the callee's parameter binding and every call site read the same
/// property rather than each re-running the analysis.
///
/// Both switches below are exhaustive on purpose: adding an AST node forces a
/// decision here instead of silently defaulting to "cannot mutate", which
/// would break by-value semantics.
///
/// The analysis errs toward reporting mutation. Rebinding the parameter itself
/// (`p is other`) is *not* mutation — it overwrites the callee's own slot and
/// leaves the caller's object untouched — but anything that writes through the
/// parameter, hands it to an in-place intrinsic, or re-passes it as an alias
/// is. Read-only intrinsics (`@length`, `@find`, conversions, ...) never write
/// through an argument and do not count as mutation (see
/// `BuiltinMethodInfo.mutates_arg`).
pub fn bodyMutatesVariable(body: []const ast.Stmt, name: []const u8) bool {
    for (body) |stmt| {
        if (stmtMutates(stmt, name)) return true;
    }
    return false;
}

/// Follow index and field chains down to the variable they are rooted at, so
/// `p[i]`, `p.field`, and `p.field[i]` all resolve to `p`.
fn rootVariableIs(expr: *const ast.Expr, name: []const u8) bool {
    return switch (expr.data) {
        .Variable => |tok| std.mem.eql(u8, tok.lexeme, name),
        .Index => |idx| rootVariableIs(idx.array, name),
        .FieldAccess => |fa| rootVariableIs(fa.object, name),
        .Grouping => |g| if (g) |inner| rootVariableIs(inner, name) else false,
        else => false,
    };
}

/// Whether `name` appears anywhere in this expression. Used for the coarse
/// cases — intrinsics and alias arguments — where any mention is treated as a
/// potential write.
fn mentions(expr: *const ast.Expr, name: []const u8) bool {
    if (rootVariableIs(expr, name)) return true;
    var found = false;
    forEachChild(expr, name, &found, mentionsVisitor);
    return found;
}

fn mentionsVisitor(child: *const ast.Expr, name: []const u8, found: *bool) void {
    if (found.*) return;
    if (mentions(child, name)) found.* = true;
}

fn exprMutates(expr: *const ast.Expr, name: []const u8) bool {
    switch (expr.data) {
        // Writes through the parameter.
        .IndexAssign => |ia| {
            if (rootVariableIs(ia.array, name)) return true;
        },
        .FieldAssignment => |fa| {
            if (rootVariableIs(fa.object, name)) return true;
        },
        .Increment, .Decrement => |operand| {
            if (rootVariableIs(operand, name)) return true;
        },

        // `p += x` rebinds `p`, but for a collection the lowering reads and
        // rewrites the existing object, so treat it as a write.
        .CompoundAssign => |ca| {
            if (std.mem.eql(u8, ca.name.lexeme, name)) return true;
        },

        // In-place intrinsic calls (`@push`, `@insert`, `@remove`, `@pop`,
        // `@clear`, ...) mutate their subject, so any mention of the parameter
        // is treated as a write. Read-only intrinsics (`@length`, `@find`,
        // `@slice`, `@print`, conversions, ...) never write through an
        // argument, so a mention is harmless — the generic child scan below
        // still catches any nested write. The registry flags which intrinsics
        // mutate their subject; an intrinsic the registry does not know stays
        // conservatively treated as a write.
        .BuiltinCall => |bc| {
            const mutates_arg = if (builtin_methods.getMethodInfoByName(bc.function.lexeme)) |info|
                info.mutates_arg
            else
                true;
            if (mutates_arg) {
                for (bc.arguments) |arg| {
                    if (mentions(arg, name)) return true;
                }
            }
        },

        // `@`-prefixed method calls. In-place compiler methods (`@push`,
        // `@insert`, ...) mutate their subject, so any mention of the
        // parameter is a write. Read-only conversions that survive lowering as
        // internal calls (`@string`, `@int`, `@float`, `@byte`) never write,
        // and the generic child scan below still catches nested writes. The
        // registry decides which; a method it does not know stays
        // conservatively treated as a write.
        .InternalCall => |ic| {
            const mutates_arg = if (builtin_methods.getMethodInfoByName(ic.method.lexeme)) |info|
                info.mutates_arg
            else
                true;
            if (mutates_arg) {
                if (mentions(ic.receiver, name)) return true;
                for (ic.arguments) |arg| {
                    if (mentions(arg, name)) return true;
                }
            }
        },

        // Re-passing as an alias hands write access to the callee.
        .FunctionCall => |fc| {
            for (fc.arguments) |arg| {
                if (arg.is_alias and mentions(arg.expr, name)) return true;
            }
        },

        else => {},
    }

    var found = false;
    forEachChild(expr, name, &found, mutatesVisitor);
    return found;
}

fn mutatesVisitor(child: *const ast.Expr, name: []const u8, found: *bool) void {
    if (found.*) return;
    if (exprMutates(child, name)) found.* = true;
}

/// Visit every sub-expression of `expr`. The switch is exhaustive so a new
/// expression kind cannot be silently skipped.
fn forEachChild(
    expr: *const ast.Expr,
    name: []const u8,
    found: *bool,
    visit: fn (*const ast.Expr, []const u8, *bool) void,
) void {
    switch (expr.data) {
        .Binary => |b| {
            if (b.left) |l| visit(l, name, found);
            if (b.right) |r| visit(r, name, found);
        },
        .Unary => |u| {
            if (u.right) |r| visit(r, name, found);
        },
        .Logical => |l| {
            visit(l.left, name, found);
            visit(l.right, name, found);
        },
        .Grouping => |g| {
            if (g) |inner| visit(inner, name, found);
        },
        .Index => |idx| {
            visit(idx.array, name, found);
            visit(idx.index, name, found);
        },
        .IndexAssign => |ia| {
            visit(ia.array, name, found);
            visit(ia.index, name, found);
            visit(ia.value, name, found);
        },
        .FieldAccess => |fa| visit(fa.object, name, found),
        .FieldAssignment => |fa| {
            visit(fa.object, name, found);
            visit(fa.value, name, found);
        },
        .Assignment => |a| {
            if (a.value) |v| visit(v, name, found);
        },
        .CompoundAssign => |ca| {
            if (ca.value) |v| visit(v, name, found);
        },
        .Increment, .Decrement => |operand| visit(operand, name, found),
        .FunctionCall => |fc| {
            visit(fc.callee, name, found);
            for (fc.arguments) |arg| visit(arg.expr, name, found);
        },
        .BuiltinCall => |bc| {
            for (bc.arguments) |arg| visit(arg, name, found);
        },
        .InternalCall => |ic| {
            visit(ic.receiver, name, found);
            for (ic.arguments) |arg| visit(arg, name, found);
        },
        .If => |i| {
            if (i.condition) |c| visit(c, name, found);
            if (i.then_branch) |t| visit(t, name, found);
            if (i.else_branch) |e| visit(e, name, found);
        },
        .Loop => |l| {
            if (l.var_decl) |vd| {
                if (stmtMutates(vd.*, name)) found.* = true;
            }
            if (l.condition) |c| visit(c, name, found);
            if (l.step) |s| visit(s, name, found);
            visit(l.body, name, found);
        },
        .Block => |blk| {
            for (blk.statements) |s| {
                if (stmtMutates(s, name)) found.* = true;
            }
            if (blk.value) |v| visit(v, name, found);
        },
        .Match => |m| {
            visit(m.value, name, found);
            for (m.cases) |case| visit(case.body, name, found);
        },
        .Array => |elements| {
            for (elements) |e| visit(e, name, found);
        },
        .Struct => |fields| {
            for (fields) |f| visit(f.value, name, found);
        },
        .StructLiteral => |sl| {
            for (sl.fields) |f| visit(f.value, name, found);
        },
        .Map => |m| {
            for (m.entries) |entry| {
                visit(entry.key, name, found);
                visit(entry.value, name, found);
            }
        },
        .MapLiteral => |m| {
            for (m.entries) |entry| {
                visit(entry.key, name, found);
                visit(entry.value, name, found);
            }
            if (m.else_value) |ev| visit(ev, name, found);
        },
        .Exists => |q| {
            visit(q.array, name, found);
            visit(q.condition, name, found);
        },
        .ForAll => |q| {
            visit(q.array, name, found);
            visit(q.condition, name, found);
        },
        .Peek => |p| visit(p.expr, name, found),
        .Print => |p| visit(p.expr, name, found),
        .PeekStruct => |p| visit(p.expr, name, found),
        .InterpolatedString => |tmpl| {
            for (tmpl.parts) |part| {
                switch (part) {
                    .Expression => |e| visit(e, name, found),
                    .String => {},
                }
            }
        },
        .Cast => |c| {
            visit(c.value, name, found);
            if (c.then_branch) |t| visit(t, name, found);
            if (c.else_branch) |e| visit(e, name, found);
        },
        .Assert => |a| {
            visit(a.condition, name, found);
            if (a.message) |m| visit(m, name, found);
        },
        .ReturnExpr => |r| {
            if (r.value) |v| visit(v, name, found);
        },
        .Range => |r| {
            visit(r.start, name, found);
            visit(r.end, name, found);
        },
        .ArrayType => |at| {
            if (at.size) |s| visit(s, name, found);
        },

        // Leaves and declarations: nothing that can reference a local.
        .Literal,
        .Variable,
        .Input,
        .StructDecl,
        .EnumDecl,
        .GroupDecl,
        .EnumMember,
        .DefaultArgPlaceholder,
        .Unreachable,
        .Break,
        .TypeExpr,
        .This,
        => {},
    }
}

/// Whether a statement can mutate the object `name` points at. Exhaustive for
/// the same reason as `forEachChild`.
fn stmtMutates(stmt: ast.Stmt, name: []const u8) bool {
    var found = false;
    switch (stmt.data) {
        .Expression => |maybe| {
            if (maybe) |e| {
                if (exprMutates(e, name)) found = true;
            }
        },
        .VarDecl => |v| {
            if (v.initializer) |init| {
                if (exprMutates(init, name)) found = true;
            }
        },
        .Block => |statements| {
            for (statements) |s| {
                if (stmtMutates(s, name)) found = true;
            }
        },
        .Return => |r| {
            if (r.value) |v| {
                if (exprMutates(v, name)) found = true;
            }
        },
        .Assert => |a| {
            if (exprMutates(a.condition, name)) found = true;
            if (a.message) |m| {
                if (exprMutates(m, name)) found = true;
            }
        },
        .Cast => |c| {
            if (exprMutates(c.value, name)) found = true;
            if (c.then_branch) |t| {
                if (exprMutates(t, name)) found = true;
            }
            if (c.else_branch) |e| {
                if (exprMutates(e, name)) found = true;
            }
        },
        .Defer => |d| {
            if (exprMutates(d, name)) found = true;
        },
        .Lift => |l| {
            if (exprMutates(l.value, name)) found = true;
        },
        .MapLiteral => |m| {
            for (m.entries) |entry| {
                if (exprMutates(entry.key, name)) found = true;
                if (exprMutates(entry.value, name)) found = true;
            }
            if (m.else_value) |ev| {
                if (exprMutates(ev, name)) found = true;
            }
        },

        // A nested function has its own parameter bindings and cannot reach this
        // frame's locals.
        .FunctionDecl => {},

        // Declarations and control-flow markers carry no expressions that could
        // reference a local.
        .ZigDecl,
        .EnumDecl,
        .GroupDecl,
        .Module,
        .Import,
        .Path,
        .Continue,
        .Break,
        => {},
    }
    return found;
}
