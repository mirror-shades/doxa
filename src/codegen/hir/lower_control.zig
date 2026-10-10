//! Statement and control-flow lowering for the register HIR: blocks, `if`,
//! `match`, `as`, loops, quantifiers, and the edges that leave them —
//! `break`, `continue` and `return` run the pending `defer`s and close the
//! arenas they leave before they jump.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Location = @import("../../utils/reporting.zig").Location;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ErrorList = Errors.ErrorList;
const generator = @import("generator.zig");
const lower_expr = @import("lower_expr.zig");
const ir = @import("register/ir.zig");
const builder_mod = @import("register/builder.zig");

const Lowering = generator.Lowering;
const Error = generator.Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;
const BlockId = ir.BlockId;

// ── Functions and statements ──

/// Lower a function's body. Falling off its end returns `nothing`; analysis
/// proves a function with a value returns on every path.
pub fn functionBody(l: *Lowering, body: []const ast.Stmt) Error!void {
    try l.pushDefers();
    for (body) |stmt| {
        if (!try statement(l, stmt)) return;
    }
    if (!try l.popDefers()) return;
    // Analysis proves a function whose type cannot hold `nothing` returns on
    // every path.
    if (!holdsNothing(l)) return l.b.unreachableTerm();
    try l.exitArenas(0);
    try l.b.ret(try fallOffValue(l));
}

fn holdsNothing(l: *Lowering) bool {
    if (l.ret == .Nothing) return true;
    if (!l.ret.isBoxed()) return false;
    return (l.g.membersNamed(l.ret, .Nothing) catch return false).len > 0;
}

/// What a function returns when control reaches its end, or a `return`
/// carries no value: `nothing`, as its return type holds it.
fn fallOffValue(l: *Lowering) Error!?ValueId {
    if (l.ret == .Nothing) return null;
    return try l.convert(try l.nothing(), .Nothing, l.ret);
}

/// Lower one statement. False when control does not reach its end, so the
/// statements after it are dead.
pub fn statement(l: *Lowering, stmt: ast.Stmt) Error!bool {
    const saved = l.b.location;
    l.b.location = stmt.base.location();
    defer l.b.location = saved;

    switch (stmt.data) {
        .Expression => |maybe| if (maybe) |e| return try effect(l, e) != null,
        .VarDecl => |decl| return varDecl(l, stmt, decl.name, decl.initializer),
        .Return => |r| {
            _ = try returnFrom(l, r.value);
            return false;
        },
        .Break => return try breakLoop(l) != null,
        .Continue => return try continueLoop(l) != null,
        .Defer => |action| {
            if (l.defers.items.len > 0) {
                try l.addDefer(action);
            } else if (try effect(l, action) == null) return false;
        },
        .Lift => |lift| return try effect(l, lift.value) != null,
        .Assert => |a| return try assert(l, a.condition, a.message, a.location) != null,
        // Declarations and imports are compile time only.
        .FunctionDecl, .ZigDecl, .EnumDecl, .GroupDecl, .Import => {},
    }
    return true;
}

/// Lower `e` for its effect: an `if`, `match`, block or `as` whose value is
/// unused merges none. Null when control leaves it.
pub fn effect(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    return switch (e.data) {
        .If => ifExpr(l, e, false),
        .Match => matchExpr(l, e, false),
        .Block => blockExpr(l, e, false),
        .Cast => cast(l, e, false),
        else => l.expr(e),
    };
}

fn varDecl(l: *Lowering, stmt: ast.Stmt, name: ast.Token, initializer: ?*ast.Expr) Error!bool {
    const target = try l.g.storeTarget(&stmt.base);
    const ty = try l.g.lowerType(target.slot);
    const value = if (initializer) |init| blk: {
        // `is []` of a fixed array zero-fills its declared length.
        if (init.data == .Array and init.data.Array.len == 0 and ty == .Array) break :blk try emptyArray(l, ty);
        break :blk try l.exprAs(init, ty) orelse return false;
    } else try defaultValue(l, ty, target.slot);

    if (target.global) {
        try l.writeStorage(target, name.lexeme, value);
    } else {
        try l.declareLocal(target.storage, name.lexeme, ty, value);
    }
    return true;
}

fn emptyArray(l: *Lowering, ty: HIRType) Error!ValueId {
    const length: ?ValueId = if (ty.Array.size == null) try l.constant(.{ .int = 0 }, .Int) else null;
    return l.define(.{ .array_new = .{ .arena = try l.arena(), .length = length } }, ty);
}

/// The value a declaration without an initializer holds.
/// TODO(struct default): a struct, map or function local has none yet; what
/// one should hold is plan/uninitialized-declarations.md.
fn defaultValue(l: *Lowering, ty: HIRType, info: *const ast.TypeInfo) Error!ValueId {
    return switch (ty) {
        .Int => l.constant(.{ .int = 0 }, .Int),
        .Float => l.constant(.{ .float = 0.0 }, .Float),
        .Byte => l.constant(.{ .byte = 0 }, .Byte),
        .String => l.constant(.{ .string = "" }, .String),
        .Tetra => l.constant(.{ .tetra = .false }, .Tetra),
        .Nothing => l.nothing(),
        .Array => emptyArray(l, ty),
        // A union holds the default of its first member as written
        // (docs/unions.md); the HIR type keeps its members in canonical order.
        .Union => {
            const first = info.union_type.?.types[0];
            const member = try l.g.lowerType(first);
            return l.convert(try defaultValue(l, member, first), member, ty);
        },
        // TODO(group default): which member a group defaults to is undecided
        // (plan/uninitialized-declarations.md).
        .Group => l.convert(try l.nothing(), .Nothing, ty),
        .Enum, .Struct, .Map, .Function, .Unknown, .Poison => {
            l.g.reporter.reportInternal("a {s} declaration without an initializer has no default value", .{@tagName(ty)}, @src());
            return ErrorList.TypeMismatch;
        },
    };
}

/// Which locals the body lends as `^`: those are slots, every other local an
/// SSA variable. Decided before the body is lowered, since a local's
/// representation is fixed where it is declared.
pub fn collectLent(l: *Lowering, body: []const ast.Stmt) Error!void {
    for (body) |stmt| try lentInStmt(l, stmt);
}

fn lentInStmt(l: *Lowering, stmt: ast.Stmt) Error!void {
    switch (stmt.data) {
        .Expression => |maybe| if (maybe) |e| try lentIn(l, e),
        .VarDecl => |v| if (v.initializer) |init| try lentIn(l, init),
        .Return => |r| if (r.value) |v| try lentIn(l, v),
        .Assert => |a| {
            try lentIn(l, a.condition);
            if (a.message) |m| try lentIn(l, m);
        },
        .Defer => |d| try lentIn(l, d),
        .Lift => |lift| try lentIn(l, lift.value),
        .FunctionDecl, .ZigDecl, .EnumDecl, .GroupDecl, .Import, .Continue, .Break => {},
    }
}

fn lentIn(l: *Lowering, e: *ast.Expr) Error!void {
    switch (e.data) {
        .FunctionCall => |fc| {
            try lentIn(l, fc.callee);
            for (fc.arguments) |arg| {
                if (arg.is_alias and arg.expr.data == .Variable) {
                    const target = try l.g.storeTarget(&arg.expr.base);
                    if (!target.global) try l.lent.put(l.g.alloc, target.storage, {});
                }
                try lentIn(l, arg.expr);
            }
        },
        .Binary => |b| {
            if (b.left) |x| try lentIn(l, x);
            if (b.right) |x| try lentIn(l, x);
        },
        .Unary => |u| if (u.right) |x| try lentIn(l, x),
        .Logical => |x| {
            try lentIn(l, x.left);
            try lentIn(l, x.right);
        },
        .Grouping => |g| if (g) |x| try lentIn(l, x),
        .Index => |x| {
            try lentIn(l, x.array);
            try lentIn(l, x.index);
        },
        .IndexAssign => |x| {
            try lentIn(l, x.array);
            try lentIn(l, x.index);
            try lentIn(l, x.value);
        },
        .FieldAccess => |x| try lentIn(l, x.object),
        .FieldAssignment => |x| {
            try lentIn(l, x.object);
            try lentIn(l, x.value);
        },
        .Assignment => |x| if (x.value) |v| try lentIn(l, v),
        .InternalCall => |x| {
            try lentIn(l, x.receiver);
            for (x.arguments) |a| try lentIn(l, a);
        },
        .If => |x| {
            if (x.condition) |c| try lentIn(l, c);
            if (x.then_branch) |t| try lentIn(l, t);
            if (x.else_branch) |t| try lentIn(l, t);
        },
        .Loop => |x| {
            if (x.var_decl) |vd| try lentInStmt(l, vd.*);
            if (x.condition) |c| try lentIn(l, c);
            if (x.step) |s| try lentIn(l, s);
            try lentIn(l, x.body);
        },
        .Block => |x| {
            for (x.statements) |s| try lentInStmt(l, s);
            if (x.value) |v| try lentIn(l, v);
        },
        .Match => |x| {
            try lentIn(l, x.value);
            for (x.cases) |case| try lentIn(l, case.body);
        },
        .Array => |elements| for (elements) |x| try lentIn(l, x),
        .Struct => |fields| for (fields) |f| try lentIn(l, f.value),
        .StructLiteral => |x| for (x.fields) |f| try lentIn(l, f.value),
        .Map => |x| for (x.entries) |entry| {
            try lentIn(l, entry.key);
            try lentIn(l, entry.value);
        },
        .MapLiteral => |x| {
            for (x.entries) |entry| {
                try lentIn(l, entry.key);
                try lentIn(l, entry.value);
            }
            if (x.else_value) |v| try lentIn(l, v);
        },
        .Exists => |q| {
            try lentIn(l, q.array);
            try lentIn(l, q.condition);
        },
        .ForAll => |q| {
            try lentIn(l, q.array);
            try lentIn(l, q.condition);
        },
        .Peek => |p| try lentIn(l, p.expr),
        .Print => |p| try lentIn(l, p.expr),
        .PeekStruct => |p| try lentIn(l, p.expr),
        .InterpolatedString => |t| for (t.parts) |part| switch (part) {
            .Expression => |x| try lentIn(l, x),
            .String => {},
        },
        .Cast => |c| {
            try lentIn(l, c.value);
            if (c.then_branch) |t| try lentIn(l, t);
            if (c.else_branch) |t| try lentIn(l, t);
        },
        .Assert => |a| {
            try lentIn(l, a.condition);
            if (a.message) |m| try lentIn(l, m);
        },
        .ReturnExpr => |r| if (r.value) |v| try lentIn(l, v),
        .Range => |r| {
            try lentIn(l, r.start);
            try lentIn(l, r.end);
        },
        .ArrayType => |at| if (at.size) |s| try lentIn(l, s),
        .Literal, .Variable, .Input, .StructDecl, .EnumDecl, .GroupDecl, .EnumMember, .DefaultArgPlaceholder, .Unreachable, .Break, .TypeExpr, .This => {},
    }
}

// ── Edges out ──

/// `return v`: run every pending `defer`, place a heap result in the
/// caller's arena, close every arena this function opened, and return.
pub fn returnFrom(l: *Lowering, value: ?*ast.Expr) Error!?ValueId {
    var v: ?ValueId = null;
    if (value) |e| {
        if (l.ret == .Nothing) {
            if (try l.expr(e) == null) return null;
        } else {
            v = try l.exprAs(e, l.ret) orelse return null;
        }
    } else {
        v = try fallOffValue(l);
    }
    if (!try l.runDefersTo(0)) return null;
    if (v) |result| {
        if (l.caller) |caller| v = try l.rehome(caller, result, l.ret);
    }
    try l.exitArenas(0);
    try l.b.ret(v);
    return null;
}

pub fn breakLoop(l: *Lowering) Error!?ValueId {
    const loop_ctx = l.loops.getLastOrNull() orelse return outsideLoop(l, "break", ErrorCode.BREAK_USED_OUTSIDE_OF_LOOP);
    if (!try l.runDefersTo(loop_ctx.defer_depth)) return null;
    try l.exitArenas(loop_ctx.arena_depth);
    try l.b.jump(.{ .block = loop_ctx.break_block });
    return null;
}

pub fn continueLoop(l: *Lowering) Error!?ValueId {
    const loop_ctx = l.loops.getLastOrNull() orelse return outsideLoop(l, "continue", ErrorCode.CONTINUE_USED_OUTSIDE_OF_LOOP);
    if (!try l.runDefersTo(loop_ctx.defer_depth)) return null;
    try l.exitArenas(loop_ctx.arena_depth);
    try l.b.jump(.{ .block = loop_ctx.continue_block });
    return null;
}

fn outsideLoop(l: *Lowering, comptime keyword: []const u8, code: []const u8) Error!?ValueId {
    l.g.report(l.location().?, code, "'" ++ keyword ++ "' used outside of a loop", .{});
    return ErrorList.UnsupportedOperator;
}

/// An `unreachable` expression: reaching it is a runtime trap.
pub fn unreachableAt(l: *Lowering, location: Location) Error!?ValueId {
    _ = location;
    try l.b.unreachableTerm();
    return null;
}

pub fn assert(l: *Lowering, condition: *ast.Expr, message: ?*ast.Expr, location: Location) Error!?ValueId {
    const c = try l.cond(condition) orelse return null;
    const fail = try l.b.createBlock();
    const ok = try l.b.createBlock();
    try l.b.branch(c, .{ .block = ok }, .{ .block = fail });
    try l.b.seal(fail);
    try l.b.seal(ok);
    l.b.switchTo(fail);
    const text: ?ValueId = if (message) |m| try l.exprAs(m, .String) else null;
    if (message == null or text != null) try l.b.assertFail(text, location);
    l.b.switchTo(ok);
    return try l.nothing();
}

// ── Blocks and branches ──

pub fn blockExpr(l: *Lowering, e: *ast.Expr, want: bool) Error!?ValueId {
    const data = e.data.Block;
    try l.pushDefers();
    for (data.statements) |stmt| {
        if (!try statement(l, stmt)) {
            // The edge that left ran the frame's defers itself.
            _ = l.defers.pop();
            return null;
        }
    }
    var value: ?ValueId = null;
    if (data.value) |v| {
        value = (if (want) try l.expr(v) else try effect(l, v)) orelse {
            _ = l.defers.pop();
            return null;
        };
    }
    if (!try l.popDefers()) return null;
    if (!want) return try l.nothing();
    if (value) |v| return try l.convert(v, try l.g.typeOf(data.value.?), try l.g.typeOf(e));
    return try l.convert(try l.nothing(), .Nothing, try l.g.typeOf(e));
}

/// The join of a value-producing branch: its block, and the parameter that
/// is the value, when one is wanted.
const Join = struct {
    block: BlockId,
    value: ?ValueId,
    ty: HIRType,

    fn init(l: *Lowering, e: *ast.Expr, want: bool) Error!Join {
        const ty: HIRType = if (want) try l.g.typeOf(e) else .Nothing;
        const block = try l.b.createBlock();
        const value: ?ValueId = if (ty != .Nothing) try l.b.blockParam(block, .of(ty)) else null;
        return .{ .block = block, .value = value, .ty = ty };
    }

    /// Jump to the join with `v`, of type `from`, as the branch's value.
    fn arrive(self: Join, l: *Lowering, v: ValueId, from: HIRType) Error!void {
        if (self.value == null) return l.b.jump(.{ .block = self.block });
        try l.b.jump(.{ .block = self.block, .args = &.{try l.convert(v, from, self.ty)} });
    }

    /// Lower `arm` and jump to the join with its value; nothing when control
    /// leaves the arm.
    fn arm(self: Join, l: *Lowering, e: *ast.Expr) Error!void {
        const v = (if (self.value != null) try l.expr(e) else try effect(l, e)) orelse return;
        try self.arrive(l, v, if (self.value != null) try l.g.typeOf(e) else .Nothing);
    }

    /// Continue after the join; null when no branch reaches it.
    fn finish(self: Join, l: *Lowering) Error!?ValueId {
        try l.b.seal(self.block);
        if (l.b.isDead(self.block)) return null;
        l.b.switchTo(self.block);
        return self.value orelse try l.nothing();
    }
};

pub fn ifExpr(l: *Lowering, e: *ast.Expr, want: bool) Error!?ValueId {
    const data = e.data.If;
    const c = try l.cond(data.condition.?) orelse return null;
    const then = try l.b.createBlock();
    const otherwise = try l.b.createBlock();
    const join = try Join.init(l, e, want);
    try l.b.branch(c, .{ .block = then }, .{ .block = otherwise });
    try l.b.seal(then);
    try l.b.seal(otherwise);

    l.b.switchTo(then);
    try join.arm(l, data.then_branch.?);
    l.b.switchTo(otherwise);
    if (data.else_branch) |else_branch| {
        try join.arm(l, else_branch);
    } else {
        try join.arrive(l, try l.nothing(), .Nothing);
    }
    return join.finish(l);
}

// ── Loops ──

/// A loop opens an arena for its own state and, inside it, one for its body,
/// reset at the start of every iteration and again before the step. The
/// condition-false exit and every `break` close both.
pub fn loop(l: *Lowering, lp: ast.Loop) Error!?ValueId {
    const outer_depth = l.arenas.items.len;
    const state = try l.b.define(.{ .scope_enter = .{ .parent = try l.arena() } }, .arena);
    try l.pushArena(state, .owned);
    defer l.arenas.shrinkRetainingCapacity(outer_depth);

    if (lp.var_decl) |init| {
        if (!try statement(l, init.*)) return null;
    }
    const body_arena = try l.b.define(.{ .scope_enter = .{ .parent = state } }, .arena);
    try l.pushArena(body_arena, .owned);
    const body_var = l.innermostArenaVar();

    const header = try l.b.createBlock();
    const body = try l.b.createBlock();
    const latch = try l.b.createBlock();
    const exit = try l.b.createBlock();
    try l.b.jump(.{ .block = header });

    l.b.switchTo(header);
    const c = if (lp.condition) |condition| try l.cond(condition) else try l.b.define(.{ .tetra_holds = .{ .operand = try l.constant(.{ .tetra = .true }, .Tetra) } }, .cond);
    if (c) |go| try l.b.branch(go, .{ .block = body }, .{ .block = exit });
    try l.b.seal(body);

    l.b.switchTo(body);
    if (!l.b.isDead(body)) {
        try l.b.defVar(body_var, try l.b.define(.{ .scope_reset = .{ .arena = try l.b.useVar(body_var) } }, .arena));
        try l.loops.append(l.g.alloc, .{
            .break_block = exit,
            .continue_block = latch,
            .arena_depth = l.arenas.items.len,
            .defer_depth = l.defers.items.len,
        });
        const reached = try effect(l, lp.body);
        _ = l.loops.pop();
        if (reached != null) try l.b.jump(.{ .block = latch });
    }

    try l.b.seal(latch);
    l.b.switchTo(latch);
    if (!l.b.isDead(latch)) {
        try l.b.defVar(body_var, try l.b.define(.{ .scope_reset = .{ .arena = try l.b.useVar(body_var) } }, .arena));
        const stepped = if (lp.step) |step| try effect(l, step) else try l.nothing();
        if (stepped != null) try l.b.jump(.{ .block = header });
    }
    try l.b.seal(header);

    try l.b.seal(exit);
    if (l.b.isDead(exit)) return null;
    l.b.switchTo(exit);
    try l.exitArenas(outer_depth);
    return try l.nothing();
}

pub const Quantifier = enum { exists, forall };

/// `exists x in a : p` / `forall x in a : p`: a loop over `a` binding each
/// element to `x`. `exists` is `true` at the first element `p` holds for and
/// `false` after the last; `forall` is `false` at the first it does not and
/// `true` after the last.
pub fn quantifier(l: *Lowering, array: *ast.Expr, condition: *ast.Expr, storage: ?u32, kind: Quantifier) Error!?ValueId {
    const array_type = try l.g.typeOf(array);
    const element = array_type.Array.element.*;
    const items = try l.expr(array) orelse return null;
    const count = try l.define(.{ .array_len = .{ .operand = items } }, .Int);
    const index = try l.b.declareVar(.of(.Int));
    try l.b.defVar(index, try l.constant(.{ .int = 0 }, .Int));

    const header = try l.b.createBlock();
    const body = try l.b.createBlock();
    const next = try l.b.createBlock();
    const done = try l.b.createBlock();
    const answer = try l.b.blockParam(done, .of(.Tetra));
    const exhausted = try l.constant(.{ .tetra = if (kind == .exists) .false else .true }, .Tetra);
    const decided = try l.constant(.{ .tetra = if (kind == .exists) .true else .false }, .Tetra);
    try l.b.jump(.{ .block = header });

    l.b.switchTo(header);
    const i = try l.b.useVar(index);
    const more = try l.b.define(.{ .cmp = .{ .op = .Lt, .lhs = i, .rhs = count } }, .cond);
    try l.b.branch(more, .{ .block = body }, .{ .block = done, .args = &.{exhausted} });
    try l.b.seal(body);

    l.b.switchTo(body);
    const item = try l.define(.{ .array_get = .{ .array = items, .index = i } }, element);
    if (storage) |s| try l.declareLocal(s, "quantified", element, item);
    if (try l.cond(condition)) |holds| {
        if (kind == .exists) {
            try l.b.branch(holds, .{ .block = done, .args = &.{decided} }, .{ .block = next });
        } else {
            try l.b.branch(holds, .{ .block = next }, .{ .block = done, .args = &.{decided} });
        }
    }
    try l.b.seal(next);
    l.b.switchTo(next);
    if (!l.b.isDead(next)) {
        const one = try l.constant(.{ .int = 1 }, .Int);
        try l.b.defVar(index, try l.define(.{ .arith = .{ .op = .Add, .lhs = i, .rhs = one } }, .Int));
        try l.b.jump(.{ .block = header });
    }
    try l.b.seal(header);
    try l.b.seal(done);
    l.b.switchTo(done);
    return answer;
}

// ── `as` ──

/// `v as T then a else b`: the subject narrowed to `T` when it holds one,
/// else the `else` branch, which analysis requires.
pub fn cast(l: *Lowering, e: *ast.Expr, want: bool) Error!?ValueId {
    const data = e.data.Cast;
    const subject_type = try l.g.typeOf(data.value);
    const subject = try l.expr(data.value) orelse return null;
    const target = try l.g.lowerType(data.target.?);
    const join = try Join.init(l, e, want);

    const ok = try l.b.createBlock();
    const fail = try l.b.createBlock();
    switch (try typeTest(l, subject, subject_type, target)) {
        .always => try l.b.jump(.{ .block = ok }),
        .never => try l.b.jump(.{ .block = fail }),
        .cond => |c| try l.b.branch(c, .{ .block = ok }, .{ .block = fail }),
    }
    try l.b.seal(ok);
    try l.b.seal(fail);

    l.b.switchTo(fail);
    if (!l.b.isDead(fail)) {
        if (data.decl_else) |binding| try bindCast(l, data.decl_name.?, binding, subject, subject_type);
        try join.arm(l, data.else_branch.?);
    }

    l.b.switchTo(ok);
    if (!l.b.isDead(ok)) {
        if (data.decl_then) |binding| try bindCast(l, data.decl_name.?, binding, subject, subject_type);
        if (data.then_branch) |then| {
            // A block `then` runs for its effect; the cast's value is the
            // narrowed subject.
            if (then.data == .Block) {
                if (try effect(l, then) != null) try join.arrive(l, try l.convert(subject, subject_type, target), target);
            } else {
                try join.arm(l, then);
            }
        } else {
            try join.arrive(l, try l.convert(subject, subject_type, target), target);
        }
    }
    return join.finish(l);
}

/// Bind the name a cast declares for one branch to the subject, read as the
/// branch's narrowed type.
fn bindCast(l: *Lowering, name: []const u8, binding: ast.CastBinding, subject: ValueId, subject_type: HIRType) Error!void {
    const narrowed = try l.g.lowerType(binding.type_info);
    try l.declareLocal(binding.storage, name, narrowed, try l.convert(subject, subject_type, narrowed));
}

const Test = union(enum) {
    always,
    never,
    cond: ValueId,
};

/// Whether `subject`, of `subject_type`, holds a `target`: decided statically
/// for an unboxed subject, by its member for a box.
fn typeTest(l: *Lowering, subject: ValueId, subject_type: HIRType, target: HIRType) Error!Test {
    if (!subject_type.isBoxed()) return if (subject_type.eql(target)) .always else .never;
    var indices: []const u32 = undefined;
    if (target.isBoxed()) {
        // Narrowing to a smaller box: every member of the target.
        var all: std.ArrayListUnmanaged(u32) = .empty;
        for (try l.g.boxMembers(target)) |member| {
            for (try l.g.membersNamed(subject_type, member)) |i| try all.append(l.g.alloc, i);
        }
        indices = all.items;
    } else {
        indices = try l.g.membersNamed(subject_type, target);
    }
    if (indices.len == 0) return .never;
    if (indices.len == (try l.g.boxMembers(subject_type)).len) return .always;
    return .{ .cond = try l.b.define(.{ .member_test = .{ .operand = subject, .members = indices } }, .cond) };
}

// ── `match` ──

/// A `match` over an enum's variants or a box's members is one `switch`;
/// any other is a chain of tests, arm by arm, pattern by pattern.
pub fn matchExpr(l: *Lowering, e: *ast.Expr, want: bool) Error!?ValueId {
    const data = e.data.Match;
    const subject_type = try l.g.typeOf(data.value);
    const subject = try l.expr(data.value) orelse return null;
    const join = try Join.init(l, e, want);

    const bodies = try l.g.alloc.alloc(BlockId, data.cases.len);
    for (bodies) |*b| b.* = try l.b.createBlock();
    const fail = try l.b.createBlock();

    if (try switchCases(l, data.cases, subject, subject_type)) |plan| {
        const cases = try l.g.alloc.alloc(builder_mod.Case, plan.cases.len);
        for (plan.cases, cases) |case, *out| out.* = .{ .value = case.value, .target = .{ .block = bodies[case.arm] } };
        const default: BlockId = if (plan.default) |arm| bodies[arm] else fail;
        try l.b.switchOn(plan.operand, cases, .{ .block = default });
    } else {
        for (data.cases, bodies) |case, body| {
            if (try armChecks(l, case, subject, subject_type, body)) break;
        }
        if (!l.b.isTerminated()) try l.b.jump(.{ .block = fail });
    }

    try l.b.seal(fail);
    l.b.switchTo(fail);
    if (!l.b.isDead(fail)) {
        // No arm matched: a value match has no value to give.
        if (join.value != null) try l.b.unreachableTerm() else try join.arrive(l, try l.nothing(), .Nothing);
    }

    for (data.cases, bodies) |case, body| {
        try l.b.seal(body);
        if (l.b.isDead(body)) continue;
        l.b.switchTo(body);
        try bindDestructured(l, case, subject, subject_type);
        try join.arm(l, case.body);
    }
    return join.finish(l);
}

fn isElse(pattern: ast.Token) bool {
    return pattern.type == .ELSE or std.mem.eql(u8, pattern.lexeme, "else");
}

/// The path pattern spelling `case.patterns[i]`, if it has one.
fn pathOf(case: ast.MatchCase, i: usize) ?ast.MatchCase.PathPattern {
    for (case.path_patterns) |path| {
        if (path.pattern == i) return path;
    }
    return null;
}

const SwitchPlan = struct {
    operand: ValueId,
    cases: []const struct { value: i64, arm: usize },
    default: ?usize,
};

/// A `switch` for a match whose every pattern is a variant of the subject's
/// enum, or a member of the subject's box naming no variant, or `else`.
fn switchCases(l: *Lowering, cases: []const ast.MatchCase, subject: ValueId, subject_type: HIRType) Error!?SwitchPlan {
    if (subject_type != .Enum and !subject_type.isBoxed()) return null;
    const Entry = @typeInfo(@FieldType(SwitchPlan, "cases")).pointer.child;
    var entries: std.ArrayListUnmanaged(Entry) = .empty;
    var default: ?usize = null;
    for (cases, 0..) |case, arm| {
        for (case.patterns, 0..) |pattern, i| {
            if (isElse(pattern)) {
                if (default == null) default = arm;
                continue;
            }
            const resolved = if (i < case.resolved.len) case.resolved[i] else return null;
            const values: []const u32 = switch (subject_type) {
                .Enum => |id| switch (resolved) {
                    .variant => |ref| if (l.g.semantic.enum_table.idOf(ref) == id) &.{try lower_expr.variantIndex(l, id, pattern)} else return null,
                    else => return null,
                },
                else => switch (resolved) {
                    // A path naming a variant inside the member narrows it
                    // further than the box index can.
                    .type => |ref| blk: {
                        if (pathOf(case, i)) |path| {
                            if (!path.is_wildcard and path.field_names.len == 0 and path.tokens.len > 1) {
                                const split = path.split(groupName(l, subject_type));
                                if (split.variant != null) return null;
                            }
                        }
                        break :blk try l.g.membersNamed(subject_type, try l.g.typeForRef(ref));
                    },
                    .token => blk: {
                        const named = try tokenType(l, pattern, subject_type) orelse return null;
                        break :blk try l.g.membersNamed(subject_type, named);
                    },
                    .variant => return null,
                },
            };
            for (values) |v| {
                const taken = for (entries.items) |entry| {
                    if (entry.value == v) break true;
                } else false;
                if (!taken) try entries.append(l.g.alloc, .{ .value = v, .arm = arm });
            }
        }
    }
    const operand = if (subject_type == .Enum) subject else try l.define(.{ .member_index = .{ .operand = subject } }, .Int);
    return .{ .operand = operand, .cases = entries.items, .default = default };
}

fn groupName(l: *Lowering, ty: HIRType) []const u8 {
    return if (ty == .Group) l.g.semantic.group_table.displayName(ty.Group).? else "";
}

/// The type a builtin type pattern names against a box subject (`int`,
/// `string[]`, `nothing`), by the name it is shown with; null for a token
/// that names no type.
fn tokenType(l: *Lowering, pattern: ast.Token, subject_type: HIRType) Error!?HIRType {
    const names_type = switch (pattern.type) {
        .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE => true,
        .NOTHING => subject_type.isBoxed(),
        else => std.mem.indexOf(u8, pattern.lexeme, "[]") != null,
    };
    if (!names_type) return null;
    const spelled = if (pattern.type == .NOTHING) "nothing" else pattern.lexeme;
    if (!subject_type.isBoxed()) return null;
    for (try l.g.boxMembers(subject_type)) |member| {
        if (std.mem.eql(u8, try lower_expr.displayName(l, member), spelled)) return member;
    }
    // The box holds no such type: the pattern names one it can never be.
    return .Unknown;
}

/// Emit the tests of one arm, each jumping to `body` when it matches.
/// True when the arm always matches, so no later arm can run.
fn armChecks(l: *Lowering, case: ast.MatchCase, subject: ValueId, subject_type: HIRType, body: BlockId) Error!bool {
    for (case.patterns, 0..) |pattern, i| {
        const t = try patternTest(l, case, i, pattern, subject, subject_type);
        switch (t) {
            .always => {
                try l.b.jump(.{ .block = body });
                return true;
            },
            .never => {},
            .cond => |c| {
                const next = try l.b.createBlock();
                try l.b.branch(c, .{ .block = body }, .{ .block = next });
                try l.b.seal(next);
                l.b.switchTo(next);
            },
        }
    }
    return false;
}

fn patternTest(l: *Lowering, case: ast.MatchCase, i: usize, pattern: ast.Token, subject: ValueId, subject_type: HIRType) Error!Test {
    if (isElse(pattern)) return .always;
    const resolved: ast.MatchCase.Resolved = if (i < case.resolved.len) case.resolved[i] else .token;
    switch (resolved) {
        .type => |ref| {
            const named = try l.g.typeForRef(ref);
            const t = try typeTest(l, subject, subject_type, named);
            // A path naming a variant inside a group's enum member narrows to
            // that variant.
            if (pathOf(case, i)) |path| {
                if (!path.is_wildcard and path.field_names.len == 0 and subject_type == .Group and named == .Enum) {
                    if (path.split(groupName(l, subject_type)).variant) |variant| {
                        return memberEquals(l, t, subject, named, .{ .enum_variant = try lower_expr.variantIndex(l, named.Enum, variant) });
                    }
                }
            }
            return t;
        },
        .variant => |ref| {
            const id = l.g.semantic.enum_table.idOf(ref).?;
            const named: HIRType = .{ .Enum = id };
            const c: ir.Constant = .{ .enum_variant = try lower_expr.variantIndex(l, id, pattern) };
            return memberEquals(l, try typeTest(l, subject, subject_type, named), subject, named, c);
        },
        .token => {
            if (subject_type.isBoxed()) {
                if (try tokenType(l, pattern, subject_type)) |named| {
                    if (named == .Unknown) return .never;
                    return typeTest(l, subject, subject_type, named);
                }
            } else if (try staticTokenType(pattern)) |named| {
                return if (named.eql(subject_type)) .always else .never;
            }
            const lit = lower_expr.literalConstant(pattern.literal) orelse return .never;
            if (subject_type.isBoxed()) return memberEquals(l, try typeTest(l, subject, subject_type, lit.ty), subject, lit.ty, lit.c);
            const value = try l.convert(try l.constant(lit.c, lit.ty), lit.ty, subject_type);
            return .{ .cond = try l.b.define(.{ .cmp = .{ .op = .Eq, .lhs = subject, .rhs = value } }, .cond) };
        },
    }
}

/// A builtin type pattern over an unboxed subject: decided by its type.
fn staticTokenType(pattern: ast.Token) Error!?HIRType {
    return switch (pattern.type) {
        .INT_TYPE => .Int,
        .FLOAT_TYPE => .Float,
        .STRING_TYPE => .String,
        .BYTE_TYPE => .Byte,
        .TETRA_TYPE => .Tetra,
        .NOTHING_TYPE => .Nothing,
        else => null,
    };
}

/// `holds` and, when the subject is that member, its value equals `c`: a
/// subject that is the member compares directly; a box compares its payload
/// once it is known to hold the member.
fn memberEquals(l: *Lowering, holds: Test, subject: ValueId, member: HIRType, c: ir.Constant) Error!Test {
    const expected = try l.constant(c, member);
    switch (holds) {
        .never => return .never,
        .always => {
            const value = if (l.b.values.items[@intFromEnum(subject)].ty.eql(.of(member))) subject else try l.define(.{ .unbox = .{ .operand = subject } }, member);
            return .{ .cond = try l.b.define(.{ .cmp = .{ .op = .Eq, .lhs = value, .rhs = expected } }, .cond) };
        },
        .cond => |is_member| {
            const payload_block = try l.b.createBlock();
            const join = try l.b.createBlock();
            const answer = try l.b.blockParam(join, .cond);
            try l.b.branch(is_member, .{ .block = payload_block }, .{ .block = join, .args = &.{is_member} });
            try l.b.seal(payload_block);
            l.b.switchTo(payload_block);
            const payload = try l.define(.{ .unbox = .{ .operand = subject } }, member);
            const equal = try l.b.define(.{ .cmp = .{ .op = .Eq, .lhs = payload, .rhs = expected } }, .cond);
            try l.b.jump(.{ .block = join, .args = &.{equal} });
            try l.b.seal(join);
            l.b.switchTo(join);
            return .{ .cond = answer };
        },
    }
}

/// Bind each field a destructuring arm names: the arm's struct, read out of
/// the subject, field by field.
fn bindDestructured(l: *Lowering, case: ast.MatchCase, subject: ValueId, subject_type: HIRType) Error!void {
    if (case.path_patterns.len == 0 or case.path_patterns[0].field_names.len == 0) return;
    const path = case.path_patterns[0];
    const struct_id = switch (case.resolved[path.pattern]) {
        .type => |ref| l.g.semantic.struct_table.idOf(ref).?,
        .token, .variant => unreachable, // analysis reports a non-struct destructure
    };
    const object = try l.convert(subject, subject_type, .{ .Struct = struct_id });
    const fields = l.g.semantic.struct_table.fields(struct_id).?;
    for (path.field_names, path.field_storages) |name, storage| {
        const field = for (fields) |f| {
            if (std.mem.eql(u8, f.name, name.lexeme)) break f;
        } else unreachable; // analysis bound only declared fields
        const v = try l.define(.{ .field_get = .{ .object = object, .index = field.index } }, field.hir_type);
        try l.declareLocal(storage, name.lexeme, field.hir_type, v);
    }
}
