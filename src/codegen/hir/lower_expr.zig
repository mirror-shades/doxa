//! Expression lowering for the register HIR. Every function here lowers one
//! analyzed expression into the current block and returns its value, of the
//! type the analyzer gave the expression — or null when control never reaches
//! the end of it (a `return`, a `break`, `@exit` inside it).

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Location = @import("../../utils/reporting.zig").Location;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ErrorList = Errors.ErrorList;
const module_graph = @import("../../module/graph.zig");
const builtin_methods = @import("../../runtime/builtin_methods.zig");
const generator = @import("generator.zig");
const lower_control = @import("lower_control.zig");
const ir = @import("register/ir.zig");

const Lowering = generator.Lowering;
const Error = generator.Error;
const HIRType = ir.HIRType;
const ValueId = ir.ValueId;

/// Lower `e`; null when control leaves it.
pub fn expr(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const saved = l.b.location;
    l.b.location = e.base.location();
    defer l.b.location = saved;

    return switch (e.data) {
        .Literal => |lit| literal(l, e, lit),
        .InterpolatedString => |template| interpolated(l, template),
        .Variable => readName(l, e),
        .Grouping => |inner| if (inner) |g| expr(l, g) else try l.nothing(),
        .EnumMember => |member| enumConstant(l, e, member),
        .This => l.this.?,

        .Binary => binary(l, e),
        .Logical => |log| logical(l, e, log),
        .Unary => |u| unary(l, e, u),

        .If => lower_control.ifExpr(l, e, true),
        .Match => lower_control.matchExpr(l, e, true),
        .Loop => |loop| lower_control.loop(l, loop),
        .Block => lower_control.blockExpr(l, e, true),
        .ReturnExpr => |r| lower_control.returnFrom(l, r.value),
        .Unreachable => lower_control.unreachableAt(l, e.base.location()),
        .Cast => lower_control.cast(l, e, true),
        .ForAll => |q| lower_control.quantifier(l, q.array, q.condition, q.storage, .forall),
        .Exists => |q| lower_control.quantifier(l, q.array, q.condition, q.storage, .exists),
        .Break => lower_control.breakLoop(l),
        .Assert => |a| lower_control.assert(l, a.condition, a.message, a.location),

        .Array => arrayLiteral(l, e),
        .Map => |m| mapLiteral(l, e, m.entries, null),
        .MapLiteral => |m| mapLiteral(l, e, m.entries, m.else_value),
        .Index => index(l, e),
        .IndexAssign => |a| indexAssign(l, a.array, a.index, a.value),
        .Increment => |operand| step(l, e, operand, .Add),
        .Decrement => |operand| step(l, e, operand, .Sub),
        .Range => |r| range(l, e, r.start, r.end),

        .FunctionCall => functionCall(l, e),
        .InternalCall => internalCall(l, e),

        .StructLiteral => structLiteral(l, e),
        .FieldAccess => |fa| fieldAccess(l, e, fa),
        .FieldAssignment => |fa| fieldAssignment(l, fa.object, fa.field, fa.value),
        // Type declarations are compile time only: analysis registered them.
        .StructDecl, .EnumDecl, .GroupDecl => try l.nothing(),

        .Assignment => assignment(l, e),
        .Peek => |p| peek(l, p.expr, p.location),
        .PeekStruct => |p| peek(l, p.expr, p.location),
        // `@print` is parsed as an internal call; a default argument is
        // replaced at its call; the parser builds no `Input`; type-position
        // expressions never reach a value context the analyzer accepts.
        .Print, .DefaultArgPlaceholder, .Input, .Struct, .TypeExpr, .ArrayType => unexpected(l, e),
    };
}

fn unexpected(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const location = e.base.location();
    l.g.reporter.reportInternal(
        "the {s} expression at {s}:{d}:{d} reached lowering. This is a compiler bug, not an error in the program",
        .{ @tagName(std.meta.activeTag(e.data)), location.file, location.range.start_line, location.range.start_col },
        @src(),
    );
    return ErrorList.UnsupportedFunctionCallType;
}

/// Lower `e` as a branch condition: a comparison yields its `cond` directly;
/// any other tetra holds when it is `true` or `both` (`docs/tetras.md`).
pub fn cond(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    switch (e.data) {
        .Binary => |bin| if (compareOp(bin.operator.type)) |op| return compare(l, bin, op),
        .Grouping => |inner| if (inner) |g| return cond(l, g),
        else => {},
    }
    const v = try expr(l, e) orelse return null;
    return try l.b.define(.{ .tetra_holds = .{ .operand = v } }, .cond);
}

/// The value of an expression whose analyzed type is `nothing`, or `v`
/// brought to the analyzed type.
fn result(l: *Lowering, e: *ast.Expr, v: ValueId, ty: HIRType) Error!ValueId {
    const want = try l.g.typeOf(e);
    if (want == .Nothing) return l.nothing();
    return l.convert(v, ty, want);
}

// ── Leaves ──

fn literal(l: *Lowering, e: *ast.Expr, lit: ast.TokenLiteral) Error!?ValueId {
    const c: struct { c: ir.Constant, ty: HIRType } = switch (lit) {
        .int => |v| .{ .c = .{ .int = v }, .ty = .Int },
        .float => |v| .{ .c = .{ .float = v }, .ty = .Float },
        .byte => |v| .{ .c = .{ .byte = v }, .ty = .Byte },
        .string => |v| .{ .c = .{ .string = v }, .ty = .String },
        .tetra => |v| .{ .c = .{ .tetra = tetra(v) }, .ty = .Tetra },
        .nothing => .{ .c = .nothing, .ty = .Nothing },
        .array, .map => return unexpected(l, e),
    };
    const v = try l.constant(c.c, c.ty);
    return try l.convert(v, c.ty, try l.g.typeOf(e));
}

pub fn tetra(t: @import("../../types/types.zig").Tetra) ir.Tetra {
    return switch (t) {
        .false => .false,
        .true => .true,
        .both => .both,
        .neither => .neither,
    };
}

/// A literal pattern or quantifier comparand: its constant and type.
pub fn literalConstant(lit: ast.TokenLiteral) ?struct { c: ir.Constant, ty: HIRType } {
    return switch (lit) {
        .int => |v| .{ .c = .{ .int = v }, .ty = .Int },
        .float => |v| .{ .c = .{ .float = v }, .ty = .Float },
        .byte => |v| .{ .c = .{ .byte = v }, .ty = .Byte },
        .string => |v| .{ .c = .{ .string = v }, .ty = .String },
        .tetra => |v| .{ .c = .{ .tetra = tetra(v) }, .ty = .Tetra },
        .nothing => .{ .c = .nothing, .ty = .Nothing },
        .array, .map => null,
    };
}

fn interpolated(l: *Lowering, template: *ast.FormatTemplate) Error!?ValueId {
    var acc: ?ValueId = null;
    for (template.parts) |part| {
        const piece = switch (part) {
            .String => |text| blk: {
                if (text.len == 0 and acc != null) continue;
                break :blk try l.constant(.{ .string = text }, .String);
            },
            .Expression => |inner| try toString(l, inner) orelse return null,
        };
        acc = if (acc) |lhs| try l.define(.{ .str_concat = .{ .arena = try l.arena(), .lhs = lhs, .rhs = piece } }, .String) else piece;
    }
    return acc orelse try l.constant(.{ .string = "" }, .String);
}

/// `e` as a string: itself, or its text (`@string`, interpolation).
fn toString(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const ty = try l.g.typeOf(e);
    try l.g.markReflected(ty);
    const v = try renderable(l, e, ty) orelse return null;
    if (ty == .String) return v;
    return try l.define(.{ .to_string = .{ .arena = try l.arena(), .operand = v } }, .String);
}

/// `e`, as the runtime renders it: a fixed array is rendered as the dynamic
/// array it converts to.
fn renderable(l: *Lowering, e: *ast.Expr, ty: HIRType) Error!?ValueId {
    const v = try expr(l, e) orelse return null;
    if (ty != .Array or ty.Array.size == null) return v;
    return try l.convert(v, ty, try l.dynamicOf(ty));
}

/// Read the name at `e`: its storage, read as what the name denotes there —
/// the member a narrowing view proved, or the storage itself.
fn readName(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const target = try l.g.storeTarget(&e.base);
    const v = try l.readStorage(target, e.data.Variable.lexeme);
    return try l.convert(v, try l.g.lowerType(target.slot), try l.g.lowerType(target.read));
}

fn enumConstant(l: *Lowering, e: *ast.Expr, member: ast.Token) Error!?ValueId {
    const ty = try l.g.typeOf(e);
    // Analysis rejects a shorthand its context left untyped.
    const id = ty.Enum;
    return try l.constant(.{ .enum_variant = try variantIndex(l, id, member) }, ty);
}

/// The index of the variant `token` names in enum `id`.
pub fn variantIndex(l: *Lowering, id: ir.EnumId, token: ast.Token) Error!u32 {
    const table = &l.g.semantic.enum_table;
    for (table.variants(id) orelse &.{}) |variant| {
        if (std.mem.eql(u8, variant.name, token.lexeme)) return variant.index;
    }
    l.g.report(ast.SourceSpan.fromToken(token).location, ErrorCode.VARIABLE_NOT_FOUND, "Unknown enum variant '{s}' for enum '{s}'", .{ token.lexeme, table.displayName(id).? });
    return ErrorList.InvalidEnumVariant;
}

// ── Operators ──

fn compareOp(t: anytype) ?ir.CompareOp {
    return switch (t) {
        .EQUALITY => .Eq,
        .BANG_EQUAL => .Ne,
        .LESS => .Lt,
        .GREATER => .Gt,
        .LESS_EQUAL => .Le,
        .GREATER_EQUAL => .Ge,
        else => null,
    };
}

fn arithOp(t: anytype) ?ir.ArithOp {
    return switch (t) {
        .PLUS => .Add,
        .MINUS => .Sub,
        .ASTERISK => .Mul,
        .SLASH => .Div,
        .DOUBLE_SLASH => .IntDiv,
        .MODULO => .Mod,
        .POWER => .Pow,
        else => null,
    };
}

fn binary(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const bin = e.data.Binary;
    if (compareOp(bin.operator.type)) |op| {
        const c = try compare(l, bin, op) orelse return null;
        return try l.define(.{ .tetra_from_cond = .{ .operand = c } }, .Tetra);
    }
    const op = arithOp(bin.operator.type) orelse {
        l.g.report(bin.left.?.base.location(), ErrorCode.UNSUPPORTED_OPERATOR, "Unsupported binary operator: {}", .{bin.operator.type});
        return ErrorList.UnsupportedOperator;
    };
    // Arithmetic computes in the type analysis gave the expression: the
    // operands' common type, or `float` for `/`. `+` of strings and of
    // arrays concatenates.
    const ty = try l.g.typeOf(e);
    const lhs = try l.exprAs(bin.left.?, ty) orelse return null;
    const rhs = try l.exprAs(bin.right.?, ty) orelse return null;
    if (op == .Add and ty == .String) return try l.define(.{ .str_concat = .{ .arena = try l.arena(), .lhs = lhs, .rhs = rhs } }, ty);
    if (op == .Add and ty == .Array) return try l.define(.{ .array_concat = .{ .arena = try l.arena(), .lhs = lhs, .rhs = rhs } }, ty);
    return try l.define(.{ .arith = .{ .op = op, .lhs = lhs, .rhs = rhs } }, ty);
}

/// A comparison, as a `cond`. Numbers compare in their common type; a box
/// compares against a value of one of its members by member and payload.
fn compare(l: *Lowering, bin: ast.Binary, op: ir.CompareOp) Error!?ValueId {
    const left = try l.g.typeOf(bin.left.?);
    const right = try l.g.typeOf(bin.right.?);
    if (left.isBoxed() or right.isBoxed()) return boxedCompare(l, bin, op, left, right);
    const common: HIRType = if (left == .Float or right == .Float)
        .Float
    else if (isNumber(left) and isNumber(right) and (left == .Int or right == .Int))
        .Int
    else
        left;
    const lhs = try l.exprAs(bin.left.?, common) orelse return null;
    const rhs = try l.exprAs(bin.right.?, common) orelse return null;
    return try l.b.define(.{ .cmp = .{ .op = op, .lhs = lhs, .rhs = rhs } }, .cond);
}

fn isNumber(t: HIRType) bool {
    return t == .Int or t == .Byte or t == .Float;
}

/// `box == value` for a value of an enum or an `int` member of the box: equal
/// when the box holds that member and its payload is the value. A box holding
/// another member is unequal whatever its payload, so `IOError.Denied` never
/// equals `ParseError.Eof` though both are variant 1.
fn boxedCompare(l: *Lowering, bin: ast.Binary, op: ir.CompareOp, left: HIRType, right: HIRType) Error!?ValueId {
    const boxed_on_left = left.isBoxed();
    const boxed = if (boxed_on_left) left else right;
    const member = if (boxed_on_left) right else left;
    const location = bin.left.?.base.location();
    if (member.isBoxed()) {
        l.g.report(location, ErrorCode.TYPE_MISMATCH, "Cannot compare two {s} values directly; narrow one side with 'as' or match first", .{@tagName(left)});
        return ErrorList.TypeMismatch;
    }
    if (op != .Eq and op != .Ne) {
        l.g.report(location, ErrorCode.TYPE_MISMATCH, "Cannot order a {s} value; narrow it with 'as' or match first", .{@tagName(boxed)});
        return ErrorList.TypeMismatch;
    }
    if (member != .Enum and member != .Int) {
        l.g.report(location, ErrorCode.TYPE_MISMATCH, "Cannot compare a union or group value with a {s} directly; narrow it with 'as' or match first", .{@tagName(member)});
        return ErrorList.TypeMismatch;
    }
    const members = try l.g.membersNamed(boxed, member);
    if (members.len == 0) {
        l.g.report(location, ErrorCode.TYPE_MISMATCH, "Cannot compare {s} with {s}: the {s} can never hold that type", .{ @tagName(left), @tagName(right), @tagName(boxed) });
        return ErrorList.TypeMismatch;
    }

    const lhs = try expr(l, bin.left.?) orelse return null;
    const rhs = try expr(l, bin.right.?) orelse return null;
    const box_value = if (boxed_on_left) lhs else rhs;
    const member_value = if (boxed_on_left) rhs else lhs;

    // Another member decides the answer without the payload.
    const holds_block = try l.b.createBlock();
    const join = try l.b.createBlock();
    const answer = try l.b.blockParam(join, .cond);
    const holds = try l.b.define(.{ .member_test = .{ .operand = box_value, .members = members[0..1] } }, .cond);
    const decided = try l.constant(.{ .tetra = if (op == .Eq) .false else .true }, .Tetra);
    const other = try l.b.define(.{ .tetra_holds = .{ .operand = decided } }, .cond);
    try l.b.branch(holds, .{ .block = holds_block }, .{ .block = join, .args = &.{other} });
    try l.b.seal(holds_block);
    l.b.switchTo(holds_block);
    const payload = try l.define(.{ .unbox = .{ .operand = box_value } }, member);
    const equal = try l.b.define(.{ .cmp = .{ .op = op, .lhs = payload, .rhs = member_value } }, .cond);
    try l.b.jump(.{ .block = join, .args = &.{equal} });
    try l.b.seal(join);
    l.b.switchTo(join);
    return answer;
}

fn logical(l: *Lowering, e: *ast.Expr, log: ast.Logical) Error!?ValueId {
    const op: ir.TetraOp = switch (log.operator.type) {
        .AND => return shortCircuit(l, log, .@"and"),
        .OR => return shortCircuit(l, log, .@"or"),
        .IFF => .iff,
        .XOR => .xor,
        .NAND => .nand,
        .NOR => .nor,
        .IMPLIES => .implies,
        else => {
            l.g.report(log.left.base.location(), ErrorCode.UNSUPPORTED_OPERATOR, "Unsupported logical operator: {}", .{log.operator.type});
            return ErrorList.UnsupportedOperator;
        },
    };
    _ = e;
    const lhs = try expr(l, log.left) orelse return null;
    const rhs = try expr(l, log.right) orelse return null;
    return try l.define(.{ .tetra_binary = .{ .op = op, .lhs = lhs, .rhs = rhs } }, .Tetra);
}

/// `a and b`: `b` when `a` holds, else `false`; `a or b`: `true` when `a`
/// holds, else `b`. The right side runs only when it decides the value.
fn shortCircuit(l: *Lowering, log: ast.Logical, op: ir.CondOp) Error!?ValueId {
    const left = try cond(l, log.left) orelse return null;
    const rhs_block = try l.b.createBlock();
    const join = try l.b.createBlock();
    const value = try l.b.blockParam(join, .of(.Tetra));
    const decided = try l.constant(.{ .tetra = if (op == .@"and") .false else .true }, .Tetra);
    if (op == .@"and") {
        try l.b.branch(left, .{ .block = rhs_block }, .{ .block = join, .args = &.{decided} });
    } else {
        try l.b.branch(left, .{ .block = join, .args = &.{decided} }, .{ .block = rhs_block });
    }
    try l.b.seal(rhs_block);
    l.b.switchTo(rhs_block);
    if (try expr(l, log.right)) |right| try l.b.jump(.{ .block = join, .args = &.{right} });
    try l.b.seal(join);
    l.b.switchTo(join);
    return value;
}

fn unary(l: *Lowering, e: *ast.Expr, u: ast.Unary) Error!?ValueId {
    const ty = try l.g.typeOf(e);
    switch (u.operator.type) {
        .NOT => {
            const v = try expr(l, u.right.?) orelse return null;
            return try l.define(.{ .tetra_not = .{ .operand = v } }, .Tetra);
        },
        .MINUS => {
            const v = try l.exprAs(u.right.?, ty) orelse return null;
            return try l.define(.{ .neg = .{ .operand = v } }, ty);
        },
        .PLUS => return l.exprAs(u.right.?, ty),
        else => {
            l.g.report(ast.SourceSpan.fromToken(u.operator).location, ErrorCode.UNSUPPORTED_OPERATOR, "Unsupported unary operator: {}", .{u.operator.type});
            return ErrorList.UnsupportedOperator;
        },
    }
}

// ── Stores ──

fn assignment(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const assign = e.data.Assignment;
    const target = try l.g.storeTarget(&e.base);
    const slot_type = try l.g.lowerType(target.slot);
    const v = try l.exprAs(assign.value.?, slot_type) orelse return null;
    try l.writeStorage(target, assign.name.lexeme, v);
    return try result(l, e, v, slot_type);
}

/// `x++` / `x--`: the operand's new value. A name is stored back; any other
/// operand only computes the value.
/// TODO: an index or field operand (`a[i]++`) is computed and not stored back,
/// as the stack HIR did; whether it should store is undecided.
fn step(l: *Lowering, e: *ast.Expr, operand: *ast.Expr, op: ir.ArithOp) Error!?ValueId {
    const ty = try l.g.typeOf(operand);
    const current = try expr(l, operand) orelse return null;
    const one = try l.convert(try l.constant(.{ .int = 1 }, .Int), .Int, ty);
    const next = try l.define(.{ .arith = .{ .op = op, .lhs = current, .rhs = one } }, ty);
    if (operand.data == .Variable) {
        const target = try l.g.storeTarget(&operand.base);
        const slot_type = try l.g.lowerType(target.slot);
        try l.writeStorage(target, operand.data.Variable.lexeme, try l.convert(next, ty, slot_type));
    }
    return try result(l, e, next, ty);
}

/// Store `value`, of `ty`, back into what `target` names: a name, or a field.
/// What an intrinsic that replaces its subject (`@push` on a string) writes.
fn storeBack(l: *Lowering, target: *ast.Expr, value: ValueId, ty: HIRType) Error!void {
    switch (target.data) {
        .Variable => |token| {
            const store = try l.g.storeTarget(&target.base);
            try l.writeStorage(store, token.lexeme, try l.convert(value, ty, try l.g.lowerType(store.slot)));
        },
        .FieldAccess => |fa| {
            const field = try fieldSlot(l, fa.object, fa.field);
            const object = try objectOf(l, fa.object, field.struct_id) orelse return;
            try l.effect(.{ .field_set = .{ .object = object, .index = field.index, .value = try l.convert(value, ty, field.ty) } });
        },
        // A subject that names no storage has nowhere to keep the result.
        else => {},
    }
}

// ── Collections ──

fn arrayLiteral(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    return arrayLiteralAs(l, e, try l.g.typeOf(e));
}

/// An array literal built as an array of type `ty`.
pub fn arrayLiteralAs(l: *Lowering, e: *ast.Expr, ty: HIRType) Error!?ValueId {
    const elements = e.data.Array;
    const element = ty.Array.element.*;
    const length: ?ValueId = if (ty.Array.size == null) try l.constant(.{ .int = @intCast(elements.len) }, .Int) else null;
    const array = try l.define(.{ .array_new = .{ .arena = try l.arena(), .length = length } }, ty);
    for (elements, 0..) |element_expr, i| {
        const v = try l.exprAs(element_expr, element) orelse return null;
        const at = try l.constant(.{ .int = @intCast(i) }, .Int);
        try l.effect(.{ .array_set = .{ .array = array, .index = at, .value = v } });
    }
    return array;
}

fn mapLiteral(l: *Lowering, e: *ast.Expr, entries: []*ast.MapEntry, else_expr: ?*ast.Expr) Error!?ValueId {
    const ty = try l.g.typeOf(e);
    const key_type = ty.Map.key.*;
    const value_type = ty.Map.value.*;
    const else_value: ?ValueId = if (else_expr) |x| try l.exprAs(x, value_type) orelse return null else null;
    const map = try l.define(.{ .map_new = .{ .arena = try l.arena(), .else_value = else_value } }, ty);
    for (entries) |entry| {
        const key = try l.exprAs(entry.key, key_type) orelse return null;
        const value = try l.exprAs(entry.value, value_type) orelse return null;
        try l.effect(.{ .map_set = .{ .map = map, .key = key, .value = value } });
    }
    return map;
}

fn index(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const idx = e.data.Index;
    const container_type = try l.g.typeOf(idx.array);
    const container = try expr(l, idx.array) orelse return null;
    switch (container_type) {
        .Map => |m| {
            const key = try l.exprAs(idx.index, m.key.*) orelse return null;
            // A map without an `else` yields `value | nothing`.
            return try l.define(.{ .map_get = .{ .map = container, .key = key } }, try l.g.typeOf(e));
        },
        .Array => |a| {
            const at = try l.exprAs(idx.index, .Int) orelse return null;
            const v = try l.define(.{ .array_get = .{ .array = container, .index = at } }, a.element.*);
            return try l.convert(v, a.element.*, try l.g.typeOf(e));
        },
        .String => {
            const at = try l.exprAs(idx.index, .Int) orelse return null;
            return try l.define(.{ .str_char = .{ .arena = try l.arena(), .string = container, .index = at } }, .String);
        },
        else => return unexpected(l, e),
    }
}

/// `a[i] is v`, `m[k] is v`: an in-place element or entry store, which the
/// runtime re-homes into the container's arena.
fn indexAssign(l: *Lowering, array: *ast.Expr, at: *ast.Expr, value: *ast.Expr) Error!?ValueId {
    const container_type = try l.g.typeOf(array);
    const container = try expr(l, array) orelse return null;
    switch (container_type) {
        .Map => |m| {
            const key = try l.exprAs(at, m.key.*) orelse return null;
            const v = try l.exprAs(value, m.value.*) orelse return null;
            try l.effect(.{ .map_set = .{ .map = container, .key = key, .value = v } });
        },
        .Array => |a| {
            const i = try l.exprAs(at, .Int) orelse return null;
            // TODO(flat literal store): a struct literal stored into a flat
            // element is built in this arena and copied; it should write the
            // element's fields in place (plan/register-hir.md, stage 2 notes).
            const v = try l.exprAs(value, a.element.*) orelse return null;
            try l.effect(.{ .array_set = .{ .array = container, .index = i, .value = v } });
        },
        else => return unexpected(l, array),
    }
    return try l.nothing();
}

fn range(l: *Lowering, e: *ast.Expr, start: *ast.Expr, end: *ast.Expr) Error!?ValueId {
    const s = try l.exprAs(start, .Int) orelse return null;
    const t = try l.exprAs(end, .Int) orelse return null;
    return try l.define(.{ .array_range = .{ .arena = try l.arena(), .start = s, .end = t } }, try l.g.typeOf(e));
}

// ── Structs ──

pub const FieldSlot = struct { struct_id: ir.StructId, index: u32, ty: HIRType };

/// The field `object.field` names. The object's type is the analyzer's: a
/// struct, or a group whose single struct member declaring the field is the
/// one read.
pub fn fieldSlot(l: *Lowering, object: *ast.Expr, field: ast.Token) Error!FieldSlot {
    const struct_id = switch (try l.g.typeOf(object)) {
        .Struct => |id| id,
        .Group => |group_id| l.g.type_system.groupMemberStructForField(group_id, field.lexeme) orelse return unresolvedField(l, field),
        else => return unresolvedField(l, field),
    };
    for (l.g.semantic.struct_table.fields(struct_id).?) |declared| {
        if (std.mem.eql(u8, declared.name, field.lexeme)) return .{ .struct_id = struct_id, .index = declared.index, .ty = declared.hir_type };
    }
    return unresolvedField(l, field);
}

fn unresolvedField(l: *Lowering, field: ast.Token) Error {
    const location = ast.SourceSpan.fromToken(field).location;
    l.g.reporter.reportInternal(
        "no analyzed struct declares the field '{s}' at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
        .{ field.lexeme, location.file, location.range.start_line, location.range.start_col },
        @src(),
    );
    return ErrorList.MissingExpressionType;
}

/// The struct `object` is, read as struct `struct_id`: a group value is the
/// member a narrowing proved.
fn objectOf(l: *Lowering, object: *ast.Expr, struct_id: ir.StructId) Error!?ValueId {
    const v = try expr(l, object) orelse return null;
    return try l.convert(v, try l.g.typeOf(object), .{ .Struct = struct_id });
}

fn fieldAccess(l: *Lowering, e: *ast.Expr, fa: ast.FieldAccess) Error!?ValueId {
    // `Color.Red`, however `Color` is reached, is a variant constant.
    if (l.g.resolutionOf(fa.object)) |resolved| {
        if (resolved == .type) {
            if (l.g.semantic.enum_table.idOf(resolved.type)) |id| {
                return try l.constant(.{ .enum_variant = try variantIndex(l, id, fa.field) }, .{ .Enum = id });
            }
        }
    }
    const field = try fieldSlot(l, fa.object, fa.field);
    const object = try objectOf(l, fa.object, field.struct_id) orelse return null;
    const v = try l.define(.{ .field_get = .{ .object = object, .index = field.index } }, field.ty);
    return try l.convert(v, field.ty, try l.g.typeOf(e));
}

/// `o.f is v`: an in-place field store, which the runtime re-homes into the
/// struct's arena. A nested object (`a.b.c is v`) is reached through the
/// field that holds it, which is the struct itself, not a copy.
fn fieldAssignment(l: *Lowering, object: *ast.Expr, field_token: ast.Token, value: *ast.Expr) Error!?ValueId {
    const field = try fieldSlot(l, object, field_token);
    const o = try objectOf(l, object, field.struct_id) orelse return null;
    const v = try l.exprAs(value, field.ty) orelse return null;
    try l.effect(.{ .field_set = .{ .object = o, .index = field.index, .value = v } });
    return try l.nothing();
}

fn structLiteral(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const literal_data = e.data.StructLiteral;
    const ref = l.g.resolutionOf(e).?.type;
    const struct_id = l.g.semantic.struct_table.idOf(ref).?;
    const declared = l.g.semantic.struct_table.fields(struct_id).?;
    if (!validateLiteralFields(l, literal_data.name, literal_data.fields, declared)) return ErrorList.TypeMismatch;

    // Fields are laid out in declared order, whatever order the literal
    // writes them in, and evaluated in that order.
    const values = try l.g.alloc.alloc(ValueId, declared.len);
    for (declared, values) |field, *out| {
        const written = for (literal_data.fields) |f| {
            if (std.mem.eql(u8, f.name.lexeme, field.name)) break f;
        } else unreachable; // `validateLiteralFields` matched every field
        out.* = try l.exprAs(written.value, field.hir_type) orelse return null;
    }
    return try l.define(.{ .struct_new = .{ .arena = try l.arena(), .fields = values } }, .{ .Struct = struct_id });
}

/// Report every undeclared, missing or duplicate field of a struct literal.
/// False when the literal does not map onto the declaration one-to-one.
fn validateLiteralFields(
    l: *Lowering,
    name: ast.Token,
    written: []const *ast.StructInstanceField,
    declared: anytype,
) bool {
    var valid = true;
    if (written.len != declared.len) {
        l.g.report(ast.SourceSpan.fromToken(name).location, ErrorCode.STRUCT_FIELD_COUNT_MISMATCH, "struct '{s}' expects {d} field{s}, but this literal provides {d}", .{
            name.lexeme, declared.len, if (declared.len == 1) "" else "s", written.len,
        });
        valid = false;
    }
    for (written) |w| {
        const found = for (declared) |d| {
            if (std.mem.eql(u8, d.name, w.name.lexeme)) break true;
        } else false;
        if (found) continue;
        var list: std.ArrayListUnmanaged(u8) = .empty;
        for (declared, 0..) |d, i| {
            if (i > 0) list.appendSlice(l.g.alloc, ", ") catch {};
            list.appendSlice(l.g.alloc, d.name) catch {};
        }
        if (declared.len == 0) list.appendSlice(l.g.alloc, "(none)") catch {};
        l.g.report(ast.SourceSpan.fromToken(w.name).location, ErrorCode.STRUCT_FIELD_NAME_MISMATCH, "struct '{s}' has no field '{s}'; declared fields: {s}", .{ name.lexeme, w.name.lexeme, list.items });
        valid = false;
    }
    for (declared) |d| {
        const found = for (written) |w| {
            if (std.mem.eql(u8, d.name, w.name.lexeme)) break true;
        } else false;
        if (!found) {
            l.g.report(ast.SourceSpan.fromToken(name).location, ErrorCode.STRUCT_FIELD_NAME_MISMATCH, "struct '{s}' is missing field '{s}'", .{ name.lexeme, d.name });
            valid = false;
        }
    }
    return valid;
}

// ── Calls ──

/// Call the Doxa function `id` with `args`, `@caller` included; its value, or
/// `nothing` for a function that returns none.
pub fn callDoxa(l: *Lowering, id: ir.FunctionId, args: []const ValueId) Error!ValueId {
    const ret = l.g.functionDecl(id).signature.ret;
    const op: ir.Op = .{ .call = .{ .callee = id, .args = try l.g.alloc.dupe(ValueId, args) } };
    if (ret == .Nothing) {
        try l.effect(op);
        return l.nothing();
    }
    return l.define(op, ret);
}

fn functionCall(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const call_data = e.data.FunctionCall;
    const resolution = l.g.resolutionOf(call_data.callee) orelse {
        l.g.report(call_data.callee.base.location(), ErrorCode.INTERNAL_ERROR, "call target was not resolved by analysis. This is a compiler bug, not an error in the program", .{});
        return ErrorList.UnsupportedFunctionCallType;
    };
    const v: ?ValueId, const ty: HIRType = switch (resolution) {
        .function => |symbol| blk: {
            const id = l.g.functionId(.{ .module = symbol.module, .name = symbol.name });
            break :blk .{ try doxaCall(l, id, null, call_data.arguments), l.g.functionDecl(id).signature.ret };
        },
        .static_method => |method| blk: {
            const id = try l.g.methodId(method);
            break :blk .{ try doxaCall(l, id, null, call_data.arguments), l.g.functionDecl(id).signature.ret };
        },
        .method => |method| blk: {
            const id = try l.g.methodId(method);
            break :blk .{ try doxaCall(l, id, call_data.callee.data.FieldAccess.object, call_data.arguments), l.g.functionDecl(id).signature.ret };
        },
        .zig_function => |symbol| blk: {
            const id = try l.g.zigId(symbol);
            break :blk .{ try zigCall(l, id, call_data.arguments), l.g.zigSignature(id).ret };
        },
        .namespace, .global, .type => return unexpected(l, e),
    };
    return try result(l, e, v orelse return null, ty);
}

/// A member-typed variable lent to a union `^` parameter: boxed into a
/// temporary slot at the call, which is lent, and read back after it.
/// Analysis admits such a loan only when the callee keeps the member.
const MemberLoan = struct {
    slot: ir.SlotId,
    target: *ast.Expr,
    box_type: HIRType,
};

fn doxaCall(l: *Lowering, id: ir.FunctionId, receiver: ?*ast.Expr, arguments: []const ast.CallArgument) Error!?ValueId {
    const decl = l.g.functionDecl(id);
    var args: std.ArrayListUnmanaged(ValueId) = .empty;
    var loans: std.ArrayListUnmanaged(MemberLoan) = .empty;
    try args.append(l.g.alloc, try l.arena());
    if (receiver) |r| {
        const object = try objectOf(l, r, decl.receiver.?) orelse return null;
        try args.append(l.g.alloc, object);
    }
    for (arguments, 0..) |arg, i| {
        const param_type = decl.param_types[i];
        if (arg.expr.data == .DefaultArgPlaceholder) {
            const default = decl.params[i].default_value orelse {
                l.g.report(arg.expr.base.location(), ErrorCode.NO_DEFAULT_VALUE_FOR_PARAMETER, "No default value for parameter {} in function '{s}'", .{ i, module_graph.displayName(decl.link_name) });
                return ErrorList.InvalidArgumentCount;
            };
            try args.append(l.g.alloc, try l.exprAs(default, param_type) orelse return null);
        } else if (arg.is_alias) {
            try lendArgument(l, arg.expr, param_type, &args, &loans);
        } else {
            try args.append(l.g.alloc, try l.exprAs(arg.expr, param_type) orelse return null);
        }
    }
    const v = try callDoxa(l, id, args.items);
    for (loans.items) |loan| {
        const ref = try l.b.define(.{ .slot_addr = loan.slot }, .{ .ref = loan.box_type });
        const lent = try l.define(.{ .load = .{ .operand = ref } }, loan.box_type);
        const target = try l.g.storeTarget(&loan.target.base);
        const member = try l.g.lowerType(target.slot);
        if (member == .Array and member.Array.size != null) {
            // A fixed array keeps its buffer: the elements are copied back.
            const fixed = try l.readStorage(target, loan.target.data.Variable.lexeme);
            try l.effect(.{ .array_copy_to_fixed = .{ .fixed = fixed, .array = lent } });
            continue;
        }
        try l.writeStorage(target, loan.target.data.Variable.lexeme, try l.convert(lent, loan.box_type, member));
    }
    return v;
}

/// Pass the variable `e` names as a `^` argument of type `param_type`: its
/// slot's address and arena, or a `^` parameter's own.
fn lendArgument(l: *Lowering, e: *ast.Expr, param_type: HIRType, args: *std.ArrayListUnmanaged(ValueId), loans: *std.ArrayListUnmanaged(MemberLoan)) Error!void {
    if (e.data != .Variable) {
        l.g.report(e.base.location(), ErrorCode.INVALID_ALIAS_ARGUMENT, "Alias argument must be a variable (e.g., ^myVar)", .{});
        return ErrorList.InvalidAliasArgument;
    }
    const target = try l.g.storeTarget(&e.base);
    const slot_type = try l.g.lowerType(target.slot);
    if (!slot_type.eql(param_type)) {
        // A member lent to a union parameter, or a fixed array to a dynamic
        // one, goes through a converted temporary that is read back after.
        const arena_value = try l.arena();
        const slot = try l.b.addSlot(param_type, arena_value, "loan");
        const ref = try l.b.define(.{ .slot_addr = slot }, .{ .ref = param_type });
        const current = try l.readStorage(target, e.data.Variable.lexeme);
        const converted = try l.convert(current, slot_type, param_type);
        try l.effect(.{ .store = .{ .ref = ref, .value = try l.rehome(arena_value, converted, param_type) } });
        try args.append(l.g.alloc, ref);
        try args.append(l.g.alloc, arena_value);
        try loans.append(l.g.alloc, .{ .slot = slot, .target = e, .box_type = param_type });
        return;
    }
    if (target.global) {
        const id = try l.g.globalId(e.data.Variable.lexeme, slot_type);
        try args.append(l.g.alloc, try l.b.define(.{ .global_addr = .{ .global = id, .root = l.root } }, .{ .ref = slot_type }));
        try args.append(l.g.alloc, l.root);
        return;
    }
    switch (l.local(target.storage)) {
        .slot => |slot| {
            try args.append(l.g.alloc, try l.b.define(.{ .slot_addr = slot }, .{ .ref = slot_type }));
            try args.append(l.g.alloc, l.b.slots.items[@intFromEnum(slot)].arena);
        },
        .alias => |a| {
            try args.append(l.g.alloc, a.ref);
            try args.append(l.g.alloc, a.arena);
        },
        // `collectLent` made every lent local a slot.
        .ssa => unreachable,
    }
}

fn zigCall(l: *Lowering, id: ir.ZigFunctionId, arguments: []const ast.CallArgument) Error!?ValueId {
    const sig = l.g.zigSignature(id);
    var args: std.ArrayListUnmanaged(ValueId) = .empty;
    var first: usize = 0;
    if (sig.callerArena() != null) {
        try args.append(l.g.alloc, try l.arena());
        first = 1;
    }
    for (arguments, sig.params[first..]) |arg, param| {
        try args.append(l.g.alloc, try l.exprAs(arg.expr, param.doxa) orelse return null);
    }
    const op: ir.Op = .{ .call_zig = .{ .callee = id, .args = args.items } };
    if (sig.ret == .Nothing) {
        try l.effect(op);
        return try l.nothing();
    }
    return try l.define(op, sig.ret);
}

// ── Intrinsics ──

fn internalCall(l: *Lowering, e: *ast.Expr) Error!?ValueId {
    const call_data = e.data.InternalCall;
    const name = call_data.method.lexeme;
    const subject = call_data.receiver;
    const args = call_data.arguments;
    if (!std.mem.eql(u8, name, "std")) {
        if (builtin_methods.getArgCountRangeByName(name)) |count| {
            if (args.len + 1 < count.min or args.len + 1 > count.max) return ErrorList.InvalidArgumentCount;
        }
    }

    const Intrinsic = enum { type, length, int, float, string, pack, unpack, byte, push, pop, insert, remove, slice, clear, find, exit, panic, print, std };
    const which = std.meta.stringToEnum(Intrinsic, name) orelse {
        l.g.report(e.base.location(), ErrorCode.NOT_IMPLEMENTED, "unimplemented built-in method '@{s}'", .{name});
        return ErrorList.NotImplemented;
    };
    const subject_type: HIRType = if (which == .std) .Nothing else try l.g.typeOf(subject);
    switch (which) {
        // `@std()` is the standard library's specifier; it names no install
        // location.
        .std => return try l.constant(.{ .string = module_graph.std_specifier }, .String),
        .type => return try l.constant(.{ .string = typeName(try l.g.typeInfoOf(subject)) }, .String),
        .length => {
            const v = try expr(l, subject) orelse return null;
            const op: ir.Op = if (subject_type == .String) .{ .str_len = .{ .operand = v } } else .{ .array_len = .{ .operand = v } };
            return try l.define(op, .Int);
        },
        .int, .float, .byte => {
            const want: HIRType = switch (which) {
                .int => .Int,
                .float => .Float,
                else => .Byte,
            };
            const v = try expr(l, subject) orelse return null;
            if (subject_type != .String) return try l.convert(v, subject_type, want);
            const op: ir.Op = switch (which) {
                .int => .{ .str_to_int = .{ .operand = v } },
                .float => .{ .str_to_float = .{ .operand = v } },
                else => .{ .str_to_byte = .{ .operand = v } },
            };
            return try l.define(op, want);
        },
        .string => return toString(l, subject),
        .pack => {
            const v = try expr(l, subject) orelse return null;
            return try l.define(.{ .str_pack = .{ .arena = try l.arena(), .bytes = v } }, .String);
        },
        .unpack => {
            const v = try expr(l, subject) orelse return null;
            return try l.define(.{ .str_unpack = .{ .arena = try l.arena(), .string = v } }, try l.g.typeOf(e));
        },
        .push => {
            const target = try expr(l, subject) orelse return null;
            if (subject_type == .String) {
                const tail = try l.exprAs(args[0], .String) orelse return null;
                const joined = try l.define(.{ .str_concat = .{ .arena = try l.arena(), .lhs = target, .rhs = tail } }, .String);
                try storeBack(l, subject, joined, .String);
            } else {
                const v = try l.exprAs(args[0], subject_type.Array.element.*) orelse return null;
                try l.effect(.{ .array_push = .{ .array = target, .value = v } });
            }
            return try l.nothing();
        },
        .pop => {
            const target = try expr(l, subject) orelse return null;
            if (subject_type == .String) {
                const last = try l.define(.{ .str_last = .{ .arena = try l.arena(), .string = target } }, .String);
                const rest = try l.define(.{ .str_drop_last = .{ .arena = try l.arena(), .string = target } }, .String);
                try storeBack(l, subject, rest, .String);
                return try result(l, e, last, .String);
            }
            const element = subject_type.Array.element.*;
            const v = try l.define(.{ .array_pop = .{ .operand = target } }, element);
            return try result(l, e, v, element);
        },
        .insert => {
            const target = try expr(l, subject) orelse return null;
            const at = try l.exprAs(args[0], .Int) orelse return null;
            if (subject_type == .String) {
                const piece = try l.exprAs(args[1], .String) orelse return null;
                const joined = try l.define(.{ .str_insert = .{ .arena = try l.arena(), .string = target, .index = at, .insert = piece } }, .String);
                try storeBack(l, subject, joined, .String);
            } else {
                const v = try l.exprAs(args[1], subject_type.Array.element.*) orelse return null;
                try l.effect(.{ .array_insert = .{ .array = target, .index = at, .value = v } });
            }
            return try l.nothing();
        },
        .remove => {
            const target = try expr(l, subject) orelse return null;
            const at = try l.exprAs(args[0], .Int) orelse return null;
            if (subject_type == .String) {
                // The removed byte, empty when `at` is out of range, as the
                // remaining string is then the whole one.
                const one = try l.constant(.{ .int = 1 }, .Int);
                const removed = try l.define(.{ .str_substring = .{ .arena = try l.arena(), .string = target, .start = at, .length = one } }, .String);
                const rest = try l.define(.{ .str_remove = .{ .arena = try l.arena(), .string = target, .index = at } }, .String);
                try storeBack(l, subject, rest, .String);
                return try result(l, e, removed, .String);
            }
            const element = subject_type.Array.element.*;
            const v = try l.define(.{ .array_remove = .{ .array = target, .index = at } }, element);
            return try result(l, e, v, element);
        },
        .slice => {
            const target = try expr(l, subject) orelse return null;
            const start = try l.exprAs(args[0], .Int) orelse return null;
            const length = try l.exprAs(args[1], .Int) orelse return null;
            if (subject_type == .String) {
                return try l.define(.{ .str_substring = .{ .arena = try l.arena(), .string = target, .start = start, .length = length } }, .String);
            }
            return try l.define(.{ .array_slice = .{ .arena = try l.arena(), .array = target, .start = start, .length = length } }, try l.g.typeOf(e));
        },
        .clear => {
            if (subject_type == .String) {
                try storeBack(l, subject, try l.constant(.{ .string = "" }, .String), .String);
            } else {
                const target = try expr(l, subject) orelse return null;
                try l.effect(.{ .array_clear = .{ .operand = target } });
            }
            return try l.nothing();
        },
        .find => {
            const haystack = try expr(l, subject) orelse return null;
            if (subject_type == .String) {
                const needle = try l.exprAs(args[0], .String) orelse return null;
                return try l.define(.{ .str_find = .{ .string = haystack, .needle = needle } }, .Int);
            }
            // An array of `nothing` holds no element.
            if (subject_type.Array.element.* == .Nothing) {
                if (try expr(l, args[0]) == null) return null;
                return try l.constant(.{ .int = -1 }, .Int);
            }
            const needle = try l.exprAs(args[0], subject_type.Array.element.*) orelse return null;
            return try l.define(.{ .array_find = .{ .array = haystack, .value = needle } }, .Int);
        },
        .exit => {
            const code = try l.exprAs(subject, .Int) orelse return null;
            try l.b.exit(code);
            return null;
        },
        .panic => {
            const message = try l.exprAs(subject, .String) orelse return null;
            try l.b.panic(message);
            return null;
        },
        .print => {
            const text = try l.exprAs(subject, .String) orelse return null;
            try l.effect(.{ .print = .{ .operand = text } });
            return try l.nothing();
        },
    }
}

/// What `@type` shows for a value of the analyzed type `info`.
fn typeName(info: *const ast.TypeInfo) []const u8 {
    if (info.custom_type) |custom| return custom.displayName();
    return switch (info.base) {
        .Int => "int",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Byte => "byte",
        .Nothing => "nothing",
        .Array => "array",
        .Union => "union",
        .Map => "map",
        .Function => "function",
        .Struct => "struct",
        .Enum => "enum",
        .Custom => "custom",
    };
}

// ── Input and output ──

fn peek(l: *Lowering, e: *ast.Expr, location: Location) Error!?ValueId {
    const ty = try l.g.typeOf(e);
    try l.g.markReflected(ty);
    const v = try expr(l, e) orelse return null;
    const shown_type = if (ty == .Array and ty.Array.size != null) try l.dynamicOf(ty) else ty;
    const shown = try l.convert(v, ty, shown_type);

    // A variant read through its enum (`Color.Red`) names no variable.
    const names_variant = e.data == .FieldAccess and
        if (l.g.resolutionOf(e.data.FieldAccess.object)) |resolved| resolved == .type else false;
    var display: ir.PeekDisplay = .{
        .path = if (names_variant) null else try peekPath(l, e),
        .location = location,
    };
    if (ty.isBoxed()) {
        const names = try memberNames(l, ty);
        display.members = names;
        if (ty == .Union) {
            if (try collapseWrittenGroups(l, e, ty, names)) |collapsed| {
                display.members = collapsed.names;
                display.member_slots = collapsed.slots;
            }
        }
    }
    try l.effect(.{ .peek = .{ .operand = shown, .display = display } });
    return v;
}

/// The path a peek shows for `e`: a name, or a chain of fields from one.
fn peekPath(l: *Lowering, e: *const ast.Expr) Error!?[]const u8 {
    return switch (e.data) {
        // A global carries its link name; a peek shows what was written.
        .Variable => |token| try l.g.alloc.dupe(u8, module_graph.displayName(token.lexeme)),
        .FieldAccess => |field| if (try peekPath(l, field.object)) |base|
            try std.fmt.allocPrint(l.g.alloc, "{s}.{s}", .{ base, field.field.lexeme })
        else
            try l.g.alloc.dupe(u8, field.field.lexeme),
        else => null,
    };
}

/// The names a peek lists for a box's members.
fn memberNames(l: *Lowering, boxed: HIRType) Error![]const []const u8 {
    if (boxed == .Group) {
        const members = l.g.semantic.group_table.members(boxed.Group) orelse &.{};
        const names = try l.g.alloc.alloc([]const u8, members.len);
        for (members, names) |member, *name| name.* = member.qualifier;
        return names;
    }
    const names = try l.g.alloc.alloc([]const u8, boxed.Union.members.len);
    for (boxed.Union.members, names) |member, *name| name.* = try displayName(l, member.*);
    return names;
}

/// How a type is shown to the user.
pub fn displayName(l: *Lowering, ty: HIRType) Error![]const u8 {
    const s = l.g.semantic;
    return switch (ty) {
        .Int => "int",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Byte => "byte",
        .Nothing => "nothing",
        .Array => |a| try std.fmt.allocPrint(l.g.alloc, "{s}[]", .{try displayName(l, a.element.*)}),
        .Map => "map",
        .Struct => |id| s.struct_table.displayName(id) orelse try std.fmt.allocPrint(l.g.alloc, "(struct#{})", .{id}),
        .Enum => |id| s.enum_table.displayName(id) orelse try std.fmt.allocPrint(l.g.alloc, "(enum#{})", .{id}),
        .Group => |id| s.group_table.displayName(id) orelse try std.fmt.allocPrint(l.g.alloc, "(group#{})", .{id}),
        .Function => "function",
        .Union => |u| blk: {
            var list: std.ArrayListUnmanaged(u8) = .empty;
            for (u.members, 0..) |m, i| {
                if (i > 0) try list.appendSlice(l.g.alloc, " | ");
                try list.appendSlice(l.g.alloc, try displayName(l, m.*));
            }
            break :blk list.items;
        },
        .Unknown => "unknown",
        .Poison => "poison",
    };
}

const CollapsedMembers = struct { names: []const []const u8, slots: []const u32 };

/// The display list of a union whose written type names groups: each member
/// flattened from a written group is shown as that group, once, at the place
/// of its first member. Null when the written type names no group.
fn collapseWrittenGroups(l: *Lowering, e: *ast.Expr, union_type: HIRType, member_names: []const []const u8) Error!?CollapsedMembers {
    const written = try l.g.typeInfoOf(e);
    if (written.base != .Union) return null;
    var groups: std.ArrayListUnmanaged(u32) = .empty;
    try collectWrittenGroups(l, written, &groups);
    if (groups.items.len == 0) return null;

    const members = union_type.Union.members;
    var names: std.ArrayListUnmanaged([]const u8) = .empty;
    const slots = try l.g.alloc.alloc(u32, members.len);
    var slot_groups: std.ArrayListUnmanaged(?u32) = .empty;
    for (members, member_names, slots) |member, member_name, *slot| {
        const group = writtenGroupOf(l, member.*, groups.items);
        const existing = if (group) |gid| for (slot_groups.items, 0..) |slot_group, idx| {
            if (slot_group == gid) break idx;
        } else null else null;
        if (existing) |idx| {
            slot.* = @intCast(idx);
            continue;
        }
        slot.* = @intCast(names.items.len);
        try names.append(l.g.alloc, if (group) |gid| l.g.semantic.group_table.displayName(gid).? else member_name);
        try slot_groups.append(l.g.alloc, group);
    }
    return .{ .names = names.items, .slots = slots };
}

fn collectWrittenGroups(l: *Lowering, written: *const ast.TypeInfo, groups: *std.ArrayListUnmanaged(u32)) Error!void {
    const ut = written.union_type orelse return;
    for (ut.types) |member| {
        if (member.base == .Union) {
            try collectWrittenGroups(l, member, groups);
            continue;
        }
        const custom = member.custom_type orelse continue;
        const gid = l.g.semantic.group_table.idOf(custom.resolved()) orelse continue;
        try groups.append(l.g.alloc, gid);
    }
}

/// The first of `groups` that flattened into the union member `member`.
fn writtenGroupOf(l: *Lowering, member: HIRType, groups: []const u32) ?u32 {
    for (groups) |gid| {
        for (l.g.semantic.group_table.members(gid) orelse &.{}) |group_member| {
            if (member.eql(generator.groupMemberType(group_member))) return gid;
        }
    }
    return null;
}
