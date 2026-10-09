//! The register HIR (`plan/register-hir.md`). A function is a set of basic
//! blocks; every value is defined once — by a block parameter or by one
//! instruction — and has one type. Merges are block parameters: a jump states
//! the arguments its target receives. Nothing downstream reconstructs a
//! value's identity, type, or arena; it reads them here.
//!
//! Every slice in a `Function` is owned by the arena it was built in
//! (`FunctionBuilder.finish`), so a function is freed as a whole.

const std = @import("std");
const soxa_types = @import("../soxa_types.zig");
const soxa_instructions = @import("../soxa_instructions.zig");
const Reporting = @import("../../../utils/reporting.zig");

pub const HIRType = soxa_types.HIRType;
pub const StructId = soxa_types.StructId;
pub const EnumId = soxa_types.EnumId;
pub const ArithOp = soxa_instructions.ArithOp;
pub const CompareOp = soxa_instructions.CompareOp;
pub const Location = Reporting.Location;

pub const ValueId = enum(u32) { _ };
pub const BlockId = enum(u32) {
    entry = 0,
    _,
};
pub const SlotId = enum(u32) { _ };
pub const GlobalId = enum(u32) { _ };
pub const FunctionId = enum(u32) { _ };
pub const ZigFunctionId = enum(u32) { _ };

/// The type of a value. A Doxa type, or one of three types only the HIR has:
/// a branch condition, an arena, and the address of a slot.
pub const Type = union(enum) {
    doxa: HIRType,
    /// A two-valued branch condition (`i1`). Produced by comparisons and
    /// `tetra.holds`; never a Doxa type, never stored, never passed to a call.
    cond,
    /// A scope arena (`*Scope` at runtime).
    arena,
    /// The address of a slot holding a value of this Doxa type: a `^`
    /// argument or parameter.
    ref: HIRType,

    pub fn eql(a: Type, b: Type) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .doxa => |t| t.eql(b.doxa),
            .ref => |t| t.eql(b.ref),
            .cond, .arena => true,
        };
    }

    pub fn of(t: HIRType) Type {
        return .{ .doxa = t };
    }

    /// A value of this type owns heap storage in some arena, so where it lives
    /// is part of its meaning.
    pub fn isHeap(self: Type) bool {
        return switch (self) {
            .doxa => |t| isHeapType(t),
            .cond, .arena, .ref => false,
        };
    }
};

pub fn isHeapType(t: HIRType) bool {
    return switch (t) {
        .String, .Array, .Map, .Struct, .Union, .Group => true,
        .Int, .Byte, .Float, .Tetra, .Nothing, .Enum, .Function, .Unknown, .Poison => false,
    };
}

pub const Tetra = enum(u2) { false = 0, true = 1, both = 2, neither = 3 };

pub const Constant = union(enum) {
    int: i64,
    byte: u8,
    float: f64,
    tetra: Tetra,
    nothing,
    /// A variant of the enum the value's type names.
    enum_variant: u32,
    /// A string literal: static storage, outliving every arena.
    string: []const u8,
    /// A Doxa function, by its program index.
    function: FunctionId,
};

pub const TetraOp = enum { @"and", @"or", iff, xor, nand, nor, implies };
pub const CondOp = enum { @"and", @"or" };

/// One operand.
pub const Unary = struct { operand: ValueId };

/// A string and the arena a result derived from it is allocated in.
pub const ArenaString = struct { arena: ValueId, string: ValueId };

/// A value and the arena a copy or conversion of it is allocated in.
pub const ArenaOperand = struct { arena: ValueId, operand: ValueId };

/// An instruction. Operands are `ValueId`s; a payload field of type `ValueId`,
/// `?ValueId` or `[]const ValueId` is an operand and nothing else is, which is
/// what `forEachOperand` relies on.
pub const Op = union(enum) {
    constant: Constant,

    // ── Numbers ──
    arith: struct { op: ArithOp, lhs: ValueId, rhs: ValueId },
    neg: Unary,
    /// A numeric conversion to the result type (int, byte, float).
    convert: Unary,

    // ── Comparison and logic ──
    /// Yields `cond`.
    cmp: struct { op: CompareOp, lhs: ValueId, rhs: ValueId },
    tetra_binary: struct { op: TetraOp, lhs: ValueId, rhs: ValueId },
    tetra_not: Unary,
    /// A comparison used as a value: `true` or `false`.
    tetra_from_cond: Unary,
    /// A tetra used as a condition: `true` and `both` hold; `false` and
    /// `neither` do not (`docs/tetras.md`).
    tetra_holds: Unary,
    cond_not: Unary,
    cond_binary: struct { op: CondOp, lhs: ValueId, rhs: ValueId },

    // ── Boxes (unions and groups) ──
    /// Box a member as the result's union or group type.
    box: Unary,
    /// Read a box as the member the result type names; a check has proved it.
    unbox: Unary,
    /// Re-pack a box for another box type holding the same member.
    repack: Unary,
    /// Whether the box holds any of `members` (indexes into its type's
    /// members). Yields `cond`.
    member_test: struct { operand: ValueId, members: []const u32 },
    /// The index of the member a box holds, as an `int`: a `switch` subject.
    member_index: Unary,

    // ── Strings ──
    str_concat: struct { arena: ValueId, lhs: ValueId, rhs: ValueId },
    str_len: Unary,
    str_substring: struct { arena: ValueId, string: ValueId, start: ValueId, length: ValueId },
    /// The last character of a string, as a string.
    str_last: ArenaString,
    /// A string without its last character.
    str_drop_last: ArenaString,
    str_find: struct { string: ValueId, needle: ValueId },
    str_to_int: Unary,
    str_to_float: Unary,
    str_to_byte: Unary,
    str_pack: struct { arena: ValueId, bytes: ValueId },
    str_unpack: struct { arena: ValueId, string: ValueId },
    to_string: ArenaOperand,

    // ── Arrays ──
    // An element store places the element in the array's own arena (the
    // runtime re-homes it), so it needs no arena operand: the array may have
    // come from a caller whose arena this function cannot name.
    array_new: struct { arena: ValueId, length: ?ValueId },
    array_from_fixed: ArenaOperand,
    array_range: struct { arena: ValueId, start: ValueId, end: ValueId },
    array_get: struct { array: ValueId, index: ValueId },
    array_set: struct { array: ValueId, index: ValueId, value: ValueId },
    array_len: Unary,
    array_push: struct { array: ValueId, value: ValueId },
    array_pop: Unary,
    array_insert: struct { array: ValueId, index: ValueId, value: ValueId },
    array_remove: struct { array: ValueId, index: ValueId },
    array_clear: Unary,
    array_slice: struct { arena: ValueId, array: ValueId, start: ValueId, end: ValueId },
    array_concat: struct { arena: ValueId, lhs: ValueId, rhs: ValueId },
    array_find: struct { array: ValueId, value: ValueId },

    // ── Maps ── (entry stores re-home into the map's arena, as arrays do)
    map_new: struct { arena: ValueId, else_value: ?ValueId },
    map_get: struct { map: ValueId, key: ValueId },
    map_set: struct { map: ValueId, key: ValueId, value: ValueId },

    // ── Structs ── (field stores re-home into the struct's arena)
    struct_new: struct { arena: ValueId, fields: []const ValueId },
    field_get: struct { object: ValueId, index: u32 },
    field_set: struct { object: ValueId, index: u32, value: ValueId },

    // ── Slots and globals ──
    slot_addr: SlotId,
    load: Unary,
    store: struct { ref: ValueId, value: ValueId },
    global_load: GlobalId,
    global_store: struct { global: GlobalId, value: ValueId },

    // ── Arenas ──
    root_arena,
    /// Open a child of `parent`, which must be the innermost open arena.
    scope_enter: struct { parent: ValueId },
    /// Close the innermost open arena.
    scope_exit: struct { arena: ValueId },
    /// Close the innermost open arena and open a fresh one in its place,
    /// reusing its memory. The result is the new arena; nothing allocated in
    /// the old one is live after it.
    scope_reset: struct { arena: ValueId },
    /// A deep copy of `operand` in `arena`.
    clone: ArenaOperand,
    /// `operand` itself when it already lives in `arena` or an ancestor of
    /// it, else a deep copy there: the one runtime-checked copy, for an arena
    /// the verifier cannot name.
    rehome: ArenaOperand,

    // ── Calls ──
    call: struct { callee: FunctionId, args: []const ValueId },
    call_zig: struct { callee: ZigFunctionId, args: []const ValueId },

    // ── Input and output ──
    print: Unary,
    peek: struct { operand: ValueId, display: PeekDisplay },
    read_line: struct { arena: ValueId },
};

/// What a peek shows beside the value.
pub const PeekDisplay = struct {
    path: ?[]const u8,
    location: Location,
    /// A union's or group's members as written, with the marker slot each
    /// member maps to (`Peek.member_slots` today).
    members: ?[]const []const u8 = null,
    member_slots: ?[]const u32 = null,
};

pub const Inst = struct {
    op: Op,
    /// The value this instruction defines; null for one that defines none
    /// (a store, a print, a call returning `nothing`).
    result: ?ValueId,
};

pub const BlockCall = struct {
    block: BlockId,
    args: []const ValueId,
};

pub const SwitchCase = struct {
    value: i64,
    target: BlockCall,
};

pub const Terminator = union(enum) {
    jump: BlockCall,
    branch: struct { cond: ValueId, then: BlockCall, @"else": BlockCall },
    @"switch": struct { operand: ValueId, cases: []const SwitchCase, default: BlockCall },
    @"return": ?ValueId,
    @"unreachable",
    panic: ValueId,
    exit: ValueId,
    assert_fail: struct { message: ?ValueId, location: Location },

    /// Call `f` with each block this terminator can transfer to.
    pub fn forEachSuccessor(self: *const Terminator, ctx: anytype, comptime f: anytype) ReturnOf(f) {
        switch (self.*) {
            .jump => |call| try f(ctx, call),
            .branch => |b| {
                try f(ctx, b.then);
                try f(ctx, b.@"else");
            },
            .@"switch" => |s| {
                for (s.cases) |case| try f(ctx, case.target);
                try f(ctx, s.default);
            },
            .@"return", .@"unreachable", .panic, .exit, .assert_fail => {},
        }
    }
};

pub const Def = union(enum) {
    param: struct { block: BlockId, index: u32 },
    inst: struct { block: BlockId, index: u32 },
};

pub const ValueInfo = struct {
    ty: Type,
    def: Def,
    location: ?Location = null,
};

pub const Block = struct {
    params: []const ValueId,
    insts: []const Inst,
    term: Terminator,
};

/// Addressable storage: a local lent as `^`, or a narrowed receiver read
/// through its box. Its heap contents live in `arena`, which must outlive
/// every value stored in it.
pub const SlotInfo = struct {
    ty: HIRType,
    arena: ValueId,
    name: []const u8,
};

/// What a function parameter is, beyond its type.
pub const ParamRole = union(enum) {
    value,
    /// `@caller`: the caller's innermost open arena. The body arena is
    /// entered as its child, and a returned heap value lives in it.
    caller_arena,
    /// A `^` parameter's address; `arena` is the index of the parameter
    /// holding the arena that owns the slot.
    alias: struct { arena: u32 },
    /// The arena that owns a `^` parameter's slot.
    alias_arena,
};

pub const Function = struct {
    name: []const u8,
    /// The entry block's parameters are the function's; `roles[i]` says what
    /// parameter `i` is.
    roles: []const ParamRole,
    ret: HIRType,
    values: []const ValueInfo,
    blocks: []const Block,
    slots: []const SlotInfo,

    pub fn params(self: *const Function) []const ValueId {
        return self.blocks[0].params;
    }

    pub fn typeOf(self: *const Function, value: ValueId) Type {
        return self.values[@intFromEnum(value)].ty;
    }

    pub fn callerArena(self: *const Function) ?u32 {
        for (self.roles, 0..) |role, i| {
            if (role == .caller_arena) return @intCast(i);
        }
        return null;
    }
};

/// A callee's signature, as a call site sees it.
pub const Signature = struct {
    params: []const Type,
    roles: []const ParamRole,
    ret: HIRType,

    pub fn callerArena(self: Signature) ?u32 {
        for (self.roles, 0..) |role, i| {
            if (role == .caller_arena) return @intCast(i);
        }
        return null;
    }
};

pub const Global = struct {
    name: []const u8,
    ty: HIRType,
};

/// What the verifier and the printer need to know about the program around a
/// function: callee signatures, globals, and the shapes of named types.
pub const Program = struct {
    functions: []const Signature,
    function_names: []const []const u8,
    zig_functions: []const Signature,
    zig_function_names: []const []const u8,
    globals: []const Global,
    /// Field types by `StructId`.
    struct_fields: []const []const HIRType,
    /// Flattened members by `GroupId`, in the group's member order.
    group_members: []const []const HIRType,
    /// Display names by id, for the textual form; an id past the end prints
    /// as the id.
    struct_names: []const []const u8 = &.{},
    enum_names: []const []const u8 = &.{},
    group_names: []const []const u8 = &.{},

    /// The members of a union or group, in the order its box indexes them.
    pub fn boxMembers(self: *const Program, boxed: HIRType, buf: []HIRType) ?[]const HIRType {
        switch (boxed) {
            .Union => |u| {
                if (u.members.len > buf.len) return null;
                for (u.members, 0..) |member, i| buf[i] = member.*;
                return buf[0..u.members.len];
            },
            .Group => |gid| return if (gid < self.group_members.len) self.group_members[gid] else null,
            else => return null,
        }
    }
};

/// Call `f` with every operand of `op`, in field order.
pub fn forEachOperand(op: *const Op, ctx: anytype, comptime f: anytype) ReturnOf(f) {
    switch (op.*) {
        inline else => |payload| {
            const P = @TypeOf(payload);
            if (P == ValueId) return f(ctx, payload);
            if (@typeInfo(P) != .@"struct") return;
            inline for (std.meta.fields(P)) |field| {
                const value = @field(payload, field.name);
                switch (field.type) {
                    ValueId => try f(ctx, value),
                    ?ValueId => if (value) |v| try f(ctx, v),
                    []const ValueId => for (value) |v| try f(ctx, v),
                    else => {},
                }
            }
        },
    }
}

/// Call `f` with every operand of `term`, block arguments included.
pub fn forEachTermOperand(term: *const Terminator, ctx: anytype, comptime f: anytype) ReturnOf(f) {
    switch (term.*) {
        .jump => |call| for (call.args) |v| try f(ctx, v),
        .branch => |b| {
            try f(ctx, b.cond);
            for (b.then.args) |v| try f(ctx, v);
            for (b.@"else".args) |v| try f(ctx, v);
        },
        .@"switch" => |s| {
            try f(ctx, s.operand);
            for (s.cases) |case| for (case.target.args) |v| try f(ctx, v);
            for (s.default.args) |v| try f(ctx, v);
        },
        .@"return" => |v| if (v) |value| try f(ctx, value),
        .@"unreachable" => {},
        .panic, .exit => |v| try f(ctx, v),
        .assert_fail => |a| if (a.message) |m| try f(ctx, m),
    }
}

/// The return type of a visitor passed to the `forEach*` functions, which
/// return what it returns.
fn ReturnOf(comptime f: anytype) type {
    return @typeInfo(@TypeOf(f)).@"fn".return_type.?;
}
