const std = @import("std");

/// Decides whether a function's scope arenas are provably unused, so the
/// `doxa_scope_enter` / `doxa_scope_exit` pair can be dropped from its body.
///
/// A scope arena only earns its keep when something inside the function is
/// allocated from it. `enter` pushes a page-allocator-backed `ScopeNode` and
/// `exit` tears it down, so a function that never allocates pays two
/// page-allocator round trips per call for an arena it never touches. That is
/// the dominant cost in scalar, call-heavy code (recursive `fib`, tight loops
/// around a leaf function).
///
/// The analysis is deliberately conservative: it only clears a function when
/// *every* value that function handles is a scalar living in a register or an
/// `alloca`. Any instruction that can put a value in the scope arena — or that
/// can receive one from a callee, which the callee clones into *our* arena on
/// return — keeps the scope alive.
pub fn Methods(comptime Ctx: type) type {
    const HIR = Ctx.HIR;
    const HIRInstruction = Ctx.HIRInstruction;
    const HIRValue = Ctx.HIRValue;

    return struct {
        /// Scalar types are held in registers or `alloca` slots and never touch
        /// the scope arena. Everything else (arrays, maps, strings, structs,
        /// groups, unions) is heap-backed and arena-owned.
        pub fn isScalarType(t: HIR.HIRType) bool {
            return switch (t) {
                .Int, .Byte, .Float, .Tetra, .Nothing, .Enum => true,
                .String, .Array, .Map, .Struct, .Group, .Function, .Union => false,
                // An unresolved type may turn out to be heap-backed; assume it is.
                .Unknown, .Poison => false,
            };
        }

        fn isScalarConst(value: HIRValue) bool {
            return switch (value) {
                .int, .byte, .float, .tetra, .nothing, .enum_variant => true,
                .string, .array, .struct_instance, .map, .group_instance, .union_instance => false,
                // An alias storage id is a plain u32 handle, not an arena value.
                .storage_id_ref => true,
            };
        }

        /// True when this instruction can neither allocate into the current
        /// scope arena nor bring an arena-owned value into this frame.
        fn instructionIsArenaFree(inst: HIRInstruction) bool {
            return switch (inst) {
                // Pure scalar computation and control flow.
                .Arith,
                .Compare,
                .LogicalOp,
                .Convert,
                .Dup,
                .Pop,
                .Swap,
                .Jump,
                .JumpCond,
                .Label,
                .Halt,
                .Unreachable,
                .GroupCheck,
                .NarrowVar,
                .RestoreVar,
                .StoreFieldName,
                => true,

                // Scope bookkeeping itself is what we are deciding about.
                .EnterScope, .ExitScope, .ResetScope => true,

                // Literals are arena-free only when the constant is a scalar;
                // a string or composite literal is materialised in the arena.
                .Const => |c| isScalarConst(c.value),

                // Variable traffic is arena-free while the value is scalar. A
                // heap store re-homes or clones into an arena.
                .StoreVar => |sv| isScalarType(sv.expected_type),
                .StoreDecl => |sd| isScalarType(sd.declared_type),
                .StoreAlias => |sa| isScalarType(sa.expected_type),
                .BindAlias => |ba| isScalarType(ba.target_type),

                // A load's type is not carried on the instruction, so these stay
                // conservative: they are safe because a *heap* variable can only
                // exist in this frame if some other instruction in the same
                // function created it, and that instruction fails the check.
                .LoadVar, .LoadAlias, .PushStorageId => true,

                // A callee clones a heap return value into our arena, so only a
                // scalar-returning call leaves the arena untouched.
                .Call => |c| isScalarType(c.return_type),
                .Return => |r| !r.has_value or isScalarType(r.return_type),

                // Everything below is heap-backed by construction.
                .ArrayNew,
                .ArrayGet,
                .ArraySet,
                .ArrayPush,
                .ArrayPop,
                .ArrayInsert,
                .ArrayRemove,
                .ArraySlice,
                .ArrayLen,
                .ArrayConcat,
                .ArrayCompoundAssign,
                .Map,
                .MapGet,
                .MapSet,
                .StructNew,
                .GetField,
                .SetField,
                .StringOp,
                .UnionConstruct,
                .GroupExtractPayload,
                .TypeCheck,
                .Peek,
                .PeekStruct,
                .AssertFail,
                .LoadModule,
                => false,
            };
        }

        /// True when the scope arenas of this function body can be elided.
        /// `body` is the instruction range for one function.
        pub fn functionScopeIsDead(
            param_types: []const HIR.HIRType,
            return_type: HIR.HIRType,
            body: []const HIRInstruction,
        ) bool {
            // A heap parameter arrives as a clone into this frame's arena, and a
            // heap return value is cloned out of it; either way the arena is live.
            for (param_types) |pt| {
                if (!isScalarType(pt)) return false;
            }
            if (!isScalarType(return_type)) return false;

            for (body) |inst| {
                if (!instructionIsArenaFree(inst)) return false;
            }
            return true;
        }
    };
}
