const std = @import("std");

/// Decides whether a function's scope arenas are provably unused, so the
/// `doxa_scope_enter` / `doxa_scope_exit` pair can be dropped from its body.
///
/// A scope arena only earns its keep when something inside the function is
/// allocated from it. `enter` takes a spare `ScopeNode` and `exit` rewinds it,
/// so a function that never allocates still pays two opaque runtime calls per
/// call for an arena it never touches, and those calls keep LLVM from inlining
/// it. That is the dominant cost in scalar, call-heavy code (recursive `fib`,
/// tight loops around a leaf function, predicates over a board).
///
/// The question is what the function *allocates*, not what it *handles*. A
/// function may read heap values it was handed — a read-only parameter is bound
/// without a copy (`heap_copy == .keep`), and loading a field, an element, or a
/// length allocates nothing — as long as no instruction can put a value in the
/// scope arena, or receive one from a callee, which the callee clones into
/// *our* arena on return.
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

        /// The function being judged, and the callees it may name.
        const Frame = struct {
            functions: []const HIR.HIRProgram.HIRFunction,
            /// Which functions' scopes are still believed dead (see `deadScopes`).
            dead: []const bool,
            /// Every parameter is a scalar, so no heap value exists in this
            /// frame unless an instruction that fails the check created it.
            all_scalar: bool,
        };

        /// A by-value argument of this parameter type is handed over as the
        /// caller's pointer or slice; the callee copies it into its own arena
        /// if it mutates it. Arrays are excluded because a fixed-size array
        /// argument is boxed into a fresh `ArrayHeader` at the call site, and
        /// groups and unions because the argument is boxed into a `%DoxaValue`.
        fn passesWithoutMarshalling(t: HIR.HIRType) bool {
            return isScalarType(t) or t == .String or t == .Struct;
        }

        /// True when the call can neither allocate into the caller's scope
        /// arena while marshalling arguments nor return a heap value.
        fn callIsArenaFree(c: std.meta.fieldInfo(HIRInstruction, .Call).type, frame: Frame) bool {
            if (!isScalarType(c.return_type)) return false;
            // A builtin or inline-Zig callee records no parameter types here,
            // so it is trusted only in a frame that holds no heap value to pass.
            const index = c.function_index orelse return frame.all_scalar;
            const callee = frame.functions[index];
            for (callee.param_types, 0..) |pt, i| {
                if (isScalarType(pt)) continue;
                // A `^` argument lends the callee our storage. A callee that
                // re-homes a heap value into it finds the owning arena by
                // counting scopes up from its own, which only holds when ours
                // exists; a callee whose own scope is dead re-homes nothing.
                const is_alias = i < callee.param_is_alias.len and callee.param_is_alias[i];
                if (is_alias) {
                    if (!frame.dead[index]) return false;
                } else if (!passesWithoutMarshalling(pt)) return false;
            }
            return true;
        }

        /// True when this instruction can neither allocate into the current
        /// scope arena nor bring an arena-owned value into this frame.
        fn instructionIsArenaFree(inst: HIRInstruction, frame: Frame) bool {
            return switch (inst) {
                // Pure computation and control flow.
                .Arith,
                .LogicalOp,
                .Dup,
                .Pop,
                .Swap,
                .Jump,
                .JumpCond,
                .Label,
                .Halt,
                .Unreachable,
                .MemberCheck,
                .StoreFieldName,
                => true,

                // Comparing strings or structs reads both operands in place.
                .Compare => true,

                // Converting to a string materialises it in the arena.
                .Convert => |cv| isScalarType(cv.to_type),

                // Reads of a heap value the frame already holds. Each yields the
                // stored scalar, or a pointer or slice into the existing object.
                .GetField, .ArrayGet, .ArrayLen => true,

                // Scope bookkeeping itself is what we are deciding about.
                .EnterScope, .ExitScope, .ResetScope => true,

                // Literals are arena-free only when the constant is a scalar;
                // a string or composite literal is materialised in the arena.
                .Const => |c| isScalarConst(c.value),

                // A store is arena-free while the value is scalar, or when it
                // binds the existing object (a read-only parameter). Re-homing
                // or snapshotting a heap value copies it into an arena.
                .StoreVar => |sv| isScalarType(sv.expected_type) or sv.heap_copy == .keep,
                .StoreDecl => |sd| isScalarType(sd.declared_type),
                .StoreAlias => |sa| isScalarType(sa.expected_type),
                // Binding a `^` parameter names the caller's storage; it
                // allocates nothing whatever it points at.
                .BindAlias => true,

                // Loads allocate nothing; whatever they read was put in the
                // frame by an instruction or parameter judged on its own.
                .LoadVar, .LoadAlias, .PushStorageId => true,

                .Call => |c| callIsArenaFree(c, frame),
                // A heap return value is cloned into the caller's arena, found
                // by counting scopes up from ours.
                .Return => |r| !r.has_value or isScalarType(r.return_type),

                // Everything below allocates or re-homes into the arena.
                .ArrayNew,
                .ArraySet,
                .ArrayPush,
                .ArrayPop,
                .ArrayInsert,
                .ArrayRemove,
                .ArraySlice,
                .ArrayConcat,
                .ArrayCompoundAssign,
                .Map,
                .MapGet,
                .MapSet,
                .StructNew,
                .SetField,
                .StringOp,
                .Box,
                .Unbox,
                .UnboxPayload,
                .TypeCheck,
                .Peek,
                .PeekStruct,
                .AssertFail,
                => false,
            };
        }

        /// Which functions' scope arenas can be elided, by index into
        /// `hir.function_table`; `bodies` holds each function's instruction
        /// range. Whether a function is dead can depend on its callees (a `^`
        /// argument is safe only for a callee that is itself dead), so this is
        /// the greatest fixpoint: every function starts dead and loses it when
        /// an instruction fails, until nothing changes. A recursive group with
        /// no allocation therefore stays dead.
        pub fn deadScopes(
            allocator: std.mem.Allocator,
            hir: *const HIR.HIRProgram,
            bodies: []const []const HIRInstruction,
        ) ![]bool {
            const functions = hir.function_table;
            const dead = try allocator.alloc(bool, functions.len);
            for (functions, dead) |func, *d| d.* = isScalarType(func.return_type);
            var changed = true;
            while (changed) {
                changed = false;
                for (functions, bodies, dead) |func, body, *d| {
                    if (!d.*) continue;
                    if (!bodyIsArenaFree(func, body, functions, dead)) {
                        d.* = false;
                        changed = true;
                    }
                }
            }
            return dead;
        }

        /// A heap parameter is bound by a prologue `StoreVar` in `body`, so
        /// whether it is copied on entry is judged with the rest of the body.
        /// A heap return value is cloned out of the frame into the caller's,
        /// found by counting scopes from ours; `deadScopes` rules it out.
        fn bodyIsArenaFree(
            func: HIR.HIRProgram.HIRFunction,
            body: []const HIRInstruction,
            functions: []const HIR.HIRProgram.HIRFunction,
            dead: []const bool,
        ) bool {
            var all_scalar = true;
            for (func.param_types) |pt| {
                if (!isScalarType(pt)) all_scalar = false;
            }
            const frame: Frame = .{ .functions = functions, .dead = dead, .all_scalar = all_scalar };
            for (body) |inst| {
                if (!instructionIsArenaFree(inst, frame)) return false;
            }
            return true;
        }
    };
}
