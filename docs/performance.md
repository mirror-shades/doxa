# Performance

The performance contract of the Doxa execution model — the structural advantages the model gives the
compiler, how the current lowering spends them, what is measured, and the roadmap to cash the rest.

This is not a benchmark log. Doxa's performance story is the execution model's story: the two pillars
that define how a Doxa program may run are **static typing** and **region memory**. Everything below
follows from them.

---

## 1. The structural edge

A C compiler is barred from a whole class of optimizations because, in C, allocation, aliasing, and
representation are *user-observable*. Doxa makes all three *compiler-owned*. Concretely:

- **Every allocation is the compiler's.** There is no user `free`, no user `malloc`, no pointer
  arithmetic. A value is born in one place — its defining block's arena — and dies when that block's
  `}` runs. The compiler knows, at compile time, exactly which scope owns every value it allocates.

- **Reclamation is bulk and nested.** A scope is a single bump region; exiting it frees everything in
  O(1). Because blocks nest lexically, the whole allocation history of a running program is a LIFO
  stack of regions. There is no per-object teardown, no free ordering to respect, no fragmentation
  work in the hot path — only the bump pointer.

- **There are no pointers, so there is no aliasing problem.** Reads and writes reach a value only
  through its name, an index, or a field. Mutation is confined to `var` bindings, in-place field and
  element stores, and `^` alias parameters — and an alias is an *exclusive* borrow, enforced at
  compile time. The compiler never has to guess whether two accesses alias; where a C compiler
  reaches for `restrict` and hopes, Doxa already knows.

- **Types are static everywhere, including container elements and struct fields.** There is no
  `auto`, no `any`, no reflection-driven dispatch in the language. When the compiler emits an access
  into `arr[i]` it knows the element's type; when it emits a field read it knows the field's type and
  its offset. Representation is therefore a compiler decision, revisitable per type and per value.

- **Copy semantics are explicit, so copy boundaries are movable.** Doxa is a value language: a value
  that crosses a scope boundary is deep-copied into the destination arena; identity is preserved only
  when the source already lives in a scope that outlives the destination. Those rules are part of the
  language, not emergent from aliasing, so the compiler may satisfy them however it likes — by
  copying, or by *not needing to* when it can prove the source outlives the destination.

- **The whole program is one compilation.** Imports are resolved and inlined into a single IR; every
  function signature and every type is known before codegen begins. Nothing about the user program is
  opaque to the compiler at link time.

- **Safety is defined, not undefined.** Out-of-bounds access and integer overflow are *defined*
  behavior (a runtime trap, by default). That means the checks are ordinary code the compiler can
  reason about and remove — not a wall of UB that forbids every transformation.

### What C cannot do with any of this

| Advantage | Removes, versus C | Why C structurally can't |
| --- | --- | --- |
| Bulk, nested reclamation | Per-object `free`, allocator metadata, free-ordering constraints | `free` is observable; its timing and order are part of the program |
| Compiler-owned allocation | The malloc/free "region" | The compiler may not move, merge, or elide a user's allocation |
| No pointers / exclusive alias | Alias analysis, `restrict`, load-ordering barriers | Pointers are first-class and freely copied in C |
| Static element/field types | Tag dispatch, dynamic layout, per-element boxing | C does not own containers; their internals are user code |
| Explicit value/identity semantics | Copy elision heuristics | Whether a C object is copied is up to the optimizer, invisibly |
| Whole-program knowledge | Cross-TU/link-time blind spots | Separate compilation + dynamic linking are defaults |

That combination is what "beyond C" means for Doxa: **liveness and layout are analysis problems the
compiler is allowed to solve**, because nothing about where a value lives or how it is laid out is
observable to the program.

---

## 2. What the edge unlocks, in principle

Each of these is a transformation a C compiler cannot express because one of the above freedoms is
missing. None of them are speculative language features — they are direct consequences of the model.

- **Escape → stack.** A value that cannot outlive its defining scope needs no arena at all. If it
  cannot even outlive its function, it can live in a register or an `alloca` and be promoted by LLVM
  like any local. The compiler decides this per value, per use.

- **Region elision.** A scope that allocates nothing needs no arena. Today this is decided per whole
  function and only for pure-scalar bodies; nothing stops it being decided per scope, per value.

- **Copy-free returns.** A returned value must land in a scope that outlives the callee. Since the
  callee's arena is freed on return, today that means clone-on-return. But the destination region is
  known statically — the callee's region sits directly inside the caller's, so the destination is
  exactly one level up the nest — and the returned object can simply be *allocated there in the first
  place*, turning a deep copy into a placement choice. Cloning then remains only for genuinely
  dynamic escapes.

- **Type-directed storage.** Because container and struct internals are compiler-owned, an array of
  structs can be contiguous by-value storage, byte fields can occupy a byte, and a hot numeric struct
  can be promoted into SSA registers the way C's `struct` locals are. LLVM can then vectorize, CSE,
  and hoist on a plain object graph — but with the *guarantee* that no alias can invalidate a load.

- **Visible allocation.** If a value's arena is a real stack object (or its clones are real,
  type-specialized inline copies), LLVM sees loads and stores to memory it can analyze, rather than
  opaque external calls it must assume have arbitrary side effects.

- **Cheap safety.** Because bounds and overflow checks are defined behavior the compiler controls, it
  can emit them, prove them away, or turn them off per policy — the language never pays for UB.

---

## 3. What the current lowering spends (the realized floor)

The backend already spends the model's edge wherever it lowers to **plain typed SSA and flat
storage** and lets LLVM optimize it like C:

- Scalars (int, float, byte, tetra, enums) travel as `i64` / `double` / `i8` / `i2` in registers.
- Strings are two words — `(ptr, len)` — with no length header to chase.
- Fixed-size arrays of scalar types lower to flat contiguous buffers (`alloca` below a size
  threshold, arena otherwise) with GEP-based element access; no per-element indirection.
- Dynamic arrays carry an `%ArrayHeader`, but scalar element *reads* are inline GEP loads computed in
  the IR, which is what lets LLVM hoist and vectorize element loops.
- Scope elision removes the `doxa_scope_enter` / `doxa_scope_exit` round trip from functions whose
  values are all scalar, so a leaf call in a tight loop is a plain call, not two page-allocator
  transactions.
- Union values are the *only* runtime-tagged box; everything else is native.

This is the C-like floor. It is why the measured scalar, string, and integer workloads sit within a
few percent of their C twins (section 5): at that point the emitted IR is structurally what clang
would have produced, and LLVM's optimizer applies unchanged.

---

## 4. Where the edge is still on the table

The same workloads expose where the lowering is conservative — where it carries a uniform,
runtime-generic representation even though the static type is sitting right there in the HIR.

- **Structs are word boxes.** A struct instance is a heap block of one `i64` word per non-string
  field (two for a string — the `(ptr, len)` pair), addressed only through an anonymous `{ i64, i64,
  … }` index type and registered into a runtime descriptor registry so the *generic* runtime clone
  and print functions can walk its fields. A struct value is never loaded as an aggregate, so it
  cannot be promoted into registers. Consequences: a `byte` field burns a full word, and every struct
  construction pays a registry write.

- **Arrays of structs are arrays of box pointers.** Each element slot is an 8-byte reference to a
  separately arena-allocated, registered box, and each element *store* deep-clones the struct into
  the array's arena. Field access is therefore pointer-then-field — two dependent loads where C has
  one — and LLVM can never see a contiguous object graph to vectorize.

- **Element access still round-trips through tag dispatch.** Dynamic-array element *stores* (and
  compound assigns) and every non-scalar element *read* (string, array, struct) call opaque runtime
  accessors — `doxa_array_set_i64`, `doxa_array_get_i64` — that bounds-check and switch on the
  header's *runtime* `elem_tag`. The compiler knows the element type statically at every one of those
  call sites; the tag exists for the generic runtime, not for the language. Because the calls are
  external, LLVM can neither inline, CSE, nor hoist them.

- **Escape and rehoming are computed at runtime.** To answer "does this value already live in a scope
  that outlives the destination?" the emitted code walks the live scope stack (`isEqualOrDescendant`)
  and consults per-object scope registries (`string_scopes`, `struct_scopes`, `ArrayHeader.scope`).
  But the allocating scope of every value is a *compile-time* fact — its defining block — and
  lifetimes are lexical and nested. The runtime walk is a dynamic emulation of a static region
  calculation.

- **Allocation and clone boundaries are opaque.** Heap allocation is an external `doxa_scope_alloc`;
  clones are external recursive calls; structs are written into process-global registries. None of
  this is visible to LLVM, so its alias, DSE, and SROA passes stop at the call.

These five costs are the difference between the realized floor and the model's ceiling. They are not
language costs; each is a representation choice that a later pass can undo.

---

## 5. Measured state (September 2026)

Checkpoint from the benchmark suite (`test/benchmark/suite.doxa`). Every workload is compiled with
`doxa compile --opt=2` and its C twin with `zig cc -O2`, so the Doxa side optimizes its code at the
C baseline's exact opt level (LLVM `-O2` on both). Percentages are Doxa compute time relative to the
C twin; lower is better. All outputs are bit-identical to C (`match: true`).

| test   | doxa  | c      | % vs C  |
| ------ | ----- | ------ | ------- |
| fib    | 1.61s | 1.64s  | −1.73%  |
| sieve  | 0.92s | 0.91s  | +0.17%  |
| matrix | 1.18s | 1.15s  | +2.38%  |
| mb     | 0.93s | 0.93s  | −0.33%  |
| arr    | 1.02s | 1.03s  | −0.59%  |
| call   | 1.20s | 1.04s  | +15.37% |
| struct | 1.93s | 0.99s  | +95.38% |
| vec    | 0.90s | 0.93s  | −4.02%  |

Read against sections 3 and 4, this table is exactly the model's story:

- **`fib`, `sieve`, `matrix`, `mb`, `arr`, `vec`** are scalar and flat-array workloads. They live on
  the realized floor (section 3): typed SSA, flat fixed arrays, elided leaf scopes. They are within
  ~4% of C because at that point Doxa *is* emitting C-shaped IR.
- **`arr`** is the largest recovery on record: from +660% in the VM era to −0.59% today. Removing the
  VM replaced a boxed, tag-dispatching value pipeline with typed SSA — the same change section 3
  describes, applied program-wide.
- **`struct`** (~2x C) is section 4 verbatim: `var arr :: Vec4[N]` lowers to an array of box
  pointers, each field access is pointer-then-field through an opaque, tag-switching runtime call,
  and every element store deep-clones a registered struct. The C twin is a flat by-value
  `Vec4[250000]`. The model has nothing to do with this gap; the representation does.
- **`call`** (~15%) is the floored-`%` sign correction on a serial carry chain plus a less favorable
  unroll shape after LLVM inlines the leaf. Residual arithmetic-lowering detail, not a model cost.

These numbers are a snapshot of the *lowering*, not the language. The workloads that exercise
section 3's floor match C; the workloads that exercise section 4's conservatism are the ones that do
not.

---

## 6. Cashing the edge (roadmap)

Ordered by leverage, and roughly by dependency. Each step spends one of the section-1 freedoms; none
requires a language change.

### A. Static region / escape analysis on the HIR

Replace the runtime rehome machinery with a compile-time liveness calculation. Every value's home is
its defining scope; compute, per value, the region it can reach (does it return, store into an outer
variable, enter a container that escapes, cross an alias call?) and then:

- decide clone-vs-move at compile time, deleting `isEqualOrDescendant`, `scopeAt` level arithmetic,
  and the `string_scopes` / `struct_scopes` / `ArrayHeader.scope` registries;
- place function results into the caller's region instead of clone-on-return (the destination is one
  region up the nest — a fact, not a runtime query), turning deep copies into placement;
- demote non-escaping values out of arenas entirely.

**Invariants to preserve** (the escape rules already documented in `memory.md`): a global store
re-homes to the root arena and globals are identity — in-place mutation must write through the
root-owned object, never a disconnected snapshot; element stores into an array re-home into that
array's arena, which is what keeps `^`-aliased arrays safe. Values that can reach a global or an
aliased container stay on the conservative path. The exclusive-borrow rule on `^` parameters is a
static guarantee the analysis can lean on.

### B. Type-directed storage

Spend the static element and field types that the HIR already carries:

- arrays of structs become contiguous by-value element storage when elements do not need independent
  rehoming — the box-pointer representation is retained only where element identity genuinely
  escapes;
- structs whose type is never reflected (never `@string`ed, peeked, or generically cloned) get real
  typed layouts — packed bytes, true aggregates — instead of `[N x i64]` word boxes;
- emit *specialized per-type* deep copies (compile-time recursive: string fields clone, nested
  structs clone, raw fields memcpy) so cloning never needs the runtime descriptor registry.

Partitioning reflection out per type, rather than globally, is what makes the boxed word-array layout
a special case instead of the default.

### C. Make allocation and cloning visible to LLVM

Where a value's arena is provably local, lower allocation to a stack region (`alloca` or a
function-local bump buffer) and lower clones to typed inline copies — or mark the remaining runtime
helpers with the attributes that are true of them (`noalias`, `readonly`/`readnone`, `alwaysinline`)
so LLVM can CSE, hoist, and delete. An opaque external call is a wall; the same operation expressed
in IR is an optimization opportunity.

### D. Wire the dormant static switches

Several static decisions already exist in the HIR but are not yet honored by the backend:
`OverflowBehavior` (Trap/Saturate/Wrap) is defined but never selected; `Call.tail` is set but no
`tail`/`musttail` is ever emitted; `bounds_check` is carried but discarded; `@push` resize is
hardcoded to `Double`. Wiring these turns the model's "defined, cheap safety" into reality: trap
policies that cost nothing when the compiler can prove them away, real tail calls (which, combined
with copy-free returns from step A, need no post-call clone), and bounds removal where the index is
statically safe.

### E. Whole-program ABI polish

Every function and type is known before codegen; nothing prevents interprocedural use of that
knowledge — cross-function inlining of the specialized helpers from step B, `noalias` on every
by-value boundary (true by construction in this language), and constant propagation across module
imports.

Each step compounds the ones before it: region analysis (A) decides *where* values live, type-directed
storage (B) decides *how* they are laid out, visibility (C) hands both to LLVM, and the static
switches (D) stop the model's defined behavior from costing anything. The measured `struct` and `call`
gaps are the canaries for B/C and D respectively.

---

## Reproducing the measurements

```
doxa run test/benchmark/suite.doxa -- --runs 10
```

Each benchmark is compiled with `doxa compile … --opt=2` and its C twin with `zig cc -O2`.
`--opt=N` mirrors clang: `--opt=2` compiles the program's `.ll` to an object with `zig cc -O2` and
links an unchecked (`ReleaseFast`) runtime. `doxa compile … --emit-opt-ir` writes the
post-LLVM-optimization IR (`<stem>.opt.ll`) to the cache directory, which is the artifact the
`struct` and `call` analyses in section 5 are based on.
