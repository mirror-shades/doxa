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
  behavior, never UB. Signed `int` `add` / `sub` / `mul` trap on overflow in the checked modes
  (`debug`, `safe`) and wrap in the unchecked ones (`fast`, `small`) — the same `--opt-mode` axis
  that governs the runtime's own checks, so `--opt=2` produces exactly the wrapping arithmetic its
  C twin does. That means the checks are ordinary code the compiler can reason about and remove — a
  trap is dropped wherever the value-range analysis proves the result fits — not a wall of UB that
  forbids every transformation.

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
  construction pays a registry write. B2/B3-lite has landed a first cut for scalar-only structs: a
  struct whose fields are all scalar and that the whole-program predicate proves never needs the
  descriptor (not reflected, not in a container/union/signature) skips the registry write and is
  cloned with a typed word copy. Reflected and heap-field structs still box and register.

- **Arrays of structs are arrays of box pointers.** Each element slot is an 8-byte reference to a
  separately arena-allocated, registered box, and each element *store* deep-clones the struct into
  the array's arena. Field access is therefore pointer-then-field — two dependent loads where C has
  one — and LLVM can never see a contiguous object graph to vectorize. B1 has landed the fix for
  *fixed-size* arrays of *scalar-only* structs: they are now a flat `[{N} x { i64, … }]` buffer with
  single-GEP element/field access and a word-copy element store. Dynamic arrays, and structs with
  heap fields, still use boxes.

- **Element access still round-trips through tag dispatch.** Dynamic-array element *stores* (and
  compound assigns) and every non-scalar element *read* (string, array, struct) call opaque runtime
  accessors — `doxa_array_set_i64`, `doxa_array_get_i64` — that bounds-check and switch on the
  header's *runtime* `elem_tag`. The compiler knows the element type statically at every one of those
  call sites; the tag exists for the generic runtime, not for the language. Because the calls are
  external, LLVM can neither inline, CSE, nor hoist them.

- **Escape and rehoming are computed at runtime.** To answer "does this value already live in a
  scope that outlives the destination?" the emitted code walks the live scope stack (`isEqualOrDescendant`)
  and consults the per-object `struct_scopes` / `ArrayHeader.scope` registries. But the allocating
  scope of every value is a *compile-time* fact — its defining block — and lifetimes are lexical and
  nested. The runtime walk is a dynamic emulation of a static region calculation. A2 has already
  retired strings from this path: immutable, their store is decided entirely at compile time (an
  unknown string is simply cloned, which is observably identical to preserving its identity), so
  `string_scopes` and the string rehome exports are gone. A3 has started retiring the clone-on-return:
  a struct or array literal that is the top-level value of a `return` is born in the caller's arena at
  construction (`doxa_scope_alloc_at` / `doxa_array_new_at`) instead of being deep-copied there, with
  only the non-direct returns still cloning.

- **Allocation and clone boundaries are opaque.** Heap allocation is an external `doxa_scope_alloc`;
  clones are external recursive calls; structs are written into process-global registries. None of
  this is visible to LLVM, so its alias, DSE, and SROA passes stop at the call.

These five costs are the difference between the realized floor and the model's ceiling. They are not
language costs; each is a representation choice that a later pass can undo.

---

## 5. Measured state (September 2026)

Checkpoint from the benchmark suite (`test/benchmark/suite.doxa`), taken as six
same-build runs on 2026-09-27 (the window `date >= 1790530050` in
`test/benchmark/stats.csv`). Every workload is compiled with `doxa compile
--opt=2` and its C twin with `zig cc -O2`, so the Doxa side optimizes its code at the
C baseline's exact opt level (LLVM `-O2` on both). Percentages are Doxa compute time relative to the
C twin — the mean of the six runs, with the range they spanned; lower is better. All outputs are
bit-identical to C (`match: true`, every run). The seconds columns are gone: absolute
times in this campaign moved by up to ~46% as background load came and went, so only
the ratio is quotable; the raw seconds are in `stats.csv`.

| test   | % vs C (mean of 6) | same-build range |
| ------ | ------------------ | ---------------- |
| fib    | −10.98%            | −16.09 … −2.91   |
| sieve  | +2.78%             | −1.04 … +5.11    |
| matrix | +2.19%             | −0.40 … +4.02    |
| mb     | +0.23%             | −0.59 … +0.79    |
| arr    | +13.74%            | +7.27 … +16.96   |
| call   | +14.10%            | +12.16 … +15.44  |
| struct | −5.00%             | −8.54 … −0.31    |
| vec    | +4.18%             | +0.78 … +9.19    |

Read against sections 3 and 4, this table is exactly the model's story:

- **`fib`, `sieve`, `matrix`, `mb`, `arr`, `vec`** are scalar and flat-array workloads. They live on
  the realized floor (section 3): typed SSA, flat fixed arrays, elided leaf scopes. Their placement
  near C is what the floor looks like.
- **`arr`** is the largest recovery on record: from +660% in the VM era to the mid-teens of C today
  (+13.74% mean, +7.27% best in this campaign). Its *level* is the one scalar row still open: the
  2026-09-03 session averaged +0.6% (rows −0.59, +1.84) and the campaign centers on +13.74 — a
  13-point move, past this row's own 9.69 same-build spread and the only comparable row that crosses
  that bar — while both twins stayed frozen in between (C 1.029 → 0.966s, Doxa 1.022 → 1.155s). That
  is a cross-session shift, not the same-build noise the note below accounts for; attribution
  was resolved as the target confound (see §6), not a codegen shift.
  Removing the VM replaced a boxed, tag-dispatching value pipeline with typed SSA — the same change
  section 3 describes, applied program-wide.
- **`struct`** was the last workload still paying a section-4 cost, and no longer does. Its object
  graph was already contiguous (section 4, first two bullets), so what remained was arithmetic: Doxa's
  `%` is *floored* (section 1) where C's truncates, and the sign correction that difference requires
  was five extra instructions per operation on a five-link serial carry chain. A constant divisor
  whose magnitude divides 2^64 — 65536 is a power of two — needs no correction at all, because an
  unsigned remainder already yields the residue. Each `%` is now a single `and`. The dividend is a
  load out of the array, so no amount of range analysis on the source values would have reached it;
  the identity came from the divisor alone.

  The C twin needed the same correction: it wrote `%`, whose operands are loads clang cannot prove
  non-negative, so every modulo in its loop kept a sign correction the data never needed. It now
  writes `& (MOD - 1)` — the same mod-2^16 fact the values satisfy — and compiles to the same shape
  Doxa does. `struct` therefore sits at **parity**: below C in all six runs of this campaign
  (mean −5.00%, worst −0.31%). Its timed region
  was also scaled ×5 (`STEPS` 550 → 2750, 2026-09-27) so the row lands at the ~1s norm the other
  workloads run at — `struct` rows dated before that change did a fifth of the work and are not
  comparable.
  Earlier readings of −81% measured floored-vs-truncating `%` semantics, not codegen; the two
  affected rows were removed from `stats.csv`.
- **`call`** was the last workload on the floored-`%` correction — the same serial carry chain as
  `struct`, with a modulus (997) that does not divide 2^64 and a dividend (`sum`) that is a
  loop-carried accumulator. In this 2026-09-27 campaign it sat at +14.10% mean (+12.16 … +15.44).
  The loop-carried / interprocedural range analysis (§6-D) now discharges the correction: it proves
  `sum >= 0` across the loop *and* the opaque `leaf_add(i, …)` call, and the emitted IR shows a single
  `urem` with no sign fixup. The row's residual **+7.06%** (after the 2026-09-28 target fix; the
  first −18.76% reading was the AVX2/SSE2 confound) turned out to be a *second* benchmark confound,
  not codegen: C's clang IR pins `"tune-cpu"="generic"` while Doxa's `.ll` carried no target
  attributes, so the `.ll` path inherited the native tune model, whose unroller sized this serial loop
  8× and spilled where generic sizes it 2×. The emitter now pins `"tune-cpu"="generic"` on every
  function (§6); `call` measures **−17.9%** vs C, so D-1b's `urem` win is finally visible. The same
  correction changes every other row too — most dramatically `arr`, which was +13.55% under the
  original confound and is **−1.29%** at target parity. The table above is left as the 2026-09-27
  snapshot; §6 records both fixes and the corrected figures.

**On reading this table.** The percentages are means over six same-build runs, and the range column
*is* the measurement's noise floor: same binary, same source, nothing recompiled between runs — yet
`fib` moved 13.18 points, `arr` 9.69, `vec` 8.41, `struct` 8.22, `sieve` 6.15, `matrix` 4.42. Only
`mb` (1.38) and `call` (3.27) sit near the ±2% that a single-run comparison would need, so one run
cannot support a claim finer than its row's range; compare campaigns, not runs. The mechanism is
visible in the log: background load slowed whole invocations by up to ~46% and slowed the Doxa and C
twins unequally, so the ratio moved without anything being recompiled. The 8-point `arr` swing
between provably identical builds that earlier notes could not explain falls inside this envelope —
it was the floor. None of `fib`, `sieve`, `matrix`, `mb`, `arr`, `vec` contains a floored division or
modulo *in its timed region* — `arr` times a plain `sum += arr[i]` reduction and `vec` a float
`y[i] += 1.5 * x[i]`, with their only `%` in untimed setup — so movement in those rows is never
lowering. `doxa run scripts/noise.doxa -- --since <epoch>` prints the distribution for any window of
the log; a delta below the workload's row is noise, and the canaries are decidable because their
gaps exceed theirs.

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
  and the `struct_scopes` / `ArrayHeader.scope` registries (A2 already deleted `string_scopes` for
  strings, which are clone-decided statically);
- place function results into the caller's region instead of clone-on-return (the destination is one
  region up the nest — a fact, not a runtime query), turning deep copies into placement — partially
  landed (A3): direct-return struct/array literals are now constructed in the caller's arena;
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
  escapes. Partially landed (B1): fixed arrays of scalar-only structs are flat; dynamic arrays and
  heap-field structs are still boxed;
- structs whose type is never reflected (never `@string`ed, peeked, or generically cloned) get real
  typed layouts — packed bytes, true aggregates — instead of `[N x i64]` word boxes. Partially landed
  (B2/B3-lite): a scalar-only struct proven never to need the descriptor skips its registry write and
  clones with a typed word copy (the word-box layout itself is unchanged);
- emit *specialized per-type* deep copies (compile-time recursive: string fields clone, nested
  structs clone, raw fields memcpy) so cloning never needs the runtime descriptor registry.
  Partially landed (B3-lite) for scalar-only structs.

Partitioning reflection out per type, rather than globally, is what makes the boxed word-array layout
a special case instead of the default.

### C. Make allocation and cloning visible to LLVM

Where a value's arena is provably local, lower allocation to a stack region (`alloca` or a
function-local bump buffer) and lower clones to typed inline copies — or mark the remaining runtime
helpers with the attributes that are true of them (`noalias`, `readonly`/`readnone`, `alwaysinline`)
so LLVM can CSE, hoist, and delete. An opaque external call is a wall; the same operation expressed
in IR is an optimization opportunity.

### D. Wire the dormant static switches

Several static decisions already exist in the HIR; some are honored, some not. Integer-overflow
behavior is now selected from the safety axis — D-1 landed: the checked modes trap with
`llvm.s*add.with.overflow` plus a real `llvm.trap`, the unchecked modes wrap, and a trap is skipped
whenever the value-range lattice proves the result fits. Still dormant: `Call.tail` is set but no
`tail`/`musttail` is ever emitted; `bounds_check` is carried but discarded; `@push` resize is
hardcoded to `Double`. Wiring the rest turns the model's "defined, cheap safety" into reality: real
tail calls (which, combined with copy-free returns from step A, need no post-call clone) and bounds
removal where the index is statically safe.

**Landed: the arithmetic lowerings these switches ride on.** Doxa's `//` and `%` are floored, so they
cannot lower to LLVM's truncating `sdiv`/`srem` without a sign correction, and that correction was
five extra instructions per operation sitting on the critical path of any serial carry chain — the
last remaining reason `struct` ran at twice C. Two static facts now remove it, and a value-range
lattice (`int_range.zig`, threaded like the region analysis in step A) supplies them: a constant
divisor whose magnitude divides 2^64 needs no correction at all, and a provably non-negative dividend
makes truncation equal flooring. `struct`'s `% 65536` is now one `and`, and with the C twin stating
the same mod-2^16 fact (`& (MOD - 1)`) the workload measures at parity with C.

**Landed: the lattice now crosses a loop and a call.** `call`'s dividend is a loop-carried
accumulator updated by an opaque `leaf_add(i, …)` call, so its sign needed two facts the original
single linear walk could not produce. `range_flow.zig` adds an abstract stack-machine interpreter
with a widened fixpoint over each loop body and return-range summaries for straight-line callees; it
proves `sum >= 0` and the `% 997` collapses to a single `urem`. The optimization is real (the C twin's
truncating `%` keeps the correction clang cannot discharge). Its first measured payoff read −18.76%
under the pre-fix confounded builds, then **+7.06%** at target parity (`-mcpu=native`, §6) — but that
residual was a second confound, not codegen: C's clang IR pins `"tune-cpu"="generic"`, Doxa's `.ll`
carried no target attributes, and the resulting native tune made the loop unroller size this serial
carry chain 8× with spills instead of 2×. The emitter now emits
`attributes #0 = { "tune-cpu"="generic" }` and every `define` references it, matching clang's
frontend convention; `call` measures **−17.9%** vs C. The analysis itself is partial and falls back
to the old behavior for anything it does not model, so it can only ever be additive.

The same range lattice is what the remaining arithmetic switches consume: D-1 skips an overflow trap
whenever the operand bounds prove the result fits, and D-3 wants the same upper bound to prove an
index is in range before dropping a check. D-3 is the one that still needs the lattice to reach
*memory* (an index is a value that has usually been stored and reloaded).

### E. Whole-program ABI polish

Every function and type is known before codegen; nothing prevents interprocedural use of that
knowledge — cross-function inlining of the specialized helpers from step B, `noalias` on every
by-value boundary (true by construction in this language), and constant propagation across module
imports.

Each step compounds the ones before it: region analysis (A) decides *where* values live, type-directed
storage (B) decides *how* they are laid out, visibility (C) hands both to LLVM, and the static
switches (D) stop the model's defined behavior from costing anything. The `struct` gap — the B/C
canary — is closed at parity with its corrected C twin, and `call` (the D canary) is now below C
after the `tune-cpu` fix.

---

## Reproducing the measurements

```
doxa run test/benchmark/suite.doxa -- --runs 10 --write
```

`--write` appends the run to `test/benchmark/stats.csv`, the log every table here is drawn from;
without it the suite only prints.

Before trusting a delta, size the noise: filter the log to one same-build campaign (a `--since`
unix epoch) and print each workload's distribution — mean, range, and largest step between
consecutive runs.

```
doxa run scripts/noise.doxa -- --since <epoch>
```

Section 5's table is the campaign at `--since 1790530050`. A delta below a workload's range is
noise; when a gate decision is close, take a fresh pair across sessions as well — this floor is
within-session.

Each benchmark is compiled with `doxa compile … --opt=2` and its C twin with `zig cc -O2
-mcpu=native -ffp-contract=off`. `--opt=N` mirrors clang: `--opt=2` compiles the program's `.ll` to
an object with `zig cc -O2` and links an unchecked (`ReleaseFast`) runtime. Both sides target the
host CPU: a native `doxa compile` adds `-mcpu=native` itself, because its required explicit `-target`
otherwise makes `zig cc` fall back to baseline x86-64 (SSE2). Without that, Doxa would be built for
SSE2 while its C twin gets AVX2 — the confound that, until 2026-09-28, biased every ratio in this
document. `doxa compile … --emit-opt-ir` writes the post-LLVM-optimization IR (`<stem>.opt.ll`) to the
cache directory, which is the artifact the `struct` and `call` analyses in section 5 are based on.

To see where compile time goes, `doxa compile … --profile` prints a per-phase span tree
(`--profile-out=<path>` also writes it as JSON). The runtime object and the user `.ll` object are
content-cached per (toolchain, target, optimizer level, source closure), so warm recompiles skip
`zig cc` / `zig build-obj`; see `plan/artifact-cache.md`. When measuring compile time, note whether
the cache is warm or cold.
