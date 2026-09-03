# Compiler performance notes

Observations from the benchmark suite (`test/benchmark/suite.doxa`), captured September 2026
after the suite's workloads were restored to their original sizes. This is a snapshot of an
ongoing investigation, not a specification.

## Reproducing

```
doxa run test/benchmark/suite.doxa -- --runs 10
```

Each benchmark is compiled with `doxa compile … --opt=2` and its C twin with `zig cc -O2`.
`doxa compile … --emit-opt-ir` writes the post-LLVM-optimization IR (`<stem>.opt.ll`) to the
cache directory, which is what the analysis below is based on.

## Current state (runs = 10)

All tests reported `match: true` (bit-identical output to C). Runtime percentages are Doxa
compute time relative to the C baseline.

| test    | doxa  | c      | % vs C  |
| ------- | ----- | ------ | ------- |
| fib     | 1.61s | 1.64s  | -1.57%  |
| sieve   | 0.94s | 0.96s  | -1.75%  |
| matrix  | 1.16s | 1.13s  | +2.99%  |
| mb      | 0.91s | 0.92s  | -1.10%  |
| arr     | 1.00s | 0.98s  | +2.36%  |
| call    | 1.21s | 1.04s  | +15.77% |
| struct  | 1.87s | 0.95s  | +97.33% |
| vec     | 0.90s | 0.95s  | -5.78%  |

`struct` and `call` are the two largest gaps.

### Not regressions

`stats.csv` records earlier runs of the same workloads. On the most recent recorded run,
`struct` was 3.51s Doxa vs 0.93s C (+278%) and `call` was 1.16s vs 1.01s (+15%). The current
numbers are therefore the best recorded for both; `struct` improved ~2x. The remaining gap is
a persistent codegen characteristic, not a recent regression.

## `struct`: ~2x off C

Sources: `test/benchmark/d-src/struct.doxa`, `test/benchmark/c-src/struct.c`. The C version is
a flat, by-value `Vec4[250000]`; the timed loop reads four fields, applies five modulo-65536
operations, and writes four fields with a serial carry dependency.

Three properties of the generated code (see the hot loop `loop_body_46` in
`doxa_bench_struct.opt.ll`) account for the gap:

1. **Boxed element representation.** `var arr :: Vec4[N]` lowers to an array whose element
   slots are 8-byte references to separately arena-allocated (`doxa_scope_alloc`) and
   registered (`doxa_struct_register`) boxes, not a contiguous by-value array. Every field
   access is therefore pointer-then-field: two dependent loads per field where C has one.
2. **Accessors are opaque external calls.** Each field load/store emits
   `doxa_array_get_i64` / `doxa_array_set_i64`, runtime calls that bounds-check and switch on
   an element tag. Because they are not defined in the module, LLVM cannot CSE the repeated
   element-pointer fetches, so the hot loop performs 8 such calls per element (4 reads + 4
   writes) that each re-read the pointer from the array.
3. **Floored-`%` sign correction.** `a % b` lowers to `srem` plus an
   `icmp`/`select`/`add` correction sequence that forces the result into `[0, b)` for `b > 0`.
   LLVM eliminates the correction only when it can prove the dividend non-negative. In the
   `struct` loop the dividends come from memory, so the correction stays and adds latency to
   the already-serial carry chain. In `call` (dividends are induction-derived) LLVM drops it.

## `call`: ~16% off C

Sources: `test/benchmark/d-src/call.doxa`, `test/benchmark/c-src/call.c`. After `-O2` the
Doxa IR is structurally close to C: `leaf_add` is inlined and `leaf_sum` is unrolled (~4x).
The residual ~0.5 ns/iteration is best explained by the remaining floored-`%` sign-fix
latency on the serial `sum % 997` chain and a less favorable unroll shape. This ~15% gap has
been steady since the test was added.

## Directions worth exploring (no code changed)

- Emit the floored-mod correction only when it can be needed (divisor sign unknown / dividend
  not provably non-negative), and otherwise let `%` lower to a plain `srem` so LLVM can fold
  constant divisors and drop the correction.
- Consider by-value lowering for arrays of structs (contiguous element storage like C) or
  module-local, inlineable accessors (`noalias`/`readnone`) so LLVM can hoist/CSE the element
  pointer fetch and vectorize the loop.
