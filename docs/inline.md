
`zig` blocks are embedded Zig modules.

## Current rules (implemented today)

1) **Top-level is restricted**: only `const X = @import("...");` and `fn ... { ... }` are allowed (comments/blank lines ok).

2) **Signatures are restricted to Doxa-compatible atomic types**, expressed using Zig types:

- **Doxa `int`** → Zig `i64`
- **Doxa `float`** → Zig `f64`
- **Doxa `byte`** → Zig `u8`
- **Doxa `tetra`** → Zig `bool` (**lossy**)
  - We treat tetra as a *truthy/falsy* value at the boundary:
    - `false` → `false`
    - `true` → `true`
    - `both` → `true` (truthy corner)
    - `neither` → `false` (falsy corner)
  - This is intentional so each Doxa atomic type has a corresponding Zig type.
- **Doxa `nothing`** → Zig `void`
- **Doxa `string`** → Zig `[]const u8`

3) The compiler validates these rules before invoking the Zig toolchain.


```
zig ZigOps {
    fn sqrt_approx(x: f64) f64 {
        // Normal Zig body is the simplest path today.
        // (The doc previously showed `@llvm_external_call(...)`, which is not implemented.)
        return @sqrt(x);
    }
}

const zigSqrt is ZigOps.sqrt_approx(10.5)
```

## Notes / current implementation
- **Caching**: zig blocks are compiled into `out/zig/cache/` keyed by a hash of `(module name + source)`. Rebuilds reuse cached artifacts when possible.
- **Run mode (`doxa run`)**: zig blocks compile to a dynamic library and are called via the VM.
  - **Current VM bridge support is minimal**: `int`/`float`/`string` only, with **0 or 1 argument**.
  - `byte` / `tetra(bool)` / `nothing` are **not supported yet** in the VM↔Zig call bridge (calls will error as not implemented).
- **Compile mode (`doxa compile`)**: zig blocks compile to object files and are linked into the final executable.
  - Calls are emitted as external calls (currently declared as varargs in LLVM IR to avoid requiring full signature types at the IR level).
  - `string` is supported via a C-string ABI; `tetra` needs explicit lossy conversion (`i2` ↔ `bool`) to be safe.
- Note: Zig does **not** support `->` return syntax; use `fn name(args) ReturnType { ... }`.

## String support (important)

- **Signatures may mention `[]const u8`**.
- **Current ABI**: `string` crosses the boundary as a **C string**.
  - Wrapper function params use `?[*:0]const u8` and convert to `[]const u8` via `std.mem.span`.
  - Wrapper return values are allocated as `?[*:0]u8` and returned as C strings.
- **Run mode (`doxa run`)**: VM copies returned C strings into a Doxa `string` and frees the temporary.
- **Compile mode (`doxa compile`)**: returned C strings are treated like other runtime strings.
- **Limitation**: strings may not contain embedded `\\0` (C-string limitation).

## Wanted / next steps (partially implemented)

- **Tetra (`i2`) ↔ `bool` boundary conversion** (lossy):
  - VM: allow `tetra` arguments/returns by converting `i2` tetra to `bool` at the call boundary (and converting `bool` results back into tetra `true/false`).
  - LLVM codegen: for compile mode, emit the same lossy conversion when calling inline Zig exports.
- **Run-mode bridge**: extend VM calls to cover `byte` and `bool` (and more than 1 argument).
- **Strings**: optionally move to a real `(ptr,len)` ABI to support embedded `\\0` and avoid scanning for length.
- **Optional convenience**: add an `@llvm_external_call(...)` shim if we want that exact syntax in examples.

With more clarity on rules, molecular types could be provided as well. Structs would need a matching Zig counterpart, and arrays would need to be handled as slices, etc. This can come later when the implementation is mature.

