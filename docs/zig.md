`zig` blocks are embedded Zig modules.

Zig can be brought into a Doxa program two ways, both subject to the same rules below:

- **Inline block** — `zig <Name> { ... }` defines module `<Name>` directly in the `.doxa` source.
- **File import** — `module <Alias> from "<path>.zig"` imports an external `.zig` file as module `<Alias>`.

Both forms are validated by the same extractor and lower to the same code generation and ABI path, so `<Name>.func(...)` calls behave identically regardless of form.

## File imports

```doxa
module Math from "mathutil.zig"

const n is Math.double(21)
```

- The import alias (`Math`) names the module; calls use `Alias.func(...)`.
- The path resolves relative to the importing `.doxa` file, like any other `module ... from` import.
- The imported file must obey the restricted subset below; violations are reported as a compile error (`E7010`).
- Edits to the imported `.zig` are picked up automatically on the next build — the generated module is content-hashed (together with the toolchain, target, and optimization level) and rebuilt whenever any of those changes, so no manual cache clearing is needed.

## Current source-level rules

1. Top-level is restricted to:
- `const X = @import("...");`
- `fn ... { ... }`

2. Function signatures are restricted to Doxa-compatible scalar types:
- Doxa `int` <-> Zig `i64`
- Doxa `float` <-> Zig `f64`
- Doxa `byte` <-> Zig `u8`
- Doxa `tetra` <-> Zig `bool` (lossy at boundary)
- Doxa `nothing` <-> Zig `void`
- Doxa `string` <-> Zig `[]const u8`

3. The compiler validates these rules before invoking Zig.

## Inline Zig ABI

Each module compiles to an object file exporting one symbol per signature,
`<Module>.<fn>`, and the LLVM backend declares and calls those symbols directly
(`declare i64 @Math.double(i64)` / `call … @Math.double(...)`). The wrapper
object is linked into the program next to the runtime — nothing is loaded or
looked up at run time (no `dlopen`, no `GetProcAddress`), and `doxa run` and
`doxa compile` build the same objects and issue the same calls.

### Generated wrapper

For a Doxa-visible `fn f` in module `<Module>`, the generator emits:

```zig
pub fn __doxa_native__<Module>_f(a0: T0, …) callconv(.c) R { … }
comptime { @export(&__doxa_native__<Module>_f, .{ .name = "<Module>.f" }); }
```

The body is a thin adapter from the ABI types below to the user's Zig
signature: it re-slices string parameters and, for string returns, performs the
arena clone described under ownership. There is no argument vector, no tag
decoding, and no status code.

### Passing rules (fixed 64-bit lengths)

- Scalars pass as their Zig types: `int` → `i64`, `float` → `f64`,
  `byte` → `u8`, `tetra` → `bool`, `nothing` → `void`.
- A `string` parameter passes as `(ptr, u64 len)` — a pointer followed by a
  **fixed 64-bit** byte length — and the wrapper re-slices it to `[]const u8`.
- A `string` return writes through `out_ptr: *?[*]u8, out_len: *u64` — two
  out-params, not a `(ptr, len)` return pair. The empty string crosses as
  `(null, 0)` and allocates nothing.
- The runtime's exported string helpers (`doxa_str_*`, `doxa_*_to_string`, …)
  use the same fixed 64-bit `len: u64` / `*u64` for lengths and byte counts.

Lengths are 64-bit on every target so the emitted LLVM IR — which declares
`%DoxaString = { ptr, i64 }` and `i64` length parameters — needs no
target-specific rewriting. Pointers keep the target's pointer width; a pointer
that meets a 64-bit length slot is widened or narrowed with `@intCast` /
`@intFromPtr`.

### String rule

- Strings cross as pointer + byte length, never as C-strings.
- Embedded `\0` is valid and must be preserved.

### Ownership and lifetime

- String parameters are borrowed for the duration of the call.
- A returned string lands in the scope arena active at the call site: the
  wrapper clones the bytes with `doxa_str_clone_current`, so the value follows
  the ordinary arena rules in [memory.md](memory.md) — bulk-freed with its
  scope, and no free hook. Codegen tags the result with that call-site region
  rather than `Root`.
- Non-string returns cross by value.

### Error model

There is no runtime ABI error channel to translate into a Doxa error — the
shape of the call is settled at compile time. Parameter or return types that
cannot cross the boundary (arrays, structs, maps, enums, functions, unions)
fail the compile with `E8002` while the wrapper is generated, before Zig is
invoked.
