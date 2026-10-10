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

2. Function signatures are restricted to Doxa-compatible types:
- Doxa `int` <-> Zig `i64`
- Doxa `float` <-> Zig `f64`
- Doxa `byte` <-> Zig `u8`
- Doxa `tetra` <-> Zig `bool` (lossy at boundary)
- Doxa `nothing` <-> Zig `void`
- Doxa `string` <-> Zig `[]const u8`
- Doxa `int[]` <-> Zig `[]const i64`
- Doxa `float[]` <-> Zig `[]const f64`
- Doxa `byte[]` <-> Zig `[]const DoxaByte` (`DoxaByte` is an injected alias for `u8`)
- Doxa `string[]` <-> Zig `[]const []const u8`
- Doxa `T[][]` <-> additional `[]const` levels (nesting is unlimited)
- Doxa `enum` <-> Zig `DoxaEnum_<name>` (an injected alias for `i64`; the value is the variant's discriminant)
- Doxa `enum[]` <-> Zig `[]const DoxaEnum_<name>`

Arrays of scalars, strings, and enums cross in both directions at any depth; the
`[]const` element slices a Zig function receives are borrowed for the call. A
bare `[]const u8` is always a `string`, so byte arrays use the `DoxaByte`
marker. A Doxa enum crosses as its `i64` variant discriminant, spelled
`DoxaEnum_<path>` where `<path>` names the enum through the bindings of the file
declaring the `zig` block — `DoxaEnum_Color` for its own `enum Color`. A
qualified path is spelled as a Zig quoted identifier: after `import error from
@std()`, `std`'s `Method` enum is `@"DoxaEnum_error.Method"`. The wrapper
declares every spelling it finds as `i64`, so the Zig author compares against
integers. A `.zig` file has no Doxa bindings, so naming a `DoxaEnum_` there is a
located error. See
[Array parameters and returns](#array-parameters-and-returns) for the ownership
rules.

3. The compiler validates these rules before invoking Zig.

## Inline Zig ABI

Each module compiles to an object file exporting one symbol per signature, named
by the function's mangled link name, and the LLVM backend declares and calls
those symbols directly. A `zig` block is a module of its own, owned by the file
that declares it, so two files may each declare `zig Math { pub fn double … }`:
their functions are distinct symbols. The wrapper
object is linked into the program next to the runtime — nothing is loaded or
looked up at run time (no `dlopen`, no `GetProcAddress`), and `doxa run` and
`doxa compile` build the same objects and issue the same calls.

### Generated wrapper

For a Doxa-visible `fn f`, the generator emits:

```zig
pub fn __doxa_native__f(a0: T0, …) callconv(.c) R { … }
// or, when f returns a string or an array (see "The caller's arena"):
pub fn __doxa_native__f(__doxa_scope: *Scope, a0: T0, …) callconv(.c) R { … }
comptime { @export(&__doxa_native__f, .{ .name = "<link name of f>" }); }
```

A link name has the form `__doxa_m1_<tag>__f<len>$<name>`: `<tag>` is a hash of
the declaring module's stable key, and every component is length-prefixed, so
no two declarations in a program share one. Diagnostics, `peek`, and
reflection show the declared name, never the link name.

The body is a thin adapter from the ABI types below to the user's Zig
signature: it re-slices string parameters and, for string returns, performs the
arena clone described under ownership. There is no argument vector, no tag
decoding, and no status code.

### The caller's arena

A function whose result is a heap value — a `string` or an array, including
the payload of a fallible return — builds it in the caller's arena, which the
caller passes as a hidden first argument, `__doxa_scope: *Scope`, before the
declared parameters. Every other function takes no hidden argument. The rule
is one predicate (`takesArena` in `src/inline_zig/compiler.zig`) that the
wrapper generator and the LLVM backend both read. The arena is the call
site's innermost open scope, the same arena a Doxa function's result would
live in; the wrapper never names a scope the caller did not hand it.

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
- A returned string lands in the caller's arena: the wrapper clones the bytes
  into `__doxa_scope` with `doxa_str_clone`, so the value follows the ordinary
  arena rules in [memory.md](memory.md) — bulk-freed with its scope, and no
  free hook.
- Non-string returns cross by value.
- A module may keep **process-lifetime Zig-owned state** (the `std.json` node
  table, the `std.http` client and connection tables) provided it never retains
  a Doxa arena pointer across calls: values crossing in are borrowed for the
  call, copied with an owned allocation if they must outlive it, and every owned
  buffer is released explicitly, since arena scopes do not cover Zig-owned
  memory.

### Array parameters and returns

- An array crosses as an opaque `*ArrayHeader` pointer. The compile-time
  element type in the signature fixes the runtime `elem_size` / `elem_tag`; the
  two must agree.
- An array parameter is borrowed for the call. The wrapper presents it to Zig as
  a `[]const T` — scalar and `byte[]` arrays alias the array's backing buffer,
  `string[]` is copied into a temporary per-call arena, and nested arrays recurse
  through that arena. The callee must not retain or store it.
- A returned `[]const T` is copied into a fresh `ArrayHeader` in the caller's
  arena (`doxa_array_new(__doxa_scope, …)`), so it follows the same arena rules
  as a returned string. Nested levels are deep-cloned as they are
  stored.

### Error model

There is no runtime ABI error channel to translate into a Doxa error — the
shape of the call is settled at compile time. Only scalars, `string`, `enum`,
and (nested) arrays of those cross the boundary; structs, maps, unions, and
functions are rejected with `E8002` while the wrapper is generated, before Zig
is invoked. A Zig function is not a value Doxa can hold or pass — Doxa only
*calls* the exported functions.
