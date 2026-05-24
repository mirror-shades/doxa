`zig` blocks are embedded Zig modules.

## Current source-level rules

1. Top-level is restricted to:
- `const X = @import("...");`
- `fn ... { ... }`

2. Function signatures are restricted to Doxa-compatible atomic types:
- Doxa `int` <-> Zig `i64`
- Doxa `float` <-> Zig `f64`
- Doxa `byte` <-> Zig `u8`
- Doxa `tetra` <-> Zig `bool` (lossy at boundary)
- Doxa `nothing` <-> Zig `void`
- Doxa `string` <-> Zig `[]const u8`

3. The compiler validates these rules before invoking Zig.

## Inline Zig ABI (argv/argc model)

The VM/runtime boundary for inline Zig must use a single argument vector ABI:

- `argv`: pointer to an array of ABI values (`DoxaAbiValue`)
- `argc`: number of entries in `argv`
- one ABI entry per Doxa call argument, in source order

Wrappers decode by tag, then call the real Zig function.

### Canonical ABI value layout

```zig
pub const DoxaAbiTag = enum(u32) {
    Int = 0,
    Float = 1,
    Byte = 2,
    String = 3,
    Tetra = 4,
    Nothing = 5,
};

pub const DoxaAbiValue = extern struct {
    tag: DoxaAbiTag,
    flags: u32,    // reserved, currently 0
    payload0: u64, // bits or pointer
    payload1: u64, // extra payload (String length)
};

pub const DoxaAbiStatus = enum(i32) {
    ok = 0,
    bad_arity = 1,
    bad_tag = 2,
    bad_value = 3,
    internal = 255,
};
```

Notes:
- Layout is part of the ABI contract and must remain stable.
- `extern struct` is required for a predictable C ABI layout.
- `flags` is reserved for future use (ownership/nullability/union metadata).

### Wrapper export signature

Generated wrapper functions should use:

```zig
pub export fn __doxa_export__Module_fn(
    argv: [*]const DoxaAbiValue,
    argc: usize,
    out_ret: *DoxaAbiValue,
) callconv(.c) DoxaAbiStatus
```

Behavior:
- validate `argc` first
- decode each argument from `argv[i]` by tag
- on mismatch, return `bad_tag` or `bad_value`
- call user Zig function only after successful decode
- encode return into `out_ret`, return `ok`

### Encoding rules

- `Int`: `payload0 = bitcast(i64 -> u64)`
- `Float`: `payload0 = bitcast(f64 -> u64)`
- `Byte`: `payload0 = u8`
- `Tetra`: `payload0 = u2` (`0=false`, `1=true`, `2=both`, `3=neither`)
- `String`: `payload0 = ptr`, `payload1 = len`
- `Nothing`: payload ignored

String rule:
- Strings are `(ptr,len)`, not C-strings.
- Embedded `\0` is valid and must be preserved.

### Ownership and lifetime (v1)

- `argv` values are borrowed for the duration of the call.
- `out_ret.String` is owned by callee and must be freed by runtime/VM using the module free hook.
- Non-string scalar returns are by value in `out_ret`.

### Error model

Wrapper returns status; no trap/panic for user type mismatch:
- `bad_arity`: `argc` does not match expected function arity
- `bad_tag`: tag incompatible with expected parameter type
- `bad_value`: malformed payload (e.g. null pointer with non-zero string length)
- `internal`: unexpected wrapper/runtime failure

The caller (VM/runtime) turns status into a Doxa runtime error.

## Compile/run parity requirement

Both modes must use the same logical ABI:
- `doxa run`: dynamic library call path uses `argv/argc`
- `doxa compile`: generated call path should align with the same value model

No per-signature C ABI shims should be required at the VM boundary.

## Migration notes

- Old per-argument exports (`fn(x: i64)`, C-string-only string bridge, 0/1-arg VM specialization) are legacy.
- New architecture is the `argv/argc` + tagged decode wrapper model.
- Existing inline Zig source syntax does not need to change.
