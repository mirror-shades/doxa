
`zig` blocks are embedded Zig modules.

Current rules:
1) **Top-level is restricted**: only `const X = @import("...");` and `fn ... { ... }` are allowed (comments/blank lines ok).
2) **Signatures are restricted to Doxa-compatible types**:
   - `int`  -> `i64`
   - `float` -> `f64`
   - `byte` -> `u8`
   - `tetra` -> `bool`
   - `nothing` -> `void`
   - `string` -> `[]const u8` (validated, but current runtime bridge is limited; see below)
3) The compiler validates these rules before invoking the Zig toolchain.


```
zig ZigOps {
    fn fast_hash(data: []const u8) -> u64 {
        return @llvm_external_call("ZigOps.fast_hash", data.ptr, data.len);
    }
    
    fn sqrt_approx(x: f64) -> f64 {
        return @llvm_external_call("ZigOps.sqrt_approx", x);
    }
}

const zigSqrt is ZigOps.sqrt_approx(10.5)
```

Notes / current implementation:
- **Caching**: zig blocks are compiled into `out/zig/cache/` keyed by a hash of `(module name + source)`. Rebuilds reuse cached artifacts when possible.
- **Run mode (`doxa run`)**: zig blocks compile to a dynamic library and are called via the VM (currently supports numeric/bool + up to 1 argument).
- **Compile mode (`doxa compile`)**: zig blocks compile to object files and are linked into the final executable.
- The example above uses `->` for readability; Zig also allows `fn sqrt_approx(x: f64) f64 { ... }`.

String support (important):
- **Signatures may mention `[]const u8`**, but full string bridging is **not implemented yet**.
- **Run mode (`doxa run`) limitation**: the VM↔Zig bridge currently marshals only numeric/bool values (and up to 1 argument). It does **not** pass `(ptr,len)` into Zig or convert returned buffers back into a Doxa string safely.
- **Compile mode (`doxa compile`) limitation**: module calls are currently emitted as external calls without a typed ABI for strings. Without an explicit `(ptr,len)` ABI and ownership rules, strings will miscompile or produce garbage.
- **Planned ABI (workable direction)**:
  - Pass Doxa `string` to Zig as `(ptr: [*]const u8, len: usize)` (or `(ptr: [*]const u8, len: u64)`).
  - Returned strings must follow a defined ownership model (e.g. allocate via a Doxa runtime allocator, or return into a caller-provided buffer), otherwise lifetime is undefined.

Ambiguities / workarounds:
- The doc shows `@llvm_external_call(...)` (not a Zig builtin). Today the simplest path is to write normal Zig bodies (or call `extern`/`@cImport` as needed). We can add a dedicated `@llvm_external_call` shim later if you want that exact syntax.

with some clairity on rules, molecular types could be provided as well. structs would need to have a matching zig counterpart, and arrays would need to be handled as slices, etc. This can come later when the implementation is mature.

