# Doxa String Architecture

## Canonical Model

```
DoxaString = { ptr: ?[*]const u8, len: usize }   // 16 bytes
```

In LLVM IR: `%DoxaString = type { ptr, i64 }`

Every layer treats strings as a pointer + explicit byte length. No layer scans for null. Length is O(1). Embedded `U+0000` is valid data.

## Layers

| Layer | Representation | Status |
|---|---|---|
| Parser / AST | `[]const u8` (Zig slice) | unchanged |
| HIR / VM | `HIRValue{ .string: []const u8 }` | unchanged |
| ABI (`DoxaAbiValue`) | `tag=3, payload0=ptr, payload1=len` | unchanged |
| LLVM IR | `%DoxaString = type { ptr, i64 }` | done |
| LLVM codegen stack | `StackType.STRING` | done |
| Runtime (`doxa_rt.zig`) | `DoxaString` struct internally; exports use `(ptr, len)` pairs + out-params | done |

## C ABI Constraint

A 16-byte struct cannot cross `callconv(.c)` on Windows x64 by value. LLVM and Zig disagree on sret vs RAX:RDX for struct returns. Therefore:

- **All export functions** take `(ptr: ?[*]const u8, len: usize)` as separate parameters.
- **All export functions returning a string** use out-parameters `(out_ptr: *?[*]u8, out_len: *usize)`.
- `DoxaString` is used internally only (heap allocations, LLVM stack values, runtime internals).
- LLVM codegen extracts ptr/len via `extractvalue %DoxaString {reg}, 0/1` before calling exports.

## Runtime Functions

### String operations (all use ptr+len pairs)

| Function | Parameters | Returns via |
|---|---|---|
| `doxa_write_cstr` | `(ptr, len)` | `void` |
| `doxa_str_concat` | `(a_ptr, a_len, b_ptr, b_len)` | out-params |
| `doxa_str_clone` | `(ptr, len)` | out-params |
| `doxa_str_clone_raw` | `(ptr, len)` | raw `?[*:0]u8` (null-terminated) |
| `doxa_str_from_cstr` | `(?[*:0]const u8)` | out-params (null-term scan) |
| `doxa_substring` | `(ptr, len, start, length)` | out-params |
| `doxa_str_insert` | `(s_ptr, s_len, idx, ins_ptr, ins_len)` | out-params |
| `doxa_str_remove` | `(ptr, len, idx)` | out-ptrs to DoxaString |
| `doxa_str_pop` | `(ptr, len)` | out-ptrs to DoxaString |
| `doxa_str_len` | `(ptr, len)` | returns `i64` (just `@intCast(len)`) |
| `doxa_str_eq` | `(a_ptr, a_len, b_ptr, b_len)` | returns `bool` |
| `doxa_find_str` | `(h_ptr, h_len, n_ptr, n_len)` | returns `i64` |
| `doxa_pack_bytes` | `(array_header)` | out-params |
| `doxa_unpack_bytes` | `(ptr, len)` | returns array header |
| `doxa_peek_string` | `(ptr, len)` | `void` (no null-term scan) |
| `doxa_char_to_string` | `(ch: u8)` | out-params |
| `doxa_int_to_string` | `(value: i64)` | out-params |
| `doxa_float_to_string` | `(value: f64)` | out-params |
| `doxa_byte_to_string` | `(value: i64)` | out-params |
| `doxa_tetra_to_string` | `(value: i64)` | out-params |
| `doxa_nothing_to_string` | `()` | out-params |
| `doxa_enum_to_string` | `(type_ptr, type_len, bits)` | out-params |
| `doxa_struct_to_string` | `(instance_ptr)` | out-params |
| `doxa_array_to_string` | `(array_header)` | out-params |

### Shims

| Function | Purpose | Planned Removal |
|---|---|---|
| `doxa_write_raw` | Peek/debug internals that still carry null-terminated globals (`internPeekString`) | After peek globals are migrated to `DoxaString` |

## LLVM IR Printer (`src/codegen/llvmir/ir_printer/`)

### Type System

```
StackType = { I64, F64, I8, I1, I2, PTR, STRING, Value, Nothing }
%DoxaString = type { ptr, i64 }
%DoxaValue   = type { i32, i32, i64 }
```

Key mappings:
- `hirTypeToStackType(.String)` → `.STRING`
- `stackTypeToLLVMType(.STRING)` → `"%DoxaString"`
- `hirTypeToLLVMType(.String)` → `"%DoxaString"`

### Coercion Matrix

| Target ↓ / Incoming → | STRING | PTR | Value | I64 | F64 | I8/I2/I1 | Nothing |
|---|---|---|---|---|---|---|---|
| **STRING** | identity | `ensureString` (len=0) | `ensurePointer`→`ensureString` | same as Value | same as Value | same as Value | identity |
| **PTR** | `extractvalue 0` | identity | extract payload→inttoptr | inttoptr | ensurePointer | ensurePointer | return null |
| **I64** | extract ptr→ptrtoint | ptrtoint | extract payload | identity | bitcast | zext | identity |
| **Value** | clone_raw→ptrtoint | ptrtoint | identity | identity | identity | identity | identity |

### String Operations (`handleStringOp`)

Every operation extracts ptr+len from the `%DoxaString` on the stack, calls the runtime function with `(ptr, len)` args, and builds a result `%DoxaString` from out-params.

Pattern:
```llvm
%ptr = extractvalue %DoxaString %arg, 0
%len = extractvalue %DoxaString %arg, 1
; ... allocate out-param slots ...
call void @doxa_...(ptr %ptr, i64 %len, ..., ptr %out_ptr, ptr %out_len)
%r_ptr = load ptr, ptr %out_ptr
%r_len = load i64, ptr %out_len
%tmp = insertvalue %DoxaString undef, ptr %r_ptr, 0
%result = insertvalue %DoxaString %tmp, i64 %r_len, 1
```

### Shared Helper: `emitRTCallReturningString`

Encapsulates the alloca → init → call → load → insertvalue pattern in one call. Used across `shared_handlers.zig`, `collections_emit.zig`, `value_helpers.zig`.

## Array Storage for Strings

String arrays use **16-byte elements** (ptr + i64 length). The runtime provides dedicated accessors:

| Function | Signature | Notes |
|---|---|---|
| `doxa_array_get_str` | `(hdr, idx, out_ptr: *?[*]u8, out_len: *usize) → void` | Reads both i64 slots via out-params |
| `doxa_array_set_str` | `(hdr, idx, ptr, len) → void` | Writes both i64 slots |
| `doxa_array_insert_str` | `(hdr, idx, ptr, len) → *ArrayHeader` | Insert with str-aware shifting |
| `doxa_array_remove_str` | `(hdr, idx, out_ptr, out_len) → *ArrayHeader` | Remove with str-aware shifting |
| `arrayElementSize(.String)` | returns `16` | LLVM printer |

**LLVM codegen** (`collections_emit.zig`): All array operations (`emitArrayGet`, `emitArraySet`, `emitArrayPop`, `emitArrayRemove`, `emitArrayInsert`) have `.String` fast-paths that use the `_str` variants (16-byte), bypassing the generic `_i64` (8-byte) + `convertArrayStorageToValue` path.

**Runtime** (`doxa_rt.zig`): `doxa_array_insert`, `doxa_array_remove`, `doxa_array_slice`, `printArrayHdrImpl`, `existsQuantifier`, and `forallQuantifier` all use `doxa_array_get_str`/`doxa_array_set_str` internally for `elem_tag == 3 && elem_size >= 16`. Legacy 8-byte C-string paths have been removed.

## Struct Field Storage for Strings

Structs store fields as single `i64` slots (8 bytes). String fields can't store 16-byte `DoxaString` without layout changes. The compromise:

- **Write path** (`emitSetField`, `emitStructNew`): calls `doxa_str_clone_raw(ptr, len)` which allocates with a null terminator, returns the raw pointer. The pointer is stored as `i64` via `ptrtoint`.
- **Read path** (`emitGetField`, `convertArrayStorageToValue`): calls `doxa_str_from_cstr(ptr)` which scans for null and recovers the length.

This preserves the single-i64-per-field struct layout at the cost of a null-term scan on read.

## Map Storage for Strings

Maps store key/value pairs as single `i64` slots (8 bytes). String values use the same pointer-only storage pattern as structs — `convertValueToArrayStorage(.String)` clones via `doxa_str_clone_raw` and stores only the pointer as `i64`. The map's own getter (`doxa_map_get_i64`) returns an i64, and the caller is responsible for recovering the string via `doxa_str_from_cstr` if needed.

## Unwrapping DoxaValue for Strings

When a `%DoxaValue` tagged as `.String` is narrowed to `string` (e.g. `expr_result as string then ...`), the `unwrapDoxaValueToType` function converts the raw i64 payload back to a `%DoxaString` by calling `@doxa_str_from_cstr` (null-scan). This is necessary because `buildDoxaValue` stores only a `ptrtoint` of the pointer in the 8-byte payload slot, discarding the length. The null-scan recovers it at the cost of a runtime call.

## Empty Strings

Empty strings use a sentinel global `@.doxa.empty = private constant [1 x i8] c"\00"`. The `handleConst` path GEPs into this for `s.len == 0` instead of using `ptr null`. This prevents crashes when code indexes into an empty string (e.g. `""[0]` → `getelementptr inbounds i8, ptr @.doxa.empty, i64 0` → valid load of `\0`).

## Inline Zig Interop

The native bridge uses `(ptr: ?[*]const u8, len: usize)` pairs, matching the runtime convention. String returns use out-params. The generated Zig preamble no longer defines `DoxaString` (dead code after the bridge changed from struct params to ptr+len pairs).

## Pool vs Peek Globals

String constants from `hir.constant_pool` are emitted as `@.str.{N}` globals WITHOUT null terminators. `handleConst` references these directly via `string_pool_len + constant_id`.

Peek/debug strings (type names, labels, field names, tetra literals, punctuation) still use `internPeekString` which creates null-terminated `@.peek.str.{N}` globals. These globals are passed to `@doxa_write_raw(ptr)` which internally calls `std.mem.span()` to recover the length. This path is documented as transitional — when peek globals are migrated to explicit ptr+len, `doxa_write_raw` can be retired. TODO present in code at `internPeekString`.

## Known Limitations

| Issue | Location | Impact |
|---|---|---|
| Struct fields store only pointer (8 bytes) | `structs_enums_emit.zig`, `doxa_str_clone_raw` | Null-scan on every struct field read. Layout change needed to store 16 bytes. |
| Map values store only pointer (8 bytes) | `convertValueToArrayStorage(.String)` | Null-scan on every map string value read. |
| `doxaStringFromBits` null-scan recovery | `doxa_rt.zig` (5 call sites) | Used for struct fields, enum descriptors, peek metadata — all contexts where only a raw pointer is stored. |
| `ensureString` from `.PTR` → len=0 | `value_helpers.zig` | Callers that need valid length must guard with `arg.ty == .STRING` before calling. |
| `ensureI64` from `.STRING` → discards length | `value_helpers.zig`, `core.zig` | Required for coercion into i64 slots (maps, structs, generic storage). |
| Peek globals null-terminated | `internPeekString` (27 call sites) | Printed via `@doxa_write_raw` (null-scan). Migration to `@doxa_write_cstr(ptr, len)` planned. |

## Test Status

| Suite | Result |
|---|---|
| LSP | 6 ok, 0 fail |
| VM (RUN) | 588 ok, 0 fail |
| Compiled (COMPILE) | 588 ok, 0 fail, 1 untested (HTTP link) |
| Error messages | 27 ok |

## Remaining Work

### 1. Peek globals migration (Medium)

**Location:** `internPeekString` in `ir_printer.zig`, 27 call sites across `shared_handlers.zig`, `structs_enums_emit.zig`, `function_emit.zig`. `doxa_write_raw` in `doxa_rt.zig`.

**Plan:** Replace null-terminated `@.peek.str.{N}` globals with the string pool format (explicit ptr+len). Change all `@doxa_write_raw(ptr)` calls to `@doxa_write_cstr(ptr, len)`. Remove `doxa_write_raw` and the null terminator from interned strings.

### 2. Struct field layout (High)

**Location:** `structs_enums_emit.zig` (`emitSetField`, `emitGetField`), `convertArrayStorageToValue(.String)`, `convertValueToArrayStorage(.String)`.

**Problem:** Struct fields are 8 bytes (i64), but DoxaString is 16 bytes. Struct construction (`$Type { ... }`) and field access store/recover strings via null-terminated C-string pointers.

**Options:**
- Expand struct fields to 16 bytes per slot, breaking struct layout compatibility.
- Keep single-i64 layout but embed both ptr (48 bits) and len (16 bits small-string) into one i64, requiring encoding.
- Accept the null-scan as the permanent cost for struct field reads and optimize the cost with a fast-path when the clone is recent.

### 3. Map string storage (Medium)

**Location:** `doxa_map_get_i64`/`doxa_map_set_i64`, `convertValueToArrayStorage(.String)` called from map emit paths.

**Problem:** Maps use `i64` key/value slots. String values stored as pointer-only.

**Options:**
- Expand map slot size to 16 bytes for string values (similar breaking change to structs).
- Add `doxa_map_get_str` / `doxa_map_set_str` with out-params (low effort, same pattern as arrays).

### 4. Enum descriptor string storage (Low)

**Location:** `doxa_rt.zig` — enum variant names stored as `[*:0]const u8` null-terminated pointers in runtime descriptors.

**Problem:** `lookupEnumVariantName`, `printEnumImpl`, `findEnumDescByName`, etc. use `std.mem.span()` to recover names. These are compile-time descriptors; migrating to ptr+len would avoid runtime null-scan on every enum-to-string conversion.

### 5. `doxa_str_clone_raw` usage audit (Low)

**Location:** Multiple call sites that create null-terminated strings for storage in 8-byte slots.

**Problem:** `doxa_str_clone_raw` allocates with a null terminator and the caller discards the length. If all 8-byte storage slots are eventually expanded to 16 bytes, this function and its callers can be replaced with `doxa_str_clone` (ptr+len, no null terminator).

### Removed Legacy Code

| Function/Path | Reason |
|---|---|
| Legacy 8-byte path in `doxa_array_get_str` | Pre-rework format; removed per no-backwards-compat policy |
| Legacy 8-byte write path in `doxa_array_set_i64` | Same |
| `cloneLegacyCStr` function | Only caller was the legacy write path above |
