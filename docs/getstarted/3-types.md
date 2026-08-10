# Types

Doxa was designed to have a minimal number of native types which can be composed into higher more complex data structures. Here are the scalar types and their default values.

When we talk about **size** below, we mean what a value of that type *occupies as its own payload* in the native compiler path (roughly: what you would expect in a local or on the operand stack after lowering): fixed-width numbers, a **discriminant** for enums, or a **reference** for heap-backed data. **It is not the byte length of string contents**, array storage, or struct fields—those live on the heap (or in separate storage) and grow with your data.

**Scalars**

| Type | Value representation | Default value |
|------|----------------------|---------------|
| `int` | 64-bit signed integer | `0` |
| `float` | 64-bit IEEE binary float | `0.0` |
| `byte` | 8-bit unsigned integer | `0` (`0x00`) |
| `string` | Reference to UTF-8 text elsewhere | `""` |
| `tetra` | 2-bit unsigned integer | `false` |
| `enum` | **Discriminant** as an integer index |
| `nothing` | No payload  | `nothing` |

The `doxa run` VM represents values with more bookkeeping than the table above (for example enum variants carry type names for introspection), but the **logical** sizes and defaults match the native path. If you embed Doxa and exchange values through the C-facing runtime, values are passed as a tagged `DoxaValue` (tag + reserved flags + 64-bit payload); see `doxa_rt.zig` in the repo for the exact ABI.


### Array

Arrays are strictly typed, you must provide a single type for an array. Arrays of composite types like arrays, structs, etc. are supported. If arrays are sized in their type declaration such as: `var x :: int[64]` the size is fixed.

### Struct

**Structs** are named records with fields, functions, and methods. Doxa has **no classes and no inheritance**—you model “is-a” relationships by **composition** (a struct field whose type is another struct). Fields and callables are **private** unless marked `public`.

Struct literals **must** use the **`$`** prefix: `$MyStruct { ... }`. An identifier immediately followed by `{` is **not** a struct literal; it is parsed as an implicit `match` pattern, so forgetting `$` is a common early mistake. Struct values are allocated from the **current scope’s arena**. See [Structs](../struct.md) for construction, methods, and performance notes.

### Group

A **group** is a **nominal** umbrella over existing `enum`, `struct`, and other `group` types: it re-exports their variants and fields under **qualified** names (for example `Error.IOError.NotFound`) without introducing new runtime values. Use it when a function or API should accept “one of these known alternatives” as a single type. Flattening, uniqueness, and cycle rules matter once groups grow; see [Groups](../groups.md).

### Union

**Typed unions** are written with **`|`** between types, for example `int | float` or `string | nothing` for an optional string. A union variable holds **one** member type at a time; if you do not initialize it, it defaults to the **first** type in the union with that type’s default value.

To react to which member is active, use **`match`** (exhaustive branches narrow the type), **`value as T else { ... }`** for a single-arm cast with a fallback, or **`@istype`** in `if` / `else if` chains when you need imperative style. See [Typed unions](../unions.md) for patterns, including error-style returns.

## Numeric conversions

Numeric types (`int`, `float`, `byte`) convert implicitly in some contexts and require explicit casts in others. The goal is to prevent silent data loss while avoiding noise for obviously-safe conversions.

| Direction | Operator position | Value position | Requires |
|---|---|---|---|
| `byte` → `int` | implicit | implicit | nothing — always lossless |
| `int` → `byte` | — | comptime literals only | `@byte()` for runtime values |
| `int` → `float` | implicit | comptime literals only | `@float()` for runtime or named const values |
| `byte` → `float` | implicit | comptime literals only | `@float()` for runtime or named const values |

**Operator positions** (binary `+`, `-`, `*`, `/`, etc.) promote automatically via the numeric lattice: `float` > `int` > `byte`.

**Value positions** (assignment, function arguments, return values, struct fields) allow widening only when the right-hand side is a literal written directly in source. Named constants and runtime variables must use the explicit intrinsic.

```doxa
var f :: float is 10          # allowed — literal 10 widens to 10.0
takesFloat(10)                # allowed — literal 10 widens to 10.0
float[] is [1, 2, 3]         # allowed — each element is a literal

const LIT is 10
takesFloat(LIT)               # error — use @float(LIT)

var i :: int is 10
takesFloat(i)                 # error — use @float(i)

var b :: byte is 200
var i2 :: int is b            # allowed — byte → int is always lossless
```

Conversion intrinsics: `@float()`, `@int()`, `@byte()`, `@string()`. See [Intrinsic Methods](../methods.md).