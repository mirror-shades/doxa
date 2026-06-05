# Strings

Doxa strings are UTF-8 encoded and backed by a contiguous `u8` buffer. The **byte length**
is explicit: it is not inferred from a terminating NUL, and embedded `U+0000` bytes are
valid. In the interpreter, values are Zig `[]const u8` slices; at the inline Zig module
boundary, strings use the tagged `DoxaAbiValue` encoding (`payload0` = pointer,
`payload1` = length) documented in [zig.md](zig.md).

The LLVM backend still declares many string builtins (`@doxa_str_*`, etc.) as `ptr` into
NUL-terminated allocations for C-style helpers—so compiled programs may still touch
null-terminated buffers **inside** those runtime calls, even though the language model is
pointer + length.

## Quote semantics

Double-quoted strings (`"..."`) interpret `{expression}` blocks anywhere in the string body.
Any valid Doxa expression — a variable, an arithmetic operation, a function call — can appear
between the braces and will be stringified in place at runtime:

```doxa
var name is "Doxa"
@print("Hello {name}!\n")

var x is 10
@print("{x} squared is {x ** 2}\n")
```

Interpolation works wherever a string literal is accepted: in `@print` / `@println` calls,
variable initialisers, function arguments, map keys, and so on.

## The `string` type

`string` is a first-class primitive type alongside `int`, `float`, `byte`, `tetra`, and
`nothing`:

```doxa
var s :: string is "hello"               # explicit type annotation
var t is "world"                         # inferred as string
```

A `string` variable always holds a value — there is no null/nil sentinel. The empty string is
represented by `""`.

## Underlying u8 array

Strings are stored as an array of unsigned 8-bit bytes. This means you can bridge between
strings and byte data with `pack` and `unpack`:

| Builtin                | Signature                | Description                          |
|------------------------|--------------------------|--------------------------------------|
| `pack(array)`          | `int[]` → `string`       | Interpret each int as a u8 codepoint |
| `unpack(string)`       | `string` → `int[]`       | Decompose a string into byte values  |

```doxa
const bytes is [0x48, 0x69, 0x21]       # [72, 105, 33]
const word is pack(bytes)                # "Hi!"
const back is unpack(word)               # [72, 105, 33]
```

`pack` / `unpack` use the same byte-width guarantee as the underlying `u8` storage —
values outside 0–255 are truncated to fit.

## Common operations

| Expression                    | Result              |
|-------------------------------|---------------------|
| `"abc" + "def"`               | `"abcdef"`          |
| `length("hello")`             | `5`                 |
| `"こんにちは"[0]`             | (byte at index 0)   |
| `string(42)`                  | `"42"`              |
| `string(3.14)`                | `"3.14"`            |
| `type("hello")`               | `"string"`          |
| `"hello"[0]`                  | `104` (byte value)  |

String indexing returns the raw `u8` byte at that position. For multi-byte UTF-8 characters
iterate with `unpack` and reconstruct manually, or use a Zig block for Unicode-aware
processing.

## Interop with Zig

For **inline Zig modules** (`zig { … }` compiled as a shared library), the VM and
generated wrappers use `DoxaAbiValue`: strings are **pointer + byte length**, not C
strings. See [zig.md](zig.md) for the full ABI.

The **LLVM runtime** (`doxa_rt`) still exposes several helpers that take or return
`?[*:0]const u8` so those entry points stay C-compatible; that is an implementation detail
of those helpers, not the definition of the `string` type at the Zig module boundary.
