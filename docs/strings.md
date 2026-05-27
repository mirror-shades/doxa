# Strings

Doxa strings are UTF-8 encoded and backed by a contiguous `u8` array under the hood. The
underlying runtime representation is a null-terminated byte sequence, which means strings
interop naturally with both Zig's `[:0]const u8` and C's `const char*`.

## Quote semantics

### Double quotes — interpolation

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

### Single quotes — literal text

Single-quoted strings (`'...'`) are **literal** — no interpolation is performed. Every
character between the delimiters, including `{` and `}`, is preserved as written:

```doxa
var json is '{ "x": 1, "y": 2 }'        # braces stay as text
var template is 'Hello {name}'           # literal "{name}", not a reference
```

Use single quotes when you need curly braces to remain part of the string value itself
(JSON, code snippets, format templates, etc.).

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

Doxa strings map directly to Zig's `?[*:0]const u8` (a nullable null-terminated pointer).
When calling into Zig from Doxa — or embedding a `zig {}` block — strings are passed and
returned as this C-compatible pointer type without any conversion overhead.
