# Strings

Doxa strings are UTF-8 encoded and backed by a contiguous `u8` buffer. The **byte length**
is explicit: it is not inferred from a terminating NUL, and embedded `U+0000` bytes are
valid. Inside Zig, values are `[]const u8` slices; at the inline Zig module boundary they
cross as pointer + byte length (a `(ptr, u64 len)` parameter pair, or `out_ptr`/`out_len`
out-params for returns), as documented in [zig.md](zig.md).

Every runtime string helper takes pointer + length and never reads past the length; no
string is NUL-terminated. Byte lengths at that compiled boundary are a fixed 64-bit `u64`
on every target (matching the emitted IR's `i64` lengths); only pointers follow the
target's pointer width.

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

A placeholder holds exactly one expression: `"{x y}"` is an error at the `y` (E2023), never
`"{x}"` with the rest dropped.

Interpolation works wherever a string literal is accepted: in `@print` / `@println` calls,
variable initialisers, function arguments, and so on. A module path and a `match` pattern
are constants, so a string there holds no placeholder.

## Escapes

A backslash starts an escape. These are all of them:

| Escape | Means |
| --- | --- |
| `\"` | a double quote |
| `\\` | a backslash |
| `\n` | a line feed |
| `\r` | a carriage return |
| `\t` | a tab |
| `\{`, `\}` | a literal brace, never a placeholder |
| `\u{...}` | the code point with 1–6 hex digits, as UTF-8 |

Any other escape is an error. A brace an escape produces is text: `"\u{7b}"` is `{`. An
unescaped `{` always opens a placeholder, which closes on its line (or on a line it
continues with `...`), so `"a{b"` is an error that says to write `"a\{b"`. A placeholder's
source is code: a string inside it has its own escapes and placeholders
(`"{"q\"}"}"` is `q"}`).

A raw line break inside a literal is one byte of the string, `\n`, unless the next line
begins with `...`, which continues the literal without it. A file with Windows line
endings (`\r\n`) means the same program: each line break is still `\n`.

## The `string` type

`string` is a first-class primitive type alongside `int`, `float`, `byte`, `tetra`, and
`nothing`:

```doxa
var s :: string is "hello"               # explicit type annotation
var t is "world"                         # inferred as string
```

A `string` variable always holds a value — there is no null/nil sentinel. The empty string is
represented by `""`.

## Values and variables

A string is a value: storing one never lets two names see each other's changes.
Underneath, a string value is a pointer and a byte length, and its bytes are never
rewritten once written, so a store, an argument or a struct field shares them rather
than copying.

A string variable — a `var`, a parameter, a module-level `var` — is a buffer: its value
plus room to grow. Appending to it writes into that room, and when the room runs out
the variable moves to a buffer twice the size. Building a string in a loop therefore
costs time and memory in proportion to its length:

```doxa
var out is ""
each word in words {
    @push(out, word)        # also: out += word, out is out + word, out is "{out}{word}"
}
```

Reading part of a string allocates nothing: `@slice`, `s[i]`, `each c in s`, `@pop`, and
`@remove` of the first or last byte are views of the bytes already there. A template or
a chain of `+` builds its result with one allocation, numbers written straight into it.
`@pack` of a fresh byte array (a literal, a call's result) keeps the array's bytes; of an
array a variable holds, it copies them, since the variable could still write them.

A value taken from the variable before an append keeps what it held. A string in a
struct field or an array element is a value, not a variable: appending to one builds a
new string, so build text in a variable and store it once. `s[i] is x` is an error on
purpose — rewriting a byte would make every string store a copy.

## Underlying u8 array

Strings are stored as an array of unsigned 8-bit bytes. This means you can bridge between
strings and byte data with `pack` and `unpack`:

| Builtin                | Signature                | Description                          |
|------------------------|--------------------------|--------------------------------------|
| `@pack(bytes)`         | `byte[]` → `string`      | Interpret each byte as a u8 codepoint |
| `@unpack(string)`      | `string` → `byte[]`      | Decompose a string into byte values  |

```doxa
const bytes :: byte[] is [0x48, 0x69, 0x21]   # [72, 105, 33]
const word is @pack(bytes)                     # "Hi!"
const back is @unpack(word)                    # byte[] is [72, 105, 33]
```

`@pack` requires a `byte[]`; an array of integer literals narrows to `byte` in the
same way as any other `byte` context. `@unpack` decomposes a string byte-for-byte, so
`@pack` / `@unpack` round-trip the underlying `u8` storage.

## Common operations

| Expression                    | Result              |
|-------------------------------|---------------------|
| `"abc" + "def"`               | `"abcdef"`          |
| `length("hello")`             | `5`                 |
| `"hello"[0]`                  | `"h"` (a 1-byte string) |
| `"こんにちは"[0]`             | the leading byte of the first character, as a string |
| `string(42)`                  | `"42"`              |
| `string(3.14)`                | `"3.14"`            |
| `type("hello")`               | `"string"`          |

A `string` and a `byte[]` do not share elements. Indexing, `@pop`, `@remove`, and
`@slice` on a `string` produce `string`s; the same operations on a `byte[]` produce
`byte`s. `@pack` and `@unpack` are the only bridge between the two.

Indexing, slicing, `@find`, `@pop`, `@remove`, and `for`/`each` iteration all address
**bytes**, not UTF-8 codepoints — `@length` is the byte length — so `"こんにちは"[0]` is
the leading byte of a three-byte character, not the whole character, and
`each c in "日本語"` yields nine one-byte strings. To walk codepoints, `@unpack` the
string and reconstruct with `@pack`, or use a Zig block for Unicode-aware processing.

## Comparison

`==` and `!=` compare strings byte for byte. `<`, `<=`, `>` and `>=` order them
**lexicographically by byte**: the first differing byte decides, and a string sorts
before any longer string it is a prefix of.

```doxa
"apple" < "banana"   # true
"app" < "apple"      # true
"Z" < "a"            # true — uppercase ASCII sorts before lowercase
"z" < "é"            # true
```

For UTF-8 this is exactly Unicode code-point order, so it is total, locale-free and
stable across platforms. It is not a collation: it knows nothing of case folding or
accents. Locale-aware ordering belongs in a library, not an operator.

## Interop with Zig

For **inline Zig modules** (`zig { … }` blocks compiled into the program), the generated
wrappers pass strings as **pointer + byte length**, not C strings. See
[zig.md](zig.md) for the full ABI.

A string variable crosses as the value it holds: a Zig function never sees a buffer's
spare room.
