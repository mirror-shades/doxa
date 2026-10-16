# Intrinsic Methods

Doxa clearly distinguishes between compiler level "builtin" methods, and user space methods. All methods and functions provided by the compiler are prefixed with the @ symbol. A couple advantages, take this line for example, imagine you are reading some code in a programming language you have never read before: 

`pet_snake.length()`

Is this a user level method defining the length of the snake, or a compiler level method on a particular type like a string or array? We cannot know without more context about the language syntax. The same ambiguity goes for built in functions as well, consider:

`length(pet_snake)`

Mixing compiler level syntax and user level syntax is too ambigious. Doxa solves this by prefixing all compiler level methods with `@`

`@length(pet_snake)`

Wherever you see and @ you know this is not a user space functions. This also allows for a second advantage, that of method collision. If you want to define a length method for your pet snake struct, you can do so without risk of collision with the builtin length method. 

All methods are function level, including ones you may not be used to seeing at the function level:

```
@push(my_array, 42)
const my_value is @pop(my_array)
```

If you would prefer using an intrinsic method post fix, you can do so using `.` and the value will be passed as the first argument:

```
my_array.@push(42)
const my_value is my_array.@pop()
```

Intrinsic methods are not forgiving: an index out of bounds, a `@pop` of an empty array, or text that names no number stops the program with a runtime fault (see [Runtime faults](#runtime-faults)). Nothing they do is undefined. When a failure must not stop the program, the standard library's `methods` sublibrary has versions that check first or fall back.

## List of Methods

Here is a list of our intrinsic methods and an explaination of what they do. 

### Collection

A `string` and a `byte[]` do not share elements: the operations below return `string`s
when the collection is a `string` and `byte`s when it is a `byte[]`. `@pack` / `@unpack`
are the only bridge between the two. String positions (an index, `@slice`, `@find`,
`@remove`) are byte offsets — see [strings.md](strings.md).

- `@length(value :: string | array)` -> `int` (the byte length for a `string`)
- `@push(collection :: string | array, value :: any)` -> `nothing` — appends; a `string` takes a `string` (concatenates it), an array takes its element type
- `@pop(collection :: string | array)` -> `string` for a `string`, the element type for an array
- `@insert(collection :: string | array, index :: int, value :: any)` -> `nothing`
- `@remove(collection :: string | array, index :: int)` -> `string` for a `string`, the element type for an array
- `@clear(collection :: string | array)` -> `nothing`
- `@find(collection :: string | array, value :: any)` -> `int`
- `@slice(collection :: string | array, start :: int, length :: int)` -> the same collection type
- `@pack(bytes :: byte[])` -> `string` — interprets each byte as a u8 codepoint
- `@unpack(word :: string)` -> `byte[]` — decomposes a string into its byte values

### Type / Conversion

- `@string(value :: any)` -> `string`
- `@int(value :: float | byte | string)` -> `int`
- `@float(value :: int | byte | string)` -> `float`
- `@byte(value :: int | float | string)` -> `byte`
- `@type(value :: any)` -> `string`

These are required in value positions (assignment, function arguments, return values) whenever a runtime or named-const value needs to cross type boundaries that aren't lossless. Literals written directly in source widen implicitly (for example `var x :: float is 60`), but `const LIT is 60; takesFloat(LIT)` is an error — write `takesFloat(@float(LIT))` instead. The rule mirrors how `int` → `byte` already works. Operators and array literals promote automatically and don't need explicit casts.

### Diagnostics / Output

- `@print(format :: string)` -> `nothing`
- `@assert(condition :: tetra, message :: string?)` -> `nothing`
- `@panic(message :: string)` -> `nothing`
- `@exit(code :: int | byte)` -> `nothing`


## Failure Behavior

### Runtime faults

Every way a program fails at run time is a *fault*: it writes one line to stderr — what happened, where, then any details — and exits. Faults are checked in every `--opt-mode`; the compiler drops a check only where it proves the fault cannot happen. It then prints the Doxa frames that led to the fault, unless built with `--strip` ([Debugging](debugging.md#backtraces-on-fault)).

| Fault | Line | Exit |
| --- | --- | --- |
| `@panic(message)` | `Panic at main.doxa:3:5: message` | 1 |
| `@assert` with a false condition | `Assertion failed at main.doxa:4:5: message` (no `: message` without one) | 1 |
| Integer `//` or `%` by zero | `Division by zero at main.doxa:2:12` | 1 |
| `int` or `byte` `+`, `-`, `*` or `**`, or unary `-`, whose result leaves the type: `int` overflow, `255 + 1`, `0 - 1` on a `byte` | `Integer overflow at main.doxa:2:12` | 1 |
| An array or string index out of bounds, read or written; an `@insert` or `@remove` index out of bounds | `Index out of bounds at main.doxa:5:9: index 7, length 3` | 1 |
| `@pop` of an empty array | `Pop from an empty array at main.doxa:6:5` | 1 |
| `@int`, `@float` or `@byte` of text that names no such number | `Not a valid int at main.doxa:2:14: "abc"` | 1 |
| The system has no memory left for an allocation | `Out of memory` (no location: see below) | 1 |
| Reaching `unreachable` | `Reached unreachable code at main.doxa:7:9` | 2 |
| A read of reclaimed memory, under `--instrument=memcheck` only ([memory](memory.md#seeing-where-memory-goes)) | `Read of memory its arena reclaimed at main.doxa:7:9` | 2 |

Wrapping arithmetic is never implicit: the operators `+%`, `-%` and `*%` are how a program asks for it ([Overflow and wrapping](math.md#overflow-and-wrapping)). Exit status 2 is reserved for `unreachable`, which marks code the program's author claims can never run, and for memcheck's fault, which only a compiler or runtime bug can raise. The location is the start of the expression that faulted. Out of memory is the one fault without a location: it is raised inside the runtime's allocator, which does not know which expression asked, so its backtrace shows the frames that led to it (not under `--strip`). A file under the entry file's directory is named relative to it (`main.doxa`, `lib/util.doxa`); a file under another root keeps the root's tag (`std//io/io.doxa`). A string's `@insert` and `@remove`, and `@slice`, clamp to the bounds instead of faulting.

### Collection

- `@length(value)`
  - Fails at compile time for statically-known non-collection values.
  - Can fail at runtime for invalid dynamic values.
- `@push(collection, value)`
  - Fails at compile time when the value type does not match: a non-string pushed into a `string`, or a non-element pushed into a typed array.
  - Can fail at runtime if target is not array/string in dynamic paths.
- `@pop(collection)`
  - Faults on an empty array.
- `@insert(collection, index, value)`
  - Fails at compile time when the inserted value type does not match (a non-string into a `string`, or a non-element into a typed array).
  - On an array, faults unless `0 <= index <= length`. On a string, the index is clamped to its bounds.
- `@remove(collection, index)`
  - On an array, faults unless `0 <= index < length`. On a string, an index out of range leaves it whole.
- `@clear(collection)`
  - Fails at compile time for statically-known non-collection values.
  - Runtime form expects array/string.
- `@find(collection, value)`
  - Fails at runtime when searching a string with a non-string needle, or when target is not array/string.
  - Current implementation can cascade into `StackUnderflow` after the primary runtime error in some statement forms.
- `@slice(collection, start, length)`
  - Fails at runtime for negative indices/length, out-of-bounds ranges, wrong index types, or non-collection targets.
  - Current implementation can cascade into `StackUnderflow` after the primary runtime error in some statement forms.
- `@pack(bytes)`
  - Fails at compile time unless the argument is a `byte[]` (an integer literal array narrows to `byte[]`).
  - Fails at runtime for arrays whose elements cannot be coerced to a u8 codepoint.
- `@unpack(word)`
  - Does not currently fail for supported runtime values.

### Type / Conversion

- `@string(value)`
  - Does not currently fail for supported runtime values.
- `@int(value)`
  - Fails at runtime for invalid parse, non-finite values, overflow, or unsupported source type.
  - Text may be decimal, `0x` hex, or a decimal float (truncated toward zero), with surrounding whitespace. Text that names no int faults: `Not a valid int at main.doxa:2:14: "abc"`.
- `@float(value)`
  - Fails at runtime for invalid parse, non-finite values, or unsupported source type.
  - Text may be a decimal float (`inf` and `nan` included) or `0x` hex. Text that names no float faults.
  - Required for any non-literal `int` or `byte` value used where a `float` is expected; literals widen implicitly.
- `@byte(value)`
  - String conversion path fails at runtime for invalid parse or out-of-range values: a one-character string is that character's code, otherwise decimal, `0x` hex, or a decimal float in 0–255.
  - Numeric conversion path can clamp/zero out some invalid values (for example, `@byte(999)` currently yields `0x00`).
- `@type(value)`
  - Does not currently fail for supported runtime values.

A conversion of text that must not stop the program belongs to the standard library: `std.methods` has `isInt`, `isFloat` and `isByte` to test text first, and `parseIntSafe`, `parseFloatSafe` and `parseByteSafe` to read it with a fallback of zero.

In a constant expression (`docs/math.md`) a conversion of text the program could not convert is a compile error (`E1037`) instead.

### Diagnostics / Output

- `@print(format :: string)`
  - Argument must be a `string`. `@print` writes that string as-is; it does not interpolate or interpret `{...}`. Interpolation happens when a string value is produced (for example, in a double-quoted literal), so `@print("value is {value}")` still prints the current value because the literal is evaluated to a `string` before `@print` runs.
- `@assert(condition :: tetra, message :: string?)`
  - A false condition faults: `Assertion failed at <file>:<line>:<column>`, then `: <message>` when one is given; exit code 1.
- `@panic(message)`
  - Always faults: `Panic at <file>:<line>:<column>: <message>`; exit code 1.
- `@exit(code)`
  - Terminates the process with the provided status code (`int | byte`).

## Standard-library wrappers

`std.methods` (from `std/methods/methods.doxa`) wraps the collection intrinsics
with bounds and empty-collection checks. Families whose return type does not
depend on the collection's own type collapse to a single union-typed entry
point:

- `methods.push(^collection :: string | int[] | float[] | byte[] | tetra[] | string[], value)` -> `nothing`
- `methods.insert(^collection :: …, index, value)` -> `error.Method`
- `methods.clear(^collection :: …)` -> `nothing`
- `methods.find(collection :: …, value)` -> `int`

The element type is checked at run time: a `methods.push` value that does not
match the collection's element type panics, and a `methods.insert` value that
does not match returns `error.Method.Unexpected`. `methods.pop`,
`methods.remove`, and `methods.slice` stay split per element type because their
return type *is* the element type (or the collection's own type). See
[stdlib-api.md](stdlib-api.md) for the generated signatures.
