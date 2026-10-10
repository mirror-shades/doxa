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

Intrinsic methods can be considered inherently considered unsafe, and they may error. If an error occurs, there may be undefined behavior, panics, and other potentially unrecoverable errors. If you want bounds checking, and other safe wrappings, you must either define them yourself, or use the standard library which provides safe versions under the `method` sublibrary.

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

Core intrinsics are intentionally unsafe. Out-of-bounds and invalid-argument failures are accepted behavior and should be treated as runtime traps unless noted otherwise.

### Collection

- `@length(value)`
  - Fails at compile time for statically-known non-collection values.
  - Can fail at runtime for invalid dynamic values.
- `@push(collection, value)`
  - Fails at compile time when the value type does not match: a non-string pushed into a `string`, or a non-element pushed into a typed array.
  - Can fail at runtime if target is not array/string in dynamic paths.
- `@pop(collection)`
  - Fails at runtime on empty array/string.
  - Current implementation can cascade into a secondary `StackUnderflow` after the primary runtime error in some statement forms.
- `@insert(collection, index, value)`
  - Fails at compile time when the inserted value type does not match (a non-string into a `string`, or a non-element into a typed array).
  - Fails at runtime for negative index, out-of-bounds index, wrong index type, or wrong inserted type for string targets.
  - Current implementation can cascade into `StackUnderflow` after the primary runtime error in some variable-assignment forms.
- `@remove(collection, index)`
  - Fails at runtime for negative index, out-of-bounds index, wrong index type, or non-collection target.
  - Current implementation can cascade into `StackUnderflow` after the primary runtime error in some variable-assignment forms.
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
  - Text may be decimal, `0x` hex, or a decimal float (truncated toward zero), with surrounding whitespace. Text that names no int stops the program: `@int: "abc" is not a valid int`.
- `@float(value)`
  - Fails at runtime for invalid parse, non-finite values, or unsupported source type.
  - Text may be a decimal float (`inf` and `nan` included) or `0x` hex. Text that names no float stops the program.
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
  - False condition writes the message (if any) and `Assertion failed` to stderr, then halts execution with exit code 1.
- `@panic(message)`
  - Writes the message to stderr and always terminates execution with exit code 1.
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
