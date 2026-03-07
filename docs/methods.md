# Method Calls

Doxa supports two method-call families:

- Core intrinsics: `@...` calls implemented in core/compiler runtime.
- Standard library calls: module functions such as `std.io.input()`.

## Core Intrinsics

These are the minimal core intrinsics intended to remain as the stable base.
Prefer `std.*` wrappers where available.

### Collection

- `@length(value)` -> `int`
- `@push(collection, value)` -> `nothing`
- `@pop(collection)` -> `any`
- `@insert(collection, index, value)` -> `nothing`
- `@remove(collection, index)` -> `any`
- `@clear(collection)` -> `nothing`
- `@find(collection, value)` -> `int`
- `@slice(collection, start, length)` -> `string | array`

### Type / Conversion

- `@string(value)` -> `string`
- `@int(value)` -> `int`
- `@float(value)` -> `float`
- `@byte(value)` -> `byte`
- `@type(value)` -> `string`

### Diagnostics / Output

- `@print("text")` -> `nothing`
- `@assert(condition, "message"?)` -> `nothing`
- `@panic(message)` -> `nothing`
- `@exit(code)` -> `nothing`

## Removed Core Intrinsics

These intrinsics were transitional and are now removed from core/runtime.
Use std-library modules instead:

- `@random()`
- `@os()`
- `@arch()`
- `@abi()`
- `@time()`
- `@tick()`
- `@input()`
- `@sleep(ms)`
- `@build(src, out, arch, os, abi, debug)`
- `@read(path)`
- `@argc()`
- `@argv(index)`

Use std-library modules (`std.io`, `std.time`, `std.process`, `std.file`, etc.)
instead of adding new dependencies on these intrinsics.

## Failure Behavior (Current Runtime)

Core intrinsics are intentionally unsafe. Out-of-bounds and invalid-argument failures are accepted behavior and should be treated as runtime traps unless noted otherwise.

### Collection

- `@length(value)`
  - Fails at compile time for statically-known non-collection values.
  - Can fail at runtime for invalid dynamic values.
- `@push(collection, value)`
  - Fails at compile time for some obvious type mismatches (for example, pushing non-string into string).
  - Can fail at runtime if target is not array/string in dynamic paths.
- `@pop(collection)`
  - Fails at runtime on empty array/string.
  - Current implementation can cascade into a secondary `StackUnderflow` after the primary runtime error in some statement forms.
- `@insert(collection, index, value)`
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

### Type / Conversion

- `@string(value)`
  - Does not currently fail for supported runtime values.
- `@int(value)`
  - Fails at runtime for invalid parse, non-finite values, overflow, or unsupported source type.
- `@float(value)`
  - Fails at runtime for invalid parse, non-finite values, or unsupported source type.
- `@byte(value)`
  - String conversion path fails at runtime for invalid parse or out-of-range values.
  - Numeric conversion path can clamp/zero out some invalid values (for example, `@byte(999)` currently yields `0x00`).
- `@type(value)`
  - Does not currently fail for supported runtime values.

### Diagnostics / Output

- `@print("text")`
  - Current parser expects string-literal format input. Calls like `@print(123)` fail at compile time. Value interpolation can be done by placing expressions in curly brackets like `@print("value is {value}")`
- `@assert(condition, "message"?)`
  - False condition halts execution and reports assertion failure location/message.
- `@panic(message)`
  - Always terminates execution with a runtime panic.
- `@exit(code)`
  - Terminates process with the provided status code (`int | byte`).

## Safe Standard Library Wrappers

Use `std.methods` for guarded wrappers around intrinsic operations that can fail/trap. Non-failing intrinsics are not wrapped.

### Import

- `module std from "std"`

### Collection Wrappers

Collection wrappers are type-specific so mismatched element types are rejected at compile time.

- Push:
  - `std.methods.pushString(^string, string)` -> `nothing`
  - `std.methods.pushInt(^int[], int)` -> `nothing`
  - `std.methods.pushFloat(^float[], float)` -> `nothing`
  - `std.methods.pushByte(^byte[], byte)` -> `nothing`
  - `std.methods.pushTetra(^tetra[], tetra)` -> `nothing`
  - `std.methods.pushStringArray(^string[], string)` -> `nothing`
- Pop:
  - `std.methods.popString(^string)` -> `string | error.Method`
  - `std.methods.popInt(^int[])` -> `int | error.Method`
  - `std.methods.popFloat(^float[])` -> `float | error.Method`
  - `std.methods.popByte(^byte[])` -> `byte | error.Method`
  - `std.methods.popTetra(^tetra[])` -> `tetra | error.Method`
  - `std.methods.popStringArray(^string[])` -> `string | error.Method`
- Insert / Remove / Slice:
  - `insert*`, `remove*`, and `slice*` wrappers exist for each type above and return `error.Method.OutOfBounds` on invalid ranges/indexes.
- Clear:
  - `clear*` wrappers exist for each type above and return `nothing`.
- Find:
  - `find*` wrappers exist for each type above and return `int` (`-1` if not found).

### Conversion Wrappers

- `std.methods.toInt(value)` where `value :: int | float | byte | string` -> `int | error.Method`
- `std.methods.toFloat(value)` where `value :: int | float | byte | string` -> `float | error.Method`
- `std.methods.toByte(value)` where `value :: int | float | byte | string` -> `byte | error.Method`

String parsing and range checks are guarded, returning `error.Method.InvalidNumber` or `error.Method.Overflow` instead of trapping.
