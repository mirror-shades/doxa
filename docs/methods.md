# Method Calls

Doxa supports two method-call families:

- Core intrinsics: `@...` calls implemented in core/compiler runtime.
- Standard library calls: module functions such as `std.io.input()`.

## Core Intrinsics

These are the core intrinsic calls currently documented for general use.

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

