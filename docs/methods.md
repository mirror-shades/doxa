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

- `@length(value :: string | array)` -> `int`
- `@push(collection :: string | array, value :: any)` -> `nothing`
- `@pop(collection :: string | array)` -> `any`
- `@insert(collection :: string | array, index :: int, value :: any)` -> `nothing`
- `@remove(collection :: string | array, index :: int)` -> `any`
- `@clear(collection :: string | array)` -> `nothing`
- `@find(collection :: string | array, value :: any)` -> `int`
- `@slice(collection :: string | array, start :: int, length :: int)` -> `string | array`

### Type / Conversion

- `@string(value :: any)` -> `string`
- `@int(value :: float | byte | string)` -> `int`
- `@float(value :: int | byte | string)` -> `float`
- `@byte(value :: int | float | string)` -> `byte`
- `@type(value :: any)` -> `string`

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

- `@print(format :: string)`
  - Argument must be a `string`. `@print` writes that string as-is; it does not interpolate or interpret `{...}`. Interpolation happens when a string value is produced (for example, in a double-quoted literal), so `@print("value is {value}")` still prints the current value because the literal is evaluated to a `string` before `@print` runs.
- `@assert(condition :: tetra, message :: string?)`
  - False condition halts execution and reports assertion failure location/message.
- `@panic(message)`
  - Always terminates execution with a runtime panic.
- `@exit(code)`
  - Terminates process with the provided status code (`int | byte`).