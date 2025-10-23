# Built-in Methods

Doxa provides built-in methods prefixed with `@` for core operations.

### Policy: Intrinsics Panic; Stdlib Provides Fallible APIs

Doxa’s built-in compiler/Vm intrinsics (the `@...` methods) prioritize ergonomics and performance. They panic on failure conditions. The standard library mirrors these with fallible wrappers that return typed unions for explicit error handling.

Guidelines:
- Intrinsics: fast, concise, panic-on-fail. Use for scripts/happy paths.
- Stdlib: fallible variants returning unions (e.g., `byte | ValueError`, `string | IOError`). Use when robustness is required.

Note: The standard library is not implemented yet. Names and signatures referenced here describe the intended design and may change. This section lists intrinsic behavior; corresponding stdlib functions will return unions with similar names (e.g., `std.conv.byte`, `std.io.input`).


### No Failure

| Method   | Return |
| -------- | ------ |
| STRING   | string | // KEEP: Core type conversion
| TYPE     | string | // KEEP: Simple type introspection
| LENGTH   | int    | // KEEP: Simple array/string length
| OS       | string | // KEEP: Simple compiler intrinsic
| ARCH     | string | // KEEP: Simple compiler intrinsic  
| ABI      | string | // KEEP: Simple compiler intrinsic
| TIME     | int    | // KEEP: Simple compiler intrinsic
| TICK     | int    | // KEEP: Simple compiler intrinsic
| RANDOM   | float  | // KEEP: Simple compiler intrinsic
| ALIVE    | tetra  | // REMOVE: Process management in stdlib

### Methods Can Panic
| Method  | Return       | Panics On               |
| ------- | ------------ | ----------------------- |
| PUSH    | nothing      | Array/string corruption | // KEEP: Core array operation
| POP     | any          | Underflow if empty      | // KEEP: Core array operation
| INSERT  | nothing      | Array/string corruption | // KEEP: Core array operation
| REMOVE  | any          | Out of bounds index     | // KEEP: Core array operation
| ASSERT  | nothing      | False condition         | // KEEP: Simple assertion
| PANIC   | never        | Always halts            | // KEEP: Simple panic
| EXIT    | never        | Always terminates       | // REMOVE: Use std.process.exit
| READ    | string       | I/O failure             | // REMOVE: Use std.fs.cwd().readFileAlloc
| WRITE   | nothing      | I/O failure             | // REMOVE: Use std.fs.cwd().writeFile
| INPUT   | string       | I/O/EOF failure         | // REMOVE: Use std.io.getStdIn().readUntilDelimiterAlloc
| INT     | int          | Parse/overflow          | // KEEP: Core type conversion
| FLOAT   | float        | Parse                   | // KEEP: Core type conversion
| BYTE    | byte         | Parse/overflow          | // KEEP: Core type conversion
| SLICE   | string/array | Index/invalid range     | // KEEP: Core array/string operation
| SLEEP   | nothing      | Sys failure             | // REMOVE: Use std.time.sleep
| PRINT   | nothing      | I/O failure             | // REMOVE: Use std.debug.print or std.io.getStdOut().writer()
| SPAWN   | int          | Sys failure             | // REMOVE: Use std.process.Child.spawn
| KILL    | nothing      | Sys failure             | // REMOVE: Use std.process.Child.kill
| WAIT    | int          | Sys failure             | // REMOVE: Use std.process.Child.wait
| BUILD   | nothing      | Compilation failure     | // KEEP: Core compiler functionality 

## Array Methods

### @length

- **Input**: `string | array`
- **Output**: `int` - length of input
- **Example**: `@length("hello")` → `5`

### @push

- **Input**: `array | string`, `value`
- **Output**: `nothing` - modifies array/string in place by adding value to end
- **Example**: `@push(arr, 3)` // adds 3 to arr, returns nothing

### @pop

- **Input**: `array | string`
- **Output**: `any` - removes and returns last element
- **Example**: `var last = @pop(arr)`

### @insert

- **Input**: `array | string`, `index`, `value`
- **Output**: `nothing` - inserts value at index
- **Example**: `@insert(arr, 1, 5)` // inserts 5 at index 1

### @remove

- **Input**: `array | string`, `index`
- **Output**: `any` - removes and returns element at index
- **Example**: `var removed = @remove(arr, 1)`

### @clear

- **Input**: `array | string`
- **Output**: `nothing` - removes all elements
- **Example**: `@clear(arr)` // arr becomes []

### @find

- **Input**: `array | string`, `value`
- **Output**: `int` - index of first occurrence, -1 if not found
- **Example**: `@find([1,2,3], 2)` → `1`

### @slice

- **Input**: `string | array`, `int` (start), `int` (length)
- Intrinsic Output: `string OR array` (panics on invalid range)
- Stdlib Output: `string OR array | IndexError`
- **Example**: `@slice("hello", 1, 3)` → `"ell"`

### @copy

- **Input**: `array | string | map`
- **Output**: `same type` - shallow copy
- **Example**: `var copy = @copy(original)`

## Type Methods

### @string

- **Input**: `any`
- **Output**: `string` - string representation
- **Example**: `@string(123)` → `"123"`

### @int

- **Input**: `float | byte | string`
- Intrinsic Output: `int` (panics on parse/overflow)
- Stdlib Output: `int | ValueError`
- **Example**: `@int("123")` → `123`

### @float

- **Input**: `int | byte | string`
- Intrinsic Output: `float` (panics on parse)
- Stdlib Output: `float | ValueError`
- **Example**: `@float("123.45")` → `123.45`

### @byte

- **Input**: `int | float | string`
- Intrinsic Output: `byte` (panics on parse/overflow)
- Stdlib Output: `byte | ValueError`
- **Example**: `@byte(10)` → `0x0A`

### @type

- **Input**: `any`
- **Output**: `string` - type name
- **Example**: `@type([1,2])` → `"array"`

### @time

- **Input**: `none`
- Output: `int` - current timestamp (intrinsic may panic on severe runtime error)
- **Example**: `var now = @time()`

### @random

- **Input**: `none`
- **Output**: `int` - random number, compile-time error if not supported
- **Example**: `var num = @random()`

## Process Methods

### @spawn

- **Input**: `string[]` (command and arguments)
- **Output**: `int | SysError` - PID of spawned process, or error
- **Example**: `var pid = @spawn(["my_program", "arg1"])`

### @kill

- **Input**: `int` (PID)
- **Output**: `nothing | SysError` - kills process, or error
- **Example**: `@kill(pid)`

### @wait

- **Input**: `int` (PID)
- **Output**: `int | Nothing | WaitError` - exit code, or error
- **Example**: `var exitCode = @wait(pid)`

### @alive

- **Input**: `int` (PID)
- **Output**: `

```
enum ParseError {        // type conversion failures
    InvalidFormat,       // e.g., "abc" -> int
    Overflow,            // too large for target type
    Underflow,           // too small for target type
}

enum IndexError {        // array/string/collection bounds issues
    OutOfBounds,         // index >= length
    Underflow,           // pop/remove from empty
    Empty,               // access empty collection
    InvalidRange         // invalid slice/range
}

enum IOError {           // filesystem / I/O issues
    EOF,                 // end-of-file / input exhausted
    NotFound,            // file missing
    PermissionDenied,    // insufficient permissions
    Failed               // read or write failed
}

enum SysError {          // system call failures
    NotFound,            // missing process/resource
    PermissionDenied,    // lack of privileges
    Timeout,             // operation timed out
    Failed               // generic system failure
}

// enum name reserved for future builtins
enum NetworkError {
    Unreachable,      // host/server cannot be reached
    Timeout,          // request timed out
    PermissionDenied, // lack of privileges to open socket/port
    Failed            // generic network failure
}
```
