# Built-in Methods

Doxa provides built-in methods prefixed with `@` for core operations.

### Methods That Should Panic or Halt on Failure

| Method | Return  | Panics On                        |
| ------ | ------- | -------------------------------- |
| PUSH   | nothing | Array/string corruption if fails |
| POP    | any     | Underflow if empty               |
| INSERT | nothing | Array/string corruption if fails |
| REMOVE | any     | Out of bounds index              |
| ASSERT | nothing | False condition                  |
| PANIC  | never   | Always halts                     |
| EXIT   | never   | Always terminates                |

### Methods That Should Return Error Unions

| Method  | Return                                                            |
| ------- | ----------------------------------------------------------------- |
| READ    | string \| IOError - file contents or error                        |
| WRITE   | nothing \| IOError - writes to file or error                      |
| INPUT   | string \| IOError - line from stdin or EOF error                  |
| TOINT   | int \| ParseError - parsed integer or parse error                 |
| TOFLOAT | float \| ParseError - parsed float or parse error                 |
| TOBYTE  | byte \| ParseError - byte representation or conversion error      |
| SLICE   | array \| IndexError or string \| IndexError - based on input type |
| SLEEP   | nothing \| SysError - sleeps or interruption error                |
| PRINT   | nothing \| IOError - prints to stdout or I/O error                |
| SPAWN   | int \| SysError - PID of spawned process or error                 |
| KILL    | nothing \| SysError - kills process or error                      |
| WAIT    | int \| Nothing \| SysError - exit code or error                   |

### No Failure

| Method   | Return |
| -------- | ------ |
| TOSTRING | string |
| TYPE     | int    |
| LENGTH   | int    |
| OS       | string |
| ARCH     | string |
| TIME     | int    |
| RANDOM   | float  |
| ALIVE    | tetra  |

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
- **Output**: `string OR array | ValueError` - substring/subarray
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
- **Output**: `int | ValueError` - parsed integer
- **Example**: `@int("123")` → `123`

### @float

- **Input**: `int | byte | string`
- **Output**: `float | ValueError` - parsed float
- **Example**: `@float("123.45")` → `123.45`

### @byte

- **Input**: `int | float | string`
- **Output**: `byte | ValueError` - byte representation
- **Example**: `@byte(10)` → `0x0A`

### @type

- **Input**: `any`
- **Output**: `string` - type name
- **Example**: `@type([1,2])` → `"array"`

### @time

- **Input**: `none`
- **Output**: `int` - current timestamp, -1 sentinel on rare runtime error
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
