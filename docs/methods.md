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
| STRING   | string |
| TYPE     | string |
| LENGTH   | int    |
| OS       | string |
| ARCH     | string |
| ABI      | string |
| TIME     | int    |
| TICK     | int    |
| RANDOM   | float  |

### Methods Can Panic
| Method  | Return       | Panics On                    |
| ------- | ------------ | ---------------------------- |
| PUSH    | nothing      | Array/string corruption      |
| POP     | any          | Underflow if empty           |
| INSERT  | nothing      | Array/string corruption      |
| REMOVE  | any          | Out of bounds index          |
| CLEAR   | nothing      | Collection corruption        |
| FIND    | int          | Collection corruption        |
| SLICE   | string/array | Index/invalid range          |
| INT     | int          | Parse/overflow               |
| FLOAT   | float        | Parse                        |
| BYTE    | byte         | Parse/overflow               |
| ASSERT  | nothing      | False condition              |
| PANIC   | never        | Always halts                 |
| EXIT    | never        | Always terminates            |
| INPUT   | string       | I/O/EOF failure              |
| SLEEP   | nothing      | Sys failure                  |
| PRINT   | nothing      | I/O failure                  |
| BUILD   | int          | — (returns non-zero exit code on failure) |

## Array Methods

Mutable intrinsics require dynamic storage. Passing a fixed-size annotation (`int[4]`) or a `const` literal (including any const alias that references it) results in a compile-time `E6018` error explaining that the array must migrate to dynamic storage first.

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

### @random

- **Input**: `none`
- **Output**: `float` - random number between 0.0 and 1.0, compile-time error if not supported
- **Example**: `var num = @random()` // returns float like 0.123456

## System Methods

### @os

- **Input**: `none`
- **Output**: `string` - operating system name
- **Example**: `@os()` → `"windows"` or `"linux"`

### @arch

- **Input**: `none`
- **Output**: `string` - CPU architecture name
- **Example**: `@arch()` → `"x86_64"` or `"aarch64"`

### @abi

- **Input**: `none`
- **Output**: `string` - ABI (Application Binary Interface) name
- **Example**: `@abi()` → `"gnu"` or `"msvc"`

### @time

- **Input**: `none`
- **Output**: `int` - current Unix timestamp in seconds
- **Example**: `var now = @time()` // seconds since Unix epoch

### @tick

- **Input**: `none`
- **Output**: `int` - monotonic high-resolution timestamp in nanoseconds
- **Example**: `var start = @tick()` // for performance measurement

## I/O Methods

### @print

- **Input**: `string`
- **Output**: `nothing` - writes string to stdout followed by newline
- **Example**: `@print("Hello, World!")`

### @input

- **Input**: `none`
- **Output**: `string` - reads line from stdin (including newline)
- **Example**: `var name = @input()` // waits for user input

## Control Flow Methods

### @assert

- **Input**: `bool`, `string?` (optional message)
- **Output**: `nothing` - panics if condition is false
- **Example**: `@assert(x > 0, "x must be positive")`

### @panic

- **Input**: `string`
- **Output**: `never` - immediately terminates with message
- **Example**: `@panic("Something went terribly wrong")`

### @exit

- **Input**: `int` (optional, defaults to 0)
- **Output**: `never` - terminates program with exit code
- **Example**: `@exit(1)` // exit with error code

### @sleep

- **Input**: `int` (milliseconds)
- **Output**: `nothing` - pauses execution
- **Example**: `@sleep(1000)` // sleep for 1 second

## Utility Methods

### @build

- **Input**: `string` (source path), `string` (output path), `string` (arch), `string` (os), `abi` (abi), `tetra` (debug)
- **Output**: `int` - exit code from compilation (0 = success)
- **Example**: `@build("main.doxa", "output.exe", "x86_64", "windows", "gnu", false)`

---

**Total Methods: 26**

**Array/Collection Methods (8)**: @length, @push, @pop, @insert, @remove, @clear, @find, @slice

**Type Conversion Methods (5)**: @string, @int, @float, @byte, @type

**System Methods (5)**: @os, @arch, @abi, @time, @tick

**I/O Methods (2)**: @print, @input

**Control Flow Methods (4)**: @assert, @panic, @exit, @sleep

**Utility Methods (2)**: @random, @build
