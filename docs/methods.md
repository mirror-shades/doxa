# Built-in Methods

Doxa provides built-in methods prefixed with `@` for core operations.

## Array Methods

### @length

- **Input**: `string | array`
- **Output**: `int` - length of input
- **Example**: `@length("hello")` → `5`

### @push

- **Input**: `array`, `value`
- **Output**: `void` - adds value to end of array
- **Example**: `@push(arr, 3)` // adds 3 to arr

### @pop

- **Input**: `array`
- **Output**: `any` - removes and returns last element
- **Example**: `var last = @pop(arr)`

### @insert

- **Input**: `array`, `index`, `value`
- **Output**: `void` - inserts value at index
- **Example**: `@insert(arr, 1, 5)` // inserts 5 at index 1

### @remove

- **Input**: `array`, `index`
- **Output**: `any` - removes and returns element at index
- **Example**: `var removed = @remove(arr, 1)`

### @clear

- **Input**: `array`
- **Output**: `void` - removes all elements
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

## I/O Methods

### @input

- **Input**: `none`
- **Output**: `string | IOError` - line from stdin, IOError.EOF on EOF
- **Example**: `var line = @input()`

### @syscall

- **Input**: `string, string[]` (command)
- **Output**: `int | string | nothing | SysError` – result of the system call or error
- **Example**:
  var output = @syscall("exec", ["ls", "-l"])
  var pid = @syscall("spawn", ["my_program", "arg1"])

## Control Flow Methods

### @assert

- **Input**: `bool`, `string` (message)
- **Output**: `void` - panics if false
- **Example**: `@assert(x > 0, "x must be positive")`

### @panic

- **Input**: `string` (message)
- **Output**: `never` - halts program
- **Example**: `@panic("Fatal error")`

## Error Handling

Methods return error unions that can be handled with `else` or `match`:

```doxa
// Using else
var content = @read("file.txt") else @panic("Read failed")

// Using match for @input EOF handling
const userInput is @input()
var line is userInput match {
    string then it, // actual input line
    IOError.EOF then "", // handle EOF explicitly
    else else @panic("Input error")
}

// Using match for type conversion
const maybeInt is "123"
var result is maybeInt match {
    int then maybeInt, // narrowed to int
    ValueError.ParseFailed then 0,
    else else -1
}
```

Common error types:

- `ValueError`: Array bounds, invalid inputs, overflow
- `IOError`: File operations
- `SysError`: System call failures

```doxa
enum ValueError {          // for invalid inputs, array issues, overflow
    OutOfBounds,
    ParseFailed,
    Overflow
}

enum IOError {             // for filesystem / I/O issues
    EOF,
    NotFound,
    ReadFailed,
    WriteFailed,
    PermissionDenied
}

enum SysError {            // for @syscall / OS errors
    Failed,
    NotFound,
    Timeout
}
```
