# Built-in Methods

Doxa provides built-in methods prefixed with `@` for core operations. These methods are fundamental to the language and don't require imports.

## Categories

- Array Methods
- String Methods
- Math Methods
- Type Methods
- I/O Methods
- Control Flow Methods

## Method Documentation Format

Each method is documented with:

- Input: Parameters and their types
- Output: Return value and type
- Modifies: Any side effects
- Errors: Possible error conditions
- Example: Usage example

## Array Methods

### _length_

Input:

- x :: string | array // Type checked at compile time

Output:

- Integer length of the input

Modifies:

- No modifications

Example:

- var x = "hello"; @length(x) -> 5

---

### _push_

Input:

- source :: array // Type checked at compile time
- value :: any // Type checked at compile time

Output:

- nothing

Modifies:

- Adds value to end of source array

Runtime Errors:

- CapacityExceeded: if array would exceed maximum size
- OutOfMemory: if system cannot allocate memory

Example:

```doxa
var arr = [1, 2]
@push(arr, 3) else {
    @panic("Failed to add element")
}
// arr is now [1, 2, 3]
```

---

### _pop_

Input:

- source: array to modify

Output:

- Last element from array

Modifies:

- Removes last element from source array
- Original array is modified in place
- Array length decreases by 1

Errors:

- EmptyArray: if array has no elements

Notes:

- Modifies the original array
- Cannot be undone (use @copy first if needed)
- Returns the removed element

Example:

```doxa
var arr = [1, 2, 3]
var last = @pop(arr)     // last is 3
@length(arr) == 2        // array now has 2 elements
arr == [1, 2]            // original array is modified

// Save array before popping
var backup = @copy(arr)
var item = @pop(arr)    // only modifies arr, not backup
```

---

### _insert_

Input:

- source: array to modify
- index: position to insert at
- value: element to insert

Output:

- nothing

Modifies:

- Inserts value at index, shifts existing elements right

Errors:

- IndexOutOfBounds: if index < 0 or index > array length
- OutOfMemory: if array cannot be expanded

Example:

- var arr = [1, 2]; @insert(arr, 1, 5); // arr is now [1, 5, 2]

---

### _remove_

Input:

- source: array to modify
- index: position to remove from

Output:

- Removed element

Modifies:

- Removes element at index, shifts remaining elements left

Errors:

- IndexOutOfBounds: if index < 0 or index >= array length

Example:

- var arr = [1, 2, 3]; var removed = @remove(arr, 1); // removed is 2, arr is [1, 3]

---

### _clear_

Input:

- source: array to clear

Output:

- nothing

Modifies:

- Removes all elements from array

Runtime Errors:

- None

Example:

- var arr = [1, 2, 3]; @clear(arr); // arr is now []

---

### _index_

Input:

- source: array or string to search
- value: element to find

Output:

- Index of first occurrence, or -1 if not found

Modifies:

- No modifications

Runtime Errors:

- None or string

Example:

- var arr = [1, 2, 3]; var idx = @index(arr, 2); // idx is 1

---

### _clone_

Input:

- value :: array | string | map

Output:

- Deep copy of the input value (recursively copies all nested structures)

Modifies:

- No modifications

Runtime Errors:

- None

Notes:

- Creates completely independent copy
- Recursively copies all nested arrays and maps
- Nested values have no connection to original
- Safe for circular references

Example:

```doxa
var nested = [1, [2, 3], {"x": [4, 5]}]
var deep = @clone(nested)
deep[1][0] = 9        // modifies copy only
nested[1][0] == 2     // original unchanged
```

---

### _copy_

Input:

- value :: array | string | map

Output:

- Shallow copy of the input value (copies only top-level structure)

Modifies:

- No modifications

Runtime Errors:

- None

Notes:

- Creates new top-level container only
- Nested structures are shared with original
- Modifying nested values affects both copies
- Faster than deep copy for simple structures

Example:

```doxa
var nested = [1, [2, 3]]
var shallow = @copy(nested)
shallow[0] = 9        // modifies copy only
shallow[1][0] = 9     // modifies both copies!
nested[1][0] == 9     // shared nested array
```

## String Methods

### _split_

Input:

- source :: string
- delimiter :: string

Output:

- Array of strings split by delimiter

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = "a,b,c"; var parts = @split(str, ","); // parts is ["a", "b", "c"]

---

### _join_

Input:

- source :: array
- delimiter :: string

Output:

- String with array elements joined by delimiter

Modifies:

- No modifications

Runtime Errors:

- None or delimiter is not a string

Example:

- var arr = ["a", "b", "c"]; var str = @join(arr, ","); // str is "a,b,c"

---

### _slice_

Input:

- source: array or string to slice
- start: starting index (inclusive)
- end: ending index (exclusive)

Output:

- New array or string containing elements from start to end

Modifies:

- No modifications

Runtime Errors:

- None or string
- IndexOutOfBounds: if start < 0 or end > length
- InvalidArgument: if start > end

Example:

- var arr = [1, 2, 3, 4]; var sliced = @slice(arr, 1, 3); // sliced is [2, 3]

---

### _bytes_

Input:

- value: string to convert

Output:

- Array of byte values

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = "ABC"; var bytes = @bytes(str); // bytes is [65, 66, 67]

---

### _trim_

Input:

- value :: string

Output:

- String with leading and trailing whitespace removed

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = " hello "; var trimmed = @trim(str); // trimmed is "hello"

---

### _lower_

Input:

- value :: string

Output:

- String converted to lowercase

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = "Hello"; var lower = @lower(str); // lower is "hello"

---

### _upper_

Input:

- value :: string

Output:

- String converted to uppercase

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = "Hello"; var upper = @upper(str); // upper is "HELLO"

## Math Methods

### _abs_

Input:

- value :: int | float

Output:

- Absolute value of the input

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var x = -5; var abs = @abs(x); // abs is 5

---

### _min_

Input:

- a :: int | float
- b :: int | float

Output:

- Smaller of the two values

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var min = @min(5, 3); // min is 3

---

### _max_

Input:

- a :: int | float
- b :: int | float

Output:

- Larger of the two values

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var max = @max(5, 3); // max is 5

---

### _round_

Input:

- value :: float

Output:

- Rounded integer value

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var x = 3.7; var rounded = @round(x); // rounded is 4

---

### _floor_

Input:

- value :: float

Output:

- Largest integer less than or equal to value

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var x = 3.7; var floor = @floor(x); // floor is 3

---

### _ceil_

Input:

- value :: float

Output:

- Smallest integer greater than or equal to value

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var x = 3.2; var ceil = @ceil(x); // ceil is 4

## Type Methods

### _string_

Input:

- value: any value to convert

Output:

- String representation of value

Modifies:

- No modifications

Errors:

- None (all values can be converted to string)

Example:

- var num = 123; var str = @string(num); // str is "123"

---

### _int_

Input:

- value :: string // Type checked at compile time

Output:

- Integer value | NumberParseError

Modifies:

- No modifications

Runtime Errors:

- ParseFailed: if string is not a valid number format
- Overflow: if number is too large for int type
- Underflow: if number is too small for int type

Example:

```doxa
// Basic parsing
var str = "123"
var num = @int(str) else {
    @panic("Not a valid number")
}

// Handle specific errors
var result = @int("-999999999999") match {
    int then it,                    // Successfully parsed
    ParseFailed then 0,            // Not a number
    Overflow then MaxInt,          // Too large
    Underflow then MinInt,         // Too small
    else => 0
}
```

---

### _float_

Input:

- value: string to convert

Output:

- Float value, or 0.0 if conversion fails

Modifies:

- No modifications

Runtime Errors:

- None

Example:

- var str = "123.45"; var num = @float(str); // num is 123.45

---

### _type_

Input:

- value: any value to check

Output:

- String name of value's type

Modifies:

- No modifications

Errors:

- None

Example:

- var arr = [1, 2]; var type = @type(arr); // type is "array"

## I/O Methods

### _input_

Input:

- none

Output:

- string containing the line read from standard input
- empty string ("") on EOF or if no data is available

Modifies:

- Blocks until a line is available or EOF is reached

Errors:

- IOError: if input stream fails (e.g., terminal disconnected)

Notes:

- Blocks execution until a complete line is available
- Line includes all characters up to but not including the newline
- Returns empty string on EOF rather than raising an error
- Use in loop to read all lines: `while (line = @input()) != "" { ... }`

Example:

```doxa
// Basic input
var name = @input() // blocks until line available

// Reading all lines
var lines = []
var line = ""
while (line = @input()) != "" {
    @push(lines, line)
}
```

---

### _read_

Input:

- path :: string - Path to file to read

Output:

- string containing file contents

Modifies:

- No modifications

Errors:

- IOError: if file cannot be read
- PathError: if path is invalid

Example:

- var content is @read("config.txt");

---

### _write_

Input:

- path :: string - Path to file to write
- content :: string - Content to write

Output:

- tetra indicating success (true) or failure (false)

Modifies:

- Creates or overwrites file at path

Errors:

- IOError: if file cannot be written
- PathError: if path is invalid

Example:

- var success is @write("log.txt", "Hello");

---

### Binary I/O and Encoding

Doxa handles binary data through composition of string and byte array operations:

Reading binary files:

```doxa
// Read file as raw bytes
var raw = @read("image.png")    // reads file as-is, no encoding
var bytes = @bytes(raw)         // convert to byte array
var firstByte = bytes[0]        // access individual bytes

// Text file with specific encoding
var text = @read("text.txt")    // reads as UTF-8 by default
var utf8Bytes = @bytes(text)    // get UTF-8 byte representation
```

Writing binary files:

```doxa
// Write raw binary data
var bytes = [0x89, 0x50, 0x4E, 0x47]   // PNG header bytes
var raw = @string(bytes)               // convert to raw string (preserves bytes)
var success = @write("file.png", raw)  // writes bytes as-is

// Write text with encoding
var text = "Hello, 世界"                 // UTF-8 text
var success = @write("hello.txt", text) // writes as UTF-8
```

Notes on Binary Data:

- `@read` reads file contents as-is, preserving all bytes
- `@write` writes raw bytes without modification
- `@bytes` converts string to its underlying bytes
- `@string` with byte array preserves raw bytes (no encoding/decoding)
- Text files use UTF-8 encoding by default
- No data loss occurs when converting between bytes and raw strings

---

### _exec_

Input:

- command :: string - Command to execute

Output:

- string containing command output (stdout and stderr combined)

Modifies:

- May modify system state depending on command
- Blocks until command completes

Errors:

- ExecError: if command fails to execute
- SecurityError: if command is not allowed

Notes:

- Captures both stdout and stderr in output
- Blocks until command completes
- Returns all output as UTF-8 text
- Command runs with same permissions as Doxa program
- Use @spawn for non-blocking execution

Example:

```doxa
// Basic usage
var status = @exec("git status")

// Handle command failure
var result = @exec("git push") else {
    @panic("Git push failed")
}

// Capture and process output
var files = @exec("ls")
var count = @length(@split(files, "\n"))
```

---

### _spawn_

Input:

- command :: string - Command to execute asynchronously

Output:

- tetra indicating if command started successfully (true)
- does NOT indicate process completion or success

Modifies:

- May modify system state depending on command
- Process continues running after function returns
- No way to get process output or result
- Process may outlive the Doxa program

Errors:

- ExecError: if command fails to start
- SecurityError: if command is not allowed

Notes:

- Only indicates if process started, not if it succeeded
- Use @exec if you need the command's output
- Spawned process runs independently
- No built-in way to check process status
- Useful for long-running background tasks

Example:

```doxa
// Start a long process
var started = @spawn("long_process")
@assert(started, "Process failed to start")

// Process runs in background while main program continues
@print("Main program continues...")
```

## Control Flow Methods

### _assert_

Input:

- condition: tetra expression to check
- message: error message if condition is false

Output:

- nothing if condition is true
- panics with message if condition is false

Modifies:

- No modifications

Runtime Errors:

- Panic: if condition is false

Example:

- var x = 5; @assert(x > 0, "x must be positive");

---

### _panic_

Input:

- message: error message

Output:

- Halts program with error message

Modifies:

- Stops program execution

Runtime Errors:

- None

Example:

- @panic("Fatal error occurred");

## Method Naming Conventions

1. Case:

   - All methods use lowercase
   - No camelCase or PascalCase

2. Type Conversions:

   - Direct names: @string, @int, @float
   - No 'to' prefix

3. Collections:

   - Array operations use verb forms: @push, @pop, @remove
   - Queries use established terms: @length, @index

4. Control Flow:

   - Simple, lowercase forms: @assert, @panic

5. I/O Operations:
   - Simple verbs: @read, @write, @exec
     - All methods are single words
     - Binary I/O uses composition: @bytes and @string with @read/@write

## Error Handling

Doxa uses a systematic approach to error handling through typed error unions:

### Core Error Types

```doxa
// I/O related errors
enum IOError {
    FileNotFound,      // File does not exist
    PermissionDenied,  // Insufficient permissions
    DeviceError,       // Hardware/device failure
    Interrupted,       // Operation interrupted
    UnexpectedEOF,     // Premature end of file
}

// Path and filesystem errors
enum PathError {
    InvalidPath,      // Malformed path
    NotFound,         // Path does not exist
    AlreadyExists,    // Path already exists
    IsDirectory,      // Expected file, got directory
    NotDirectory,     // Expected directory, got file
}

// Runtime value error
enum ValueError {
    // Bounds and range errors
    IndexOutOfRange,  // Array/string index outside valid range
    EmptyCollection,  // Operation requires non-empty collection
    InvalidRange,     // Invalid range specification (e.g., start > end)

    // Numeric errors
    Overflow,         // Number exceeds type capacity
    Underflow,        // Number below minimum value
    DivideByZero,    // Division by zero

    // Parse/format errors
    ParseFailed,      // String-to-number parse failed (e.g., "abc" as int)
    InvalidFormat,    // Invalid data format (e.g., malformed JSON)
    InvalidEncoding,  // Invalid string encoding (e.g., invalid UTF-8)

    // Collection errors
    CapacityExceeded, // Collection would exceed max capacity
    DuplicateKey,     // Duplicate key in map/set
    KeyNotFound       // Key not found in map/set
}

// Resource errors
enum ResourceError {
    OutOfMemory,      // Memory allocation failed
    TooLarge,         // Resource exceeds limits
    Exhausted,        // Resource pool depleted
}
```

### Method Error Types

Methods combine relevant errors into union types:

```doxa
// Define specific error combinations for methods
// Method-specific error types (runtime errors only)
alias ArrayOpError =
    | ValueError.EmptyCollection  // Array is empty
    | ValueError.IndexOutOfRange  // Index out of bounds
    | ValueError.CapacityExceeded // Can't grow array further
    | ResourceError.OutOfMemory  // System out of memory

alias NumberParseError =
    | ValueError.ParseFailed  // Invalid number format
    | ValueError.Overflow     // Number too large
    | ValueError.Underflow   // Number too small

alias ReadError =
    | IOError.FileNotFound
    | IOError.PermissionDenied
    | PathError.InvalidPath

alias WriteError =
    | IOError.PermissionDenied
    | PathError.InvalidPath
    | ResourceError.OutOfMemory

// Method signatures include error unions
function @read(path :: string) returns(string | ReadError)
function @write(path :: string, content :: string) returns(tetra | WriteError)
```

### Error Handling Examples

```doxa
// Using else for error handling
var content = @read("config.txt") as string else {
    @panic("Failed to read config")
}

// Pattern matching on specific errors
var result = @write("log.txt", "Hello") match {
    true then "Success",
    PermissionDenied then "Permission denied",
    InvalidPath then "Invalid path",
    else => "Other error"
}

// Composing error handling
function readConfig() returns(string | ParseError) {
    var raw = @read("config.txt")
    var rawString is raw as string else ""
    return @parse(rawString)
}
```

### Benefits

1. **Type Safety**: Errors are typed and checked at compile time
2. **Composability**: Error types can be combined and transformed
3. **Clarity**: Methods document their exact error cases
4. **Consistency**: Common errors are reused across methods
5. **Flexibility**: New error types can be added without breaking changes

### Error Categories

- **IOError**: Input/output operations
- **PathError**: File system paths

- **ResourceError**: System resource management
- **SecurityError**: Permission and security
- **ParseError**: Data parsing and validation
- **ExecError**: Process execution
- **NetworkError**: Network operations

Each built-in method documents its specific error types in its signature and documentation.
