# Typed Unions

## What are unions

Unions are a special type which can represent one type or another. For instance, you can make a value that either returns a number, or a string. A union can only hold a single value, and therefore represent a single type when used.

## Syntax

Unions are declared using pipes:

```doxa
var x :: int | float
var y :: string | int | float
```

The pipe's only usage in Doxa is to specify a typed union.

## Examples

```doxa
// A union that can be either an integer or a float
var number :: int | float
number is 42     // Valid - integer
number is 3.14   // Valid - float

// A union that can be a string, integer, or float
var value :: string | int | float
value is "hello" // Valid - string
value is 100     // Valid - integer
value is 2.718   // Valid - float

// Unions default to the first type when not initialized
var defaulted :: int | float
defaulted?       // Prints 0 (default int value)

var float_first :: float | int
float_first?     // Prints 0.0 (default float value)
```

## Default Behavior

When a union is declared without initialization, it defaults to the **first type** in the union with its default value:

- `int` defaults to `0`
- `float` defaults to `0.0`
- `byte` defaults to `0x00`
- `string` defaults to `""`
- `tetra` defaults to `false`
- `nothing` defaults to `nothing`

This ensures that unions always have a well-defined state and the compiler knows which type is currently active.

## Optional Values

A common use case for unions is representing optional values using `nothing`:

```doxa
// Optional integer
var maybe_name :: string | nothing
maybe_name is "Alice"     // Has a value
maybe_name is nothing // No value

// Function that might not return a value
function findValue(arr :: MyObject[], target :: string) returns(int | nothing) {
    each x in arr {
        if (x.name equals target) then {
            return x
        }
    }
    return nothing // Not found
}
```

## Why unions are useful

Unions are particularly useful when:

- A function can return different types depending on the input
- A variable needs to hold different types at different times
- You want to represent optional values (like `T | nothing`)
- You're working with data that can have multiple valid representations

## Error Handling

Here is a robust example of how error handling can be done using Doxa. Note that none of this handling is specific to _errors_, it is a general return pattern which relies on normal values making semanitcs extremely clear and flexible. This relies on the `as` keyword which attempts to narrow a value into another type.

```doxa
enum ErrorList {
    Overflow,
    Underflow,
}

// Match function to process errors
function handleError(err :: ErrorList) {
    match err {
        .Overflow then doSomething(),
        .Underflow then doSomethingElse(),
    }
}

// Function returns a union - we won't know if the value is a number or one of our errors until we check
function addLimit(a :: int, b :: int) returns( int | ErrorList ) {
    const result is a + b
    if result > 255 then return ErrorList.Overflow
    if result < 0 then return ErrorList.Underflow
    return result
}

// All of these return unions
const unknownBigResult is addLimit(1000, 1000)   // ErrorList.Overflow
const unknownSmallResult is addLimit(100, -1000) // ErrorList.Underflow
const unknownRightResult is addLimit(100, -10)   // 90

// Handle errors - several approaches:

// Approach 1: Pattern matching (cleanest)
match unknownRightResult {
    int then onlyUsesInts(unknownRightResult),  // unknownRightResult is typed as int here
    ErrorList then handleError(unknownRightResult),  // unknownRightResult is typed as ErrorList here
}

// Approach 2: Explicit fallback with error handling
const myInt is unknownRightResult as int else {
    @panic("This should never happen in production!")
}
onlyUsesInts(myInt)

// Note type checking in control flow doesn't implicitly cast unlike match case which is exhaustive
// this can be done by narrowing within the branch but it is not as ergonomic as match case
if unknownRightResult @istype int then {
    onlyUsesInts(unknownRightResult) as int else{}
} else if unknownRightResult @istype ErrorList then {
    handleError(unknownRightResult) as ErrorList else{}
}
```

## Type Casting with `as`

The `as` keyword attempts to cast a union to a specific type. If the cast fails, the `else` block is executed:

```doxa
var value :: int | string
value is "hello"

// This will execute the else block since value is currently a string
const number is value as int else {
    return error.ExpectedInteger
}

// This will succeed since value is now an int
value is 42
const doubled is value as int else {
    return error.ExpectedInteger
} // doubled is 84
```

`as` can be used for control flow as well, using then blocks:

```
// using as like an 'istype' style conditional
asThenUnion as int then {
    var result is 20 + 30
    result?
} else {
    var result is 30 + 40
    result?
} // 50
```

### Common Patterns

**Safe extraction with default:**

```doxa
const result is someUnion as int else {
    return 0 // Default value if not an int
}
```

**Error handling:**

```doxa
const number is value as int else {
    @panic("This should never happen in production!")
}
```

**Conditional processing:**

```doxa
const processed is value as string else {
    // Handle non-string case
    return "default"
}
```

The `as` keyword is essential for safely working with unions when you need to extract a specific type.
