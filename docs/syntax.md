# Doxa Language Reference

## Modes of Operation

Doxa supports three operational modes:

- **Normal Mode** (default): Dynamic typing with flexible syntax
- **Safe Mode**: Enabled via `#safe` at file start, enforces static typing
- **Warn Mode**: Compiles in normal mode but warns about safe convention violations

!!! note
Safe files can only import other safe files, while normal files can import both.

## Core Language Features

### Syntax Alternatives

Doxa provides alternative syntax for common operations. Using mixed styles in the same file triggers warnings.

| Primary    | Alternative | Purpose              |
| ---------- | ----------- | -------------------- |
| `and`      | `&&`        | Logical AND          |
| `or`       | `\|\|`      | Logical OR           |
| `equals`   | `==`        | Equality             |
| `where`    | `\|`        | Filter conditions    |
| `function` | `fn`        | Function declaration |

### Type System

- No operator or function overloading
- No classes, only structs and composition
- Garbage collected memory management

### Basic Composition Example

```doxa
struct Animal {
    name: string
}

struct Dog {
    // Composition instead of inheritance
    animal: Animal,
    breed: string,

    fn bark(self) {
        print("${self.animal.name} says woof!");
    }
}

var dog = Dog {
    animal: Animal { name: "Spot" },
    breed: "Labrador"
};
```

### Modules

```doxa
// math.doxa
#safe
export fn add(a: int, b: int) -> int {
    return a + b;
}

// main.doxa
import { add } from "./math.doxa";
var sum = add(1, 2);
```

## Data Types

### Arrays

Arrays are homogeneous collections with type inference:

```doxa
var nums = [1, 2, 3];             // Inferred as int[]
var strs = ["a", "b"];            // Inferred as string[]
var explicit: int[] = [1, 2, 3];  // Explicit typing

// Invalid operations
var mixed = [1, "two", true];     // Error: mixed types
nums.push("four");                // Error: type mismatch
```

### Tuples

Fixed-size collections supporting different types:

```doxa
var point = (10, 20, 30);         // Simple tuple
var nested = ((1, 2), (3, 4));    // Nested tuple

point[0];                         // Access first element
nested[1][0];                     // Access nested element
```

### Maps

String-keyed dictionaries:

```doxa
var scores = {
    "alice": 100,
    "bob": 85
};
scores["alice"];                  // Access value
```

## Control Flow

### Pattern Matching

```doxa
enum Status { Success, Error, Pending }

var result = match status {
    .Success => "all good",
    .Error => "failed",
    else => "waiting"
};
```

!!! warning
Match expressions must be exhaustive or include an `else` clause.

### Error Handling

```doxa
try {
    riskyOperation();
} catch {
    handleError();
}
```

## Special Operators

### Inspect (`?`)

```doxa
var x = computeValue();
x?;                              // Prints value with location
```

### Type Information

```doxa
typeof(42);                      // "int"
typeof("hello");                // "string"
typeof([1,2,3]);               // "array"
```

### Collection Quantifiers

```doxa
exists x in numbers where x > 10  // Any match
forall x in numbers | x > 0      // All match
```

## Type System Details

### Normal Mode

Variables are dynamically typed by default:

```doxa
var x = 1;                      // int
x = true;                       // bool (allowed)

var y: auto = 3.14;            // float
y = "pi";                      // string (allowed)
```

### Safe Mode

Variables require explicit typing:

```doxa
var x: int;                    // Valid declaration
var x = "two";                // Error: needs type
var x: auto = 3.14;           // Type locked to float
x = "five";                   // Error: type mismatch
```

Explicit return type declarations are required:

```doxa
// Safe Mode - Valid
fn greet(name: string) -> string {
    return "Hello ${name}!";
}

// Safe Mode - Error: missing return type
fn greet(name: string) {
    return "Hello ${name}!";
}
```

## Conditional Expressions

All conditionals are expressions and return values:

```doxa
var result = if condition then {
    value1
} else {
    value2
};
```

!!! note
Expressions without a value return `nothing`:
`doxa
    var x = if (false) { y = 1 };  // x becomes nothing
    `

### Function Return Types

Functions can specify return types using either `->` or `returns` syntax:

```doxa
fn add(a: int, b: int) -> int {
    return a + b;
}

// Alternative syntax
fn add(a: int, b: int) returns(int) {
    return a + b;
}
```

return types are optional and inferred by default:

```doxa
// Normal Mode - Valid
fn add(a, b) {
    return a + b;
}
```
