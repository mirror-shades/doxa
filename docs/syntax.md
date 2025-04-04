# Doxa Language Reference

## Modes of Operation

Doxa supports three operational modes:

- **Normal Mode** (default): Dynamic typing with flexible syntax
- **Safe Mode**: Enabled via `#safe` at file start, enforces static typing
- **Warn Mode**: Compiles in normal mode but warns about safe convention violations

!!! note
Safe files can only import other safe files, while normal files can import both.

## Core Language Features

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

var dog is Dog {
    animal: Animal { name: "Spot" },
    breed: "Labrador"
};
```

### Modules

```doxa
// math.doxa
#safe
export fn add(a :: int, b :: int) -> int {
    return a + b;
}

// main.doxa
import { add } from "./math.doxa";
var sum is add(1, 2);
```

## Data Types

### Basic Types

Standard types include:

- `int`: Integer numbers
- `float`: Floating point numbers
- `string`: Text strings
- `bool`: Boolean true/false
- `tetra`: Four-valued logic system

The `tetra` type represents a four-valued logic system with these possible values:

- `true`: Logical truth
- `false`: Logical false
- `both`: Both true and false
- `neither`: Neither true nor false

Example:

```doxa
var logic :: tetra is both;
var uncertain :: tetra is neither;
```

### Arrays

Arrays are homogeneous collections with type inference:

```doxa
var nums :: int[] is [1, 2, 3];    // Explicit typing
var strs is ["a", "b"];            // Inferred as string[]

// Invalid operations
var mixed is [1, "two", true];     // Error: mixed types
nums.push("four");                // Error: type mismatch
```

### Tuples

Fixed-size collections supporting different types:

```doxa
var point is (10, 20, 30);         // Simple tuple
var nested is ((1, 2), (3, 4));    // Nested tuple

point[0];                         // Access first element
nested[1][0];                     // Access nested element
```

### Maps

String-keyed dictionaries:

```doxa
var scores is {
    "alice": 100,
    "bob": 85
};
scores["alice"];                  // Access value
```

## Control Flow

### Pattern Matching

```doxa
enum Status { Success, Error, Pending }

var result is match status {
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
var x is computeValue();
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
(∃x ∈ numbers : x > 10) // Logical notation
(exists x in numbers where x > 10) // English prose
(∀x ∈ numbers : x > 0) // Logical notation
(forall x in numbers where x > 0) // English prose
```

## Type System Details

### Normal Mode

Variables are dynamically typed by default:

```doxa
var x is 1;                      // int
x is true;                       // bool (allowed)

var y: auto is 3.14;            // float
y is "pi";                      // string (allowed)
```

### Safe Mode

Variables require explicit typing:

```doxa
var x :: int;                   // Valid declaration
var x is "two";                  // Error: needs type
var x :: auto is 3.14;          // Type locked to float
x is "five";                    // Error: type mismatch
```

Explicit return type declarations are required:

```doxa
// Safe Mode - Valid
fn greet(name :: string) -> string {
    return "Hello ${name}!";
}

// Safe Mode - Error: missing return type
fn greet(name :: string) {
    return "Hello ${name}!";
}
```

## Conditional Expressions

All conditionals are expressions and return values:

```doxa
var result is if condition then {
    value1
} else {
    value2
};
```

!!! note
Expressions without a value return `nothing`:
`doxa
    var x is if (false) { y is 1 };  // x becomes nothing
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

## Logical Operators

### First-Order Logic Notation

Doxa supports traditional first-order logic notation alongside standard programming syntax:

| Symbol | Meaning     | Description                              |
| ------ | ----------- | ---------------------------------------- |
| `∃`    | Exists      | At least one element satisfies condition |
| `∀`    | For all     | All elements satisfy condition           |
| `¬`    | Not         | Logical negation                         |
| `∧`    | And         | Logical AND                              |
| `∨`    | Or          | Logical OR                               |
| `↔`    | Equivalence | Logical equivalence (if and only if)     |
| `⊕`    | XOR         | Exclusive OR                             |
| `↑`    | NAND        | Not AND                                  |
| `↓`    | NOR         | Not OR                                   |
| `→`    | Implies     | Logical implication                      |

Examples:

```doxa
const arr :: int[] is [1, 2, 3, 4, 5];
// Quantifiers
(∃x ∈ arr : x > 3)?;     // true
(∀x ∈ arr : x > 3)?;     // false
(¬∀x ∈ arr : x > 3)?;    // true

// Logical operations
(false ↔ false)?;        // true (equivalent)
(true ⊕ true)?;         // false (XOR)
(true ∧ false)?;        // false
(true ∨ false)?;        // true
(true → false)?;        // false
```
