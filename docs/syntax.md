# Doxa Language Reference

## Core Language Features

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
        print(animal.name + " says woof!");
    }
}

var dog is Dog {
    animal: Animal {
        name: "Spot"
        },
    breed: "Labrador"
};
```

<!-- ### Modules

```doxa
// math.doxa
#safe
export fn add(a :: int, b :: int) -> int {
    return a + b;
}

// main.doxa
import { add } from "./math.doxa";
var sum is add(1, 2);
``` -->

## Data Types

### Basic Types

Standard types include:

- `int`: Integer numbers
- `float`: Floating point numbers
- `string`: Text strings
- `tetra`: Four-valued logic system

The `tetra` type represents a four cornered value with the possible states: `true`, `false`, `both`, and `neither`. For additional information see the tetra page.

````

### Arrays

Arrays are homogeneous collections with type inference:

```doxa
var nums :: int[] is [1, 2, 3];    // Explicit typing
var strs is ["a", "b"];            // Inferred as string[]

// Invalid operations
var mixed is [1, "two", true];     // Error: mixed types
nums.push("four");                // Error: type mismatch
````

### Tuples

Fixed-size collections supporting heterogenus types:

```doxa
var point is (: 10, "hello", true :);         // Simple tuple
var nested is (: (: 1, "hello" :), (: 3, true :) :);    // Nested tuple

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

### Peek operator (`?`)

```doxa
var x is computeValue();
x?;                              // Prints value with location, name, and type
```

### Type Information

```doxa
typeof(42);                      // "int"
typeof("hello");                 // "string"
typeof([1,2,3]);                 // "array"
```

### Collection Quantifiers

```doxa
(∃x ∈ numbers : x > 10) // Logical notation
(exists x in numbers where x > 10) // English prose
(∀x ∈ numbers : x > 0) // Logical notation
(forall x in numbers where x > 0) // English prose
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

## Logic

### First order logic

First order logic always produces a true or false value. For more information on tetras see the tetra page.

Doxa has extensive for traditional first order logics that work as expected with true and false values. These can be represented in formal unicode notation:

```
const arr :: int[] = [1, 2, 3, 4, 5]

// existential quantifier ∃, element of ∈
∃x ∈ arr : x > 3; // true

// universal quantifier ∀, where :
∀x ∈ arr : x > 3; // false

// NOT ¬
¬false; // true

// biconditional ↔
false ↔ false; // true

// XOR ⊕
true ⊕ true; // false

// AND ∧
true ∧ false; // false

// OR ∨
true ∨ false; // true

// NAND ↑
true ↑ false; // true

// NOR ↓
true ↓ false; // false

// implication →
true → false; // false
```

This unicode support is paired with plaintext keywords which act in an identical fashion:

```
∃ - exists
∀ - forall
∈ - in
: - where
¬ - not
↔ - iff
⊕ - xor
∧ - and
∨ - or
↑ - nand
↓ - nor
→ - implies
```

This means formal logical representation can be written in either way:

```
const arr :: int[] = [1, 2, 3, 4, 5]

¬(∀x ∈ arr : x > 3); // true
not (forall x in arr where x > 3); // true
```

### Trancendental logic

There are currently two trancendental operators, `and` and `not`. These can be represented by the following truth tables:

| ^     | T   | F   | B   | N   |
| ----- | --- | --- | --- | --- |
| **F** | F   | B   | B   | F   |
| **T** | B   | T   | B   | F   |
| **B** | B   | B   | B   | F   |
| **N** | F   | F   | F   | F   |

| ~     |     |
| ----- | --- |
| **F** | T   |
| **T** | F   |
| **B** | N   |
| **N** | B   |

## Modes of Operation (not yet implemented)

Doxa supports three operational modes:

- **Normal Mode** (default): Dynamic typing with flexible syntax
- **Safe Mode**: Enabled via `#safe` at file start, enforces static typing
- **Warn Mode**: Compiles in normal mode but warns about safe convention violations

!!! note
Safe files can only import other safe files, while normal files can import both.
