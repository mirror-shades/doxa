# Doxa Language Reference

## Core Language Features

### Primitive Types

Types in Doxa are grouped in three ways, atomic, molecular, and meta. All together there are 14 of these primitive types to represent data in Doxa.

### Atomic

Atomic types are the most basic units of data. Currently Doxa has 3 default number types, with plans for optional dynamic number sizing in the future.

int - i64
float - f64
byte - u8 hex literal
string
tetra - logical value, see page on tetras for more info
nothing - empty type

### Molecular

Molecular types are constructed out of atomic types. They are

array - must be homogenous
struct - no classes, no inheretance, only composition
enum
map
function - user functions, library functions, etc
union - can be handled with the collapse operator, see page on unions for more info

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
        print(animal.name + " says woof!")
    }
}

var dog is Dog {
    animal: Animal {
        name: "Spot"
        },
    breed: "Labrador"
}
```

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
var nums :: int[] is [1, 2, 3]    // Explicit typing
var strs is ["a", "b"]            // Inferred as string[]

// Invalid operations
var mixed is [1, "two", true]     // Error: mixed types
nums.push("four")                // Error: type mismatch
````

### Tuples

Fixed-size collections supporting heterogenus types:

```doxa
var point is (: 10, "hello", true :)         // Simple tuple
var nested is (: (: 1, "hello" :), (: 3, true :) :)    // Nested tuple

point[0]                         // Access first element
nested[1][0]                     // Access nested element
```

### Maps

String-keyed dictionaries:

```doxa
var scores is {
    "alice": 100,
    "bob": 85
}
scores["alice"]                  // Access value
```

## Control Flow

### Pattern Matching

```doxa
enum Status { Success, Error, Pending }

var result is match status {
    .Success => "all good",
    .Error => "failed",
    else => "waiting"
}
```

!!! warning
Match expressions must be exhaustive or include an `else` clause.

### Error Handling

```doxa
try {
    riskyOperation()
} catch {
    handleError()
}
```

## Special Operators

### Peek operator (`?`)

```doxa
var x is computeValue()
x?                              // Prints value with location, name, and type
```

### Type Information

```doxa
typeof(42)                      // "int"
typeof("hello")                 // "string"
typeof([1,2,3])                 // "array"
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
}
```

!!! note
Expressions without a value return `nothing`:
`doxa
    var x is if (false) { y is 1 }  // x becomes nothing
    `

### Function Return Types

Functions can specify return types using either `->` or `returns` syntax:

```doxa
fn add(a: int, b: int) -> int {
    return a + b
}

// Alternative syntax
fn add(a: int, b: int) returns(int) {
    return a + b
}
```

return types are optional and inferred by default:

```doxa
// Normal Mode - Valid
fn add(a, b) {
    return a + b
}
```

## Logic

### First order logic

First order logic always produces a true or false value. For more information on tetras see the tetra page.

Doxa has extensive for traditional first order logics that work as expected with true and false values. These can be represented in formal unicode notation:

```
const arr :: int[] = [1, 2, 3, 4, 5]

// existential quantifier ∃, element of ∈
∃x ∈ arr : x > 3 // true

// universal quantifier ∀, where :
∀x ∈ arr : x > 3 // false

// NOT ¬
¬false // true

// biconditional ↔
false ↔ false // true

// XOR ⊕
true ⊕ true // false

// AND ∧
true ∧ false // false

// OR ∨
true ∨ false // true

// NAND ↑
true ↑ false // true

// NOR ↓
true ↓ false // false

// implication →
true → false // false
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

¬(∀x ∈ arr : x > 3) // true
not (forall x in arr where x > 3) // true
```

### Trancendental logic

There are currently two trancendental operators, `and` and `not`. These can be represented by the following truth tables:

| ^     | T   | F   | B   | N   |
| ----- | --- | --- | --- | --- |
| **F** | F   | B   | B   | N   |
| **T** | B   | T   | B   | N   |
| **B** | B   | B   | B   | N   |
| **N** | N   | N   | N   | N   |

| ~     |     |
| ----- | --- |
| **F** | T   |
| **T** | F   |
| **B** | N   |
| **N** | B   |
