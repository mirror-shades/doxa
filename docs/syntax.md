# Doxa Language Reference

## Core Language Features

### Primitive Types

Doxa has two general catagory of types, atomic and molecular:

### Atomic

Atomic types are the most basic units of data. Currently Doxa has 3 default number types. Types listed reflect their Zig equivilent. Bigint may be added in the future for increased flexibility.

int - i64
float - f64
byte - u8 hex
string - []const u8
tetra - i2
nothing - void

The `tetra` type represents a four cornered value with the possible states: `true`, `false`, `both`, and `neither`. For additional information see the tetra page.


### Molecular

Molecular types are constructed out of atomic types. They are

array - must be homogenous
struct - no classes, no inheretance, only composition
enum
union - can be handled with switch statements and type narrowing, see page on unions for more info

````

### Arrays

Arrays are homogeneous collections with type inference:

```doxa
var nums :: int[] is [1, 2, 3]    # Explicit typing
var strs is ["a", "b"]            # Inferred as string[]

# Invalid operations
var mixed is [1, "two", true]     # Error: mixed types
@push(nums, "four")                # Error: type mismatch
```

#### Array Storage Kinds

Doxa tracks how every array is stored, and that storage kind determines which builtins can mutate it.

- `dynamic` — default for `var` arrays and anything returned from runtime helpers. These can grow and shrink freely.
- `fixed` — any type annotation that names a literal size (for example, `int[4]`) produces a fixed array. Type aliases preserve this metadata, so wrapping `int[4]` in an alias and then declaring `var data :: MyAlias` still behaves as fixed storage with an immutable length/capacity.
- `const literal` — a `const` binding that receives a compile-time literal (including nested literals) is tagged immutable. Any other `const` that aliases that value inherits the same storage kind.

Mutating methods like `@push`, `@insert`, `@remove`, or `@clear` require dynamic storage. Coerce a fixed/const literal array into dynamic form by copying it into a mutable declaration:

```doxa
const literal = [1, 2]
var copy :: int[] is literal  # copy becomes dynamic and @push-friendly
```
````

### Maps

String-keyed dictionaries:

```doxa
map scores {
    "alice" is 100,
    "bob" is 85
}
scores["alice"]                  # Access value
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

Match expressions must be exhaustive or include an `else` clause.

### Error Handling

Errors are best handled with custom enum and type unions.

```doxa
enum Error {
    TOO_BIG,
    TOO_SMALL,
}

const res is intOrError()
res as int then {
    intsOnly(res) # narrowed to int
} else {
    match res { # else blocks do not narrow
        Error.TOO_BIG then {
            @print("result was too big")
        }
        Error.TOO_SMALL then {
            @print("result was too small")
        }
        else { # we know this will never be reached
            unreachable
        }
    }
}

```

### `unreachable`

Use the `unreachable` keyword when a control-flow path should be impossible. It behaves like Zig's `unreachable`: the compiler treats it as a no-return expression for type checking and it traps at runtime with a helpful location if execution ever arrives there.

## Special Operators

### Peek operator (`?`)

```doxa
var x is computeValue()
x?                              # Prints value with location, name, and type
```
```
[./test.doxa:2:2] x :: int is 62
```
### Range (`to`)

Ranges are arrays which can be declared between two ints or bytes

```
const range is 10 to 15
@print("{range}") # [10, 11, 12, 13, 14, 15]
```

### Collection Quantifiers

As with all formal logic operations, both symbolic notation and keyword notations are supported.

```doxa
(∃x ∈ numbers : x > 10) # Logical notation
(exists x in numbers where x > 10) # English prose
(∀x ∈ numbers : x > 0) # Logical notation
(forall x in numbers where x > 0) # English prose
```

## Conditional Expressions

All conditionals can be used as expressions to assign values:

```doxa
var result is if condition then {
    value1
} else {
    value2
}
```

Be aware assigning an expression without a value will assign `nothing`:
```doxa
    var x is if (false) { y is 1 }  # x becomes nothing
```

### Function Return Types

Functions can specify return types using the `returns` syntax. Any type can be returned, including molecular types, but only one type at a time. This is one of the places where type unions can come in handy.

```doxa
fn add(a: int, b: int) returns int {
    return a + b
}
```

Return types are optional and inferred by default:

```doxa
fn add(a :: int, b :: int) {
    return a + b # inferred as returning an int
}
```

Explicit typing is encouraged as unions will be inferred if more than one type is returned.

## Logic

### First order logic

First order logic always produces a true or false value. For more information on tetras see the tetra page.

Doxa has extensive for traditional first order logics that work as expected with true and false values. These can be represented in formal unicode notation:

```
const arr :: int[] = [1, 2, 3, 4, 5]

# existential quantifier ∃, element of ∈
∃x ∈ arr : x > 3 # true

# universal quantifier ∀, where :
∀x ∈ arr : x > 3 # false

# NOT ¬
¬false # true

# biconditional ↔
false ↔ false # true

# XOR ⊕
true ⊕ true # false

# AND ∧
true ∧ false # false

# OR ∨
true ∨ false # true

# NAND ↑
true ↑ false # true

# NOR ↓
true ↓ false # false

# implication →
true → false # false
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

¬(∀x ∈ arr : x > 3) # true
not (forall x in arr where x > 3) # true
```

### Paradoxical logic

There are currently two paradoxical operators, `and` and `not`. These can be represented by the following truth tables:

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
