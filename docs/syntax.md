# Doxa Language Reference

## Core Language Features

### Primitive Types

Doxa has two general categories of types, scalar and composite:

### Scalar

Scalar types are the most basic units of data. Currently Doxa has 3 default number types. Types listed reflect their Zig equivilent. Bigint may be added in the future for increased flexibility.

int (64 bit integer)
float (64 bit floating point)
byte (8 bit hex literal)
string
tetra (2 bit integer)
enum
nothing (void)


### Composite

Composite types are constructed out of scalar types. They are

array - must be homogenous
struct - no classes, no inheretance, only composition
group -  can be used as an umbrella type for enums and structs
union - can be handled with switch statements and type narrowing, see page on unions for more info

### Names

A name has one meaning wherever it can be seen. Doxa has no shadowing and no
overloading:

- A declaration inside a function, a block, a loop or a pattern may not reuse a
  name already visible where it appears: an enclosing local or parameter, or any
  top-level name of the file. A file's top-level names are visible throughout
  it, so a parameter may not share a name with a global declared further down
  (E1035).
- A file binds each top-level name once, whether by a declaration, a `module`
  alias, an `import`, or a `zig` block (E1002). Two imports of symbols that share
  a name conflict; import one through a `module` alias instead
  ([Modules](modules.md)).
- A struct names each member once: no two fields, no two methods or functions,
  and no method or function sharing a field's name (E1002).
- Functions and types are declared at file scope only (E2029), so a function
  never hides another inside a body.

A binding that is not visible does not conflict, so sibling blocks may each
declare the same name:

```doxa
{
    var t :: int is 1
}
{
    var t :: string is "two"   # fine: the first `t` is out of scope
}
```

Narrowing does not declare anything: inside `x as int then { … }` or a match
arm, `x` is the same variable seen at a narrower type. Likewise, inside the
branches of `const v is expr as T else { … }`, `v` is the declaration itself,
holding the subject narrowed to that branch's type.

### Line Continuation

A newline ends a statement. To continue one expression across lines, begin the
next line with `...`, which joins it to the line above:

```doxa
function inRange(v :: int) returns tetra {
    return v >= 0
        ...and v <= 7
}
```

`...` must be the first thing on its line. Placed anywhere else it is an error:

```
return @length("ab") == 2 ...and true
                          ^^^ EllipsisWithoutNewline: '...' continuation must follow a completed line
```

### Arithmetic Operators

Arithmetic follows Python-like semantics. See the [Arithmetic](math.md) page for full details on type promotion, division, and modulo.

| Operator | Name | Example |
|----------|------|---------|
| `+` | Addition | `1 + 2` → `3` |
| `-` | Subtraction | `3 - 1` → `2` |
| `*` | Multiplication | `2 * 3` → `6` |
| `**` | Exponentiation | `2 ** 3` → `8` |
| `/` | Float division | `10 / 4` → `2.5` |
| `//` | Integer division (floored) | `-7 // 2` → `-4` |
| `%` | Modulo (floored) | `-7 % 2` → `1` |

Compound assignment is supported for all arithmetic operators: `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `**=`.

`//=` and `%=` are integer-only, matching their binary forms: both operands must be `int` or `byte`.

`/=` is the exception that looks like the others but is not. Doxa's `/` is *float* division, so `/=` computes a `float` whatever the operands are — assigning that back into an `int` is a narrowing, which Doxa rejects everywhere else. Use `//=` for integer division:

```doxa
var n is 10
n /= 3      # Error: Float is not assignable to type Int
n //= 3     # n is now 3

var f is 10.0
f /= 4      # f is now 2.5
```

Dividing or taking a remainder by zero is a runtime trap (`Division by zero`, exit 1) rather than undefined behaviour. Float division is unaffected and follows IEEE 754, so `5 / 0` is `inf`.

### Strings

Double-quoted strings interpolate expressions in `{...}` anywhere a string expression is accepted:

```doxa
var name is "Doxa"
var greeting is "Hello {name}"
@print("{greeting}\n")
```

The `tetra` type represents a four cornered value with the possible states: `true`, `false`, `both`, and `neither`. For additional information see the tetra page.

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

### Enum variant shorthand

`.Variant` names a variant of the enum its position expects, and nothing else
gives it a type. The context must be explicit: a parameter, an annotated or
already-declared variable, a struct field, an annotated array or map key, a
return, or the other side of a comparison.

```doxa
var status :: Status is .Pending   # annotation
status is .Success                 # declared variable
if status == .Error then { … }     # comparison
report(.Error)                     # parameter

const bad is .Success              # error: no context; write `Status.Success`
```

A shorthand the expected enum does not declare is an error naming the enum.

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

Evaluates the operand for its side effect only: it prints a debug line (file/line/column when available, variable name, static type, and runtime value) and leaves the value on the stack unchanged. Output goes through Zig’s `std.debug.print` (typically **stderr**), so it stays separate from normal **stdout** (`@print`, `std.io.print`, and so on) and does not use the same fallible stdout path.

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

Functions can specify return types using the `returns` syntax. Any type can be returned, including composite types, but only one type at a time. This is one of the places where type unions can come in handy.

```doxa
function add(a :: int, b :: int) returns int {
    return a + b
}
```

A function or method without `returns` returns `nothing`. It may `return` early,
but returning a value from it is an error:

```doxa
function log(message :: string) {
    if message == "" then return  # fine: returns nothing
    @print(message)
}

function add(a :: int, b :: int) {
    return a + b # error: 'add' declares no `returns`, so it cannot return a value
}
```

The converse holds too: a bare `return` returns `nothing`, so a function that
declares `returns` may use one only when its return type includes `nothing`. A
function returns exactly what it declares; nothing widens the type for it.

```doxa
function check(n :: int) returns nothing | Error {
    if n > 0 then return   # fine: the declared type includes nothing
    return IOError.Denied
}

function strict(n :: int) returns Error {
    if n > 0 then return   # error: 'strict' returns Error, so a bare `return` has no value to give
    return IOError.Denied
}
```

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
