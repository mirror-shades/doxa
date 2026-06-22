## Control Flow

Doxa unifies conditional branching with a consistent then / else pattern across if, as, and match.

### if / then / else

- **then required**, **else optional**.

Examples:

```doxa
if x > 10 then {
    @print("x is greater than 10/n")
} else {
    @print("x is 10 or less/n")
}

var condition is if true then "true" else "false"
```

Chaining and statement-style:

```doxa
if current % 3 equals 0 and current % 5 equals 0 then @print("fizzbuzz/n")
else if current % 3 equals 0 then @print("fizz/n")
else if current % 5 equals 0 then @print("buzz/n")
else @print(current)
```

### as / then / else (type narrowing)

- **else required**, **then optional**.
- Same body rules: expression bodies end with ` `, block bodies do not.
- The tested variable is narrowed in both branches: the **then** branch narrows to the target type, the **else** branch narrows to the remainder (union minus target).

Examples:

```doxa
# Inline fallback value
const n_or_zero is value as int else 0

# Block with explicit return (guard pattern)
const n is value as int else {
    @print("value is not an int/n")
    return -1
}

# Block with lift (provides fallback value, continues)
const n is value as int else {
    @print("value is not an int, using default/n")
    lift 0
}

# Type narrowing in both branches
const val :: int | float | Error is foo()
val as int then {
    # val :: int here
} else {
    # val :: float | Error here
}
```

**Then block behavior**: when the then branch is a block, it runs for side effects only. The `as` expression's value on the success path is the original narrowed value. When the then branch is an inline expression, its value is used as the result.

```doxa
var x is val as int then 20 else 0        # x is 20 (inline then)
var x is val as int then { work() } else 0 # x is val's int value (block then, side-effects)
```

### Block values and `lift`

In `if` and `as` branches, blocks do **not** implicitly return their last expression. The last expression in a block is discarded (side-effect only). To provide a value from a block, use `lift`:

```doxa
# lift provides a value from a block
var x is if cond then { lift compute() } else { lift 0 }

# Without lift, blocks produce nothing — use inline for simple values
var x is if cond then compute() else 0
```

`lift` terminates the block (code after `lift` is unreachable). It scopes to the immediately enclosing block `{ }`.

`return` always exits the enclosing function. `lift` always provides a value to the enclosing expression.

```doxa
var x is foo() as int else {
    @print("error/n")
    lift 0               # x gets 0, execution continues after the as expression
}

var x is foo() as int else {
    @print("fatal/n")
    return -1            # function exits, x is never assigned
}
```

When `as` or `if` is used in assignment, block branches must either `lift` a value or diverge via `return`/`break`. A block that falls through without `lift` produces `nothing`, which is a compile error when a value is expected.

`match` arms are not affected — implicit last-expression returns are preserved in match blocks.

### if vs as

In Doxa, if and as both use the then / else pattern, but they invert the emphasis:

| Keyword | Requires | Optional | Implied meaning                                 |
| ------- | -------- | -------- | ----------------------------------------------- |
| if      | then     | else     | If success then do x, failure branch optional   |
| as      | else     | then     | As a type or else do x, success branch optional |

`if` is a keyword which implicitly requires a success condition, `as` implicity requires a fail condition, but both use then/else blocks in the same way.

Examples

```
# IF: Truth-driven
if x > 10 then @print("big/n") else @print("small/n")

# AS: Fallback-driven
value as int else 0         # If not int, use 0
value as int then 20 else 0  # If int, use 20  else 0

# Both in action
if isReady then start() else wait()
data as string then parse(data) else log("Bad data/n")
```

### match (values and union types)

Use match to branch on:

- **Concrete values**: numbers, strings, enum variants (`.Red`, `.Green`, ...)
- **Union type arms**: `int`, `float`, `string`, `byte`, `tetra`, `nothing`, or custom types

Arm syntax and delimiters:

- **Pattern arms**: `pattern then BODY`
- **Else arm**: `else BODY` (no then)
- **Body**: expression (end with ` `) or block `{ ... }` (no trailing ` `)

Match on union types:

```doxa
fn kind(value :: int | float) returns(string) {
    return match value {
        int then @print("integer/n")
        float then @print("float/n")
    }
}
```

Match on enums and values:

```doxa
enum Color { Red, Green, Blue }

var msg1 is match color {
    .Red then "It's red"
    .Blue then "It's blue"
    else "It's something else"
}

const x is 5
var msg2 is match x {
    0 then "It's zero"
    5 then "It's five"
    else "It's something else"
}

const s is "big"
var msg3 is match s {
    "big" then "It's big"
    "small" then "It's small"
    else "It's something else"
}
```

Block arms in match:

```doxa
var msg is match color {
    # semicolons can be used to represent line breaks
    .Red then { @print("stop/n"); "red"}
    .Green then { @print("go/n"); "green"
    else { @print("caution/n"); "yellowish"}
}
```

Notes:

- **Exhaustiveness**: For enums, prefer covering all variants or add an `else` arm. For unions, cover the needed type arms `else` is optional.
- **Result type**: All arms must produce a compatible result type. For block arms, the last expression is the arm's value.

### Quick reference

- **if**: then required else optional. Inline expressions or blocks with `lift`.
- **as**: else required then optional. Type narrowing in both branches. Blocks use `lift` or `return`.
- **match**: pattern then BODY else BODY. Block arms use implicit last expression.
- **lift**: provides a value from a block, terminates the block. Scopes to the immediately enclosing `{ }`.
