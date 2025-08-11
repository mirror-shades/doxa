## Control Flow

Doxa unifies conditional branching with a consistent then / else pattern across if, as, and match.

### Core rules

- **Expression bodies end with a semicolon**; block bodies `{ ... }` are self-terminating (no extra semicolon after the closing brace).
- **then** attaches the success branch where applicable; **else** attaches the failure/default branch and does not use then.
- These rules apply consistently to if, as, and match.

### if / then / else

- **then required**, **else optional**.
- Body can be an expression (end with `;`) or a block `{ ... }` (no trailing `;`).

Examples:

```doxa
// Block bodies (no trailing semicolons)
if x > 10 then {
    "x is greater than 10"?;
} else {
    "x is 10 or less"?;
}

// Expression body (ends with semicolon)
var condition is if true then "true" else "false";
```

Chaining and statement-style:

```doxa
if current % 3 equals 0 and current % 5 equals 0 then "fizzbuzz"?;
else if current % 3 equals 0 then "fizz"?;
else if current % 5 equals 0 then "buzz"?;
else current?;
```

### as / then / else (type narrowing)

- **else required**, **then optional**.
- Same body rules: expression bodies end with `;`, block bodies do not.

Examples:

```doxa
// Success and failure as expressions
const n_or_zero is value as int else 0;

// Block failure case
value as int else {
    "value is not an int"?;
};

// Explicit success block
value as int then {
    // use the narrowed int value here
} else {
    // handle non-int here
}
```

### if vs as

In Doxa, if and as both use the then / else pattern, but they invert the emphasis:

| Keyword | Requires | Optional | Implied meaning                                 |
| ------- | -------- | -------- | ----------------------------------------------- |
| if      | then     | else     | If success then do x, failure branch optional   |
| as      | else     | then     | As a type or else do x, success branch optional |

`if` is a keyword which implicitly requires a success condition, `as` implicity requires a fail condition, but both use then/else blocks in the same way.

Examples

```
// IF: Truth-driven
if x > 10 then "big"? else "small"?;

// AS: Fallback-driven
value as int else 0;        // If not int, use 0
value as int then 20 else 0; // If int, use 20; else 0

// Both in action
if isReady then start() else wait();
data as string then parse(data) else log("Bad data");
```

### match (values and union types)

Use match to branch on:

- **Concrete values**: numbers, strings, enum variants (`.Red`, `.Green`, ...)
- **Union type arms**: `int`, `float`, `string`, `byte`, `tetra`, `nothing`, or custom types

Arm syntax and delimiters:

- **Pattern arms**: `pattern then BODY`
- **Else arm**: `else BODY` (no then)
- **Body**: expression (end with `;`) or block `{ ... }` (no trailing `;`)
- Arms are separated by either the semicolon of an expression body, or by the end of a block body

Match on union types:

```doxa
fn kind(value :: int | float) returns(string) {
    return match value {
        int then "integer"?;
        float then "float"?;
    };
}
```

Match on enums and values:

```doxa
enum Color { Red, Green, Blue }

var msg1 is match color {
    .Red then "It's red";
    .Blue then "It's blue";
    else "It's something else";
};

const x is 5;
var msg2 is match x {
    0 then "It's zero";
    5 then "It's five";
    else "It's something else";
};

const s is "big";
var msg3 is match s {
    "big" then "It's big";
    "small" then "It's small";
    else "It's something else";
};
```

Block arms in match:

```doxa
var msg is match color {
    .Red then { "stop"?; "red"; }
    .Green then "green";
    else { "caution"?; "yellowish"; }
};
```

Notes:

- **Exhaustiveness**: For enums, prefer covering all variants or add an `else` arm. For unions, cover the needed type arms; `else` is optional.
- **Result type**: All arms must produce a compatible result type. For block arms, the last expression is the arm’s value.

### Quick reference

- **if**: then required; else optional; expression body ends with `;`, block body does not.
- **as**: else required; then optional; same body rules.
- **match**: pattern then BODY; else BODY; arm separation via expression `;` or closing block `}`.
