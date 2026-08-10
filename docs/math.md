# Arithmetic

## Operators

| Operator | Name | Description |
|----------|------|-------------|
| `+` | Addition | Adds two numbers |
| `-` | Subtraction | Subtracts right from left |
| `*` | Multiplication | Multiplies two numbers |
| `**` | Exponentiation | Raises left to the power of right |
| `/` | Float division | Always returns `float` |
| `//` | Integer division | Floored division, returns `int` |
| `%` | Modulo | Floored remainder, same sign as divisor |

## Float division (`/`)

Division with `/` always produces a `float` result, regardless of operand types. Integer operands are implicitly promoted to float.

```doxa
10 / 4         # 2.5     (int operands, float result)
10.0 / 2       # 5.0     (mixed types)
-7 / 2         # -3.5    (negative operands)
```

## Integer division (`//`)

Integer division uses **floored** division: the result is rounded toward negative infinity. Floats are rejected as operands.

```doxa
7 // 2         # 3       (positive)
-7 // 2        # -4      (floored toward negative infinity)
7 // -2        # -4
-7 // -2       # 3
```

## Modulo (`%`)

The modulo operator uses **floored** remainder. The result always has the same sign as the divisor. Together with floored integer division, the identity `(a // b) * b + (a % b) == a` holds for all inputs.

```doxa
7 % 2          # 1
-7 % 2         # 1       (same sign as divisor)
7 % -2         # -1
-7 % -2        # -1
```

## Type promotion

Doxa promotes numeric **operands** to a common type before evaluation. This applies to binary operators (`+`, `-`, `*`, `/`, `**`, `%`, `//`) and array literals, but **not** to variable assignment, function arguments, return values, or struct fields.

| Position | Rule |
|---|---|
| **Operator** (e.g. `1 + 2.5`) | Int or byte operands promote to float automatically |
| **Array initializer** (e.g. `float[] is [1, 2, 3]`) | Elements promote to the array's element type |
| **Value position** (assignment, call arg, return) | Only comptime literals are implicit; runtime values require `@float()` |

### Operator promotion

1. **Float dominance:** If either operand is `float`, or the operator is `/`, the result is `float`.
2. **Int fallback:** If either operand is `int`, the result is `int` (except for `/`).
3. **Byte:** If both operands are `byte`, operations stay in `byte`.

```doxa
1 + 2.5        # 3.5     (int promoted to float)
1 + 2          # 3       (both int)
1 // 2         # 0       (integer division)
1 / 2          # 0.5     (float division, always)
```

### Value position widening

For value positions (assignment, function arguments, return values), int or byte does **not** implicitly widen to float unless the value is a literal written directly in source:

```doxa
var x :: float is 60         # allowed — literal 60 widens to 60.0
takesFloat(60)               # allowed — literal 60 widens to 60.0

const LIT is 60
takesFloat(LIT)              # error — named const, use @float(LIT)

var i :: int is 60
takesFloat(i)                # error — runtime value, use @float(i)

var f :: float is i          # error — runtime value, use @float(i)
```

This rule mirrors how `int` → `byte` already works (comptime literals are allowed; runtime values require `@byte()`). The `@float()` intrinsic converts int, byte, or string values to float.

## Division by zero

Division or modulo by zero in any context (float, int, byte) produces a runtime error.
