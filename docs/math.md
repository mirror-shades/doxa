# Arithmetic

## Operators

| Operator | Name | Description |
|----------|------|-------------|
| `+` | Addition | Adds two numbers |
| `-` | Subtraction | Subtracts right from left |
| `*` | Multiplication | Multiplies two numbers |
| `**` | Exponentiation | Raises left to the power of right. On `int` and `byte` it is exact; a negative `int` exponent truncates the fraction toward zero (`2 ** -1` is `0`, `(-1) ** -3` is `-1`), and `0 ** -1` overflows |
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
| **Array initializer** (e.g. `float[] is [1, 2.5, 3]`) | A float element makes the array `float[]` wherever it sits; each element is then a value position, so int literals widen and runtime ints need `@float()` |
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

A literal widens only when the float holds it exactly. Above 2^53 not every int has a float, so `var x :: float is 9007199254740993` is an error (`E1034`) rather than a silent rounding to `9007199254740992.0`; write the float literal you mean instead. A runtime value has no such check, which is why it needs `@float()`.

This rule mirrors how `int` → `byte` already works (comptime literals are allowed; runtime values require `@byte()`). The `@float()` intrinsic converts int, byte, or string values to float.

## Constant expressions

A *constant expression* is a literal, a `const` whose initializer is itself constant, or an operator or conversion (`@int`, `@float`, `@byte`) applied to those. `@length` of a fixed-size array is constant too. An array size must be a constant expression.

```doxa
const N is 4
const M is N * 3 - 1      # constant
var xs :: int[M // 2]     # a constant size

var n is 4                # a var is never constant
const k is n + 1          # not constant: computed when it runs
```

The compiler computes a constant expression with exactly the program's arithmetic. Where a running program would trap, a constant expression is a compile error instead:

| In a constant expression | Error |
|---|---|
| `int` or `byte` overflow or underflow (`9223372036854775807 + 1`, `0xFF + 0x01`, `0x00 - 0x01`) | `E1024` |
| `//` or `%` by an integer zero | `E1036` |
| `@int` of a float with no `int` value (`@int(1e300)`) | `E1032` |
| `@byte` of a number outside 0–255 | `E1031` |
| `@int`, `@float` or `@byte` of text that names no such number (`@int("abc")`) | `E1037` |

Anything else is not a constant, and the compiler never guesses its value. That includes a `var`, a field, an array element, and a call. Such an expression runs as written.

## Overflow and wrapping

`+`, `-`, `*` and `**` on `int` and `byte`, and unary `-`, fault when the result leaves the type: an `int` is a signed 64-bit integer, a `byte` an unsigned 8-bit one, so `0xFF + 0x01` and `0x00 - 0x01` are faults just as `maxInt() + 1` is (`Integer overflow at <file>:<line>:<column>`, exit 1; see [Runtime faults](methods.md#runtime-faults)). This holds in every `--opt-mode`. The compiler drops a check only where it proves the result fits.

Modular arithmetic is explicit, through the wrapping operators. `+%`, `-%` and `*%` (and unary `-%x`) wrap modulo 2^64 on `int` and modulo 256 on `byte`, and never fault. They take `int` and `byte` operands only and type like `+`, `-` and `*`; each has a compound form (`+%=`, `-%=`, `*%=`). A wrapping operator costs exactly the bare instruction. Hashes, random number generators, checksums and byte codecs use them:

```doxa
function mix(h :: int, x :: int) returns int {
    return (h +% x) *% 1099511628211
}

const b is 0xFF +% 0x01    # 0x00
```

In a constant expression they wrap too: `0xFF +% 0x01` is `0x00`, not `E1024`.

`std.math` also holds the limits: `maxInt()` / `minInt()`, `maxByte()` / `minByte()`, and `maxFloat()` (the largest finite `float`), `minFloat()` (its negation) and `minNormalFloat()` (the smallest positive normal). See [Standard Library](stdlib.md#math).

## Division by zero

Integer division and modulo by zero (`//`, `%`, on `int` or `byte`) are a runtime fault (`Division by zero at <file>:<line>:<column>`, exit 1; see [Runtime faults](methods.md#runtime-faults)). Float division follows IEEE 754 instead: `1 / 0` is `inf`, `-1 / 0` is `-inf`, and `0 / 0` is NaN.

## Floating point

`float` is an IEEE 754 double, and its special values behave as the standard says:

- **NaN** comes only from an operation with no defined answer: `0.0 / 0.0`, `inf - inf`, `inf * 0`, `inf / inf`, or `@float("nan")`. It is unordered: `<`, `<=`, `>`, `>=`, and `==` against NaN are all `false`, and `!=` is `true` — NaN is unequal even to itself, so `x != x` holds exactly when `x` is NaN.
- **Negative zero** is a distinct value that compares equal to `0.0`. Negation keeps its sign (`-0.0` prints `-0.0`), and the sign shows through division: `1 / -0.0` is `-inf`.
- **Overflow** rounds to `inf` or `-inf`; **underflow** rounds to `0.0`.

A float prints as the shortest positional decimal that reads back as the same value, with `.0` on an integral one so it never reads as an `int`: `0.1 + 0.2` prints `0.30000000000000004`, `1.0e10` prints `10000000000.0`.
