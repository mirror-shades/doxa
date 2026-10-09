Alias Parameters (^)

Overview
Alias parameters allow a function to modify a caller’s variable directly, without general-purpose pointers or references. They are opt-in at both the function signature and call site, making mutations explicit and safe. Aliases are stack-bound, cannot escape their defining function, and cannot be taken from partial subfields or array elements.

Syntax
```
fn modify(^param :: int) {
param += 1 # Directly mutates caller's variable
}

var x = 0
modify(^x) # Caller opts-in to mutation
# x == 1
```
Core Rules

    Parameter-only — ^T may only appear in function parameters, never in locals, returns, or stored types.

    Lvalue-only — Only full variables can be aliased no temporaries, expressions, struct fields, or array elements.

    Explicit call-site — Caller must pass ^var to grant mutation rights.

    No alias duplication — The same variable cannot be passed to more than one alias parameter in the same call.

    No alias + by-value mix — A variable cannot be passed aliased and by-value in the same call.

    No escaping — Aliases cannot be returned, stored, or captured by closures.

    No async/threads — Aliases are banned in async functions and cannot be passed to concurrent tasks.

    Exclusive borrow — Aliases grant exclusive mutable access for the duration of the call no overlapping borrows allowed.

    Evaluation order — Arguments evaluate left-to-right, but banned alias patterns remain errors.

Example: Safe Swap

fn swap(^a :: int, ^b :: int) {
let tmp is a
a is b
b is tmp
}

var x is 1, y is 2
swap(^x, ^y)
# x == 2, y == 1

Example: Compile-time Error Cases

swap(^x, ^x) # ❌ same variable passed twice
modify(^(x + 1)) # ❌ temporaries not allowed
f(^obj.field) # ❌ partial object aliasing not allowed
g(^x, x) # ❌ aliased and by-value in same call

Types
An alias argument lends its storage, so the callee may both read it and store
into it. Its type must therefore be exactly the parameter's (E1025): a narrower
variable could be handed a value it cannot hold, and a wider one could be read
as something it is not.

```
function reset(^x :: int | string) {
    x is 0
}

var s :: string is "hi"
reset(^s) # ❌ reset may store an int into a string
```

The one exception is a union parameter that preserves its member: every store
into it goes through a narrowing of it to a single member (`as`, or a `match`
arm), so it only ever stores back the member it was found to hold. A narrowing
to a smaller union, such as the `else` of `x as int` on an `int | string |
float`, does not count. Such a parameter may be lent a variable of any one member
type, and the variable keeps its type.

```
function exclaim(^x :: int | string) {
    match x {
        string then x is x + "!",
        int then x is x + 1,
    }
}

var s :: string is "hi"
exclaim(^s) # ✓ s is "hi!"
```

This is what lets `std.methods.push`, `insert` and `clear` take any
collection.

Lending a union parameter on to another counts as storing through it: it
preserves its member only if the parameter it is lent to does. A call through
a function value cannot be checked against the callee's body, so it never
takes a member-typed variable for a union parameter.

Design Intent
This model offers mutation semantics without full pointers, preserving deterministic lifetime and memory safety under stack-based allocation. By forcing explicit syntax and disallowing escaping or partial aliasing, it avoids common pitfalls of traditional references while retaining enough flexibility for in-place updates.
