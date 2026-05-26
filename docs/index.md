# Doxa

```
Everything is real and is not real,
Both real and not real,
Neither real nor not real.
This is Lord Buddha’s teaching.

- Nāgārjuna
    Verses on the Middle Way (2nd–3rd century CE)
```

Doxa is a simple to write, highly readible language based on four corned logical values inspired by the logic of Nagarjuna. In Doxa there are no booleans. Doxa instead uses a four cournered logical unit called a tetra. Tetras can represent true and false, which act as you would expect them to in a traditional programing language.

## Documentation

- [Language syntax](syntax.md) — primitives, operators, and core language rules
- [Alias parameters](alias.md) — `^` parameters for explicit, stack-bound caller mutation
- [Arrays](arrays.md) — dynamic and fixed-size arrays
- [Control flow](control.md) — `if`, `match`, `as`, and related branching
- [Loops](loops.md) — composable `for` / `while` / `do` loop forms
- [Maps](maps.md) — key–value maps and typing
- [Memory management](memory.md) — arena scopes and how allocation is tied to blocks
- [Methods and intrinsics](methods.md) — `@` intrinsics
- [Groups](groups.md) — named tagged unions of enums, structs, and other groups
- [Standard Library](stdlib.md) — standard library functions
- [Structs](struct.md) — composition-based records, visibility, and `$` construction
- [Tetras](tetras.md) — four-valued logic in place of booleans
- [Typed unions](unions.md) — `A | B` unions and narrowing
- [Zig blocks](zig.md) — embedded Zig modules and the inline Zig ABI