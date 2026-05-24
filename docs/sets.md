# Enum sets (specification)

This document specifies **enum sets**: a type that merges multiple existing enums into one nominal type by re-exporting their variants under qualified names. The primary motivation is **error handling without privileged error types** — several domain enums (`error.IO`, `error.Common`, …) merge into one portable type (`std.error.Std`) so function signatures use a single set arm instead of long `A | B | C` unions, while preserving **traceability** (which domain a variant came from).

**Status:** specification (not yet implemented). When implemented, update [syntax.md](syntax.md) and [unions.md](unions.md) to reference this page.

---

## 1. Goals and non-goals

### Goals

- Merge multiple named enums into one **nominal** set type for signatures and APIs.
- Keep **source enums** as the canonical definition of variants (single place to document and evolve).
- Make merged variants **traceable** at the type and syntax level (caller knows `IO` vs `Common`).
- Support **implicit widening**: a value of a source enum is assignable where the set type is expected, **zero-cost** via shared variant identity.
- Work with existing **typed unions** (`T | E`) and **match** / **`as`** narrowing without a separate error channel.
- **Cross-type equality**: `error.IO.NotFound == Std.IO.NotFound` is true (same variant identity).

### Non-goals

- Automatic flattening of duplicate **simple** variant names across domains (e.g. multiple `Unexpected`) into one variant; see section 6.
- Changing how Zig blocks report failures (thread-local codes, mappers); this spec is the **Doxa type layer**.
- Overloading the `^` **alias parameter** terminology ([alias.md](alias.md)).

---

## 2. Syntax

### 2.1 Set declaration

A set is declared with the `set` keyword. The body lists source enums whose variants are included. Each source contributes its variants under a **qualifier** (the unqualified name of the source type).

```doxa
public set Std {
    error.Common,
    error.IO,
    error.Process,
    error.Method,
}
```

**Qualified variant names** use the qualifier as a prefix segment:

- `Std.Common.InvalidArgument`
- `Std.IO.NotFound`
- `Std.Process.SpawnFailed`

The qualifier defaults to the **unqualified** name of the source enum: `Common`, `IO`, `Process`, `Method`. If two members would yield the same qualifier, the program is **ill-formed**.

### 2.2 Local variants

The declaring set may add its own variants alongside source members. Recommended style: source members first, then local variants.

```doxa
public set AppErr {
    std.error.Std,
    UserCancelled,
    ConfigInvalid,
}
```

Local variants are **unqualified** under the declaring set: `AppErr.UserCancelled`.

### 2.3 Transitive sets

A set may include another set as a source member. All variants flow through transitively with the same variant identities.

```doxa
public set Std {
    error.IO,
    error.Common,
}

public set AppErr {
    Std,             # includes all error.IO + error.Common variants
    UserCancelled,
}
```

`AppErr.Std.IO.NotFound` is valid (qualifier chain preserved: `AppErr` → `Std` → `IO` → variant). Widening from `error.IO` to `AppErr` is possible (transitively via `Std`) and zero-cost.

Qualifier chain rules (qualifiers do **not** flatten):
- Each set member contributes exactly one qualifier segment.
- When a member is itself a set, its internal qualifiers are nested underneath the member's qualifier.
- Example: `AppErr` includes `Std`, which includes `error.IO`. The full path is `AppErr.Std.IO.NotFound`.

---

## 3. Name resolution

### 3.1 References inside the same module

- `Std.IO.NotFound` — full path from declaring set type.
- If unambiguous, implementations **may** allow omitting the set name when the expected type is `Std` (e.g. in `match` arms); this is **optional** and not required for v1.

### 3.2 References from outside

Use the path exported by the module, e.g. `std.error.Std.IO.NotFound`.

### 3.3 Source enum variants unchanged

`error.IO.NotFound` remains a valid expression of type `error.IO`. No change to existing enums.

---

## 4. Type system

### 4.1 Nominal identity

- Each `set` declaration introduces a distinct nominal type.
- `Std` is not the same type as `error.IO` or `error.Common`.
- Two sets with identical members are distinct types.

### 4.2 Shared variant identity (core design)

Every enum variant in the program has a **globally unique runtime identity**. When a set includes a source enum, it does **not** create new variants — it creates qualified names that reference the **same** variant identities.

| Expression | Variant identity |
|---|---|
| `error.IO.NotFound` | `#v_io_notfound` |
| `Std.IO.NotFound` | `#v_io_notfound` (same) |
| `AppErr.Std.IO.NotFound` | `#v_io_notfound` (same) |

Consequences:

- **Widening is a compile-time no-op.** The bit pattern does not change when going from `error.IO` to `Std`. No re-tagging, no mapping table.
- **Equality compares identity directly.** Two values are equal iff they carry the same variant identity, regardless of declared type.
- **`@istype` is a range/membership check** against the source enum's variant set (section 7).

The implementation may realize variant identity as a global discriminant, a pointer to a variant descriptor, or any scheme that guarantees uniqueness. The user-visible guarantee is: a variant **is** the same value no matter which set type it is viewed through.

### 4.3 Widening and narrowing

**Widening (implicit, zero-cost):** If `S` is a source enum **included in** set `T` (directly or transitively), an expression of type `S` is assignable to `T`. The variant identity is unchanged — no runtime work.

| Expression | Type | Widens to | Zero-cost? |
|---|---|---|---|
| `error.IO.NotFound` | `error.IO` | `Std` | Yes |
| `Std.IO.NotFound` | `Std` | `AppErr` | Yes (if `AppErr` includes `Std`) |
| `error.IO.NotFound` | `error.IO` | `AppErr` | Yes (transitive via `Std`) |

**Narrowing (explicit):** From a set type to a source enum:

- **`x as error.IO`** — succeeds when the variant identity belongs to `error.IO`'s variant set; otherwise the `else` branch of `as` runs.
- No implicit narrowing from a set type to a source enum.

### 4.4 Same set, different qualifiers

`Std.IO.Unexpected` and `Std.Common.Unexpected` are **different** variants of `Std` (different variant identities) even though the simple name `Unexpected` repeats in both source enums. The qualifier disambiguates them.

### 4.5 Equality

Equality compares **variant identity**. `a == b` is true iff both values carry the same variant identity.

```
error.IO.NotFound == Std.IO.NotFound       → true
Std.IO.NotFound == AppErr.Std.IO.NotFound    → true (if AppErr includes Std)
error.IO.NotFound == error.Common.NotFound  → false (different identities)
```

Cross-type equality works without explicit widening because the same variant identity is exposed through all set types.

### 4.6 Ordering

Variant identities have a total order defined by their global discriminant (or equivalent). `a < b` reflects this order. Two values with different declared types but the same variant identity compare equal (both `==` and ordering).

### 4.7 Unions

**Union members** are compared nominally. A single member `Std` replaces `error.IO | error.Common | error.Process` in public APIs when all those arms are only needed as "any of these errors."

**Caller side:** A function declared `returns string | Std` accepts:

- `string`
- Any value of type `Std`
- Any value of a **source enum** listed in `Std`'s body (directly or transitively), **widened** to `Std` (zero-cost)

**Return type inference:** If a function returns both `error.IO` and `error.Common` literals and the annotated return is `string | Std`, both widen to `Std`. If the annotated return is `string | error.IO | error.Common`, no widening to `Std` is required; both forms may coexist during migration.

---

## 5. Pattern matching

### 5.1 Exhaustiveness

Matching on type `Std`:

- **Exhaustive** match on `Std` requires either:
  - a pattern covering every variant of `Std`, or
  - an **`else`** arm, or
  - **domain wildcard** patterns (section 5.2) that cover all source members and local variants.

### 5.2 Domain wildcards (recommended for v1)

```doxa
match err {
    Std.IO.* then handle_io(err),       # err narrowed to error.IO
    Std.Common.* then handle_common(err),
    else then unreachable,
}
```

**Narrowing rule (normative):** Inside `Std.IO.*`, the bound value is narrowed to type **`error.IO`**. The `*` wildcard matches any variant identity belonging to the `IO` variant set. This is a compile-time membership check and requires no runtime overhead beyond what `match` already does.

### 5.3 Single-variant patterns

```doxa
match err {
    Std.IO.NotFound then …,
    Std.Common.OutOfMemory then …,
    else …,
}
```

### 5.4 `match` on unions `T | Std`

Same as today: arms may be `string`, `Std`, or specific `Std.Q.V` patterns. When matching `Std` as a whole, use `Std then …` and handle inside, or use wildcards / `else`.

---

## 6. Collisions and ill-formed programs

| Situation | Verdict |
|---|---|
| Two source members with the same default qualifier | **Error**. |
| Same variant identity reachable through different qualified paths (e.g. direct `error.IO` and transitive `error.IO` via `Std`, or two sources sharing a variant) | **Error**. |
| Local variant name equals a qualifier name | **Error** (ambiguous). |
| Local variant simple name equals a source variant's simple name (different qualifier) | **Warning** (shadowing); full qualified names differ. |
| Duplicate **simple** names across source members | **Allowed**; always use `Set.Qualifier.Variant`. |

---

## 7. `@istype` semantics

With shared variant identity, `x @istype T` checks whether `x`'s active variant identity belongs to `T`'s variant set. This works uniformly for enums and sets.

```doxa
x: Std = Std.IO.NotFound;

x @istype error.IO       → true   (variant identity is from error.IO)
x @istype error.Common   → false  (variant identity is not from error.Common)
x @istype Std            → true   (variant identity is in Std's set)
x @istype AppErr         → false  (unless AppErr includes Std → then true)
```

Key behaviors:

- `x` can be of any enum/set type; `@istype` always checks against the variant identity, not the declared type.
- Transitive membership works: if `AppErr` includes `Std` and `Std` includes `error.IO`, then `x @istype error.IO` is true for `x: AppErr` carrying an IO variant.
- This is the **natural v1 behavior**. No special cases, no "optional sugar."

---

## 8. Runtime representation

### 8.1 Shared variant identity implementation

Each variant is assigned a globally unique identity at compile time. Two possible implementation strategies (compiler's choice):

| Strategy | Variant identity | Size | Notes |
|---|---|---|---|
| **Global discriminant** | Unique integer per variant, assigned at link time | `u16`/`u32` typical | Fastest equality and switch; discriminants may be sparse |
| **Variant descriptor pointer** | Pointer to a static descriptor (name, parent type, payload type info) | Pointer-sized | Richer runtime metadata; slightly slower comparison |

Either strategy satisfies the semantic guarantees. The key invariant: a given source variant has the **same** identity when accessed through any set that includes it, directly or transitively.

### 8.2 Set memory layout

A set value is the variant identity plus an optional payload (if the variant carries data). The size is the same regardless of which set type the value is viewed through.

Widening does not allocate, copy, or re-tag. It is a type-level operation only.

### 8.3 Debugging and printing

Runtime metadata registers **qualified** names for each set's view of a variant. Printing a `Std` value shows `"Std.IO.NotFound"`; printing the same value at type `error.IO` shows `"error.IO.NotFound"`. Both resolve from the same variant identity.

### 8.4 FFI / Zig blocks

Zig blocks remain unaware of Doxa sets unless bridged. Doxa wrappers continue to map Zig/i64 codes to **source** enums or directly to **set** variants; both are valid; the std library should converge on sets at public boundaries.

---

## 9. Standard library convention (normative for std layout)

### 9.1 Module layout

- Keep domain enums in `std/error/error.doxa` or split into `std/error/common.doxa`, `io.doxa`, etc.
- Declare **`public set Std { … }`** in `std/error/std.doxa` (or same file as enums if small).

### 9.2 Public API return types

- **Fallible void / status-only:** `returns Std` (or `returns nothing` with side-channel — not this spec).
- **Fallible value:** `returns T | Std`.
- **Internal** module functions may still return `error.IO | error.Common` if not exported; exported functions use `Std` for consistency.

### 9.3 Migration

1. Introduce `Std` as a set including existing error enums.
2. Widen return types of `public function` from `error.IO | error.Common` to `Std`.
3. Keep `map*Error` returning `Std` or individual types then widen at return site.
4. Update docs and examples.

---

## 10. Grammar sketch (informative)

```text
SetDecl         := 'set' IDENT '{' SetBody '}'
SetBody         := ( SetMember | VariantDecl )*
SetMember       := TypePath
TypePath        := IDENT ( '.' IDENT )*
VariantDecl     := IDENT ( ',' )?   # existing enum variant syntax
```

`TypePath` aligns with how `error.IO` is parsed today (module-qualified custom types).

---

## 11. Open issues (for implementers)

1. **Optional shorthand:** Allow `IO.NotFound` when expecting `Std` without writing `Std.IO.NotFound`.
2. **Same unqualified name, different enums:** Two source enums from different modules with the same unqualified name (e.g. `mod_a.Error` and `mod_b.Error`) cannot currently coexist in a set; a future rename mechanism may lift this restriction.
3. **Deprecation path** for long unions in user code (warning when `error.IO | error.Common` appears in `pub` APIs where a set type exists).
4. **Set member order stability:** changing member order or adding members may reorder global discriminants; document that set discriminants are not ABI-stable across compiler versions unless stabilized explicitly.
5. **Wildcard in set members:** `error.IO.*` as explicit sugar for "all IO variants." Low priority; listing `error.IO` already means all variants.
6. **Payload variance:** when source enums have different payload types for the same variant in different sets — not yet addressed by this spec.

---

## 12. Summary

| Feature | Rule |
|---|---|
| Syntax | `public set Name { SourceType, … }` |
| Qualified names | `SetName.Qualifier.SourceVariant` |
| Widening | Implicit, zero-cost (same variant identity) |
| Narrowing | Explicit `as SourceType` or match wildcards |
| Equality | Compares variant identity; cross-type equality works |
| `@istype` | Checks variant identity membership in target type's variant set |
| Duplicate simple names | Allowed; qualifiers disambiguate |
| Duplicate qualifiers | Ill-formed |
| Transitive sets | Supported; widening chains zero-cost; qualifiers nest (not flatten) |
| Unions | Prefer `T \| Std` over `T \| error.IO \| error.Common \| …` at public std API |

This spec is sufficient to implement parsing, semantic analysis, shared variant identity, coercion rules, and runtime tagging for enum sets in Doxa.
