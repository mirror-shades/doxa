# Enum composition (specification)

This document specifies **enum composition**: composing existing enums into a new enum type by re-exporting their variants under qualified names. The primary motivation is **error handling without privileged error types**—several domain enums (`error.IO`, `error.Common`, …) merge into one portable type (`std.error.Std`) so function signatures use a single enum arm instead of long `A | B | C` unions, while preserving **traceability** (which domain a variant came from).

**Status:** specification (not yet implemented). When implemented, update [syntax.md](syntax.md) and [unions.md](unions.md) to reference this page.

---

## 1. Goals and non-goals

### Goals

- Merge multiple named enums into one **nominal** super-enum for signatures and APIs.
- Keep **source enums** as the canonical definition of variants (single place to document and evolve).
- Make merged variants **traceable** at the type and syntax level (caller knows `IO` vs `Common`).
- Support **implicit widening**: a value of a source enum is assignable where the super-enum is expected.
- Work with existing **typed unions** (`T | E`) and **match** / **`as`** narrowing without a separate error channel.

### Non-goals

- Reusing or overloading the `^` **alias parameter** terminology ([alias.md](alias.md)); this feature is called **enum composition** and uses **`using`** clauses in enum declarations (distinct from **module** `import`).
- Automatic flattening of duplicate **simple** variant names across domains (e.g. multiple `Unexpected`) into one variant; see section 6.
- Changing how Zig blocks report failures (thread-local codes, mappers); this spec is the **Doxa type layer**.

---

## 2. Syntax

### 2.1 Enum declaration with `using` clauses

An enum may include zero or more **`using`** clauses. Each clause brings variants from one **source enum** into the declaring enum’s namespace under a **qualifier** (the last path segment of the source type, unless renamed). **`using` here only appears inside enum bodies** and does not load modules (module loading stays `import` / `module` as today).

```doxa
pub enum Std {
    using error.Common,
    using error.IO,
    using error.Process,
    using error.Method,
}
```

**Qualified variant names** use the qualifier as a prefix segment:

- `Std.Common.InvalidArgument`
- `Std.IO.NotFound`
- `Std.Process.SpawnFailed`

The qualifier defaults to the **unqualified** name of the source enum: `Common`, `IO`, `Process`, `Method`. If two `using` clauses would yield the same qualifier, the program is **ill-formed** unless an explicit rename is used (section 2.2).

### 2.2 Renamed `using`

```doxa
pub enum Std {
    using error.Common,
    using error.IO as Io,   # variants: Std.Io.NotFound, …
}
```

After `as Io`, the qualifier is `Io` (PascalCase identifier). Rules for valid identifiers match ordinary enum/type names.

### 2.3 Wildcard `using` (optional sugar)

If the language accepts a wildcard form, it must be equivalent to listing every variant explicitly:

```doxa
using error.IO.*   # means: same as using error.IO for variant set purposes
```

Implementations may support only `using error.IO` first and add `.*` later; semantics are identical.

### 2.4 Local variants

The declaring enum may add its own variants **after** `using` clauses, or interleaved if the grammar allows; recommended style is `using` clauses first, then locals.

```doxa
pub enum AppErr {
    using std.error.Std,
    UserCancelled,
    ConfigInvalid,
}
```

Local variants are **unqualified** under the declaring enum: `AppErr.UserCancelled`.

### 2.5 Merge built-in (optional alternative syntax)

As an alternative surface syntax, the compiler may accept:

```doxa
pub enum Std = @mergeEnums(error.Common, error.IO, error.Process, error.Method)
```

Semantics are identical to section 2.1 with default qualifiers `Common`, `IO`, `Process`, `Method`. `@mergeEnums` is a **compile-time** construct; it does not exist at runtime as a callable.

**Normative:** at most one of `using` blocks or `= @mergeEnums(...)` should be used per enum declaration; mixing in one declaration is ill-formed unless the spec is extended later.

---

## 3. Name resolution

### 3.1 References inside the same module

- `Std.IO.NotFound` — full path from declaring enum type.
- If unambiguous, implementations **may** allow omitting the enum name when the expected type is `Std` (e.g. in `match` arms); this is **optional** and not required for v1.

### 3.2 References from outside

Use the path exported by the module, e.g. `std.error.Std.IO.NotFound`.

### 3.3 Source enum variants unchanged

`error.IO.NotFound` remains a valid expression of type `error.IO`. No change to existing enums.

---

## 4. Type system

### 4.1 Nominal identity

- Each `enum` declaration introduces a distinct nominal type.
- `Std` is not the same type as `error.IO` or `error.Common`.

### 4.2 Variant typing

Each qualified variant `Std.Q.V` has type **`Std`** (the declaring enum), not `error.Q`.

The source variant `error.IO.NotFound` has type **`error.IO`**.

### 4.3 Subtyping and widening

**Widening (implicit, allowed):** If `S` is a source enum **composed into** super-enum `T` via `using`, then an expression of type `S` is assignable to `T` by mapping the variant to the corresponding `T` variant:

| Expression            | Type       | Widens to type | Value after widen   |
|-----------------------|------------|----------------|----------------------|
| `error.IO.NotFound`   | `error.IO` | `Std`          | `Std.IO.NotFound`    |
| `error.Common.Unexpected` | `error.Common` | `Std`    | `Std.Common.Unexpected` |

Widening is **injective** per composed source enum: each source variant maps to exactly one `T.Q.V`. Implementations implement this as a compile-time coercion or tagged representation (section 8).

**Narrowing (explicit):** From `Std` to `error.IO`:

- **`x as error.IO`** — succeeds when the runtime variant is one of the `IO` variants; else the `else` branch of `as` runs (existing union/enum `as` rules).
- No implicit narrow from `Std` to `error.IO`.

### 4.4 Same super-enum, different qualifiers

`Std.IO.Unexpected` and `Std.Common.Unexpected` are **different** variants of `Std` even if the simple name `Unexpected` repeats in source enums.

### 4.5 Unions

**Union members** are compared nominally for enums (including custom types). A single member `Std` replaces `error.IO | error.Common | error.Process` in APIs when all those arms are only needed as “any of these errors.”

**Subtyping for unions (caller side):** A function declared `returns string | Std` accepts:

- `string`
- Any value of type `Std`
- Any value of a **source enum** listed in `Std`’s `using` clauses, **widened** to `Std` (per section 4.3)

**Return type inference:** If a function returns both `error.IO` and `error.Common` literals and the annotated return is `string | Std`, both widen to `Std`. If the annotated return is `string | error.IO | error.Common`, no widening to `Std` is required; both forms may coexist during migration.

### 4.6 Equality and order

Enum equality compares **declaring type and variant** (after any implicit widen, both sides are the same `Std` variant). Cross-type equality between `error.IO.NotFound` and `Std.IO.NotFound` is **false** unless the language adds an explicit operator; default is **not equal** across types.

---

## 5. Pattern matching

### 5.1 Exhaustiveness

Matching on type `Std`:

- **Exhaustive** match on `Std` requires either:
  - a pattern covering every variant of `Std`, or
  - an **`else`** arm, or
  - **domain wildcard** patterns (section 5.2) that cover all `using` sources and any local variants.

### 5.2 Domain wildcards (recommended for v1 or v1.1)

```doxa
match err {
    Std.IO.* then handle_io(err),       # err narrowed to … see below
    Std.Common.* then handle_common(err),
    else then unreachable,
}
```

**Narrowing rule (normative):** Inside `Std.IO.*`, the bound value may be given type **`error.IO`** (narrowed view) **or** a synthetic “subset of `Std` containing only `Std.IO.*` variants.” Implementations should pick one and document it; recommended for interop with existing helpers: narrow to **`error.IO`** when the arm is only `Std.IO.*`.

If wildcards are not implemented in v1, users use `else` or list variants.

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
|-----------|---------|
| Two `using` clauses with the same default qualifier | **Error** unless one uses `as`. |
| Local variant `Foo` equals a qualifier name `Foo` | **Error** (ambiguous). |
| Local variant simple name equals a source variant’s simple name but different qualifier | **Allowed**; full names differ (`Std.IO.Unexpected` vs `Std.LocalName` if local were `Unexpected` — prefer disallowing local `Unexpected` if it duplicates a qualified path for readability; **implementation choice**: warn or error on shadowing simple names). |
| Duplicate **simple** names across composed source enums | **Allowed**; always use `Std.Q.V`. |

---

## 7. Interaction with `as` and `@istype`

- **`x @istype error.IO`:** true iff `x` is of type `error.IO`, or (if specified) `x` is `Std` and its active variant is in the `IO` image; the latter is **optional sugar** — normative v1: only `error.IO` values test true; widened `Std` values require `x as error.IO` or match on `Std.IO.*`.
- **Recommended v1.1:** `@istype error.IO` is true for `Std` values whose variant is in the `IO` composed set (from `using error.IO`), to reduce friction.

Document the chosen behavior in release notes.

---

## 8. Runtime representation

### 8.1 Tagged representation

Super-enum `Std` is a tagged union at runtime: **discriminant** identifies the variant uniquely across all composed sources and locals. Widening from `error.IO` to `Std` **re-tags** or uses a shared discriminant table.

### 8.2 Debugging and printing

Runtime enum metadata (e.g. registry used for printing) must register **qualified** names for `Std` variants (`"Std.IO.NotFound"` or `"IO.NotFound"` under `Std` — pick one string format and use consistently).

### 8.3 FFI / Zig blocks

Zig blocks remain unaware of Doxa enum merging unless bridged. Doxa wrappers continue to map Zig/i64 codes to **source** enums or directly to **`Std`** variants; both are valid; std should converge on **`Std`** at public boundaries.

---

## 9. Standard library convention (normative for std layout)

### 9.1 Module layout

- Keep domain enums in `std/error/error.doxa` or split into `std/error/common.doxa`, `io.doxa`, etc.
- Declare **`pub enum Std { using … }`** in `std/error/std.doxa` (or same file as enums if small).

### 9.2 Public API return types

- **Fallible void / status-only:** `returns Std` (or `returns nothing` with side-channel — not this spec).
- **Fallible value:** `returns T | Std`.
- **Internal** module functions may still return `error.IO | error.Common` if not exported; exported functions use `Std` for consistency.

### 9.3 Migration

1. Introduce `Std` with `using` clauses referring to existing enums.
2. Widen return types of `pub function` from `error.IO | error.Common` to `Std` (and similar).
3. Keep `map*Error` returning `Std` or individual types then widen at return site.
4. Update docs and examples.

---

## 10. Grammar sketch (informative)

```text
EnumDecl        := 'enum' IDENT '{' EnumBody '}'
EnumBody        := ( UsingClause | VariantDecl )*
UsingClause     := 'using' TypePath ( 'as' IDENT )? ( '.*' )?
TypePath        := IDENT ( '.' IDENT )*
VariantDecl     := IDENT ( ',' )?   # existing enum variant syntax; exact comma rules follow current Doxa
```

Align `TypePath` with how `error.IO` is parsed today (module-qualified custom types).

---

## 11. Open issues (for implementers)

1. **Optional shorthand:** Allow `IO.NotFound` when expecting `Std` without writing `Std.IO.NotFound`.
2. **`@istype` for subsets** of `Std` (section 7).
3. **Deprecation path** for long unions in user code (warning when `error.IO | error.Common` appears in `pub` APIs).
4. **Binary stability:** changing `using`-clause order or adding sources may reorder discriminants; document that `Std` is not ABI-stable across compiler versions unless stabilized explicitly.

---

## 12. Summary

| Feature | Rule |
|--------|------|
| Syntax | `using SourceEnum` / `using SourceEnum as Qual` inside enum body (not module `import`) |
| Qualified names | `DeclaringEnum.Qualifier.SourceVariant` |
| Widening | `SourceEnum` → super-enum implicitly where super-enum is expected |
| Narrowing | Explicit `as SourceEnum` or match wildcards |
| Duplicate simple names | Allowed; qualifiers disambiguate |
| Duplicate qualifiers | Ill-formed without `as` |
| Unions | Prefer `T \| Std` over `T \| error.IO \| error.Common \| …` at public std API |

This spec is sufficient to implement parsing, semantic analysis, coercion rules, and runtime tagging for merged enums in Doxa.
