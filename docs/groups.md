# Groups

This document specifies **groups**: a nominal type that merges existing enums, structs, and other groups into a single type, re-exporting their members under qualified names. A group is a **named tagged union** — it provides a single type arm for function signatures and APIs without creating new values.

**Status:** specification. Replaces the prior `set` design. When implemented, update [syntax.md](syntax.md) and [unions.md](unions.md) to reference this page.

---

## 1. Design invariants

| Rule | Meaning |
|------|---------|
| Members are existing types or other groups | A group contains `enums`, `structs`, and `groups` declared elsewhere |
| Group membership is flattened | Transitive group members are expanded into a flat member list at the declaration site |
| Groups do not create new values | `Error.IO.NotFound` is the same value as `IO.NotFound` |
| A type appears at most once | Deduplication during flattening; reaching the same type through multiple paths is harmless |
| Cycles are illegal | A group may not include itself directly or transitively |
| Nominal type | Each `group` declaration introduces a distinct type; two groups with identical members are different types |

---

## 2. Syntax

### 2.1 Group declaration

A group is declared with the `group` keyword. The body lists member types.

```doxa
public group Error {
    CommonError,
    IOError,
    CompilerError,
    FileError,
}
```

Members are referenced by their unqualified or module-qualified name. Each member contributes its variants/fields under a **qualifier** — its unqualified type name.

```doxa
public group Error {
    error.CommonError,
    error.IOError,
    error.CompilerError,
    structs.FileError,
}
```

### 2.2 Transitive groups

A group may include another group. All members of the included group are **flattened** into the declaring group.

```doxa
public group CoreError {
    IOError,
    CommonError,
}

public group AppError {
    CoreError,      // flattened → IOError, CommonError
    UserCancelled,
    ConfigInvalid,
}
```

`AppError` expands to `{ IOError, CommonError, UserCancelled, ConfigInvalid }`. There is no nesting in the expanded member list.

### 2.3 Qualified access

Members are accessed through the group name with the qualifier:

```doxa
Error.IOError.NotFound
AppError.UserCancelled           // local variant — unqualified under the group
```

When a variant name is unique across all group members, it may be accessed directly:

```doxa
Error.NotFound                   // OK if NotFound exists in exactly one member
```

When ambiguous, qualified access is required:

```doxa
group G {
    A,    // enum with variant X
    B,    // enum with variant X
}
G.X                              // ERROR: ambiguous — X exists in both A and B
G.A.X                            // OK
```

---

## 3. Type system

### 3.1 Nominal identity

Each `group` declaration introduces a distinct nominal type. `AppError` is not the same type as `CoreError`, `IOError`, or `UserCancelled`. Two groups with identical member lists are distinct types.

### 3.2 Member identity preserved

A value's identity is determined by its source type — not by the group that wraps it. `Error.IOError.NotFound` and `IOError.NotFound` are the same value.

| Expression | Type | Identity |
|---|---|---|
| `IOError.NotFound` | `IOError` | `IOError::NotFound` |
| `Error.IOError.NotFound` | `Error` | `IOError::NotFound` (same) |
| `AppError.IOError.NotFound` | `AppError` | `IOError::NotFound` (same) |

### 3.3 Widening and narrowing

**Widening (implicit):** A value of a member type is assignable where the group type is expected. The compiler inserts a tag to track which member the value belongs to at runtime.

```doxa
function open(path: string) -> string | Error {
    if path is "" then return IOError.NotFound  // widens to Error
    if path is "/" then return FileError { path } // widens to Error
    return "ok"
}
```

**Narrowing (explicit):** From a group type to a member type via `match` or `as`:

```doxa
x as IOError          // succeeds if x is an IOError member; else the `else` branch runs
```

No implicit narrowing from a group type to a member type.

### 3.4 Equality

Values compare equal when they carry the same source type and same payload.

```
Error.IOError.NotFound == IOError.NotFound     → true
IOError.NotFound == FileError { path: "x" }    → type error (different members)
```

Cross-member equality is a compile error, not `false`.

### 3.5 Unions

A single group member replaces multiple individual arms in return types:

```doxa
// Before
function read() -> string | IOError | CommonError | CompilerError

// After
function read() -> string | Error
```

A function declared `returns string | Error` accepts `string`, any `Error` value, or any value of a member type (widened to `Error`).

A union holds a group's members, not the group: `string | Error` is the same type as `string | IOError | CommonError | CompilerError`. A value keeps the member it is through the union and back — narrowing to `Error` with `as` or `match` gives the same `Error` value that went in, and `Error then` is a test that the union holds any of `Error`'s members. Diagnostics and `?` still name the group as written (`string | >Error`).

---

## 4. Pattern matching

### 4.1 Exhaustiveness

Matching on a group must cover every member. This is a **flat** check over the expanded member list.

```doxa
group Error { IOError, CommonError, FileError }

match err {
    IOError.NotFound then { }
    IOError.PermissionDenied then { }
    // IOError.EOF missing → not exhaustive
    CommonError.* then { }       # wildcard covers all CommonError variants
    FileError { path } then { }
}
```

An exhaustive match requires either:
- a pattern for every variant in every enum member, plus every struct member, or
- a `*` wildcard for any remaining enum members, or
- an `else` arm

### 4.2 Enum patterns

```doxa
match err {
    Error.IOError.NotFound then { }      # single variant
    Error.IOError.* then handle_io(err), # all IOError variants (narrows to IOError)
    else { }
}
```

Inside `Error.IOError.*`, the bound value is narrowed to `IOError`.

### 4.3 Struct patterns

Struct members are matched with destructuring syntax:

```doxa
match err {
    Error.FileError { path } then print(path),
    Error.ConfigError { key, value } then { }
}
```

The struct is narrowed to its member type, and fields are available directly.

### 4.4 Matching on unions containing groups

```doxa
match result {
    string then use(result),
    Error then handle(result),      # catch-all for the group
    Error.IOError.* then { }
}
```

---

## 5. Collisions and ill-formed programs

| Situation | Verdict |
|-----------|---------|
| Two members with the same qualifier after flattening | **Error** |
| Unqualified name exists in more than one member | **Error** (ambiguous access) |
| Cycle in group membership | **Error** |
| Empty group (no members) | **Error** |
| Group member is not a declared type | **Error** |

---

## 6. Runtime representation

A group value has a hidden tag to identify which member it belongs to, followed by the member's payload.

```
Group value = [member_index: u32] [payload: union { enum_variant, struct_instance }]
```

This is identical in shape to how tagged unions already work. The difference is that group members are named and traceable, whereas union arms are anonymous. A group value stored in a union is re-indexed to the union's position for the same member, and back when it is narrowed to the group, so the index always names a concrete member type.

The member index is stable for the lifetime of the group declaration. Reordering members changes the index and is an ABI break.

---

## 7. Grammar sketch

```text
GroupDecl       := 'group' IDENT '{' GroupBody '}'
GroupBody       := ( GroupMember )*
GroupMember     := TypePath  (',' | NEWLINE)*
TypePath        := IDENT ( '.' IDENT )*
```

---

## 8. Differences from the prior `set` design

| Aspect | `set` (old) | `group` (new) |
|--------|-------------|---------------|
| Members | Enums only | Enums, structs, other groups |
| Local variants | Supported | Not supported — everything is a member type |
| Transitivity | Nested qualifier chains (`AppErr.Std.IO.NotFound`) | Flattened (`AppError.IOError.NotFound`) |
| Duplicate types | Error | Deduplicated silently |
| Runtime shape | Enum discriminant only (zero-cost widen) | Tag + payload (tagged union) |
| `is_set` flag | Unused | Not needed — flattened at parse/semantic time |

---

## 9. Implementation checklist

- [x] Rename `SET_TYPE` token to `GROUP_TYPE` and register the `group` keyword in the lexer.
- [x] Rename `SetDecl`, `SetSource`, `SetMember` AST nodes to `GroupDecl`, `GroupMember` throughout `ast.zig`, parser, semantic, and HIR layers.
- [x] Update `parseSetDecl` to `parseGroupDecl` and remove local variant parsing from the group body.
- [x] Create a `GroupTable` global registry with `GroupId` and flat `Member` entries (`qualifier`, `kind`, `id`).
- [x] Add `HIRType.Group(GroupId)` and remove `HIRType.Set(EnumId)` from `soxa_types.zig`.
- [ ] Unify the duplicated `CustomTypeInfo` definition across `types/types.zig` and `type_system.zig` into a single source of truth.
- [x] Implement flattening: when a group member is another group, expand its members into the declaring group's member list with deduplication by type identity.
- [x] Implement cycle detection by tracking visited group IDs during expansion.
- [ ] Implement ambiguous-name detection by collecting all variant/field names across expanded members and flagging duplicates.
- [x] Add `group_instance: HIRGroup { member_index, payload }` to `HIRValue` for runtime group representation.
- [x] Implement implicit widening from member types to their enclosing group in the type checker (`typeIsGroupMember`, `typeMatchesUnionMember` in `unifyTypes`).
- [x] Emit the group tag at every construction site so a member value widens to its group on assignment, return, and call argument (`buildDoxaValue`, `isBoxedMemberType`).
- [x] Implement `match` on a group subject: `GroupCheck` decides an arm by boxed member index, and an arm naming a variant (`Group.Member.Variant`) additionally compares the extracted payload.
- [x] Implement domain wildcard patterns (`Group.Member.*`) that narrow the matched value to the member type.
- [x] Implement struct destructuring patterns (`Group.Member { field }`) inside `match` arms, including binding the fields as arm locals.
- [x] Implement `match` exhaustiveness checking across the flat expanded member list per §4.1: an arm list that leaves a member uncovered is a compile error (`E1033`) unless it ends in `else`.
- [ ] Implement `as` narrowing from a group type to a member type as a value transform. The cast decides correctly and the semantic side narrows the bound name, but reading the narrowed binding inside the branch is broken — and so is reading a narrowed union binding on `master`, so the defect sits in the shared narrowing path rather than in groups.
- [ ] Implement cross-member equality as a compile error.
- [x] Add group type handling to the native LLVM backend: `GroupCheck` and `GroupExtractPayload` lower against `%DoxaValue`, and group members are wrapped like union arms.
- [x] Validate that `registerSpecificSymbol` uses the correct `.Group` kind instead of `.Enum` for imported group symbols.
- [x] Fix the fixed-2 allocation in `getGroupMemberNames` (was `getSetSourceMemberNames`) to iterate actual expanded members.
- [ ] Give the group-cycle error (`E1003`) a real source location; it currently reports `:0:0`.
