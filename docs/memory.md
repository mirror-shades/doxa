# Memory Management

Doxa's memory system uses arena-based scoping to provide automatic management that is consistent, predictable, and fast. Doxa does not use garbage collection. It relies on arena allocations tied to block scopes.

## Core Principles

### Every Block is an Arena

The fundamental rule: **every `{` creates a new arena, every `}` bulk-frees it.** This includes anonymous blocks, loop bodies, `if`/`else` branches, `match` cases, and function bodies. Each curly brace pair establishes a scope with its own arena allocator. When the closing brace executes, all memory allocated within that scope is freed in O(1) time.

```doxa
fn registerEmployee(name :: string, age :: int) returns Employee
{ # arena scope created for this function body
    return $Employee {name is name, age is age}
} # arena scope bulk-freed — returned value is deep-copied to caller
```

Nothing can outlive its allocating scope. This means:
- Any value returned from a scope must be deep-copied into the parent scope's arena.
- There are no dangling pointers, no use-after-free, no memory leaks.
- Deep copies are the cost of this guarantee; aliases are the escape hatch.

### Anonymous Block Cleanup

The easiest mental model: curly braces are where memory arenas are created and destroyed. Anonymous blocks are a tool for controlling memory lifetime within a function.

```doxa
fn processData(id :: int) returns Employee | nothing {
    var result :: Employee | nothing is nothing
    { # anonymous arena created
        const list is loadEmployeeList()   # large allocation
        result is list.find(id)            # deep-copied out to parent arena
    } # arena bulk-freed — list is gone
    return result
}
```

Because the anonymous block is its own arena, the large list is freed immediately after `result` is extracted, rather than living until the function returns.

### Parameter Aliasing

Pointers don't play well with arena scoping — a pointer into a freed arena is a dangling pointer. Instead, Doxa uses **parameter aliasing** (`^`). When you declare a parameter with `^`, the function accesses the *caller's* variable directly. This is the mechanism for mutation without pointers.

```doxa
fn incByReturn(value :: int) returns int {
    return value + 1   # caller gets a copy in its arena
}

fn incByAlias(^value :: int) {
    value += 1          # caller's variable mutated in place, no allocation
}
```

Aliases can be chained — `levelOne(^x)` calls `levelTwo(^x)` — passing the alias reference through the call stack.

**Safety rules:**
- Only full variables can be aliased (no temporaries, fields, or array elements)
- Explicit opt-in at call site with `^`
- No alias duplication or mixing aliased/by-value parameters
- Exclusive borrow during call (the aliased variable cannot be read or written elsewhere)

## Native Runtime Model

The compiled backend implements the arena model directly. The runtime keeps a **scope-arena stack** (`src/runtime/scope_arena.zig`): a linked list of `std.heap.ArenaAllocator` nodes. The IR printer emits a call to `doxa_scope_enter()` at each `EnterScope` HIR instruction and `doxa_scope_exit()` at each `ExitScope`. The program root scope is pushed once at the start of `doxa_program_main` and is never freed — the OS reclaims it at process exit.

All heap values are allocated from the **current** scope arena:

| Value kind | Allocation path |
| --- | --- |
| Arrays | `doxa_array_new` → `doxa_scope_alloc` (header + data) |
| Strings | `doxa_str_clone`, `doxa_str_concat`, `doxa_substring`, … |
| Structs | `doxa_scope_alloc` (contiguous `i64` field array) |
| Maps | `doxa_map_new` (`map_runtime.zig`) |

Because a value can only be reached while its allocating scope is on the stack, values that cross a scope boundary are **deep-copied into the destination scope**:

- **Clone on store.** Storing a heap value into a variable copies it into the persistent scope (the function scope, or the program root at top level), so it survives the exit of the block that created it.
- **Clone on return.** A function frees its body scope on `return`; heap return values are copied into the caller's scope first (`doxa_str_clone_at`, `doxa_array_clone_at`, `doxa_struct_clone_at`, `doxa_clone_doxa_value_at`).
- **Clone on element store.** Pushing or assigning a heap element into an array re-homes it into the array's own arena (tracked in the `ArrayHeader.scope` field), which is what makes `^`-aliased arrays safe — structs built in a callee are copied into the caller's array before the callee tears down.

Structs and arrays are cloned recursively: string fields and array fields are re-cloned into the destination arena, so a deep copy is genuinely independent of the source.

## Performance Characteristics

### Advantages
- **Fast allocation:** Arena allocation is bump-pointer, extremely efficient
- **Bulk deallocation:** Scopes clean up in O(1) via a single `arena.deinit()`
- **Cache-friendly:** Related data allocated contiguously within the arena
- **No GC pauses:** Deterministic cleanup timing, scope-exit bounded
- **Memory safe:** No dangling pointers, no use-after-free, no leaks by construction

### Trade-offs
- **Memory usage:** Arenas hold all memory until scope exit (no incremental free)
- **No heap compaction:** Memory fragmentation within arenas
- **Scope lifetime:** Objects can't outlive their allocating scope
- **Deep copy cost:** Return values are copied between scopes

### Known Trade-offs

| Issue | Rationale |
|-------|-----------|
| **Arena buffer accumulation on resize** — When an array or map grows, the old buffer is abandoned in the arena. Repeated resizes accumulate dead buffers. | All buffers bulk-freed on scope exit in O(1). |
| **Clone-on-store is eager** — Every store of a heap value into a variable copies it, even a same-scope store. | Simpler than tracking arena ownership in LLVM IR. Cloning is bump-pointer allocation + memcpy, so cost is low. |
| **Struct clones are registered, not garbage-collected** — `struct_registry` entries (address → descriptor) are appended, never removed. | Needed for `@string(struct)` and recursive cloning. Bounded by the number of structs created; acceptable for program lifetime. |
| **Empty array literals require an element type** — `var x is []` is a compile error (E6019); the element type must come from an annotation (`var x :: int[] is []`) or surrounding context (`x = []`, `foo([])`, `return []`). | An unannotated empty literal has no element type to infer, so it would otherwise carry tag 255 and be treated as struct pointers by the runtime. |

## Edge Cases

### Cyclic References
Scope cleanup prevents cycles by design. A value's arena outlives all references to it.

### Dangling Pointers
Objects can't outlive their scope. There are no pointers in user code; aliases are scoped to the call.

### Memory Leaks
Scopes guarantee cleanup. Every `{` is matched by a `}` that frees the arena.

### Large Allocations
Controlled through scope-based cleanup timing. Anonymous blocks can be used within a scope to create narrower arenas that will clean up large allocations.

### Analysis-Phase Scopes

During semantic analysis, a parallel scope tree is maintained by `MemoryManager` / `ScopeManager` (see `src/utils/memory.zig`). These analysis scopes track variable declarations, types, and aliasing for type-checking and name resolution. They are independent of the runtime scope-arena stack and are cleaned up after IR generation.
