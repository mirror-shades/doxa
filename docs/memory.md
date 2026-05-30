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

## Allocation Strategy

Values fall into categories based on where they live:

### Scope-Owned Values (Runtime)

During execution, all complex runtime values (arrays, maps, structs, strings, enums) are allocated from the **current scope's arena**. Multiple values created in the same scope share one arena and are freed together when the scope exits. Struct fields and map entries use **allocation pooling** — all field/entry values are placed in a single contiguous slice alongside metadata, eliminating per-element heap allocations and improving cache locality.

### Frame-Owned Variables (Runtime)

Function-local variable **slots** are allocated from a per-frame arena. The frame arena is created on function entry and bulk-freed on return. This separation exists because the frame is created before the function's scope (the `EnterScope` bytecode runs after the frame is established), so the frame cannot use the scope arena for its own storage.

### Module-Owned Values

Module global variables use a persistent allocator that lives for the program's duration. These values are never mutated in-place within the module's arena; any mutation triggers a deep copy into a scope or frame arena.

### Analysis-Phase Scopes

During semantic analysis, a parallel scope tree is maintained by `ScopeManager` (see `src/utils/memory.zig`). These analysis scopes (`ScopeKind.analysis`) track variable declarations, types, and aliasing for type-checking and name resolution. They are separate from runtime scopes (`ScopeKind.runtime`) which use pure O(1) arena cleanup.

### Compiler Backend

In the compiled output, each `EnterScope` HIR instruction emits a call to `doxa_scope_enter()` (in `src/runtime/arena.zig`), creating a child arena backed by `std.heap.ArenaAllocator`. Each `ExitScope` emits `doxa_scope_exit()`, popping back to the parent arena. Heap values stored into variables are deep-copied via `emitCloneForStore` (cloning strings via `doxa_str_clone`, arrays via `doxa_array_clone`, structs via `doxa_struct_clone`, maps via `doxa_map_clone`). The root scope is never destroyed — the OS reclaims it at process exit.

## Performance Characteristics

### Advantages
- **Fast allocation:** Arena allocation is bump-pointer, extremely efficient
- **Bulk deallocation:** Scopes clean up in O(1) via single `arena.deinit()`
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
| **Arena buffer accumulation on resize** — When an array or map grows, the old buffer is abandoned in the arena. Repeated resizes accumulate dead buffers. | Matches VM behavior. All buffers bulk-freed on scope exit in O(1). |
| **Functions with explicit `return` skip `ExitScope`** — The bytecode generator omits `ExitScope` when a function has a return statement. Scopes accumulate on `scope_stack` until VM reset/deinit. | Bounded per program execution, cleaned at VM deinit. Proper unwinding requires deep-copying frame locals that reference scope memory before deinit — deferred. |
| **Runtime scopes allocate empty hashmaps** — `Scope.initRuntime` initializes `variables`/`name_map` hashmaps that are never populated by the VM. ~200 bytes of arena waste per scope. | Making them optional adds `.?` overhead to all analysis code paths. |
| **Compiler clone-on-store is always-on** — Every `StoreVar`/`StoreDecl` clones heap values, even same-scope stores. | Simpler than tracking arena ownership in LLVM IR. Cloning is bump-pointer allocation + memcpy, so cost is low. |

### Deferred: Return Value Move Semantics

When a function returns a complex value (array, struct, map) and the callee's scope persists (ExitScope skipped for functions with `return`), the return value's inner pointers remain valid in the callee's scope arena. Pushing the original value (move) instead of deep-copying avoids O(n) allocation + memcpy. Infrastructure (`isScopeOnStack` helper) was prototyped. The optimization is correct for values in persistent scopes but interacted poorly with alias parameters in the brainfuck test — aliased variables may reference scope memory that the move incorrectly assumes is still valid. Deferred pending deeper alias analysis.

## Edge Cases

### Cyclic References
Scope cleanup prevents cycles by design. A value's arena outlives all references to it.

### Dangling Pointers
Objects can't outlive their scope. There are no pointers in user code; aliases are scoped to the call.

### Memory Leaks
Scopes guarantee cleanup. Every `{` is matched by a `}` that frees the arena.

### Large Allocations
Controlled through scope-based cleanup timing. Anonymous blocks can be used within a scope to create narrower arenas that will clean up large allocations.

### Two-Phase Execution
Doxa runs analysis (semantic checks, type inference, IR generation) then execution (VM or compiled). Analysis scopes are managed by `MemoryManager` / `ScopeManager` and are cleaned up after IR generation. Runtime scopes are managed by the VM's scope stack and cleaned up on scope exit during execution.
