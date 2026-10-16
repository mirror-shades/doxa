# Memory Management

Doxa's memory system uses arena-based scoping to provide automatic management that is consistent, predictable, and fast. Doxa does not use garbage collection. It relies on arena allocations tied to block scopes.

## Core Principles

### Every Block is a Lifetime Boundary

The fundamental rule is that **every `{` creates a lexical lifetime boundary and every `}` ends it.** The compiler may represent that boundary without creating a physical arena when the block contains only primitives, borrows, or other trivially managed values. When managed storage is required, the arena is entered and all memory allocated within that scope is freed in O(1) time.

```doxa
fn registerEmployee(name :: string, age :: int) returns Employee
{ # arena scope created for this function body
    return $Employee {name is name, age is age}
} # arena scope bulk-freed — returned value is copied (or constructed) into the caller's arena
```

Nothing can outlive its allocating scope. This means:
- Any value returned from a scope must be deep-copied into the parent scope's arena.
  The compiler satisfies this either by cloning after the fact or, for a value
  built directly in the `return`, by constructing it in the parent arena.
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

Because the anonymous block is its own managed lifetime, the large list is freed immediately after `result` is extracted, rather than living until the function returns. Whatever leaves a block (a value stored to an outer variable, the block's own value, a `return`) is copied out before its arena is reclaimed, as it is out of a function. A block that allocates nothing opens no arena. `if`, `match` and `while` bodies written with braces are blocks too; a loop's body reuses its loop's arena, reset every iteration.

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

The compiled backend implements the arena model directly. Each scope is an arena the program holds as a value (`src/runtime/scope_arena.zig`, a tree of `std.heap.ArenaAllocator` nodes), and every allocation names the arena it is made in — there is no implicit current scope. The compiler opens one arena per function body, per loop body and per block (`doxa_scope_enter(parent)`, reset each iteration with `doxa_scope_reset`, closed with `doxa_scope_exit`), and drops every arena nothing allocates in. The program root scope (`doxa_scope_root`) is created once at the start of `doxa_program_main` and is never freed — the OS reclaims it at process exit. Exited scope nodes are not returned to the OS: they are rewound and kept on a bounded spare list (at most 64 nodes, each keeping at most 64 KiB of its buffers), so entering a scope does not cost a page allocation and the memory held for reuse stays bounded.

Loop bodies have two logical lifetimes: a loop scope for state that persists between iterations and a body scope for iteration-local values. The body arena is entered once, reset at each iteration boundary, and exited once when the loop ends. `continue` resets the body before continuing; `break` exits both scopes; `return` unwinds all active scopes after preparing any escaping result.

All heap values are allocated from the **current** scope arena:

| Value kind | Allocation path |
| --- | --- |
| Arrays | `doxa_array_new` → `doxa_scope_alloc` (header + data) |
| Strings | `doxa_str_clone`, `doxa_str_concat`, `doxa_substring`, … |
| Structs | `doxa_scope_alloc` (contiguous `i64` field array) |
| Maps | `doxa_map_new` (`map_runtime.zig`) |

Because a value can only be reached while its allocating scope is on the stack, values that escape a scope are **rehomed into the destination scope**:

- **Rehome on escape.** Heap values are copied only when lifetime or value-semantics require it:
  - **Globals** live in the program-root arena. Storing a new value into a global rehomes it there. In-place mutations (`@push`, `arr[i] is x`, `n.field is …`) write through the existing root-owned object — they do not snapshot it.
  - **Structs** keep identity when they already outlive the destination (a global array element, a same-scope struct). An unknown allocation is treated as long-lived, so `each n in neutrons { n.x_pos += 1 }` mutates the element. By-value parameters always snapshot.
  - **Arrays** preserve identity when their owning arena already outlives the destination and are cloned only when the source could be destroyed first. Explicit snapshot operations still produce independent arrays.
  - **Strings** never carry identity: they are immutable, so a store that does not already outlive its destination is simply a copy. Because a copy is observationally identical to keeping the original, the compiler decides every string store statically — there is no runtime scope lookup for strings.

- **Clone on return.** A function frees its body scope on `return`; heap return
  values are copied into the caller's scope first (`doxa_str_clone_at`,
  `doxa_array_clone_at`, `doxa_struct_clone_at`, `doxa_clone_doxa_value_at`).
  A value constructed *directly* in the `return` (a struct or array literal) is
  an exception: its destination is statically one region up the nest, so it is
  allocated in the caller's arena at construction instead of being cloned there
  (`doxa_scope_alloc_at`, `doxa_array_new_at`, `doxa_struct_register_at`). The
  result is identical — a fresh object owned by the caller — with the deep copy
  folded into construction.
- **Clone on element store.** Pushing or assigning a heap element into an array re-homes it into the array's own arena (tracked in the `ArrayHeader.scope` field), which is what makes `^`-aliased arrays safe — structs built in a callee are copied into the caller's array before the callee tears down.

Structs and arrays are cloned recursively: string fields and array fields are re-cloned into the destination arena, so a deep copy is genuinely independent of the source.

## Seeing where memory goes

`--emit-memory` writes the compiler's own account of a program's memory beside
the binary, and `doxa report` prints it:

```
doxa run app.doxa --emit-memory
doxa report app
```

- **Allocations:** every allocation site, its type, the arena it lands in (a
  function's, a loop's, a block's, the caller's, a `^` argument's, the root),
  and when that arena is reclaimed, with the line of each explicit `return`
  that closes it.
- **Copies kept:** each copy the compiler placed, a `clone` it decided
  statically or a `rehome` it left to a runtime check, with what the copy is
  for (returned, stored to a longer-lived variable, carried out of a loop, …).
- **Copies removed:** copies the compiler proved unnecessary, because the
  value already lives where it is going or nothing used it.
- **Arenas dropped:** arenas nothing allocates in, and `@caller` parameters a
  function never needs, which the compiler removed.
- **Retention:** what a loop makes every iteration that no iteration
  reclaims: a value kept by a global (the root arena grows by it every
  iteration, until the program ends), one carried across iterations in a
  variable outside the loop (it stays until the function returns; a block
  around the loop and that variable reclaims it at the block's `}`), or one
  that lands in an enclosing loop's arena. Built and run with
  `--instrument=memtrace` too, each row also says what it allocated in that
  run.

The rows come from the decisions the compiler made, so they always match the
program it built. The flag changes nothing in the program itself.

`--instrument=memtrace` measures the same sites while the program runs, and
`doxa report` adds four tables:

- **Allocations by site:** the bytes each site asked for, how many
  allocations that took (a deep copy counts its nested ones to the copy's
  site), and the most the site held at once.
- **Arenas:** for each place that opens an arena, how many it opened and reset,
  the most one of them took from the system, and the bytes released when they
  were reclaimed, plus what the root arena holds at exit (what globals retain).
- **Root growth by site:** the bytes each site put into the root arena,
  which is never reclaimed: which code grew what globals retain.
- **Runtime-checked copies:** for each `rehome` of an array or struct, how
  often it copied and how often the value already lived where it was going.

A traced binary runs slower and is a diagnostic build: never time it. Without
the flag nothing is counted and the program is unchanged.

`--instrument=memcheck` checks the memory model while the program runs. When
an arena is reset or closed, its memory is filled with a poison byte (`0xDD`)
and kept aside, never reused. Every read of a string, array, struct or map
first checks that it is not in such memory, and a read that is faults:

```
Read of memory its arena reclaimed at app.doxa:12:9
```

That can happen only through a compiler or runtime bug, since the compiler
proves every value outlives its uses, so it exits with status 2, like
`unreachable`. Kept memory is capped at 256 MiB, oldest freed first. A stale
read into memory freed past the cap is not caught. Combine it with the other
members (`--instrument=memcheck,memtrace`); it is a diagnostic build too.

## Performance Characteristics

### Advantages
- **Fast allocation:** Arena allocation is bump-pointer, extremely efficient
- **Bulk deallocation:** Scopes clean up in O(1) by rewinding their arena; the node and up to 64 KiB of its buffers are kept for the next scope
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
| **Rehome decisions follow lifetime, not access** — Arrays preserve identity when their owner outlives the destination, while explicit snapshots remain independent. `@push` / index assignment mutate the header in place and keep it. Struct stores keep identity when the source already outlives the destination, which is what makes `each n in arr { n.field is ... }` mutate the element. Strings are immutable, so they are simply copied when they do not already outlive the destination; identity never matters. | Only an actual escape needs a copy. Globals and longer-lived containers still receive storage that survives the source scope. |
| **A described struct carries a two-word header** — its descriptor and its owning arena, just before its fields, as an array header carries its arena. A scalar-only struct that is never reflected and never crosses a container/union/signature boundary has no descriptor and no header, and is cloned with a typed word copy (B2/B3-lite). | Needed for `@string(struct)`, descriptor-backed recursive cloning, and placing a field's string or box in the struct's own arena. 16 bytes per described struct, reclaimed with its arena; there is no global registry. |
| **Empty array literals require an element type** — `var x is []` is a compile error (E6019); the element type must come from an annotation (`var x :: int[] is []`) or surrounding context (`x = []`, `foo([])`, `return []`). | An unannotated empty literal has no element type to infer, so it would otherwise carry tag 255 and be treated as struct pointers by the runtime. |

## Edge Cases

### Cyclic References
Scope cleanup prevents cycles by design. A value's arena outlives all references to it.

### Dangling Pointers
Objects can't outlive their scope. There are no pointers in user code; aliases are scoped to the call.

### Memory Leaks
Scopes guarantee cleanup. A function's arena is freed when it returns, and a loop's at the end of every iteration.

### Large Allocations
Controlled through scope-based cleanup timing: a loop body's allocations are reclaimed every iteration, a block's at its `}`, and a helper function's when it returns.

### Analysis-Phase Scopes

During semantic analysis, a parallel scope tree is maintained by `MemoryManager` / `ScopeManager` (see `src/utils/memory.zig`). These analysis scopes track variable declarations, types, and aliasing for type-checking and name resolution. They are independent of the runtime arenas and are cleaned up after IR generation.

### Lexer-Lifetime Borrowing

The AST borrows lexer-owned buffers: string literal contents and the lexemes of tokens inside format-string placeholders (`{@string(x)}`) are slices into a `LexicalAnalyzer`'s buffers. A lexer must not be freed while the AST it produced is still alive. The main-file lexer is deliberately leaked into the analysis arena for this reason; module and placeholder lexers transfer ownership of their string buffers (`takeOwnershipOfStrings`) before deinit. The analysis arena reclaims everything at program end.
