# Memory Management

Doxa's memory system combines **scope-based automatic cleanup** with **explicit mutation controls** to provide memory safety without garbage collection overhead.

## Core Principles

### Scope-Based Cleanup

Every scope (function, block, loop body) has its own **arena allocator**. When a scope exits, its entire arena is deallocated in bulk:

```doxa
function processData() {
    var temp :: Buffer is allocateLargeBuffer()  // Allocated in function arena
    transformData(temp)
    return temp  // temp gets copied to caller's scope
} // Function arena deallocated here - temp freed
```

### Parameter Aliasing

Functions can mutate caller variables through **alias parameters** (`^param`):

```doxa
fn increment(^value :: int) {
    value += 1  // Directly modifies caller's variable
}

var x = 5
increment(^x)  // x becomes 6
```

**Safety rules:**
- Only full variables can be aliased (no temporaries, fields, or array elements)
- Explicit opt-in at call site with `^`
- No alias duplication or mixing aliased/by-value parameters
- Exclusive borrow during call

## Return Values

Return values are allocated directly in the caller's scope to avoid copying:

```doxa
function createData() -> Data {
    var result :: Data is Data{}  // Allocated in createData's arena
    populateData(result)
    return result  // Gets copied to caller's scope
}

function caller() {
    var data is createData()  // Space allocated here before call
    useData(data)
}
```

## Resource Management with Defer

Use `defer` for guaranteed cleanup, even during errors:

```doxa
function processFile(filename :: string) {
    var file is openFile(filename)
    defer file.close()  // Guaranteed to run when scope exits

    var data is readFile(file)
    processData(data)   // If this throws, file still gets closed
    return data
}
```

Defer actions run in reverse order (LIFO) when scope exits.

## Global Variables

Global variables are allocated in a **global arena** that persists for the entire program:

```doxa
// Module/file level - allocated in global arena
var globalCache :: Cache is Cache{}
const config :: Config is loadConfig()

function useGlobals() {
    globalCache.add("key", "value")  // Accessible from anywhere
}
```

**Initialization:** Globals are initialized before any code runs, regardless of whether the file executes top-to-bottom (script mode) or starts with `main()` (function mode).

## Memory Hierarchy

```
Global Arena
├── Global variables and constants
├── Module-level data
└── Persists until program exit

Function Scopes (Nested)
├── Function locals and parameters
├── Nested block scopes
└── Deallocated on function return

Defer Actions
├── Resource cleanup functions
├── Run on scope exit (success or failure)
└── Executed in reverse order (LIFO)
```

## Allocation Strategy

### Dynamic Arrays
```doxa
var dynamic :: int[] is [1, 2, 3]     // Heap allocated, resizable
```

### Fixed Arrays
```doxa
var fixed :: int[100] is [1, 2, 3]    // Stack allocated, fixed size
```

### Constants
```doxa
const data is [1, 2, 3]               // Read-only static data
```

## Performance Characteristics

### Advantages
- **Fast allocation:** Arena allocation is extremely efficient
- **Bulk deallocation:** Scopes clean up in O(1) time
- **Cache-friendly:** Related data allocated contiguously
- **No GC pauses:** Deterministic cleanup timing
- **Memory safe:** No dangling pointers or use-after-free

### Trade-offs
- **Memory usage:** Arenas hold memory until scope exit
- **No heap compaction:** Memory fragmentation within arenas
- **Scope lifetime:** Objects can't outlive their allocating scope
- **Copy on return:** Return values may be copied between scopes

## Error Handling & Cleanup

Defer ensures cleanup even during exceptions:

```doxa
function riskyOperation() {
    var resource = acquireResource()
    defer resource.release()  // Runs even if error occurs

    doRiskyThing()            // Might throw
    return result            // defer still runs
} // resource.release() called here
```

## Best Practices

### Memory Management
- **Use defer** for any resource that needs explicit cleanup (clean up instances are limited to user space, std lib, etc.)
- **Keep scopes small** to minimize memory usage
- **Return early** from functions to free temporary allocations
- **Use fixed arrays** when size is known at compile time

### Performance
- **Batch operations** to reuse allocations within a scope
- **Minimize returns** of large data structures
- **Use aliases** instead of passing large structs by value
- **Profile memory usage** in performance-critical code

### Safety
- **Always use `^`** for mutation parameters
- **Prefer immutable data** when possible
- **Use defer** for resource management
- **Keep scopes focused** on single responsibilities

## Edge Cases Handled

### Cyclic References
Not applicable - scope cleanup prevents cycles by design.

### Dangling Pointers
Impossible - objects can't outlive their scope.

### Memory Leaks
Prevented - scopes guarantee cleanup.

### Resource Leaks
Mitigated by defer statements.

### Large Allocations
Handled through scope-based cleanup timing.

This system provides **C++-level performance** with **Rust-level safety** through simple, predictable rules.
