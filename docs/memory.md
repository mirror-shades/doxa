# Memory Management

Doxa's memory system uses arena based scoping to provide automatic management which is consistent, predicitable, and fast. Doxa does not use garbage collection, and it uses limited amounts of refrence counting. It does this by relying on arena allocations based on block scopes.

## Core Principles

### Scope-Based Cleanup

Every scope (function, block, loop body) has its own arena allocator. When a scope exits, the entire arena is deallocated in bulk. Since deallocation of arenas is O(1) cleanup is fast.

```doxa
fn registerEmployee(name :: string, age :: int) returns Employee 
{ # an arena scope is created for this function
    return $Employee {name is name, age is age} # since this will be destroyed, the return is a deep copy
} # the arena scope is destroyed

```

### Block Cleanup

The easiest mental model is to think of curly braces as where memory arenas are created and destroyed. This can also be used to clean up data allocations using blocks.

```doxa
fn registerEmployee(id :: int) returns Employee | nothing { # scope one created
    var new_employee :: Employee | nothing is nothing
    { # scope 2 is created
        const list is loadEmployeeList() # big list loaded into memory
        new_employee is list.find(id)
    } # scope 2 is cleaned up, list is freed
    ...
    return new_employee
} # scope one is cleaned up
```

### Parameter Aliasing

One of the tradeoffs is that pointers don't play nice with this kind of cleanup. So rather than use pointers, Doxa uses parameter aliasing. When you specify a parameter as an alias, the function will access the caller's arena scope when changing that variable. Values can be passed by alias in only one direction, all return statements produce copies.

```doxa
fn inByReturn(value :: int) returns int {
    return value + 1  # A new allocation is created in the caller scope for the return copy
}

fn incByAlias(^value :: int) {
    value += 1  # The int from the caller scope is incremented directly
}
```

These alias calls can be chained allowing you to pass around values by alias throughout your program.

```doxa
function levelOne(^value :: int) {
    value += 1
    levelTwo(^value)
}

function levelTwo(^value :: int) {
    value *= 2
}
var x is 1
levelOne(^x) # x becomes 4
```


**Safety rules:**
- Only full variables can be aliased (no temporaries, fields, or array elements)
- Explicit opt-in at call site with `^`
- No alias duplication or mixing aliased/by-value parameters
- Exclusive borrow during call

## Allocation Strategy

### Dynamic Arrays
```doxa
var dynamic :: int[] is [1, 2, 3]     # Heap allocated, resizable
```

### Fixed Arrays
```doxa
var fixed :: int[100] is [1, 2, 3]    # Stack allocated, fixed size
```

### Constants
```doxa
const data is [1, 2, 3]               # Read-only static data
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
- **Copy on return:** Return values are copied between scopes

## Edge Cases

An advantage to this approach is that many common edge cases and foot guns are avoided with zero effort on the part of the programmer.

### Cyclic References
Not applicable - scope cleanup prevents cycles by design.

### Dangling Pointers
Impossible - objects can't outlive their scope.

### Memory Leaks
Prevented - scopes guarantee cleanup.

### Large Allocations
Handled through scope-based cleanup timing.