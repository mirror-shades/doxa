# Prompt: Plan Return Value Move Semantics for Doxa VM

## Background

Doxa is an arena-scoped language. Every `{` creates a new arena, every `}` bulldozes it. Nothing outlives its scope. Values returned from functions are deep-copied from the callee's arena into the caller's frame. Deep copies are the cost of this guarantee. Parameter aliases (`^`) are the escape hatch for mutation without pointers.

The bytecode generator has a specific behavior: when a function contains an explicit `return` statement, it **skips emitting `ExitScope`** for that function. This means the callee's scope arena is never deinitialized. It persists on the VM's `scope_stack` until program termination (where `VM.deinit()` -> `clearScopeStack()` cleans everything up).

## The Optimization

Since the callee's scope arena persists after return, the return value's heap-allocated inner pointers (strings, array elements, struct fields, map entries) remain valid. The deep copy in `handleReturn` is unnecessary — we can **move** the value instead of copying it.

Current flow:
```
callee computes result in scope arena
→ pushes result to operand stack (in frame arena)
→ Return bytecode
→ handleReturn pops result from stack
→ deepCopyValueToAllocator(dest=caller_frame_arena, src=result)
→ push cloned value to caller's operand stack
→ frame.deinit() frees callee frame arena (but NOT scope arena)
```

Proposed flow:
```
callee computes result in scope arena
→ pushes result to operand stack
→ Return bytecode
→ handleReturn pops result from stack
→ push ORIGINAL value to caller's operand stack (no deep copy)
→ frame.deinit() frees callee frame arena (but NOT scope arena)
→ value's inner pointers are still in callee scope arena, which persists
```

The operand stack data (`HIRFrame` structs containing `HIRValue`) lives in the frame arena and gets freed by `frame.deinit()`. This is safe because `popValue()` returns `HIRValue` by value — the struct has been extracted to the C stack before deinit.

For the optimization to be safe, the value's inner pointers must outlive `frame.deinit()`. Since all VM allocations (strings, arrays, structs, maps) use `scopeAllocator()` — confirmed in `src/interpreter/ops/strings.zig`, `src/interpreter/vm.zig:execStructNew`, `execMap`, `execArrayNew` — inner pointers are in scope memory, not frame memory. The scope persists. So the move is safe for the common case.

## The Complication

Not ALL inner pointers are in persistent scopes. Consider:

```doxa
function split(input :: string) returns string[] {
    # EnterScope(split) — will be skipped on return
    var result :: string[]
    for each char... {
        # EnterScope(loop) — EXITED normally
        var temp is substring(input, start, end)
        @push(result, temp)  # temp's string is in LOOP scope arena
        # ExitScope(loop) — temp's arena is DEINITED here
    }
    return result  # result's elements point to DEINITED loop arenas!
}
```

The loop's `ExitScope` runs and destroys the loop arena. `result`'s elements were allocated in that now-dead arena. A deep copy clones each element into the outer scope, but a move would preserve dangling pointers.

**Solution:** Check if the value's `scope_id` is still on the scope stack. `HIRValue` types that carry `scope_id` (array, struct_instance, map, enum_variant, group_instance) can be validated via a new helper:

```zig
fn isScopeOnStack(self: *VM, scope_id: u32) bool {
    for (self.scope_stack.items) |record| {
        if (record.id == scope_id) return true;
    }
    return false;
}
```

Values from still-active scopes can be moved. Values from exited scopes need a deep copy. Simple types (int, byte, float, tetra, nothing, storage_id_ref) have no heap pointers and are already cheap — no optimization needed.

## What Was Tried

### handleReturn optimization (vm.zig)

The `handleReturn` function was modified to check `isScopeOnStack` and skip `deepCopyValueToAllocator` when the scope is alive:

```zig
if (return_value) |val| {
    const sid = getScopeId(val);
    if (sid != 0 and !self.isScopeOnStack(sid)) {
        // scope was exited — deep copy needed
        var mut_val = val;
        const promoted = try self.deepCopyValueToAllocator(dest_alloc, &mut_val, ...);
        try self.stack.pushValue(promoted);
    } else {
        // scope is alive — move
        try self.stack.pushValue(val);
    }
}
```

### storeValueAtPtr optimization (vm.zig)

The `storeValueAtPtr` function was modified to skip the deep copy for Runtime-owner stores when the scope is alive:

```zig
.Range => if (getScopeId(value) != 0 and self.isScopeOnStack(getScopeId(value))) value 
          else { ... deep copy ... },
```

### Test Results

- Bigfile (252 peek tests): PASSED
- Methods (113 peek tests): PASSED  
- Complex print, expressions, array storage migration, etc.: PASSED
- **Brainfuck: FAILED** — produced corrupted output (all zeros instead of expected values) then hit integer overflow
- **Calculator: FAILED** — segfault in `deepCopyValueToAllocator` when storing a string into a module slot

## What Went Wrong

### Brainfuck failure

The brainfuck test (`test/examples/brainfuck.doxa`) uses alias parameters extensively. Functions like `startLoop(^loop_spot, ^loops, ip)` and `endLoop(loop_spot, ^loops, ^ip, tape, tp)` mutate the caller's variables through aliases.

The test output changed from correct values (0x02, 0x04, 0x02) to all zeros, suggesting the `tape` byte array wasn't being properly modified. This implies alias parameters are somehow interacting with the move optimization — possibly the aliased variable's storage is in a scope that the move optimization incorrectly treats as alive.

**Open question:** Do alias parameters (`^`) reference frame-local storage (which is in the frame arena, not scope arena) or scope-local storage? If frame-local, the move optimization should be irrelevant (it only affects scope values). If scope-local, the optimizer might be preserving scope memory that the alias path expects to be deinited.

### Calculator failure

The calculator test (`test/examples/calculator.doxa`) imports an inline Zig IO module. The crash trace:
```
deepCopyValueToAllocator → allocator.dupe(u8, s) → segfault
```
This means a string pointer in a `HIRValue` was dangling — `dupe` tried to read from freed memory. The string was being deep-copied because `storeValueAtPtr` for a `.Module` owner always deep-copies (the optimization only skips for `.Runtime`). But the source string itself was already freed.

The calculator's control flow involves functions that return arrays of structs (`CalcToken[]`), which are then passed through multiple function calls. The nested function calls create and exit inner scopes, and the optimization may have preserved a scope that the alias path expected to be deinited.

## Infrastructure Already In Place

- `isScopeOnStack(scope_id)` helper method on VM — not currently used, ready to be reinstated
- `getScopeId(value: HIRValue)` returns scope_id for complex types, 0 for simple types
- `ScopeRecord.id` is the scope identifier on the scope stack
- `VM.scope_stack` is `Managed(ScopeRecord)` — array list of active scope records
- All VM allocations confirmed to use `scopeAllocator()` (not frame allocator)

## Design Decisions Needed

1. **Alias parameter interaction:** Do `^` parameters reference scope memory or frame memory? If scope, does the move optimization interact with alias bind/clear lifecycle? The `runScopeCleanup` function clears alias bindings — does this happen at the right time relative to the move?

2. **Scope_id for strings:** Strings don't carry a `scope_id` in `HIRValue` (only arrays, structs, maps, enums, groups do). This means `isScopeOnStack` can't validate string pointers. Should strings get a `scope_id` field, or is there another way to track their lifetime?

3. **Optimization granularity:** Should the move optimization apply only to `handleReturn` (return value promotion) or also to `storeValueAtPtr` (variable assignment)? The former is simpler; the latter catches cases where a function return value is immediately stored.

4. **Early scope exit:** Functions without returns DO emit `ExitScope`. Their return values (always `nothing`) don't need optimization. But functions WITHRETURNS skip `ExitScope` — yet inner scopes (loops, blocks) within those functions DO exit normally. Should `isScopeOnStack` be checked only against the function-level scope, or against ALL ancestor scopes?

5. **String optimization:** Most deep-copy overhead comes from strings (array/struct deep copies are less common). Since strings don't carry scope_id, how do we validate their pointers? Options:
   a. Add `scope_id` to the string variant in `HIRValue`
   b. Walk the scope stack's arenas to see if the string pointer falls within any active arena's buffer range
   c. Accept that string moves are always safe (since string ops use `scopeAllocator()`, and the function's outer scope persists)
   d. Use a separate string interning table

6. **Testing strategy:** The brainfuck and calculator tests are the stress tests for this optimization. Both need to pass before the optimization can be merged. What instrumentation or debug logging would help trace the exact failure point?

## Deliverable

Please produce a detailed implementation plan addressing the above design decisions. The plan should:
- Specify the exact code changes needed (files, line ranges, logic)
- Address each design decision with a concrete recommendation
- Include a testing strategy
- Order the changes to minimize risk (e.g., handleReturn-only first, then storeValueAtPtr)
