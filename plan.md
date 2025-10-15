# Streaming Print HIR Refactor

This document proposes replacing format-style print handling with a small streaming-print instruction set in HIR. The goal is simpler codegen, consistent behavior between interpreter and LLVM, and zero hidden newlines.

## Goals
- Consistent output semantics across interpreter and LLVM.
- Eliminate special "interpolated print" payloads in HIR.
- Reduce allocations (no format-string construction for common cases).
- Keep lowering trivial and explicit.

## New HIR Instructions
- `PrintBegin`: start a print sequence.
- `PrintStr {const_id}`: print a raw string literal.
- `PrintVal`: print the top-of-stack value using ToString rules.
- `PrintNewline`: print a newline explicitly.
- `PrintEnd`: end a print sequence (no-op for now, future hook for flush).

Optional compatibility (during transition):
- `Print`: legacy single-value print; lowered as `PrintBegin → PrintVal → PrintEnd`.

## Semantics
- No implicit newline. Newlines are printed only via `PrintStr("\n")` or `PrintNewline`.
- Ordering is preserved: the sequence encodes "what to print" deterministically.
- `PrintVal` prints the current top-of-stack value and pops it.

## Example Expansion
From:
```doxa
@print("hello {name}\n")
```
To HIR:
```
PrintBegin
PrintStr   "hello "
# evaluate name (leaves value on stack)
PrintVal
PrintNewline
PrintEnd
```

Plain string:
```doxa
@print("hello world!")
```
Becomes:
```
PrintBegin
PrintStr   "hello world!"
PrintEnd
```

## Parser / IO Handling
- Parser keeps producing a single `Print` AST node with a format string and embedded expressions.
- `IOHandler.generatePrint` expands interpolation into the streaming sequence:
  - Push `PrintBegin`.
  - For each format-part:
    - If string chunk: `PrintStr`.
    - If expression: generate expression; insert `StringOp.ToString` if needed; `PrintVal`.
  - If the original string literal ends with `\n`, emit `PrintNewline`.
  - Push `PrintEnd`.
- For the current legacy format path:
  - If zero placeholders: emit `PrintBegin → PrintStr(full) → PrintEnd`.
  - If placeholders exist: emit the sequence as above.

## Interpreter Lowering
- `PrintBegin`: no-op.
- `PrintStr`: write bytes to output.
- `PrintVal`: stringify top value using existing ToString rules; write result; pop.
- `PrintNewline`: write `"\n"`.
- `PrintEnd`: no-op.

## LLVM Lowering
- `PrintStr`: emit `printf("%s", str_ptr)`.
- `PrintVal`:
  - If value is already a string pointer: `printf("%s", val)`.
  - If primitive: use type-specific `printf` or convert to string via helper later.
- `PrintNewline`: `printf("%s", "\n")` (or `puts("")` is equivalent but keep `printf` unified).
- Keep string globals unique (e.g., `str.lit.{id}`).
- Declare `printf` lazily on first use.

## Backward Compatibility
- Keep current `.Print` HIR variant temporarily:
  - Lower as `PrintBegin → PrintVal → PrintEnd` if it targets the top-of-stack.
  - Or, when fed a string constant: `PrintBegin → PrintStr → PrintEnd`.
- Remove `.PrintInterpolated` after callers migrate.

## Migration Plan
- Phase 1:
  - Implement new HIR variants and lowering (VM + LLVM).
  - Change `IOHandler.generatePrint` to emit streaming sequence.
  - Keep `.Print` and `.PrintInterpolated` supported in lowering.
- Phase 2:
  - Update call sites/tests to rely on streaming prints.
  - Remove `.PrintInterpolated` and map `.Print` to streaming sequence.
- Phase 3:
  - Remove legacy `.Print` if no longer used.

## Testing
- Unit: expansion logic for zero/one/many placeholders; trailing newline handling.
- Snapshot: compare interpreter vs LLVM outputs across:
  - Pure literal prints.
  - String + values.
  - Non-string values (int/float/tetra/byte).
  - Empty string and explicit newline cases.
- IR sanity: multiple string literals produce distinct globals; `printf` declared once.

## Risks / Notes
- `PrintVal` needs clear ToString behavior for all supported types; where missing, add minimal helpers or use type-specific `printf`.
- Ensure evaluation order of embedded expressions matches current semantics.
- Avoid hidden allocations in hot paths (prefer direct `printf` where possible).

## Future Work
- Introduce buffered/stream writer abstraction (performant batched writes).
- Add `PrintSpace` or `PrintChar` if needed for common patterns.
- Extend to formatted numerics if/when language adds format specifiers.
