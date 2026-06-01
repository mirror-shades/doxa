# LL(1) Compliance

Doxa's parser aims to be LL(1) — one token of lookahead, no backtracking.

## Resolved

### `Identifier { ... }` ambiguity (removed)

`foo { ... }` could be a match expression, struct literal, or variable reference. Required arbitrary lookahead past `{` (skipping newlines/commas) to inspect interior content.

**Fix:** Require keywords/prefixes that make the construct unambiguous from the first token:
- `match value { ... }` — `.MATCH` token, 1-token lookahead
- `$Struct { ... }` — `.STRUCT_INSTANCE` token, 1-token lookahead
- `identifier { ... }` — always a variable; `{` belongs to enclosing construct

Removed `parseStructOrMatch` and `identifierOrStructLiteral` (~160 lines).

### `{ ... }` map vs block ambiguity (removed)

`{` could start a map literal or a block. Required 2-token lookahead past `{`, plus a 2-token lookback behind `{` to check for `then`/`else`/`do`/`while`/`for`/`function`.

**Fix:** Require `map` keyword for map declarations. `{` always means block.

Simplified `braceExpr` (~30 lines removed).

## Remaining LL(2) violations (bounded 2-token lookahead)

These all need exactly 2 tokens — shallow, bounded, no arbitrary skipping. Acceptable as LL(2) or fixable.

### 1. `if` newline-before-`else` (`expression_parser.zig:979`)

`if cond then body \n else body` — checks `NEWLINE + ELSE`.

**Fix:** Tokenize `\n else` as a single `.ELSE_NEWLINE` token, or require explicit semicolons and treat `\n else` as a parse error (user adds `;`).

### 2. Match dotted path (`expression_parser.zig:1650`)

`Error.IOError.NotFound` in match patterns — checks `IDENTIFIER + DOT` after consuming the first identifier.

**Fix:** Handle the dotted path as a tokenization rule — lexer produces a single `.PATH` token for consecutive `IDENTIFIER . IDENTIFIER` sequences.

### 3. `struct` keyword vs type expression (`statement_parser.zig:301`)

`struct Foo { ... }` vs `struct` as a type annotation — checks `STRUCT_TYPE + LEFT_BRACE`.

**Fix:** Use distinct tokens. E.g., `struct` keyword for declarations, `@Struct` for type references. Or keep `.STRUCT_TYPE` and `.STRUCT_DECL` as separate tokens.

### 4. For-each iterable (`statement_parser.zig:480`)

`each x in foo { ... }` vs `each x in Foo { ... }` — checks `IDENTIFIER + LEFT_BRACE`.

**Fix:** Reserve `{` after an identifier for blocks only (already the case after the `Identifier {` fix above). The `each x in expr` iterable is parsed as an expression; if `expr` is `Foo { ... }`, the `{` would need to start a struct literal (`$Foo`) or be a block already handled. With the current rules, this check may be dead code — verify and remove if so.

## Architectural hacks (need redesign)

These bypass LL(1) at the design level. They exist because the parser conflates parsing, name resolution, and import loading into a single pass.

### 5. Import pre-pass (`parser_types.zig:251-282`)

First pass walks the raw token array manually (`self.tokens[start + 1]`), saves/restores `self.current`, and collects `import`/`module` statements before the main parse. Bypasses the parser abstraction entirely.

**Fix:** Parse imports as normal statements in the single pass. Defer actual file loading and namespace registration to a post-parse resolution phase. The AST holds `Import`/`Module` nodes; the resolver loads them.

### 6. Function forward-declaration scan (`statement_parser.zig:27-43`)

First pass calls `parseFunctionDecl` on every function in the file, stores signatures, then rewinds to the start. Two-pass parse.

**Fix:** Parse function declarations once. Collect signatures into a symbol table. Forward references to functions not yet seen are unresolved during parsing and resolved in a post-parse pass. This is standard practice — the parser produces an AST; a resolver builds the symbol table and wires up references.

## Target architecture

```
Source → Lexer → Parser (LL(1), single pass) → AST
                                                    ↓
                                              Resolver
                                          (symbol table,
                                           imports,
                                           forward refs)
                                                    ↓
                                              Analyzer
                                          (types, constants)
                                                    ↓
                                              Codegen
```

Parser produces a raw AST without resolving names or loading imports. Resolver handles all cross-reference wiring. This eliminates both architectural hacks and makes the parser trivially LL(1).
