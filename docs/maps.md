# Maps

### Simple Mapping (Immutable)

Maps can store key-value pairs. By default, using const makes the map immutable.

```doxa
const map Entries {
    1 is "wow",
    2 is "cool",
}
```

### Mutable Maps

Using var allows the map data to be updated.

```doxa
var map Dict {
    1 is "first"
    2 is "third"
}

Dict[2] is "second"
```

### Explicit Typing (Optional)

If types cannot be inferred from the first entry or for empty maps, explicit key and/or value types are required. The syntax mirrors function declarations:

```doxa
var map PlayerToData :: Player returns Data {
    .Main is mainData
}
```

### Union Returns

Return types can be return unions for more flexibility on value storage.

```doxa
const map CarSeats returns string | nothing {
    1 is "Bob",
    2 is nothing,
    3 is "Sam"
    4 is nothing
}
```

### Accessing Maps

```doxa
var entry is MyMap["troll"]
var entry is MyMap[.Troll]

// if mutable
MyMap["troll"].hp is 100
```

### Notes on Inference

Key type is inferred from the first entry if not explicitly declared.

Value type is encouraged to be explicit (returns <ValueType>).

Empty maps must have both key and value types declared.

### Roadmap: Map Syntax and Functionality Improvement Plan

#### Current State Analysis

- **Parser**: Only supports map expressions `{key: value}` (see `src/parser/expression_parser.zig:93-96`), not declarations.
- **Documentation**: Shows `var map Name { ... }` syntax that doesn't exist in parser (see this file: lines 19-24).
- **VM**: Only has `MapGet` instruction (see `src/codegen/hir/soxa_instructions.zig:454-456`), missing `MapSet` for assignments.
- **Type System**: Hardcoded to string keys (see `src/codegen/hir/soxa_generator.zig:2136` and `src/interpreter/vm.zig:2681-2684`).

#### Core Problems to Solve

- **Syntax Mismatch**: Documentation shows `is` syntax but parser uses `:` syntax.
- **Missing Declarations**: No support for `var map` or `const map` declarations.
- **Incomplete Operations**: Can't assign to maps, only read from them.
- **Type Limitations**: Only string keys supported, no type inference for empty maps.
- **No Methods**: Missing `.set()`, `.get()`, `.keys()`, etc.

#### Implementation Plan

##### Phase 1: Declaration Syntax Support

- **Goal**: Enable `var map Name { ... }` and `const map Name { ... }` declarations.
- **Parser Changes**:
  - Add map declaration parsing to `declaration_parser.zig` alongside existing struct/enum parsing.
  - Support both `:` and `is` syntax for backward compatibility.
  - Parse type annotations like `:: KeyType returns ValueType`.
- **AST Changes**:
  - Add `MapDeclaration` statement type to `ast.zig`.
  - Enhance existing `MapEntry` to handle both syntaxes.
  - Add map type information to declaration nodes.
- **Codegen Changes**:
  - Generate map declaration instructions in `soxa_generator.zig`.
  - Create map constants for immutable maps.
  - Allocate map storage for mutable maps.

##### Phase 2: Assignment Operations

- **Goal**: Enable `MyMap[key] is value` assignments.
- **Parser Changes**:
  - Add `IndexAssign` expression type for map assignments.
  - Distinguish between array and map assignments in `expression_parser.zig`.
- **VM Changes**:
  - Add `MapSet` instruction to `soxa_instructions.zig`.
  - Implement map assignment in `vm.zig` similar to existing `ArraySet`.
  - Support different key types (not just strings).
- **Codegen Changes**:
  - Generate `MapSet` instructions for map assignments.
  - Handle type checking for key/value types.

##### Phase 3: Enhanced Type System

- **Goal**: Support typed maps and better type inference.
- **Type System Changes**:
  - Support explicit key/value types: `var map :: string returns int`.
  - Infer types from first entry when possible.
  - Require explicit types for empty maps.
  - Support union return types: `returns string | nothing`.
- **Semantic Analysis**:
  - Add map type checking in `semantic.zig`.
  - Validate key/value type consistency.
  - Handle type inference for map declarations.

##### Phase 4: Methods and Utilities

- **Parser Changes**:
  - Extend method call parsing to handle map methods.
  - Add method resolution for map types.
- **VM Changes**:
  - Add instructions: `MapKeys`, `MapValues`, `MapSize`, `MapContains`, `MapDelete`, `MapClear`.
  - Implement each method in `vm.zig`.
- **Codegen Changes**:
  - Generate appropriate instructions for each method call.
  - Handle method chaining and return types.

##### Phase 5: Advanced Features

- **Goal**: Map iteration and comprehensions.
- **Iteration Support**:
  - Add `foreach key, value in map` syntax.
  - Generate iteration instructions.
  - Support map iteration in VM.
- **Map Comprehensions**:
  - Add `{key: value for key, value in source}` syntax.
  - Generate comprehension instructions.
  - Support filtering: `{k: v for k, v in map if condition}`.

#### Implementation Order

- **Immediate (Phase 1)**
  - Add map declaration parsing to `declaration_parser.zig`.
  - Support both `:` and `is` syntax in map expressions.
  - Add `MapDeclaration` AST node.
  - Generate map declaration instructions.
- **Next (Phase 2)**
  - Add `MapSet` instruction.
  - Implement map assignment parsing.
  - Add map assignment VM implementation.
  - Support different key types.
- **Then (Phase 3)**
  - Add explicit type annotations for maps.
  - Implement type inference for empty maps.
  - Add union return type support.
  - Enhance semantic analysis for maps.

#### Key Files to Modify

- **Parser Layer**:
  - `src/parser/declaration_parser.zig` — Add map declarations.
  - `src/parser/expression_parser.zig` — Support both syntaxes.
  - `src/parser/parser_types.zig` — Map parsing logic.
- **AST Layer**:
  - `src/ast/ast.zig` — Add `MapDeclaration` statement type.
- **Codegen Layer**:
  - `src/codegen/hir/soxa_instructions.zig` — Add map instructions.
  - `src/codegen/hir/soxa_generator.zig` — Generate map code.
- **VM Layer**:
  - `src/interpreter/vm.zig` — Implement map operations.
- **Analysis Layer**:
  - `src/analysis/semantic.zig` — Map type checking.

This roadmap transforms maps from a basic key-value expression into a first-class data type with full declaration, assignment, and method support, matching the functionality shown in the documentation.
