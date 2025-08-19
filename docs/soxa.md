# SOXA File Format Specification

## Overview

SOXA (Stacked Doxa) is Doxa's high level intermediate representation format. SOXA files contain compiled instructions that can be executed directly by the HIR VM without parsing overhead.

## Purpose

SOXA serves as an intermediate compilation target that enables:

- Direct execution of pre-compiled instructions
- Compilation result caching
- Platform-independent distribution of compiled programs
- Future native code generation via LLVM IR compilation

## Compilation Pipeline

```
Source Code (.doxa) → HIR Generation → SOXA File (.soxa) → VM Execution, LLVM IR generation
```

## File Format Structure

### Header Format

```
Offset | Size | Field
-------|------|-------
0x00   | 4    | Magic Number (0x534F5841 "SOXA")
0x04   | 2    | Version (0x0001)
0x06   | 4    | Instruction Count
0x0A   | 4    | Constant Count
0x0E   | 4    | String Count
0x12   | 4    | Function Count
0x16   | 6    | Reserved (padding)
```

### Data Sections

1. Constant Pool - Serialized HIR values
2. String Pool - Shared string literals
3. Instruction Stream - HIR instructions with operands
4. Function Table - Function metadata and entry points

## HIR Instruction Set

### Stack Operations (Tags 0, 9, 10)

- `Const` (0) - Push constant from constant pool onto stack
- `Dup` (9) - Duplicate top stack value
- `Pop` (10) - Remove top stack value

### Variable Operations (Tags 1, 2)

- `LoadVar` (1) - Load variable by index and name with scope context
- `StoreVar` (2) - Store top of stack to variable with scope management

### Arithmetic Operations (Tags 3, 4)

- `IntArith` (3) - Integer arithmetic (+, -, \*, /, %) with overflow behavior
- `FloatArith` - Floating-point arithmetic with exception handling
- `Compare` (4) - Comparison operations (==, !=, <, >, <=, >=) with type info

### Control Flow (Tags 5, 6, 7, 8, 11)

- `Jump` (5) - Unconditional jump to label
- `JumpCond` (6) - Conditional jump with true/false labels
- `Call` (7) - Function call with CallKind resolution (LocalFunction, ModuleFunction, BuiltinFunction)
- `Return` (8) - Return from function with optional value
- `Label` (11) - Label marker for jump targets

### Scope Management (Tags 21, 22)

- `EnterScope` (21) - Enter new scope block with scope ID and variable count
- `ExitScope` (22) - Exit scope block and cleanup variables

### Array Operations (Tags 14-20)

- `ArrayNew` (14) - Create new array with element type and initial size
- `ArrayGet` (15) - Get element by index with optional bounds checking
- `ArraySet` (16) - Set element by index with optional bounds checking
- `ArrayPush` (17) - Append element with ResizeBehavior (Double, Fixed, Exact)
- `ArrayPop` (18) - Remove last element
- `ArrayLen` (19) - Get array length
- `ArrayConcat` (20) - Concatenate two arrays

### Debug Operations (Tags 12, 13)

- `Halt` (12) - Program termination
- `Peek` (13) - Print/debug value with optional variable name

## Value Serialization

Values are encoded with type tags:

| Tag | Type         | Format                                                |
| --- | ------------ | ----------------------------------------------------- |
| 0   | Integer      | 4-byte signed little-endian                           |
| 1   | Float        | 8-byte IEEE-754 double (bitcast as u64)               |
| 2   | String       | 4-byte length + UTF-8 data                            |
| 3   | Tetra        | 2-byte (0=false, 1=true, 3=both, 4=neither)           |
| 4   | U8           | 1-byte unsigned                                       |
| 5   | Nothing      | No data                                               |
| 6   | Array        | 4-byte length + 4-byte capacity + 1-byte element type |
| 7   | Struct       | Struct instance (placeholder)                         |
| 8   | Tuple        | Tuple instance (placeholder)                          |
| 9   | Map          | Map instance (placeholder)                            |
| 10  | Enum Variant | Enum variant (placeholder)                            |

## Call Kind System

Function calls are tagged with their resolution type:

```zig
pub const CallKind = enum {
    LocalFunction,    // User-defined function in current module
    ModuleFunction,   // Function from imported module
    BuiltinFunction,  // Built-in language functions (like array.push, array.length)
};
```

Call instructions include:

- Function index for VM function table lookup
- Qualified name for LLVM function resolution
- Argument count for stack management
- Call kind for dispatch optimization
- Optional target module context

### Built-in Functions

The VM provides built-in functions for core operations:

- **Array Methods**: `length()`, `push(element)`, `pop()`, array indexing
- **String Methods**: `length()`, concatenation, substring operations
- **Debug Functions**: `peek()` for value debugging
- **Type Functions**: `typeof()` for runtime type information

Built-in functions are optimized with direct VM implementation rather than function calls.

## Usage

### Build Integration

```makefile
%.soxa: %.doxa
	doxa compile $< -o $@
```

## Implementation Details

### Byte Order

All multi-byte values use little-endian encoding.

### Version Compatibility

- Forward compatibility: newer versions read older SOXA files
- Unknown instruction tags trigger compilation fallback
- Version field enables format evolution

### Error Handling

- Magic number validation prevents loading invalid files
- Malformed SOXA files trigger source compilation fallback
- Unknown instructions generate runtime errors

### Array Serialization

Arrays serialize both length and capacity:

- Length: current number of elements (calculated by counting non-nothing elements)
- Capacity: allocated storage size for resize operations
- Element type: HIR type tag (Auto, Int, Float, String, Tetra, etc.)
- Elements: serialized element values (padded with HIRValue.nothing)

Empty arrays receive minimum capacity allocation (8 elements) to enable growth.

### Array Resize Behavior

The VM supports three resize strategies:

- **Double**: Double capacity when full (default for dynamic growth)
- **Fixed**: Error if capacity exceeded (for bounded collections)
- **Exact**: Only allocate exact amount needed (memory-optimized)

## Instruction Encoding

Instructions are encoded as:

1. Instruction tag (1 byte)
2. Operand data (variable length)

### Complete Instruction Format

| Tag | Instruction | Format                                                                                                     |
| --- | ----------- | ---------------------------------------------------------------------------------------------------------- |
| 0   | Const       | [constant_id: 4 bytes]                                                                                     |
| 1   | LoadVar     | [var_index: 4 bytes] [name_length: 4 bytes] [name: variable]                                               |
| 2   | StoreVar    | [var_index: 4 bytes] [name_length: 4 bytes] [name: variable]                                               |
| 3   | IntArith    | [op: 1 byte]                                                                                               |
| 4   | Compare     | [op: 1 byte]                                                                                               |
| 5   | Jump        | [label_length: 4 bytes] [label: variable]                                                                  |
| 6   | JumpCond    | [true_label_len: 4 bytes] [true_label: variable] [false_label_len: 4 bytes] [false_label: variable]        |
| 7   | Call        | [function_index: 4 bytes] [arg_count: 4 bytes] [name_length: 4 bytes] [name: variable] [call_kind: 1 byte] |
| 8   | Return      | [has_value: 1 byte]                                                                                        |
| 9   | Dup         | (no operands)                                                                                              |
| 10  | Pop         | (no operands)                                                                                              |
| 11  | Label       | [name_length: 4 bytes] [name: variable]                                                                    |
| 12  | Halt        | (no operands)                                                                                              |
| 13  | Peek        | [has_name: 1 byte] [name_length: 4 bytes if has_name] [name: variable if has_name] [value_type: 1 byte]    |
| 14  | ArrayNew    | [element_type: 1 byte] [size: 4 bytes]                                                                     |
| 15  | ArrayGet    | [bounds_check: 1 byte]                                                                                     |
| 16  | ArraySet    | [bounds_check: 1 byte]                                                                                     |
| 17  | ArrayPush   | [resize_behavior: 1 byte]                                                                                  |
| 18  | ArrayPop    | (no operands)                                                                                              |
| 19  | ArrayLen    | (no operands)                                                                                              |
| 20  | ArrayConcat | (no operands)                                                                                              |
| 21  | EnterScope  | [scope_id: 4 bytes] [var_count: 4 bytes]                                                                   |
| 22  | ExitScope   | [scope_id: 4 bytes]                                                                                        |

## Function Table Format

Each function entry contains:

- Function name (4-byte length + UTF-8 string)
- Parameter count (4 bytes)
- Return type (1 byte HIRType enum value)
- Start label (4-byte length + UTF-8 string)
- Local variable count (4 bytes)
- Is entry point flag (2 byte tetra)

## Constant Pool Format

Constants are serialized sequentially using the value serialization format. The constant pool enables:

- Deduplication of identical values
- Efficient loading of large literals
- Type information preservation

## String Pool Format

Strings are stored once and referenced by index. String pool entries contain:

- String length (4 bytes)
- UTF-8 encoded string data

## Debug Information

Debug information (when present) includes:

- Source file mapping
- Line number information
- Variable name preservation
- Type information for peekion

## File Size Characteristics

Typical SOXA files are 60-80% the size of equivalent source code due to:

- Binary encoding efficiency
- Constant pool deduplication
- Elimination of whitespace and comments
- Compact instruction representation

## VM Execution Engine

The HIR VM executes SOXA files directly with these components:

### Execution State

- Instruction pointer (IP) indexing into instruction array
- Heap-allocated HIRStack (1MB) to avoid system stack overflow
- CallStack for function call management with return address tracking
- ScopeManager integration for variable isolation in recursive calls

### Performance Optimizations

- **Fast Mode**: Variable caching and reduced debug output for computational workloads
- **Turbo Mode**: Hot variable cache with direct storage lookup (4-slot cache)
- **Label Pre-resolution**: All labels resolved to instruction indices at startup (O(1) jumps)
- **Scope Isolation**: Each function call gets isolated variable scope for recursion safety

### Memory Management

- Integration with existing MemoryManager and ScopeManager
- Automatic scope creation/cleanup for function calls
- Variable storage deduplication and hot caching
- Proper cleanup on VM shutdown

### Execution Flow

1. Pre-resolve all labels to instruction indices
2. Execute instructions in linear sequence
3. Handle jumps via pre-resolved label map
4. Manage function calls with isolated scopes
5. Clean up scopes and memory on return/exit

## Validation

SOXA files undergo validation during loading:

1. Magic number verification (0x534F5841 "SOXA")
2. Version compatibility check (version 1)
3. Section size validation
4. Instruction stream integrity
5. Constant pool consistency
6. Function table validation

Invalid files trigger fallback to source compilation.
