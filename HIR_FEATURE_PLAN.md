# HIR Implementation Plan - Complete Feature Coverage

## 🎯 **Phase 1: Core Data Types (Week 1)**

### **Arrays**

```zig
// Missing HIR Instructions:
ArrayNew: struct { element_type: HIRType, size: u32 },
ArrayGet: struct { bounds_check: bool },
ArraySet: struct { bounds_check: bool },
ArrayPush: struct { resize_behavior: ResizeBehavior },
ArrayPop,
ArrayLen,
ArrayConcat,
ArraySlice: struct { start_index: u32, end_index: u32 },
```

### **Structs**

```zig
// Missing HIR Instructions:
StructNew: struct {
    struct_name: []const u8,
    field_count: u32,
    size_bytes: u32, // Pre-calculated for efficiency
},
StructGetField: struct {
    field_name: []const u8,
    field_offset: u32, // Pre-resolved offset
    field_type: HIRType,
},
StructSetField: struct {
    field_name: []const u8,
    field_offset: u32,
    field_type: HIRType,
},
```

### **Tuples**

```zig
TupleNew: struct { element_count: u32 },
TupleGet: struct { index: u32 },
TupleSet: struct { index: u32 },
```

### **Maps**

```zig
MapNew: struct { key_type: HIRType, value_type: HIRType },
MapGet: struct { key_type: HIRType },
MapSet: struct { key_type: HIRType, value_type: HIRType },
MapHas: struct { key_type: HIRType },
MapRemove: struct { key_type: HIRType },
```

### **Enums**

```zig
EnumNew: struct {
    enum_name: []const u8,
    variant_name: []const u8,
    variant_index: u32, // Pre-resolved for efficiency
},
EnumMatch: struct {
    variant_count: u32,
    jump_table: []u32, // Pre-calculated jump addresses
},
```

## 🎯 **Phase 2: Advanced Control Flow (Week 2)**

### **Pattern Matching**

```zig
// Current AST: Match { value: *Expr, cases: []MatchCase }
MatchBegin: struct {
    value_type: HIRType,
    case_count: u32,
},
MatchCase: struct {
    pattern_type: PatternType,
    pattern_value: HIRValue,
    jump_label: []const u8,
},
MatchEnd,
```

### **Loops & Control**

```zig
// For loops: for (init; condition; increment) body
ForInit: struct { var_count: u32 },
ForCondition,
ForIncrement,
ForEnd,

// ForEach: for item in array
ForEachBegin: struct {
    iterator_var: []const u8,
    array_type: HIRType,
},
ForEachNext,
ForEachEnd,

// Break/Continue
Break: struct { loop_label: []const u8 },
Continue: struct { loop_label: []const u8 },
```

## 🎯 **Phase 3: Advanced Features (Week 3)**

### **Exception Handling**

```zig
// Current AST: Try { try_body: []Stmt, catch_body: []Stmt }
TryBegin: struct {
    catch_label: []const u8,
    finally_label: ?[]const u8,
},
TryEnd,
Throw: struct {
    exception_type: HIRType,
},
CatchBegin: struct {
    exception_var: []const u8,
    exception_type: HIRType,
},
CatchEnd,
```

### **Quantifiers**

```zig
// Current AST: Exists { variable: Token, array: *Expr, condition: *Expr }
ExistsBegin: struct {
    iterator_var: []const u8,
    array_type: HIRType,
},
ExistsCondition,
ExistsEnd,

ForAllBegin: struct {
    iterator_var: []const u8,
    array_type: HIRType,
},
ForAllCondition,
ForAllEnd,
```

### **Method Calls**

```zig
// Current AST: MethodCall { receiver: *Expr, method: Token, arguments: []const *Expr }
MethodCall: struct {
    receiver_type: HIRType,
    method_name: []const u8,
    method_index: u32, // Pre-resolved method table index
    arg_count: u32,
    is_builtin: bool, // Array.push vs custom methods
},
```

## 🎯 **Phase 4: Module System (Week 4)**

### **Import/Export**

```zig
ModuleImport: struct {
    module_name: []const u8,
    symbol_name: ?[]const u8, // null = import all
    alias_name: ?[]const u8,
},
ModuleExport: struct {
    symbol_name: []const u8,
    symbol_type: HIRType,
},
```

## 🔄 **Implementation Strategy**

### **Week 1: Data Types**

1. **Day 1-2**: Extend `HIRValue` union with complex types
2. **Day 3-4**: Add array/struct instructions to HIR
3. **Day 5-7**: Update HIR generator to handle these in AST

### **Week 2: Control Flow**

1. **Day 1-3**: Pattern matching HIR instructions
2. **Day 4-5**: Advanced loop constructs
3. **Day 6-7**: Break/continue with label resolution

### **Week 3: Advanced Features**

1. **Day 1-3**: Exception handling
2. **Day 4-5**: Quantifiers (exists/forall)
3. **Day 6-7**: Method dispatch system

### **Week 4: Module System**

1. **Day 1-4**: Import/export resolution
2. **Day 5-7**: Cross-module HIR generation

## 🎯 **Success Criteria**

**Phase 1 Complete When:**

- ✅ All basic data types (array, struct, tuple, map, enum) have HIR representation
- ✅ HIR generator handles all data type AST nodes
- ✅ Memory manager integrates with complex types

**Phase 2 Complete When:**

- ✅ Pattern matching generates efficient HIR (jump tables)
- ✅ All loop types supported in HIR
- ✅ Break/continue with proper label resolution

**Phase 3 Complete When:**

- ✅ Exception handling with proper stack unwinding
- ✅ Quantifiers generate efficient iteration HIR
- ✅ Method dispatch works for builtin and custom methods

**Phase 4 Complete When:**

- ✅ Module system fully represented in HIR
- ✅ Cross-module calls and symbol resolution
- ✅ Full bigfile.doxa compilation and execution

## 🔥 **Priority Order**

1. **Arrays** (needed for benchmark.doxa)
2. **Structs** (major language feature)
3. **Pattern Matching** (complex but high-value)
4. **Exception Handling** (production requirement)
5. **Module System** (architectural completion)

**Next Step:** Start with array HIR instructions since they're needed for the benchmark!
