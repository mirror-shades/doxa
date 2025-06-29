data types:

- int: i32
- float: f64
- bool: bool
- nothing: void
- string: []const u8

support for more precise data types will be handled by libraries in the future such as:

- int8, int16, int64
- uint8, uint16, uint32, uint64
- float16, float64

1. Implement a mark-and-sweep collector that tracks all heap-allocated objects (Frames, Arrays, Strings, Structs) through a central object table.
2. Add try/catch block tracking to the VM with a separate exception stack that maintains the call chain during unwinding.
3. Add exception-safe resource management (RAII-style) and automatic cleanup during stack unwinding.
4. Create a module loader that can resolve and cache imported modules with proper dependency tracking.
5. Implement a namespace system to prevent naming conflicts between modules and support qualified imports.
6. Create a Foreign Function Interface (FFI) that can marshal data between VM types and native types.
7. Implement proper variable resolution that checks local scope, then upvalues, then global scope.
