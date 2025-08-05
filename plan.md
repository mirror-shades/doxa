# Semantic Analysis → VM Integration Plan

## Overview
Integrate semantic analysis custom types (structs, enums, unions) into the HIR VM for runtime type safety and proper memory management.

## Current State
- Semantic analysis tracks custom types in `CustomTypeInfo` structures
- HIR VM executes instructions but lacks type awareness
- Memory management handled by `MemoryManager` with arena allocation
- Custom types flow: Semantic Analysis → HIR Generation → VM (but VM doesn't use type info)

## Unified Memory Architecture Decisions

### Current Memory Architecture Analysis

#### ✅ **What's Already Unified:**
1. **Single MemoryManager Instance**: Both semantic analyzer and VM share the same `MemoryManager` instance
2. **Arena Allocator**: Both use `memoryManager.getAllocator()` for all allocations
3. **Scope Management**: Both use the same `ScopeManager` for variable and scope tracking
4. **Value Storage**: Both use the same `ValueStorage` system for variable values

#### ❌ **Current Memory Gaps:**

1. **Custom Type Allocation Tracking**:
   - Semantic analyzer allocates `CustomTypeInfo` but doesn't track in memory manager
   - VM creates custom type instances but doesn't register with memory manager
   - No unified cleanup of custom type allocations

2. **Type Registry Memory Management**:
   - No persistent storage for type definitions across phases
   - Type information duplicated between semantic analysis and VM
   - Memory leaks from type definition copies

3. **Scope Isolation Issues**:
   - Semantic analysis scopes vs VM execution scopes not properly coordinated
   - Custom type instances not properly scoped to execution contexts
   - Potential memory leaks when scopes exit

### Architectural Decisions for Unified Memory

#### Decision 1: **Single Source of Truth for Type Definitions**

**Problem**: Type definitions are currently duplicated between semantic analysis and VM
**Solution**: Store type definitions in `MemoryManager` as persistent, shared data

```zig
pub const MemoryManager = struct {
    arena: std.heap.ArenaAllocator,
    scope_manager: *ScopeManager,
    
    // NEW: Unified type registry
    type_registry: std.StringHashMap(CustomTypeInfo),
    
    // NEW: Type instance tracking
    custom_type_instances: std.AutoHashMap(u32, *CustomTypeInstance),
    next_instance_id: u32 = 0,
};

pub const CustomTypeInstance = struct {
    id: u32,
    type_name: []const u8,
    scope_id: u32, // Which scope owns this instance
    data: union {
        struct_instance: *HIRStruct,
        enum_instance: *HIREnum,
        union_instance: *HIRUnion,
    },
};
```

**Benefits**:
- Single allocation point for type definitions
- Automatic cleanup when memory manager deinitializes
- Shared access between semantic analysis and VM
- Proper scope tracking for type instances

#### Decision 2: **Phase-Separated Memory Pools**

**Problem**: Semantic analysis and VM execution have different memory lifetime requirements
**Solution**: Use separate arena pools within the same memory manager

```zig
pub const MemoryManager = struct {
    // Analysis phase memory (persistent through compilation)
    analysis_arena: std.heap.ArenaAllocator,
    
    // Execution phase memory (reset between runs)
    execution_arena: std.heap.ArenaAllocator,
    
    // Shared persistent data
    type_registry: std.StringHashMap(CustomTypeInfo),
    scope_manager: *ScopeManager,
    
    pub fn getAnalysisAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.analysis_arena.allocator();
    }
    
    pub fn getExecutionAllocator(self: *MemoryManager) std.mem.Allocator {
        return self.execution_arena.allocator();
    }
    
    pub fn resetExecutionMemory(self: *MemoryManager) void {
        self.execution_arena.deinit();
        self.execution_arena = std.heap.ArenaAllocator.init(self.analysis_arena.child_allocator);
    }
};
```

**Benefits**:
- Analysis results persist through multiple VM executions
- Execution memory can be reset without losing type definitions
- Clear separation of concerns between phases
- Efficient memory reuse for multiple program runs

#### Decision 3: **Unified Custom Type Lifecycle Management**

**Problem**: Custom type instances aren't properly tracked and cleaned up
**Solution**: Integrate custom type lifecycle with scope management

```zig
pub const Scope = struct {
    // ... existing fields ...
    
    // NEW: Custom type instances owned by this scope
    custom_type_instances: std.AutoHashMap(u32, *CustomTypeInstance),
    
    pub fn createCustomTypeInstance(self: *Scope, type_name: []const u8, data: CustomTypeInstanceData) !*CustomTypeInstance {
        const instance = try self.manager.allocator.create(CustomTypeInstance);
        instance.* = .{
            .id = self.manager.next_instance_id,
            .type_name = type_name,
            .scope_id = self.id,
            .data = data,
        };
        
        self.manager.next_instance_id += 1;
        try self.custom_type_instances.put(instance.id, instance);
        try self.manager.custom_type_instances.put(instance.id, instance);
        
        return instance;
    }
    
    pub fn deinit(self: *Scope) void {
        // ... existing cleanup ...
        
        // NEW: Clean up custom type instances
        var instance_iter = self.custom_type_instances.iterator();
        while (instance_iter.next()) |entry| {
            const instance = entry.value_ptr.*;
            _ = self.manager.custom_type_instances.remove(instance.id);
            self.manager.allocator.destroy(instance);
        }
    }
};
```

**Benefits**:
- Automatic cleanup when scopes exit
- Proper ownership tracking
- Memory leak prevention
- Consistent with existing variable management

#### Decision 4: **Type-Safe Memory Allocation Patterns**

**Problem**: Custom type allocations aren't type-safe and can lead to memory corruption
**Solution**: Use typed allocation functions with compile-time safety

```zig
pub const MemoryManager = struct {
    // ... existing fields ...
    
    // NEW: Type-safe allocation functions
    pub fn allocateStruct(self: *MemoryManager, scope: *Scope, type_name: []const u8, fields: []HIRStructField) !*HIRStruct {
        const struct_data = try self.execution_arena.allocator().create(HIRStruct);
        struct_data.* = .{
            .type_name = try self.execution_arena.allocator().dupe(u8, type_name),
            .fields = try self.execution_arena.allocator().dupe(HIRStructField, fields),
            .field_name = null,
            .path = null,
        };
        
        const instance_data = CustomTypeInstanceData{ .struct_instance = struct_data };
        _ = try scope.createCustomTypeInstance(type_name, instance_data);
        
        return struct_data;
    }
    
    pub fn allocateEnum(self: *MemoryManager, scope: *Scope, type_name: []const u8, variant_name: []const u8, variant_index: u32) !*HIREnum {
        const enum_data = try self.execution_arena.allocator().create(HIREnum);
        enum_data.* = .{
            .type_name = try self.execution_arena.allocator().dupe(u8, type_name),
            .variant_name = try self.execution_arena.allocator().dupe(u8, variant_name),
            .variant_index = variant_index,
            .path = null,
        };
        
        const instance_data = CustomTypeInstanceData{ .enum_instance = enum_data };
        _ = try scope.createCustomTypeInstance(type_name, instance_data);
        
        return enum_data;
    }
};
```

**Benefits**:
- Compile-time type safety
- Consistent allocation patterns
- Automatic instance tracking
- Memory leak prevention

#### Decision 5: **Memory Manager as Bridge Between Phases**

**Problem**: No clean way to transfer type information between semantic analysis and VM
**Solution**: Use memory manager as the bridge with explicit phase transitions

```zig
pub const MemoryManager = struct {
    // ... existing fields ...
    
    // NEW: Phase management
    current_phase: Phase = .Analysis,
    
    pub const Phase = enum {
        Analysis,    // Semantic analysis phase
        Generation,  // HIR generation phase  
        Execution,   // VM execution phase
    };
    
    pub fn transitionToExecution(self: *MemoryManager) !void {
        if (self.current_phase != .Generation) {
            return error.InvalidPhaseTransition;
        }
        
        // Reset execution memory while preserving analysis results
        self.resetExecutionMemory();
        
        // Create execution scope
        const execution_scope = try self.scope_manager.createScope(self.scope_manager.root_scope);
        
        self.current_phase = .Execution;
    }
    
    pub fn bridgeTypesToVM(self: *MemoryManager, vm: *HIRVM) !void {
        // Transfer type registry to VM
        var type_iter = self.type_registry.iterator();
        while (type_iter.next()) |entry| {
            try vm.registerCustomType(entry.value_ptr.*);
        }
        
        // Set VM's current scope to execution scope
        vm.current_scope = self.scope_manager.root_scope;
    }
};
```

**Benefits**:
- Clear phase boundaries
- Explicit data transfer
- Error prevention for invalid transitions
- Clean separation of concerns

### Implementation Strategy

#### Phase 1: Memory Manager Extensions
1. Add type registry and instance tracking to `MemoryManager`
2. Implement phase-separated memory pools
3. Add type-safe allocation functions
4. Implement phase transition logic

#### Phase 2: Scope Integration
1. Extend `Scope` to track custom type instances
2. Implement automatic cleanup in scope deinitialization
3. Add custom type instance creation methods

#### Phase 3: Semantic Analyzer Integration
1. Use `getAnalysisAllocator()` for all allocations
2. Register custom types in memory manager's type registry
3. Use typed allocation functions for custom type data

#### Phase 4: VM Integration
1. Use `getExecutionAllocator()` for all allocations
2. Implement type registry transfer from memory manager
3. Use typed allocation functions for runtime instances

#### Phase 5: Bridge Implementation
1. Implement phase transition from analysis to execution
2. Add type information transfer between phases
3. Ensure proper cleanup and memory safety

### Benefits of Unified Memory Architecture

1. **Memory Safety**: All custom type allocations tracked and automatically cleaned up
2. **Performance**: Efficient memory reuse and minimal allocation overhead
3. **Type Safety**: Compile-time guarantees for custom type operations
4. **Debugging**: Clear memory ownership and lifecycle tracking
5. **Maintainability**: Single source of truth for memory management
6. **Extensibility**: Easy to add new custom types and memory patterns

## VM Analysis Notes

### Current VM Custom Type Support

#### ✅ What's Already Working:
1. **Basic Custom Type Representation**: VM has `HIRValue` variants for:
   - `struct_instance`: HIRStruct with type_name, fields, path
   - `enum_variant`: HIREnum with type_name, variant_name, variant_index, path
   - `map`: HIRMap with entries, key_type, value_type

2. **Struct Operations**: VM supports:
   - `StructNew`: Creates struct instances with field values
   - `GetField`: Accesses struct fields by name with path tracking
   - `SetField`: Modifies struct field values
   - `PeekStruct`: Debug output for struct contents
   - `StoreFieldName`: Legacy field name assignment

3. **Enum Operations**: VM supports:
   - Basic enum variant creation via `enum_variant` HIRValue
   - Enum comparison in `compareEqual` function
   - Enum variant name storage and retrieval

4. **Type Conversion**: VM has conversion functions:
   - `tokenLiteralToHIRValue`: Converts semantic types to VM values
   - `hirValueToTokenLiteral`: Converts VM values back to semantic types
   - `hirValueToTypeInfo`: Maps VM values to TypeInfo

#### ❌ Current Gaps & Issues:

1. **No Type Registry**: VM lacks a custom type registry to store type definitions
   - No `custom_type_registry` field in HIRVM struct
   - No `registerCustomType()` method
   - Type information is lost between semantic analysis and VM execution

2. **Limited Type Safety**: VM doesn't validate against type definitions
   - Struct field access doesn't check if field exists in type definition
   - Enum variant creation doesn't validate against declared variants
   - No runtime type checking for custom types

3. **Memory Management Issues**:
   - Custom type allocations not tracked in memory manager
   - Potential memory leaks in struct/enum creation
   - No cleanup of custom type instances

4. **Missing Instructions**:
   - No `CreateEnum` instruction for proper enum instantiation
   - No `GetEnumVariant` instruction for enum pattern matching
   - No `Union` support at all (not in HIRValue variants)

5. **Type Information Loss**:
   - Semantic analysis `CustomTypeInfo` not preserved in VM
   - HIR generator has `CustomTypeInfo` but VM doesn't use it
   - No bridge between semantic types and VM runtime types

### Integration Points

#### 1. VM Structure Extensions Needed:
```zig
pub const HIRVM = struct {
    // ... existing fields ...
    
    // NEW: Custom type registry
    custom_type_registry: std.StringHashMap(CustomTypeInfo),
    
    // NEW: Type-aware value storage
    type_cache: std.StringHashMap(HIRType), // Cache for type lookups
}
```

#### 2. Memory Management Integration:
- Use existing `MemoryManager` arena allocator for custom type allocations
- Track custom type instances in memory manager's scope system
- Ensure proper cleanup when scopes exit

#### 3. Instruction Extensions Needed:
```zig
pub const HIRInstruction = union(enum) {
    // ... existing instructions ...
    
    // NEW: Custom type instructions
    CreateEnum: struct {
        type_name: []const u8,
        variant_name: []const u8,
    },
    GetEnumVariant: struct {
        type_name: []const u8,
        variant_name: []const u8,
    },
    CreateUnion: struct {
        type_name: []const u8,
        variant_index: u32,
    },
    // Enhanced struct instructions
    CreateStructTyped: struct {
        type_name: []const u8,
        field_count: u32,
    },
}
```

#### 4. Bridge Function Needed:
```zig
fn bridgeSemanticToVM(semantic_analyzer: *SemanticAnalyzer, vm: *HIRVM) !void {
    var custom_types_iter = semantic_analyzer.getCustomTypes().iterator();
    while (custom_types_iter.next()) |entry| {
        const custom_type = entry.value_ptr.*;
        try vm.registerCustomType(custom_type);
    }
}
```

### Current Memory Allocation Patterns

#### VM Allocation Points:
1. **Struct Creation**: `StructNew` instruction allocates:
   - `HIRStructField` array for fields
   - Field name strings via `allocator.dupe()`
   - Field values (recursive allocation)

2. **Enum Creation**: `enum_variant` HIRValue allocates:
   - Type name string
   - Variant name string
   - Path tracking strings

3. **Type Conversion**: `tokenLiteralToHIRValue` allocates:
   - String copies for struct field names
   - Array elements for complex types
   - Map entries for map types

#### Memory Manager Integration:
- VM uses `memory_manager.getAllocator()` for all allocations
- Custom types should use same arena allocator
- Scope-based cleanup should handle custom type instances

## Integration Strategy

### Phase 1: Foundation & Analysis
1. **Audit Current Allocation Patterns**
   - Map all allocation points in semantic analysis (`CustomTypeInfo`, `TypeInfo`, etc.)
   - Identify temporary vs. persistent allocations
   - Document memory ownership patterns between semantic analysis and VM

2. **Design Integration Architecture**
   - Define boundaries: semantic analysis owns type information, VM owns runtime values
   - Plan custom type flow: semantic analysis → HIR generation → VM execution
   - Design type registry that persists across compilation phases

### Phase 2: Gradual Integration

#### Step 2.1: Custom Type Registry
- Add `custom_type_registry` to HIRVM
- Implement `registerCustomType()` method
- Bridge semantic analysis types to VM registry

#### Step 2.2: Type-Aware Value Storage
- Enhance `HIRValue` to include type information
- Add `custom_struct` and `custom_enum` variants
- Support type-safe field access and variant dispatch

#### Step 2.3: Semantic Analysis → VM Bridge
- Create bridge function in main.zig
- Transfer custom type information after semantic analysis
- Ensure type information persists through VM execution

### Phase 3: Memory Management Strategy

#### Shared Memory Manager 
- Use existing `MemoryManager` as single source of truth
- Both semantic analysis and VM use same arena allocator
- Track custom type allocations in memory manager's scope system
- Ensure proper cleanup when scopes exit

### Phase 4: Incremental Implementation

#### Priority Order:
1. **Structs first** (simpler, direct field access)
2. **Enums second** (variant dispatch logic)
3. **Unions last** (most complex, type checking)

#### For each type:
1. Add type registration to VM
2. Add runtime value representation
3. Add instruction support (e.g., `CreateStruct`, `GetField`, `SetField`)
4. Add type checking in VM execution
5. Test with simple examples

### Phase 5: Optimization & Cleanup

1. **Remove redundant allocations**
   - Eliminate temporary type copies between phases
   - Use references instead of duplicating type information

2. **Add type caching**
   - Cache frequently accessed type information
   - Optimize struct field lookups

3. **Memory profiling**
   - Profile allocation patterns
   - Optimize hot paths

## Implementation Details

### Custom Type Registry in VM
```zig
pub const HIRVM = struct {
    // ... existing fields ...
    custom_type_registry: std.StringHashMap(CustomTypeInfo),
    
    pub fn registerCustomType(self: *HIRVM, type_info: CustomTypeInfo) !void {
        try self.custom_type_registry.put(type_info.name, type_info);
    }
}
```

### Enhanced HIRValue with Type Information
```zig
pub const HIRValue = union(enum) {
    // ... existing variants ...
    custom_struct: struct {
        type_name: []const u8,
        fields: []HIRValue,
    },
    custom_enum: struct {
        type_name: []const u8,
        variant_index: u32,
        payload: ?HIRValue,
    },
};
```

### Type-Aware HIR Instructions
```zig
pub const HIRInstruction = union(enum) {
    // ... existing instructions ...
    CreateStruct: struct {
        type_name: []const u8,
        field_count: u32,
    },
    GetField: struct {
        field_name: []const u8,
        type_name: []const u8,
    },
    CreateEnum: struct {
        type_name: []const u8,
        variant_index: u32,
    },
};
```

### Bridge Function
```zig
fn bridgeSemanticToVM(semantic_analyzer: *SemanticAnalyzer, vm: *HIRVM) !void {
    var custom_types_iter = semantic_analyzer.getCustomTypes().iterator();
    while (custom_types_iter.next()) |entry| {
        const custom_type = entry.value_ptr.*;
        try vm.registerCustomType(custom_type);
    }
}
```

## Testing Strategy
- Test each custom type individually
- Use existing test framework
- Add benchmarks to ensure no performance regression
- Test memory cleanup and type safety

## Success Criteria
- Custom types work correctly at runtime
- No memory leaks or double allocations
- Type safety maintained throughout execution
- Performance remains acceptable
- Existing functionality unaffected
