const HIRValue = @import("soxa_values.zig").HIRValue;
const HIRType = @import("soxa_types.zig").HIRType;
const HIRMapEntry = @import("soxa_values.zig").HIRMapEntry;
const ScopeKind = @import("soxa_types.zig").ScopeKind;
const CallKind = @import("soxa_types.zig").CallKind;
const Reporting = @import("../../utils/reporting.zig");

pub const ArithOp = enum { Add, Sub, Mul, Div, Mod };

pub const CompareOp = enum { Eq, Ne, Lt, Le, Gt, Ge };

pub const LogicalOpType = enum { And, Or, Not, Iff, Xor, Nand, Nor, Implies };

pub const StringOpType = enum { Concat, Length, Substring, Bytes };

pub const OverflowBehavior = enum {
    Trap, // VM: throw error, LLVM: generate trap
    Saturate, // VM: clamp to limits, LLVM: use saturating intrinsics
    Wrap, // VM: wrap around, LLVM: normal arithmetic
};

pub const ExceptionBehavior = enum {
    Trap, // VM: throw error, LLVM: generate trap
    NaN, // VM: return nothing, LLVM: allow NaN result
};

pub const ResizeBehavior = enum {
    Double, // Double capacity when full (default)
    Fixed, // Error if capacity exceeded
    Exact, // Only allocate exact amount needed
};

/// Stack-based HIR - The central intermediate representation
/// Maps directly to VM OpCodes while carrying semantic information for LLVM
pub const HIRInstruction = union(enum) {
    //==================================================================
    // STACK OPERATIONS (Direct VM mapping)
    //==================================================================

    /// Push literal constant onto stack
    /// VM: OP_CONST -> Frame.initInt(value)
    /// LLVM: LLVMConstInt(context, value, signed)
    Const: struct {
        value: HIRValue,
        constant_id: u32, // For VM constant pool lookup
    },

    /// Duplicate top stack value
    /// VM: OP_DUP
    /// LLVM: Create temporary for value reuse
    Dup,

    /// Pop and discard top stack value
    /// VM: OP_POP
    /// LLVM: (no-op, just don't use the value)
    Pop,

    /// Swap top two stack values
    /// VM: OP_SWAP
    /// LLVM: Create temporary for value reordering
    Swap,

    //==================================================================
    // VARIABLE OPERATIONS (Context-aware)
    //==================================================================

    /// Load variable with full resolution context
    /// VM: OP_VAR -> currentScopeVars[var_index]
    /// LLVM: LLVMBuildLoad -> symbol_table[var_name]
    LoadVar: struct {
        var_index: u32, // VM: Direct index into current_scope_vars
        var_name: []const u8, // LLVM: Symbol table lookup
        scope_kind: ScopeKind, // Resolution context
        module_context: ?[]const u8, // For imported variables
    },

    /// Store to variable
    /// VM: OP_SET_VAR
    /// LLVM: LLVMBuildStore
    StoreVar: struct {
        var_index: u32,
        var_name: []const u8,
        scope_kind: ScopeKind,
        module_context: ?[]const u8,
        expected_type: HIRType, // Add expected type for coercion
    },

    /// Store constant (one-time assignment)
    /// VM: OP_SET_CONST
    /// LLVM: LLVMAddGlobal with constant flag
    StoreConst: struct {
        var_index: u32,
        var_name: []const u8,
    },

    //==================================================================
    // ARITHMETIC OPERATIONS (Type-preserving)
    //==================================================================

    /// Integer arithmetic
    /// VM: OP_IADD, OP_ISUB, OP_IMUL
    /// LLVM: LLVMBuildAdd, LLVMBuildSub, LLVMBuildMul
    IntArith: struct {
        op: ArithOp,
        overflow_behavior: OverflowBehavior, // For VM error handling and LLVM optimization
    },

    /// Float arithmetic
    /// VM: OP_FADD, OP_FSUB, OP_FMUL, OP_FDIV
    /// LLVM: LLVMBuildFAdd, LLVMBuildFSub, etc.
    FloatArith: struct {
        op: ArithOp,
        exception_behavior: ExceptionBehavior, // For division by zero handling
    },

    /// Type conversion
    /// VM: OP_CONVERT_NUMBER
    /// LLVM: LLVMBuildSIToFP, LLVMBuildFPToSI
    Convert: struct {
        from_type: HIRType,
        to_type: HIRType,
    },

    //==================================================================
    // COMPARISON OPERATIONS
    //==================================================================

    /// Comparison with type handling
    /// VM: OP_EQUAL, OP_GREATER, OP_LESS
    /// LLVM: LLVMBuildICmp, LLVMBuildFCmp with appropriate predicate
    Compare: struct {
        op: CompareOp,
        operand_type: HIRType, // Determines VM behavior and LLVM predicate
    },

    //==================================================================
    // LOGICAL OPERATIONS (From old VM - proven implementations)
    //==================================================================

    /// Logical operations (AND, OR, NOT)
    /// VM: Strict boolean type checking from old VM
    /// LLVM: LLVMBuildAnd, LLVMBuildOr, LLVMBuildNot
    LogicalOp: struct {
        op: LogicalOpType,
    },

    //==================================================================
    // STRING OPERATIONS (From old VM - proven implementations)
    //==================================================================

    /// String operations (concatenation, length, substring)
    /// VM: String interning and memory management from old VM
    /// LLVM: String manipulation with proper memory management
    StringOp: struct {
        op: StringOpType,
    },

    //==================================================================
    // CONTROL FLOW (Label-based for both targets)
    //==================================================================

    /// Unconditional jump to label
    /// VM: OP_JUMP -> ip += offset
    /// LLVM: LLVMBuildBr -> basic_block_map[label]
    Jump: struct {
        label: []const u8,
        vm_offset: i32, // Pre-calculated for VM
    },

    /// Conditional jump
    /// VM: OP_JUMP_IF_FALSE
    /// LLVM: LLVMBuildCondBr
    JumpCond: struct {
        label_true: []const u8,
        label_false: []const u8,
        vm_offset: i32,
        condition_type: HIRType, // For type validation
    },

    /// Label marker
    /// VM: (instruction pointer bookmark)
    /// LLVM: LLVMAppendBasicBlock
    Label: struct {
        name: []const u8,
        vm_address: u32, // Pre-resolved for VM
    },

    //==================================================================
    // FUNCTION OPERATIONS (Context-rich)
    //==================================================================

    /// Function call with full context
    /// VM: OP_CALL -> getFunction(function_index)
    /// LLVM: LLVMBuildCall2 -> function_map[qualified_name]
    Call: struct {
        function_index: u32, // VM: Direct function table index
        qualified_name: []const u8, // LLVM: Full function name with module prefix
        arg_count: u32, // Stack management for both targets
        call_kind: CallKind, // Resolution context
        target_module: ?[]const u8, // For cross-module calls
        return_type: HIRType, // For stack type management and LLVM return handling
    },

    /// Tail call optimization - function call in tail position
    /// VM: Reuse current stack frame, jump to function start
    /// LLVM: Generate as optimized tail call
    TailCall: struct {
        function_index: u32, // VM: Direct function table index
        qualified_name: []const u8, // LLVM: Full function name with module prefix
        arg_count: u32, // Stack management for both targets
        call_kind: CallKind, // Resolution context
        target_module: ?[]const u8, // For cross-module calls
        return_type: HIRType, // For stack type management and LLVM return handling
    },

    /// Return from function
    /// VM: OP_RETURN
    /// LLVM: LLVMBuildRet or LLVMBuildRetVoid
    Return: struct {
        has_value: bool,
        return_type: HIRType,
    },

    //==================================================================
    // COMPLEX OPERATIONS
    //==================================================================

    /// Type reflection operation
    /// VM: Returns type information for a value
    /// LLVM: Generate type metadata
    TypeOf: struct {
        value_type: HIRType,
    },

    /// Array/struct field access
    /// VM: OP_GET_FIELD
    /// LLVM: LLVMBuildStructGEP or LLVMBuildGEP
    GetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        field_index: u32, // Pre-resolved for efficiency
        field_for_peek: bool = false,
    },

    /// Array/struct field assignment
    /// VM: OP_SET_FIELD
    /// LLVM: LLVMBuildStore with GEP
    SetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        field_index: u32,
    },

    /// Store field name for struct field
    /// VM: Store field name for later use in peek
    /// LLVM: No-op
    StoreFieldName: struct {
        field_name: []const u8,
    },

    /// Exception handling
    /// VM: OP_TRY, OP_CATCH, OP_THROW
    /// LLVM: Landing pads and exception tables
    TryBegin: struct {
        catch_label: []const u8,
        vm_catch_offset: i32,
    },

    TryCatch: struct {
        exception_type: ?HIRType,
    },

    Throw: struct {
        exception_type: HIRType,
    },

    //==================================================================
    // SCOPE MANAGEMENT
    //==================================================================

    /// Enter new scope block
    /// VM: OP_BEGIN_BLOCK -> BlockScope management
    /// LLVM: (variable lifetime tracking)
    EnterScope: struct {
        scope_id: u32,
        var_count: u32, // Pre-calculated for VM efficiency
    },

    /// Exit scope block
    /// VM: OP_END_BLOCK -> cleanup variables
    /// LLVM: (end lifetime tracking)
    ExitScope: struct {
        scope_id: u32,
    },

    //==================================================================
    // ARRAY OPERATIONS (Phase 1 - High Priority)
    //==================================================================

    /// Create new array
    /// VM: Allocate array storage, set element type
    /// LLVM: LLVMBuildArrayAlloca or heap allocation
    ArrayNew: struct {
        element_type: HIRType,
        size: u32, // 0 = dynamic array
    },

    /// Get array element by index
    /// VM: Bounds check + direct access
    /// LLVM: LLVMBuildGEP with bounds checking
    ArrayGet: struct {
        bounds_check: bool, // Enable/disable for performance
    },

    /// Set array element by index
    /// VM: Bounds check + assignment
    /// LLVM: LLVMBuildStore with GEP
    ArraySet: struct {
        bounds_check: bool,
    },

    /// Push element to end of array
    /// VM: Resize if needed, append element
    /// LLVM: Realloc logic or vector operations
    ArrayPush: struct {
        resize_behavior: ResizeBehavior,
    },

    /// Pop element from end of array
    /// VM: Return element, decrease size
    /// LLVM: Load + resize
    ArrayPop,

    /// Get array length
    /// VM: Return stored length
    /// LLVM: Load from array header
    ArrayLen,

    /// Concatenate two arrays
    /// VM: Allocate new array, copy elements
    /// LLVM: Complex allocation + memcpy
    ArrayConcat,

    /// Exists quantifier operation
    /// VM: Check if any element satisfies predicate
    /// LLVM: Generate loop with early exit
    Exists: struct {
        predicate_type: HIRType,
    },

    /// Forall quantifier operation
    /// VM: Check if all elements satisfy predicate
    /// LLVM: Generate loop with early exit
    Forall: struct {
        predicate_type: HIRType,
    },

    //==================================================================
    // STRUCT OPERATIONS (Phase 1)
    //==================================================================

    /// Create new struct instance
    /// VM: Allocate memory, initialize fields
    /// LLVM: LLVMBuildStructGEP for initialization
    StructNew: struct {
        type_name: []const u8, // Changed from struct_name to type_name
        field_count: u32,
        field_types: []HIRType,
        size_bytes: u32, // Pre-calculated for VM efficiency
    },

    //==================================================================
    // ENUM OPERATIONS (Phase 1)
    //==================================================================

    /// Create enum variant
    /// VM: Store variant index and type info
    /// LLVM: Tagged union representation
    EnumNew: struct {
        enum_name: []const u8,
        variant_name: []const u8,
        variant_index: u32,
    },

    //==================================================================
    // DEBUG/INTROSPECTION
    //==================================================================

    /// Print/peek value
    /// VM: Complex printValue logic
    /// LLVM: Generate printf calls with format strings
    Peek: struct {
        name: ?[]const u8,
        value_type: HIRType,
        location: ?Reporting.Reporter.Location,
    },

    /// Prints a struct
    /// VM: OP_PEEK_STRUCT
    /// LLVM: Generate constant string based on LLVM type
    PeekStruct: struct {
        type_name: []const u8, // Changed from struct_name to type_name
        field_count: u32,
        field_names: [][]const u8,
        field_types: []HIRType,
        location: ?Reporting.Reporter.Location,
        should_pop_after_peek: bool,
    },

    /// Program termination
    /// VM: OP_HALT -> running = false
    /// LLVM: LLVMBuildRet from main function
    Halt,

    /// Map expression
    /// VM: OP_MAP -> map(key, value)
    /// LLVM: Generate map creation and lookup
    Map: struct {
        entries: []HIRMapEntry,
        key_type: HIRType,
        value_type: HIRType,
    },

    /// Get map value by key
    /// VM: OP_MAP_GET -> map[key]
    /// LLVM: Generate map lookup
    MapGet: struct {
        key_type: HIRType,
    },

    /// Assertion failure with formatted error message
    /// VM: Print formatted "Assertion failed at [location]: [message]" and halt
    /// LLVM: Generate formatted error output and exit
    AssertFail: struct {
        location: Reporting.Reporter.Location,
        has_message: bool,
    },
};
