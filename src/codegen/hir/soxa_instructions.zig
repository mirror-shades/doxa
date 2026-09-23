const HIRValue = @import("soxa_values.zig").HIRValue;
const HIRType = @import("soxa_types.zig").HIRType;
const HIRMapEntry = @import("soxa_values.zig").HIRMapEntry;
const SoxaTypes = @import("soxa_types.zig");
const ScopeKind = SoxaTypes.ScopeKind;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;
const StructId = @import("soxa_types.zig").StructId;
pub const CallKind = @import("soxa_types.zig").CallKind;
const Reporting = @import("../../utils/reporting.zig");
const Expr = @import("../../ast/ast.zig").Expr;

pub const ArithOp = enum { Add, Sub, Mul, Div, IntDiv, Mod, Pow };

pub const CompareOp = enum { Eq, Ne, Lt, Le, Gt, Ge };

pub const LogicalOpType = enum { And, Or, Not, Iff, Xor, Nand, Nor, Implies };

pub const StringOpType = enum { Concat, Length, Substring, ToInt, ToFloat, ToByte, ToString, Pop, Pack, Unpack };

pub const OverflowBehavior = enum {
    Trap, // throw error
    Saturate, // clamp to limits
    Wrap, // wrap around
};

pub const ExceptionBehavior = enum {
    Trap, // throw error
    NaN, // allow NaN result
};

pub const ResizeBehavior = enum {
    Double, // Double capacity when full (default)
    Fixed, // Error if capacity exceeded
    Exact, // Only allocate exact amount needed
};

/// Stack-based HIR - the central intermediate representation, consumed by the
/// LLVM backend.
pub const HIRInstruction = union(enum) {
    //==================================================================
    // STACK OPERATIONS
    //==================================================================

    /// Push literal constant onto stack
    /// LLVM: LLVMConstInt(context, value, signed)
    Const: struct {
        value: HIRValue,
        constant_id: u32,
    },

    /// Duplicate top stack value
    /// LLVM: Create temporary for value reuse
    Dup,

    /// Pop and discard top stack value
    /// LLVM: (no-op, just don't use the value)
    Pop,

    /// Swap top two stack values
    /// LLVM: Create temporary for value reordering
    Swap,

    //==================================================================
    // VARIABLE OPERATIONS (Context-aware)
    //==================================================================

    /// Load variable with full resolution context
    /// LLVM: LLVMBuildLoad -> symbol_table[var_name]
    LoadVar: struct {
        var_index: u32, // Direct index into the current scope's variables
        var_name: []const u8, // LLVM: Symbol table lookup
        scope_kind: ScopeKind, // Resolution context
        module_context: ?[]const u8, // For imported variables
    },

    /// Store to variable
    /// LLVM: LLVMBuildStore
    StoreVar: struct {
        var_index: u32,
        var_name: []const u8,
        scope_kind: ScopeKind,
        module_context: ?[]const u8,
        expected_type: HIRType, // Add expected type for coercion
        heap_copy: SoxaTypes.HeapCopyKind = .rehome,
    },

    /// Store variable declaration (var/const with initializer)
    /// LLVM: LLVMAddGlobal with proper type inference
    StoreDecl: struct {
        var_index: u32,
        var_name: []const u8,
        scope_kind: ScopeKind,
        module_context: ?[]const u8,
        declared_type: HIRType, // The type from declaration (not inferred)
        is_const: bool,
    },

    /// This is used for passing alias arguments by reference
    PushStorageId: struct {
        var_index: u32,
        var_name: []const u8,
        scope_kind: ScopeKind,
    },

    /// Load value from an alias parameter
    LoadAlias: struct {
        var_name: []const u8,
        slot_index: u32,
    },

    /// Store value to an alias parameter
    StoreAlias: struct {
        var_name: []const u8,
        slot_index: u32,
        expected_type: HIRType,
    },

    /// Bind an alias to its target variable (unified from StoreParamAlias)
    BindAlias: struct {
        alias_name: []const u8,
        target_variable_name: []const u8,
        alias_slot: u32,
        target_type: HIRType,
    },

    //==================================================================
    // ARITHMETIC OPERATIONS (Type-preserving)
    //==================================================================

    /// Integer arithmetic
    /// LLVM: LLVMBuildAdd, LLVMBuildSub, LLVMBuildMul
    Arith: struct {
        op: ArithOp,
        operand_type: HIRType,
    },

    /// Type conversion
    /// LLVM: LLVMBuildSIToFP, LLVMBuildFPToSI
    Convert: struct {
        from_type: HIRType,
        to_type: HIRType,
    },

    //==================================================================
    // COMPARISON OPERATIONS
    //==================================================================

    /// Comparison with type handling
    /// LLVM: LLVMBuildICmp, LLVMBuildFCmp with appropriate predicate
    Compare: struct {
        op: CompareOp,
        operand_type: HIRType, // Determines the comparison predicate
    },

    /// Type checking for union types and as expressions
    /// LLVM: Generate type checking code
    TypeCheck: struct {
        target_type: []const u8, // The type name to check against
    },

    /// Group member index check for group match patterns
    GroupCheck: struct {
        member_index: u32,
    },

    /// Extract payload from a group_instance, replacing it with the member value.
    GroupExtractPayload: struct {},

    /// Construct a union value from the current top-of-stack value.
    /// LLVM: Build a canonical %DoxaValue with union_id + active member index
    ///       encoded into the reserved field.
    UnionConstruct: struct {
        union_type: HIRType,
        member_index: u32,
    },

    /// Narrow a union-typed variable to a member view for the following span of
    /// instructions. Mirrors the `as`-cast narrowing the HIR symbol table
    /// tracks; the LLVM backend uses it to unwrap the boxed `%DoxaValue` at the
    /// next `LoadVar`, so consumers see the concrete member representation.
    NarrowVar: struct {
        var_name: []const u8,
        narrowed_type: HIRType,
    },

    /// End the narrowing span started by `NarrowVar`.
    RestoreVar: struct {
        var_name: []const u8,
    },

    //==================================================================
    // LOGICAL OPERATIONS
    //==================================================================

    /// Logical operations (AND, OR, NOT)
    /// LLVM: LLVMBuildAnd, LLVMBuildOr, LLVMBuildNot
    LogicalOp: struct {
        op: LogicalOpType,
    },

    //==================================================================
    // STRING OPERATIONS
    //==================================================================

    /// String operations (concatenation, length, substring)
    /// LLVM: String manipulation with proper memory management
    StringOp: struct {
        op: StringOpType,
    },

    //==================================================================
    // CONTROL FLOW (Label-based)
    //==================================================================

    /// Unconditional jump to label
    /// LLVM: LLVMBuildBr -> basic_block_map[label]
    Jump: struct {
        label: []const u8,
    },

    /// Conditional jump
    /// LLVM: LLVMBuildCondBr
    JumpCond: struct {
        label_true: []const u8,
        label_false: []const u8,
        condition_type: HIRType, // For type validation
    },

    /// Label marker
    /// LLVM: LLVMAppendBasicBlock
    Label: struct {
        name: []const u8,
    },

    //==================================================================
    // FUNCTION OPERATIONS (Context-rich)
    //==================================================================

    /// Function call with full context
    /// LLVM: LLVMBuildCall2 -> function_map[qualified_name]
    Call: struct {
        function_index: ?u32, // Direct function table index (null = zig module / builtin)
        qualified_name: []const u8, // LLVM: Full function name with module prefix
        arg_count: u32, // Stack management
        call_kind: CallKind, // Resolution context
        target_module: ?[]const u8, // For cross-module calls
        return_type: HIRType, // For stack type management and LLVM return handling
        tail: bool = false, // Tail call optimization
    },

    /// Return from function
    /// LLVM: LLVMBuildRet or LLVMBuildRetVoid
    Return: struct {
        has_value: bool,
        return_type: HIRType,
        /// Number of active reusable loop scopes to unwind before returning.
        loop_scope_count: u32 = 0,
    },

    //==================================================================
    // COMPLEX OPERATIONS
    //==================================================================

    /// Array/struct field access
    /// LLVM: LLVMBuildStructGEP or LLVMBuildGEP
    GetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        struct_id: StructId,
        field_index: u32, // Pre-resolved for efficiency
        field_type: HIRType,
        field_for_peek: bool = false,
        nested_struct_id: ?StructId = null,
    },

    /// Array/struct field assignment
    /// LLVM: LLVMBuildStore with GEP
    SetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        struct_id: StructId,
        field_index: u32,
        field_type: HIRType,
        nested_struct_id: ?StructId = null,
    },

    /// Store field name for struct field
    /// LLVM: No-op
    StoreFieldName: struct {
        field_name: []const u8,
    },

    //==================================================================
    // SCOPE MANAGEMENT
    //==================================================================

    /// Enter new scope block
    /// LLVM: (variable lifetime tracking)
    EnterScope: struct {
        scope_id: u32,
        var_count: u32, // Number of variables in the scope
    },

    /// Reset a reusable scope without releasing its arena node.
    /// LLVM: doxa_scope_reset
    ResetScope: struct {
        scope_id: u32,
    },

    /// Exit scope block
    /// LLVM: (end lifetime tracking)
    ExitScope: struct {
        scope_id: u32,
    },

    //==================================================================
    // ARRAY OPERATIONS
    //==================================================================

    /// Create new array
    /// LLVM: LLVMBuildArrayAlloca or heap allocation
    ArrayNew: struct {
        element_type: HIRType,
        size: u32, // 0 = dynamic array
        nested_element_type: ?HIRType = null, // For nested arrays like int[][]
        storage_kind: ArrayStorageKind = .dynamic,
        nested_sizes: [4]u32 = [_]u32{0} ** 4,
        nested_depth: u3 = 0,
    },

    /// Get array element by index
    /// LLVM: LLVMBuildGEP with bounds checking
    ArrayGet: struct {
        bounds_check: bool, // Enable/disable for performance
    },

    /// Set array element by index
    /// LLVM: LLVMBuildStore with GEP
    ArraySet: struct {
        bounds_check: bool,
    },

    /// Push element to end of array
    /// LLVM: Realloc logic or vector operations
    ArrayPush: struct {
        resize_behavior: ResizeBehavior,
    },

    /// Pop element from end of array
    /// LLVM: Load + resize
    ArrayPop,

    /// Insert element at index
    /// LLVM: Realloc/memmove as needed
    ArrayInsert,

    /// Remove element at index
    /// LLVM: memmove
    ArrayRemove,

    /// Slice array or string
    /// LLVM: memcpy or vector slice operations
    ArraySlice,

    /// Get array length
    /// LLVM: Load from array header
    ArrayLen,

    /// Concatenate two arrays
    /// LLVM: Complex allocation + memcpy
    ArrayConcat,

    //==================================================================
    // COMPOUND ASSIGNMENT OPERATIONS
    //==================================================================

    /// Compound assignment: array[index] +=, -=, *=, etc.
    /// LLVM: Generate optimized compound assignment
    ArrayCompoundAssign: struct {
        bounds_check: bool,
        op: ArithOp,
    },

    //==================================================================
    // STRUCT OPERATIONS
    //==================================================================

    /// Create new struct instance
    /// LLVM: LLVMBuildStructGEP for initialization
    StructNew: struct {
        type_name: []const u8, // Human-readable struct name
        struct_id: StructId,
        field_count: u32,
        /// Field names aligned with `field_types` / initialization order.
        /// Used by the native backend for debug printing.
        field_names: [][]const u8,
        field_types: []HIRType,
    },

    //==================================================================
    // DEBUG/INTROSPECTION
    //==================================================================

    /// Print/peek value
    /// LLVM: Generate printf calls with format strings
    Peek: struct {
        name: ?[]const u8,
        value_type: HIRType,
        location: ?Reporting.Location,
        union_members: ?[][]const u8 = null,
        /// Optional enum type name for enum peeks (e.g., "Species").
        /// This lets the LLVM backend print enums with their concrete type
        /// names independent of stack metadata.
        enum_type_name: ?[]const u8 = null,
    },

    /// Prints a struct
    /// LLVM: Generate constant string based on LLVM type
    PeekStruct: struct {
        type_name: []const u8, // Changed from struct_name to type_name
        struct_id: StructId,
        field_count: u32,
        field_names: [][]const u8,
        field_types: []HIRType,
        location: ?Reporting.Location,
        should_pop_after_peek: bool,
    },

    /// Program termination
    /// LLVM: LLVMBuildRet from main function
    Halt,

    /// Load module as struct instance
    /// LLVM: Generate struct with module variables
    LoadModule: struct {
        module_name: []const u8,
        field_names: []const []const u8 = &[_][]const u8{},
        field_slots: []const u32 = &[_]u32{},
    },

    /// Map expression
    /// LLVM: Generate map creation and lookup
    Map: struct {
        entries: []HIRMapEntry,
        key_type: HIRType,
        value_type: HIRType,
        has_else_value: bool = false,
    },

    /// Get map value by key
    /// LLVM: Generate map lookup
    MapGet: struct {
        key_type: HIRType,
        value_type: HIRType,
    },

    /// Set map value by key
    /// LLVM: Generate map update
    MapSet: struct {
        key_type: HIRType,
    },

    /// Assertion failure with formatted error message
    /// LLVM: Generate formatted error output and exit
    AssertFail: struct {
        location: Reporting.Location,
        has_message: bool,
    },
    Unreachable: struct {
        location: Reporting.Location,
    },
};
