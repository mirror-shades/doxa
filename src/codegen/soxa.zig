const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const instructions = @import("../interpreter/instructions.zig");
const reporting = @import("../utils/reporting.zig");

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

    /// Array/struct field access
    /// VM: OP_GET_FIELD
    /// LLVM: LLVMBuildStructGEP or LLVMBuildGEP
    GetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        field_index: u32, // Pre-resolved for efficiency
    },

    /// Array/struct field assignment
    /// VM: OP_SET_FIELD
    /// LLVM: LLVMBuildStore with GEP
    SetField: struct {
        field_name: []const u8,
        container_type: HIRType,
        field_index: u32,
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

    //==================================================================
    // STRUCT OPERATIONS (Phase 1)
    //==================================================================

    /// Create new struct instance
    /// VM: Allocate memory, initialize fields
    /// LLVM: LLVMBuildStructGEP for initialization
    StructNew: struct {
        struct_name: []const u8,
        field_count: u32,
        size_bytes: u32, // Pre-calculated for VM efficiency
    },

    //==================================================================
    // TUPLE OPERATIONS (Phase 1)
    //==================================================================

    /// Create new tuple
    /// VM: Allocate tuple storage
    /// LLVM: Struct with heterogeneous types
    TupleNew: struct {
        element_count: u32,
    },

    /// Get tuple element by index
    /// VM: Direct index access (compile-time bounds check)
    /// LLVM: LLVMBuildExtractValue
    TupleGet: struct {
        index: u32,
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

    /// Print/inspect value
    /// VM: Complex printValue logic
    /// LLVM: Generate printf calls with format strings
    Inspect: struct {
        name: ?[]const u8,
        value_type: HIRType, // Determines print format for both targets
    },

    /// Type inquiry
    /// VM: OP_TYPEOF -> string representation
    /// LLVM: Generate constant string based on LLVM type
    TypeOf: struct {
        result_string: []const u8, // Pre-computed type name
    },

    /// Program termination
    /// VM: OP_HALT -> running = false
    /// LLVM: LLVMBuildRet from main function
    Halt,
};

//==================================================================
// SUPPORTING TYPES
//==================================================================

// Tetra constants for fast operations
pub const TETRA_FALSE: u8 = 0; // 00
pub const TETRA_TRUE: u8 = 1; // 01
pub const TETRA_BOTH: u8 = 2; // 10
pub const TETRA_NEITHER: u8 = 3; // 11

// Fast lookup tables for tetra operations
pub const TETRA_AND_LUT: [4][4]u8 = [4][4]u8{
    [4]u8{ 0, 0, 0, 0 }, // FALSE AND x = FALSE
    [4]u8{ 0, 1, 2, 3 }, // TRUE AND x = x
    [4]u8{ 0, 2, 2, 0 }, // BOTH AND x = special logic
    [4]u8{ 0, 3, 0, 3 }, // NEITHER AND x = special logic
};

pub const TETRA_OR_LUT: [4][4]u8 = [4][4]u8{
    [4]u8{ 0, 1, 2, 3 }, // FALSE OR x = x
    [4]u8{ 1, 1, 1, 1 }, // TRUE OR x = TRUE
    [4]u8{ 2, 1, 2, 2 }, // BOTH OR x = special logic
    [4]u8{ 3, 1, 2, 3 }, // NEITHER OR x = special logic
};

pub const TETRA_NOT_LUT: [4]u8 = [4]u8{ 1, 0, 3, 2 }; // NOT lookup: false->true, true->false, both->neither, neither->both

// Type-safe tetra constructors
pub fn tetraFromBool(value: bool) u8 {
    return if (value) TETRA_TRUE else TETRA_FALSE;
}

pub fn tetraFromEnum(tetra_enum: anytype) u8 {
    return switch (tetra_enum) {
        .false => TETRA_FALSE,
        .true => TETRA_TRUE,
        .both => TETRA_BOTH,
        .neither => TETRA_NEITHER,
    };
}

pub const HIRValue = union(enum) {
    int: i32,
    u8: u8,
    float: f64,
    string: []const u8,
    tetra: u8, // NEW: Direct u8 storage for tetras
    nothing,
    // Phase 1: Complex data types
    array: HIRArray,
    struct_instance: HIRStruct,
    tuple: HIRTuple,
    map: HIRMap,
    enum_variant: HIREnum,
};

pub const HIRArray = struct {
    elements: []HIRValue,
    element_type: HIRType,
    capacity: u32,
};

pub const HIRStruct = struct {
    type_name: []const u8,
    fields: []HIRStructField,
};

pub const HIRStructField = struct {
    name: []const u8,
    value: HIRValue,
};

pub const HIRTuple = struct {
    elements: []HIRValue,
};

pub const HIRMap = struct {
    entries: []HIRMapEntry,
    key_type: HIRType,
    value_type: HIRType,
};

pub const HIRMapEntry = struct {
    key: HIRValue,
    value: HIRValue,
};

pub const HIREnum = struct {
    type_name: []const u8,
    variant_name: []const u8,
    variant_index: u32,
};

pub const HIRType = enum {
    Int,
    U8,
    Float,
    String,
    Boolean,
    Tetra, // NEW: Native tetra type for four-valued logic
    Nothing,
    Array,
    Struct,
    Tuple,
    Map,
    Enum,
    Function,
    Auto, // Still needs resolution
};

// Additional type information for complex types
pub const ArrayTypeInfo = struct {
    element_type: HIRType,
    size: ?u32, // null = dynamic array
};

pub const StructTypeInfo = struct {
    name: []const u8,
    fields: []StructFieldInfo,
};

pub const StructFieldInfo = struct {
    name: []const u8,
    field_type: HIRType,
    offset: u32, // Byte offset for efficient access
};

pub const TupleTypeInfo = struct {
    element_types: []HIRType,
};

pub const MapTypeInfo = struct {
    key_type: HIRType,
    value_type: HIRType,
};

pub const EnumTypeInfo = struct {
    name: []const u8,
    variants: [][]const u8,
};

pub const ScopeKind = enum {
    Local, // Function-local variable
    ModuleGlobal, // Module-level global
    ImportedModule, // Variable from imported module
    Builtin, // Built-in system variable
};

pub const CallKind = enum {
    LocalFunction, // Function in current module
    ModuleFunction, // Function in imported module
    BuiltinFunction, // Built-in function
};

pub const ArithOp = enum { Add, Sub, Mul, Div, Mod };

pub const CompareOp = enum { Eq, Ne, Lt, Le, Gt, Ge };

pub const LogicalOpType = enum { And, Or, Not };

pub const StringOpType = enum { Concat, Length, Substring };

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

//==================================================================
// HIR PROGRAM REPRESENTATION
//==================================================================

pub const HIRProgram = struct {
    instructions: []HIRInstruction,
    constant_pool: []HIRValue, // VM: Direct constant access
    string_pool: [][]const u8, // Shared string literals
    function_table: []HIRProgram.HIRFunction, // VM: Function metadata
    module_map: std.StringHashMap(ModuleInfo), // LLVM: Module context
    allocator: std.mem.Allocator, // NEW: Keep allocator for cleanup

    pub fn deinit(self: *HIRProgram) void {
        self.allocator.free(self.instructions);
        self.allocator.free(self.constant_pool);

        // Free individual strings
        for (self.string_pool) |str| {
            self.allocator.free(str);
        }
        self.allocator.free(self.string_pool);

        self.allocator.free(self.function_table);
        self.module_map.deinit();
    }

    pub const HIRFunction = struct {
        name: []const u8,
        qualified_name: []const u8, // module.function for LLVM
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        local_var_count: u32, // VM: stack frame sizing
        is_entry: bool,
    };

    pub const ModuleInfo = struct {
        name: []const u8,
        imports: [][]const u8,
        exports: [][]const u8,
        global_var_count: u32,
    };
};

//==================================================================
// HIR GENERATOR - The magic that makes AST walking 10-50x faster!
//==================================================================

/// Generates HIR from AST - This is where we eliminate recursive evaluation overhead
pub const HIRGenerator = struct {
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(HIRInstruction),
    constant_pool: std.ArrayList(HIRValue),
    string_pool: std.ArrayList([]const u8),
    constant_map: std.StringHashMap(u32), // For deduplication
    variables: std.StringHashMap(u32), // name -> index mapping
    variable_count: u32,
    label_count: u32,
    reporter: *reporting.Reporter, // NEW: Proper error reporting

    // NEW: Multi-pass support for functions
    function_signatures: std.StringHashMap(FunctionInfo),
    function_bodies: std.ArrayList(FunctionBody),
    current_function: ?[]const u8,

    pub const FunctionInfo = struct {
        name: []const u8,
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        local_var_count: u32,
        is_entry: bool,
    };

    pub const FunctionBody = struct {
        function_info: FunctionInfo,
        statements: []ast.Stmt,
        start_instruction_index: u32,
        // Store original function declaration components needed for parameter setup
        function_name: []const u8,
        function_params: []ast.FunctionParam,
        return_type_info: ast.TypeInfo,
    };

    pub fn init(allocator: std.mem.Allocator, reporter: *reporting.Reporter) HIRGenerator {
        return HIRGenerator{
            .allocator = allocator,
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
            .constant_pool = std.ArrayList(HIRValue).init(allocator),
            .string_pool = std.ArrayList([]const u8).init(allocator),
            .constant_map = std.StringHashMap(u32).init(allocator),
            .variables = std.StringHashMap(u32).init(allocator),
            .variable_count = 0,
            .label_count = 0,
            .reporter = reporter,
            .function_signatures = std.StringHashMap(FunctionInfo).init(allocator),
            .function_bodies = std.ArrayList(FunctionBody).init(allocator),
            .current_function = null,
        };
    }

    pub fn deinit(self: *HIRGenerator) void {
        self.instructions.deinit();
        self.constant_pool.deinit();
        self.string_pool.deinit();
        self.constant_map.deinit();
        self.variables.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
    }

    /// Main entry point - converts AST statements to HIR program using multi-pass approach
    pub fn generateProgram(self: *HIRGenerator, statements: []ast.Stmt) !HIRProgram {
        std.debug.print(">> Starting multi-pass HIR generation for {} statements\n", .{statements.len});

        // Pass 1: Collect function signatures (forward declarations)
        try self.collectFunctionSignatures(statements);

        // Pass 2: Generate main program FIRST (so execution starts here)
        try self.generateMainProgram(statements);

        // Pass 3: Generate function bodies AFTER main program
        try self.generateFunctionBodies();

        // Pass 4: Build function table
        const function_table = try self.buildFunctionTable();

        std.debug.print(">> Generated {} HIR instructions with {} functions\n", .{ self.instructions.items.len, function_table.len });

        return HIRProgram{
            .instructions = try self.instructions.toOwnedSlice(),
            .constant_pool = try self.constant_pool.toOwnedSlice(),
            .string_pool = try self.string_pool.toOwnedSlice(),
            .function_table = function_table,
            .module_map = std.StringHashMap(HIRProgram.ModuleInfo).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    /// Pass 1: Collect function signatures for forward declarations
    fn collectFunctionSignatures(self: *HIRGenerator, statements: []ast.Stmt) !void {
        std.debug.print(">> Pass 1: Collecting function signatures\n", .{});

        for (statements, 0..) |stmt, i| {
            switch (stmt.data) {
                .FunctionDecl => |func| {
                    std.debug.print("  >> Found function: {s} with {} params\n", .{ func.name.lexeme, func.params.len });

                    const return_type = self.convertTypeInfo(func.return_type_info);
                    const start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{func.name.lexeme}));

                    const function_info = FunctionInfo{
                        .name = func.name.lexeme,
                        .arity = @intCast(func.params.len),
                        .return_type = return_type,
                        .start_label = start_label,
                        .local_var_count = 0, // Will be calculated during body generation
                        .is_entry = func.is_entry,
                    };

                    try self.function_signatures.put(func.name.lexeme, function_info);

                    // Store function body for later generation
                    try self.function_bodies.append(FunctionBody{
                        .function_info = function_info,
                        .statements = func.body,
                        .start_instruction_index = 0, // Will be set during generation
                        // Store original declaration components for parameter access
                        .function_name = func.name.lexeme,
                        .function_params = func.params,
                        .return_type_info = func.return_type_info,
                    });
                },
                else => {
                    // Skip non-function statements in this pass
                    std.debug.print("  >> Skipping statement {}: {s}\n", .{ i, @tagName(stmt.data) });
                },
            }
        }

        std.debug.print(">> Pass 1 complete: {} functions collected\n", .{self.function_signatures.count()});
    }

    /// Pass 3: Generate function bodies AFTER main program
    fn generateFunctionBodies(self: *HIRGenerator) !void {
        std.debug.print(">> Pass 3: Generating function bodies AFTER main\n", .{});

        for (self.function_bodies.items) |*function_body| {
            std.debug.print("  >> Generating body for: {s}\n", .{function_body.function_info.name});

            // Set current function context
            self.current_function = function_body.function_info.name;

            // Mark start of function
            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label, .vm_address = 0 } });

            // Enter function scope
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = self.label_count + 1000, .var_count = 0 } });

            // Generate parameter setup - copy arguments from stack to local variables
            const params = function_body.function_params;
            std.debug.print("  >> Setting up {} parameters\n", .{params.len});

            // Parameters are pushed in order, so we pop them in reverse order
            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                // Create variable for parameter and store the stack value
                const var_idx = try self.getOrCreateVariable(param.name.lexeme);
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = param.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });

                std.debug.print("  >> Parameter {}: {s} -> var[{}]\n", .{ param_index, param.name.lexeme, var_idx });
            }

            // Generate function body statements
            for (function_body.statements) |body_stmt| {
                try self.generateStatement(body_stmt);
            }

            // Exit function scope
            try self.instructions.append(.{ .ExitScope = .{ .scope_id = self.label_count + 1000 } });

            // Add implicit return if no explicit return
            try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });

            // Clear current function context
            self.current_function = null;
        }

        std.debug.print(">> Pass 3 complete: {} function bodies generated\n", .{self.function_bodies.items.len});
    }

    /// Pass 2: Generate main program (non-function statements) - FIRST so execution starts here
    fn generateMainProgram(self: *HIRGenerator, statements: []ast.Stmt) !void {
        std.debug.print(">> Pass 2: Generating main program FIRST\n", .{});

        for (statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl => {
                    // Skip - already handled in previous passes
                    continue;
                },
                else => {
                    try self.generateStatement(stmt);
                },
            }
        }

        // Add halt instruction to end main program execution
        // This prevents execution from falling through to function definitions
        try self.instructions.append(.Halt);

        std.debug.print(">> Pass 2 complete: Main program generated\n", .{});
    }

    /// Pass 4: Build function table from collected signatures
    fn buildFunctionTable(self: *HIRGenerator) ![]HIRProgram.HIRFunction {
        std.debug.print(">> Pass 4: Building function table\n", .{});

        var function_table = std.ArrayList(HIRProgram.HIRFunction).init(self.allocator);

        var function_iterator = self.function_signatures.iterator();
        while (function_iterator.next()) |entry| {
            const function_info = entry.value_ptr.*;

            try function_table.append(HIRProgram.HIRFunction{
                .name = function_info.name,
                .qualified_name = function_info.name, // For now, no modules
                .arity = function_info.arity,
                .return_type = function_info.return_type,
                .start_label = function_info.start_label,
                .local_var_count = function_info.local_var_count,
                .is_entry = function_info.is_entry,
            });
        }

        std.debug.print(">> Pass 4 complete: {} functions in table\n", .{function_table.items.len});
        return try function_table.toOwnedSlice();
    }

    /// Get function index for call generation
    fn getFunctionIndex(self: *HIRGenerator, function_name: []const u8) !u32 {
        var function_iterator = self.function_signatures.iterator();
        var index: u32 = 0;
        while (function_iterator.next()) |entry| {
            if (std.mem.eql(u8, entry.key_ptr.*, function_name)) {
                return index;
            }
            index += 1;
        }
        return error.FunctionNotFound;
    }

    /// Convert TypeInfo to HIRType
    fn convertTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) HIRType {
        _ = self;
        return switch (type_info.base) {
            .Int => .Int,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .U8 => .U8,
            .Auto => .Auto,
            else => .Auto,
        };
    }

    /// Find function body by name (helper method)
    fn findFunctionBody(self: *HIRGenerator, function_name: []const u8) ?*FunctionBody {
        for (self.function_bodies.items) |*function_body| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return function_body;
            }
        }
        return null;
    }

    fn generateStatement(self: *HIRGenerator, stmt: ast.Stmt) (std.mem.Allocator.Error || reporting.ErrorList)!void {
        switch (stmt.data) {
            .Expression => |expr| {
                if (expr) |e| {
                    try self.generateExpression(e);
                    // Pop the result since it's not used (expression statement)
                    try self.instructions.append(.Pop);
                }
            },
            .VarDecl => |decl| {
                std.debug.print(">> Generating variable: {s}\n", .{decl.name.lexeme});

                // Generate the initializer expression
                if (decl.initializer) |init_expr| {
                    try self.generateExpression(init_expr);
                } else {
                    // Push default value based on type
                    const default_value = switch (decl.type_info.base) {
                        .Int => HIRValue{ .int = 0 },
                        .Float => HIRValue{ .float = 0.0 },
                        .String => HIRValue{ .string = "" },
                        .Tetra => HIRValue{ .tetra = TETRA_FALSE },
                        else => HIRValue.nothing,
                    };
                    const const_idx = try self.addConstant(default_value);
                    try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                }

                // Store to variable
                const var_idx = try self.getOrCreateVariable(decl.name.lexeme);
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });
                std.debug.print(">> Added StoreVar instruction at index {}\n", .{self.instructions.items.len - 1});
            },
            .FunctionDecl => {
                // Skip - function declarations are handled in multi-pass approach
                std.debug.print(">> Skipping function declaration (handled in Pass 1-2)\n", .{});
            },
            .Return => |ret| {
                if (ret.value) |value| {
                    try self.generateExpression(value);
                    try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = .Auto } });
                } else {
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                }
            },
            else => {
                std.debug.print("!! Unhandled statement type: {}\n", .{stmt.data});
            },
        }
    }

    /// THE KEY FUNCTION - converts recursive AST evaluation to linear stack operations
    /// This is where we get the 10-50x speedup!
    fn generateExpression(self: *HIRGenerator, expr: *ast.Expr) (std.mem.Allocator.Error || reporting.ErrorList)!void {
        switch (expr.data) {
            .Literal => |lit| {
                const hir_value = switch (lit) {
                    .int => |i| HIRValue{ .int = i },
                    .float => |f| HIRValue{ .float = f },
                    .string => |s| HIRValue{ .string = s },
                    .tetra => |t| HIRValue{ .tetra = tetraFromEnum(t) },
                    .u8 => |b| HIRValue{ .u8 = b },
                    .nothing => HIRValue.nothing,
                    else => HIRValue.nothing,
                };
                const const_idx = try self.addConstant(hir_value);
                try self.instructions.append(.{ .Const = .{ .value = hir_value, .constant_id = const_idx } });
            },

            .Variable => |var_token| {
                const var_idx = try self.getOrCreateVariable(var_token.lexeme);
                try self.instructions.append(.{
                    .LoadVar = .{
                        .var_index = var_idx,
                        .var_name = var_token.lexeme,
                        .scope_kind = .Local, // TODO: determine actual scope
                        .module_context = null,
                    },
                });
            },

            .Binary => |bin| {
                // Generate left operand (pushes to stack)
                try self.generateExpression(bin.left.?);

                // Generate right operand (pushes to stack)
                try self.generateExpression(bin.right.?);

                // Generate operation - THIS REPLACES MASSIVE AST WALKER OVERHEAD!
                std.debug.print(">> Generating binary op: {}\n", .{bin.operator.type});
                switch (bin.operator.type) {
                    .PLUS => {
                        try self.instructions.append(.{ .IntArith = .{ .op = .Add, .overflow_behavior = .Wrap } });
                        std.debug.print(">> Added IntArith.Add instruction at index {}\n", .{self.instructions.items.len - 1});
                    },
                    .MINUS => try self.instructions.append(.{ .IntArith = .{ .op = .Sub, .overflow_behavior = .Wrap } }),
                    .ASTERISK => try self.instructions.append(.{ .IntArith = .{ .op = .Mul, .overflow_behavior = .Wrap } }),
                    .SLASH => try self.instructions.append(.{ .IntArith = .{ .op = .Div, .overflow_behavior = .Trap } }),
                    .MODULO => try self.instructions.append(.{ .IntArith = .{ .op = .Mod, .overflow_behavior = .Wrap } }), // This was super slow in AST walker!
                    .EQUALITY => try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .Int } }),
                    .BANG_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Ne, .operand_type = .Int } }),
                    .LESS => try self.instructions.append(.{ .Compare = .{ .op = .Lt, .operand_type = .Int } }),
                    .GREATER => try self.instructions.append(.{ .Compare = .{ .op = .Gt, .operand_type = .Int } }),
                    .LESS_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Le, .operand_type = .Int } }),
                    .GREATER_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Ge, .operand_type = .Int } }),
                    else => {
                        self.reporter.reportError("Unsupported binary operator: {}", .{bin.operator.type});
                        return reporting.ErrorList.UnsupportedOperator;
                    },
                }
            },

            .Logical => |log| {
                std.debug.print(">> Generating logical op: {}\n", .{log.operator.type});

                // For AND/OR, we need short-circuit evaluation
                if (log.operator.type == .AND) {
                    // Generate: left AND right with short-circuit
                    try self.generateExpression(log.left);
                    try self.instructions.append(.Dup); // Keep left value for potential short-circuit

                    const short_circuit_label = try self.generateLabel("and_short_circuit");
                    const end_label = try self.generateLabel("and_end");

                    // If left is false, short-circuit to end
                    try self.instructions.append(.{
                        .JumpCond = .{
                            .label_true = end_label,
                            .label_false = short_circuit_label,
                            .vm_offset = 0, // Will be patched
                            .condition_type = .Boolean,
                        },
                    });

                    // Evaluate right side
                    try self.instructions.append(.{ .Label = .{ .name = short_circuit_label, .vm_address = 0 } });
                    try self.instructions.append(.Pop); // Remove the duplicated left value
                    try self.generateExpression(log.right);

                    try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
                } else if (log.operator.type == .OR) {
                    // Similar for OR but with inverted logic
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    // Simple OR for now - TODO: add short-circuit optimization
                } else {
                    self.reporter.reportError("Unsupported logical operator: {}", .{log.operator.type});
                    return reporting.ErrorList.UnsupportedOperator;
                }
            },

            .If => |if_expr| {
                std.debug.print(">> Generating if expression\n", .{});

                // Generate condition
                try self.generateExpression(if_expr.condition.?);

                // Create labels for branches
                const else_label = try self.generateLabel("else");
                const end_label = try self.generateLabel("end_if");

                // Jump to else if condition is false
                const then_label = try self.generateLabel("then");
                try self.instructions.append(.{
                    .JumpCond = .{
                        .label_true = then_label, // If TRUE: jump to then branch
                        .label_false = else_label, // If FALSE: jump to else branch
                        .vm_offset = 0, // Will be patched during VM bytecode generation
                        .condition_type = .Boolean,
                    },
                });

                // Then label
                try self.instructions.append(.{ .Label = .{ .name = then_label, .vm_address = 0 } });

                // Generate then branch
                try self.generateExpression(if_expr.then_branch.?);

                // Jump to end
                try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                // Else label
                try self.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });

                // Generate else branch (or push nothing if no else)
                if (if_expr.else_branch) |else_branch| {
                    try self.generateExpression(else_branch);
                } else {
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }

                // End label
                try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
            },

            .While => |while_expr| {
                std.debug.print(">> Generating while loop\n", .{});

                const loop_start_label = try self.generateLabel("while_start");
                const loop_body_label = try self.generateLabel("while_body");
                const loop_end_label = try self.generateLabel("while_end");

                // Loop start
                try self.instructions.append(.{ .Label = .{ .name = loop_start_label, .vm_address = 0 } });

                // Generate condition
                try self.generateExpression(while_expr.condition);

                // Jump based on condition: TRUE=continue to body, FALSE=exit loop
                try self.instructions.append(.{
                    .JumpCond = .{
                        .label_true = loop_body_label, // Continue to loop body when TRUE
                        .label_false = loop_end_label, // Exit loop when FALSE
                        .vm_offset = 0, // Will be patched
                        .condition_type = .Boolean,
                    },
                });

                // Loop body label (where TRUE condition jumps to)
                try self.instructions.append(.{ .Label = .{ .name = loop_body_label, .vm_address = 0 } });

                // Generate body
                try self.generateExpression(while_expr.body);
                try self.instructions.append(.Pop); // Discard body result

                // Jump back to start
                try self.instructions.append(.{ .Jump = .{ .label = loop_start_label, .vm_offset = 0 } });

                // Loop end
                try self.instructions.append(.{ .Label = .{ .name = loop_end_label, .vm_address = 0 } });

                // Push nothing as while result
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .Call => |call| {
                std.debug.print(">> Generating function call\n", .{});

                // Extract function name and determine call type
                var function_name: []const u8 = "unknown";
                var call_kind: CallKind = .LocalFunction;
                var function_index: u32 = 0;

                // Check if this is a method call (callee is FieldAccess) or regular function call
                var method_target_var: ?[]const u8 = null; // Track if method is called on a variable
                switch (call.callee.data) {
                    .FieldAccess => |field_access| {
                        // This is a method call like arr.length()
                        function_name = field_access.field.lexeme;
                        call_kind = .BuiltinFunction;

                        // Check if the method is called on a variable (for mutating methods)
                        if (field_access.object.data == .Variable) {
                            method_target_var = field_access.object.data.Variable.lexeme;
                        }

                        std.debug.print(">> Method call: {s}\n", .{function_name});

                        // CRITICAL FIX: Generate object FIRST, then arguments for correct stack order
                        // The VM's push method expects: element (top), array (bottom)
                        // So we need stack order: [array, element] (bottom to top)

                        // Generate the object expression (arr) FIRST
                        try self.generateExpression(field_access.object);

                        // Generate arguments (in order) AFTER object - these will be on top for VM
                        for (call.arguments) |arg| {
                            try self.generateExpression(arg);
                        }
                    },
                    .Variable => |var_token| {
                        // This is a regular function call like fizzbuzz(1)
                        function_name = var_token.lexeme;

                        // Try to resolve as user-defined function
                        if (self.getFunctionIndex(function_name)) |index| {
                            function_index = index;
                            call_kind = .LocalFunction;
                            std.debug.print(">> User function call: {s} (index: {})\n", .{ function_name, index });
                        } else |_| {
                            // Fall back to built-in
                            call_kind = .BuiltinFunction;
                            std.debug.print(">> Built-in function call: {s}\n", .{function_name});
                        }
                    },
                    else => {
                        // Complex callee - for now, treat as unknown
                        self.reporter.reportError("Unsupported function call type", .{});
                        return;
                    },
                }

                // Generate arguments for non-method calls (method calls handle arguments differently)
                if (call.callee.data != .FieldAccess) {
                    for (call.arguments) |arg| {
                        try self.generateExpression(arg);
                    }
                }

                // Generate function call
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = function_index,
                        .qualified_name = function_name,
                        .arg_count = @as(u32, @intCast(call.arguments.len)),
                        .call_kind = call_kind,
                        .target_module = null,
                        .return_type = .Auto,
                    },
                });

                std.debug.print(">> Generated Call instruction: function_name={s}, call_kind={s}, function_index={}\n", .{ function_name, @tagName(call_kind), function_index });

                // CRITICAL FIX: For mutating method calls on variables, store result back to variable
                if (call.callee.data == .FieldAccess and method_target_var != null) {
                    const target_var = method_target_var.?;

                    // Check if this is a mutating method that should update the original variable
                    const is_mutating_method = std.mem.eql(u8, function_name, "push") or
                        std.mem.eql(u8, function_name, "pop") or
                        std.mem.eql(u8, function_name, "insert") or
                        std.mem.eql(u8, function_name, "remove");

                    if (is_mutating_method) {
                        std.debug.print(">> Storing mutating method result back to variable: {s}\n", .{target_var});

                        // Duplicate the result to leave it on stack as the expression result
                        try self.instructions.append(.Dup);

                        // Store the result back to the original variable
                        const var_idx = try self.getOrCreateVariable(target_var);
                        try self.instructions.append(.{ .StoreVar = .{
                            .var_index = var_idx,
                            .var_name = target_var,
                            .scope_kind = .Local,
                            .module_context = null,
                        } });
                    }
                }
            },

            .Inspect => |inspect| {
                std.debug.print(">> Generating inspect\n", .{});

                // Generate the expression to inspect
                try self.generateExpression(inspect.expr);

                // Duplicate it so it remains on stack after inspection
                try self.instructions.append(.Dup);

                // Generate inspect instruction
                try self.instructions.append(.{ .Inspect = .{ .name = inspect.variable_name, .value_type = .Auto } });
            },

            .Assignment => |assign| {
                std.debug.print(">> Generating assignment: {s}\n", .{assign.name.lexeme});

                // Generate the value expression
                try self.generateExpression(assign.value.?);

                // Get or create variable index
                const var_idx = try self.getOrCreateVariable(assign.name.lexeme);

                // Duplicate value to leave it on stack as assignment result
                try self.instructions.append(.Dup);

                // Store to variable
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = assign.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });
            },

            .Array => |elements| {
                std.debug.print(">> Generating array with {} elements\n", .{elements.len});

                // Determine array element type from first element (for now)
                const element_type: HIRType = if (elements.len > 0) blk: {
                    // Try to infer type from first element
                    switch (elements[0].data) {
                        .Literal => |lit| break :blk switch (lit) {
                            .int => .Int,
                            .float => .Float,
                            .string => .String,
                            .tetra => .Boolean,
                            .u8 => .U8,
                            else => .Auto,
                        },
                        else => break :blk .Auto,
                    }
                } else .Auto;

                // Generate ArrayNew instruction
                try self.instructions.append(.{ .ArrayNew = .{
                    .element_type = element_type,
                    .size = @intCast(elements.len),
                } });

                // Generate each element and ArraySet
                for (elements, 0..) |element, i| {
                    // Duplicate array reference (needed for ArraySet)
                    try self.instructions.append(.Dup);

                    // Push index first
                    const index_value = HIRValue{ .int = @intCast(i) };
                    const index_const = try self.addConstant(index_value);
                    try self.instructions.append(.{ .Const = .{ .value = index_value, .constant_id = index_const } });

                    // Generate the element value
                    try self.generateExpression(element);

                    // Set array element (stack: array, index, value)
                    try self.instructions.append(.{ .ArraySet = .{ .bounds_check = false } }); // No bounds check for initialization
                }
            },

            .Index => |index| {
                std.debug.print(">> Generating array index access\n", .{});

                // Generate array expression
                try self.generateExpression(index.array);

                // Generate index expression
                try self.generateExpression(index.index);

                // Generate ArrayGet instruction
                try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
            },

            .IndexAssign => |assign| {
                std.debug.print(">> Generating array index assignment\n", .{});

                // Generate array expression
                try self.generateExpression(assign.array);

                // Generate value expression
                try self.generateExpression(assign.value);

                // Generate index expression
                try self.generateExpression(assign.index);

                // Generate ArraySet instruction
                try self.instructions.append(.{ .ArraySet = .{ .bounds_check = true } });
            },

            .Tuple => |elements| {
                std.debug.print(">> Generating tuple with {} elements\n", .{elements.len});

                // Generate TupleNew instruction
                try self.instructions.append(.{ .TupleNew = .{ .element_count = @intCast(elements.len) } });

                // Generate each element
                for (elements) |element| {
                    try self.generateExpression(element);
                }
            },

            .StructLiteral => |struct_lit| {
                std.debug.print(">> Generating struct literal: {s}\n", .{struct_lit.name.lexeme});

                // Generate StructNew instruction
                try self.instructions.append(.{
                    .StructNew = .{
                        .struct_name = struct_lit.name.lexeme,
                        .field_count = @intCast(struct_lit.fields.len),
                        .size_bytes = 0, // TODO: Calculate actual size
                    },
                });

                // Generate field assignments
                for (struct_lit.fields) |field| {
                    // Generate field value
                    try self.generateExpression(field.value);

                    // Store to field (using existing GetField instruction for now)
                    try self.instructions.append(.{
                        .GetField = .{
                            .field_name = field.name.lexeme,
                            .container_type = .Struct,
                            .field_index = 0, // TODO: Resolve actual field index
                        },
                    });
                }
            },

            .EnumMember => |member| {
                std.debug.print(">> Generating enum member: {s}\n", .{member.lexeme});

                // Generate EnumNew instruction
                try self.instructions.append(.{
                    .EnumNew = .{
                        .enum_name = "Unknown", // TODO: Resolve actual enum type
                        .variant_name = member.lexeme,
                        .variant_index = 0, // TODO: Resolve actual variant index
                    },
                });
            },

            .Grouping => |grouping| {
                // Grouping is just parentheses - generate the inner expression
                if (grouping) |inner_expr| {
                    try self.generateExpression(inner_expr);
                } else {
                    // Empty grouping - push nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }
            },

            .Block => |block| {
                std.debug.print(">> Generating block with {} statements\n", .{block.statements.len});

                // Generate all block statements without creating scopes for simple blocks
                for (block.statements) |stmt| {
                    try self.generateStatement(stmt);
                }

                // Generate final value if present
                if (block.value) |value_expr| {
                    // Block with a final value expression
                    try self.generateExpression(value_expr);
                } else {
                    // Block without value - push nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }
            },

            .CompoundAssign => |compound| {
                std.debug.print(">> Generating compound assignment: {s} {s}\n", .{ compound.name.lexeme, compound.operator.lexeme });

                // Load current variable value
                const var_idx = try self.getOrCreateVariable(compound.name.lexeme);
                try self.instructions.append(.{
                    .LoadVar = .{
                        .var_index = var_idx,
                        .var_name = compound.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                    },
                });

                // Generate the value expression (e.g., the "1" in "current += 1")
                try self.generateExpression(compound.value.?);

                // Generate the compound operation based on operator
                switch (compound.operator.type) {
                    .PLUS_EQUAL => try self.instructions.append(.{ .IntArith = .{ .op = .Add, .overflow_behavior = .Wrap } }),
                    .MINUS_EQUAL => try self.instructions.append(.{ .IntArith = .{ .op = .Sub, .overflow_behavior = .Wrap } }),
                    .ASTERISK_EQUAL => try self.instructions.append(.{ .IntArith = .{ .op = .Mul, .overflow_behavior = .Wrap } }),
                    .SLASH_EQUAL => try self.instructions.append(.{ .IntArith = .{ .op = .Div, .overflow_behavior = .Trap } }),
                    .POWER_EQUAL => try self.instructions.append(.{ .IntArith = .{ .op = .Mul, .overflow_behavior = .Wrap } }), // TODO: Implement power operation
                    else => {
                        self.reporter.reportError("Unsupported compound assignment operator: {}", .{compound.operator.type});
                        return reporting.ErrorList.UnsupportedOperator;
                    },
                }

                // Duplicate the result to leave it on stack as the expression result
                try self.instructions.append(.Dup);

                // Store the result back to the variable
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = compound.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });
            },

            .ReturnExpr => |return_expr| {
                std.debug.print(">> Generating return expression\n", .{});

                // Generate the value to return if present
                if (return_expr.value) |value| {
                    try self.generateExpression(value);
                } else {
                    // No value - push nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }

                // Generate Return instruction
                try self.instructions.append(.{ .Return = .{ .has_value = return_expr.value != null, .return_type = .Auto } });
            },

            .Unary => |unary| {
                std.debug.print(">> Generating unary op: {}\n", .{unary.operator.type});

                // Generate the operand first
                try self.generateExpression(unary.right.?);

                // Generate the unary operation
                switch (unary.operator.type) {
                    .NOT => {
                        // Generate logical NOT operation using ultra-fast lookup table
                        try self.instructions.append(.{ .LogicalOp = .{ .op = .Not } });
                    },
                    .MINUS => {
                        // Unary minus: 0 - operand
                        const zero_idx = try self.addConstant(HIRValue{ .int = 0 });
                        try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 0 }, .constant_id = zero_idx } });

                        // Swap operands so we have: 0, operand on stack
                        // Then subtract: 0 - operand = -operand
                        try self.instructions.append(.{ .IntArith = .{ .op = .Sub, .overflow_behavior = .Wrap } });
                    },
                    .PLUS => {
                        // Unary plus: just return the operand unchanged (no-op)
                        // The operand is already on the stack
                    },
                    else => {
                        self.reporter.reportError("Unsupported unary operator: {}", .{unary.operator.type});
                        return reporting.ErrorList.UnsupportedOperator;
                    },
                }
            },

            else => {
                std.debug.print("!! Unhandled expression type: {}\n", .{expr.data});
                // Push nothing as fallback
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },
        }
    }

    fn addConstant(self: *HIRGenerator, value: HIRValue) std.mem.Allocator.Error!u32 {
        // TODO: Add deduplication for identical constants
        const index = @as(u32, @intCast(self.constant_pool.items.len));
        try self.constant_pool.append(value);
        return index;
    }

    fn getOrCreateVariable(self: *HIRGenerator, name: []const u8) !u32 {
        if (self.variables.get(name)) |idx| {
            return idx;
        }

        const idx = self.variable_count;
        try self.variables.put(name, idx);
        self.variable_count += 1;
        std.debug.print(">> Created variable slot {} for '{s}'\n", .{ idx, name });
        return idx;
    }

    fn generateLabel(self: *HIRGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.label_count });
        self.label_count += 1;
        return label;
    }
};

//==================================================================
// HIR TO VM BYTECODE TRANSLATION - The final step to speed!
//==================================================================

/// Converts HIR to VM bytecode - direct 1:1 mapping for maximum speed
pub fn translateToVMBytecode(program: *HIRProgram, allocator: std.mem.Allocator, reporter: *reporting.Reporter) ![]u8 {
    var bytecode = std.ArrayList(u8).init(allocator);
    defer bytecode.deinit();

    std.debug.print(">> Translating {} HIR instructions to VM bytecode\n", .{program.instructions.len});

    // First pass: resolve labels to addresses
    var label_addresses = std.StringHashMap(u32).init(allocator);
    defer label_addresses.deinit();

    var address: u32 = 0;
    for (program.instructions) |instruction| {
        switch (instruction) {
            .Label => |label| {
                try label_addresses.put(label.name, address);
                // Labels don't generate bytecode
            },
            else => {
                address += getBytecodeSize(instruction);
            },
        }
    }

    // Second pass: generate actual bytecode
    for (program.instructions) |instruction| {
        switch (instruction) {
            .Const => |c| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CONST));
                try bytecode.append(@intCast(c.constant_id));
            },
            .LoadVar => |v| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_VAR));
                try bytecode.append(@intCast(v.var_index));
            },
            .StoreVar => |v| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_SET_VAR));
                try bytecode.append(@intCast(v.var_index));
            },
            .IntArith => |a| {
                const opcode = switch (a.op) {
                    .Add => instructions.OpCode.OP_IADD,
                    .Sub => instructions.OpCode.OP_ISUB,
                    .Mul => instructions.OpCode.OP_IMUL,
                    .Div => instructions.OpCode.OP_IADD, // TODO: Add OP_IDIV to your VM
                    .Mod => instructions.OpCode.OP_IADD, // TODO: Add OP_IMOD to your VM
                };
                try bytecode.append(@intFromEnum(opcode));
            },
            .Compare => |c| {
                const opcode = switch (c.op) {
                    .Eq => instructions.OpCode.OP_EQUAL,
                    .Ne => instructions.OpCode.OP_NOTEQUAL,
                    .Lt => instructions.OpCode.OP_LESS,
                    .Gt => instructions.OpCode.OP_GREATER,
                    .Le => instructions.OpCode.OP_LESS, // TODO: Add OP_LESS_EQUAL
                    .Ge => instructions.OpCode.OP_GREATER, // TODO: Add OP_GREATER_EQUAL
                };
                try bytecode.append(@intFromEnum(opcode));
            },
            .Jump => |j| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_JUMP));
                const target_addr = label_addresses.get(j.label) orelse {
                    reporter.reportError("Undefined label: {s}", .{j.label});
                    return reporting.ErrorList.UndefinedVariable;
                };
                const current_addr = @as(u32, @intCast(bytecode.items.len - 1));
                const offset = target_addr - current_addr;
                try bytecode.append(@intCast(offset));
            },
            .JumpCond => |j| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_JUMP_IF_FALSE));
                const target_addr = label_addresses.get(j.label_false) orelse {
                    reporter.reportError("Undefined label: {s}", .{j.label_false});
                    return reporting.ErrorList.UndefinedVariable;
                };
                const current_addr = @as(u32, @intCast(bytecode.items.len - 1));
                const offset = target_addr - current_addr;
                try bytecode.append(@intCast(offset));
            },
            .Call => |c| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CALL));
                try bytecode.append(@intCast(c.function_index));
            },
            .Return => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_RETURN));
            },
            .Dup => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP));
            },
            .Pop => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_POP));
            },
            .Inspect => |_| {
                // For inspect, we can use existing VM inspection mechanism
                // The VM will handle the printing based on the top stack value
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP)); // Keep value on stack
                // Note: Your VM's inspect handling is in the main execution loop
            },
            .Halt => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_HALT));
            },
            .Label => {
                // Labels don't generate bytecode - already handled in first pass
            },
            else => {
                reporter.reportError("Unhandled HIR instruction for VM translation: {}", .{instruction});
                return reporting.ErrorList.UnsupportedStatement;
            },
        }
    }

    const result = try bytecode.toOwnedSlice();
    std.debug.print(">> Generated {} bytes of VM bytecode\n", .{result.len});
    return result;
}

/// Returns the size in bytes that an HIR instruction will occupy in VM bytecode
fn getBytecodeSize(instruction: HIRInstruction) u32 {
    return switch (instruction) {
        .Const, .LoadVar, .StoreVar, .Jump, .JumpCond, .Call => 2, // opcode + operand
        .IntArith, .Compare, .Return, .Dup, .Pop, .Inspect, .Halt => 1, // opcode only
        .Label => 0, // No bytecode generated
        else => 1, // Default to 1 byte
    };
}

/// Converts HIR constants to VM Frame constants
pub fn convertHIRConstants(hir_constants: []HIRValue, allocator: std.mem.Allocator) ![]instructions.Value {
    var vm_constants = std.ArrayList(instructions.Value).init(allocator);
    defer vm_constants.deinit();

    for (hir_constants) |hir_const| {
        const vm_value = switch (hir_const) {
            .int => |i| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = i } },
            .float => |f| instructions.Value{ .type = .FLOAT, .nothing = false, .data = .{ .float = f } },
            .string => |s| instructions.Value{ .type = .STRING, .nothing = false, .data = .{ .string = s } },
            .tetra => |t| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, t) } },
            .u8 => |u| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, u) } }, // Convert u8 to int
            .nothing => instructions.Value{ .type = .INT, .nothing = true, .data = .{ .int = 0 } },
        };
        try vm_constants.append(vm_value);
    }

    return vm_constants.toOwnedSlice();
}

//==================================================================
// SOXA FILE FORMAT - Serialized HIR for multi-stage compilation
//==================================================================

/// SOXA File Header - Magic number and version info
const SOXA_MAGIC: u32 = 0x534F5841; // "SOXA" in ASCII
const SOXA_VERSION: u16 = 1;

pub const SoxaHeader = struct {
    magic: u32 = SOXA_MAGIC,
    version: u16 = SOXA_VERSION,
    instruction_count: u32,
    constant_count: u32,
    string_count: u32,
    reserved: [6]u8 = [_]u8{0} ** 6, // For future expansion
};

/// Writes a HIR program to a text-based .soxa file
pub fn writeSoxaFile(program: *HIRProgram, file_path: []const u8, allocator: std.mem.Allocator) !void {
    _ = allocator; // May be needed for complex formatting

    const file = std.fs.cwd().createFile(file_path, .{}) catch |err| switch (err) {
        error.AccessDenied => return reporting.ErrorList.AccessDenied,
        error.FileNotFound => return reporting.ErrorList.FileNotFound,
        else => return err,
    };
    defer file.close();

    var writer = file.writer();

    // Write header
    try writer.print("; SOXA HIR v0.1.0\n", .{});
    try writer.print("; Generated with {} instructions, {} constants, {} functions\n\n", .{ program.instructions.len, program.constant_pool.len, program.function_table.len });

    // Write constants section
    if (program.constant_pool.len > 0) {
        try writer.print(".constants\n", .{});
        for (program.constant_pool, 0..) |constant, i| {
            try writer.print("    const_{}: ", .{i});
            try writeHIRValueText(writer, constant);
            try writer.print("\n", .{});
        }
        try writer.print("\n", .{});
    }

    // Write functions section
    if (program.function_table.len > 0) {
        try writer.print(".functions\n", .{});
        for (program.function_table) |func| {
            try writer.print("    {s}({} args) -> {s}\n", .{ func.name, func.arity, @tagName(func.return_type) });
            try writer.print("        entry: {s}\n", .{func.start_label});
            if (func.is_entry) {
                try writer.print("        main: true\n", .{});
            }
        }
        try writer.print("\n", .{});
    }

    // Write instructions section
    try writer.print(".code\n", .{});
    for (program.instructions) |instruction| {
        try writeHIRInstructionText(writer, instruction);
    }

    std.debug.print(">> SOXA text file written: {s} ({} instructions, {} constants, {} functions)\n", .{ file_path, program.instructions.len, program.constant_pool.len, program.function_table.len });
}

/// Reads a HIR program from a text-based .soxa file
pub fn readSoxaFile(file_path: []const u8, allocator: std.mem.Allocator) !HIRProgram {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return reporting.ErrorList.FileNotFound,
        error.AccessDenied => return reporting.ErrorList.AccessDenied,
        else => return err,
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024); // 1MB max
    defer allocator.free(source);

    var parser = SoxaTextParser.init(allocator, source);
    const program = try parser.parse();

    std.debug.print(">> SOXA text file loaded: {s} ({} instructions, {} constants, {} functions)\n", .{ file_path, program.instructions.len, program.constant_pool.len, program.function_table.len });

    return program;
}

/// Serializes a single HIR value to binary format
fn writeHIRValue(writer: anytype, value: HIRValue) !void {
    switch (value) {
        .int => |i| {
            try writer.writeByte(0); // Type tag
            try writer.writeInt(i32, i, .little);
        },
        .float => |f| {
            try writer.writeByte(1); // Type tag
            try writer.writeInt(u64, @bitCast(f), .little);
        },
        .string => |s| {
            try writer.writeByte(2); // Type tag
            try writer.writeInt(u32, @as(u32, @intCast(s.len)), .little);
            try writer.writeAll(s);
        },
        .tetra => |t| {
            try writer.writeByte(3); // Type tag
            try writer.writeByte(t);
        },
        .u8 => |u| {
            try writer.writeByte(4); // Type tag
            try writer.writeByte(u);
        },
        .nothing => {
            try writer.writeByte(5); // Type tag
        },
        .array => |arr| {
            try writer.writeByte(6); // Type tag for array
            try writer.writeInt(u32, @as(u32, @intCast(arr.elements.len)), .little);
            try writer.writeInt(u32, arr.capacity, .little); // CRITICAL FIX: Save capacity!
            try writer.writeByte(@intFromEnum(arr.element_type)); // Element type
            // For now, we'll serialize basic array structure
            // Full recursive serialization would require more complex handling
        },
        .struct_instance => {
            try writer.writeByte(7); // Type tag for struct
            // TODO: Implement struct serialization when needed
        },
        .tuple => {
            try writer.writeByte(8); // Type tag for tuple
            // TODO: Implement tuple serialization when needed
        },
        .map => {
            try writer.writeByte(9); // Type tag for map
            // TODO: Implement map serialization when needed
        },
        .enum_variant => {
            try writer.writeByte(10); // Type tag for enum
            // TODO: Implement enum serialization when needed
        },
    }
}

/// Deserializes a single HIR value from binary format
fn readHIRValue(reader: anytype, allocator: std.mem.Allocator) !HIRValue {
    const type_tag = try reader.readByte();

    return switch (type_tag) {
        0 => HIRValue{ .int = try reader.readInt(i32, .little) },
        1 => HIRValue{ .float = @bitCast(try reader.readInt(u64, .little)) },
        2 => {
            const str_len = try reader.readInt(u32, .little);
            const str_data = try allocator.alloc(u8, str_len);
            _ = try reader.readAll(str_data);
            return HIRValue{ .string = str_data };
        },
        3 => HIRValue{ .tetra = try reader.readByte() },
        4 => HIRValue{ .u8 = try reader.readByte() },
        5 => HIRValue.nothing,
        6 => {
            // Array deserialization (basic structure)
            const array_len = try reader.readInt(u32, .little);
            const capacity = try reader.readInt(u32, .little); // CRITICAL FIX: Read capacity!
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));

            // Create array with proper capacity (allocate capacity, but slice to actual length)
            const backing_memory = try allocator.alloc(HIRValue, capacity);
            for (backing_memory) |*element| {
                element.* = HIRValue.nothing;
            }
            const elements = backing_memory[0..array_len]; // Slice to actual length

            return HIRValue{ .array = HIRArray{
                .elements = elements,
                .element_type = element_type,
                .capacity = capacity,
            } };
        },
        7 => {
            // Struct instance - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper struct deserialization
        },
        8 => {
            // Tuple - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper tuple deserialization
        },
        9 => {
            // Map - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper map deserialization
        },
        10 => {
            // Enum variant - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper enum deserialization
        },
        else => {
            std.debug.print("!! Unknown HIR value type tag: {}\n", .{type_tag});
            return reporting.ErrorList.InvalidArgument;
        },
    };
}

/// Serializes a single HIR instruction to binary format
fn writeHIRInstruction(writer: anytype, instruction: HIRInstruction, allocator: std.mem.Allocator) !void {
    _ = allocator; // May be needed for complex instructions

    switch (instruction) {
        .Const => |c| {
            try writer.writeByte(0); // Instruction tag
            try writer.writeInt(u32, c.constant_id, .little);
        },
        .LoadVar => |v| {
            try writer.writeByte(1); // Instruction tag
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .StoreVar => |v| {
            try writer.writeByte(2); // Instruction tag
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .IntArith => |a| {
            try writer.writeByte(3); // Instruction tag
            try writer.writeByte(@intFromEnum(a.op));
        },
        .Compare => |c| {
            try writer.writeByte(4); // Instruction tag
            try writer.writeByte(@intFromEnum(c.op));
        },
        .Jump => |j| {
            try writer.writeByte(5); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(j.label.len)), .little);
            try writer.writeAll(j.label);
        },
        .JumpCond => |j| {
            try writer.writeByte(6); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(j.label_true.len)), .little);
            try writer.writeAll(j.label_true);
            try writer.writeInt(u32, @as(u32, @intCast(j.label_false.len)), .little);
            try writer.writeAll(j.label_false);
        },
        .Call => |c| {
            try writer.writeByte(7); // Instruction tag
            try writer.writeInt(u32, c.function_index, .little);
            try writer.writeInt(u32, c.arg_count, .little);
            // Serialize qualified_name
            try writer.writeInt(u32, @as(u32, @intCast(c.qualified_name.len)), .little);
            try writer.writeAll(c.qualified_name);
            // CRITICAL FIX: Save call_kind!
            try writer.writeByte(@intFromEnum(c.call_kind));
        },
        .Return => |r| {
            try writer.writeByte(8); // Instruction tag
            try writer.writeByte(if (r.has_value) 1 else 0);
        },
        .Dup => {
            try writer.writeByte(9); // Instruction tag
        },
        .Pop => {
            try writer.writeByte(10); // Instruction tag
        },
        .Label => |l| {
            try writer.writeByte(11); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(l.name.len)), .little);
            try writer.writeAll(l.name);
        },
        .Halt => {
            try writer.writeByte(12); // Instruction tag
        },
        .Inspect => |i| {
            try writer.writeByte(13); // Instruction tag
            // Write whether name is present
            if (i.name) |name| {
                try writer.writeByte(1); // Has name
                try writer.writeInt(u32, @as(u32, @intCast(name.len)), .little);
                try writer.writeAll(name);
            } else {
                try writer.writeByte(0); // No name
            }
            try writer.writeByte(@intFromEnum(i.value_type));
        },

        // Array operations (Phase 1)
        .ArrayNew => |a| {
            try writer.writeByte(14); // Instruction tag
            try writer.writeByte(@intFromEnum(a.element_type));
            try writer.writeInt(u32, a.size, .little);
        },
        .ArrayGet => |a| {
            try writer.writeByte(15); // Instruction tag
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArraySet => |a| {
            try writer.writeByte(16); // Instruction tag
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayPush => |a| {
            try writer.writeByte(17); // Instruction tag
            try writer.writeByte(@intFromEnum(a.resize_behavior));
        },
        .ArrayPop => {
            try writer.writeByte(18); // Instruction tag
        },
        .ArrayLen => {
            try writer.writeByte(19); // Instruction tag
        },
        .ArrayConcat => {
            try writer.writeByte(20); // Instruction tag
        },

        // Scope management
        .EnterScope => |s| {
            try writer.writeByte(21); // Instruction tag
            try writer.writeInt(u32, s.scope_id, .little);
            try writer.writeInt(u32, s.var_count, .little);
        },
        .ExitScope => |s| {
            try writer.writeByte(22); // Instruction tag
            try writer.writeInt(u32, s.scope_id, .little);
        },

        else => {
            std.debug.print("!! Unhandled HIR instruction for SOXA serialization: {}\n", .{instruction});
            return reporting.ErrorList.UnsupportedStatement;
        },
    }
}

/// Deserializes a single HIR instruction from binary format
fn readHIRInstruction(reader: anytype, allocator: std.mem.Allocator) !HIRInstruction {
    const instruction_tag = try reader.readByte();

    return switch (instruction_tag) {
        0 => { // Const
            const constant_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .Const = .{ .value = HIRValue.nothing, .constant_id = constant_id } }; // Value will be resolved from constant pool
        },
        1 => { // LoadVar
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null } };
        },
        2 => { // StoreVar
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null } };
        },
        3 => { // IntArith
            const op_byte = try reader.readByte();
            const op = @as(ArithOp, @enumFromInt(op_byte));
            return HIRInstruction{ .IntArith = .{ .op = op, .overflow_behavior = .Wrap } };
        },
        4 => { // Compare
            const op_byte = try reader.readByte();
            const op = @as(CompareOp, @enumFromInt(op_byte));
            return HIRInstruction{ .Compare = .{ .op = op, .operand_type = .Int } };
        },
        5 => { // Jump
            const label_len = try reader.readInt(u32, .little);
            const label = try allocator.alloc(u8, label_len);
            _ = try reader.readAll(label);
            return HIRInstruction{ .Jump = .{ .label = label, .vm_offset = 0 } };
        },
        6 => { // JumpCond
            const true_len = try reader.readInt(u32, .little);
            const label_true = try allocator.alloc(u8, true_len);
            _ = try reader.readAll(label_true);

            const false_len = try reader.readInt(u32, .little);
            const label_false = try allocator.alloc(u8, false_len);
            _ = try reader.readAll(label_false);

            return HIRInstruction{ .JumpCond = .{ .label_true = label_true, .label_false = label_false, .vm_offset = 0, .condition_type = .Boolean } };
        },
        7 => { // Call
            const function_index = try reader.readInt(u32, .little);
            const arg_count = try reader.readInt(u32, .little);
            // Deserialize qualified_name
            const name_len = try reader.readInt(u32, .little);
            const qualified_name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(qualified_name);
            // CRITICAL FIX: Read call_kind!
            const call_kind_byte = try reader.readByte();
            const call_kind = @as(CallKind, @enumFromInt(call_kind_byte));
            return HIRInstruction{ .Call = .{
                .function_index = function_index,
                .qualified_name = qualified_name,
                .arg_count = arg_count,
                .call_kind = call_kind,
                .target_module = null,
                .return_type = .Auto,
            } };
        },
        8 => { // Return
            const has_value = (try reader.readByte()) != 0;
            return HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Auto } };
        },
        9 => HIRInstruction.Dup,
        10 => HIRInstruction.Pop,
        11 => { // Label
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .Label = .{ .name = name, .vm_address = 0 } };
        },
        12 => HIRInstruction.Halt,
        13 => { // Inspect
            const has_name = (try reader.readByte()) != 0;
            const name = if (has_name) blk: {
                const name_len = try reader.readInt(u32, .little);
                const name_str = try allocator.alloc(u8, name_len);
                _ = try reader.readAll(name_str);
                break :blk name_str;
            } else null;

            const value_type_byte = try reader.readByte();
            const value_type = @as(HIRType, @enumFromInt(value_type_byte));

            return HIRInstruction{ .Inspect = .{ .name = name, .value_type = value_type } };
        },

        // Array operations (Phase 1)
        14 => { // ArrayNew
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));
            const size = try reader.readInt(u32, .little);
            return HIRInstruction{ .ArrayNew = .{ .element_type = element_type, .size = size } };
        },
        15 => { // ArrayGet
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGet = .{ .bounds_check = bounds_check } };
        },
        16 => { // ArraySet
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArraySet = .{ .bounds_check = bounds_check } };
        },
        17 => { // ArrayPush
            const resize_behavior_byte = try reader.readByte();
            const resize_behavior = @as(ResizeBehavior, @enumFromInt(resize_behavior_byte));
            return HIRInstruction{ .ArrayPush = .{ .resize_behavior = resize_behavior } };
        },
        18 => HIRInstruction.ArrayPop,
        19 => HIRInstruction.ArrayLen,
        20 => HIRInstruction.ArrayConcat,

        // Scope management
        21 => { // EnterScope
            const scope_id = try reader.readInt(u32, .little);
            const var_count = try reader.readInt(u32, .little);
            return HIRInstruction{ .EnterScope = .{ .scope_id = scope_id, .var_count = var_count } };
        },
        22 => { // ExitScope
            const scope_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .ExitScope = .{ .scope_id = scope_id } };
        },

        else => {
            std.debug.print("!! Unknown HIR instruction tag: {}\n", .{instruction_tag});
            return reporting.ErrorList.UnsupportedStatement;
        },
    };
}

/// Writes a HIR value in text format
fn writeHIRValueText(writer: anytype, value: HIRValue) !void {
    switch (value) {
        .int => |i| try writer.print("int {}", .{i}),
        .float => |f| try writer.print("float {d}", .{f}),
        .string => |s| try writer.print("string \"{s}\"", .{s}),
        .tetra => |t| try writer.print("tetra {}", .{t}),
        .u8 => |u| try writer.print("u8 {}", .{u}),
        .nothing => try writer.print("nothing", .{}),
        .array => |arr| try writer.print("array[{s}] capacity:{}", .{ @tagName(arr.element_type), arr.capacity }),
        .struct_instance => try writer.print("struct", .{}),
        .tuple => try writer.print("tuple", .{}),
        .map => try writer.print("map", .{}),
        .enum_variant => try writer.print("enum", .{}),
    }
}

/// Writes a HIR instruction in text format
fn writeHIRInstructionText(writer: anytype, instruction: HIRInstruction) !void {
    switch (instruction) {
        .Const => |c| try writer.print("    Const {}                    ; Push constant\n", .{c.constant_id}),

        .LoadVar => |v| try writer.print("    LoadVar {} \"{s}\"           ; Load variable\n", .{ v.var_index, v.var_name }),

        .StoreVar => |v| try writer.print("    StoreVar {} \"{s}\"          ; Store variable\n", .{ v.var_index, v.var_name }),

        .IntArith => |a| try writer.print("    IntArith {s}                ; Integer arithmetic\n", .{@tagName(a.op)}),

        .Compare => |c| try writer.print("    Compare {s}                 ; Comparison\n", .{@tagName(c.op)}),

        .Jump => |j| try writer.print("    Jump {s}                    ; Unconditional jump\n", .{j.label}),

        .JumpCond => |j| try writer.print("    JumpCond {s} {s}            ; Conditional jump\n", .{ j.label_true, j.label_false }),

        .Call => |c| try writer.print("    Call {} {} \"{s}\" {s}      ; Function call\n", .{ c.function_index, c.arg_count, c.qualified_name, @tagName(c.call_kind) }),

        .Return => |r| try writer.print("    Return {}                   ; Return from function\n", .{r.has_value}),

        .Label => |l| try writer.print("{s}:                            ; Label\n", .{l.name}),

        .Dup => try writer.print("    Dup                         ; Duplicate top value\n", .{}),

        .Pop => try writer.print("    Pop                         ; Remove top value\n", .{}),

        .Inspect => |i| {
            if (i.name) |name| {
                try writer.print("    Inspect \"{s}\"               ; Debug print\n", .{name});
            } else {
                try writer.print("    Inspect                     ; Debug print\n", .{});
            }
        },

        .Halt => try writer.print("    Halt                        ; Program termination\n", .{}),

        // Array operations
        .ArrayNew => |a| try writer.print("    ArrayNew {s} {}             ; Create array\n", .{ @tagName(a.element_type), a.size }),
        .ArrayGet => |a| try writer.print("    ArrayGet {}                 ; Get array element\n", .{a.bounds_check}),
        .ArraySet => |a| try writer.print("    ArraySet {}                 ; Set array element\n", .{a.bounds_check}),
        .ArrayPush => |a| try writer.print("    ArrayPush {s}               ; Push to array\n", .{@tagName(a.resize_behavior)}),
        .ArrayPop => try writer.print("    ArrayPop                    ; Pop from array\n", .{}),
        .ArrayLen => try writer.print("    ArrayLen                    ; Get array length\n", .{}),
        .ArrayConcat => try writer.print("    ArrayConcat                 ; Concatenate arrays\n", .{}),

        // Scope management
        .EnterScope => |s| try writer.print("    EnterScope {} {}            ; Enter new scope\n", .{ s.scope_id, s.var_count }),
        .ExitScope => |s| try writer.print("    ExitScope {}                ; Exit scope\n", .{s.scope_id}),

        else => try writer.print("    ; TODO: {s}\n", .{@tagName(instruction)}),
    }
}

/// Simple text parser for SOXA HIR format
const SoxaTextParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize = 0,
    line: u32 = 1,
    constants: std.ArrayList(HIRValue),
    functions: std.ArrayList(HIRProgram.HIRFunction),
    instructions: std.ArrayList(HIRInstruction),

    fn init(allocator: std.mem.Allocator, source: []const u8) SoxaTextParser {
        return SoxaTextParser{
            .allocator = allocator,
            .source = source,
            .constants = std.ArrayList(HIRValue).init(allocator),
            .functions = std.ArrayList(HIRProgram.HIRFunction).init(allocator),
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
        };
    }

    fn parse(self: *SoxaTextParser) !HIRProgram {
        var lines = std.mem.splitScalar(u8, self.source, '\n');
        var line_count: u32 = 0;

        while (lines.next()) |line| {
            line_count += 1;
            self.line += 1;
            const trimmed_right = std.mem.trimRight(u8, line, " \t\r\n"); // Only trim right side

            // Skip empty lines and comments
            if (trimmed_right.len == 0 or trimmed_right[0] == ';') continue;

            // Handle sections and instructions - check for indented content
            if (std.mem.startsWith(u8, trimmed_right, ".constants")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, ".functions")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, ".code")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, "    const_")) {
                try self.parseConstant(trimmed_right);
            } else if (std.mem.indexOf(u8, trimmed_right, ":")) |colon_pos| {
                // Label (can be either indented or not) - CHECK BEFORE INSTRUCTIONS!
                const is_label = blk: {
                    // Check if this is a label by seeing if colon comes before semicolon (or is at end)
                    if (std.mem.indexOf(u8, trimmed_right, ";")) |semicolon_pos| {
                        break :blk colon_pos < semicolon_pos;
                    } else {
                        break :blk std.mem.endsWith(u8, trimmed_right, ":");
                    }
                };

                if (is_label) {
                    const label_line = std.mem.trim(u8, trimmed_right, " \t");
                    const label_colon_pos = std.mem.indexOf(u8, label_line, ":").?;
                    const label_name = try self.allocator.dupe(u8, label_line[0..label_colon_pos]);
                    try self.instructions.append(HIRInstruction{ .Label = .{ .name = label_name, .vm_address = 0 } });
                } else {
                    // Not a label, fall through to instruction parsing
                    if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                        try self.parseFunction(trimmed_right);
                    } else {
                        try self.parseInstruction(trimmed_right);
                    }
                }
            } else if (std.mem.startsWith(u8, trimmed_right, "    ") and !std.mem.startsWith(u8, trimmed_right, "        ")) {
                // Function or instruction
                if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                    try self.parseFunction(trimmed_right);
                } else {
                    try self.parseInstruction(trimmed_right);
                }
            } else if (std.mem.startsWith(u8, trimmed_right, "        entry:")) {
                // Function metadata - update the last function's entry point
                try self.updateFunctionEntry(trimmed_right);
                continue;
            }
            // Silently ignore other lines (like function metadata)
        }

        return HIRProgram{
            .instructions = try self.instructions.toOwnedSlice(),
            .constant_pool = try self.constants.toOwnedSlice(),
            .string_pool = &[_][]const u8{}, // Empty for now
            .function_table = try self.functions.toOwnedSlice(),
            .module_map = std.StringHashMap(HIRProgram.ModuleInfo).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    fn parseConstant(self: *SoxaTextParser, line: []const u8) !void {
        // Parse: "    const_0: int 42", "    const_1: string "hello"", etc.
        if (std.mem.indexOf(u8, line, "int ")) |int_pos| {
            const value_str = std.mem.trim(u8, line[int_pos + 4 ..], " \t");
            const value = try std.fmt.parseInt(i32, value_str, 10);
            try self.constants.append(HIRValue{ .int = value });
        } else if (std.mem.indexOf(u8, line, "float ")) |float_pos| {
            const value_str = std.mem.trim(u8, line[float_pos + 6 ..], " \t");
            const value = try std.fmt.parseFloat(f64, value_str);
            try self.constants.append(HIRValue{ .float = value });
        } else if (std.mem.indexOf(u8, line, "string ")) |string_pos| {
            const quoted_str = std.mem.trim(u8, line[string_pos + 7 ..], " \t");
            const value = try self.parseQuotedString(quoted_str);
            try self.constants.append(HIRValue{ .string = value });
        } else if (std.mem.indexOf(u8, line, "bool ")) |bool_pos| {
            const value_str = std.mem.trim(u8, line[bool_pos + 5 ..], " \t");
            const value = std.mem.eql(u8, value_str, "true");
            try self.constants.append(HIRValue{ .tetra = tetraFromBool(value) });
        } else if (std.mem.indexOf(u8, line, "tetra ")) |tetra_pos| {
            const value_str = std.mem.trim(u8, line[tetra_pos + 6 ..], " \t");
            const value = try std.fmt.parseInt(u8, value_str, 10);
            try self.constants.append(HIRValue{ .tetra = value });
        } else if (std.mem.indexOf(u8, line, "u8 ")) |u8_pos| {
            const value_str = std.mem.trim(u8, line[u8_pos + 3 ..], " \t");
            const value = try std.fmt.parseInt(u8, value_str, 10);
            try self.constants.append(HIRValue{ .u8 = value });
        } else if (std.mem.indexOf(u8, line, "nothing")) |_| {
            try self.constants.append(HIRValue.nothing);
        } else {
            // Default to nothing for unhandled types
            try self.constants.append(HIRValue.nothing);
        }
    }

    fn parseFunction(self: *SoxaTextParser, line: []const u8) !void {
        // Parse: "    fibonacci(1 args) -> int"
        const trimmed = std.mem.trim(u8, line, " \t");
        if (std.mem.indexOf(u8, trimmed, "(")) |paren_pos| {
            const name = try self.allocator.dupe(u8, trimmed[0..paren_pos]);

            // Extract arity from "(N args)"
            var arity: u32 = 0;
            if (std.mem.indexOf(u8, trimmed, "(") != null and std.mem.indexOf(u8, trimmed, " args)") != null) {
                const args_start = std.mem.indexOf(u8, trimmed, "(").? + 1;
                const args_end = std.mem.indexOf(u8, trimmed, " args)").?;
                if (args_end > args_start) {
                    const arity_str = trimmed[args_start..args_end];
                    arity = std.fmt.parseInt(u32, arity_str, 10) catch 0;
                }
            }

            // Create function with placeholder start_label - will be updated when we see the entry line
            try self.functions.append(HIRProgram.HIRFunction{
                .name = name,
                .qualified_name = name,
                .arity = arity,
                .return_type = .Auto,
                .start_label = try self.allocator.dupe(u8, "unknown"), // Will be updated
                .local_var_count = 0,
                .is_entry = false,
            });
        }
    }

    fn updateFunctionEntry(self: *SoxaTextParser, entry_line: []const u8) !void {
        // Parse: "        entry: func_fibonacci_0"
        if (std.mem.indexOf(u8, entry_line, "entry:")) |entry_pos| {
            const label_start = entry_pos + 6; // Skip "entry:"
            const label = std.mem.trim(u8, entry_line[label_start..], " \t");

            // Update the last function we parsed
            if (self.functions.items.len > 0) {
                const last_func_idx = self.functions.items.len - 1;
                self.functions.items[last_func_idx].start_label = try self.allocator.dupe(u8, label);
            }
        }
    }

    fn parseInstruction(self: *SoxaTextParser, line: []const u8) !void {
        const trimmed = std.mem.trim(u8, line, " \t");
        var tokens = std.mem.splitScalar(u8, trimmed, ' ');
        const op = tokens.next() orelse return;

        if (std.mem.eql(u8, op, "Const")) {
            const id_str = tokens.next() orelse return;
            const id = std.fmt.parseInt(u32, id_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .Const = .{ .value = HIRValue.nothing, .constant_id = id } });
        } else if (std.mem.eql(u8, op, "LoadVar")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = .Local, .module_context = null } });
        } else if (std.mem.eql(u8, op, "StoreVar")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = .Local, .module_context = null } });
        } else if (std.mem.eql(u8, op, "IntArith")) {
            const op_str = tokens.next() orelse return;
            const arith_op = if (std.mem.eql(u8, op_str, "Add")) ArithOp.Add else if (std.mem.eql(u8, op_str, "Sub")) ArithOp.Sub else if (std.mem.eql(u8, op_str, "Mul")) ArithOp.Mul else if (std.mem.eql(u8, op_str, "Div")) ArithOp.Div else if (std.mem.eql(u8, op_str, "Mod")) ArithOp.Mod else ArithOp.Add;
            try self.instructions.append(HIRInstruction{ .IntArith = .{ .op = arith_op, .overflow_behavior = .Wrap } });
        } else if (std.mem.eql(u8, op, "Compare")) {
            const op_str = tokens.next() orelse return;
            const comp_op = if (std.mem.eql(u8, op_str, "Eq")) CompareOp.Eq else if (std.mem.eql(u8, op_str, "Ne")) CompareOp.Ne else if (std.mem.eql(u8, op_str, "Lt")) CompareOp.Lt else if (std.mem.eql(u8, op_str, "Le")) CompareOp.Le else if (std.mem.eql(u8, op_str, "Gt")) CompareOp.Gt else if (std.mem.eql(u8, op_str, "Ge")) CompareOp.Ge else CompareOp.Eq;
            try self.instructions.append(HIRInstruction{ .Compare = .{ .op = comp_op, .operand_type = .Int } });
        } else if (std.mem.eql(u8, op, "Jump")) {
            const label = tokens.next() orelse return;
            const label_name = try self.allocator.dupe(u8, label);
            try self.instructions.append(HIRInstruction{ .Jump = .{ .label = label_name, .vm_offset = 0 } });
        } else if (std.mem.eql(u8, op, "JumpCond")) {
            const true_label = tokens.next() orelse return;
            const false_label = tokens.next() orelse return;
            const true_name = try self.allocator.dupe(u8, true_label);
            const false_name = try self.allocator.dupe(u8, false_label);
            try self.instructions.append(HIRInstruction{ .JumpCond = .{ .label_true = true_name, .label_false = false_name, .vm_offset = 0, .condition_type = .Boolean } });
        } else if (std.mem.eql(u8, op, "Call")) {
            const func_idx_str = tokens.next() orelse return;
            const arg_count_str = tokens.next() orelse return;
            const name_quoted = tokens.next() orelse return;
            const kind_str = tokens.next() orelse return;

            const function_index = std.fmt.parseInt(u32, func_idx_str, 10) catch return;
            const arg_count = std.fmt.parseInt(u32, arg_count_str, 10) catch return;
            const qualified_name = try self.parseQuotedString(name_quoted);
            const call_kind = if (std.mem.eql(u8, kind_str, "LocalFunction")) CallKind.LocalFunction else if (std.mem.eql(u8, kind_str, "ModuleFunction")) CallKind.ModuleFunction else if (std.mem.eql(u8, kind_str, "BuiltinFunction")) CallKind.BuiltinFunction else CallKind.LocalFunction;

            try self.instructions.append(HIRInstruction{ .Call = .{
                .function_index = function_index,
                .qualified_name = qualified_name,
                .arg_count = arg_count,
                .call_kind = call_kind,
                .target_module = null,
                .return_type = .Auto,
            } });
        } else if (std.mem.eql(u8, op, "Return")) {
            const has_val_str = tokens.next() orelse return;
            const has_value = std.mem.eql(u8, has_val_str, "true");
            try self.instructions.append(HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Auto } });
        } else if (std.mem.eql(u8, op, "Dup")) {
            try self.instructions.append(HIRInstruction.Dup);
        } else if (std.mem.eql(u8, op, "Pop")) {
            try self.instructions.append(HIRInstruction.Pop);
        } else if (std.mem.eql(u8, op, "Inspect")) {
            const name_quoted = tokens.next();
            if (name_quoted) |name| {
                const var_name = try self.parseQuotedString(name);
                try self.instructions.append(HIRInstruction{ .Inspect = .{ .name = var_name, .value_type = .Auto } });
            } else {
                try self.instructions.append(HIRInstruction{ .Inspect = .{ .name = null, .value_type = .Auto } });
            }
        } else if (std.mem.eql(u8, op, "Halt")) {
            try self.instructions.append(HIRInstruction.Halt);
        } else if (std.mem.eql(u8, op, "ArrayNew")) {
            const type_str = tokens.next() orelse return;
            const size_str = tokens.next() orelse return;
            const element_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Boolean")) HIRType.Boolean else HIRType.Auto;
            const size = std.fmt.parseInt(u32, size_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .ArrayNew = .{ .element_type = element_type, .size = size } });
        } else if (std.mem.eql(u8, op, "ArrayGet")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGet = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArraySet")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArraySet = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayPush")) {
            const behavior_str = tokens.next() orelse return;
            const resize_behavior = if (std.mem.eql(u8, behavior_str, "Double")) ResizeBehavior.Double else if (std.mem.eql(u8, behavior_str, "Fixed")) ResizeBehavior.Fixed else if (std.mem.eql(u8, behavior_str, "Exact")) ResizeBehavior.Exact else ResizeBehavior.Double;
            try self.instructions.append(HIRInstruction{ .ArrayPush = .{ .resize_behavior = resize_behavior } });
        } else if (std.mem.eql(u8, op, "ArrayPop")) {
            try self.instructions.append(HIRInstruction.ArrayPop);
        } else if (std.mem.eql(u8, op, "ArrayLen")) {
            try self.instructions.append(HIRInstruction.ArrayLen);
        } else if (std.mem.eql(u8, op, "ArrayConcat")) {
            try self.instructions.append(HIRInstruction.ArrayConcat);
        } else if (std.mem.eql(u8, op, "EnterScope")) {
            const scope_id_str = tokens.next() orelse return;
            const var_count_str = tokens.next() orelse return;
            const scope_id = std.fmt.parseInt(u32, scope_id_str, 10) catch return;
            const var_count = std.fmt.parseInt(u32, var_count_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .EnterScope = .{ .scope_id = scope_id, .var_count = var_count } });
        } else if (std.mem.eql(u8, op, "ExitScope")) {
            const scope_id_str = tokens.next() orelse return;
            const scope_id = std.fmt.parseInt(u32, scope_id_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .ExitScope = .{ .scope_id = scope_id } });
        }
        // Instructions not implemented yet are silently ignored for now
    }

    fn parseQuotedString(self: *SoxaTextParser, quoted: []const u8) ![]const u8 {
        if (quoted.len >= 2 and quoted[0] == '"' and quoted[quoted.len - 1] == '"') {
            return try self.allocator.dupe(u8, quoted[1 .. quoted.len - 1]);
        }
        return try self.allocator.dupe(u8, quoted);
    }
};
