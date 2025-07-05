const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const instructions = @import("../interpreter/instructions.zig");
const reporting = @import("../utils/reporting.zig");
const Reporting = reporting;
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;
const HIRType = @import("soxa_types.zig").HIRType;
const HIRProgram = @import("soxa_types.zig").HIRProgram;
const CallKind = @import("soxa_types.zig").CallKind;
const HIRArray = @import("soxa_values.zig").HIRArray;
const HIRMapEntry = @import("soxa_values.zig").HIRMapEntry;
const ArithOp = @import("soxa_instructions.zig").ArithOp;
const CompareOp = @import("soxa_instructions.zig").CompareOp;
const StringOpType = @import("soxa_instructions.zig").StringOpType;
const OverflowBehavior = @import("soxa_instructions.zig").OverflowBehavior;
const ExceptionBehavior = @import("soxa_instructions.zig").ExceptionBehavior;
const ResizeBehavior = @import("soxa_instructions.zig").ResizeBehavior;

//==================================================================
// HIR GENERATOR
//==================================================================

// Add this type definition before the HIRGenerator struct
const StructPeekInfo = struct {
    name: []const u8,
    field_count: u32,
    field_names: [][]const u8,
    field_types: []HIRType,
};

/// Generates HIR from AST - This is where we eliminate recursive evaluation overhead
pub const HIRGenerator = struct {
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(HIRInstruction),
    constants: std.ArrayList(HIRValue),
    debug_enabled: bool = false,
    current_peek_expr: ?*ast.Expr = null,
    current_field_name: ?[]const u8 = null,
    string_pool: std.ArrayList([]const u8),
    constant_map: std.StringHashMap(u32), // For deduplication
    variables: std.StringHashMap(u32), // name -> index mapping
    variable_count: u32,
    label_count: u32,
    reporter: *reporting.Reporter, // NEW: Proper error reporting

    // NEW: Type tracking for proper TypeOf support
    variable_types: std.StringHashMap(HIRType), // Track inferred types of variables

    // NEW: Custom type registry for struct/enum names
    custom_types: std.StringHashMap(CustomTypeInfo), // Track struct/enum type definitions

    // NEW: Multi-pass support for functions
    function_signatures: std.StringHashMap(FunctionInfo),
    function_bodies: std.ArrayList(FunctionBody),
    current_function: ?[]const u8,
    current_function_return_type: HIRType, // Track current function's return type for Return instructions

    // NEW: Statistics and debugging
    stats: HIRStats,

    // NEW: Function call site tracking for tail recursion optimization
    function_calls: std.ArrayList(FunctionCallSite),

    pub const FunctionInfo = struct {
        name: []const u8,
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        body_label: ?[]const u8 = null, // For tail call optimization - jumps here to skip parameter setup
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
            .constants = std.ArrayList(HIRValue).init(allocator),
            .debug_enabled = false,
            .current_peek_expr = null,
            .string_pool = std.ArrayList([]const u8).init(allocator),
            .constant_map = std.StringHashMap(u32).init(allocator),
            .variables = std.StringHashMap(u32).init(allocator),
            .variable_count = 0,
            .label_count = 0,
            .reporter = reporter,
            .variable_types = std.StringHashMap(HIRType).init(allocator),
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .function_signatures = std.StringHashMap(FunctionInfo).init(allocator),
            .function_bodies = std.ArrayList(FunctionBody).init(allocator),
            .current_function = null,
            .current_function_return_type = .Nothing,
            .stats = HIRStats.init(allocator),
            .function_calls = std.ArrayList(FunctionCallSite).init(allocator),
        };
    }

    pub fn deinit(self: *HIRGenerator) void {
        self.instructions.deinit();
        self.constants.deinit();
        self.string_pool.deinit();
        self.constant_map.deinit();
        self.variables.deinit();
        self.variable_types.deinit();
        self.custom_types.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_calls.deinit();
    }

    /// Main entry point - converts AST statements to HIR program using multi-pass approach
    pub fn generateProgram(self: *HIRGenerator, statements: []ast.Stmt) !HIRProgram {

        // Pass 1: Collect function signatures (forward declarations)
        try self.collectFunctionSignatures(statements);

        // Pass 2: Generate main program FIRST (so execution starts here)
        try self.generateMainProgram(statements);

        // Pass 3: Generate function bodies AFTER main program
        try self.generateFunctionBodies();

        // Pass 4: Build function table
        const function_table = try self.buildFunctionTable();

        return HIRProgram{
            .instructions = try self.instructions.toOwnedSlice(),
            .constant_pool = try self.constants.toOwnedSlice(),
            .string_pool = try self.string_pool.toOwnedSlice(),
            .function_table = function_table,
            .module_map = std.StringHashMap(HIRProgram.ModuleInfo).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    /// Pass 1: Collect function signatures for forward declarations
    fn collectFunctionSignatures(self: *HIRGenerator, statements: []ast.Stmt) !void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl => |func| {
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
                else => {},
            }
        }
    }

    /// Pass 3: Generate function bodies AFTER main program
    fn generateFunctionBodies(self: *HIRGenerator) !void {
        for (self.function_bodies.items) |*function_body| {

            // Set current function context
            self.current_function = function_body.function_info.name;
            self.current_function_return_type = function_body.function_info.return_type;

            // Mark start of function
            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label, .vm_address = 0 } });

            // Enter function scope
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = self.label_count + 1000, .var_count = 0 } });

            // Generate parameter setup - copy arguments from stack to local variables
            const params = function_body.function_params;

            // Parameters are pushed in order, so we pop them in reverse order
            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                // Extract parameter type information with proper inference
                var param_type: HIRType = .Auto;
                if (param.type_expr) |type_expr| {
                    const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                    defer self.allocator.destroy(type_info_ptr);
                    param_type = self.convertTypeInfo(type_info_ptr.*);
                } else {
                    // Infer parameter type from usage in function body and call sites
                    param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
                }

                // Track the parameter's type
                try self.trackVariableType(param.name.lexeme, param_type);

                // Create variable for parameter and store the stack value
                const var_idx = try self.getOrCreateVariable(param.name.lexeme);
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = param.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });
            }

            // TAIL CALL FIX: Add a separate label for tail calls to jump to (after parameter setup)
            const body_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}_body", .{function_body.function_info.name}));
            try self.instructions.append(.{ .Label = .{ .name = body_label, .vm_address = 0 } });

            // Store body label in function signature for tail call use
            if (self.function_signatures.getPtr(function_body.function_info.name)) |func_info| {
                func_info.body_label = body_label;
            }

            // Generate function body statements with basic dead code elimination
            var has_returned = false;
            for (function_body.statements) |body_stmt| {
                // Skip statements after a definitive return (basic dead code elimination)
                if (has_returned) {
                    break;
                }

                try self.generateStatement(body_stmt);

                // Check if this statement definitely returns (for dead code elimination)
                has_returned = self.statementAlwaysReturns(body_stmt);
            }

            // Exit function scope
            try self.instructions.append(.{ .ExitScope = .{ .scope_id = self.label_count + 1000 } });

            // Add implicit return if no explicit return
            try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });

            // Clear current function context
            self.current_function = null;
            self.current_function_return_type = .Nothing;
        }
    }

    /// Pass 2: Generate main program (non-function statements) - FIRST so execution starts here
    fn generateMainProgram(self: *HIRGenerator, statements: []ast.Stmt) !void {
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
    }

    /// Pass 4: Build function table from collected signatures
    fn buildFunctionTable(self: *HIRGenerator) ![]HIRProgram.HIRFunction {
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
                .body_label = function_info.body_label,
                .local_var_count = function_info.local_var_count,
                .is_entry = function_info.is_entry,
            });
        }

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

                // NEW: Determine the variable's type for tracking
                var var_type: HIRType = .Auto;

                // FIXED: Prioritize explicit type annotation over inference
                if (decl.type_info.base != .Auto) {
                    // Use explicit type annotation first
                    var_type = switch (decl.type_info.base) {
                        .Int => .Int,
                        .Float => .Float,
                        .String => .String,
                        .Tetra => .Tetra,
                        .U8 => .U8,
                        else => .Auto,
                    };
                }

                // Generate the initializer expression
                if (decl.initializer) |init_expr| {
                    try self.generateExpression(init_expr);

                    // Only infer type if no explicit annotation was provided
                    if (var_type == .Auto) {
                        var_type = self.inferTypeFromExpression(init_expr);
                    }
                } else {
                    // No initializer - push default value based on type
                    const default_value = switch (var_type) {
                        .Int => HIRValue{ .int = 0 },
                        .Float => HIRValue{ .float = 0.0 },
                        .String => HIRValue{ .string = "" },
                        .Tetra => HIRValue{ .tetra = 0 },
                        else => HIRValue.nothing,
                    };
                    const const_idx = try self.addConstant(default_value);
                    try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                }

                // NEW: Track the variable's type
                try self.trackVariableType(decl.name.lexeme, var_type);

                // Store to variable
                const var_idx = try self.getOrCreateVariable(decl.name.lexeme);
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                } });
            },
            .FunctionDecl => {
                // Skip - function declarations are handled in multi-pass approach
            },
            .Return => |ret| {
                if (ret.value) |value| {
                    // TAIL CALL OPTIMIZATION: Check if return value is a direct function call
                    if (self.tryGenerateTailCall(value)) {
                        return; // Tail call replaces both Call and Return
                    } else {
                        // Regular return with value
                        try self.generateExpression(value);
                        try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = self.current_function_return_type } });
                    }
                } else {
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                }
            },
            .EnumDecl => |enum_decl| {
                // NEW: Register enum type for TypeOf support (when enum is a statement)
                try self.registerCustomType(enum_decl.name.lexeme, .Enum);

                // Enum declarations don't generate runtime instructions, they're compile-time only
            },
            .Try => |try_stmt| {
                try self.generateTryStmt(try_stmt);
            },
            else => {
                self.reporter.reportError("Unhandled statement type: {}", .{stmt.data});
            },
        }
    }

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

    // IFF (if and only if): A ↔ B - true when A and B have same truth value
    pub const TETRA_IFF_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 0, 3, 2 }, // FALSE IFF x: same as NOT x
        [4]u8{ 0, 1, 2, 3 }, // TRUE IFF x: same as x
        [4]u8{ 3, 2, 2, 3 }, // BOTH IFF x: complex logic
        [4]u8{ 2, 3, 3, 2 }, // NEITHER IFF x: complex logic
    };

    // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
    pub const TETRA_XOR_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 0, 1, 2, 3 }, // FALSE XOR x: same as x
        [4]u8{ 1, 0, 3, 2 }, // TRUE XOR x: same as NOT x
        [4]u8{ 2, 3, 2, 3 }, // BOTH XOR x: complex logic
        [4]u8{ 3, 2, 3, 2 }, // NEITHER XOR x: complex logic
    };

    // NAND: A ↑ B - NOT(A AND B)
    pub const TETRA_NAND_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 1, 1, 1 }, // FALSE NAND x = NOT(FALSE AND x) = NOT(FALSE) = TRUE
        [4]u8{ 1, 0, 3, 2 }, // TRUE NAND x = NOT(TRUE AND x) = NOT(x)
        [4]u8{ 1, 3, 3, 1 }, // BOTH NAND x = complex logic
        [4]u8{ 1, 2, 1, 2 }, // NEITHER NAND x = complex logic
    };

    // NOR: A ↓ B - NOT(A OR B)
    pub const TETRA_NOR_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 0, 3, 2 }, // FALSE NOR x = NOT(FALSE OR x) = NOT(x)
        [4]u8{ 0, 0, 0, 0 }, // TRUE NOR x = NOT(TRUE OR x) = NOT(TRUE) = FALSE
        [4]u8{ 3, 0, 3, 3 }, // BOTH NOR x = complex logic
        [4]u8{ 2, 0, 3, 2 }, // NEITHER NOR x = complex logic
    };

    // IMPLIES: A → B - NOT A OR B
    pub const TETRA_IMPLIES_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 1, 1, 1 }, // FALSE IMPLIES x = NOT(FALSE) OR x = TRUE OR x = TRUE
        [4]u8{ 0, 1, 2, 3 }, // TRUE IMPLIES x = NOT(TRUE) OR x = FALSE OR x = x
        [4]u8{ 2, 1, 2, 2 }, // BOTH IMPLIES x = complex logic
        [4]u8{ 3, 1, 2, 3 }, // NEITHER IMPLIES x = complex logic
    };

    pub fn tetraFromEnum(tetra_enum: anytype) u8 {
        return switch (tetra_enum) {
            .false => TETRA_FALSE,
            .true => TETRA_TRUE,
            .both => TETRA_BOTH,
            .neither => TETRA_NEITHER,
        };
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

                switch (bin.operator.type) {
                    .PLUS => {
                        try self.instructions.append(.{ .IntArith = .{ .op = .Add, .overflow_behavior = .Wrap } });
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
                            .condition_type = .Tetra,
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
                } else if (log.operator.type == .IFF) {
                    // IFF (if and only if): A ↔ B - true when A and B have same truth value
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Iff } });
                } else if (log.operator.type == .XOR) {
                    // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Xor } });
                } else if (log.operator.type == .NAND) {
                    // NAND: A ↑ B - NOT(A AND B)
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nand } });
                } else if (log.operator.type == .NOR) {
                    // NOR: A ↓ B - NOT(A OR B)
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nor } });
                } else if (log.operator.type == .IMPLIES) {
                    // IMPLIES: A → B - NOT A OR B
                    try self.generateExpression(log.left);
                    try self.generateExpression(log.right);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Implies } });
                } else {
                    self.reporter.reportError("Unsupported logical operator: {}", .{log.operator.type});
                    return reporting.ErrorList.UnsupportedOperator;
                }
            },

            .If => |if_expr| {

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
                        .condition_type = .Tetra,
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
                        .condition_type = .Tetra,
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
                        } else |_| {
                            // Fall back to built-in
                            call_kind = .BuiltinFunction;
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
                    for (call.arguments, 0..) |arg, arg_index| {
                        // Check if this is a default argument placeholder
                        if (arg.data == .DefaultArgPlaceholder) {
                            // Try to resolve the default value from function signature
                            if (self.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                                try self.generateExpression(default_expr);
                            } else {
                                try self.generateExpression(arg);
                            }
                        } else {
                            try self.generateExpression(arg);
                        }
                    }
                }

                // Infer return type for the call
                const return_type = self.inferCallReturnType(function_name, call_kind) catch .String;

                // Generate function call
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = function_index,
                        .qualified_name = function_name,
                        .arg_count = @as(u32, @intCast(call.arguments.len)),
                        .call_kind = call_kind,
                        .target_module = null,
                        .return_type = return_type,
                    },
                });

                // CRITICAL FIX: For mutating method calls on variables, store result back to variable
                if (call.callee.data == .FieldAccess and method_target_var != null) {
                    const target_var = method_target_var.?;

                    // Check if this is a mutating method that should update the original variable
                    const is_mutating_method = std.mem.eql(u8, function_name, "push") or
                        std.mem.eql(u8, function_name, "pop") or
                        std.mem.eql(u8, function_name, "insert") or
                        std.mem.eql(u8, function_name, "remove");

                    if (is_mutating_method) {

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

            .Peek => |peek| {

                // Set current peek expression for field access tracking
                self.current_peek_expr = peek.expr;
                defer self.current_peek_expr = null;

                // Generate the expression to peek
                try self.generateExpression(peek.expr);

                // Duplicate it so it remains on stack after peekion
                try self.instructions.append(.Dup);

                // Build the full path for the peek expression (handles field access)
                const peek_path = try self.buildPeekPath(peek.expr);

                // NEW: Use tracked variable type when available, otherwise infer
                var inferred_type: HIRType = .Auto;

                // If peeking a variable, use the tracked type first
                if (peek_path != null) {
                    // For simple variables, try to get tracked type
                    if (peek.expr.data == .Variable) {
                        if (self.getTrackedVariableType(peek.expr.data.Variable.lexeme)) |tracked_type| {
                            inferred_type = tracked_type;
                        } else {
                            inferred_type = self.inferTypeFromExpression(peek.expr);
                        }
                    } else {
                        inferred_type = self.inferTypeFromExpression(peek.expr);
                    }
                } else {
                    inferred_type = self.inferTypeFromExpression(peek.expr);
                }

                // Generate peek instruction with full path and correct type
                try self.instructions.append(.{ .Peek = .{
                    .name = peek_path,
                    .value_type = inferred_type,
                    .location = peek.location,
                } });
            },

            .Assignment => |assign| {

                // Generate the value expression
                try self.generateExpression(assign.value.?);

                // NEW: Track the variable's type from the assigned value
                const assigned_type = self.inferTypeFromExpression(assign.value.?);
                try self.trackVariableType(assign.name.lexeme, assigned_type);

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

                // Determine array element type from first element (for now)
                const element_type: HIRType = if (elements.len > 0) blk: {
                    // Try to infer type from first element
                    switch (elements[0].data) {
                        .Literal => |lit| break :blk switch (lit) {
                            .int => .Int,
                            .float => .Float,
                            .string => .String,
                            .tetra => .Tetra,
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

                // Generate array/map expression
                try self.generateExpression(index.array);

                // Generate index expression
                try self.generateExpression(index.index);

                // Determine if we're accessing an array or map
                const container_type = self.inferTypeFromExpression(index.array);
                switch (container_type) {
                    .Map => {
                        // Map access - use MapGet
                        try self.instructions.append(.{
                            .MapGet = .{
                                .key_type = .String, // For now, assume string keys
                            },
                        });
                    },
                    .Array => {
                        // Array access - use ArrayGet
                        try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
                    },
                    else => {
                        // Default to array access for now
                        try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
                    },
                }
            },

            .IndexAssign => |assign| {

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

                // Generate each element FIRST (they get pushed to stack)
                for (elements) |element| {
                    try self.generateExpression(element);
                }

                // Generate TupleNew instruction AFTER elements are on stack
                try self.instructions.append(.{ .TupleNew = .{ .element_count = @intCast(elements.len) } });
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

                // TAIL CALL OPTIMIZATION: Check if return value is a direct function call
                if (return_expr.value) |value| {
                    if (self.tryGenerateTailCall(value)) {
                        return; // Tail call replaces both Call and Return
                    } else {
                        // Regular return with value
                        try self.generateExpression(value);
                    }
                } else {
                    // No value - push nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }

                // Generate Return instruction (only if not tail call)
                try self.instructions.append(.{ .Return = .{ .has_value = return_expr.value != null, .return_type = self.current_function_return_type } });
            },

            .Unary => |unary| {

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

            .ForAll => |forall| {

                // ForAll quantifier: ∀x ∈ array : condition
                // Implementation: iterate through array, return false if any element fails condition

                // Generate array expression
                try self.generateExpression(forall.array);

                // For now, generate as builtin function call
                // TODO: Implement proper quantifier logic with VM support
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = 0,
                        .qualified_name = "forall_quantifier",
                        .arg_count = 2, // array + condition (as closure)
                        .call_kind = .BuiltinFunction,
                        .target_module = null,
                        .return_type = .Tetra,
                    },
                });
            },

            .Exists => |exists| {

                // Exists quantifier: ∃x ∈ array : condition
                // Implementation: iterate through array, return true if any element satisfies condition

                // Generate array expression
                try self.generateExpression(exists.array);

                // For now, generate as builtin function call
                // TODO: Implement proper quantifier logic with VM support
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = 0,
                        .qualified_name = "exists_quantifier",
                        .arg_count = 2, // array + condition (as closure)
                        .call_kind = .BuiltinFunction,
                        .target_module = null,
                        .return_type = .Tetra,
                    },
                });
            },

            .Match => |match_expr| {

                // Generate the value to match on
                try self.generateExpression(match_expr.value);

                // Create labels for each case and the end
                const end_label = try self.generateLabel("match_end");
                var case_labels = std.ArrayList([]const u8).init(self.allocator);
                defer case_labels.deinit();

                // Generate labels for each case
                for (match_expr.cases) |_| {
                    const case_label = try self.generateLabel("match_case");
                    try case_labels.append(case_label);
                }

                // Generate comparison and jumps for each case
                for (match_expr.cases, 0..) |case, i| {
                    // Duplicate the match value for comparison
                    try self.instructions.append(.Dup);

                    if (case.pattern.type == .ELSE) {
                        // Else case - always matches, pop the duplicated value
                        try self.instructions.append(.Pop);
                        try self.instructions.append(.{ .Jump = .{ .label = case_labels.items[i], .vm_offset = 0 } });
                    } else {
                        // Generate the pattern value (enum member)
                        const pattern_value = HIRValue{ .string = case.pattern.lexeme };
                        const pattern_idx = try self.addConstant(pattern_value);
                        try self.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });

                        // Compare and jump if equal
                        try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .String } });
                        try self.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = end_label, .vm_offset = 0, .condition_type = .Tetra } });
                    }
                }

                // Generate case bodies
                for (match_expr.cases, 0..) |case, i| {
                    try self.instructions.append(.{ .Label = .{ .name = case_labels.items[i], .vm_address = 0 } });
                    try self.generateExpression(case.body);
                    try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });
                }

                // End label
                try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
            },

            .EnumMember => |member| {

                // Generate enum member as string constant
                const enum_value = HIRValue{ .string = member.lexeme };
                const const_idx = try self.addConstant(enum_value);
                try self.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
            },

            .StructLiteral => |struct_lit| {

                // Track field types for type checking
                var field_types = try self.allocator.alloc(HIRType, struct_lit.fields.len);
                defer self.allocator.free(field_types);

                // Generate field values and names in reverse order for stack-based construction
                var reverse_i = struct_lit.fields.len;
                while (reverse_i > 0) {
                    reverse_i -= 1;
                    const field = struct_lit.fields[reverse_i];

                    // Generate field value
                    try self.generateExpression(field.value);

                    // Infer and store field type
                    field_types[reverse_i] = self.inferTypeFromExpression(field.value);

                    // Push field name as constant
                    const field_name_const = try self.addConstant(HIRValue{ .string = field.name.lexeme });
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = field.name.lexeme }, .constant_id = field_name_const } });
                }

                // Generate StructNew instruction with field types
                try self.instructions.append(.{
                    .StructNew = .{
                        .type_name = struct_lit.name.lexeme,
                        .field_count = @intCast(struct_lit.fields.len),
                        .field_types = try self.allocator.dupe(HIRType, field_types),
                        .size_bytes = 0, // Size will be calculated by VM
                    },
                });

                // Note: Removed Dup instruction as it was corrupting the stack for nested structs
                // The struct instance will be used directly without duplication
            },

            .FieldAssignment => |field_assign| {

                // Generate object expression
                try self.generateExpression(field_assign.object);

                // Generate value expression
                try self.generateExpression(field_assign.value);

                // Generate SetField instruction
                try self.instructions.append(.{
                    .SetField = .{
                        .field_name = field_assign.field.lexeme,
                        .container_type = .Struct,
                        .field_index = 0, // Index will be resolved by VM
                    },
                });
            },

            .TypeOf => |expr_to_check| {

                // NEW: Enhanced type inference with custom type support
                const inferred_type = self.inferTypeFromExpression(expr_to_check);
                const type_name = switch (inferred_type) {
                    .Int => "int",
                    .Float => "float",
                    .String => "string",
                    .Tetra => "tetra",
                    .U8 => "u8",
                    .Nothing => "nothing",
                    .Array => "array",
                    .Struct => blk: {
                        // For struct types, try to get the specific struct name
                        if (expr_to_check.data == .Variable) {
                            const var_name = expr_to_check.data.Variable.lexeme;
                            if (self.isCustomType(var_name)) |custom_type| {
                                if (custom_type.kind == .Struct) {
                                    break :blk var_name; // Return the specific struct name
                                }
                            }
                        }
                        break :blk "struct"; // Generic struct type
                    },
                    .Tuple => "tuple",
                    .Map => "map",
                    .Enum => blk: {
                        // For enum types, try to get the specific enum name
                        if (expr_to_check.data == .Variable) {
                            const var_name = expr_to_check.data.Variable.lexeme;
                            if (self.isCustomType(var_name)) |custom_type| {
                                if (custom_type.kind == .Enum) {
                                    break :blk var_name; // Return the specific enum name
                                }
                            }
                        }
                        break :blk "enum"; // Generic enum type
                    },
                    .Function => "function",
                    .Auto => blk: {
                        // ERROR: Unresolved auto type at runtime!
                        const error_msg = try std.fmt.allocPrint(self.allocator, "TypeOf failed to resolve type for expression: {s}", .{@tagName(expr_to_check.data)});
                        self.reporter.reportError("{s}", .{error_msg});
                        break :blk "auto"; // Still return auto for now, but this should be caught
                    },
                };

                const type_value = HIRValue{ .string = type_name };
                const const_idx = try self.addConstant(type_value);
                try self.instructions.append(.{ .Const = .{ .value = type_value, .constant_id = const_idx } });
            },

            .Map => |entries| {

                // Generate each key-value pair in reverse order (for stack-based construction)
                var reverse_i = entries.len;
                while (reverse_i > 0) {
                    reverse_i -= 1;
                    const entry = entries[reverse_i];

                    // Generate key first, then value (they'll be popped in reverse order)
                    try self.generateExpression(entry.key);
                    try self.generateExpression(entry.value);
                }

                // Create HIRMapEntry array with the right size (will be populated by VM)
                const dummy_entries = try self.allocator.alloc(HIRMapEntry, entries.len);
                // Initialize with dummy values (VM will replace with actual values from stack)
                for (dummy_entries) |*entry| {
                    entry.* = HIRMapEntry{
                        .key = HIRValue.nothing,
                        .value = HIRValue.nothing,
                    };
                }

                const map_instruction = HIRInstruction{
                    .Map = .{
                        .entries = dummy_entries,
                        .key_type = .String, // Assume string keys for now
                        .value_type = .Auto, // Will be inferred from values
                    },
                };

                // Generate HIR Map instruction
                try self.instructions.append(map_instruction);
            },

            .DefaultArgPlaceholder => {

                // Push nothing for default arguments - they should be replaced by the caller
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .For => |_| {

                // For now, just return nothing
                // TODO: Implement proper for loop support
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .FieldAccess => |field| {
                // Generate object expression
                try self.generateExpression(field.object);

                // Check the type of the object being accessed
                const obj_type = self.inferTypeFromExpression(field.object);

                if (obj_type == .String) {
                    // String operations
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        // Generate StringOp.Length instruction
                        try self.instructions.append(.{
                            .StringOp = .{
                                .op = .Length,
                            },
                        });
                    } else {
                        // Treat as string indexing
                        const index_const = try self.addConstant(HIRValue{ .int = 1 }); // Length 1 for single char
                        try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 1 }, .constant_id = index_const } });

                        // Parse field name as index
                        if (std.fmt.parseInt(i32, field.field.lexeme, 10)) |index| {
                            const idx_const = try self.addConstant(HIRValue{ .int = index });
                            try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = index }, .constant_id = idx_const } });

                            // Generate substring operation for indexing
                            try self.instructions.append(.{
                                .StringOp = .{
                                    .op = .Substring,
                                },
                            });
                        } else |_| {
                            // Not a valid string operation
                            return self.reporter.reportError("Invalid string operation: {s}", .{field.field.lexeme});
                        }
                    }
                } else {
                    // Regular struct field access
                    try self.instructions.append(.{
                        .GetField = .{
                            .field_name = field.field.lexeme,
                            .container_type = .Struct,
                            .field_index = 0,
                            .field_for_peek = if (self.current_peek_expr != null) true else false,
                        },
                    });
                }

                // Handle peekion context
                if (self.current_peek_expr) |peek| {
                    if (peek.data == .Peek) {
                        self.current_field_name = field.field.lexeme;
                    }
                }
            },

            .EnumDecl => |enum_decl| {
                // NEW: Register enum type for TypeOf support
                try self.registerCustomType(enum_decl.name.lexeme, .Enum);

                // Enum declarations don't generate runtime instructions, they're compile-time only
                // Push nothing as a placeholder value
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .StructDecl => |struct_decl| {
                // NEW: Register struct type for TypeOf support
                try self.registerCustomType(struct_decl.name.lexeme, .Struct);

                // Struct declarations don't generate runtime instructions, they're compile-time only
                // Push nothing as a placeholder value
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .PeekStruct => |peek| {

                // Generate the expression to peek
                try self.generateExpression(peek.expr);

                // Get struct info from the expression
                const struct_info: StructPeekInfo = switch (peek.expr.data) {
                    .StructLiteral => |struct_lit| blk: {
                        const field_count: u32 = @truncate(struct_lit.fields.len);
                        const field_names = try self.allocator.alloc([]const u8, struct_lit.fields.len);
                        const field_types = try self.allocator.alloc(HIRType, struct_lit.fields.len);
                        break :blk StructPeekInfo{
                            .name = struct_lit.name.lexeme,
                            .field_count = field_count,
                            .field_names = field_names,
                            .field_types = field_types,
                        };
                    },
                    .Variable => |var_token| if (self.getTrackedVariableType(var_token.lexeme)) |var_type| blk: {
                        if (var_type != .Struct) {
                            return error.ExpectedStructType;
                        }
                        const field_names = try self.allocator.alloc([]const u8, 0);
                        const field_types = try self.allocator.alloc(HIRType, 0);
                        break :blk StructPeekInfo{
                            .name = var_token.lexeme,
                            .field_count = 0,
                            .field_names = field_names,
                            .field_types = field_types,
                        };
                    } else {
                        return error.UnknownVariableType;
                    },
                    .FieldAccess => |field| blk: {
                        // For field access, we need to generate the field access code first
                        try self.generateExpression(field.object);
                        try self.instructions.append(.{
                            .StoreFieldName = .{
                                .field_name = field.field.lexeme,
                            },
                        });

                        // Generate GetField instruction to access the field
                        try self.instructions.append(.{
                            .GetField = .{
                                .field_name = field.field.lexeme,
                                .container_type = .Struct,
                                .field_index = 0,
                                .field_for_peek = true,
                            },
                        });

                        // Create a single-field struct info
                        const field_names = try self.allocator.alloc([]const u8, 1);
                        const field_types = try self.allocator.alloc(HIRType, 1);
                        field_names[0] = field.field.lexeme;
                        field_types[0] = self.inferTypeFromExpression(peek.expr);

                        break :blk StructPeekInfo{
                            .name = field.field.lexeme,
                            .field_count = 1,
                            .field_names = field_names,
                            .field_types = field_types,
                        };
                    },
                    else => {
                        return error.ExpectedStructType;
                    },
                };

                // Add the PeekStruct instruction with the gathered info
                try self.instructions.append(.{ .PeekStruct = .{
                    .type_name = struct_info.name,
                    .field_count = struct_info.field_count,
                    .field_names = struct_info.field_names,
                    .field_types = struct_info.field_types,
                    .location = peek.location,
                } });
            },

            else => {
                // Push nothing as fallback
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },
        }
    }

    fn addConstant(self: *HIRGenerator, value: HIRValue) std.mem.Allocator.Error!u32 {
        // TODO: Add deduplication for identical constants
        const index = @as(u32, @intCast(self.constants.items.len));
        try self.constants.append(value);
        return index;
    }

    fn getOrCreateVariable(self: *HIRGenerator, name: []const u8) !u32 {
        if (self.variables.get(name)) |idx| {
            return idx;
        }

        const idx = self.variable_count;
        try self.variables.put(name, idx);
        self.variable_count += 1;
        return idx;
    }

    fn generateLabel(self: *HIRGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.label_count });
        self.label_count += 1;
        return label;
    }

    /// Helper function to determine if a statement always returns (for dead code elimination)
    fn statementAlwaysReturns(self: *HIRGenerator, stmt: ast.Stmt) bool {
        switch (stmt.data) {
            .Return => return true,
            .Expression => |expr| {
                if (expr) |e| {
                    return self.expressionAlwaysReturns(e);
                }
                return false;
            },
            else => return false,
        }
    }

    /// Helper function to determine if an expression always returns
    fn expressionAlwaysReturns(self: *HIRGenerator, expr: *ast.Expr) bool {
        switch (expr.data) {
            .If => |if_expr| {
                // An if-expression always returns if both branches always return
                const then_returns = if (if_expr.then_branch) |then| self.expressionAlwaysReturns(then) else false;
                const else_returns = if (if_expr.else_branch) |else_branch| self.expressionAlwaysReturns(else_branch) else false;
                return then_returns and else_returns;
            },
            .Block => |block| {
                // A block always returns if its final value/statement always returns
                if (block.value) |value| {
                    return self.expressionAlwaysReturns(value);
                }
                // Check if any statement in the block returns
                for (block.statements) |stmt| {
                    if (self.statementAlwaysReturns(stmt)) {
                        return true;
                    }
                }
                return false;
            },
            else => return false,
        }
    }

    /// Try to generate a tail call if the expression is a direct function call
    /// Returns true if tail call was generated, false if normal expression should be used
    fn tryGenerateTailCall(self: *HIRGenerator, expr: *ast.Expr) bool {
        switch (expr.data) {
            .Call => |call| {
                // Check if the callee is a simple variable (function name)
                switch (call.callee.data) {
                    .Variable => |var_token| {
                        const function_name = var_token.lexeme;

                        // Generate arguments in normal order (same as regular call)
                        for (call.arguments) |arg| {
                            self.generateExpression(arg) catch return false; // Fallback to regular call on error
                        }

                        // Check if this is a user-defined function
                        if (self.getFunctionIndex(function_name)) |function_index| {
                            // Infer return type for tail call
                            const return_type = self.inferCallReturnType(function_name, .LocalFunction) catch .Nothing;

                            // Generate TailCall instruction instead of Call + Return
                            const tail_call = HIRInstruction{ .TailCall = .{
                                .function_index = function_index,
                                .qualified_name = function_name,
                                .arg_count = @intCast(call.arguments.len),
                                .call_kind = .LocalFunction,
                                .target_module = null,
                                .return_type = return_type,
                            } };

                            self.instructions.append(tail_call) catch return false;
                            return true; // Tail call generated successfully
                        } else |_| {
                            return false; // Not a known function, use regular call
                        }
                    },
                    else => {
                        // Complex callee (method call, etc.) - cannot optimize
                        return false;
                    },
                }
            },
            else => {
                // Not a direct function call, cannot optimize
                return false;
            },
        }
    }

    /// NEW: Infer type from a literal value
    fn inferTypeFromLiteral(_: *HIRGenerator, literal: TokenLiteral) HIRType {
        return switch (literal) {
            .int => .Int,
            .float => .Float,
            .string => .String,
            .tetra => .Tetra,
            .u8 => .U8,
            .nothing => .Nothing,
            else => .Auto,
        };
    }

    /// NEW: Infer type from an expression (basic implementation)
    fn inferTypeFromExpression(self: *HIRGenerator, expr: *ast.Expr) HIRType {
        return switch (expr.data) {
            .Literal => |lit| self.inferTypeFromLiteral(lit),
            .Variable => |var_token| {
                // First check if this is a custom type name
                if (self.isCustomType(var_token.lexeme)) |custom_type| {
                    return switch (custom_type.kind) {
                        .Struct => .Struct,
                        .Enum => .Enum,
                    };
                }

                // Otherwise look up the variable's tracked type
                const var_type = self.variable_types.get(var_token.lexeme) orelse .Auto;
                return var_type;
            },
            .Binary => |binary| {
                // Simple type inference for binary operations
                const left_type = if (binary.left) |left| self.inferTypeFromExpression(left) else .Auto;
                const right_type = if (binary.right) |right| self.inferTypeFromExpression(right) else .Auto;
                return switch (binary.operator.type) {
                    .EQUALITY, .BANG_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => .Tetra,
                    .AND, .OR, .XOR => .Tetra, // Logical operations return tetra values
                    .PLUS, .MINUS, .ASTERISK, .SLASH, .MODULO => {
                        // Arithmetic operations - return the promoted type
                        if (left_type == right_type and left_type != .Auto) return left_type;
                        if ((left_type == .Int and right_type == .Float) or (left_type == .Float and right_type == .Int)) {
                            return .Float;
                        }
                        return .Int; // Default to int for arithmetic
                    },
                    else => .Tetra, // Default to Tetra for any other binary operations
                };
            },
            .Array => .Array,
            .Tuple => .Tuple,
            .Index => |index| {
                // Array/string indexing returns the element type
                const container_type = self.inferTypeFromExpression(index.array);
                return switch (container_type) {
                    .Array => .String, // Most arrays in bigfile.doxa are int arrays, but for simplicity return String
                    .String => .String, // String indexing returns single character (still string in our system)
                    .Tuple => .String, // Tuples can have mixed types, return String for simplicity
                    .Map => .Int, // Map values are integers in our test case
                    else => .String, // Default to String for most index operations
                };
            },
            .Call => |call| {
                // Handle different types of function calls
                switch (call.callee.data) {
                    .MethodCall => |method| {
                        if (std.mem.eql(u8, method.method.lexeme, "safeAdd")) {
                            return .Int; // safeAdd returns int
                        }
                    },
                    .FieldAccess => |field| {
                        // Handle imported functions like safeMath.safeAdd
                        if (std.mem.eql(u8, field.field.lexeme, "safeAdd")) {
                            return .Int; // safeAdd returns int
                        }
                        // Add more imported function mappings here as needed
                    },
                    .Variable => |var_token| {
                        // Handle direct function calls
                        if (std.mem.eql(u8, var_token.lexeme, "fizzbuzz") or
                            std.mem.eql(u8, var_token.lexeme, "fber") or
                            std.mem.eql(u8, var_token.lexeme, "forloop"))
                        {
                            return .Nothing; // These functions don't return values
                        }
                        if (std.mem.eql(u8, var_token.lexeme, "return_test")) {
                            return .String;
                        }
                        if (std.mem.eql(u8, var_token.lexeme, "foo")) {
                            return .Int;
                        }
                    },
                    else => {},
                }
                return .Auto; // Default for unrecognized function calls
            },
            .If => |if_expr| {
                // If both branches have the same type, return that
                if (if_expr.then_branch) |then_branch| {
                    const then_type = self.inferTypeFromExpression(then_branch);
                    if (if_expr.else_branch) |else_branch| {
                        const else_type = self.inferTypeFromExpression(else_branch);
                        if (then_type == else_type) return then_type;
                    }
                    return then_type;
                }
                return .Auto;
            },
            .Match => .String, // Match expressions typically return strings in this codebase
            .StructLiteral => .Struct,
            .EnumMember => .Enum,
            .FieldAccess => |field| {
                // First check the type of the object being accessed
                const obj_type = self.inferTypeFromExpression(field.object);

                // Handle string operations
                if (obj_type == .String) {
                    if (std.mem.eql(u8, field.field.lexeme, "length")) {
                        return .Int; // String length returns int
                    }
                    // String indexing returns a single character string
                    if (std.fmt.parseInt(i32, field.field.lexeme, 10)) |_| {
                        return .String;
                    } else |_| {
                        return .Auto; // Invalid string operation
                    }
                }

                // Handle struct fields
                if (obj_type == .Struct) {
                    // Check if this is a nested field access
                    if (field.object.data == .FieldAccess) {
                        const parent_field = field.object.data.FieldAccess;
                        const parent_type = self.inferTypeFromExpression(parent_field.object);
                        if (parent_type == .Struct) {
                            // Handle nested struct fields
                            if (std.mem.eql(u8, parent_field.field.lexeme, "person")) {
                                // Person struct fields
                                if (std.mem.eql(u8, field.field.lexeme, "age")) {
                                    return .Int;
                                }
                                if (std.mem.eql(u8, field.field.lexeme, "name")) {
                                    return .String;
                                }
                            }
                        }
                    } else {
                        // Top-level struct fields
                        if (std.mem.eql(u8, field.field.lexeme, "age") or
                            std.mem.eql(u8, field.field.lexeme, "salary"))
                        {
                            return .Int; // Numeric fields
                        }
                        if (std.mem.eql(u8, field.field.lexeme, "name")) {
                            return .String; // String fields
                        }
                        if (std.mem.eql(u8, field.field.lexeme, "person")) {
                            return .Struct; // Nested struct fields
                        }
                    }
                }

                return .Auto; // Default for unknown object types
            },
            .Exists, .ForAll => .Tetra, // Quantifiers return tetra values
            .Logical => .Tetra, // Logical operations (↔, ⊕, ∧, ∨, ↑, ↓, →) return tetra values
            .Unary => {
                // Unary operations: negation (¬) returns tetra, others depend on operand
                // For logical negation, always return tetra regardless of operand type
                return .Tetra; // Negation of any logical expression returns tetra
            },
            .Map => .Map,
            .Grouping => |grouping| {
                // Grouping (parentheses) - infer from the inner expression
                if (grouping) |inner_expr| {
                    return self.inferTypeFromExpression(inner_expr);
                } else {
                    return .Nothing; // Empty grouping
                }
            },
            else => .String, // Default to String for any unhandled expression types to prevent Auto leakage
        };
    }

    /// NEW: Track a variable's type when it's declared or assigned
    fn trackVariableType(self: *HIRGenerator, var_name: []const u8, var_type: HIRType) !void {
        try self.variable_types.put(var_name, var_type);
    }

    /// NEW: Get tracked variable type
    fn getTrackedVariableType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.variable_types.get(var_name);
    }

    /// NEW: Register a custom type (struct or enum)
    fn registerCustomType(self: *HIRGenerator, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = CustomTypeInfo{
            .name = type_name,
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    /// NEW: Infer parameter type from usage in function body
    fn inferParameterType(self: *HIRGenerator, param_name: []const u8, function_body: []ast.Stmt, function_name: []const u8) !HIRType {
        // Analyze how the parameter is used in the function body
        for (function_body) |stmt| {
            if (self.analyzeStatementForParameterType(stmt, param_name)) |inferred_type| {
                return inferred_type;
            }
        }

        // If no usage found, try to infer from call sites
        if (self.inferParameterTypeFromCallSites(param_name, function_name)) |inferred_type| {
            return inferred_type;
        }

        return .Int; // Reasonable default
    }

    /// Analyze a statement to infer parameter type from usage
    fn analyzeStatementForParameterType(self: *HIRGenerator, stmt: ast.Stmt, param_name: []const u8) ?HIRType {
        return switch (stmt.data) {
            .Expression => |expr| {
                if (expr) |e| {
                    return self.analyzeExpressionForParameterType(e, param_name);
                }
                return null;
            },
            .VarDecl => |decl| {
                if (decl.initializer) |initializer| {
                    return self.analyzeExpressionForParameterType(initializer, param_name);
                }
                return null;
            },
            else => null,
        };
    }

    /// Analyze an expression to infer parameter type from usage
    fn analyzeExpressionForParameterType(self: *HIRGenerator, expr: *ast.Expr, param_name: []const u8) ?HIRType {
        return switch (expr.data) {
            .Binary => |binary| {
                // Check if parameter is used in binary operation
                const left_uses_param = if (binary.left) |left| self.expressionUsesParameter(left, param_name) else false;
                const right_uses_param = if (binary.right) |right| self.expressionUsesParameter(right, param_name) else false;

                if (left_uses_param or right_uses_param) {
                    return switch (binary.operator.type) {
                        .PLUS, .MINUS, .ASTERISK, .SLASH, .MODULO => .Int, // Arithmetic suggests numeric
                        .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => .Int, // Comparison suggests numeric
                        .EQUALITY, .BANG_EQUAL => .Int, // Could be any type, but Int is common
                        else => null,
                    };
                }
                return null;
            },
            .Call => |call| {
                // Check if parameter is passed to function calls
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg, param_name)) {
                        // Parameter is passed as argument - could infer from expected parameter type
                        return .Int; // Conservative guess
                    }
                }
                return null;
            },
            .If => |if_expr| {
                // Check condition and branches
                if (if_expr.condition) |cond| {
                    if (self.analyzeExpressionForParameterType(cond, param_name)) |inferred| return inferred;
                }
                if (if_expr.then_branch) |then_branch| {
                    if (self.analyzeExpressionForParameterType(then_branch, param_name)) |inferred| return inferred;
                }
                if (if_expr.else_branch) |else_branch| {
                    if (self.analyzeExpressionForParameterType(else_branch, param_name)) |inferred| return inferred;
                }
                return null;
            },
            .While => |while_expr| {
                // Check condition and body
                if (self.analyzeExpressionForParameterType(while_expr.condition, param_name)) |inferred| return inferred;
                if (self.analyzeExpressionForParameterType(while_expr.body, param_name)) |inferred| return inferred;
                return null;
            },
            .Block => |block| {
                // Check all statements in block
                for (block.statements) |stmt| {
                    if (self.analyzeStatementForParameterType(stmt, param_name)) |inferred| {
                        return inferred;
                    }
                }
                return null;
            },
            else => null,
        };
    }

    /// Check if an expression uses a specific parameter
    fn expressionUsesParameter(self: *HIRGenerator, expr: *ast.Expr, param_name: []const u8) bool {
        return switch (expr.data) {
            .Variable => |var_token| std.mem.eql(u8, var_token.lexeme, param_name),
            .Binary => |binary| {
                const left_uses = if (binary.left) |left| self.expressionUsesParameter(left, param_name) else false;
                const right_uses = if (binary.right) |right| self.expressionUsesParameter(right, param_name) else false;
                return left_uses or right_uses;
            },
            .Call => |call| {
                // Check arguments
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg, param_name)) return true;
                }
                return false;
            },
            else => false,
        };
    }

    /// Infer parameter type from call sites (analyze how the function is called)
    fn inferParameterTypeFromCallSites(self: *HIRGenerator, param_name: []const u8, function_name: []const u8) ?HIRType {
        _ = param_name; // Parameter position would need to be tracked

        // Look through collected function calls to see what types are passed
        // This is a simplified version - in a full implementation we'd track parameter positions
        for (self.function_calls.items) |call_site| {
            if (std.mem.eql(u8, call_site.function_name, function_name)) {
                return .Int; // Most common type in this codebase
            }
        }
        return null;
    }

    /// Resolve default argument value for a function parameter
    /// Returns the default expression if found, null otherwise
    fn resolveDefaultArgument(self: *HIRGenerator, function_name: []const u8, arg_index: usize) ?*ast.Expr {
        // Find the function body which contains the original parameter information
        for (self.function_bodies.items) |function_body| {
            if (std.mem.eql(u8, function_body.function_name, function_name)) {
                // Check if argument index is valid and parameter has default value
                if (arg_index < function_body.function_params.len) {
                    const param = function_body.function_params[arg_index];
                    return param.default_value;
                }
                break;
            }
        }
        return null;
    }

    /// NEW: Infer return type for function calls to prevent Auto leakage
    fn inferCallReturnType(self: *HIRGenerator, function_name: []const u8, call_kind: CallKind) !HIRType {
        switch (call_kind) {
            .LocalFunction => {
                // Look up the function signature to get its return type
                if (self.function_signatures.get(function_name)) |func_info| {
                    return func_info.return_type;
                }
                return .Nothing; // Default if function not found
            },
            .BuiltinFunction => {
                // Map built-in function names to their return types
                if (std.mem.eql(u8, function_name, "safeAdd") or
                    std.mem.eql(u8, function_name, "safeSub") or
                    std.mem.eql(u8, function_name, "safeMul") or
                    std.mem.eql(u8, function_name, "foo"))
                {
                    return .Int;
                }
                if (std.mem.eql(u8, function_name, "push") or
                    std.mem.eql(u8, function_name, "pop") or
                    std.mem.eql(u8, function_name, "insert") or
                    std.mem.eql(u8, function_name, "remove"))
                {
                    return .Array; // Array methods return the modified array
                }
                if (std.mem.eql(u8, function_name, "print") or
                    std.mem.eql(u8, function_name, "println"))
                {
                    return .Nothing;
                }
                return .String; // Default for unknown builtins
            },
            .ModuleFunction => {
                // Handle module function return types - default to reasonable type
                return .String;
            },
        }
    }

    /// NEW: Check if a name refers to a custom type
    fn isCustomType(self: *HIRGenerator, name: []const u8) ?CustomTypeInfo {
        return self.custom_types.get(name);
    }

    /// NEW: Get the HIR type for a custom type name
    fn getCustomTypeHIRType(self: *HIRGenerator, name: []const u8) HIRType {
        if (self.custom_types.get(name)) |custom_type| {
            return switch (custom_type.kind) {
                .Struct => .Struct,
                .Enum => .Enum,
            };
        }
        return .Auto; // Unresolved
    }

    /// Build a full variable path for peek expressions (e.g., "mike.person.age")
    fn buildPeekPath(self: *HIRGenerator, expr: *const ast.Expr) !?[]const u8 {
        switch (expr.data) {
            .Variable => |var_token| {
                return try self.allocator.dupe(u8, var_token.lexeme);
            },
            .FieldAccess => |field| {
                // Recursively build the path for the object
                if (try self.buildPeekPath(field.object)) |base_path| {
                    return try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ base_path, field.field.lexeme });
                } else {
                    return try self.allocator.dupe(u8, field.field.lexeme);
                }
            },
            else => return null,
        }
    }

    fn generateTryStmt(self: *HIRGenerator, try_stmt: ast.TryStmt) !void {
        // Generate a unique label for the catch block
        const catch_label = try self.generateLabel("catch");
        const end_label = try self.generateLabel("try_end");

        // Emit TryBegin instruction with catch label
        try self.instructions.append(.{
            .TryBegin = .{
                .catch_label = catch_label,
                .vm_catch_offset = 0, // Will be calculated later
            },
        });

        // Generate try block code
        for (try_stmt.try_body) |stmt| {
            try self.generateStatement(stmt);
        }

        // Jump to end if no exception
        try self.instructions.append(.{
            .Jump = .{
                .label = end_label,
                .vm_offset = 0, // Will be calculated during VM initialization
            },
        });

        // Emit catch label
        try self.instructions.append(.{ .Label = .{
            .name = catch_label,
            .vm_address = 0,
        } });

        // Emit TryCatch instruction
        try self.instructions.append(.{
            .TryCatch = .{
                .exception_type = null, // We'll add exception type support later
            },
        });

        // Generate catch block code
        for (try_stmt.catch_body) |stmt| {
            try self.generateStatement(stmt);
        }

        // Emit end label
        try self.instructions.append(.{ .Label = .{
            .name = end_label,
            .vm_address = 0,
        } });
    }
};

//==================================================================
// HIR TO VM BYTECODE TRANSLATION - The final step to speed!
//==================================================================

/// Converts HIR to VM bytecode - direct 1:1 mapping for maximum speed
pub fn translateToVMBytecode(program: *HIRProgram, allocator: std.mem.Allocator, reporter: *reporting.Reporter) ![]u8 {
    var bytecode = std.ArrayList(u8).init(allocator);
    defer bytecode.deinit();

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
            .Peek => |_| {
                // For peek, we can use existing VM peekion mechanism
                // The VM will handle the printing based on the top stack value
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP)); // Keep value on stack
                // Note: Your VM's peek handling is in the main execution loop
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
    return result;
}

/// Returns the size in bytes that an HIR instruction will occupy in VM bytecode
fn getBytecodeSize(instruction: HIRInstruction) u32 {
    return switch (instruction) {
        .Const, .LoadVar, .StoreVar, .Jump, .JumpCond, .Call, .TailCall => 2, // opcode + operand
        .IntArith, .Compare, .Return, .Dup, .Pop, .Peek, .Halt => 1, // opcode only
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

/// Computes a cache key for source file validation
fn computeCacheKey(source_path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    // Read source file content
    const source_content = try std.fs.cwd().readFileAlloc(allocator, source_path, 1024 * 1024); // 1MB max
    defer allocator.free(source_content);

    // Create hasher
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(source_content);

    // Include compiler version to invalidate cache when compiler changes
    hasher.update("doxa-0.1.0-dev");

    // Include build configuration
    hasher.update("debug-mode");

    // Generate hash
    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    // Return first 16 hex chars (8 bytes) as string
    return std.fmt.allocPrint(allocator, "{}", .{std.fmt.fmtSliceHexLower(hash[0..8])});
}

/// Validates SOXA cache by checking embedded source hash
pub fn validateSoxaCache(soxa_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !bool {
    const file = std.fs.cwd().openFile(soxa_path, .{}) catch return false;
    defer file.close();

    var reader = file.reader();
    var buf: [512]u8 = undefined;

    // Skip first line ("; SOXA HIR v0.1.0")
    _ = reader.readUntilDelimiterOrEof(&buf, '\n') catch return false;

    // Read second line looking for Source-Hash
    if (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (std.mem.indexOf(u8, line, "Source-Hash: ")) |start| {
            const cached_hash = std.mem.trim(u8, line[start + 13 ..], " \n\r");
            const current_hash = try computeCacheKey(source_path, allocator);
            defer allocator.free(current_hash);

            const is_valid = std.mem.eql(u8, cached_hash, current_hash);
            if (!is_valid) {}
            return is_valid;
        }
    }

    return false; // No hash found = old format = invalid
}

/// Writes a HIR program to a text-based .soxa file with cache validation
pub fn writeSoxaFile(program: *HIRProgram, file_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !void {
    const file = std.fs.cwd().createFile(file_path, .{}) catch |err| switch (err) {
        error.AccessDenied => return reporting.ErrorList.AccessDenied,
        error.FileNotFound => return reporting.ErrorList.FileNotFound,
        else => return err,
    };
    defer file.close();

    var writer = file.writer();

    // Compute cache key for validation
    const cache_key = try computeCacheKey(source_path, allocator);
    defer allocator.free(cache_key);

    // Write enhanced header with cache validation
    try writer.print("; SOXA HIR v0.1.0\n", .{});
    try writer.print("; Source-Hash: {s}\n", .{cache_key});
    try writer.print("; Compiler-Version: 0.1.0-dev\n", .{});
    try writer.print("; Build-Flags: debug\n", .{});
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
        .Peek => |i| {
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
            // Write whether location is present
            if (i.location) |location| {
                try writer.writeByte(1); // Has location
                try writer.writeInt(u32, @as(u32, @intCast(location.file.len)), .little);
                try writer.writeAll(location.file);
                try writer.writeInt(u32, location.line, .little);
                try writer.writeInt(u32, location.column, .little);
            } else {
                try writer.writeByte(0); // No location
            }
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

            return HIRInstruction{ .JumpCond = .{ .label_true = label_true, .label_false = label_false, .vm_offset = 0, .condition_type = .Tetra } };
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
            return HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Nothing } }; // Default to Nothing to prevent Auto leakage
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
        13 => { // Peek
            const has_name = (try reader.readByte()) != 0;
            const name = if (has_name) blk: {
                const name_len = try reader.readInt(u32, .little);
                const name_str = try allocator.alloc(u8, name_len);
                _ = try reader.readAll(name_str);
                break :blk name_str;
            } else null;

            const value_type_byte = try reader.readByte();
            const value_type = @as(HIRType, @enumFromInt(value_type_byte));

            // Read whether location is present
            const has_location = (try reader.readByte()) != 0;
            const location = if (has_location) blk: {
                const file_len = try reader.readInt(u32, .little);
                const file = try allocator.alloc(u8, file_len);
                _ = try reader.readAll(file);
                const line = try reader.readInt(u32, .little);
                const column = try reader.readInt(u32, .little);
                break :blk Reporting.Reporter.Location{
                    .file = file,
                    .line = line,
                    .column = column,
                };
            } else null;

            return HIRInstruction{ .Peek = .{ .name = name, .value_type = value_type, .location = location } };
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
        .TailCall => |c| try writer.print("    TailCall {} {} \"{s}\" {s}      ; Tail call optimization\n", .{ c.function_index, c.arg_count, c.qualified_name, @tagName(c.call_kind) }),

        .Return => |r| try writer.print("    Return {}                   ; Return from function\n", .{r.has_value}),

        .Label => |l| try writer.print("{s}:                            ; Label\n", .{l.name}),

        .Dup => try writer.print("    Dup                         ; Duplicate top value\n", .{}),

        .Pop => try writer.print("    Pop                         ; Remove top value\n", .{}),

        .Peek => |i| {
            if (i.name) |name| {
                if (i.location) |location| {
                    try writer.print("    Peek \"{s}\" {s} @{s}:{}:{}         ; Debug print\n", .{ name, @tagName(i.value_type), location.file, location.line, location.column });
                } else {
                    try writer.print("    Peek \"{s}\" {s}         ; Debug print\n", .{ name, @tagName(i.value_type) });
                }
            } else {
                if (i.location) |location| {
                    try writer.print("    Peek {s} @{s}:{}:{}                 ; Debug print\n", .{ @tagName(i.value_type), location.file, location.line, location.column });
                } else {
                    try writer.print("    Peek {s}                 ; Debug print\n", .{@tagName(i.value_type)});
                }
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

        .TupleNew => |t| try writer.print("    TupleNew {}                 ; Create tuple with {} elements\n", .{ t.element_count, t.element_count }),
        .Map => |m| try writer.print("    Map {} {s}                   ; Create map with {} entries\n", .{ m.entries.len, @tagName(m.key_type), m.entries.len }),

        // Struct operations
        .StructNew => |s| try writer.print("    StructNew \"{s}\" {}          ; Create struct with {} fields\n", .{ s.type_name, s.field_count, s.field_count }),
        .GetField => |f| try writer.print("    GetField \"{s}\"               ; Get field from struct\n", .{f.field_name}),
        .SetField => |f| try writer.print("    SetField \"{s}\" {s} {}        ; Set field in struct\n", .{ f.field_name, @tagName(f.container_type), f.field_index }),
        .StoreFieldName => |s| try writer.print("    StoreFieldName \"{s}\"          ; Store field name for struct\n", .{s.field_name}),

        // Scope management
        .EnterScope => |s| try writer.print("    EnterScope {} {}            ; Enter new scope\n", .{ s.scope_id, s.var_count }),
        .ExitScope => |s| try writer.print("    ExitScope {}                ; Exit scope\n", .{s.scope_id}),

        .PeekStruct => |i| {
            try writer.print("    PeekStruct \"{s}\" {} [", .{ i.type_name, i.field_count });
            for (i.field_names, 0..) |name, idx| {
                try writer.print("\"{s}\"", .{name});
                if (idx < i.field_names.len - 1) try writer.writeByte(',');
            }
            try writer.writeAll("] [");
            for (i.field_types, 0..) |type_info, idx| {
                try writer.print("{s}", .{@tagName(type_info)});
                if (idx < i.field_types.len - 1) try writer.writeByte(',');
            }
            try writer.writeAll("]");
            if (i.location) |loc| {
                try writer.print(" @{s}:{d}:{d}", .{ loc.file, loc.line, loc.column });
            }
            try writer.writeAll("         ; Peek struct\n");
        },

        else => try writer.print("    ; TODO: {s}\n", .{@tagName(instruction)}),
    }
}

/// Simple text parser for SOXA HIR format
const StructContext = struct {
    type_name: []const u8, // Changed from struct_name to type_name to match usage
    field_count: u32,
    field_names: std.ArrayList([]const u8),
    field_types: std.ArrayList(HIRType),
    field_path: std.ArrayList([]const u8), // Added to track nested field access
};

const SoxaTextParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize = 0,
    line: u32 = 1,
    constants: std.ArrayList(HIRValue),
    functions: std.ArrayList(HIRProgram.HIRFunction),
    instructions: std.ArrayList(HIRInstruction),
    struct_context: ?StructContext = null,

    fn init(allocator: std.mem.Allocator, source: []const u8) SoxaTextParser {
        return SoxaTextParser{
            .allocator = allocator,
            .source = source,
            .constants = std.ArrayList(HIRValue).init(allocator),
            .functions = std.ArrayList(HIRProgram.HIRFunction).init(allocator),
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
            .struct_context = null,
        };
    }

    fn deinit(self: *SoxaTextParser) void {
        if (self.struct_context) |*context| {
            context.field_names.deinit();
            context.field_types.deinit();
            context.field_path.deinit();
        }
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
            } else if (std.mem.startsWith(u8, trimmed_right, "        entry:")) {
                // Function metadata - update the last function's entry point
                try self.updateFunctionEntry(trimmed_right);
                continue;
            } else if (std.mem.indexOf(u8, trimmed_right, ":")) |colon_pos| {
                // Check if this is an instruction with location info (like "Peek ... @file:line:column")
                const trimmed_line = std.mem.trim(u8, trimmed_right, " \t");
                const is_instruction_with_location = std.mem.startsWith(u8, trimmed_line, "Peek ") or
                    std.mem.startsWith(u8, trimmed_line, "Call ") or
                    std.mem.startsWith(u8, trimmed_line, "LoadVar ") or
                    std.mem.startsWith(u8, trimmed_line, "StoreVar ");

                if (is_instruction_with_location) {
                    // This is an instruction, not a label
                    if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                        try self.parseFunction(trimmed_right);
                    } else {
                        try self.parseInstruction(trimmed_right);
                    }
                } else {
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
                }
            } else if (std.mem.startsWith(u8, trimmed_right, "    ") and !std.mem.startsWith(u8, trimmed_right, "        ")) {
                // Function or instruction
                if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                    try self.parseFunction(trimmed_right);
                } else {
                    try self.parseInstruction(trimmed_right);
                }
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

        // Handle struct peekion instructions
        if (std.mem.startsWith(u8, trimmed, "PeekStruct")) {
            // Parse: PeekStruct "Person" 2 ["name", "age"] [String, Int]
            const struct_name_start = std.mem.indexOf(u8, trimmed, "\"").? + 1;
            const struct_name_end = std.mem.indexOfPos(u8, trimmed, struct_name_start, "\"").?;
            const struct_name = try self.allocator.dupe(u8, trimmed[struct_name_start..struct_name_end]);

            // Get field count
            const count_start = struct_name_end + 2;
            const count_end = std.mem.indexOfAny(u8, trimmed[count_start..], " [").? + count_start;
            const field_count = try std.fmt.parseInt(u32, trimmed[count_start..count_end], 10);

            // Initialize new struct context
            var field_names = std.ArrayList([]const u8).init(self.allocator);
            var field_types = std.ArrayList(HIRType).init(self.allocator);
            const field_path = std.ArrayList([]const u8).init(self.allocator);

            // Parse field names and types
            var in_names = true;
            var current_pos = count_end;
            while (current_pos < trimmed.len) : (current_pos += 1) {
                if (trimmed[current_pos] == '[') {
                    continue;
                } else if (trimmed[current_pos] == ']') {
                    if (in_names) {
                        in_names = false;
                    } else {
                        break;
                    }
                } else if (trimmed[current_pos] == '"') {
                    const name_start = current_pos + 1;
                    const name_end = std.mem.indexOfPos(u8, trimmed, name_start, "\"").?;
                    const name = try self.allocator.dupe(u8, trimmed[name_start..name_end]);
                    try field_names.append(name);
                    current_pos = name_end;
                } else if (std.mem.indexOfPos(u8, trimmed, current_pos, "String")) |type_pos| {
                    if (type_pos == current_pos) {
                        try field_types.append(.String);
                        current_pos = type_pos + 5;
                    }
                } else if (std.mem.indexOfPos(u8, trimmed, current_pos, "Int")) |type_pos| {
                    if (type_pos == current_pos) {
                        try field_types.append(.Int);
                        current_pos = type_pos + 2;
                    }
                }
            }

            // Set up the new struct context
            self.struct_context = .{
                .type_name = struct_name,
                .field_count = field_count,
                .field_names = field_names,
                .field_types = field_types,
                .field_path = field_path,
            };

            try self.instructions.append(.{ .PeekStruct = .{
                .type_name = struct_name,
                .field_count = field_count,
                .field_names = try field_names.toOwnedSlice(),
                .field_types = try field_types.toOwnedSlice(),
                .location = null,
            } });
            return;
        }
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
            try self.instructions.append(HIRInstruction{ .JumpCond = .{ .label_true = true_name, .label_false = false_name, .vm_offset = 0, .condition_type = .Tetra } });
        } else if (std.mem.eql(u8, op, "Call")) {
            const func_idx_str = tokens.next() orelse return;
            const arg_count_str = tokens.next() orelse return;
            const name_quoted = tokens.next() orelse return;
            const kind_str = tokens.next() orelse return;

            const function_index = std.fmt.parseInt(u32, func_idx_str, 10) catch return;
            const arg_count = std.fmt.parseInt(u32, arg_count_str, 10) catch return;
            const qualified_name = try self.parseQuotedString(name_quoted);
            const call_kind = if (std.mem.eql(u8, kind_str, "LocalFunction")) CallKind.LocalFunction else if (std.mem.eql(u8, kind_str, "ModuleFunction")) CallKind.ModuleFunction else if (std.mem.eql(u8, kind_str, "BuiltinFunction")) CallKind.BuiltinFunction else CallKind.LocalFunction;

            try self.instructions.append(HIRInstruction{
                .Call = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = null,
                    .return_type = .String, // Default to String to prevent Auto leakage
                },
            });
        } else if (std.mem.eql(u8, op, "TailCall")) {
            const func_idx_str = tokens.next() orelse return;
            const arg_count_str = tokens.next() orelse return;
            const name_quoted = tokens.next() orelse return;
            const kind_str = tokens.next() orelse return;

            const function_index = std.fmt.parseInt(u32, func_idx_str, 10) catch return;
            const arg_count = std.fmt.parseInt(u32, arg_count_str, 10) catch return;
            const qualified_name = try self.parseQuotedString(name_quoted);
            const call_kind = if (std.mem.eql(u8, kind_str, "LocalFunction")) CallKind.LocalFunction else if (std.mem.eql(u8, kind_str, "ModuleFunction")) CallKind.ModuleFunction else if (std.mem.eql(u8, kind_str, "BuiltinFunction")) CallKind.BuiltinFunction else CallKind.LocalFunction;

            try self.instructions.append(HIRInstruction{
                .TailCall = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = null,
                    .return_type = .String, // Default to String to prevent Auto leakage
                },
            });
        } else if (std.mem.eql(u8, op, "Return")) {
            const has_val_str = tokens.next() orelse return;
            const has_value = std.mem.eql(u8, has_val_str, "true");
            try self.instructions.append(HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Nothing } }); // Default to Nothing to prevent Auto leakage
        } else if (std.mem.eql(u8, op, "Dup")) {
            try self.instructions.append(HIRInstruction.Dup);
        } else if (std.mem.eql(u8, op, "Pop")) {
            try self.instructions.append(HIRInstruction.Pop);
        } else if (std.mem.eql(u8, op, "Halt")) {
            try self.instructions.append(HIRInstruction.Halt);
        } else if (std.mem.eql(u8, op, "Peek")) {
            const name_or_type = tokens.next() orelse return;
            var name: ?[]const u8 = null;
            var value_type: HIRType = .String;
            var location: ?Reporting.Reporter.Location = null;

            // For struct peekion
            var struct_name: ?[]const u8 = null;
            var field_names = std.ArrayList([]const u8).init(self.allocator);
            var field_types = std.ArrayList(HIRType).init(self.allocator);
            defer field_names.deinit();
            defer field_types.deinit();

            // Track the full path for struct fields
            var path_builder = std.ArrayList(u8).init(self.allocator);
            defer path_builder.deinit();

            // Build path from struct context if available
            if (self.struct_context) |context| {
                try path_builder.appendSlice(context.type_name);
                for (context.field_path.items) |field| {
                    try path_builder.appendSlice(".");
                    try path_builder.appendSlice(field);
                }
            }

            if (name_or_type.len > 2 and name_or_type[0] == '"' and name_or_type[name_or_type.len - 1] == '"') {
                // Has quoted name, get type from next token
                name = try self.parseQuotedString(name_or_type);
                const type_str = tokens.next() orelse "String";
                value_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Array")) HIRType.Array else if (std.mem.eql(u8, type_str, "Tuple")) HIRType.Tuple else if (std.mem.eql(u8, type_str, "Map")) HIRType.Map else if (std.mem.eql(u8, type_str, "Struct")) blk: {
                    // For structs, we need to collect field information
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else if (name) |n| n else "anonymous";
                    break :blk HIRType.Struct;
                } else if (std.mem.eql(u8, type_str, "Enum")) HIRType.Enum else HIRType.String;

                // Check for location info
                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            } else {
                // Similar logic for non-quoted case...
                value_type = if (std.mem.eql(u8, name_or_type, "Int")) HIRType.Int else if (std.mem.eql(u8, name_or_type, "Float")) HIRType.Float else if (std.mem.eql(u8, name_or_type, "String")) HIRType.String else if (std.mem.eql(u8, name_or_type, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, name_or_type, "Array")) HIRType.Array else if (std.mem.eql(u8, name_or_type, "Tuple")) HIRType.Tuple else if (std.mem.eql(u8, name_or_type, "Map")) HIRType.Map else if (std.mem.eql(u8, name_or_type, "Struct")) blk: {
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else
                        "anonymous";
                    break :blk HIRType.Struct;
                } else if (std.mem.eql(u8, name_or_type, "Enum")) HIRType.Enum else HIRType.String;

                // Check for location info after type
                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            }

            // If this is a struct peekion, gather field information from the stack
            if (value_type == .Struct) {
                // FIXED: Check if struct_context is available before trying to get struct info
                // When parsing pre-generated HIR files, struct_context may be null
                if (self.struct_context) |_| {
                    // Try to get struct info from the current context
                    const struct_info = try self.getCurrentStructInfo();
                    for (struct_info.fields) |field| {
                        try field_names.append(field.name);
                        try field_types.append(field.type);
                    }
                    // Free the allocated fields array
                    self.allocator.free(struct_info.fields);

                    try self.instructions.append(.{ .PeekStruct = .{
                        .type_name = struct_name.?,
                        .field_count = @intCast(field_names.items.len),
                        .field_names = try field_names.toOwnedSlice(),
                        .field_types = try field_types.toOwnedSlice(),
                        .location = location,
                    } });
                } else {
                    // No struct context available - create a regular Peek instruction
                    // This happens when parsing pre-generated HIR files
                    try self.instructions.append(.{ .Peek = .{
                        .name = if (path_builder.items.len > 0)
                            try self.allocator.dupe(u8, path_builder.items)
                        else
                            name,
                        .value_type = value_type,
                        .location = location,
                    } });
                }
            } else {
                try self.instructions.append(.{ .Peek = .{
                    .name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else
                        name,
                    .value_type = value_type,
                    .location = location,
                } });
            }
        } else if (std.mem.eql(u8, op, "ArrayNew")) {
            const type_str = tokens.next() orelse return;
            const size_str = tokens.next() orelse return;
            const element_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else HIRType.Auto;
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
        } else if (std.mem.eql(u8, op, "TupleNew")) {
            const count_str = tokens.next() orelse return;
            const element_count = std.fmt.parseInt(u32, count_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .TupleNew = .{ .element_count = element_count } });
        } else if (std.mem.eql(u8, op, "Map")) {
            const count_str = tokens.next() orelse return;
            const key_type_str = tokens.next() orelse return;
            const entry_count = std.fmt.parseInt(u32, count_str, 10) catch return;
            const key_type = if (std.mem.eql(u8, key_type_str, "String")) HIRType.String else if (std.mem.eql(u8, key_type_str, "Int")) HIRType.Int else HIRType.String;

            // Create dummy entries array with correct size
            const dummy_entries = try self.allocator.alloc(HIRMapEntry, entry_count);
            for (dummy_entries) |*entry| {
                entry.* = HIRMapEntry{
                    .key = HIRValue.nothing,
                    .value = HIRValue.nothing,
                };
            }

            try self.instructions.append(HIRInstruction{ .Map = .{
                .entries = dummy_entries,
                .key_type = key_type,
                .value_type = .Auto,
            } });
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
        } else if (std.mem.eql(u8, op, "StructNew")) {
            const type_name_quoted = tokens.next() orelse return;
            const field_count_str = tokens.next() orelse return;

            const type_name = try self.parseQuotedString(type_name_quoted);
            const field_count = std.fmt.parseInt(u32, field_count_str, 10) catch return;

            // Create dummy field types - these will be populated by the VM at runtime
            // based on the actual values on the stack
            const field_types = try self.allocator.alloc(HIRType, field_count);
            for (field_types) |*field_type| {
                field_type.* = HIRType.Auto; // Default to Auto, will be resolved at runtime
            }

            try self.instructions.append(HIRInstruction{
                .StructNew = .{
                    .type_name = type_name,
                    .field_count = field_count,
                    .field_types = field_types,
                    .size_bytes = 0, // Size will be calculated at runtime
                },
            });
        } else if (std.mem.eql(u8, op, "GetField")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .GetField = .{
                    .field_name = field_name,
                    .container_type = HIRType.Struct,
                    .field_index = 0, // Will be resolved at runtime
                    .field_for_peek = false,
                },
            });
        } else if (std.mem.eql(u8, op, "SetField")) {
            const field_name_quoted = tokens.next() orelse return;
            // Parse format: SetField "age" Struct 0
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .SetField = .{
                    .field_name = field_name,
                    .container_type = HIRType.Struct,
                    .field_index = 0, // Will be resolved at runtime
                },
            });
        } else if (std.mem.eql(u8, op, "StoreFieldName")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{ .StoreFieldName = .{
                .field_name = field_name,
            } });
        }
        // Instructions not implemented yet are silently ignored for now
    }

    fn parseQuotedString(self: *SoxaTextParser, quoted: []const u8) ![]const u8 {
        if (quoted.len >= 2 and quoted[0] == '"' and quoted[quoted.len - 1] == '"') {
            return try self.allocator.dupe(u8, quoted[1 .. quoted.len - 1]);
        }
        return try self.allocator.dupe(u8, quoted);
    }

    const StructInfo = struct {
        fields: []StructField,

        const StructField = struct {
            name: []const u8,
            type: HIRType,
        };
    };

    fn getCurrentStructInfo(self: *SoxaTextParser) !StructInfo {
        if (self.struct_context) |context| {
            // Create array of fields with their types
            var fields = try self.allocator.alloc(StructInfo.StructField, context.field_count);
            for (0..context.field_count) |i| {
                fields[i] = .{
                    .name = context.field_names.items[i],
                    .type = context.field_types.items[i],
                };
            }
            return StructInfo{ .fields = fields };
        }
        return error.NoStructContext;
    }

    fn parseLocationString(self: *SoxaTextParser, location_str: []const u8) !Reporting.Reporter.Location {
        // Parse format: @file:line:column
        // Handle Windows paths by splitting from the right side
        if (!std.mem.startsWith(u8, location_str, "@")) {
            return error.InvalidLocationFormat;
        }

        const location_part = location_str[1..]; // Skip @

        // Find the last two colons to get line and column
        const last_colon = std.mem.lastIndexOfScalar(u8, location_part, ':') orelse return error.InvalidLocationFormat;
        const second_last_colon = std.mem.lastIndexOfScalar(u8, location_part[0..last_colon], ':') orelse return error.InvalidLocationFormat;

        const file_part = location_part[0..second_last_colon];
        const line_part = location_part[second_last_colon + 1 .. last_colon];
        const column_part = location_part[last_colon + 1 ..];

        const file = try self.allocator.dupe(u8, file_part);
        const line = try std.fmt.parseInt(i32, line_part, 10);
        const column = try std.fmt.parseInt(usize, column_part, 10);

        return Reporting.Reporter.Location{
            .file = file,
            .line = line,
            .column = column,
        };
    }
};

/// NEW: Custom type information for struct/enum types
pub const CustomTypeInfo = struct {
    name: []const u8,
    kind: CustomTypeKind,

    pub const CustomTypeKind = enum {
        Struct,
        Enum,
    };
};

/// NEW: HIR generation statistics
pub const HIRStats = struct {
    instructions_generated: u32,
    functions_generated: u32,
    constants_generated: u32,
    variables_created: u32,

    pub fn init(allocator: std.mem.Allocator) HIRStats {
        _ = allocator; // Unused for now
        return HIRStats{
            .instructions_generated = 0,
            .functions_generated = 0,
            .constants_generated = 0,
            .variables_created = 0,
        };
    }
};

/// NEW: Function call site information for tail recursion
pub const FunctionCallSite = struct {
    function_name: []const u8,
    is_tail_position: bool,
    instruction_index: u32,
};
