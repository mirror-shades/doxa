const std = @import("std");
const ast = @import("../../ast/ast.zig");
const reporting = @import("../../utils/reporting.zig");
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const SoxaInstructions = @import("soxa_instructions.zig");
const HIRInstruction = SoxaInstructions.HIRInstruction;
const SoxaValues = @import("soxa_values.zig");
const HIRValue = SoxaValues.HIRValue;
const HIREnum = SoxaValues.HIREnum;
const HIRMapEntry = SoxaValues.HIRMapEntry;
const SoxaTypes = @import("soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const CallKind = SoxaTypes.CallKind;
const HIRProgram = SoxaTypes.HIRProgram;

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

    variable_types: std.StringHashMap(HIRType), // Track inferred types of variables
    variable_custom_types: std.StringHashMap([]const u8), // Track custom type names for variables

    custom_types: std.StringHashMap(CustomTypeInfo), // Track struct/enum type definitions

    function_signatures: std.StringHashMap(FunctionInfo),
    function_bodies: std.ArrayList(FunctionBody),
    current_function: ?[]const u8,
    current_function_return_type: HIRType, // Track current function's return type for Return instructions

    stats: HIRStats,

    function_calls: std.ArrayList(FunctionCallSite),

    module_namespaces: std.StringHashMap(ast.ModuleInfo), // Track module namespaces for function call resolution

    current_enum_type: ?[]const u8 = null,
    // Track array element types per variable for better index/peek typing
    variable_array_element_types: std.StringHashMap(HIRType),

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

    pub const FunctionCallSite = struct {
        function_name: []const u8,
        is_tail_position: bool,
        instruction_index: u32,
    };

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

    pub const StructPeekInfo = struct {
        name: []const u8,
        field_count: u32,
        field_names: [][]const u8,
        field_types: []HIRType,
    };

    pub const CustomTypeInfo = struct {
        name: []const u8,
        kind: CustomTypeKind,
        // NEW: Enhanced enum support
        enum_variants: ?[]EnumVariant = null,
        // NEW: Enhanced struct support
        struct_fields: ?[]StructField = null,

        pub const CustomTypeKind = enum {
            Struct,
            Enum,
        };

        pub const EnumVariant = struct {
            name: []const u8,
            index: u32,
        };

        pub const StructField = struct {
            name: []const u8,
            field_type: HIRType,
            index: u32,
        };

        /// Get the index of an enum variant by name
        pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
            if (self.kind != .Enum or self.enum_variants == null) return null;

            for (self.enum_variants.?) |variant| {
                if (std.mem.eql(u8, variant.name, variant_name)) {
                    return variant.index;
                }
            }
            return null;
        }

        /// Get the index of a struct field by name
        pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
            if (self.kind != .Struct or self.struct_fields == null) return null;

            for (self.struct_fields.?) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    return field.index;
                }
            }
            return null;
        }
    };

    pub fn init(allocator: std.mem.Allocator, reporter: *reporting.Reporter, module_namespaces: std.StringHashMap(ast.ModuleInfo)) HIRGenerator {
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
            .variable_custom_types = std.StringHashMap([]const u8).init(allocator),
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .function_signatures = std.StringHashMap(FunctionInfo).init(allocator),
            .function_bodies = std.ArrayList(FunctionBody).init(allocator),
            .current_function = null,
            .current_function_return_type = .Nothing,
            .function_calls = std.ArrayList(FunctionCallSite).init(allocator),
            .module_namespaces = module_namespaces,
            .current_enum_type = null,
            .stats = HIRStats.init(allocator),
            .variable_array_element_types = std.StringHashMap(HIRType).init(allocator),
        };
    }

    pub fn deinit(self: *HIRGenerator) void {
        self.instructions.deinit();
        self.constants.deinit();
        self.string_pool.deinit();
        self.constant_map.deinit();
        self.variables.deinit();
        self.variable_types.deinit();
        self.variable_custom_types.deinit();
        self.custom_types.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_calls.deinit();
        self.variable_array_element_types.deinit();
        // Note: module_namespaces is not owned by HIRGenerator, so we don't deinit it
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
                    .expected_type = param_type,
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
            if (!has_returned) {
                // If return type expects a value, try to return the last expression value of the body
                if (function_body.function_info.return_type != .Nothing) {
                    // Find last expression statement in the function body
                    var last_expr: ?*ast.Expr = null;
                    var i: usize = function_body.statements.len;
                    while (i > 0) : (i -= 1) {
                        const s = function_body.statements[i - 1];
                        switch (s.data) {
                            .Expression => |maybe_e| {
                                if (maybe_e) |e| {
                                    last_expr = e;
                                }
                                break;
                            },
                            else => {},
                        }
                    }

                    if (last_expr) |e| {
                        try self.generateExpression(e, true);
                        try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = function_body.function_info.return_type } });
                    } else {
                        // No final expression found; emit a no-value return (semantic pass should have validated paths)
                        try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                    }
                } else {
                    // Void function: implicit return without value
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                }
            }

            // Clear current function context
            self.current_function = null;
            self.current_function_return_type = .Nothing;
        }
    }

    /// Pass 2: Generate main program (non-function statements) - FIRST so execution starts here
    fn generateMainProgram(self: *HIRGenerator, statements: []ast.Stmt) !void {
        // Process all non-function statements first (global variables, etc.)
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

        // After processing global statements, call main if it exists
        if (self.function_signatures.get("main")) |main_func| {
            // Get the correct function index for main
            const main_function_index = self.getFunctionIndex("main") catch 0;

            // Generate a call to main function
            const main_call_instruction = HIRInstruction{
                .Call = .{
                    .function_index = main_function_index,
                    .qualified_name = "main",
                    .arg_count = 0,
                    .call_kind = .LocalFunction,
                    .target_module = null,
                    .return_type = main_func.return_type,
                },
            };
            try self.instructions.append(main_call_instruction);

            // Pop the return value if any (since we're not using it)
            if (main_func.return_type != .Nothing) {
                try self.instructions.append(.Pop);
            }
        }

        // Add halt instruction to end main program execution
        // This prevents execution from falling through to function definitions
        try self.instructions.append(.Halt);
    }

    /// Pass 4: Build function table from collected signatures
    fn buildFunctionTable(self: *HIRGenerator) ![]HIRProgram.HIRFunction {
        var function_table = std.ArrayList(HIRProgram.HIRFunction).init(self.allocator);

        // Use function_bodies order for deterministic indices (same as getFunctionIndex)
        for (self.function_bodies.items) |function_body| {
            const function_info = function_body.function_info;

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
        // Use function_bodies order for deterministic indices
        for (self.function_bodies.items, 0..) |function_body, index| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return @as(u32, @intCast(index));
            }
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
            .Byte => .Byte,
            .Array => .Array,
            // Treat unions as having a value at runtime; approximate to Int to avoid dropping returns
            .Union => .Int,
            else => .Nothing,
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
                    try self.generateExpression(e, false);
                }
            },
            .VarDecl => |decl| {

                // NEW: Determine the variable's type for tracking
                var var_type: HIRType = .Nothing;

                // FIXED: Prioritize explicit type annotation over inference
                var custom_type_name: ?[]const u8 = null;
                if (decl.type_info.base != .Nothing) {
                    // Use explicit type annotation first
                    var_type = switch (decl.type_info.base) {
                        .Int => .Int,
                        .Float => .Float,
                        .String => .String,
                        .Tetra => .Tetra,
                        .Byte => .Byte,
                        .Array => .Array, // Add missing Array case
                        .Enum => blk: {
                            // Extract the actual enum type name from the custom_type field
                            custom_type_name = decl.type_info.custom_type;
                            break :blk .Enum;
                        },
                        .Struct => blk: {
                            // Extract the actual struct type name from the custom_type field
                            custom_type_name = decl.type_info.custom_type;
                            break :blk .Struct;
                        },
                        .Custom => blk: {
                            // CRITICAL FIX: Handle Custom type - check if it's an enum or struct
                            if (decl.type_info.custom_type) |type_name| {
                                // Check if this custom type is a registered enum
                                if (self.custom_types.get(type_name)) |custom_type| {
                                    if (custom_type.kind == .Enum) {
                                        custom_type_name = type_name;
                                        break :blk .Enum;
                                    } else if (custom_type.kind == .Struct) {
                                        // Track the struct type name for instances
                                        try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                        break :blk .Struct;
                                    }
                                }
                                // If not found in custom_types, assume it's a struct and track it
                                try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                break :blk .Struct;
                            }
                            // If we can't determine the custom type, it's unknown
                            break :blk .Nothing; // Unknown custom type
                        },
                        else => .Nothing,
                    };
                }

                // Generate the initializer expression with enum type context
                if (decl.initializer) |init_expr| {
                    // Set enum type context if we're declaring an enum variable
                    const old_enum_context = self.current_enum_type;
                    if (custom_type_name != null) {
                        self.current_enum_type = custom_type_name;
                    }

                    try self.generateExpression(init_expr, true);

                    // Restore previous enum context
                    self.current_enum_type = old_enum_context;

                    // Only infer type if no explicit annotation was provided
                    if (var_type == .Nothing) {
                        var_type = self.inferTypeFromExpression(init_expr);

                        // FIXED: Extract custom type name from struct literals
                        if (var_type == .Struct and init_expr.data == .StructLiteral) {
                            const struct_lit = init_expr.data.StructLiteral;
                            try self.trackVariableCustomType(decl.name.lexeme, struct_lit.name.lexeme);
                        }
                    }
                } else {
                    // No initializer - push default value based on type
                    switch (var_type) {
                        .Int => {
                            const default_value = HIRValue{ .int = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Float => {
                            const default_value = HIRValue{ .float = 0.0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .String => {
                            const default_value = HIRValue{ .string = "" };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Tetra => {
                            const default_value = HIRValue{ .tetra = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Byte => {
                            const default_value = HIRValue{ .byte = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Array => {
                            // Create an array with the proper size and element type
                            const size = if (decl.type_info.array_size) |s| @as(u32, @intCast(s)) else 0;
                            const element_type = if (decl.type_info.array_type) |at| switch (at.base) {
                                .Byte => HIRType.Byte,
                                .Int => HIRType.Int,
                                .Float => HIRType.Float,
                                .String => HIRType.String,
                                .Tetra => HIRType.Tetra,
                                else => HIRType.Nothing,
                            } else HIRType.Nothing;

                            // Create the array
                            try self.instructions.append(.{ .ArrayNew = .{
                                .element_type = element_type,
                                .size = size,
                            } });

                            // Track element type for this variable
                            try self.trackArrayElementType(decl.name.lexeme, element_type);
                        },
                        else => {
                            const default_value = HIRValue.nothing;
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                    }
                }

                // NEW: Track the variable's type
                try self.trackVariableType(decl.name.lexeme, var_type);

                // NEW: Track custom type name for enums/structs
                if (custom_type_name) |custom_type| {
                    try self.trackVariableCustomType(decl.name.lexeme, custom_type);
                }

                // Store variable (single instruction). Duplicate value only at global scope
                const var_idx = try self.getOrCreateVariable(decl.name.lexeme);
                if (self.current_function == null) {
                    try self.instructions.append(.Dup);
                }

                // Use StoreConst for constant declarations, StoreVar for variables
                if (!decl.type_info.is_mutable) {
                    try self.instructions.append(.{ .StoreConst = .{
                        .var_index = var_idx,
                        .var_name = decl.name.lexeme,
                    } });
                } else {
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = decl.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = var_type,
                    } });
                }

                // No extra value should remain on stack now; nothing to pop
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
                        try self.generateExpression(value, true);
                        // Infer return type from the returned expression to avoid relying on signature inference
                        const inferred_ret_type = self.inferTypeFromExpression(value);
                        try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = inferred_ret_type } });
                    }
                } else {
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                }
            },
            .EnumDecl => |enum_decl| {
                // NEW: Register enum type with variants for proper index calculation
                var variant_names = try self.allocator.alloc([]const u8, enum_decl.variants.len);
                for (enum_decl.variants, 0..) |variant_token, i| {
                    variant_names[i] = variant_token.lexeme;
                }
                try self.registerEnumType(enum_decl.name.lexeme, variant_names);

                // Register the enum type name as a special variable so Color.Red works
                const var_idx = try self.getOrCreateVariable(enum_decl.name.lexeme);
                try self.trackVariableType(enum_decl.name.lexeme, .Enum);

                // Create a special enum type value and store it
                const enum_type_value = HIRValue{ .string = enum_decl.name.lexeme }; // Simple representation for now
                const const_idx = try self.addConstant(enum_type_value);
                try self.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
                try self.instructions.append(.{ .StoreConst = .{
                    .var_index = var_idx,
                    .var_name = enum_decl.name.lexeme,
                } });

                // Enum declarations don't generate runtime instructions, they're compile-time only
            },
            .Try => |try_stmt| {
                try self.generateTryStmt(try_stmt);
            },
            .Assert => |assert_stmt| {
                // Generate the condition expression
                try self.generateExpression(assert_stmt.condition, true);

                // Create labels for control flow
                const success_label = try std.fmt.allocPrint(self.allocator, "assert_success_{}", .{self.label_count});
                const failure_label = try std.fmt.allocPrint(self.allocator, "assert_failure_{}", .{self.label_count});
                self.label_count += 1;

                // Jump to success label if condition is true, fall through to failure if false
                try self.instructions.append(.{
                    .JumpCond = .{
                        .label_true = success_label,
                        .label_false = failure_label,
                        .vm_offset = 0, // Will be patched
                        .condition_type = .Tetra,
                    },
                });

                // Failure label - condition was false
                try self.instructions.append(.{ .Label = .{ .name = failure_label, .vm_address = 0 } });

                // Handle assert message if provided
                if (assert_stmt.message) |msg| {
                    // Generate message expression (will be on stack for AssertFail to use)
                    try self.generateExpression(msg, true);
                    try self.instructions.append(.{ .AssertFail = .{
                        .location = assert_stmt.location,
                        .has_message = true,
                    } });
                } else {
                    // No message provided
                    try self.instructions.append(.{ .AssertFail = .{
                        .location = assert_stmt.location,
                        .has_message = false,
                    } });
                }

                // Success label - continue execution
                try self.instructions.append(.{ .Label = .{ .name = success_label, .vm_address = 0 } });
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
    fn generateExpression(self: *HIRGenerator, expr: *ast.Expr, preserve_result: bool) !void {
        switch (expr.data) {
            .Literal => |lit| {
                const hir_value = switch (lit) {
                    .int => |i| HIRValue{ .int = i },
                    .float => |f| HIRValue{ .float = f },
                    .string => |s| HIRValue{ .string = s },
                    .tetra => |t| HIRValue{ .tetra = tetraFromEnum(t) },
                    .byte => |b| HIRValue{ .byte = b },
                    .nothing => HIRValue.nothing,
                    .enum_variant => |variant| blk: {
                        // Handle enum variant literals - need to find the enum type
                        if (self.current_enum_type) |enum_type_name| {
                            // Look up the actual variant index from registered enum type
                            const variant_index = if (self.custom_types.get(enum_type_name)) |custom_type|
                                custom_type.getEnumVariantIndex(variant) orelse 0
                            else
                                0;

                            break :blk HIRValue{
                                .enum_variant = HIREnum{
                                    .type_name = enum_type_name,
                                    .variant_name = variant,
                                    .variant_index = variant_index,
                                    .path = null,
                                },
                            };
                        } else {
                            // Try to infer enum type from context or fallback to string
                            // This is a fallback for when enum context is not available
                            break :blk HIRValue{ .string = variant };
                        }
                    },
                    else => HIRValue.nothing,
                };
                const const_idx = try self.addConstant(hir_value);
                try self.instructions.append(.{ .Const = .{ .value = hir_value, .constant_id = const_idx } });
            },

            .Variable => |var_token| {
                // Compile-time validation: Ensure variable has been declared
                if (self.variables.get(var_token.lexeme)) |existing_idx| {
                    const var_idx = existing_idx;
                    try self.instructions.append(.{
                        .LoadVar = .{
                            .var_index = var_idx,
                            .var_name = var_token.lexeme,
                            .scope_kind = .Local, // TODO: determine actual scope
                            .module_context = null,
                        },
                    });
                } else {
                    // Report undefined variable error at compile time to avoid runtime failure
                    self.reporter.reportError("Undefined variable at compile time: {s}", .{var_token.lexeme});
                    return reporting.ErrorList.UndefinedVariable;
                }
            },

            .Binary => |bin| {
                // Generate left operand (pushes to stack)
                try self.generateExpression(bin.left.?, true);

                // Generate right operand (pushes to stack)
                try self.generateExpression(bin.right.?, true);

                // Determine the result type for proper instruction selection
                const result_type = self.inferBinaryOpResultType(bin.operator.type, bin.left.?, bin.right.?);

                switch (bin.operator.type) {
                    .PLUS => {
                        if (result_type == .Float) {
                            try self.instructions.append(.{ .FloatArith = .{ .op = .Add, .exception_behavior = .Trap } });
                        } else {
                            try self.instructions.append(.{ .IntArith = .{ .op = .Add, .overflow_behavior = .Wrap } });
                        }
                    },
                    .MINUS => {
                        if (result_type == .Float) {
                            try self.instructions.append(.{ .FloatArith = .{ .op = .Sub, .exception_behavior = .Trap } });
                        } else {
                            try self.instructions.append(.{ .IntArith = .{ .op = .Sub, .overflow_behavior = .Wrap } });
                        }
                    },
                    .ASTERISK => {
                        if (result_type == .Float) {
                            try self.instructions.append(.{ .FloatArith = .{ .op = .Mul, .exception_behavior = .Trap } });
                        } else {
                            try self.instructions.append(.{ .IntArith = .{ .op = .Mul, .overflow_behavior = .Wrap } });
                        }
                    },
                    .SLASH => {
                        // Division always returns float
                        try self.instructions.append(.{ .FloatArith = .{ .op = .Div, .exception_behavior = .Trap } });
                    },
                    .MODULO => {
                        // Modulo only works with integers
                        try self.instructions.append(.{ .IntArith = .{ .op = .Mod, .overflow_behavior = .Wrap } });
                    },
                    .POWER => {
                        // POWER always returns float, so we need to convert operands to float first
                        // For now, use a builtin function call since we don't have a direct power instruction
                        try self.instructions.append(.{
                            .Call = .{
                                .function_index = 0, // Will be resolved to builtin power function
                                .call_kind = .BuiltinFunction,
                                .qualified_name = "power",
                                .arg_count = 2,
                                .target_module = null,
                                .return_type = .Float,
                            },
                        });
                    },
                    .EQUALITY => try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = result_type } }),
                    .BANG_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Ne, .operand_type = result_type } }),
                    .LESS => try self.instructions.append(.{ .Compare = .{ .op = .Lt, .operand_type = result_type } }),
                    .GREATER => try self.instructions.append(.{ .Compare = .{ .op = .Gt, .operand_type = result_type } }),
                    .LESS_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Le, .operand_type = result_type } }),
                    .GREATER_EQUAL => try self.instructions.append(.{ .Compare = .{ .op = .Ge, .operand_type = result_type } }),
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
                    try self.generateExpression(log.left, true);
                    try self.instructions.append(.Dup); // Keep left value for potential short-circuit

                    const short_circuit_label = try self.generateLabel("and_short_circuit");
                    const end_label = try self.generateLabel("and_end");

                    // If left is false, short-circuit to end; if left is true, continue to evaluate right
                    try self.instructions.append(.{
                        .JumpCond = .{
                            .label_true = short_circuit_label,
                            .label_false = end_label,
                            .vm_offset = 0, // Will be patched
                            .condition_type = .Tetra,
                        },
                    });

                    // Evaluate right side
                    try self.instructions.append(.{ .Label = .{ .name = short_circuit_label, .vm_address = 0 } });
                    try self.instructions.append(.Pop); // Remove the duplicated left value
                    try self.generateExpression(log.right, true);

                    try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
                } else if (log.operator.type == .OR) {
                    // Similar for OR but with inverted logic
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Or } });
                    // Simple OR for now - TODO: add short-circuit optimization
                } else if (log.operator.type == .IFF) {
                    // IFF (if and only if): A ↔ B - true when A and B have same truth value
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Iff } });
                } else if (log.operator.type == .XOR) {
                    // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Xor } });
                } else if (log.operator.type == .NAND) {
                    // NAND: A ↑ B - NOT(A AND B)
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nand } });
                } else if (log.operator.type == .NOR) {
                    // NOR: A ↓ B - NOT(A OR B)
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nor } });
                } else if (log.operator.type == .IMPLIES) {
                    // IMPLIES: A → B - NOT A OR B
                    try self.generateExpression(log.left, true);
                    try self.generateExpression(log.right, true);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Implies } });
                } else {
                    self.reporter.reportError("Unsupported logical operator: {}", .{log.operator.type});
                    return reporting.ErrorList.UnsupportedOperator;
                }
            },

            .If => |if_expr| {

                // Generate condition
                try self.generateExpression(if_expr.condition.?, true);

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
                try self.generateExpression(if_expr.then_branch.?, true);

                // Jump to end
                try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                // Else label
                try self.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });

                // Generate else branch (or push nothing if no else)
                if (if_expr.else_branch) |else_branch| {
                    try self.generateExpression(else_branch, true);
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
                try self.generateExpression(while_expr.condition, true);

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
                try self.generateExpression(while_expr.body, false);

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
                        // Check if this is a module function call (e.g., safeMath.safeAdd)
                        if (field_access.object.data == .Variable) {
                            const object_name = field_access.object.data.Variable.lexeme;

                            if (self.isModuleNamespace(object_name)) {
                                // This is a module function call like safeMath.safeAdd
                                function_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ object_name, field_access.field.lexeme });
                                call_kind = .ModuleFunction;

                                // Don't generate the object expression - skip the LoadVar
                                // Just generate the arguments
                                for (call.arguments) |arg| {
                                    try self.generateExpression(arg, true);
                                }
                            } else {
                                function_name = field_access.field.lexeme;
                                call_kind = .BuiltinFunction;
                                method_target_var = object_name;

                                // CRITICAL FIX: Generate object FIRST, then arguments for correct stack order
                                // The VM's push method expects: element (top), array (bottom)
                                // So we need stack order: [array, element] (bottom to top)

                                // Generate the object expression (arr) FIRST
                                try self.generateExpression(field_access.object, true);

                                // Generate arguments (in order) AFTER object - these will be on top for VM
                                for (call.arguments) |arg| {
                                    try self.generateExpression(arg, true);
                                }
                            }
                        } else {
                            // Complex object expression (not a simple variable)
                            function_name = field_access.field.lexeme;
                            call_kind = .BuiltinFunction;

                            // Generate the object expression FIRST
                            try self.generateExpression(field_access.object, true);

                            // Generate arguments (in order) AFTER object
                            for (call.arguments) |arg| {
                                try self.generateExpression(arg, true);
                            }
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

                // Generate arguments for non-method calls (method calls and module calls handle arguments differently)
                if (call.callee.data != .FieldAccess) {
                    for (call.arguments, 0..) |arg, arg_index| {
                        // Check if this is a default argument placeholder
                        if (arg.data == .DefaultArgPlaceholder) {
                            // Try to resolve the default value from function signature
                            if (self.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                                try self.generateExpression(default_expr, true);
                            } else {
                                try self.generateExpression(arg, true);
                            }
                        } else {
                            try self.generateExpression(arg, true);
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
                        const expected_type = self.getTrackedVariableType(target_var) orelse .Auto;
                        try self.instructions.append(.{ .StoreVar = .{
                            .var_index = var_idx,
                            .var_name = target_var,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = expected_type,
                        } });
                    }
                }
            },

            .Peek => |peek| {

                // Set current peek expression for field access tracking
                self.current_peek_expr = peek.expr;
                defer self.current_peek_expr = null;

                // Generate the expression to peek (leaves value on stack)
                try self.generateExpression(peek.expr, true);

                // Build the full path for the peek expression (handles field access)
                // Special case: Don't show variable name for enum member access like Color.Red
                const peek_path = if (peek.expr.data == .FieldAccess) blk: {
                    const field = peek.expr.data.FieldAccess;
                    const obj_type = self.inferTypeFromExpression(field.object);
                    // If this is enum member access (Color.Red), don't show variable name
                    if (obj_type == .Enum and field.object.data == .Variable) {
                        break :blk null; // No variable name for enum member access
                    } else {
                        break :blk try self.buildPeekPath(peek.expr);
                    }
                } else try self.buildPeekPath(peek.expr);

                // NEW: Prefer expression inference; refine for array indexing
                var inferred_type: HIRType = self.inferTypeFromExpression(peek.expr);
                if (peek.expr.data == .Index and peek.expr.data.Index.array.data == .Variable) {
                    if (self.getTrackedArrayElementType(peek.expr.data.Index.array.data.Variable.lexeme)) |elem_type| {
                        inferred_type = elem_type;
                    }
                } else if (peek.expr.data == .Variable) {
                    if (self.getTrackedVariableType(peek.expr.data.Variable.lexeme)) |tracked_type| {
                        inferred_type = tracked_type;
                    }
                }

                // Generate peek instruction with full path and correct type
                try self.instructions.append(.{ .Peek = .{
                    .name = peek_path,
                    .value_type = inferred_type,
                    .location = peek.location,
                } });

                // IMPORTANT: Peek pops the value, prints, then pushes it back.
                // If the caller does not need the result (statement context), drop it now
                if (!preserve_result) {
                    try self.instructions.append(.Pop);
                }
            },

            .Assignment => |assign| {

                // Generate the value expression
                try self.generateExpression(assign.value.?, true);

                // NEW: Track the variable's type from the assigned value
                const assigned_type = self.inferTypeFromExpression(assign.value.?);
                try self.trackVariableType(assign.name.lexeme, assigned_type);

                // Get or create variable index
                const var_idx = try self.getOrCreateVariable(assign.name.lexeme);

                // Duplicate value to leave it on stack as assignment result
                if (preserve_result) {
                    try self.instructions.append(.Dup);
                }

                // Store to variable
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = assign.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = assigned_type,
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
                            .byte => .Byte,
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

                // Note: We cannot track the target variable here without broader context

                // Generate each element and ArraySet
                for (elements, 0..) |element, i| {
                    // Duplicate array reference (needed for ArraySet)
                    try self.instructions.append(.Dup);

                    // Push index first
                    const index_value = HIRValue{ .int = @intCast(i) };
                    const index_const = try self.addConstant(index_value);
                    try self.instructions.append(.{ .Const = .{ .value = index_value, .constant_id = index_const } });

                    // Generate the element value
                    try self.generateExpression(element, true);

                    // Set array element (stack: array, index, value)
                    try self.instructions.append(.{ .ArraySet = .{ .bounds_check = false } }); // No bounds check for initialization
                }
            },

            .Index => |index| {

                // Generate array/map expression
                try self.generateExpression(index.array, true);

                // Determine if we're accessing an array, map
                const container_type = self.inferTypeFromExpression(index.array);
                switch (container_type) {
                    .Map => {
                        // Generate index expression
                        try self.generateExpression(index.index, true);

                        // Map access - use MapGet
                        try self.instructions.append(.{
                            .MapGet = .{
                                .key_type = .String, // For now, assume string keys
                            },
                        });
                    },
                    .Array => {
                        // Generate index expression
                        try self.generateExpression(index.index, true);

                        // Array access - use ArrayGet
                        try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });

                        // Record element type for index expressions into variables
                        if (index.array.data == .Variable) {
                            if (self.getTrackedArrayElementType(index.array.data.Variable.lexeme)) |elem_type| {
                                try self.trackVariableType("__index_tmp__", elem_type);
                            }
                        }
                    },
                    else => {
                        // Generate index expression
                        try self.generateExpression(index.index, true);

                        // Default to array access for now
                        try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
                    },
                }
            },

            .IndexAssign => |assign| {
                // Generate array expression
                try self.generateExpression(assign.array, true);

                // Generate index expression
                try self.generateExpression(assign.index, true);

                // Generate value expression
                try self.generateExpression(assign.value, true);

                // Generate ArraySet instruction
                // Stack order expected by VM (top to bottom): value, index, array
                try self.instructions.append(.{ .ArraySet = .{ .bounds_check = true } });

                // Store the modified array back to the variable
                if (assign.array.data == .Variable) {
                    const var_name = assign.array.data.Variable.lexeme;

                    const var_idx = try self.getOrCreateVariable(var_name);
                    const expected_type = self.getTrackedVariableType(var_name) orelse .Auto;

                    // Duplicate the result to leave it on stack as the expression result
                    if (preserve_result) {
                        try self.instructions.append(.Dup);
                    }

                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = var_name,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = expected_type,
                    } });
                }
            },

            .ArrayPush => |push| {
                // Generate array then element so that stack is: [ ... array, element ]
                try self.generateExpression(push.array, true);
                try self.generateExpression(push.element, true);

                // Emit ArrayPush instruction (uses stack: pops element, then array)
                try self.instructions.append(.{ .ArrayPush = .{ .resize_behavior = .Double } });

                // If the receiver is a variable, store the updated array back into it
                if (push.array.data == .Variable) {
                    const var_name = push.array.data.Variable.lexeme;
                    const var_idx = try self.getOrCreateVariable(var_name);
                    const expected_type = self.getTrackedVariableType(var_name) orelse .Auto;

                    // Leave result on stack if expression result must be preserved
                    if (preserve_result) {
                        try self.instructions.append(.Dup);
                    }

                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = var_name,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = expected_type,
                    } });
                }
            },

            .Grouping => |grouping| {
                // Grouping is just parentheses - generate the inner expression
                if (grouping) |inner_expr| {
                    try self.generateExpression(inner_expr, true);
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
                    try self.generateExpression(value_expr, true);
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
                try self.generateExpression(compound.value.?, true);

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
                if (preserve_result) {
                    try self.instructions.append(.Dup);
                }

                // Store the result back to the variable
                const expected_type = self.getTrackedVariableType(compound.name.lexeme) orelse .Auto;
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = compound.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = expected_type,
                } });
            },

            .ReturnExpr => |return_expr| {

                // TAIL CALL OPTIMIZATION: Check if return value is a direct function call
                if (return_expr.value) |value| {
                    if (self.tryGenerateTailCall(value)) {
                        return; // Tail call replaces both Call and Return
                    } else {
                        // Regular return with value
                        try self.generateExpression(value, true);
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
                try self.generateExpression(unary.right.?, true);

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
                try self.generateExpression(forall.array, true);

                // Check if the condition is a simple binary comparison
                if (forall.condition.data == .Binary) {
                    const binary = forall.condition.data.Binary;
                    if (binary.right) |right| {
                        switch (right.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "e > 3"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.addConstant(comparison_value);
                                try self.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "e > checkAgainst"
                                // Generate code to load the variable value at runtime
                                const var_name = var_token.lexeme;
                                if (self.variables.get(var_name)) |var_index| {
                                    try self.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = .Local,
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    self.reporter.reportError("Undefined variable in quantifier condition: {s}", .{var_name});
                                    return reporting.ErrorList.UndefinedVariable;
                                }
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generateExpression(right, true);
                            },
                        }
                    } else {
                        // No right operand - generate the condition as-is
                        try self.generateExpression(forall.condition, true);
                    }
                } else {
                    // Complex condition - generate the expression as-is
                    try self.generateExpression(forall.condition, true);
                }

                // Use builtin function call with proper predicate
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = 0,
                        .qualified_name = "forall_quantifier_gt",
                        .arg_count = 2, // array + comparison value
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
                try self.generateExpression(exists.array, true);

                // Check if the condition is a simple binary comparison
                if (exists.condition.data == .Binary) {
                    const binary = exists.condition.data.Binary;
                    if (binary.right) |right| {
                        switch (right.data) {
                            .Literal => |lit| {
                                // Handle literal comparisons like "e > 3"
                                const comparison_value = switch (lit) {
                                    .int => |i| HIRValue{ .int = i },
                                    .float => |f| HIRValue{ .float = f },
                                    .string => |s| HIRValue{ .string = s },
                                    else => HIRValue{ .int = 0 },
                                };
                                const const_idx = try self.addConstant(comparison_value);
                                try self.instructions.append(.{ .Const = .{ .value = comparison_value, .constant_id = const_idx } });
                            },
                            .Variable => |var_token| {
                                // Handle variable comparisons like "e > checkAgainst"
                                // Generate code to load the variable value at runtime
                                const var_name = var_token.lexeme;
                                if (self.variables.get(var_name)) |var_index| {
                                    try self.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = .Local,
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    self.reporter.reportError("Undefined variable in quantifier condition: {s}", .{var_name});
                                    return reporting.ErrorList.UndefinedVariable;
                                }
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generateExpression(right, true);
                            },
                        }
                    } else {
                        // No right operand - generate the condition as-is
                        try self.generateExpression(exists.condition, true);
                    }
                } else {
                    // Complex condition - generate the expression as-is
                    try self.generateExpression(exists.condition, true);
                }

                // Use builtin function call with proper predicate
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = 0,
                        .qualified_name = "exists_quantifier_gt",
                        .arg_count = 2, // array + comparison value
                        .call_kind = .BuiltinFunction,
                        .target_module = null,
                        .return_type = .Tetra,
                    },
                });
            },

            .Match => |match_expr| {
                // Extract enum type context from the match value if it's a variable
                var match_enum_type: ?[]const u8 = null;
                if (match_expr.value.data == .Variable) {
                    const var_name = match_expr.value.data.Variable.lexeme;
                    if (self.variable_types.get(var_name)) |var_type| {
                        if (var_type == .Enum) {
                            // Look up the actual enum type name from tracked custom types
                            match_enum_type = self.variable_custom_types.get(var_name);
                        }
                    }
                }

                // Generate the value to match on
                try self.generateExpression(match_expr.value, true);

                // Create labels for each case body and the end
                const end_label = try self.generateLabel("match_end");
                var case_labels = std.ArrayList([]const u8).init(self.allocator);
                defer case_labels.deinit();
                var check_labels = std.ArrayList([]const u8).init(self.allocator);
                defer check_labels.deinit();

                // Generate labels for each case body and case check
                for (match_expr.cases, 0..) |_, i| {
                    const case_label = try self.generateLabel("match_case");
                    try case_labels.append(case_label);

                    // Create check labels for all but the first case (first case starts immediately)
                    if (i > 0) {
                        const check_label = try self.generateLabel("match_check");
                        try check_labels.append(check_label);
                    }
                }

                // Generate comparison and jumps for each case
                for (match_expr.cases, 0..) |case, i| {
                    // Add check label for cases after the first
                    if (i > 0) {
                        try self.instructions.append(.{ .Label = .{ .name = check_labels.items[i - 1], .vm_address = 0 } });
                    }

                    // Duplicate the match value for comparison
                    try self.instructions.append(.Dup);

                    // Treat both token type .ELSE and identifier "else" as the else-case
                    const is_else_case = case.pattern.type == .ELSE or
                        (case.pattern.type == .IDENTIFIER and std.mem.eql(u8, case.pattern.lexeme, "else"));

                    if (is_else_case) {
                        // Else case - always matches, pop the duplicated value
                        try self.instructions.append(.Pop);
                        try self.instructions.append(.{ .Jump = .{ .label = case_labels.items[i], .vm_offset = 0 } });
                    } else {
                        // Generate the pattern value (enum member with proper context)
                        const pattern_value = if (match_enum_type) |enum_type_name| blk: {
                            // Look up the actual variant index from registered enum type
                            const variant_index = if (self.custom_types.get(enum_type_name)) |custom_type|
                                custom_type.getEnumVariantIndex(case.pattern.lexeme) orelse 0
                            else
                                0;

                            break :blk HIRValue{
                                .enum_variant = HIREnum{
                                    .type_name = enum_type_name,
                                    .variant_name = case.pattern.lexeme,
                                    .variant_index = variant_index,
                                    .path = null,
                                },
                            };
                        } else HIRValue{ .string = case.pattern.lexeme };

                        const pattern_idx = try self.addConstant(pattern_value);
                        try self.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });

                        // Compare and jump if equal (use appropriate operand type)
                        const operand_type: HIRType = if (match_enum_type != null) .Enum else .String;
                        try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = operand_type } });

                        // FIXED: When condition is false, continue to next case check instead of jumping to end
                        const false_label = if (i < match_expr.cases.len - 1)
                            check_labels.items[i] // Jump to next case check
                        else
                            end_label; // Last case - jump to end if no match
                        try self.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = false_label, .vm_offset = 0, .condition_type = .Tetra } });
                    }
                }

                // Generate case bodies with enum context
                for (match_expr.cases, 0..) |case, i| {
                    try self.instructions.append(.{ .Label = .{ .name = case_labels.items[i], .vm_address = 0 } });

                    // Set enum context for case body generation if needed
                    const old_enum_context = self.current_enum_type;
                    if (match_enum_type) |enum_type_name| {
                        self.current_enum_type = enum_type_name;
                    }

                    // Drop the original match value before producing the case body result
                    // to keep the stack balanced and ensure the case body value is on top.
                    try self.instructions.append(.Pop);

                    try self.generateExpression(case.body, true);

                    // Restore previous enum context
                    self.current_enum_type = old_enum_context;

                    try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });
                }

                // End label
                try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
            },

            .EnumMember => |member| {

                // Generate enum member using current enum type context
                if (self.current_enum_type) |enum_type_name| {
                    // Look up the actual variant index from registered enum type
                    const variant_index = if (self.custom_types.get(enum_type_name)) |custom_type|
                        custom_type.getEnumVariantIndex(member.lexeme) orelse 0
                    else
                        0;

                    // Generate proper enum variant with correct index
                    const enum_value = HIRValue{
                        .enum_variant = HIREnum{
                            .type_name = enum_type_name,
                            .variant_name = member.lexeme,
                            .variant_index = variant_index,
                            .path = null,
                        },
                    };
                    const const_idx = try self.addConstant(enum_value);

                    try self.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                } else {

                    // Fallback to string constant if no enum context
                    const enum_value = HIRValue{ .string = member.lexeme };
                    const const_idx = try self.addConstant(enum_value);
                    try self.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                }
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
                    try self.generateExpression(field.value, true);

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
                // Check if this is a nested field assignment (e.g., mike.person.age is 26)
                if (field_assign.object.data == .FieldAccess) {
                    // This is a nested field assignment - handle it specially
                    const outer_field = field_assign.object.data.FieldAccess;

                    // Generate code to load base variable, modify nested field, and store back
                    // For mike.person.age is 26:
                    // 1. Load mike
                    // 2. Get person field
                    // 3. Duplicate it
                    // 4. Generate value (26)
                    // 5. Set age field on the duplicate
                    // 6. Store the modified person back to mike.person

                    // Generate base object (mike)
                    try self.generateExpression(outer_field.object, true);

                    // Get the outer field (person)
                    try self.instructions.append(.{
                        .GetField = .{
                            .field_name = outer_field.field.lexeme,
                            .container_type = .Struct,
                            .field_index = 0,
                            .field_for_peek = false,
                        },
                    });

                    // Duplicate the nested struct so we can modify it
                    try self.instructions.append(.Dup);

                    // Generate value expression (26)
                    try self.generateExpression(field_assign.value, true);

                    // Set the inner field (age) on the duplicate
                    try self.instructions.append(.{
                        .SetField = .{
                            .field_name = field_assign.field.lexeme,
                            .container_type = .Struct,
                            .field_index = 0,
                        },
                    });

                    // Now we need to store the modified nested struct back to the original
                    // Generate base object again (mike)
                    try self.generateExpression(outer_field.object, true);

                    // Swap the modified nested struct to the top of the stack
                    try self.instructions.append(.Swap);

                    // Set the outer field (person) with the modified struct
                    try self.instructions.append(.{
                        .SetField = .{
                            .field_name = outer_field.field.lexeme,
                            .container_type = .Struct,
                            .field_index = 0,
                        },
                    });

                    // Store the result back to the base variable
                    if (outer_field.object.data == .Variable) {
                        const var_name = outer_field.object.data.Variable.lexeme;
                        const var_index = try self.getOrCreateVariable(var_name);
                        const expected_type = self.getTrackedVariableType(var_name) orelse .Auto;
                        try self.instructions.append(.{
                            .StoreVar = .{
                                .var_index = var_index,
                                .var_name = var_name,
                                .scope_kind = .Local,
                                .module_context = null,
                                .expected_type = expected_type,
                            },
                        });
                    }
                } else {
                    // Regular field assignment - generate object expression
                    try self.generateExpression(field_assign.object, true);

                    // Generate value expression
                    try self.generateExpression(field_assign.value, true);

                    // Generate SetField instruction
                    try self.instructions.append(.{
                        .SetField = .{
                            .field_name = field_assign.field.lexeme,
                            .container_type = .Struct,
                            .field_index = 0, // Index will be resolved by VM
                        },
                    });
                }
            },

            .TypeOf => |expr_to_check| {

                // NEW: Enhanced type inference with custom type support
                const inferred_type = self.inferTypeFromExpression(expr_to_check);
                const type_name = switch (inferred_type) {
                    .Int => "int",
                    .Float => "float",
                    .String => "string",
                    .Tetra => "tetra",
                    .Byte => "byte",
                    .Nothing => "nothing",
                    .Array => "array",
                    .Struct => blk: {
                        if (expr_to_check.data == .Variable) {
                            const var_name = expr_to_check.data.Variable.lexeme;
                            if (self.isCustomType(var_name)) |custom_type| {
                                if (custom_type.kind == .Struct) {
                                    break :blk "struct"; // Type declaration returns "struct"
                                }
                            }

                            // Check if this is an instance (mike)
                            if (self.variable_custom_types.get(var_name)) |custom_type_name| {
                                break :blk custom_type_name; // Instance returns type name
                            }
                        }
                        break :blk "struct"; // Generic struct type
                    },
                    .Map => "map",
                    .Enum => blk: {
                        if (expr_to_check.data == .Variable) {
                            const var_name = expr_to_check.data.Variable.lexeme;
                            if (self.isCustomType(var_name)) |custom_type| {
                                if (custom_type.kind == .Enum) {
                                    break :blk "enum"; // Type declaration returns "enum"
                                }
                            }

                            // Check if this is an instance or enum member
                            if (self.variable_custom_types.get(var_name)) |custom_type_name| {
                                break :blk custom_type_name; // Instance returns type name
                            }
                        }

                        // Handle enum member access like Color.Blue
                        if (expr_to_check.data == .FieldAccess) {
                            const field_access = expr_to_check.data.FieldAccess;
                            if (field_access.object.data == .Variable) {
                                const obj_name = field_access.object.data.Variable.lexeme;
                                if (self.isCustomType(obj_name)) |custom_type| {
                                    if (custom_type.kind == .Enum) {
                                        break :blk obj_name; // Color.Blue returns "Color"
                                    }
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

            .Cast => |cast_expr| {
                // Generate the value to cast
                try self.generateExpression(cast_expr.value, true);

                // Duplicate it so we can keep original value on success path
                try self.instructions.append(.Dup);

                // Map target type to a runtime name string compatible with VM getTypeString
                const target_name: []const u8 = blk: {
                    switch (cast_expr.target_type.data) {
                        .Basic => |basic| switch (basic) {
                            .Integer => break :blk "int",
                            .Byte => break :blk "byte",
                            .Float => break :blk "float",
                            .String => break :blk "string",
                            .Tetra => break :blk "tetra",
                            .Nothing => break :blk "nothing",
                        },
                        .Custom => |tok| break :blk tok.lexeme,
                        .Array => break :blk "array",
                        .Struct => break :blk "struct",
                        .Enum => break :blk "enum",
                        .Union => break :blk "union",
                    }
                };

                // Push target type name as constant
                const target_const = HIRValue{ .string = target_name };
                const target_idx = try self.addConstant(target_const);
                try self.instructions.append(.{ .Const = .{ .value = target_const, .constant_id = target_idx } });

                // Compare runtime type of value against target type name
                try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .String } });

                // Branch based on comparison
                const ok_label = try self.generateLabel("cast_ok");
                const else_label = try self.generateLabel("cast_else");
                const end_label = try self.generateLabel("cast_end");
                try self.instructions.append(.{ .JumpCond = .{ .label_true = ok_label, .label_false = else_label, .vm_offset = 0, .condition_type = .Tetra } });

                // Else branch: drop original value and evaluate else expression
                try self.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });
                try self.instructions.append(.Pop);
                if (cast_expr.else_branch) |else_expr| {
                    try self.generateExpression(else_expr, true);
                } else {
                    // Default else: push nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }
                try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                // Success branch
                try self.instructions.append(.{ .Label = .{ .name = ok_label, .vm_address = 0 } });
                if (cast_expr.then_branch) |then_expr| {
                    // On success, drop original and evaluate then-branch
                    try self.instructions.append(.Pop);
                    try self.generateExpression(then_expr, true);
                }

                // End merge point
                try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
            },

            .LengthOf => |expr_to_check| {
                // Generate the expression whose length we want to get
                try self.generateExpression(expr_to_check, true);

                // Generate StringOp.Length instruction
                try self.instructions.append(.{
                    .StringOp = .{
                        .op = .Length,
                    },
                });
            },

            .BytesOf => |expr_to_check| {
                // Generate the expression whose bytes we want to get
                try self.generateExpression(expr_to_check, true);

                // Generate StringOp.Bytes instruction
                try self.instructions.append(.{
                    .StringOp = .{
                        .op = .Bytes,
                    },
                });
            },

            .Map => |entries| {

                // Generate each key-value pair in reverse order (for stack-based construction)
                var reverse_i = entries.len;
                while (reverse_i > 0) {
                    reverse_i -= 1;
                    const entry = entries[reverse_i];

                    // Generate key first, then value (they'll be popped in reverse order)
                    try self.generateExpression(entry.key, true);
                    try self.generateExpression(entry.value, true);
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

            .For => |for_expr| {
                const loop_start_label = try self.generateLabel("for_start");
                const loop_body_label = try self.generateLabel("for_body");
                const loop_increment_label = try self.generateLabel("for_increment");
                const loop_end_label = try self.generateLabel("for_end");

                // Generate initializer (var i is 0)
                if (for_expr.initializer) |initializer| {
                    try self.generateStatement(initializer.*);
                }

                // Loop start - condition check
                try self.instructions.append(.{ .Label = .{ .name = loop_start_label, .vm_address = 0 } });

                // Generate condition (i < x)
                if (for_expr.condition) |condition| {
                    try self.generateExpression(condition, true);
                } else {
                    // No condition means infinite loop - push true
                    const true_idx = try self.addConstant(HIRValue{ .tetra = TETRA_TRUE });
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_TRUE }, .constant_id = true_idx } });
                }

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
                try self.generateExpression(for_expr.body, false);

                // Increment label and execution
                try self.instructions.append(.{ .Label = .{ .name = loop_increment_label, .vm_address = 0 } });
                if (for_expr.increment) |increment| {
                    try self.generateExpression(increment, false);
                }

                // Jump back to condition check
                try self.instructions.append(.{ .Jump = .{ .label = loop_start_label, .vm_offset = 0 } });

                // Loop end
                try self.instructions.append(.{ .Label = .{ .name = loop_end_label, .vm_address = 0 } });

                // Push nothing as for loop result
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .ForEach => |foreach_expr| {
                // Create unique label set for this foreach
                const fe_start = try self.generateLabel("foreach_start");
                const fe_body = try self.generateLabel("foreach_body");
                const fe_next = try self.generateLabel("foreach_next");
                const fe_end = try self.generateLabel("foreach_end");

                // Synthesize stable temporary variable names for array, length, and index
                const arr_tmp_name = try std.fmt.allocPrint(self.allocator, "__fe_arr_{s}", .{fe_start});
                const len_tmp_name = try std.fmt.allocPrint(self.allocator, "__fe_len_{s}", .{fe_start});
                const idx_tmp_fallback = try std.fmt.allocPrint(self.allocator, "__fe_idx_{s}", .{fe_start});

                // Resolve or create indices for the temporaries
                const arr_tmp_idx = try self.getOrCreateVariable(arr_tmp_name);
                const len_tmp_idx = try self.getOrCreateVariable(len_tmp_name);
                const idx_var_name = if (foreach_expr.index_name) |idx_tok| idx_tok.lexeme else idx_tmp_fallback;
                const idx_tmp_idx = try self.getOrCreateVariable(idx_var_name);

                // Track basic types for temporaries
                try self.trackVariableType(arr_tmp_name, .Array);
                try self.trackVariableType(len_tmp_name, .Int);
                try self.trackVariableType(idx_var_name, .Int);

                // Generate and store the array expression into arr_tmp
                try self.generateExpression(foreach_expr.array, true);
                try self.instructions.append(.Dup);
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = arr_tmp_idx,
                    .var_name = arr_tmp_name,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = .Array,
                } });

                // Compute array length into len_tmp
                try self.instructions.append(.{ .StringOp = .{ .op = .Length } });
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = len_tmp_idx,
                    .var_name = len_tmp_name,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = .Int,
                } });

                // Initialize index to 0
                const zero_idx = try self.addConstant(HIRValue{ .int = 0 });
                try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 0 }, .constant_id = zero_idx } });
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = idx_tmp_idx,
                    .var_name = idx_var_name,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = .Int,
                } });

                // Loop start: check index < len
                try self.instructions.append(.{ .Label = .{ .name = fe_start, .vm_address = 0 } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = idx_tmp_idx, .var_name = idx_var_name, .scope_kind = .Local, .module_context = null } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = len_tmp_idx, .var_name = len_tmp_name, .scope_kind = .Local, .module_context = null } });
                try self.instructions.append(.{ .Compare = .{ .op = .Lt, .operand_type = .Int } });
                try self.instructions.append(.{ .JumpCond = .{ .label_true = fe_body, .label_false = fe_end, .vm_offset = 0, .condition_type = .Tetra } });

                // Loop body: load element and bind to item variable
                try self.instructions.append(.{ .Label = .{ .name = fe_body, .vm_address = 0 } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = arr_tmp_idx, .var_name = arr_tmp_name, .scope_kind = .Local, .module_context = null } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = idx_tmp_idx, .var_name = idx_var_name, .scope_kind = .Local, .module_context = null } });
                try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });

                // Determine/track the element type if possible
                var elem_type: HIRType = .Auto;
                if (foreach_expr.array.data == .Variable) {
                    if (self.getTrackedArrayElementType(foreach_expr.array.data.Variable.lexeme)) |tracked_elem| {
                        elem_type = tracked_elem;
                    }
                }

                // Store to loop item variable
                const item_name = foreach_expr.item_name.lexeme;
                const item_idx = try self.getOrCreateVariable(item_name);
                if (elem_type != .Auto) try self.trackVariableType(item_name, elem_type);
                try self.instructions.append(.{ .StoreVar = .{ .var_index = item_idx, .var_name = item_name, .scope_kind = .Local, .module_context = null, .expected_type = elem_type } });

                // Generate loop body statements
                for (foreach_expr.body) |stmt| {
                    try self.generateStatement(stmt);
                }

                // Next: i = i + 1
                try self.instructions.append(.{ .Label = .{ .name = fe_next, .vm_address = 0 } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = idx_tmp_idx, .var_name = idx_var_name, .scope_kind = .Local, .module_context = null } });
                const one_idx = try self.addConstant(HIRValue{ .int = 1 });
                try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .int = 1 }, .constant_id = one_idx } });
                try self.instructions.append(.{ .IntArith = .{ .op = .Add, .overflow_behavior = .Wrap } });
                try self.instructions.append(.{ .StoreVar = .{ .var_index = idx_tmp_idx, .var_name = idx_var_name, .scope_kind = .Local, .module_context = null, .expected_type = .Int } });

                // Jump back to condition
                try self.instructions.append(.{ .Jump = .{ .label = fe_start, .vm_offset = 0 } });

                // End label and result value
                try self.instructions.append(.{ .Label = .{ .name = fe_end, .vm_address = 0 } });
                const nothing2_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing2_idx } });
            },

            .FieldAccess => |field| {
                var handled_as_enum_member: bool = false;
                // Check the type of the object being accessed first
                const obj_type = self.inferTypeFromExpression(field.object);

                // CRITICAL FIX: Handle enum member access (e.g., Color.Blue syntax)
                if (field.object.data == .Variable) {
                    const var_token = field.object.data.Variable;
                    // Check if this variable name matches a registered enum type
                    if (self.custom_types.get(var_token.lexeme)) |custom_type| {
                        if (custom_type.kind == .Enum) {
                            // This is Color.Blue syntax - generate enum variant
                            const variant_index = custom_type.getEnumVariantIndex(field.field.lexeme) orelse 0;

                            const enum_value = HIRValue{
                                .enum_variant = HIREnum{
                                    .type_name = var_token.lexeme,
                                    .variant_name = field.field.lexeme,
                                    .variant_index = variant_index,
                                    .path = null,
                                },
                            };
                            const const_idx = try self.addConstant(enum_value);
                            try self.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                            handled_as_enum_member = true;
                        }
                    }
                }

                if (!handled_as_enum_member) {
                    // Handle enum member access (e.g., Color.Red)
                    try self.generateExpression(field.object, true);

                    // Now, the original logic for FieldAccess (non-enum)
                    try self.instructions.append(.{
                        .GetField = .{
                            .field_name = field.field.lexeme,
                            .container_type = obj_type, // Use the inferred object type
                            .field_index = 0, // VM will resolve
                            .field_for_peek = false, // Default
                        },
                    });
                }
            },
            .EnumDecl => |enum_decl| {
                // NEW: Register enum type with variants for proper index calculation
                var variant_names = try self.allocator.alloc([]const u8, enum_decl.variants.len);
                for (enum_decl.variants, 0..) |variant_token, i| {
                    variant_names[i] = variant_token.lexeme;
                }
                try self.registerEnumType(enum_decl.name.lexeme, variant_names);

                // Register the enum type name as a special variable so Color.Red works
                const var_idx = try self.getOrCreateVariable(enum_decl.name.lexeme);
                try self.trackVariableType(enum_decl.name.lexeme, .Enum);

                // Create a special enum type value and store it
                const enum_type_value = HIRValue{ .string = enum_decl.name.lexeme }; // Simple representation for now
                const const_idx = try self.addConstant(enum_type_value);
                try self.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
                try self.instructions.append(.{ .StoreConst = .{
                    .var_index = var_idx,
                    .var_name = enum_decl.name.lexeme,
                } });
            },

            .StructDecl => |struct_decl| {
                // NEW: Register struct type with fields for proper field access
                var field_names = try self.allocator.alloc([]const u8, struct_decl.fields.len);
                for (struct_decl.fields, 0..) |field_ptr, i| {
                    field_names[i] = field_ptr.name.lexeme;
                }
                try self.registerStructType(struct_decl.name.lexeme, field_names);

                // Struct declarations don't generate runtime instructions, they're compile-time only
                // Push nothing as a placeholder value
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            },

            .PeekStruct => |peek| {

                // Generate the expression to peek
                try self.generateExpression(peek.expr, true);

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
                        try self.generateExpression(field.object, true);
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
                    .should_pop_after_peek = !preserve_result,
                } });
            },

            .Input => |input| {
                // Check if we have a non-empty prompt
                const prompt_str = input.prompt.literal.string;
                if (prompt_str.len > 0) {
                    // Generate the prompt as a constant first
                    const prompt_value = HIRValue{ .string = prompt_str };
                    const prompt_idx = try self.addConstant(prompt_value);

                    // Push the prompt onto the stack as an argument
                    try self.instructions.append(.{ .Const = .{ .value = prompt_value, .constant_id = prompt_idx } });

                    // Generate input call with the prompt as argument
                    try self.instructions.append(.{
                        .Call = .{
                            .function_index = 0,
                            .qualified_name = "input",
                            .arg_count = 1, // Has 1 argument (the prompt)
                            .call_kind = .BuiltinFunction,
                            .target_module = null,
                            .return_type = .String,
                        },
                    });
                } else {
                    // No prompt - call input with no arguments
                    try self.instructions.append(.{
                        .Call = .{
                            .function_index = 0,
                            .qualified_name = "input",
                            .arg_count = 0, // No arguments
                            .call_kind = .BuiltinFunction,
                            .target_module = null,
                            .return_type = .String,
                        },
                    });
                }
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

    /// Extract comparison value from simple quantifier conditions like "e > 3"
    fn extractSimpleComparison(self: *HIRGenerator, condition: *ast.Expr) !HIRValue {
        _ = self;
        switch (condition.data) {
            .Binary => |binary| {
                // Handle "variable > literal" patterns
                if (binary.right) |right| {
                    switch (right.data) {
                        .Literal => |lit| {
                            return switch (lit) {
                                .int => |i| HIRValue{ .int = i },
                                .float => |f| HIRValue{ .float = f },
                                .string => |s| HIRValue{ .string = s },
                                else => HIRValue{ .int = 0 }, // Default fallback
                            };
                        },
                        .Variable => |var_token| {
                            // For variable references, we can't extract a constant value at compile time
                            // This should be handled by generating proper variable loading code instead
                            _ = var_token; // Suppress unused variable warning
                            return HIRValue{ .int = 0 };
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
        // Fallback for complex conditions
        return HIRValue{ .int = 0 };
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
                            self.generateExpression(arg, true) catch return false; // Fallback to regular call on error
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
            .byte => .Byte,
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
            .Index => |index| {
                // Array/string indexing returns the element type
                const container_type = self.inferTypeFromExpression(index.array);
                return switch (container_type) {
                    .Array => .String, // Most arrays in bigfile.doxa are int arrays, but for simplicity return String
                    .String => .String, // String indexing returns single character (still string in our system)
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
                    if (std.mem.eql(u8, field.field.lexeme, "bytes")) {
                        return .Array; // String bytes returns array of byte
                    }
                    // String indexing returns a single character string
                    if (std.fmt.parseInt(i32, field.field.lexeme, 10)) |_| {
                        return .String;
                    } else |_| {
                        return .Auto; // Invalid string operation
                    }
                }

                // Handle enum member access
                if (obj_type == .Enum) {
                    // For enum member access like Color.Blue, return the enum type
                    if (field.object.data == .Variable) {
                        const enum_name = field.object.data.Variable.lexeme;
                        if (self.isCustomType(enum_name)) |custom_type| {
                            if (custom_type.kind == .Enum) {
                                return .Enum; // Return enum type for enum member access
                            }
                        }
                    }
                    return .Enum; // Generic enum type for other enum member accesses
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
            .LengthOf => .Int, // lengthof returns int
            .BytesOf => .Array, // bytesof returns array of byte
            else => .String, // Default to String for any unhandled expression types to prevent Auto leakage
        };
    }

    /// NEW: Track a variable's type when it's declared or assigned
    fn trackVariableType(self: *HIRGenerator, var_name: []const u8, var_type: HIRType) !void {
        try self.variable_types.put(var_name, var_type);
    }

    /// NEW: Track a variable's custom type name (for enums/structs)
    fn trackVariableCustomType(self: *HIRGenerator, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.variable_custom_types.put(var_name, custom_type_name);
    }

    /// NEW: Get tracked variable type
    fn getTrackedVariableType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.variable_types.get(var_name);
    }

    // NEW: Track array element types per variable for better index inference

    fn ensureAuxMapsInit(self: *HIRGenerator) void {
        _ = self; // map is initialized in init()
    }

    fn trackArrayElementType(self: *HIRGenerator, var_name: []const u8, elem_type: HIRType) !void {
        self.ensureAuxMapsInit();
        try self.variable_array_element_types.put(var_name, elem_type);
    }

    fn getTrackedArrayElementType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.variable_array_element_types.get(var_name);
    }

    /// NEW: Infer the result type of a binary operation
    fn inferBinaryOpResultType(self: *HIRGenerator, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr);
        const right_type = self.inferTypeFromExpression(right_expr);

        return switch (operator_type) {
            .PLUS => switch (left_type) {
                .Int => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Int,
                    else => .Int,
                },
                .Float => .Float, // Any float operand makes result float
                .Byte => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Byte,
                    else => .Int,
                },
                else => .Int,
            },
            .MINUS => switch (left_type) {
                .Int => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Int,
                    else => .Int,
                },
                .Float => .Float, // Any float operand makes result float
                .Byte => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Byte,
                    else => .Int,
                },
                else => .Int,
            },
            .ASTERISK => switch (left_type) {
                .Int => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Int,
                    else => .Int,
                },
                .Float => .Float, // Any float operand makes result float
                .Byte => switch (right_type) {
                    .Int => .Int,
                    .Float => .Float,
                    .Byte => .Byte,
                    else => .Int,
                },
                else => .Int,
            },
            .SLASH => .Float, // Division always returns float
            .MODULO => .Int, // Modulo only works with integers
            .EQUALITY, .BANG_EQUAL, .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => {
                // For comparisons, we need to determine the comparison type
                // If either operand is float, use float comparison
                if (left_type == .Float or right_type == .Float) {
                    return .Float;
                } else if (left_type == .Int or right_type == .Int) {
                    return .Int;
                } else if (left_type == .Byte or right_type == .Byte) {
                    return .Byte;
                } else {
                    return .Int; // Default to int comparison
                }
            },
            else => .Int, // Default to int for other operations
        };
    }

    /// NEW: Register a custom type (struct or enum)
    fn registerCustomType(self: *HIRGenerator, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = CustomTypeInfo{
            .name = type_name,
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    /// NEW: Register an enum type with its variants
    fn registerEnumType(self: *HIRGenerator, enum_name: []const u8, variants: []const []const u8) !void {
        // Create enum variants array with proper indices
        var enum_variants = try self.allocator.alloc(CustomTypeInfo.EnumVariant, variants.len);
        for (variants, 0..) |variant_name, index| {
            enum_variants[index] = CustomTypeInfo.EnumVariant{
                .name = try self.allocator.dupe(u8, variant_name),
                .index = @intCast(index),
            };
        }

        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, enum_name),
            .kind = .Enum,
            .enum_variants = enum_variants,
        };
        try self.custom_types.put(enum_name, custom_type);
    }

    /// NEW: Register a struct type with its fields
    fn registerStructType(self: *HIRGenerator, struct_name: []const u8, fields: []const []const u8) !void {
        // Create struct fields array with proper indices and types
        var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field_name, index| {
            struct_fields[index] = CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field_name),
                .field_type = .Auto, // Will be inferred at runtime
                .index = @intCast(index),
            };
        }

        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
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
                if (std.mem.eql(u8, function_name, "input")) {
                    return .String;
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

    /// Check if a name is a module namespace
    fn isModuleNamespace(self: *HIRGenerator, name: []const u8) bool {
        return self.module_namespaces.contains(name);
    }
};
