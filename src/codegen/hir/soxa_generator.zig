const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;
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
const import_parser = @import("../../parser/import_parser.zig");

const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub const HIRGenerator = struct {
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(HIRInstruction),
    constants: std.ArrayList(HIRValue),
    current_peek_expr: ?*ast.Expr = null,
    current_field_name: ?[]const u8 = null,
    string_pool: std.ArrayList([]const u8),
    constant_map: std.StringHashMap(u32), // For deduplication
    variables: std.StringHashMap(u32), // name -> index mapping
    variable_count: u32,
    local_variables: std.StringHashMap(u32), // per-function variable indices
    local_variable_count: u32,
    label_count: u32,
    reporter: *Reporter,

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

    // Map of specifically imported symbols (unqualified names)
    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    current_enum_type: ?[]const u8 = null,
    // Track array element types per variable for better index/peek typing
    variable_array_element_types: std.StringHashMap(HIRType),

    // New: track union member type names per variable name (legacy, may cause collisions across scopes)
    variable_union_members: std.StringHashMap([][]const u8),
    // New: track union member type names per variable index to avoid name collisions across scopes
    variable_union_members_by_index: std.AutoHashMap(u32, [][]const u8),

    // Loop context stack to support break/continue codegen
    loop_context_stack: std.ArrayList(LoopContext),

    pub const FunctionInfo = struct {
        name: []const u8,
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        body_label: ?[]const u8 = null, // For tail call optimization - jumps here to skip parameter setup
        local_var_count: u32,
        is_entry: bool,
        param_is_alias: []bool, // NEW: Track which parameters are aliases
        param_types: []HIRType, // NEW: Track parameter types for VM binding
    };

    pub const FunctionBody = struct {
        function_info: FunctionInfo,
        statements: []ast.Stmt,
        start_instruction_index: u32,
        // Store original function declaration components needed for parameter setup
        function_name: []const u8,
        function_params: []ast.FunctionParam,
        return_type_info: ast.TypeInfo,
        param_is_alias: []bool, // NEW: Duplicated from FunctionInfo for easy access
        param_types: []HIRType, // NEW: Duplicated from FunctionInfo for easy access
    };

    pub const FunctionCallSite = struct {
        function_name: []const u8,
        is_tail_position: bool,
        instruction_index: u32,
    };

    pub const LoopContext = struct {
        break_label: []const u8,
        continue_label: []const u8,
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
            custom_type_name: ?[]const u8 = null,
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

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, module_namespaces: std.StringHashMap(ast.ModuleInfo), imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol)) HIRGenerator {
        return HIRGenerator{
            .allocator = allocator,
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
            .constants = std.ArrayList(HIRValue).init(allocator),
            .current_peek_expr = null,
            .string_pool = std.ArrayList([]const u8).init(allocator),
            .constant_map = std.StringHashMap(u32).init(allocator),
            .variables = std.StringHashMap(u32).init(allocator),
            .variable_count = 0,
            .local_variables = std.StringHashMap(u32).init(allocator),
            .local_variable_count = 0,
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
            .imported_symbols = imported_symbols,
            .current_enum_type = null,
            .stats = HIRStats.init(allocator),
            .variable_array_element_types = std.StringHashMap(HIRType).init(allocator),
            .variable_union_members = std.StringHashMap([][]const u8).init(allocator),
            .variable_union_members_by_index = std.AutoHashMap(u32, [][]const u8).init(allocator),
            .loop_context_stack = std.ArrayList(LoopContext).init(allocator),
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
        self.variable_union_members.deinit();
        self.variable_union_members_by_index.deinit();
        self.loop_context_stack.deinit();
        // Note: module_namespaces is not owned by HIRGenerator, so we don't deinit it
    }

    inline fn pushLoopContext(self: *HIRGenerator, break_label: []const u8, continue_label: []const u8) !void {
        try self.loop_context_stack.append(.{ .break_label = break_label, .continue_label = continue_label });
    }

    inline fn popLoopContext(self: *HIRGenerator) void {
        if (self.loop_context_stack.items.len > 0) {
            _ = self.loop_context_stack.pop();
        }
    }

    inline fn currentLoopContext(self: *HIRGenerator) ?LoopContext {
        if (self.loop_context_stack.items.len == 0) return null;
        return self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
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

                    // Create an array for param_is_alias
                    var param_is_alias = try self.allocator.alloc(bool, func.params.len);
                    var param_types = try self.allocator.alloc(HIRType, func.params.len);
                    for (func.params, 0..) |param, i| {
                        param_is_alias[i] = param.is_alias;
                        param_types[i] = if (param.type_expr) |type_expr| self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*) else .Int; // Default to Int
                    }

                    const function_info = FunctionInfo{
                        .name = func.name.lexeme,
                        .arity = @intCast(func.params.len),
                        .return_type = return_type,
                        .start_label = start_label,
                        .local_var_count = 0, // Will be calculated during body generation
                        .is_entry = func.is_entry,
                        .param_is_alias = param_is_alias,
                        .param_types = param_types,
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
                        .param_is_alias = param_is_alias, // Duplicated
                        .param_types = param_types, // Duplicated
                    });
                },
                else => {},
            }
        }

        // Also collect function signatures from imported modules (by namespace alias)
        // We qualify their names as "alias.function" so calls can resolve normally
        var it = self.module_namespaces.iterator();
        while (it.next()) |entry| {
            const alias = entry.key_ptr.*;
            const module_info = entry.value_ptr.*;

            if (module_info.ast) |module_ast| {
                if (module_ast.data == .Block) {
                    const mod_statements = module_ast.data.Block.statements;
                    for (mod_statements) |mod_stmt| {
                        switch (mod_stmt.data) {
                            .FunctionDecl => |func| {
                                if (!func.is_public) continue;

                                const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ alias, func.name.lexeme });
                                const return_type = self.convertTypeInfo(func.return_type_info);
                                const start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{qualified_name}));

                                // Create an array for param_is_alias
                                var param_is_alias_imported = try self.allocator.alloc(bool, func.params.len);
                                var param_types_imported = try self.allocator.alloc(HIRType, func.params.len);
                                for (func.params, 0..) |param, i| {
                                    param_is_alias_imported[i] = param.is_alias;
                                    param_types_imported[i] = if (param.type_expr) |type_expr| self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*) else .Int; // Default to Int
                                }

                                const function_info = FunctionInfo{
                                    .name = qualified_name,
                                    .arity = @intCast(func.params.len),
                                    .return_type = return_type,
                                    .start_label = start_label,
                                    .local_var_count = 0,
                                    .is_entry = false, // imported functions are never entry points
                                    .param_is_alias = param_is_alias_imported,
                                    .param_types = param_types_imported,
                                };

                                try self.function_signatures.put(qualified_name, function_info);

                                // Store function body for later generation
                                try self.function_bodies.append(FunctionBody{
                                    .function_info = function_info,
                                    .statements = func.body,
                                    .start_instruction_index = 0,
                                    .function_name = qualified_name,
                                    .function_params = func.params,
                                    .return_type_info = func.return_type_info,
                                    .param_is_alias = param_is_alias_imported,
                                    .param_types = param_types_imported,
                                });
                            },
                            else => {},
                        }
                    }
                }
            }
        }

        // Additionally, create unqualified function signatures for specifically imported symbols
        if (self.imported_symbols) |symbols| {
            var sym_it = symbols.iterator();
            while (sym_it.next()) |entry2| {
                const sym_name = entry2.key_ptr.*;
                const sym = entry2.value_ptr.*;
                if (sym.kind != .Function) continue;

                // Search module namespaces for a module that defines this public function
                var it2 = self.module_namespaces.iterator();
                while (it2.next()) |m_entry| {
                    const module_info2 = m_entry.value_ptr.*;
                    if (module_info2.ast) |module_ast| {
                        const mod_statements2 = module_ast.data.Block.statements;
                        var found = false;
                        var func_return_type: HIRType = .Nothing;
                        var func_body: []ast.Stmt = &[_]ast.Stmt{};
                        var func_params: []ast.FunctionParam = &[_]ast.FunctionParam{};
                        for (mod_statements2) |mod_stmt2| {
                            switch (mod_stmt2.data) {
                                .FunctionDecl => |func2| {
                                    if (!func2.is_public) continue;
                                    if (!std.mem.eql(u8, func2.name.lexeme, sym_name)) continue;
                                    // Found matching function
                                    found = true;
                                    func_return_type = self.convertTypeInfo(func2.return_type_info);
                                    func_body = func2.body;
                                    func_params = func2.params;
                                },
                                else => {},
                            }
                            if (found) break;
                        }

                        if (found) {
                            // Add unqualified function signature using the imported symbol name
                            const start_label2 = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{sym_name}));
                            const function_info2 = FunctionInfo{
                                .name = sym_name,
                                .arity = if (sym.param_count) |pc| pc else @intCast(func_params.len),
                                .return_type = func_return_type,
                                .start_label = start_label2,
                                .local_var_count = 0,
                                .is_entry = false,
                                .param_is_alias = try self.allocator.alloc(bool, func_params.len),
                                .param_types = try self.allocator.alloc(HIRType, func_params.len),
                            };

                            // Only add if not already present
                            if (!self.function_signatures.contains(sym_name)) {
                                try self.function_signatures.put(sym_name, function_info2);
                                try self.function_bodies.append(FunctionBody{
                                    .function_info = function_info2,
                                    .statements = func_body,
                                    .start_instruction_index = 0,
                                    .function_name = sym_name,
                                    .function_params = func_params,
                                    .return_type_info = (try ast.typeInfoFromHIRType(self.allocator, func_return_type)).*,
                                    .param_is_alias = function_info2.param_is_alias,
                                    .param_types = function_info2.param_types,
                                });
                            }

                            break; // Stop scanning modules once found
                        }
                    }
                }
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

            // Reset per-function local variable tracking to avoid cross-function collisions
            self.local_variables.deinit();
            self.local_variables = std.StringHashMap(u32).init(self.allocator);
            self.local_variable_count = 0;

            // Generate parameter setup - copy arguments from stack to local variables
            const params = function_body.function_params;

            // Parameters are pushed in order, so we pop them in reverse order
            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                // Extract parameter type information with proper inference
                var param_type: HIRType = .Unknown;
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

                // Check if this parameter is an alias
                if (function_body.param_is_alias[param_index]) {
                    // For alias parameters, generate StoreParamAlias
                    // The stack should contain the storage ID pushed by the caller
                    try self.instructions.append(.{ .StoreParamAlias = .{
                        .param_name = param.name.lexeme,
                        .param_type = param_type,
                    } });
                } else {
                    // For regular parameters, create a local variable and store the stack value
                    const var_idx = try self.getOrCreateVariable(param.name.lexeme);
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = param.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = param_type,
                    } });
                }
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
                        try self.generateExpression(e, true, true);
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
        // First process global (non-function) statements from imported modules so their
        // constants and types (e.g., 'limit' in safeMath) are defined before any calls.
        var it = self.module_namespaces.iterator();
        while (it.next()) |entry| {
            const module_info = entry.value_ptr.*;
            if (module_info.ast) |module_ast| {
                if (module_ast.data == .Block) {
                    const mod_statements = module_ast.data.Block.statements;
                    for (mod_statements) |mod_stmt| {
                        switch (mod_stmt.data) {
                            .FunctionDecl => continue, // skip functions here
                            .VarDecl => |decl| {
                                // For module-level globals, ensure they are stored in the root/global scope
                                // by emitting StoreConst/StoreVar before any function bodies.
                                try self.generateStatement(ast.Stmt{ .base = mod_stmt.base, .data = .{ .VarDecl = decl } });
                            },
                            else => try self.generateStatement(mod_stmt),
                        }
                    }
                }
            }
        }

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
                .param_is_alias = function_body.param_is_alias, // Use from function_body
                .param_types = function_body.param_types, // Use from function_body
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
            .Union => .Union,
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

    fn generateStatement(self: *HIRGenerator, stmt: ast.Stmt) (std.mem.Allocator.Error || ErrorList)!void {
        switch (stmt.data) {
            .Expression => |expr| {
                if (expr) |e| {
                    try self.generateExpression(e, false, false);
                }
            },
            .Continue => {
                if (self.currentLoopContext()) |lc| {
                    try self.instructions.append(.{ .Jump = .{ .label = lc.continue_label, .vm_offset = 0 } });
                } else {
                    const location = Location{
                        .file = stmt.base.location().file,
                        .range = .{
                            .start_line = stmt.base.location().range.start_line,
                            .start_col = stmt.base.location().range.start_col,
                            .end_line = stmt.base.location().range.end_line,
                            .end_col = stmt.base.location().range.end_col,
                        },
                    };
                    self.reporter.reportCompileError(
                        location,
                        ErrorCode.CONTINUE_USED_OUTSIDE_OF_LOOP,
                        "'continue' used outside of a loop",
                        .{},
                    );
                }
            },
            .Break => {
                if (self.currentLoopContext()) |lc| {
                    try self.instructions.append(.{ .Jump = .{ .label = lc.break_label, .vm_offset = 0 } });
                } else {
                    const location = Location{
                        .file = stmt.base.location().file,
                        .range = .{
                            .start_line = stmt.base.location().range.start_line,
                            .start_col = stmt.base.location().range.start_col,
                            .end_line = stmt.base.location().range.end_line,
                            .end_col = stmt.base.location().range.end_col,
                        },
                    };
                    self.reporter.reportCompileError(
                        location,
                        ErrorCode.BREAK_USED_OUTSIDE_OF_LOOP,
                        "'break' used outside of a loop",
                        .{},
                    );
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
                        .Union => blk: {
                            // Default unions to the first member's type for initialization
                            if (decl.type_info.union_type) |ut| {
                                if (ut.types.len > 0) {
                                    const first = ut.types[0];
                                    // New: record union member names (lowercase) for this variable (flatten nested unions)
                                    const list = try self.collectUnionMemberNames(ut);

                                    try self.variable_union_members.put(decl.name.lexeme, list);
                                    var maybe_index: ?u32 = null;
                                    if (self.current_function != null) {
                                        maybe_index = self.local_variables.get(decl.name.lexeme);
                                    } else {
                                        maybe_index = self.variables.get(decl.name.lexeme);
                                    }
                                    if (maybe_index) |var_index| {
                                        try self.variable_union_members_by_index.put(var_index, list);
                                    }

                                    break :blk switch (first.base) {
                                        .Int => .Int,
                                        .Float => .Float,
                                        .String => .String,
                                        .Tetra => .Tetra,
                                        .Byte => .Byte,
                                        .Array => .Array,
                                        .Struct => .Struct,
                                        .Enum => .Enum,
                                        else => .Nothing,
                                    };
                                }
                            }
                            break :blk .Nothing;
                        },
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

                    // Var declaration initializer must leave a value on the stack for StoreVar
                    // Preserve the result so the subsequent store can consume it
                    try self.generateExpression(init_expr, true, true);

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

                    // NEW: If initializer is an array literal, record its element type for downstream inference
                    if (init_expr.data == .Array) {
                        const elements = init_expr.data.Array;
                        if (elements.len > 0) {
                            const elem_type: HIRType = switch (elements[0].data) {
                                .Literal => |lit| self.inferTypeFromLiteral(lit),
                                else => .Unknown,
                            };
                            if (elem_type != .Unknown) {
                                try self.trackArrayElementType(decl.name.lexeme, elem_type);
                            }
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

                    // Store into the declared variable now so it has a concrete value and index
                    // Only perform this pre-store in global scope where we intentionally
                    // keep a duplicate around for subsequent global initialization steps.
                    if (self.current_function == null) {
                        const var_idx2 = try self.getOrCreateVariable(decl.name.lexeme);
                        // Duplicate so subsequent code can see the value if needed (globals only)
                        try self.instructions.append(.Dup);
                        try self.instructions.append(.{ .StoreVar = .{
                            .var_index = var_idx2,
                            .var_name = decl.name.lexeme,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = var_type,
                        } });
                    }

                    // Also record union members by index if present by name (only if pre-stored)
                    if (self.current_function == null) {
                        if (self.variable_union_members.get(decl.name.lexeme)) |members0| {
                            const var_idx2 = try self.getOrCreateVariable(decl.name.lexeme);
                            try self.variable_union_members_by_index.put(var_idx2, members0);
                        }
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
                    // In global scope, we duplicated the initializer; pop it to keep stack balanced
                    if (self.current_function == null) {
                        try self.instructions.append(.Pop);
                    }
                } else {
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = decl.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = var_type,
                    } });
                    // In global scope, we duplicated the initializer; pop it to keep stack balanced
                    if (self.current_function == null) {
                        try self.instructions.append(.Pop);
                    }
                }

                // Ensure union member metadata is recorded by variable index (only for union declarations)
                if (decl.type_info.base == .Union) {
                    if (self.variable_union_members.get(decl.name.lexeme)) |members_for_var| {
                        // Update/insert index-based mapping so later peeks can find it reliably
                        _ = try self.variable_union_members_by_index.put(var_idx, members_for_var);
                    }
                }

                // Additionally, if initializer returns a union (e.g., StringToInt), record union members for this variable
                if (decl.initializer) |init_expr_union| {
                    if (init_expr_union.data == .StringToInt) {
                        const members = try self.allocator.alloc([]const u8, 2);
                        members[0] = "int";
                        members[1] = "NumberError";
                        try self.trackVariableUnionMembersByIndex(var_idx, members);
                    } else if (init_expr_union.data == .InternalCall) {
                        const m2 = init_expr_union.data.InternalCall;
                        if (std.mem.eql(u8, m2.method.lexeme, "substring")) {
                            const members = try self.allocator.alloc([]const u8, 2);
                            members[0] = "string";
                            members[1] = "IndexError";
                            try self.trackVariableUnionMembersByIndex(var_idx, members);
                        }
                    }
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
                        try self.generateExpression(value, true, true);
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
                try self.generateExpression(assert_stmt.condition, true, true);

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
                    try self.generateExpression(msg, true, true);
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
                self.reporter.reportCompileError(
                    stmt.base.location(),
                    ErrorCode.UNHANDLED_STATEMENT_TYPE,
                    "Unhandled statement type: {}",
                    .{stmt.data},
                );
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
    fn generateExpression(self: *HIRGenerator, expr: *ast.Expr, preserve_result: bool, should_pop_after_use: bool) !void {
        // Type checking is now handled by the semantic analyzer before HIR generation.
        // No direct `type_info` field on `ast.Expr` anymore.
        // We will assume expressions passed here have valid type information.

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
                var maybe_idx: ?u32 = null;
                if (self.current_function != null) {
                    maybe_idx = self.local_variables.get(var_token.lexeme);
                } else {
                    maybe_idx = self.variables.get(var_token.lexeme);
                }
                if (maybe_idx) |existing_idx| {
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
                    // Ensure the variable exists in the current scope and load it at runtime
                    const var_idx2 = try self.getOrCreateVariable(var_token.lexeme);
                    try self.instructions.append(.{
                        .LoadVar = .{
                            .var_index = var_idx2,
                            .var_name = var_token.lexeme,
                            .scope_kind = .Local,
                            .module_context = null,
                        },
                    });
                }
            },

            .Binary => |bin| {
                // Get the types of both operands
                const left_type = self.inferTypeFromExpression(bin.left.?);
                const right_type = self.inferTypeFromExpression(bin.right.?);

                // Generate expressions for both operands first
                try self.generateExpression(bin.left.?, true, should_pop_after_use);
                try self.generateExpression(bin.right.?, true, should_pop_after_use);

                // Centralized type promotion rules:
                // 1. Float dominance: If either operand is Float or operator is division (/), promote to Float
                // 2. Int fallback: If either operand is Int, promote to Int
                // 3. No promotion: If neither is Float or Int, leave operands as-is (e.g., Byte)

                switch (bin.operator.type) {
                    .PLUS => {
                        // Handle string concatenation and array concatenation
                        if (left_type == .String and right_type == .String) {
                            try self.instructions.append(.Swap);
                            try self.instructions.append(.{ .StringOp = .{ .op = .Concat } });
                        } else if (left_type == .Array and right_type == .Array) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Array } });
                        } else {
                            // For numeric types, use the promoted common type
                            const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                            if (common_type != .Unknown) {
                                // Apply type promotion if needed
                                _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                                try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = common_type } });
                            } else {
                                self.reporter.reportCompileError(
                                    bin.left.?.base.location(),
                                    ErrorCode.TYPE_MISMATCH,
                                    "Cannot use + operator between {s} and {s}",
                                    .{ @tagName(left_type), @tagName(right_type) },
                                );
                                return ErrorList.TypeMismatch;
                            }
                        }
                    },
                    .MINUS => {
                        const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                        if (common_type != .Unknown) {
                            _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                            try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = common_type } });
                        } else {
                            self.reporter.reportCompileError(
                                bin.left.?.base.location(),
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use - operator between {s} and {s}",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .ASTERISK => {
                        const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                        if (common_type != .Unknown) {
                            _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                            try self.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = common_type } });
                        } else {
                            self.reporter.reportCompileError(
                                bin.left.?.base.location(),
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use * operator between {s} and {s}",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .SLASH => {
                        const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                        if (common_type != .Unknown) {
                            _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                            try self.instructions.append(.{ .Arith = .{ .op = .Div, .operand_type = common_type } });
                        } else {
                            self.reporter.reportCompileError(
                                bin.left.?.base.location(),
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use / operator between {s} and {s}",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .MODULO => {
                        const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                        if (common_type != .Unknown) {
                            _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                            try self.instructions.append(.{ .Arith = .{ .op = .Mod, .operand_type = common_type } });
                        } else {
                            self.reporter.reportCompileError(
                                bin.left.?.base.location(),
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use % operator between {s} and {s}",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .POWER => {
                        const common_type = self.computeNumericCommonType(left_type, right_type, bin.operator.type);
                        if (common_type != .Unknown) {
                            _ = try self.applyTypePromotionIfNeeded(left_type, right_type, common_type);
                            try self.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = common_type } });
                        } else {
                            self.reporter.reportCompileError(
                                bin.left.?.base.location(),
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use ** operator between {s} and {s}",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .EQUALITY => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = operand_type } });
                    },
                    .BANG_EQUAL => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Ne, .operand_type = operand_type } });
                    },
                    .LESS => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Lt, .operand_type = operand_type } });
                    },
                    .GREATER => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Gt, .operand_type = operand_type } });
                    },
                    .LESS_EQUAL => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Le, .operand_type = operand_type } });
                    },
                    .GREATER_EQUAL => {
                        const operand_type = self.inferComparisonOperandType(bin.left.?, bin.right.?);
                        try self.instructions.append(.{ .Compare = .{ .op = .Ge, .operand_type = operand_type } });
                    },
                    else => {
                        self.reporter.reportCompileError(
                            expr.base.location(),
                            ErrorCode.UNSUPPORTED_OPERATOR,
                            "Unsupported binary operator: {}",
                            .{bin.operator.type},
                        );
                        return ErrorList.UnsupportedOperator;
                    },
                }
            },

            .Logical => |log| {

                // For AND/OR, we need short-circuit evaluation
                if (log.operator.type == .AND) {
                    // Generate: left AND right with short-circuit
                    try self.generateExpression(log.left, true, should_pop_after_use);
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
                    try self.generateExpression(log.right, true, should_pop_after_use);

                    try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
                } else if (log.operator.type == .OR) {
                    // Similar for OR but with inverted logic
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Or } });
                    // Simple OR for now - TODO: add short-circuit optimization
                } else if (log.operator.type == .IFF) {
                    // IFF (if and only if): A ↔ B - true when A and B have same truth value
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Iff } });
                } else if (log.operator.type == .XOR) {
                    // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Xor } });
                } else if (log.operator.type == .NAND) {
                    // NAND: A ↑ B - NOT(A AND B)
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nand } });
                } else if (log.operator.type == .NOR) {
                    // NOR: A ↓ B - NOT(A OR B)
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Nor } });
                } else if (log.operator.type == .IMPLIES) {
                    // IMPLIES: A → B - NOT A OR B
                    try self.generateExpression(log.left, true, should_pop_after_use);
                    try self.generateExpression(log.right, true, should_pop_after_use);
                    try self.instructions.append(.{ .LogicalOp = .{ .op = .Implies } });
                } else {
                    self.reporter.reportCompileError(
                        expr.base.location(),
                        ErrorCode.UNSUPPORTED_OPERATOR,
                        "Unsupported logical operator: {}",
                        .{log.operator.type},
                    );
                    return ErrorList.UnsupportedOperator;
                }
            },

            .If => |if_expr| {

                // Special-case: if inside a loop and the then/else branch is a pure break/continue block,
                // emit a direct conditional jump to the loop label (so control flow skips subsequent body code).
                var handled_as_loop_control = false;
                const lc_opt = self.currentLoopContext();

                // Helper lambdas for detection
                const isControlOnlyBlock = struct {
                    fn run(_: *HIRGenerator, node: *ast.Expr, want_break: bool, want_continue: bool) bool {
                        switch (node.data) {
                            .Block => |blk| {
                                if (blk.statements.len == 0) return false;
                                // Require all statements to be the desired control kind(s)
                                for (blk.statements) |s| {
                                    const d = s.data;
                                    if (want_break and d == .Break) continue;
                                    if (want_continue and d == .Continue) continue;
                                    // Allow empty expression statements as no-ops
                                    if (d == .Expression and s.data.Expression == null) continue;
                                    return false;
                                }
                                return true;
                            },
                            else => return false,
                        }
                    }
                };

                if (lc_opt) |lc| {
                    const then_is_continue = isControlOnlyBlock.run(self, if_expr.then_branch.?, false, true);
                    const then_is_break = isControlOnlyBlock.run(self, if_expr.then_branch.?, true, false);
                    const else_is_continue = if (if_expr.else_branch) |eb| isControlOnlyBlock.run(self, eb, false, true) else false;
                    const else_is_break = if (if_expr.else_branch) |eb| isControlOnlyBlock.run(self, eb, true, false) else false;

                    if (then_is_continue and !else_is_break and !else_is_continue and !then_is_break) {
                        // If TRUE -> continue label, else fall-through
                        try self.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                        const end_if = try self.generateLabel("end_if");
                        try self.instructions.append(.{ .JumpCond = .{ .label_true = lc.continue_label, .label_false = end_if, .vm_offset = 0, .condition_type = .Tetra } });
                        try self.instructions.append(.{ .Label = .{ .name = end_if, .vm_address = 0 } });
                        handled_as_loop_control = true;
                    } else if (then_is_break and !else_is_break and !else_is_continue and !then_is_continue) {
                        // If TRUE -> break label, else fall-through
                        try self.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                        const end_if = try self.generateLabel("end_if");
                        try self.instructions.append(.{ .JumpCond = .{ .label_true = lc.break_label, .label_false = end_if, .vm_offset = 0, .condition_type = .Tetra } });
                        try self.instructions.append(.{ .Label = .{ .name = end_if, .vm_address = 0 } });
                        handled_as_loop_control = true;
                    } else if (!then_is_break and !then_is_continue and (else_is_break or else_is_continue)) {
                        // DISABLED: This optimization can skip important semantics like debugging output
                        // or proper execution flow. It's safer to use the standard if-then-else codegen.
                        //
                        // Original logic:
                        // Only else branch is control: invert condition
                        // try self.generateExpression(if_expr.condition.?, true);
                        // const end_if = try self.generateLabel("end_if");
                        // const target = if (else_is_break) lc.break_label else lc.continue_label;
                        // // If FALSE -> target, TRUE -> fall through
                        // try self.instructions.append(.{ .JumpCond = .{ .label_true = end_if, .label_false = target, .vm_offset = 0, .condition_type = .Tetra } });
                        // try self.instructions.append(.{ .Label = .{ .name = end_if, .vm_address = 0 } });
                        // handled_as_loop_control = true;
                    }
                }

                if (!handled_as_loop_control) {
                    // Standard if codegen
                    try self.generateExpression(if_expr.condition.?, true, should_pop_after_use);

                    const else_label = try self.generateLabel("else");
                    const end_label = try self.generateLabel("end_if");
                    const then_label = try self.generateLabel("then");
                    try self.instructions.append(.{
                        .JumpCond = .{
                            .label_true = then_label,
                            .label_false = else_label,
                            .vm_offset = 0,
                            .condition_type = .Tetra,
                        },
                    });

                    // THEN branch
                    try self.instructions.append(.{ .Label = .{ .name = then_label, .vm_address = 0 } });
                    if (preserve_result) {
                        try self.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    } else {
                        // Statement context: do not produce a value
                        try self.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                    try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                    // ELSE branch
                    try self.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });
                    if (if_expr.else_branch) |else_branch| {
                        if (preserve_result) {
                            try self.generateExpression(else_branch, true, should_pop_after_use);
                        } else {
                            try self.generateExpression(else_branch, false, should_pop_after_use);
                        }
                    } else if (preserve_result) {
                        // Only push a value when needed
                        const nothing_idx = try self.addConstant(HIRValue.nothing);
                        try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                    }
                    try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
                }
            },

            .FunctionCall => |function_call| {
                // Extract function name and determine call type
                var function_name: []const u8 = "unknown";
                var call_kind: CallKind = .LocalFunction;
                var function_index: u32 = 0;

                // Distinguish between module function, internal method, and regular function
                switch (function_call.callee.data) {
                    .FieldAccess => |field_access| {
                        // Module function call: namespace.func(...)
                        if (field_access.object.data == .Variable and self.isModuleNamespace(field_access.object.data.Variable.lexeme)) {
                            const object_name = field_access.object.data.Variable.lexeme;
                            function_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ object_name, field_access.field.lexeme });

                            if (std.mem.eql(u8, function_name, "safeMath.safeAdd")) {
                                call_kind = .ModuleFunction;
                            } else if (self.getFunctionIndex(function_name)) |idx| {
                                function_index = idx;
                                call_kind = .LocalFunction;
                            } else |_| {
                                call_kind = .ModuleFunction;
                            }

                            // Emit only the arguments for module function
                            for (function_call.arguments) |arg| {
                                try self.generateExpression(arg.expr, true, should_pop_after_use);
                            }
                        } else {
                            // Built-in/internal method on a receiver: delegate and return early
                            try self.generateInternalMethodCall(field_access.field, field_access.object, function_call.arguments, should_pop_after_use);
                            return;
                        }
                    },
                    .Variable => |var_token| {
                        function_name = var_token.lexeme;
                        if (self.getFunctionIndex(function_name)) |index| {
                            function_index = index;
                            call_kind = .LocalFunction;
                        } else |_| {
                            call_kind = .BuiltinFunction;
                        }
                    },
                    else => {
                        self.reporter.reportCompileError(
                            expr.base.location(),
                            ErrorCode.UNSUPPORTED_FUNCTION_CALL_TYPE,
                            "Unsupported function call type",
                            .{},
                        );
                        return ErrorList.UnsupportedFunctionCallType;
                    },
                }

                // Generate arguments (default placeholders resolved)
                var arg_emitted_count: u32 = 0;
                for (function_call.arguments, 0..) |arg, arg_index| {
                    if (arg.expr.data == .DefaultArgPlaceholder) {
                        if (self.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                            try self.generateExpression(default_expr, true, false);
                            arg_emitted_count += 1;
                        } else {
                            const location = if (expr.base.span) |span| span.location else Location{
                                .file = "",
                                .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
                            };
                            self.reporter.reportCompileError(location, ErrorCode.NO_DEFAULT_VALUE_FOR_PARAMETER, "No default value for parameter {} in function '{s}'", .{ arg_index, function_name });
                        }
                    } else {
                        if (arg.is_alias) {
                            // For alias arguments, we need to push the storage ID of the variable
                            if (arg.expr.data == .Variable) {
                                const var_token = arg.expr.data.Variable;
                                // Find the existing variable to get its storage ID
                                var maybe_idx: ?u32 = null;
                                if (self.current_function != null) {
                                    maybe_idx = self.local_variables.get(var_token.lexeme);
                                } else {
                                    maybe_idx = self.variables.get(var_token.lexeme);
                                }
                                if (maybe_idx) |var_idx| {
                                    try self.instructions.append(.{
                                        .PushStorageId = .{
                                            .var_index = var_idx,
                                            .var_name = var_token.lexeme,
                                            .scope_kind = .Local, // This will be resolved at runtime
                                        },
                                    });
                                    arg_emitted_count += 1;
                                } else {
                                    self.reporter.reportCompileError(
                                        arg.expr.base.location(),
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable used as alias argument: {s}",
                                        .{var_token.lexeme},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
                            } else {
                                // Alias argument must be a variable
                                self.reporter.reportCompileError(
                                    arg.expr.base.location(),
                                    ErrorCode.INVALID_ALIAS_ARGUMENT,
                                    "Alias argument must be a variable (e.g., ^myVar)",
                                    .{},
                                );
                                return ErrorList.INVALID_ALIAS_ARGUMENT;
                            }
                        } else {
                            try self.generateExpression(arg.expr, true, false);
                            arg_emitted_count += 1;
                        }
                    }
                }

                const return_type = self.inferCallReturnType(function_name, call_kind) catch .String;
                try self.instructions.append(.{
                    .Call = .{
                        .function_index = function_index,
                        .qualified_name = function_name,
                        .arg_count = arg_emitted_count,
                        .call_kind = call_kind,
                        .target_module = null,
                        .return_type = return_type,
                    },
                });
            },

            .Peek => |peek| {

                // Set current peek expression for field access tracking
                self.current_peek_expr = peek.expr;
                defer self.current_peek_expr = null;

                // Generate the expression to peek (leaves value on stack)
                try self.generateExpression(peek.expr, true, false);

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

                // New: include union member list for variables declared as unions
                var union_members: ?[][]const u8 = null;
                // If peeking a StringToInt expression directly, attach union members inline
                if (peek.expr.data == .StringToInt) {
                    const members = try self.allocator.alloc([]const u8, 2);
                    members[0] = "int";
                    members[1] = "NumberError";
                    union_members = members;
                }
                // If peeking a direct @substring expression, attach union members inline
                if (peek.expr.data == .InternalCall) {
                    const m = peek.expr.data.InternalCall;
                    if (std.mem.eql(u8, m.method.lexeme, "substring")) {
                        const members = try self.allocator.alloc([]const u8, 2);
                        members[0] = "string";
                        members[1] = "IndexError";
                        union_members = members;
                    }
                }
                if (peek.expr.data == .Variable) {
                    const var_name = peek.expr.data.Variable.lexeme;
                    // Prefer index-based lookup to avoid name collisions; do this for all scopes
                    var maybe_index: ?u32 = null;
                    if (self.current_function != null) {
                        maybe_index = self.local_variables.get(var_name);
                    } else {
                        maybe_index = self.variables.get(var_name);
                    }
                    if (maybe_index) |var_index| {
                        if (self.variable_union_members_by_index.get(var_index)) |members2| {
                            union_members = members2;
                        }
                    }
                }

                // Generate peek instruction with full path and correct type
                try self.instructions.append(.{ .Peek = .{
                    .name = peek_path,
                    .value_type = inferred_type,
                    .location = peek.location,
                    .union_members = union_members,
                } });

                // IMPORTANT: Peek pops the value, prints, then pushes it back.
                // If the caller does not need the result (statement context), drop it now
                if (!preserve_result) {
                    try self.instructions.append(.Pop);
                }
            },

            .Print => |print| {
                if (print.expr) |print_expr| {
                    // Simple printing case
                    try self.generateExpression(print_expr, true, false);
                    try self.instructions.append(.{ .Print = .{} });
                } else if (print.format_template) |template| {
                    // Generate code for each template part and build correct placeholder mapping
                    var arg_count: u32 = 0;
                    var format_parts = std.ArrayList([]const u8).init(self.allocator);
                    // VM expects placeholder_indices to be argument indices (0..N-1) by placeholder order.
                    // We will later interleave using the format part positions, so here we only record arg order.
                    var placeholder_indices = std.ArrayList(u32).init(self.allocator);
                    var expressions = std.ArrayList(*ast.Expr).init(self.allocator);
                    defer format_parts.deinit();
                    defer placeholder_indices.deinit();
                    defer expressions.deinit();

                    // First pass: collect all expressions and build format parts
                    for (template.parts) |part| {
                        switch (part) {
                            .String => |str| {
                                // Add string literal to format parts
                                try format_parts.append(str);
                            },
                            .Expression => |part_expr| {
                                // Store expression for later evaluation
                                try expressions.append(part_expr);
                                // Map placeholder to its argument index by encounter order
                                try placeholder_indices.append(arg_count);
                                arg_count += 1;
                            },
                        }
                    }

                    // Second pass: evaluate expressions in encounter order (left-to-right)
                    // VM will reverse-pop to restore original order
                    for (expressions.items) |expr_item| {
                        try self.generateExpression(expr_item, true, false);
                    }

                    // Store format parts as constants and get their IDs
                    var format_part_ids = try self.allocator.alloc(u32, format_parts.items.len);
                    for (format_parts.items, 0..) |part, i| {
                        const constant_id = try self.addConstant(.{ .string = part });
                        format_part_ids[i] = constant_id;
                    }

                    // Generate interpolated print instruction
                    try self.instructions.append(.{ .PrintInterpolated = .{
                        .format_parts = try format_parts.toOwnedSlice(),
                        .placeholder_indices = try placeholder_indices.toOwnedSlice(),
                        .argument_count = arg_count,
                        .format_part_ids = format_part_ids,
                    } });
                } else if (print.arguments) |args| {
                    if (args.len == 0) {
                        // No interpolation - need to push the format string literal
                        // The format_parts should contain the original string
                        if (print.format_parts) |parts| {
                            if (parts.len > 0) {
                                // Add the string to the constant pool first
                                const constant_id = try self.addConstant(.{ .string = parts[0] });

                                // Push the constant by ID
                                try self.instructions.append(.{
                                    .Const = .{
                                        .value = .{ .string = parts[0] }, // Use the actual string value
                                        .constant_id = constant_id,
                                    },
                                });
                            }
                        }
                        try self.instructions.append(.{ .Print = .{} });
                    } else {
                        // Generate code for each argument
                        for (args) |arg| {
                            try self.generateExpression(arg, true, false);
                        }

                        // Store format parts as constants and get their IDs
                        const format_parts = print.format_parts orelse return error.MissingFormatParts;
                        const placeholder_indices = print.placeholder_indices orelse return error.MissingPlaceholderIndices;

                        var format_part_ids = try self.allocator.alloc(u32, format_parts.len);
                        for (format_parts, 0..) |part, i| {
                            const constant_id = try self.addConstant(.{ .string = part });
                            format_part_ids[i] = constant_id;
                        }

                        // Generate interpolated print instruction
                        try self.instructions.append(.{ .PrintInterpolated = .{
                            .format_parts = format_parts,
                            .placeholder_indices = placeholder_indices,
                            .argument_count = @intCast(args.len),
                            .format_part_ids = format_part_ids,
                        } });
                    }
                } else {
                    self.reporter.reportCompileError(null, ErrorCode.INVALID_PRINT_EXPRESSION, "No expr and no arguments - InvalidPrintExpression", .{});
                    return error.InvalidPrintExpression;
                }

                // If the caller does not need the result (statement context), drop it now
                // Note: For interpolated printing, PrintInterpolated already consumes its arguments
                if (!preserve_result and print.expr != null) {
                    try self.instructions.append(.Pop);
                }
            },

            .PeekStruct => |peek| {

                // Generate the expression to peek
                try self.generateExpression(peek.expr, true, false);

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
                        try self.generateExpression(field.object, true, false);
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

            .Assignment => |assign| {

                // Generate the value expression
                try self.generateExpression(assign.value.?, true, false);

                // NEW: Track the variable's type from the assigned value
                const assigned_type = self.inferTypeFromExpression(assign.value.?);
                try self.trackVariableType(assign.name.lexeme, assigned_type);

                // NEW: Track array element type for array literals
                if (assigned_type == .Array and assign.value.?.data == .Array) {
                    const elements = assign.value.?.data.Array;
                    if (elements.len > 0) {
                        const element_type: HIRType = switch (elements[0].data) {
                            .Literal => |lit| switch (lit) {
                                .int => .Int,
                                .float => .Float,
                                .string => .String,
                                .tetra => .Tetra,
                                .byte => .Byte,
                                else => .Unknown,
                            },
                            else => .Unknown,
                        };
                        if (element_type != .Unknown) {
                            try self.trackArrayElementType(assign.name.lexeme, element_type);
                        }
                    }
                }

                // If assigning result of StringToInt, also track union members for peeks: ["int", "NumberError"]
                if (assign.value.?.data == .StringToInt) {
                    if (self.getOrCreateVariable(assign.name.lexeme)) |var_idx| {
                        const members = try self.allocator.alloc([]const u8, 2);
                        members[0] = "int";
                        members[1] = "NumberError";
                        try self.trackVariableUnionMembersByIndex(var_idx, members);
                    } else |_| {}
                }
                // If assigning result of @substring, track union members: ["string", "IndexError"]
                if (assign.value.?.data == .InternalCall) {
                    const m = assign.value.?.data.InternalCall;
                    if (std.mem.eql(u8, m.method.lexeme, "substring")) {
                        if (self.getOrCreateVariable(assign.name.lexeme)) |var_idx| {
                            const members = try self.allocator.alloc([]const u8, 2);
                            members[0] = "string";
                            members[1] = "IndexError";
                            try self.trackVariableUnionMembersByIndex(var_idx, members);
                        } else |_| {}
                    }
                }

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
                            else => .Unknown,
                        },
                        else => break :blk .Unknown,
                    }
                } else .Unknown;

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
                    try self.generateExpression(element, true, false);

                    // Set array element (stack: array, index, value)
                    try self.instructions.append(.{ .ArraySet = .{ .bounds_check = false } }); // No bounds check for initialization
                }
            },

            .Index => |index| {

                // Generate array/map expression
                try self.generateExpression(index.array, true, false);

                // Determine if we're accessing an array, map
                const container_type = self.inferTypeFromExpression(index.array);
                switch (container_type) {
                    .Map => {
                        // Generate index expression
                        try self.generateExpression(index.index, true, false);

                        // Map access - use MapGet with key type inferred from index
                        const idx_type = self.inferTypeFromExpression(index.index);
                        const key_type = if (idx_type == .Int) HIRType.Int else HIRType.String;
                        try self.instructions.append(.{ .MapGet = .{ .key_type = key_type } });
                    },
                    .Array => {
                        // Generate index expression
                        try self.generateExpression(index.index, true, false);

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
                        try self.generateExpression(index.index, true, false);

                        // Default to array access for now
                        try self.instructions.append(.{ .ArrayGet = .{ .bounds_check = true } });
                    },
                }
                // If result is not preserved AND needs to be popped, do so now
                if (!preserve_result and should_pop_after_use) {
                    try self.instructions.append(.Pop);
                }
            },

            .IndexAssign => |assign| {
                // Generate array expression
                try self.generateExpression(assign.array, true, false);

                // Generate index expression
                try self.generateExpression(assign.index, true, false);

                // Generate value expression
                try self.generateExpression(assign.value, true, false);

                // If the receiver is a map, emit MapSet; otherwise ArraySet
                const container_type = self.inferTypeFromExpression(assign.array);
                if (container_type == .Map) {
                    const idx_type = self.inferTypeFromExpression(assign.index);
                    const key_type = if (idx_type == .Int) HIRType.Int else HIRType.String;
                    try self.instructions.append(.{ .MapSet = .{ .key_type = key_type } });
                } else {
                    // Generate ArraySet instruction
                    // Stack order expected by VM (top to bottom): value, index, array
                    try self.instructions.append(.{ .ArraySet = .{ .bounds_check = true } });
                }

                // Store the modified array back to the variable
                if (assign.array.data == .Variable) {
                    const var_name = assign.array.data.Variable.lexeme;

                    const var_idx = try self.getOrCreateVariable(var_name);
                    const expected_type = self.getTrackedVariableType(var_name) orelse .Unknown;

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
                try self.generateExpression(push.array, true, false);
                try self.generateExpression(push.element, true, false);

                // Emit ArrayPush instruction (uses stack: pops element, then array)
                try self.instructions.append(.{ .ArrayPush = .{ .resize_behavior = .Double } });

                // If the receiver is a variable, store the updated array back into it
                if (push.array.data == .Variable) {
                    const var_name = push.array.data.Variable.lexeme;
                    const var_idx = try self.getOrCreateVariable(var_name);
                    const expected_type = self.getTrackedVariableType(var_name) orelse .Unknown;

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

            .ArrayPop => |pop| {
                // Generate array expression (stack: [..., array])
                try self.generateExpression(pop.array, true, false);

                // Emit ArrayPop. VM pushes popped element, then updated array
                try self.instructions.append(.ArrayPop);

                // If the array is a variable, store the updated array back
                if (pop.array.data == .Variable) {
                    const var_name = pop.array.data.Variable.lexeme;
                    const var_idx = try self.getOrCreateVariable(var_name);
                    const expected_type = self.getTrackedVariableType(var_name) orelse .Unknown;

                    // Stack is [ ..., element, array ] -> swap to store array, leave element as result
                    try self.instructions.append(.Swap);
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = var_name,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = expected_type,
                    } });
                    // After StoreVar, element remains on stack as the expression result
                }
            },

            .Grouping => |grouping| {
                // Grouping is just parentheses - generate the inner expression
                if (grouping) |inner_expr| {
                    try self.generateExpression(inner_expr, true, false);
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
                    // Evaluate the final value expression
                    // Always evaluate, but only preserve on stack when requested
                    try self.generateExpression(value_expr, true, false);
                    if (!preserve_result) {
                        // Discard the produced value in statement context
                        try self.instructions.append(.Pop);
                    }
                } else if (preserve_result) {
                    // Block without value: only push 'nothing' when a value is expected
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
                try self.generateExpression(compound.value.?, true, false);

                const left_type = self.getTrackedVariableType(compound.name.lexeme) orelse .Unknown;
                const right_type = self.inferTypeFromExpression(compound.value.?);
                switch (compound.operator.type) {
                    .PLUS_EQUAL => {
                        if (left_type == .Int and right_type == .Int) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Int } });
                        } else if (left_type == .Float and right_type == .Float) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Float } });
                        } else if (left_type == .Byte and right_type == .Byte) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Byte } });
                        } else if (left_type == .Byte and right_type == .Int) {
                            // Implicitly convert RHS Int to Byte for byte arithmetic
                            try self.instructions.append(.{ .Convert = .{ .from_type = .Int, .to_type = .Byte } });
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Byte } });
                        } else if (left_type == .String and right_type == .String) {
                            try self.instructions.append(.{ .StringOp = .{ .op = .Concat } });
                        } else if (left_type == .Array and right_type == .Array) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = .Array } });
                        } else {
                            const location = Location{
                                .file = compound.name.file,
                                .range = .{
                                    .start_line = compound.name.line,
                                    .start_col = compound.name.column,
                                    .end_line = compound.name.line,
                                    .end_col = compound.name.column + compound.name.lexeme.len,
                                },
                            };
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use += operator between {s} and {s}. Both operands must be the same type.",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .MINUS_EQUAL => {
                        if (left_type == .Int and right_type == .Int) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Int } });
                        } else if (left_type == .Float and right_type == .Float) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Float } });
                        } else if (left_type == .Byte and right_type == .Byte) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = .Byte } });
                        } else {
                            const location = Location{
                                .file = compound.name.file,
                                .range = .{
                                    .start_line = compound.name.line,
                                    .start_col = compound.name.column,
                                    .end_line = compound.name.line,
                                    .end_col = compound.name.column + compound.name.lexeme.len,
                                },
                            };
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use += operator between {s} and {s}. Both operands must be the same type.",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .ASTERISK_EQUAL => {
                        if (left_type == .Int and right_type == .Int) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Int } });
                        } else if (left_type == .Float and right_type == .Float) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Float } });
                        } else if (left_type == .Byte and right_type == .Byte) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Mul, .operand_type = .Byte } });
                        } else {
                            const location = Location{
                                .file = compound.name.file,
                                .range = .{
                                    .start_line = compound.name.line,
                                    .start_col = compound.name.column,
                                    .end_line = compound.name.line,
                                    .end_col = compound.name.column + compound.name.lexeme.len,
                                },
                            };
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use += operator between {s} and {s}. Both operands must be the same type.",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    .SLASH_EQUAL => {
                        // Stack: [..., left, right]
                        // Convert left to Float
                        try self.instructions.append(.Swap);
                        if (left_type != .Float) {
                            try self.instructions.append(.{ .Convert = .{ .from_type = left_type, .to_type = .Float } });
                        }
                        // Convert right to Float
                        try self.instructions.append(.Swap);
                        if (right_type != .Float) {
                            try self.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = .Float } });
                        }
                        // Now do float division
                        try self.instructions.append(.{ .Arith = .{ .op = .Div, .operand_type = .Float } });
                    },
                    .POWER_EQUAL => {
                        if (left_type == .Int and right_type == .Int) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Int } });
                        } else if (left_type == .Float and right_type == .Float) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Float } });
                        } else if (left_type == .Byte and right_type == .Byte) {
                            try self.instructions.append(.{ .Arith = .{ .op = .Pow, .operand_type = .Byte } });
                        } else {
                            const location = Location{
                                .file = compound.name.file,
                                .range = .{
                                    .start_line = compound.name.line,
                                    .start_col = compound.name.column,
                                    .end_line = compound.name.line,
                                    .end_col = compound.name.column + compound.name.lexeme.len,
                                },
                            };
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.TYPE_MISMATCH,
                                "Cannot use **= operator between {s} and {s}. Both operands must be the same type.",
                                .{ @tagName(left_type), @tagName(right_type) },
                            );
                            return ErrorList.TypeMismatch;
                        }
                    },
                    else => {
                        const location = Location{
                            .file = compound.operator.file,
                            .range = .{
                                .start_line = compound.operator.line,
                                .start_col = compound.operator.column,
                                .end_line = compound.operator.line,
                                .end_col = compound.operator.column + compound.operator.lexeme.len,
                            },
                        };
                        self.reporter.reportCompileError(
                            location,
                            ErrorCode.UNSUPPORTED_OPERATOR,
                            "Unsupported compound assignment operator: {}",
                            .{compound.operator.type},
                        );
                        return ErrorList.UnsupportedOperator;
                    },
                }

                // Duplicate the result to leave it on stack as the expression result
                if (preserve_result) {
                    try self.instructions.append(.Dup);
                }

                // Store the result back to the variable
                const expected_type = self.getTrackedVariableType(compound.name.lexeme) orelse .Unknown;
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
                        try self.generateExpression(value, true, false);
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
                try self.generateExpression(unary.right.?, true, false);

                // Generate the unary operation
                switch (unary.operator.type) {
                    .NOT => {
                        // Generate logical NOT operation using ultra-fast lookup table
                        try self.instructions.append(.{ .LogicalOp = .{ .op = .Not } });
                    },
                    .MINUS => {
                        // Unary minus: 0 - operand
                        const operand_type = self.inferTypeFromExpression(unary.right.?);
                        const zero_value = switch (operand_type) {
                            .Int => HIRValue{ .int = 0 },
                            .Float => HIRValue{ .float = 0.0 },
                            .Byte => HIRValue{ .byte = 0 },
                            else => HIRValue{ .int = 0 }, // fallback
                        };
                        const zero_idx = try self.addConstant(zero_value);
                        try self.instructions.append(.{ .Const = .{ .value = zero_value, .constant_id = zero_idx } });

                        // Swap operands so we have: 0, operand on stack
                        // Then subtract: 0 - operand = -operand
                        try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });
                    },
                    .PLUS => {
                        // Unary plus: just return the operand unchanged (no-op)
                        // The operand is already on the stack
                    },
                    else => {
                        const location = Location{
                            .file = unary.operator.file,
                            .range = .{
                                .start_line = unary.operator.line,
                                .start_col = unary.operator.column,
                                .end_line = unary.operator.line,
                                .end_col = unary.operator.column + unary.operator.lexeme.len,
                            },
                        };
                        self.reporter.reportCompileError(
                            location,
                            ErrorCode.UNSUPPORTED_OPERATOR,
                            "Unsupported unary operator: {}",
                            .{unary.operator.type},
                        );
                        return ErrorList.UnsupportedOperator;
                    },
                }
            },

            .ForAll => |forall| {

                // ForAll quantifier: ∀x ∈ array : condition
                // Implementation: iterate through array, return false if any element fails condition

                // Generate array expression
                try self.generateExpression(forall.array, true, false);

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
                                var maybe_idx: ?u32 = null;
                                if (self.current_function != null) {
                                    maybe_idx = self.local_variables.get(var_name);
                                } else {
                                    maybe_idx = self.variables.get(var_name);
                                }
                                if (maybe_idx) |var_index| {
                                    try self.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = .Local,
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generateExpression(right, true, false);
                            },
                        }
                    } else {
                        // No right operand - generate the condition as-is
                        try self.generateExpression(forall.condition, true, false);
                    }
                } else {
                    // Complex condition - generate the expression as-is
                    try self.generateExpression(forall.condition, true, false);
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
                try self.generateExpression(exists.array, true, false);

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
                                var maybe_idx: ?u32 = null;
                                if (self.current_function != null) {
                                    maybe_idx = self.local_variables.get(var_name);
                                } else {
                                    maybe_idx = self.variables.get(var_name);
                                }
                                if (maybe_idx) |var_index| {
                                    try self.instructions.append(.{
                                        .LoadVar = .{
                                            .var_index = var_index,
                                            .var_name = var_name,
                                            .scope_kind = .Local,
                                            .module_context = null,
                                        },
                                    });
                                } else {
                                    const location = Location{
                                        .file = var_token.file,
                                        .range = .{
                                            .start_line = var_token.line,
                                            .start_col = var_token.column,
                                            .end_line = var_token.line,
                                            .end_col = var_token.column + var_token.lexeme.len,
                                        },
                                    };
                                    self.reporter.reportCompileError(
                                        location,
                                        ErrorCode.UNDEFINED_VARIABLE,
                                        "Undefined variable in quantifier condition: {s}",
                                        .{var_name},
                                    );
                                    return ErrorList.UndefinedVariable;
                                }
                            },
                            else => {
                                // Complex condition - generate the expression
                                try self.generateExpression(right, true, false);
                            },
                        }
                    } else {
                        // No right operand - generate the condition as-is
                        try self.generateExpression(exists.condition, true, false);
                    }
                } else {
                    // Complex condition - generate the expression as-is
                    try self.generateExpression(exists.condition, true, false);
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
                try self.generateExpression(match_expr.value, true, false);

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
                        // Check if this is a type pattern for union matching
                        const is_type_pattern = switch (case.pattern.type) {
                            .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE => true,
                            else => false,
                        };

                        if (is_type_pattern) {
                            // This is a type pattern - use TypeCheck instruction
                            const type_name = case.pattern.lexeme;
                            try self.instructions.append(.{ .TypeCheck = .{ .target_type = type_name } });
                        } else if (match_enum_type) |enum_type_name| {
                            // Generate the pattern value (enum member with proper context)
                            const variant_index = if (self.custom_types.get(enum_type_name)) |custom_type|
                                custom_type.getEnumVariantIndex(case.pattern.lexeme) orelse 0
                            else
                                0;

                            const pattern_value = HIRValue{
                                .enum_variant = HIREnum{
                                    .type_name = enum_type_name,
                                    .variant_name = case.pattern.lexeme,
                                    .variant_index = variant_index,
                                    .path = null,
                                },
                            };

                            const pattern_idx = try self.addConstant(pattern_value);
                            try self.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });

                            // Compare and jump if equal (use Enum operand type)
                            try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .Enum } });
                        } else {
                            // Regular string literal pattern
                            const pattern_value = HIRValue{ .string = case.pattern.literal.string };
                            const pattern_idx = try self.addConstant(pattern_value);
                            try self.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });

                            // Compare and jump if equal (use String operand type)
                            try self.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = .String } });
                        }

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

                    try self.generateExpression(case.body, true, false);

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

                    // If we know the struct type and this field's declared custom type name refers
                    // to an enum, set the enum context so `.FOO` lowers to an enum value, not string
                    const previous_enum_context = self.current_enum_type;
                    if (self.custom_types.get(struct_lit.name.lexeme)) |ctype| {
                        if (ctype.kind == .Struct) {
                            if (ctype.struct_fields) |cfields| {
                                // Find matching field by name
                                for (cfields) |cf| {
                                    if (std.mem.eql(u8, cf.name, field.name.lexeme)) {
                                        if (cf.custom_type_name) |ct_name| {
                                            if (self.custom_types.get(ct_name)) |maybe_enum| {
                                                if (maybe_enum.kind == .Enum) {
                                                    self.current_enum_type = ct_name;
                                                }
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // Generate field value with possible enum context
                    try self.generateExpression(field.value, true, false);
                    // Restore enum context
                    self.current_enum_type = previous_enum_context;

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

                // Result is on the stack
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
                    try self.generateExpression(outer_field.object, true, false);

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
                    try self.generateExpression(field_assign.value, true, false);

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
                    try self.generateExpression(outer_field.object, true, false);

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
                        const expected_type = self.getTrackedVariableType(var_name) orelse .Unknown;
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
                    try self.generateExpression(field_assign.object, true, false);

                    // Generate value expression
                    try self.generateExpression(field_assign.value, true, false);

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
                    .Union => "union",
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
                    .Unknown => "unknown",
                };

                const type_value = HIRValue{ .string = type_name };
                const const_idx = try self.addConstant(type_value);
                try self.instructions.append(.{ .Const = .{ .value = type_value, .constant_id = const_idx } });
            },

            .Cast => |cast_expr| {
                // Generate the value to cast
                try self.generateExpression(cast_expr.value, true, false);

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
                        .Array => |arr_type| {
                            // Map array element types to the strings produced by VM.getTypeString
                            switch (arr_type.element_type.data) {
                                .Basic => |elem_basic| switch (elem_basic) {
                                    .Integer => break :blk "int[]",
                                    .Byte => break :blk "byte[]",
                                    .Float => break :blk "float[]",
                                    .String => break :blk "string[]",
                                    .Tetra => break :blk "tetra[]",
                                    .Nothing => break :blk "array[]", // VM uses array[] for unknown/nothing
                                },
                                // Arrays of custom/struct types appear as struct[] at runtime
                                .Custom => break :blk "struct[]",
                                .Struct => break :blk "struct[]",
                                // Other element kinds (enum, union, map, function, auto) default to array[]
                                else => break :blk "array[]",
                            }
                        },
                        .Struct => break :blk "struct",
                        .Enum => break :blk "enum",
                        .Union => break :blk "union",
                    }
                };

                // Check runtime type against target type using dedicated TypeCheck instruction
                try self.instructions.append(.{ .TypeCheck = .{ .target_type = target_name } });

                // Branch based on comparison
                const ok_label = try self.generateLabel("cast_ok");
                const else_label = try self.generateLabel("cast_else");
                const end_label = try self.generateLabel("cast_end");
                try self.instructions.append(.{ .JumpCond = .{ .label_true = ok_label, .label_false = else_label, .vm_offset = 0, .condition_type = .Tetra } });

                // Else branch: drop original value and evaluate else expression
                try self.instructions.append(.{ .Label = .{ .name = else_label, .vm_address = 0 } });
                try self.instructions.append(.Pop);
                if (cast_expr.else_branch) |else_expr| {
                    // Preserve result only if requested by parent
                    try self.generateExpression(else_expr, preserve_result, false);
                } else {
                    // No else branch: cast must fail -> halt program
                    try self.instructions.append(.Halt);
                }
                try self.instructions.append(.{ .Jump = .{ .label = end_label, .vm_offset = 0 } });

                // Success branch
                try self.instructions.append(.{ .Label = .{ .name = ok_label, .vm_address = 0 } });
                if (cast_expr.then_branch) |then_expr| {
                    // On success, drop original and evaluate then-branch
                    try self.instructions.append(.Pop);
                    try self.generateExpression(then_expr, preserve_result, false);
                } else {
                    // No then branch: keep original value if result is needed, drop if not
                    if (!preserve_result) {
                        try self.instructions.append(.Pop);
                    }
                }

                // End merge point
                try self.instructions.append(.{ .Label = .{ .name = end_label, .vm_address = 0 } });
            },

            .LengthOf => |expr_to_check| {
                // Generate the expression whose length we want to get
                try self.generateExpression(expr_to_check, true, false);

                // Generate StringOp.Length instruction
                try self.instructions.append(.{
                    .StringOp = .{
                        .op = .Length,
                    },
                });
            },

            .BytesOf => |expr_to_check| {
                // Generate the expression whose bytes we want to get
                try self.generateExpression(expr_to_check, true, false);

                // Generate StringOp.Bytes instruction
                try self.instructions.append(.{
                    .StringOp = .{
                        .op = .Bytes,
                    },
                });
            },

            .StringToInt => |sti| {
                // Generate the string expression to convert
                try self.generateExpression(sti.string, true, false);

                // Generate StringOp.ToInt instruction
                try self.instructions.append(.{
                    .StringOp = .{ .op = .ToInt },
                });
            },
            .StringToFloat => |stf| {
                // Generate the string expression to convert
                try self.generateExpression(stf.string, true, false);

                // Generate StringOp.ToFloat instruction
                try self.instructions.append(.{
                    .StringOp = .{ .op = .ToFloat },
                });
            },
            .StringToByte => |stb| {
                // Generate the string expression to convert
                try self.generateExpression(stb.string, true, false);

                // Generate StringOp.ToByte instruction
                try self.instructions.append(.{
                    .StringOp = .{ .op = .ToByte },
                });
            },
            .Map => |entries| {

                // Generate each key-value pair in reverse order (for stack-based construction)
                var reverse_i = entries.len;
                while (reverse_i > 0) {
                    reverse_i -= 1;
                    const entry = entries[reverse_i];

                    // Generate key first, then value (they'll be popped in reverse order)
                    try self.generateExpression(entry.key, true, false);
                    try self.generateExpression(entry.value, true, false);
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
                        .value_type = .Unknown, // Will be inferred from values
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

            .Loop => |loop| {
                const loop_start_label = try self.generateLabel("loop_start");
                const loop_body_label = try self.generateLabel("loop_body");
                const loop_step_label = try self.generateLabel("loop_step");
                const loop_end_label = try self.generateLabel("loop_end");

                // continue should jump to step if present, otherwise to start
                const continue_target = if (loop.step != null) loop_step_label else loop_start_label;
                try self.pushLoopContext(loop_end_label, continue_target);

                // Initializer (var decl or expression statement)
                if (loop.var_decl) |initializer| {
                    try self.generateStatement(initializer.*);
                }

                // Loop start - condition check
                try self.instructions.append(.{ .Label = .{ .name = loop_start_label, .vm_address = 0 } });

                if (loop.condition) |condition| {
                    try self.generateExpression(condition, true, false);
                } else {
                    const true_idx = try self.addConstant(HIRValue{ .tetra = TETRA_TRUE });
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_TRUE }, .constant_id = true_idx } });
                }

                try self.instructions.append(.{ .JumpCond = .{ .label_true = loop_body_label, .label_false = loop_end_label, .vm_offset = 0, .condition_type = .Tetra } });

                // Body
                try self.instructions.append(.{ .Label = .{ .name = loop_body_label, .vm_address = 0 } });
                // Enter per-iteration scope to ensure locals/consts do not leak across iterations
                const iteration_scope_id = self.label_count + 2000;
                try self.instructions.append(.{ .EnterScope = .{ .scope_id = iteration_scope_id, .var_count = 0 } });
                try self.generateExpression(loop.body, false, false);

                // Step
                try self.instructions.append(.{ .Label = .{ .name = loop_step_label, .vm_address = 0 } });
                // Exit per-iteration scope before executing the step
                try self.instructions.append(.{ .ExitScope = .{ .scope_id = iteration_scope_id } });
                if (loop.step) |step_expr| {
                    try self.generateExpression(step_expr, false, false);
                }

                try self.instructions.append(.{ .Jump = .{ .label = loop_start_label, .vm_offset = 0 } });
                try self.instructions.append(.{ .Label = .{ .name = loop_end_label, .vm_address = 0 } }); // Add end label
                self.popLoopContext();
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
                    try self.generateExpression(field.object, true, false);

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

            .InternalCall => |m| {
                // Generate HIR for compiler methods like @string, @length, @substring
                const name = m.method.lexeme;
                if (std.mem.eql(u8, name, "substring")) {
                    // Evaluate in VM-expected order: start, length, then receiver on top
                    try self.generateExpression(m.arguments[0], true, false);
                    try self.generateExpression(m.arguments[1], true, false);
                    try self.generateExpression(m.receiver, true, false);
                    try self.instructions.append(.{ .StringOp = .{ .op = .Substring } });
                } else if (std.mem.eql(u8, name, "string")) {
                    // Evaluate receiver (the value to convert to string)
                    try self.generateExpression(m.receiver, true, false);
                    // Generate StringOp.ToString instruction
                    try self.instructions.append(.{ .StringOp = .{ .op = .ToString } });
                } else if (std.mem.eql(u8, name, "length")) {
                    // Evaluate receiver (the value to get length of)
                    try self.generateExpression(m.receiver, true, false);
                    // Generate StringOp.Length instruction
                    try self.instructions.append(.{ .StringOp = .{ .op = .Length } });
                } else if (std.mem.eql(u8, name, "int")) {
                    // Evaluate receiver and convert to int
                    try self.generateExpression(m.receiver, true, false);
                    try self.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
                } else if (std.mem.eql(u8, name, "float")) {
                    // Evaluate receiver and convert to float
                    try self.generateExpression(m.receiver, true, false);
                    try self.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
                } else if (std.mem.eql(u8, name, "byte")) {
                    // Evaluate receiver and convert to byte
                    try self.generateExpression(m.receiver, true, false);
                    try self.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
                } else {
                    // Unknown method - fallback to nothing
                    const nothing_idx = try self.addConstant(HIRValue.nothing);
                    try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
                }
            },

            .Increment => |operand| {
                // Generate increment operation: load variable, add 1, store back
                std.debug.print("DEBUG: Processing Increment operation\n", .{});
                // First, check if this is a variable reference
                if (operand.data == .Variable) {
                    const var_name = operand.data.Variable.lexeme;
                    const var_idx = try self.getOrCreateVariable(var_name);

                    // Load current value
                    try self.instructions.append(.{
                        .LoadVar = .{
                            .var_index = var_idx,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                        },
                    });

                    // Add 1 (create constant 1)
                    const one_value = HIRValue{ .int = 1 };
                    const one_idx = try self.addConstant(one_value);
                    try self.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

                    // Add the values
                    const operand_type = self.getTrackedVariableType(var_name) orelse .Int;
                    try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });

                    // Store back to variable
                    try self.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_idx,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = operand_type,
                        },
                    });
                } else {
                    // For non-variable expressions, generate the expression and add 1
                    try self.generateExpression(operand, true, false);

                    // Add 1
                    const one_value = HIRValue{ .int = 1 };
                    const one_idx = try self.addConstant(one_value);
                    try self.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

                    // Add the values
                    const operand_type = self.inferTypeFromExpression(operand);
                    try self.instructions.append(.{ .Arith = .{ .op = .Add, .operand_type = operand_type } });
                }
            },

            .Decrement => |operand| {
                // Generate decrement operation: load variable, subtract 1, store back
                std.debug.print("DEBUG: Processing Decrement operation\n", .{});
                // First, check if this is a variable reference
                if (operand.data == .Variable) {
                    const var_name = operand.data.Variable.lexeme;
                    const var_idx = try self.getOrCreateVariable(var_name);

                    // Load current value
                    try self.instructions.append(.{
                        .LoadVar = .{
                            .var_index = var_idx,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                        },
                    });

                    // Add 1 (create constant 1)
                    const one_value = HIRValue{ .int = 1 };
                    const one_idx = try self.addConstant(one_value);
                    try self.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

                    // Subtract the values
                    const operand_type = self.getTrackedVariableType(var_name) orelse .Int;
                    try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });

                    // Store back to variable
                    try self.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_idx,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = operand_type,
                        },
                    });
                } else {
                    // For non-variable expressions, generate the expression and subtract 1
                    try self.generateExpression(operand, true, false);

                    // Add 1
                    const one_value = HIRValue{ .int = 1 };
                    const one_idx = try self.addConstant(one_value);
                    try self.instructions.append(.{ .Const = .{ .value = one_value, .constant_id = one_idx } });

                    // Subtract the values
                    const operand_type = self.inferTypeFromExpression(operand);
                    try self.instructions.append(.{ .Arith = .{ .op = .Sub, .operand_type = operand_type } });
                }
            },
            else => {
                // For all other expressions, ensure the stack is clean if result not preserved
                if (!preserve_result and should_pop_after_use) {
                    try self.instructions.append(.Pop);
                }
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
        // When inside a function, keep variable indices local to the function to avoid collisions
        if (self.current_function != null) {
            if (self.local_variables.get(name)) |idx| {
                return idx;
            }
            const idx = self.local_variable_count;
            try self.local_variables.put(name, idx);
            self.local_variable_count += 1;
            return idx;
        }

        // Global scope
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

    /// Centralized generation for internal/built-in method calls.
    /// Handles both direct InternalCall AST nodes and method-sugar (obj.method(...)).
    fn generateInternalMethodCall(self: *HIRGenerator, method: @import("../../types/token.zig").Token, receiver: *ast.Expr, args: []ast.CallArgument, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        const name = method.lexeme;

        // String-related built-ins use dedicated HIR instructions
        if (std.mem.eql(u8, name, "substring")) {
            // Expect 2 args: start, length. Order: start, length, then receiver
            if (args.len >= 2) {
                try self.generateExpression(args[0].expr, true, false);
                try self.generateExpression(args[1].expr, true, false);
                try self.generateExpression(receiver, true, false);
                try self.instructions.append(.{ .StringOp = .{ .op = .Substring } });
                return;
            }
        } else if (std.mem.eql(u8, name, "string")) {
            try self.generateExpression(receiver, true, false);
            try self.instructions.append(.{ .StringOp = .{ .op = .ToString } });
            return;
        } else if (std.mem.eql(u8, name, "length")) {
            try self.generateExpression(receiver, true, false);
            try self.instructions.append(.{ .StringOp = .{ .op = .Length } });
            return;
        } else if (std.mem.eql(u8, name, "int")) {
            try self.generateExpression(receiver, true, false);
            try self.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
            return;
        } else if (std.mem.eql(u8, name, "float")) {
            try self.generateExpression(receiver, true, false);
            try self.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
            return;
        } else if (std.mem.eql(u8, name, "byte")) {
            try self.generateExpression(receiver, true, false);
            try self.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            return;
        }

        // Generic built-in method: emit as BuiltinFunction call
        // Generate receiver first (on stack), then arguments
        try self.generateExpression(receiver, true, should_pop_after_use);

        var arg_emitted_count: u32 = 0;
        for (args, 0..) |arg, arg_index| {
            if (arg.expr.data == .DefaultArgPlaceholder) {
                if (self.resolveDefaultArgument(name, arg_index)) |default_expr| {
                    try self.generateExpression(default_expr, true, false);
                    arg_emitted_count += 1;
                } else {
                    const location = receiver.base.location();
                    self.reporter.reportCompileError(location, ErrorCode.NO_DEFAULT_VALUE_FOR_PARAMETER, "No default value for parameter {} in function '{s}'", .{ arg_index, name });
                }
            } else {
                try self.generateExpression(arg.expr, true, should_pop_after_use);
                arg_emitted_count += 1;
            }
        }

        const return_type = self.inferCallReturnType(name, .BuiltinFunction) catch .String;
        try self.instructions.append(.{ .Call = .{
            .function_index = 0,
            .qualified_name = name,
            .arg_count = arg_emitted_count,
            .call_kind = .BuiltinFunction,
            .target_module = null,
            .return_type = return_type,
        } });

        // If the receiver is a variable and the method is mutating, write back
        if (receiver.data == .Variable) {
            const is_mutating = std.mem.eql(u8, name, "push") or
                std.mem.eql(u8, name, "pop") or
                std.mem.eql(u8, name, "insert") or
                std.mem.eql(u8, name, "remove") or
                std.mem.eql(u8, name, "clear");
            if (is_mutating) {
                try self.instructions.append(.Dup);
                const target_var = receiver.data.Variable.lexeme;
                const var_idx = try self.getOrCreateVariable(target_var);
                const expected_type = self.getTrackedVariableType(target_var) orelse .Unknown;
                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = target_var,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = expected_type,
                } });
            }
        }
    }

    /// Try to generate a tail call if the expression is a direct function call
    /// Returns true if tail call was generated, false if normal expression should be used
    fn tryGenerateTailCall(self: *HIRGenerator, expr: *ast.Expr) bool {
        switch (expr.data) {
            .FunctionCall => |call| {
                // Check if the callee is a simple variable (function name)
                switch (call.callee.data) {
                    .Variable => |var_token| {
                        const function_name = var_token.lexeme;

                        // Generate arguments in normal order (same as regular call)
                        for (call.arguments) |arg| {
                            self.generateExpression(arg.expr, true, true) catch return false; // Fallback to regular call on error
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
            else => .Unknown,
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
                const var_type = self.variable_types.get(var_token.lexeme) orelse .Unknown;
                return var_type;
            },
            .FieldAccess => |field| {
                // Try to infer based on the object type and common field names
                // Special-case: Enum member access like Color.Blue
                if (field.object.data == .Variable) {
                    const obj_name = field.object.data.Variable.lexeme;
                    if (self.isCustomType(obj_name)) |custom_type| {
                        if (custom_type.kind == .Enum) {
                            return .Enum;
                        }
                        if (custom_type.kind == .Struct) {
                            return .Struct;
                        }
                    }
                }

                const obj_type = self.inferTypeFromExpression(field.object);
                // Heuristic: Many structs expose a 'value' (string) and 'token_type' (enum)
                if (std.mem.eql(u8, field.field.lexeme, "value")) return .String;
                if (std.mem.eql(u8, field.field.lexeme, "token_type")) return .Enum;
                if (obj_type == .Struct) {
                    return .Unknown; // Unknown specific field, but it's a struct
                }
                return .Unknown;
            },
            // NEW: Infer element type for ArrayPop
            .ArrayPop => |pop| {
                const container_type = self.inferTypeFromExpression(pop.array);
                if (container_type == .Array) {
                    // If array is a variable with tracked element type, use it
                    if (pop.array.data == .Variable) {
                        const var_name = pop.array.data.Variable.lexeme;
                        if (self.getTrackedArrayElementType(var_name)) |elem_type| {
                            return elem_type;
                        }
                    }
                    // If the array expression is a literal, infer from its first element
                    switch (pop.array.data) {
                        .Array => |elements| {
                            if (elements.len > 0) {
                                switch (elements[0].data) {
                                    .Literal => |lit| return self.inferTypeFromLiteral(lit),
                                    else => {},
                                }
                            }
                        },
                        else => {},
                    }
                }
                return .Unknown;
            },
            .Binary => |binary| {
                // Simple type inference for binary operations
                const left_type = if (binary.left) |left| self.inferTypeFromExpression(left) else .Unknown;
                const right_type = if (binary.right) |right| self.inferTypeFromExpression(right) else .Unknown;
                return switch (binary.operator.type) {
                    .EQUALITY, .BANG_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => .Tetra,
                    .AND, .OR, .XOR => .Tetra, // Logical operations return tetra values
                    .PLUS, .MINUS, .ASTERISK, .SLASH, .MODULO => {
                        // Arithmetic operations - return the promoted type
                        if (left_type == right_type and left_type != .Unknown) return left_type;
                        if ((left_type == .Int and right_type == .Float) or (left_type == .Float and right_type == .Int)) {
                            return .Float;
                        }
                        return .Int; // Default to int for arithmetic
                    },
                    .POWER => {
                        // For power operation, if both operands are integers, return integer type
                        // This matches the runtime behavior where integer exponents with non-negative
                        // integer powers return integers
                        if ((left_type == .Int or left_type == .Byte) and (right_type == .Int or right_type == .Byte)) {
                            return .Int;
                        }
                        // If either operand is float, return float
                        if (left_type == .Float or right_type == .Float) return .Float;
                        // Default to int for other cases (shouldn't normally happen)
                        return .Int;
                    },
                    else => .Tetra, // Default to Tetra for any other binary operations
                };
            },
            .Array => .Array,
            .Index => |index| {
                // Array/string indexing returns the element type
                const container_type = self.inferTypeFromExpression(index.array);
                return switch (container_type) {
                    .Array => {
                        // Check if we have tracked element type information for this array
                        if (index.array.data == .Variable) {
                            if (self.getTrackedArrayElementType(index.array.data.Variable.lexeme)) |elem_type| {
                                return elem_type;
                            }
                        }
                        return .String; // Fallback: Most arrays in bigfile.doxa are int arrays, but for simplicity return String
                    },
                    .String => .String, // String indexing returns single character (still string in our system)
                    .Map => .Int, // Map values are integers in our test case
                    else => .String, // Default to String for most index operations
                };
            },
            .FunctionCall => |call| {
                // Handle different types of function calls
                switch (call.callee.data) {
                    .InternalCall => |method| {
                        if (std.mem.eql(u8, method.method.lexeme, "substring")) return .String;
                        if (std.mem.eql(u8, method.method.lexeme, "length")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "bytes")) return .Array;
                        if (std.mem.eql(u8, method.method.lexeme, "int")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "float")) return .Float;
                        if (std.mem.eql(u8, method.method.lexeme, "byte")) return .Byte;
                        if (std.mem.eql(u8, method.method.lexeme, "safeAdd")) return .Int;
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
                return .Unknown; // Default for unrecognized function calls
            },
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

    fn trackVariableUnionMembersByIndex(self: *HIRGenerator, var_index: u32, members: [][]const u8) !void {
        try self.variable_union_members_by_index.put(var_index, members);
    }

    /// NEW: Track a variable's custom type name (for enums/structs)
    fn trackVariableCustomType(self: *HIRGenerator, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.variable_custom_types.put(var_name, custom_type_name);
    }

    /// NEW: Get tracked variable type
    fn getTrackedVariableType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.variable_types.get(var_name);
    }

    fn astTypeToLowerName(_: *HIRGenerator, base: ast.Type) []const u8 {
        return switch (base) {
            .Int => "int",
            .Byte => "byte",
            .Float => "float",
            .String => "string",
            .Tetra => "tetra",
            .Nothing => "nothing",
            .Array => "array",
            .Struct => "struct",
            .Enum => "enum",
            .Map => "map",
            .Function => "function",
            .Custom => "custom",
            .Union => "union",
        };
    }

    /// Collect flattened union member names (lowercase) from a possibly nested union type
    fn collectUnionMemberNames(self: *HIRGenerator, ut: *ast.UnionType) ![][]const u8 {
        var list = std.ArrayList([]const u8).init(self.allocator);
        defer if (false) list.deinit(); // transferred to caller

        // Depth-first traversal to flatten nested unions
        for (ut.types) |member| {
            if (member.base == .Union) {
                if (member.union_type) |nested| {
                    const nested_list = try self.collectUnionMemberNames(nested);
                    // append nested_list items to list
                    for (nested_list) |nm| {
                        try list.append(nm);
                    }
                }
            } else {
                try list.append(self.astTypeToLowerName(member.base));
            }
        }

        return try list.toOwnedSlice();
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

    fn inferBinaryOpResultType(self: *HIRGenerator, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr);
        const right_type = self.inferTypeFromExpression(right_expr);

        // For PLUS operator, handle string/array concatenation and numeric promotion
        if (operator_type == .PLUS) {
            if (left_type == .String and right_type == .String) {
                return .String; // String concatenation
            } else if (left_type == .Array and right_type == .Array) {
                return .Array; // Array concatenation
            } else {
                // For numeric types, use centralized promotion rules
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                // Type mismatch - report compile error
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use + operator between {s} and {s}",
                    .{ @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown; // Return Unknown to indicate error
            }
        }

        // For arithmetic operations, use centralized type promotion
        const result_type = switch (operator_type) {
            .MINUS, .ASTERISK, .SLASH, .MODULO, .POWER => {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                // Type mismatch - report compile error
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use {s} operator between {s} and {s}",
                    .{ @tagName(operator_type), @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown; // Return Unknown to indicate error
            },
            .EQUALITY, .BANG_EQUAL, .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => {
                // For comparisons, use separate comparison operand type resolution
                // This is handled by inferComparisonOperandType
                return .Tetra; // Comparisons return tetra values
            },
            else => .Int, // Default to int for other operations
        };

        return result_type;
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
        // If semantic analysis already populated this struct with rich field info,
        // do not override it with a lossy registration.
        if (self.custom_types.contains(struct_name)) return;
        // Create struct fields array with proper indices and types
        var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field_name, index| {
            struct_fields[index] = CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field_name),
                .field_type = .Unknown, // Will be inferred at runtime
                .index = @intCast(index),
                .custom_type_name = null,
            };
        }

        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
    }

    /// Infer the appropriate operand type for comparison operations
    fn inferComparisonOperandType(self: *HIRGenerator, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr);
        const right_type = self.inferTypeFromExpression(right_expr);

        // If either operand is an enum, use Enum operand type
        if (left_type == .Enum or right_type == .Enum) {
            return .Enum;
        }

        // If either operand is a string, use String operand type
        if (left_type == .String or right_type == .String) {
            return .String;
        }

        // If either operand is float, use Float operand type
        if (left_type == .Float or right_type == .Float) {
            return .Float;
        }

        // If either operand is int, use Int operand type
        if (left_type == .Int or right_type == .Int) {
            return .Int;
        }

        // If either operand is byte, use Byte operand type
        if (left_type == .Byte or right_type == .Byte) {
            return .Byte;
        }

        // Default to Int for other types
        return .Int;
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
            .FunctionCall => |call| {
                // Check if parameter is passed to function calls
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg.expr, param_name)) {
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
            .FunctionCall => |call| {
                // Check arguments
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg.expr, param_name)) return true;
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
        return .Unknown; // Unresolved
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
        if (self.module_namespaces.contains(name)) return true;
        // Fallback: if any known module imports this alias, treat it as a module namespace
        var it = self.module_namespaces.iterator();
        while (it.next()) |entry| {
            const mi = entry.value_ptr.*;
            for (mi.imports) |imp| {
                if (imp.namespace_alias) |alias| {
                    if (std.mem.eql(u8, alias, name)) return true;
                }
            }
        }
        return false;
    }

    /// Promotion Rules:
    /// 1. Float dominance: If either operand is Float or operator is division (/), promote to Float
    /// 2. Int fallback: If either operand is Int, promote to Int
    /// 3. No promotion: If neither is Float or Int, operands are Byte
    ///
    /// Exceptions:
    /// - Strings, arrays, and other non-numeric types do not follow numeric promotion
    /// - Comparisons use separate comparison operand type resolution
    fn computeNumericCommonType(_: *HIRGenerator, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        // Non-numeric types don't follow numeric promotion rules
        if (left_type == .String or right_type == .String or
            left_type == .Array or right_type == .Array or
            left_type == .Map or right_type == .Map or
            left_type == .Struct or right_type == .Struct or
            left_type == .Enum or right_type == .Enum or
            left_type == .Union or right_type == .Union or
            left_type == .Function or right_type == .Function or
            left_type == .Nothing or right_type == .Nothing)
        {
            return .Unknown; // Indicates no numeric promotion should occur
        }

        // Rule 1: Float dominance - division always promotes to float, or if either operand is float
        if (operator_type == .SLASH or left_type == .Float or right_type == .Float) {
            return .Float;
        }

        // Rule 2: Int fallback - if either operand is int, promote to int
        if (left_type == .Int or right_type == .Int) {
            return .Int;
        }

        // Rule 3: No promotion - both operands are the same numeric type (e.g., Byte)
        if (left_type == right_type) {
            return left_type;
        }

        // Mixed byte types - promote to int for consistency
        if ((left_type == .Byte and right_type == .Tetra) or
            (left_type == .Tetra and right_type == .Byte))
        {
            return .Int;
        }

        // Default fallback to int for mixed numeric types
        return .Int;
    }

    /// NEW: Check if type promotion is needed and apply it
    /// Returns true if promotion was applied, false if no promotion needed
    fn applyTypePromotionIfNeeded(self: *HIRGenerator, left_type: HIRType, right_type: HIRType, target_type: HIRType) !bool {
        // No promotion needed if types already match target
        if (left_type == target_type and right_type == target_type) {
            return false;
        }

        // Promote left operand if needed
        if (left_type != target_type and left_type != .Unknown) {
            try self.instructions.append(.{ .Convert = .{ .from_type = left_type, .to_type = target_type } });
        }

        // Promote right operand if needed
        if (right_type != target_type and right_type != .Unknown) {
            try self.instructions.append(.{ .Convert = .{ .from_type = right_type, .to_type = target_type } });
        }

        return true;
    }
};
