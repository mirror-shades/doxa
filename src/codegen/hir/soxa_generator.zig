const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenImport = @import("../../types/token.zig");
const TokenType = TokenImport.TokenType;
const Token = TokenImport.Token;
const SoxaInstructions = @import("soxa_instructions.zig");
const SoxaStatements = @import("soxa_statements.zig");
pub const HIRInstruction = SoxaInstructions.HIRInstruction;
const SoxaValues = @import("soxa_values.zig");
pub const HIRValue = SoxaValues.HIRValue;
const HIREnum = SoxaValues.HIREnum;
pub const HIRMapEntry = SoxaValues.HIRMapEntry;
const SoxaTypes = @import("soxa_types.zig");
pub const HIRType = SoxaTypes.HIRType;
const CallKind = SoxaTypes.CallKind;
const HIRProgram = SoxaTypes.HIRProgram;
const import_parser = @import("../../parser/import_parser.zig");

// New component imports
const ResourceManager = @import("resource_manager.zig");
const LabelGenerator = ResourceManager.LabelGenerator;
const ConstantManager = ResourceManager.ConstantManager;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const TypeSystem = @import("type_system.zig").TypeSystem;

const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const SemanticAnalyzer = @import("../../analysis/semantic/semantic.zig");
const StructMethodInfo = SemanticAnalyzer.StructMethodInfo;

// Expression handler imports
const BasicHandler = @import("expressions/basic.zig").BasicExpressionHandler;
const BinaryHandler = @import("expressions/binary.zig").BinaryExpressionHandler;
const ControlFlowHandler = @import("expressions/control_flow.zig").ControlFlowHandler;
const CollectionsHandler = @import("expressions/collections.zig").CollectionsHandler;
const CallsHandler = @import("expressions/calls.zig").CallsHandler;
const StructsHandler = @import("expressions/structs.zig").StructsHandler;
const AssignmentsHandler = @import("expressions/assignments.zig").AssignmentsHandler;
const IOHandler = @import("expressions/io.zig").IOHandler;

// Tetra constants for fast operations
pub const TETRA_FALSE: u8 = 0; // 00
pub const TETRA_TRUE: u8 = 1; // 01
pub const TETRA_BOTH: u8 = 2; // 10
pub const TETRA_NEITHER: u8 = 3; // 11

pub const HIRGenerator = struct {
    allocator: std.mem.Allocator,
    instructions: std.array_list.Managed(HIRInstruction),
    current_peek_expr: ?*ast.Expr = null,
    current_field_name: ?[]const u8 = null,
    string_pool: std.array_list.Managed([]const u8),
    reporter: *Reporter,

    // New composed components
    symbol_table: SymbolTable,
    constant_manager: ConstantManager,
    label_generator: LabelGenerator,
    type_system: TypeSystem,
    struct_methods: std.StringHashMap(std.StringHashMap(StructMethodInfo)), // Track struct methods

    function_signatures: std.StringHashMap(FunctionInfo),
    function_bodies: std.array_list.Managed(FunctionBody),
    current_function: ?[]const u8,
    current_function_return_type: HIRType, // Track current function's return type for Return instructions

    stats: HIRStats,

    function_calls: std.array_list.Managed(FunctionCallSite),

    module_namespaces: std.StringHashMap(ast.ModuleInfo), // Track module namespaces for function call resolution

    // Map of specifically imported symbols (unqualified names)
    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    current_enum_type: ?[]const u8 = null,

    // Loop context stack to support break/continue codegen
    loop_context_stack: std.array_list.Managed(LoopContext),

    // Track when we're generating nested array elements to avoid stack interference
    is_generating_nested_array: bool = false,

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

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, module_namespaces: std.StringHashMap(ast.ModuleInfo), imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol)) HIRGenerator {
        return HIRGenerator{
            .allocator = allocator,
            .instructions = std.array_list.Managed(HIRInstruction).init(allocator),
            .current_peek_expr = null,
            .string_pool = std.array_list.Managed([]const u8).init(allocator),
            .reporter = reporter,

            // Initialize new composed components
            .symbol_table = SymbolTable.init(allocator),
            .constant_manager = ConstantManager.init(allocator),
            .label_generator = LabelGenerator.init(allocator),
            .type_system = TypeSystem.init(allocator, reporter),
            .struct_methods = std.StringHashMap(std.StringHashMap(StructMethodInfo)).init(allocator),
            .function_signatures = std.StringHashMap(FunctionInfo).init(allocator),
            .function_bodies = std.array_list.Managed(FunctionBody).init(allocator),
            .current_function = null,
            .current_function_return_type = .Nothing,
            .function_calls = std.array_list.Managed(FunctionCallSite).init(allocator),
            .module_namespaces = module_namespaces,
            .imported_symbols = imported_symbols,
            .current_enum_type = null,
            .stats = HIRStats.init(allocator),
            .loop_context_stack = std.array_list.Managed(LoopContext).init(allocator),
        };
    }

    pub fn deinit(self: *HIRGenerator) void {
        self.instructions.deinit();
        self.string_pool.deinit();

        // Deinit new composed components
        self.symbol_table.deinit();
        self.constant_manager.deinit();
        self.label_generator.deinit();
        self.type_system.deinit();
        var methods_it = self.struct_methods.valueIterator();
        while (methods_it.next()) |tbl| tbl.*.deinit();
        self.struct_methods.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_calls.deinit();
        self.loop_context_stack.deinit();
        // Note: module_namespaces is not owned by HIRGenerator, so we don't deinit it
        // Note: imported_symbols is not owned by HIRGenerator, so we don't deinit it
    }

    pub inline fn pushLoopContext(self: *HIRGenerator, break_label: []const u8, continue_label: []const u8) !void {
        try self.loop_context_stack.append(.{ .break_label = break_label, .continue_label = continue_label });
    }

    pub inline fn popLoopContext(self: *HIRGenerator) void {
        if (self.loop_context_stack.items.len > 0) {
            _ = self.loop_context_stack.pop();
        }
    }

    pub inline fn currentLoopContext(self: *HIRGenerator) ?LoopContext {
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
            .constant_pool = try self.constant_manager.toOwnedSlice(),
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
                    const start_label = try self.label_generator.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{func.name.lexeme}));

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
                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        if (expr.data == .StructDecl) {
                            const s = expr.data.StructDecl;
                            // Register struct methods (static and instance) as functions: StructName.MethodName
                            for (s.methods) |method| {
                                const qualified = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ s.name.lexeme, method.name.lexeme });
                                const return_type = self.convertTypeInfo(method.return_type_info);
                                const start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{qualified}));

                                var param_is_alias = try self.allocator.alloc(bool, method.params.len);
                                var param_types = try self.allocator.alloc(HIRType, method.params.len);
                                for (method.params, 0..) |param, i| {
                                    param_is_alias[i] = param.is_alias;
                                    param_types[i] = if (param.type_expr) |type_expr| self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*) else .Int;
                                }

                                // Compute arity at runtime
                                var arity: u32 = @intCast(method.params.len);
                                if (!method.is_static) arity += 1;

                                const function_info = FunctionInfo{
                                    .name = qualified,
                                    .arity = arity,
                                    .return_type = return_type,
                                    .start_label = start_label,
                                    .local_var_count = 0,
                                    .is_entry = false,
                                    .param_is_alias = param_is_alias,
                                    .param_types = param_types,
                                };

                                // Avoid duplicates if same struct appears multiple times
                                if (!self.function_signatures.contains(qualified)) {
                                    try self.function_signatures.put(qualified, function_info);
                                    try self.function_bodies.append(FunctionBody{
                                        .function_info = function_info,
                                        .statements = method.body,
                                        .start_instruction_index = 0,
                                        .function_name = qualified,
                                        .function_params = method.params,
                                        .return_type_info = method.return_type_info,
                                        .param_is_alias = param_is_alias,
                                        .param_types = param_types,
                                    });
                                }
                            }
                        }
                    }
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
            try self.symbol_table.enterFunctionScope(function_body.function_info.name);

            // Mark start of function
            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label, .vm_address = 0 } });

            // Enter function scope
            const function_scope_id = self.label_generator.label_count + 1000;
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = function_scope_id, .var_count = 0 } });

            // Generate parameter setup - copy arguments from stack to local variables
            const params = function_body.function_params;

            // Process ALL parameters in reverse order (both alias and regular) since they're all popped from the same LIFO stack
            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                if (function_body.param_is_alias[param_index]) {
                    // Alias parameter: bind to incoming storage ID
                    var param_type: HIRType = .Unknown;
                    if (param.type_expr) |type_expr| {
                        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                        defer self.allocator.destroy(type_info_ptr);
                        param_type = self.convertTypeInfo(type_info_ptr.*);
                    } else {
                        param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
                    }

                    try self.trackVariableType(param.name.lexeme, param_type);

                    // Handle custom types for alias parameters
                    if (param.type_expr) |type_expr_for_custom| {
                        const type_info_for_custom = try ast.typeInfoFromExpr(self.allocator, type_expr_for_custom);
                        // Explicit struct parameter: track custom type so instance methods resolve
                        if (type_info_for_custom.base == .Struct) {
                            if (type_info_for_custom.custom_type) |struct_type_name_for_param| {
                                try self.trackVariableCustomType(param.name.lexeme, struct_type_name_for_param);
                                try self.trackVariableType(param.name.lexeme, .Struct);
                            }
                            // Explicit enum parameter
                        } else if (type_info_for_custom.base == .Enum) {
                            if (type_info_for_custom.custom_type) |enum_type_name_for_param| {
                                try self.trackVariableCustomType(param.name.lexeme, enum_type_name_for_param);
                                try self.trackVariableType(param.name.lexeme, .Enum);
                            }
                            // Custom(named) type: resolve whether it's a struct or enum
                        } else if (type_info_for_custom.base == .Custom) {
                            if (type_info_for_custom.custom_type) |custom_type_name_for_param| {
                                if (self.isCustomType(custom_type_name_for_param)) |ct| {
                                    switch (ct.kind) {
                                        .Struct => {
                                            try self.trackVariableCustomType(param.name.lexeme, custom_type_name_for_param);
                                            try self.trackVariableType(param.name.lexeme, .Struct);
                                        },
                                        .Enum => {
                                            try self.trackVariableCustomType(param.name.lexeme, custom_type_name_for_param);
                                            try self.trackVariableType(param.name.lexeme, .Enum);
                                        },
                                    }
                                }
                            }
                        }
                    }

                    // For alias parameters, bind the alias to the incoming storage id
                    // Use the actual parameter position (not the reverse index)
                    const param_position = function_body.function_params.len - param_index;
                    try self.instructions.append(.{ .StoreParamAlias = .{
                        .param_name = param.name.lexeme,
                        .param_type = param_type,
                        .var_index = @intCast(param_position),
                    } });
                } else {
                    // Regular parameter: create local variable and store stack value
                    var param_type: HIRType = .Unknown;
                    if (param.type_expr) |type_expr| {
                        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                        defer self.allocator.destroy(type_info_ptr);
                        param_type = self.convertTypeInfo(type_info_ptr.*);
                    } else {
                        param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
                    }

                    try self.trackVariableType(param.name.lexeme, param_type);

                    // Create local variable and store stack value
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

            // Handle implicit receiver for instance methods: alias-bind 'this'
            // Only for struct instance methods (not static and not free functions)
            if (std.mem.indexOfScalar(u8, function_body.function_info.name, '.')) |dot_idx| {
                const struct_name = function_body.function_info.name[0..dot_idx];
                const method_name = function_body.function_info.name[dot_idx + 1 ..];
                if (self.struct_methods.get(struct_name)) |method_table| {
                    if (method_table.get(method_name)) |mi| {
                        if (!mi.is_static) {
                            // For instance methods, automatically bind implicit 'this' as an alias parameter.
                            // Caller is expected to have pushed a storage id reference first.
                            // Track type info for 'this' for downstream inference.
                            try self.trackVariableType("this", .Struct);
                            // Bind alias parameter named 'this'. var_index is informational here.
                            try self.instructions.append(.{ .StoreParamAlias = .{
                                .param_name = "this",
                                .param_type = .Struct,
                                .var_index = 1,
                            } });
                        }
                        // Functions (static) don't get implicit 'this'
                    }
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

                try SoxaStatements.generateStatement(self, body_stmt);

                // Check if this statement definitely returns (for dead code elimination)
                has_returned = self.statementAlwaysReturns(body_stmt);
            }

            // Exit function scope
            try self.instructions.append(.{ .ExitScope = .{ .scope_id = function_scope_id } });

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
            self.symbol_table.exitFunctionScope();
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
                                try SoxaStatements.generateStatement(self, ast.Stmt{ .base = mod_stmt.base, .data = .{ .VarDecl = decl } });
                            },
                            else => try SoxaStatements.generateStatement(self, mod_stmt),
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
                    try SoxaStatements.generateStatement(self, stmt);
                },
            }
        }

        // After processing global statements, call main if it exists
        if (self.function_signatures.get("main")) |main_func| {
            // Get the correct function index for main
            const main_function_index = self.getFunctionIndex("main") orelse 0;

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
        var function_table = std.array_list.Managed(HIRProgram.HIRFunction).init(self.allocator);

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
                .start_ip = 0,
                .body_ip = null,
                .local_var_count = function_info.local_var_count,
                .is_entry = function_info.is_entry,
                .param_is_alias = function_body.param_is_alias, // Use from function_body
                .param_types = function_body.param_types, // Use from function_body
            });
        }

        return try function_table.toOwnedSlice();
    }

    /// Get function index for call generation
    pub fn getFunctionIndex(self: *HIRGenerator, function_name: []const u8) ?u32 {
        // Use function_bodies order for deterministic indices
        for (self.function_bodies.items, 0..) |function_body, index| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return @as(u32, @intCast(index));
            }
        }
        return null;
    }

    /// Check if a function is a module function
    pub fn isModuleFunction(self: *HIRGenerator, function_name: []const u8) bool {
        // Check if the function is in any of the imported modules
        if (self.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(function_name)) |imported_symbol| {
                return imported_symbol.kind == .Function;
            }
        }
        return false;
    }

    /// Convert TypeInfo to HIRType
    fn convertTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) HIRType {
        return self.type_system.convertTypeInfo(type_info);
    }

    /// Find function body by name (helper method)
    pub fn findFunctionBody(self: *HIRGenerator, function_name: []const u8) ?*FunctionBody {
        for (self.function_bodies.items) |*function_body| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return function_body;
            }
        }
        return null;
    }

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

    pub fn generateExpression(self: *HIRGenerator, expr: *ast.Expr, preserve_result: bool, should_pop_after_use: bool) ErrorList!void {
        // Type checking is now handled by the semantic analyzer before HIR generation.
        // No direct `type_info` field on `ast.Expr` anymore.
        // We will assume expressions passed here have valid type information.

        // Initialize handlers
        var basic_handler = BasicHandler.init(self);
        var binary_handler = BinaryHandler.init(self);
        var control_flow_handler = ControlFlowHandler.init(self);
        var collections_handler = CollectionsHandler.init(self);
        var calls_handler = CallsHandler.init(self);
        var structs_handler = StructsHandler.init(self);
        var assignments_handler = AssignmentsHandler.init(self);
        var io_handler = IOHandler.init(self);

        switch (expr.data) {
            // Basic expressions
            .This => try basic_handler.generateThis(),
            .Literal => |lit| try basic_handler.generateLiteral(lit),
            .Variable => |var_token| try basic_handler.generateVariable(var_token),
            .Grouping => |grouping| try basic_handler.generateGrouping(grouping, preserve_result),
            .EnumMember => |member| try basic_handler.generateEnumMember(member),
            .DefaultArgPlaceholder => try basic_handler.generateDefaultArgPlaceholder(),

            // Binary and unary operations
            .Binary => |bin| try binary_handler.generateBinary(bin, should_pop_after_use),
            .Logical => |log| try binary_handler.generateLogical(log, should_pop_after_use),
            .Unary => |unary| try binary_handler.generateUnary(unary),

            // Control flow
            .If => |if_expr| try control_flow_handler.generateIf(if_expr, preserve_result, should_pop_after_use),
            .Match => |match_expr| try control_flow_handler.generateMatch(match_expr),
            .Loop => |loop| try control_flow_handler.generateLoop(loop, preserve_result),
            .Block => try control_flow_handler.generateBlock(expr.data, preserve_result),
            .ReturnExpr => try control_flow_handler.generateReturn(expr.data),
            .Cast => try control_flow_handler.generateCast(expr.data, preserve_result),

            // Collections
            .Array => |elements| {
                if (self.is_generating_nested_array) {
                    // We're already inside a nested array context, use the internal method
                    try collections_handler.generateArrayInternal(elements, preserve_result);
                } else {
                    // Normal top-level array
                    try collections_handler.generateArray(elements, preserve_result);
                }
            },
            .Map => |entries| try collections_handler.generateMap(entries),
            .Index => |index| try collections_handler.generateIndex(index, preserve_result, should_pop_after_use),
            .IndexAssign => try collections_handler.generateIndexAssign(expr.data, preserve_result),
            .ForAll => try collections_handler.generateForAll(expr.data),
            .Exists => try collections_handler.generateExists(expr.data),
            .Increment => |operand| try collections_handler.generateIncrement(operand),
            .Decrement => |operand| try collections_handler.generateDecrement(operand),
            .Range => |range| try collections_handler.generateRange(.{ .start = range.start, .end = range.end }, preserve_result),

            // Function and method calls
            .FunctionCall => try calls_handler.generateFunctionCall(expr.data, should_pop_after_use),
            .BuiltinCall => try calls_handler.generateBuiltinCall(expr.data, preserve_result),
            .InternalCall => try calls_handler.generateInternalCall(expr.data),

            // Struct and type operations
            .StructLiteral => try structs_handler.generateStructLiteral(expr.data),
            .FieldAccess => |field| try structs_handler.generateFieldAccess(field),
            .FieldAssignment => try structs_handler.generateFieldAssignment(expr.data),
            .EnumDecl => try structs_handler.generateEnumDecl(expr.data),
            .StructDecl => try structs_handler.generateStructDecl(expr.data),

            // Assignments
            .Assignment => |assign| try assignments_handler.generateAssignment(assign, preserve_result),
            .CompoundAssign => |compound| try assignments_handler.generateCompoundAssign(compound, preserve_result),

            // I/O and debugging
            .Print => |print| try io_handler.generatePrint(print, preserve_result),
            .Peek => |peek| try io_handler.generatePeek(peek, preserve_result),
            .PeekStruct => try io_handler.generatePeekStruct(expr.data, preserve_result),
            .Input => try io_handler.generateInput(expr.data),

            // Fallback for unhandled cases
            else => {
                // For all other expressions, ensure the stack is clean if result not preserved
                if (!preserve_result and should_pop_after_use) {
                    try self.instructions.append(.Pop);
                }
            },
        }
    }

    pub fn addConstant(self: *HIRGenerator, value: HIRValue) std.mem.Allocator.Error!u32 {
        return self.constant_manager.addConstant(value);
    }

    pub fn getOrCreateVariable(self: *HIRGenerator, name: []const u8) !u32 {
        return self.symbol_table.getOrCreateVariable(name);
    }

    pub fn generateLabel(self: *HIRGenerator, prefix: []const u8) ![]const u8 {
        return self.label_generator.generateLabel(prefix);
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
    pub fn generateInternalMethodCall(self: *HIRGenerator, method: Token, receiver: *ast.Expr, args: []ast.CallArgument, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        const name = method.lexeme;

        // Check if this is a struct method call
        const receiver_type = self.inferTypeFromExpression(receiver);
        if (receiver_type == .Struct) {
            // Try to find the struct type and method
            if (receiver.data == .Variable) {
                // Resolve the receiver's struct type name (variable instance -> type name)
                const recv_var_name = receiver.data.Variable.lexeme;
                const struct_name = blk: {
                    if (self.symbol_table.getVariableCustomType(recv_var_name)) |ctype| break :blk ctype;
                    // Fallback: if the variable name itself is a type name
                    break :blk recv_var_name;
                };
                if (self.struct_methods.get(struct_name)) |method_table| {
                    if (method_table.get(name)) |mi| {
                        // This is a struct method call - generate as a function call
                        const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_name, name });

                        // Generate arguments: for instance methods, push receiver storage-id (alias) first
                        if (!mi.is_static) {
                            switch (receiver.data) {
                                .Variable => |recv_var| {
                                    // Push storage id of the receiver variable
                                    const recv_idx = try self.getOrCreateVariable(recv_var.lexeme);
                                    try self.instructions.append(.{ .PushStorageId = .{
                                        .var_index = recv_idx,
                                        .var_name = recv_var.lexeme,
                                        .scope_kind = .Local,
                                    } });
                                },
                                else => {
                                    // Spill temporary receiver into a hidden local, then push its storage id
                                    try self.generateExpression(receiver, true, false);
                                    const tmp_name = try std.fmt.allocPrint(self.allocator, "__this_tmp_{}", .{self.instructions.items.len});
                                    // Track as struct to guide SetField/etc.
                                    try self.trackVariableType(tmp_name, .Struct);
                                    const tmp_idx = try self.getOrCreateVariable(tmp_name);
                                    try self.instructions.append(.{ .StoreVar = .{
                                        .var_index = tmp_idx,
                                        .var_name = tmp_name,
                                        .scope_kind = .Local,
                                        .module_context = null,
                                        .expected_type = .Struct,
                                    } });
                                    try self.instructions.append(.{ .PushStorageId = .{
                                        .var_index = tmp_idx,
                                        .var_name = tmp_name,
                                        .scope_kind = .Local,
                                    } });
                                },
                            }
                        }
                        // Then generate explicit arguments
                        for (args) |arg| {
                            try self.generateExpression(arg.expr, true, false);
                        }

                        // Determine return type from semantic info
                        const ret_type: HIRType = self.convertTypeInfo(mi.return_type.*);

                        // Resolve function index if already registered
                        const fn_index: u32 = blk: {
                            if (self.getFunctionIndex(qualified_name)) |idx| break :blk idx;
                            break :blk 0;
                        };

                        // Compute argument count at runtime
                        var arg_count: u32 = @intCast(args.len);
                        if (!mi.is_static) arg_count += 1;

                        // Generate function call
                        try self.instructions.append(.{
                            .Call = .{
                                .function_index = fn_index, // May be resolved later if 0
                                .qualified_name = qualified_name,
                                .arg_count = arg_count,
                                .call_kind = .LocalFunction,
                                .target_module = null,
                                .return_type = ret_type,
                            },
                        });
                        return;
                    }
                }
            }
        }

        // If this is not a known compiler/builtin method, treat it as a no-op on the receiver.
        // This avoids emitting a BuiltinFunction call like "get".
        const is_known_builtin = std.mem.eql(u8, name, "substring") or
            std.mem.eql(u8, name, "string") or
            std.mem.eql(u8, name, "length") or
            std.mem.eql(u8, name, "int") or
            std.mem.eql(u8, name, "float") or
            std.mem.eql(u8, name, "byte");
        if (!is_known_builtin) {
            // Just evaluate the receiver and return it as the expression value
            try self.generateExpression(receiver, true, should_pop_after_use);
            return;
        }

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
    pub fn tryGenerateTailCall(self: *HIRGenerator, expr: *ast.Expr) bool {
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
                        } else {
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

    pub fn inferTypeFromLiteral(self: *HIRGenerator, literal: TokenLiteral) HIRType {
        return self.type_system.inferTypeFromLiteral(literal);
    }

    fn resolveFieldAccessType(self: *HIRGenerator, e: *ast.Expr) ?TypeSystem.FieldResolveResult {
        return self.type_system.resolveFieldAccessType(e, &self.symbol_table);
    }

    pub fn inferTypeFromExpression(self: *HIRGenerator, expr: *ast.Expr) HIRType {
        // Special-case function calls to leverage known function signatures
        // so we don't fall back to Unknown types during codegen (e.g., for '+=')
        switch (expr.data) {
            .FunctionCall => |call| {
                // Try to resolve callee and determine call kind
                var function_name: []const u8 = "";
                var call_kind: CallKind = .LocalFunction;

                switch (call.callee.data) {
                    .Variable => |var_token| {
                        function_name = var_token.lexeme;
                        // If this is a known user-defined function, keep LocalFunction; otherwise treat as builtin
                        if (self.getFunctionIndex(function_name) == null) {
                            call_kind = .BuiltinFunction;
                        }
                    },
                    .FieldAccess => |field_access| {
                        // Module function or method - treat as ModuleFunction for return type inference
                        if (field_access.object.data == .Variable) {
                            function_name = field_access.field.lexeme;
                            call_kind = .ModuleFunction;
                        } else {
                            // Fallback to generic inference
                            return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
                        }
                    },
                    else => {
                        // Fallback to generic inference for complex callees
                        return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
                    },
                }

                // Use call return type inference based on collected function signatures
                const inferred = self.inferCallReturnType(function_name, call_kind) catch .Unknown;
                return inferred;
            },
            else => {
                return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
            },
        }
    }

    pub fn trackVariableType(self: *HIRGenerator, var_name: []const u8, var_type: HIRType) !void {
        try self.symbol_table.trackVariableType(var_name, var_type);
    }

    pub fn trackVariableUnionMembersByIndex(self: *HIRGenerator, var_index: u32, members: [][]const u8) !void {
        try self.symbol_table.trackVariableUnionMembersByIndex(var_index, members);
    }

    /// NEW: Track a variable's custom type name (for enums/structs)
    pub fn trackVariableCustomType(self: *HIRGenerator, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.symbol_table.trackVariableCustomType(var_name, custom_type_name);
    }

    /// NEW: Get tracked variable type
    pub fn getTrackedVariableType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.symbol_table.getTrackedVariableType(var_name);
    }

    fn astTypeToLowerName(self: *HIRGenerator, base: ast.Type) []const u8 {
        return self.type_system.astTypeToLowerName(base);
    }

    /// Collect flattened union member names (lowercase) from a possibly nested union type
    pub fn collectUnionMemberNames(self: *HIRGenerator, ut: *ast.UnionType) ![][]const u8 {
        return self.type_system.collectUnionMemberNames(ut);
    }

    // NEW: Track array element types per variable for better index inference

    fn ensureAuxMapsInit(self: *HIRGenerator) void {
        _ = self; // map is initialized in init()
    }

    pub fn trackArrayElementType(self: *HIRGenerator, var_name: []const u8, elem_type: HIRType) !void {
        try self.symbol_table.trackArrayElementType(var_name, elem_type);
    }

    pub fn getTrackedArrayElementType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.symbol_table.getTrackedArrayElementType(var_name);
    }

    fn inferBinaryOpResultType(self: *HIRGenerator, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        return self.type_system.inferBinaryOpResultType(operator_type, left_expr, right_expr, &self.symbol_table);
    }

    /// NEW: Register a custom type (struct or enum)
    fn registerCustomType(self: *HIRGenerator, type_name: []const u8, kind: TypeSystem.CustomTypeInfo.CustomTypeKind) !void {
        try self.type_system.registerCustomType(type_name, kind);
    }

    /// NEW: Register an enum type with its variants
    pub fn registerEnumType(self: *HIRGenerator, enum_name: []const u8, variants: []const []const u8) !void {
        try self.type_system.registerEnumType(enum_name, variants);
    }

    /// NEW: Register a struct type with its fields
    pub fn registerStructType(self: *HIRGenerator, struct_name: []const u8, fields: []const []const u8) !void {
        try self.type_system.registerStructType(struct_name, fields);
    }

    /// Infer the appropriate operand type for comparison operations
    pub fn inferComparisonOperandType(self: *HIRGenerator, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        return self.type_system.inferComparisonOperandType(left_expr, right_expr, &self.symbol_table);
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
    pub fn resolveDefaultArgument(self: *HIRGenerator, function_name: []const u8, arg_index: usize) ?*ast.Expr {
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
    pub fn inferCallReturnType(self: *HIRGenerator, function_name: []const u8, call_kind: CallKind) !HIRType {
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
                    std.mem.eql(u8, function_name, "foo") or
                    std.mem.eql(u8, function_name, "time"))
                {
                    return .Int;
                }
                if (std.mem.eql(u8, function_name, "push") or
                    std.mem.eql(u8, function_name, "insert") or
                    std.mem.eql(u8, function_name, "clear"))
                {
                    return .Nothing; // Mutating methods return nothing
                }
                if (std.mem.eql(u8, function_name, "pop") or
                    std.mem.eql(u8, function_name, "remove"))
                {
                    return .Array; // Pop/remove methods return the removed element
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
    pub fn isCustomType(self: *HIRGenerator, name: []const u8) ?TypeSystem.CustomTypeInfo {
        return self.type_system.isCustomType(name);
    }

    /// NEW: Get the HIR type for a custom type name
    fn getCustomTypeHIRType(self: *HIRGenerator, name: []const u8) HIRType {
        return self.type_system.getCustomTypeHIRType(name);
    }

    /// Build a full variable path for peek expressions (e.g., "mike.person.age")
    pub fn buildPeekPath(self: *HIRGenerator, expr: *const ast.Expr) !?[]const u8 {
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

    pub fn generateTryStmt(self: *HIRGenerator, try_stmt: ast.TryStmt) !void {
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
            try SoxaStatements.generateStatement(self, stmt);
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
            try SoxaStatements.generateStatement(self, stmt);
        }

        // Emit end label
        try self.instructions.append(.{ .Label = .{
            .name = end_label,
            .vm_address = 0,
        } });
    }

    /// Check if a name is a module namespace
    pub fn isModuleNamespace(self: *HIRGenerator, name: []const u8) bool {
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

    pub fn computeNumericCommonType(self: *HIRGenerator, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        return self.type_system.computeNumericCommonType(left_type, right_type, operator_type);
    }

    /// NEW: Check if type promotion is needed and apply it
    /// Returns true if promotion was applied, false if no promotion needed
    pub fn applyTypePromotionIfNeeded(self: *HIRGenerator, left_type: HIRType, right_type: HIRType, target_type: HIRType) !bool {
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
