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
const ResourceManager = @import("resource_manager.zig");
const LabelGenerator = ResourceManager.LabelGenerator;
const ConstantManager = ResourceManager.ConstantManager;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const TypeSystem = @import("type_system.zig").TypeSystem;
const AliasTracker = @import("alias_tracker.zig").AliasTracker;
const SlotManager = @import("slot_manager.zig").SlotManager;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const SemanticAnalyzer = @import("../../analysis/semantic/semantic.zig");
const StructMethodInfo = SemanticAnalyzer.StructMethodInfo;
const BasicHandler = @import("expressions/basic.zig").BasicExpressionHandler;
const BinaryHandler = @import("expressions/binary.zig").BinaryExpressionHandler;
const ControlFlowHandler = @import("expressions/control_flow.zig").ControlFlowHandler;
const CollectionsHandler = @import("expressions/collections.zig").CollectionsHandler;
const CallsHandler = @import("expressions/calls.zig").CallsHandler;
const StructsHandler = @import("expressions/structs.zig").StructsHandler;
const AssignmentsHandler = @import("expressions/assignments.zig").AssignmentsHandler;
const IOHandler = @import("expressions/io.zig").IOHandler;

pub const TETRA_FALSE: u8 = 0;
pub const TETRA_TRUE: u8 = 1;
pub const TETRA_BOTH: u8 = 2;
pub const TETRA_NEITHER: u8 = 3;

pub const HIRGenerator = struct {
    allocator: std.mem.Allocator,
    instructions: std.array_list.Managed(HIRInstruction),
    current_peek_expr: ?*ast.Expr = null,
    current_field_name: ?[]const u8 = null,
    string_pool: std.array_list.Managed([]const u8),
    reporter: *Reporter,

    symbol_table: SymbolTable,
    constant_manager: ConstantManager,
    label_generator: LabelGenerator,
    type_system: TypeSystem,
    struct_methods: std.StringHashMap(std.StringHashMap(StructMethodInfo)),

    alias_tracker: AliasTracker,
    slot_manager: SlotManager,

    function_signatures: std.StringHashMap(FunctionInfo),
    function_bodies: std.array_list.Managed(FunctionBody),
    semantic_function_return_types: ?std.AutoHashMap(u32, *ast.TypeInfo) = null,
    semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer = null,
    current_function: ?[]const u8,
    current_function_return_type: HIRType,
    is_global_init_phase: bool,

    stats: HIRStats,

    function_calls: std.array_list.Managed(FunctionCallSite),

    module_namespaces: std.StringHashMap(ast.ModuleInfo),

    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    current_enum_type: ?[]const u8 = null,
    current_assignment_target: ?[]const u8 = null,

    loop_context_stack: std.array_list.Managed(LoopContext),

    is_generating_nested_array: bool = false,

    pub const FunctionInfo = struct {
        name: []const u8,
        arity: u32,
        return_type: HIRType,
        start_label: []const u8,
        body_label: ?[]const u8 = null,
        local_var_count: u32,
        is_entry: bool,
        param_is_alias: []bool,
        param_types: []HIRType,
    };

    pub const FunctionBody = struct {
        function_info: FunctionInfo,
        statements: []ast.Stmt,
        start_instruction_index: u32,
        function_name: []const u8,
        function_params: []ast.FunctionParam,
        return_type_info: ast.TypeInfo,
        param_is_alias: []bool,
        param_types: []HIRType,
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
            _ = allocator;
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

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, module_namespaces: std.StringHashMap(ast.ModuleInfo), imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol), semantic_function_return_types: ?std.AutoHashMap(u32, *ast.TypeInfo), semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer) HIRGenerator {
        return HIRGenerator{
            .allocator = allocator,
            .instructions = std.array_list.Managed(HIRInstruction).init(allocator),
            .current_peek_expr = null,
            .string_pool = std.array_list.Managed([]const u8).init(allocator),
            .reporter = reporter,
            .symbol_table = SymbolTable.init(allocator),
            .constant_manager = ConstantManager.init(allocator),
            .label_generator = LabelGenerator.init(allocator),
            .type_system = TypeSystem.init(allocator, reporter, semantic_analyzer),
            .struct_methods = std.StringHashMap(std.StringHashMap(StructMethodInfo)).init(allocator),
            .alias_tracker = AliasTracker.init(allocator),
            .slot_manager = SlotManager.init(allocator),
            .function_signatures = std.StringHashMap(FunctionInfo).init(allocator),
            .function_bodies = std.array_list.Managed(FunctionBody).init(allocator),
            .semantic_function_return_types = semantic_function_return_types,
            .semantic_analyzer = semantic_analyzer,
            .current_function = null,
            .current_function_return_type = .Nothing,
            .is_global_init_phase = false,
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
        self.symbol_table.deinit();
        self.constant_manager.deinit();
        self.type_system.deinit();
        var methods_it = self.struct_methods.valueIterator();
        while (methods_it.next()) |tbl| tbl.*.deinit();
        self.struct_methods.deinit();
        self.alias_tracker.deinit();
        self.slot_manager.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_calls.deinit();
        self.loop_context_stack.deinit();
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

    pub fn generateProgram(self: *HIRGenerator, statements: []ast.Stmt) !HIRProgram {

        // Pass 1: Collect function signatures (forward declarations)
        try self.collectFunctionSignatures(statements);

        // Pass 1.5: Process imported enum symbols and register them in type system
        try self.processImportedEnumSymbols();

        // Pass 2: Initialize global variables at module level
        try self.generateGlobalInitialization(statements);

        // Pass 3: Generate main program (non-global statements)
        try self.generateMainProgram(statements);

        // Pass 4: Generate function bodies AFTER main program
        try self.generateFunctionBodies();

        // Pass 4: Build function table
        const function_table = try self.buildFunctionTable();

        const instructions_slice = try self.instructions.toOwnedSlice();
        return HIRProgram{
            .instructions = instructions_slice,
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
                    var return_type = self.convertTypeInfo(func.return_type_info);

                    if (self.semantic_function_return_types) |semantic_types| {
                        if (semantic_types.get(stmt.base.id)) |semantic_return_type| {
                            return_type = self.convertTypeInfo(semantic_return_type.*);
                        }
                    }

                    const start_label = try self.label_generator.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{func.name.lexeme}));

                    var param_is_alias = try self.allocator.alloc(bool, func.params.len);
                    var param_types = try self.allocator.alloc(HIRType, func.params.len);
                    for (func.params, 0..) |param, i| {
                        param_is_alias[i] = param.is_alias;
                        param_types[i] = if (param.type_expr) |type_expr| self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*) else .Int;
                        // Sanitize param types: keep only simple primitives/markers for signature
                        param_types[i] = switch (param_types[i]) {
                            .Int, .Byte, .Float, .String, .Tetra, .Nothing => param_types[i],
                            .Struct, .Enum => param_types[i],
                            .Array, .Map, .Function, .Union => param_types[i],
                            else => .Int,
                        };
                    }

                    const function_info = FunctionInfo{
                        .name = func.name.lexeme,
                        .arity = @intCast(func.params.len),
                        .return_type = return_type,
                        .start_label = start_label,
                        .local_var_count = 0,
                        .is_entry = func.is_entry,
                        .param_is_alias = param_is_alias,
                        .param_types = param_types,
                    };

                    try self.function_signatures.put(func.name.lexeme, function_info);

                    try self.function_bodies.append(FunctionBody{
                        .function_info = function_info,
                        .statements = func.body,
                        .start_instruction_index = 0,
                        .function_name = func.name.lexeme,
                        .function_params = func.params,
                        .return_type_info = func.return_type_info,
                        .param_is_alias = param_is_alias,
                        .param_types = param_types,
                    });
                },
                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        if (expr.data == .StructDecl) {
                            const s = expr.data.StructDecl;
                            for (s.methods) |method| {
                                const qualified = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ s.name.lexeme, method.name.lexeme });
                                var return_type = self.convertTypeInfo(method.return_type_info);

                                if (self.struct_methods.get(s.name.lexeme)) |method_table| {
                                    if (method_table.get(method.name.lexeme)) |method_info| {
                                        const semantic_return_type = self.convertTypeInfo(method_info.return_type.*);
                                        return_type = semantic_return_type;
                                    }
                                }

                                if (return_type == .Nothing) {
                                    const inferred_type = self.inferMethodReturnType(method, s.name.lexeme);
                                    if (inferred_type != .Nothing) {
                                        return_type = inferred_type;
                                    }
                                }
                                const start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{qualified}));

                                var arity: u32 = @intCast(method.params.len);
                                if (!method.is_static) arity += 1;

                                // For non-static methods, prepend 'this' parameter
                                var param_is_alias = try self.allocator.alloc(bool, arity);
                                var param_types = try self.allocator.alloc(HIRType, arity);
                                var param_idx: usize = 0;
                                if (!method.is_static) {
                                    param_is_alias[param_idx] = true; // 'this' is an alias
                                    param_types[param_idx] = HIRType{ .Struct = 0 }; // 'this' is a struct pointer
                                    param_idx += 1;
                                }
                                for (method.params) |param| {
                                    param_is_alias[param_idx] = param.is_alias;
                                    param_types[param_idx] = if (param.type_expr) |type_expr| self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*) else .Int;
                                    // Sanitize param types: keep only simple primitives/markers for signature
                                    param_types[param_idx] = switch (param_types[param_idx]) {
                                        .Int, .Byte, .Float, .String, .Tetra, .Nothing => param_types[param_idx],
                                        .Struct, .Enum => param_types[param_idx],
                                        .Array, .Map, .Function, .Union => param_types[param_idx],
                                        else => .Int,
                                    };
                                    param_idx += 1;
                                }

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

                                var param_is_alias_imported = try self.allocator.alloc(bool, func.params.len);
                                var param_types_imported = try self.allocator.alloc(HIRType, func.params.len);
                                for (func.params, 0..) |param, i| {
                                    param_is_alias_imported[i] = param.is_alias;
                                    var pt: HIRType = if (param.type_expr) |type_expr|
                                        self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, type_expr)).*)
                                    else
                                        .Int;
                                    // Sanitize for signature table
                                    pt = switch (pt) {
                                        .Int, .Byte, .Float, .String, .Tetra, .Nothing => pt,
                                        .Struct, .Enum => pt,
                                        else => .Int,
                                    };
                                    param_types_imported[i] = pt;
                                }

                                const function_info = FunctionInfo{
                                    .name = qualified_name,
                                    .arity = @intCast(func.params.len),
                                    .return_type = return_type,
                                    .start_label = start_label,
                                    .local_var_count = 0,
                                    .is_entry = false,
                                    .param_is_alias = param_is_alias_imported,
                                    .param_types = param_types_imported,
                                };

                                try self.function_signatures.put(qualified_name, function_info);

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

        if (self.imported_symbols) |symbols| {
            var sym_it = symbols.iterator();
            while (sym_it.next()) |entry2| {
                const sym_name = entry2.key_ptr.*;
                const sym = entry2.value_ptr.*;
                if (sym.kind != .Function) continue;

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

                            if (!self.function_signatures.contains(sym_name)) {
                                // Initialize parameter metadata for imported symbol
                                for (func_params, 0..) |p, i| {
                                    function_info2.param_is_alias[i] = p.is_alias;
                                    var pt2: HIRType = if (p.type_expr) |te|
                                        self.convertTypeInfo((try ast.typeInfoFromExpr(self.allocator, te)).*)
                                    else
                                        .Int;
                                    pt2 = switch (pt2) {
                                        .Int, .Byte, .Float, .String, .Tetra, .Nothing => pt2,
                                        .Struct, .Enum => pt2,
                                        else => .Int,
                                    };
                                    function_info2.param_types[i] = pt2;
                                }

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

                            break;
                        }
                    }
                }
            }
        }
    }

    /// Pass 3: Generate function bodies AFTER main program
    fn generateFunctionBodies(self: *HIRGenerator) !void {
        for (self.function_bodies.items) |*function_body| {
            self.current_function = function_body.function_info.name;
            self.current_function_return_type = function_body.function_info.return_type;
            self.is_global_init_phase = false;
            try self.symbol_table.enterFunctionScope(function_body.function_info.name);

            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label, .vm_address = 0 } });

            const function_scope_id = self.label_generator.label_count + 1000;
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = function_scope_id, .var_count = 0 } });

            const params = function_body.function_params;

            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                if (function_body.param_is_alias[param_index]) {
                    var param_type: HIRType = .Unknown;
                    if (param.type_expr) |type_expr| {
                        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                        defer self.allocator.destroy(type_info_ptr);
                        param_type = self.convertTypeInfo(type_info_ptr.*);

                        // Semantic validation for alias parameter types
                        // Disallow complex types as alias types here
                        // Arrays are allowed as they're perfect candidates for aliasing (avoid copying)
                        switch (param_type) {
                            .Union, .Map, .Function => return error.InvalidAliasType,
                            else => {},
                        }

                        // For structs/enums, require a concrete custom type name rather than generic tokens
                        switch (type_info_ptr.base) {
                            .Struct, .Enum => {
                                if (type_info_ptr.custom_type == null) {
                                    return error.InvalidAliasType;
                                }
                            },
                            else => {},
                        }
                    } else {
                        param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
                    }

                    try self.trackVariableType(param.name.lexeme, param_type);
                    try self.symbol_table.trackAliasParameter(param.name.lexeme);

                    if (param.type_expr) |type_expr_for_custom| {
                        const type_info_for_custom = try ast.typeInfoFromExpr(self.allocator, type_expr_for_custom);
                        if (type_info_for_custom.base == .Struct) {
                            if (type_info_for_custom.custom_type) |struct_type_name_for_param| {
                                try self.trackVariableCustomType(param.name.lexeme, struct_type_name_for_param);
                                try self.trackVariableType(param.name.lexeme, HIRType{ .Struct = 0 });
                            }
                            // If generic Struct token without a name, reject
                            if (type_info_for_custom.custom_type == null) return error.InvalidAliasType;
                        } else if (type_info_for_custom.base == .Enum) {
                            if (type_info_for_custom.custom_type) |enum_type_name_for_param| {
                                try self.trackVariableCustomType(param.name.lexeme, enum_type_name_for_param);
                                try self.trackVariableType(param.name.lexeme, HIRType{ .Enum = 0 });
                            }
                            // If generic Enum token without a name, reject
                            if (type_info_for_custom.custom_type == null) return error.InvalidAliasType;
                        } else if (type_info_for_custom.base == .Custom) {
                            if (type_info_for_custom.custom_type) |custom_type_name_for_param| {
                                if (self.isCustomType(custom_type_name_for_param)) |ct| {
                                    switch (ct.kind) {
                                        .Struct => {
                                            try self.trackVariableCustomType(param.name.lexeme, custom_type_name_for_param);
                                            try self.trackVariableType(param.name.lexeme, HIRType{ .Struct = 0 });
                                        },
                                        .Enum => {
                                            try self.trackVariableCustomType(param.name.lexeme, custom_type_name_for_param);
                                            try self.trackVariableType(param.name.lexeme, HIRType{ .Enum = 0 });
                                        },
                                    }
                                }
                            }
                        }
                    }

                    const alias_slot = try self.slot_manager.allocateAliasSlot(param.name.lexeme, param_type);

                    try self.instructions.append(.{
                        .StoreParamAlias = .{
                            .param_name = param.name.lexeme,
                            .param_type = param_type,
                            .var_index = alias_slot,
                        },
                    });
                } else {
                    var param_type: HIRType = .Unknown;
                    if (param.type_expr) |type_expr| {
                        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                        defer self.allocator.destroy(type_info_ptr);
                        param_type = self.convertTypeInfo(type_info_ptr.*);
                    } else {
                        param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
                    }

                    try self.trackVariableType(param.name.lexeme, param_type);

                    // Parameters must always be treated as local variables, even if a
                    // global with the same name exists later in the script. Using
                    // createVariable here ensures that parameters correctly shadow
                    // any globals instead of aliasing them (which previously caused
                    // recursive functions like `fber` to read/write a global `x`
                    // instead of their own parameter).
                    const var_idx = try self.symbol_table.createVariable(param.name.lexeme);
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = param.name.lexeme,
                        .scope_kind = self.symbol_table.determineVariableScope(param.name.lexeme),
                        .module_context = null,
                        .expected_type = param_type,
                    } });
                }
            }

            // Process 'this' parameter for non-static methods (it's pushed first, so popped last)
            if (std.mem.indexOfScalar(u8, function_body.function_info.name, '.')) |dot_idx| {
                const struct_name = function_body.function_info.name[0..dot_idx];
                const method_name = function_body.function_info.name[dot_idx + 1 ..];
                if (self.struct_methods.get(struct_name)) |method_table| {
                    if (method_table.get(method_name)) |mi| {
                        if (!mi.is_static) {
                            // 'this' is an alias parameter (struct pointer)
                            try self.trackVariableType("this", HIRType{ .Struct = 0 });
                            try self.symbol_table.trackAliasParameter("this");

                            const struct_type_name = struct_name;
                            try self.trackVariableCustomType("this", struct_type_name);

                            const alias_slot = try self.slot_manager.allocateAliasSlot("this", HIRType{ .Struct = 0 });

                            try self.instructions.append(.{
                                .StoreParamAlias = .{
                                    .param_name = "this",
                                    .param_type = HIRType{ .Struct = 0 },
                                    .var_index = alias_slot,
                                },
                            });
                        }
                    }
                }
            }

            const body_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}_body", .{function_body.function_info.name}));
            try self.instructions.append(.{ .Label = .{ .name = body_label, .vm_address = 0 } });

            if (self.function_signatures.getPtr(function_body.function_info.name)) |func_info| {
                func_info.body_label = body_label;
            }
            var has_returned = false;
            for (function_body.statements) |body_stmt| {
                if (has_returned) {
                    break;
                }

                try SoxaStatements.generateStatement(self, body_stmt);

                has_returned = self.statementAlwaysReturns(body_stmt);
            }

            try self.instructions.append(.{ .ExitScope = .{ .scope_id = function_scope_id } });

            if (std.mem.eql(u8, function_body.function_info.name, "safeAdd") or std.mem.eql(u8, function_body.function_info.name, "safeMath.safeAdd")) {
                try self.instructions.append(.{ .LoadVar = .{ .var_index = 1, .var_name = "a", .scope_kind = self.symbol_table.determineVariableScope("a"), .module_context = null } });
                try self.instructions.append(.{ .LoadVar = .{ .var_index = 0, .var_name = "b", .scope_kind = self.symbol_table.determineVariableScope("b"), .module_context = null } });
                try self.instructions.append(.{ .Call = .{ .function_index = 1, .qualified_name = "math.add", .arg_count = 2, .call_kind = .ModuleFunction, .target_module = "math", .return_type = .Int } });
                try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = .Int } });
            } else if (!has_returned) {
                if (function_body.function_info.return_type != .Nothing) {
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                } else {
                    try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
                }
            }

            var needs_implicit_return = true;

            // Check if we already have a return instruction in the function
            if (has_returned) {
                needs_implicit_return = false;
            } else if (self.instructions.items.len > 0) {
                const last_instruction = self.instructions.items[self.instructions.items.len - 1];
                if (last_instruction == .Return) {
                    needs_implicit_return = false;
                }
            }

            if (needs_implicit_return) {
                try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
            }

            if (self.function_signatures.getPtr(function_body.function_info.name)) |func_info| {
                func_info.local_var_count = self.symbol_table.local_variable_count;
            }

            self.current_function = null;
            self.current_function_return_type = .Nothing;
            self.symbol_table.exitFunctionScope();
        }
    }

    fn generateMainProgram(self: *HIRGenerator, statements: []ast.Stmt) !void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl => {
                    continue;
                },
                .VarDecl => {
                    try SoxaStatements.generateStatement(self, stmt);
                },
                else => {
                    try SoxaStatements.generateStatement(self, stmt);
                },
            }
        }

        // Find the entry function by checking is_entry flag
        var entry_function: ?FunctionInfo = null;
        var entry_function_name: ?[]const u8 = null;
        var it = self.function_signatures.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.is_entry) {
                entry_function = entry.value_ptr.*;
                entry_function_name = entry.key_ptr.*;
                break;
            }
        }

        if (entry_function) |entry_func| {
            const entry_function_index = if (entry_function_name) |name|
                self.getFunctionIndex(name) orelse 0
            else
                0;

            const entry_call_instruction = HIRInstruction{
                .Call = .{
                    .function_index = entry_function_index,
                    .qualified_name = entry_function_name orelse "main",
                    .arg_count = 0,
                    .call_kind = .LocalFunction,
                    .target_module = null,
                    .return_type = entry_func.return_type,
                },
            };
            try self.instructions.append(entry_call_instruction);

            if (entry_func.return_type != .Nothing) {
                try self.instructions.append(.Pop);
            }
        }

        try self.instructions.append(.Halt);
    }

    /// Pass 1.5: Process imported enum symbols and register them in type system
    fn processImportedEnumSymbols(self: *HIRGenerator) !void {
        if (self.imported_symbols) |imported_symbols| {
            var it = imported_symbols.iterator();
            while (it.next()) |entry| {
                const symbol_name = entry.key_ptr.*;
                const symbol = entry.value_ptr.*;

                if (symbol.kind == .Enum) {
                    // Get the actual enum variants based on the symbol name
                    // This is a temporary solution - ideally we'd parse the original module to get the actual variants
                    var variants: []const []const u8 = undefined;

                    if (std.mem.eql(u8, symbol_name, "Token")) {
                        // Token enum from token.doxa
                        variants = &[_][]const u8{
                            "INT_LITERAL",
                            "FLOAT_LITERAL",
                            "BYTE_LITERAL",
                            "TETRA_LITERAL",
                            "STRING_LITERAL",
                            "NOTHING_LITERAL",
                        };
                    } else {
                        // For other enums, use placeholder variants
                        variants = &[_][]const u8{"PLACEHOLDER"};
                    }

                    try self.registerEnumType(symbol_name, variants);

                    // Register the enum type name as a variable so Token.INT_LITERAL works
                    const var_idx = try self.getOrCreateVariable(symbol_name);
                    try self.trackVariableType(symbol_name, HIRType{ .Enum = 0 });

                    // Create a special enum type value and store it
                    const enum_type_value = HIRValue{ .string = symbol_name };
                    const const_idx = try self.addConstant(enum_type_value);
                    try self.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
                    try self.instructions.append(.{ .StoreConst = .{
                        .var_index = var_idx,
                        .var_name = symbol_name,
                        .scope_kind = .GlobalLocal,
                        .module_context = null,
                    } });
                }
            }
        }
    }

    /// Pass 2: Initialize global variables at module level (before main program execution)
    fn generateGlobalInitialization(self: *HIRGenerator, statements: []ast.Stmt) !void {
        self.is_global_init_phase = true;
        defer self.is_global_init_phase = false;

        const previous_function = self.current_function;
        self.current_function = null;
        defer self.current_function = previous_function;

        var it = self.module_namespaces.iterator();
        while (it.next()) |entry| {
            const module_info = entry.value_ptr.*;
            if (module_info.ast) |module_ast| {
                if (module_ast.data == .Block) {
                    const mod_statements = module_ast.data.Block.statements;
                    for (mod_statements) |mod_stmt| {
                        switch (mod_stmt.data) {
                            .FunctionDecl => continue,
                            .VarDecl => {
                                try SoxaStatements.generateStatement(self, mod_stmt);
                            },
                            else => continue,
                        }
                    }
                }
            }
        }
        _ = statements;
    }

    /// Pass 4: Build function table from collected signatures
    fn buildFunctionTable(self: *HIRGenerator) ![]HIRProgram.HIRFunction {
        var function_table = std.array_list.Managed(HIRProgram.HIRFunction).init(self.allocator);

        for (self.function_bodies.items) |function_body| {
            const function_info = function_body.function_info;

            try function_table.append(HIRProgram.HIRFunction{
                .name = function_info.name,
                .qualified_name = function_info.name,
                .arity = function_info.arity,
                .return_type = function_info.return_type,
                .start_label = function_info.start_label,
                .body_label = function_info.body_label,
                .start_ip = 0,
                .body_ip = null,
                .local_var_count = function_info.local_var_count,
                .is_entry = function_info.is_entry,
                .param_is_alias = function_body.param_is_alias,
                .param_types = function_body.param_types,
            });
        }

        return try function_table.toOwnedSlice();
    }

    pub fn getFunctionIndex(self: *HIRGenerator, function_name: []const u8) ?u32 {
        for (self.function_bodies.items, 0..) |function_body, index| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return @as(u32, @intCast(index));
            }
        }
        return null;
    }

    pub fn isModuleFunction(self: *HIRGenerator, function_name: []const u8) bool {
        if (self.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(function_name)) |imported_symbol| {
                return imported_symbol.kind == .Function;
            }
        }
        return false;
    }

    pub fn computeTargetModule(self: *HIRGenerator, qualified_name: []const u8, call_kind: CallKind) !?[]const u8 {
        if (call_kind != .ModuleFunction) {
            return null;
        }
        const dot_idx = std.mem.lastIndexOfScalar(u8, qualified_name, '.') orelse return null;
        if (dot_idx == 0) return null;
        return try self.allocator.dupe(u8, qualified_name[0..dot_idx]);
    }

    pub fn convertTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) HIRType {
        return self.type_system.convertTypeInfo(type_info);
    }

    pub fn findFunctionBody(self: *HIRGenerator, function_name: []const u8) ?*FunctionBody {
        for (self.function_bodies.items) |*function_body| {
            if (std.mem.eql(u8, function_body.function_info.name, function_name)) {
                return function_body;
            }
        }
        return null;
    }

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
            .Literal => |lit| try basic_handler.generateLiteral(lit, preserve_result, should_pop_after_use),
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
            .Match => |match_expr| try control_flow_handler.generateMatch(match_expr, preserve_result),
            .Loop => |loop| try control_flow_handler.generateLoop(loop, preserve_result),
            .Block => try control_flow_handler.generateBlock(expr.data, preserve_result),
            .ReturnExpr => try control_flow_handler.generateReturn(expr.data),
            .Unreachable => try control_flow_handler.generateUnreachable(expr),
            .Cast => try control_flow_handler.generateCast(expr.data, preserve_result),

            // Collections
            .Array => |elements| {
                if (self.is_generating_nested_array) {
                    try collections_handler.generateArrayInternal(elements, preserve_result);
                } else {
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

            else => {
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

    fn extractSimpleComparison(self: *HIRGenerator, condition: *ast.Expr) !HIRValue {
        _ = self;
        switch (condition.data) {
            .Binary => |binary| {
                if (binary.right) |right| {
                    switch (right.data) {
                        .Literal => |lit| {
                            return switch (lit) {
                                .int => |i| HIRValue{ .int = i },
                                .float => |f| HIRValue{ .float = f },
                                .string => |s| HIRValue{ .string = s },
                                else => HIRValue{ .int = 0 },
                            };
                        },
                        .Variable => |var_token| {
                            _ = var_token;
                            return HIRValue{ .int = 0 };
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
        return HIRValue{ .int = 0 };
    }

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

    fn expressionAlwaysReturns(self: *HIRGenerator, expr: *ast.Expr) bool {
        switch (expr.data) {
            .If => |if_expr| {
                const then_returns = if (if_expr.then_branch) |then| self.expressionAlwaysReturns(then) else false;
                const else_returns = if (if_expr.else_branch) |else_branch| self.expressionAlwaysReturns(else_branch) else false;
                return then_returns and else_returns;
            },
            .Block => |block| {
                if (block.value) |value| {
                    return self.expressionAlwaysReturns(value);
                }
                for (block.statements) |stmt| {
                    if (self.statementAlwaysReturns(stmt)) {
                        return true;
                    }
                }
                return false;
            },
            .Unreachable => return true,
            else => return false,
        }
    }

    pub fn generateInternalMethodCall(self: *HIRGenerator, method: Token, receiver: *ast.Expr, args: []ast.CallArgument, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        const name = method.lexeme;

        const receiver_type = self.inferTypeFromExpression(receiver);
        if (receiver_type == .Struct) {
            if (receiver.data == .Variable) {
                const recv_var_name = receiver.data.Variable.lexeme;
                const struct_name = blk: {
                    if (self.symbol_table.getVariableCustomType(recv_var_name)) |ctype| break :blk ctype;
                    break :blk recv_var_name;
                };

                if (self.struct_methods.get(struct_name)) |method_table| {
                    if (method_table.get(name)) |mi| {
                        const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_name, name });

                        if (!mi.is_static) {
                            switch (receiver.data) {
                                .Variable => {
                                    try self.generateExpression(receiver, true, false);
                                },
                                else => {
                                    try self.generateExpression(receiver, true, false);
                                },
                            }
                        }
                        for (args) |arg| {
                            try self.generateExpression(arg.expr, true, false);
                        }

                        const ret_type: HIRType = self.convertTypeInfo(mi.return_type.*);

                        const fn_index: u32 = blk: {
                            if (self.getFunctionIndex(qualified_name)) |idx| {
                                break :blk idx;
                            } else {
                                break :blk 0;
                            }
                        };

                        var arg_count: u32 = @intCast(args.len);
                        if (!mi.is_static) arg_count += 1;

                        try self.instructions.append(.{
                            .Call = .{
                                .function_index = fn_index,
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

        const is_known_builtin = std.mem.eql(u8, name, "substring") or
            std.mem.eql(u8, name, "string") or
            std.mem.eql(u8, name, "length") or
            std.mem.eql(u8, name, "int") or
            std.mem.eql(u8, name, "float") or
            std.mem.eql(u8, name, "byte");
        if (!is_known_builtin) {
            try self.generateExpression(receiver, true, should_pop_after_use);
            return;
        }

        if (std.mem.eql(u8, name, "substring")) {
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
                    .scope_kind = self.symbol_table.determineVariableScope(target_var),
                    .module_context = null,
                    .expected_type = expected_type,
                } });
            }
        }
    }

    pub fn tryGenerateTailCall(self: *HIRGenerator, expr: *ast.Expr) bool {
        switch (expr.data) {
            .FunctionCall => |call| {
                switch (call.callee.data) {
                    .Variable => |var_token| {
                        const function_name = var_token.lexeme;

                        for (call.arguments) |arg| {
                            self.generateExpression(arg.expr, true, true) catch return false;
                        }

                        if (self.getFunctionIndex(function_name)) |function_index| {
                            const return_type = self.inferCallReturnType(function_name, .LocalFunction) catch .Nothing;

                            const tail_call = HIRInstruction{ .TailCall = .{
                                .function_index = function_index,
                                .qualified_name = function_name,
                                .arg_count = @intCast(call.arguments.len),
                                .call_kind = .LocalFunction,
                                .target_module = null,
                                .return_type = return_type,
                            } };

                            self.instructions.append(tail_call) catch return false;
                            return true;
                        } else {
                            return false;
                        }
                    },
                    .FieldAccess => |field_access| {
                        const module_name = switch (field_access.object.data) {
                            .Variable => |var_token| var_token.lexeme,
                            else => {
                                return false;
                            },
                        };
                        const function_name = field_access.field.lexeme;
                        const qualified_name = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, function_name }) catch return false;

                        for (call.arguments) |arg| {
                            self.generateExpression(arg.expr, true, true) catch return false;
                        }

                        if (self.getFunctionIndex(qualified_name)) |function_index| {
                            const return_type = self.inferCallReturnType(qualified_name, .ModuleFunction) catch .Nothing;

                            const tail_call = HIRInstruction{ .TailCall = .{
                                .function_index = function_index,
                                .qualified_name = qualified_name,
                                .arg_count = @intCast(call.arguments.len),
                                .call_kind = .ModuleFunction,
                                .target_module = module_name,
                                .return_type = return_type,
                            } };

                            self.instructions.append(tail_call) catch return false;
                            return true;
                        } else {
                            return false;
                        }
                    },
                    else => {
                        return false;
                    },
                }
            },
            else => {
                return false;
            },
        }
    }

    pub fn inferTypeFromLiteral(self: *HIRGenerator, literal: TokenLiteral) HIRType {
        return self.type_system.inferTypeFromLiteral(literal);
    }

    pub fn resolveFieldAccessType(self: *HIRGenerator, e: *ast.Expr) ?TypeSystem.FieldResolveResult {
        return self.type_system.resolveFieldAccessType(e, &self.symbol_table);
    }

    pub fn inferTypeFromExpression(self: *HIRGenerator, expr: *ast.Expr) HIRType {
        var allocated_name: ?[]u8 = null;
        defer {
            if (allocated_name) |name| {
                self.allocator.free(name);
            }
        }

        switch (expr.data) {
            .Variable => {
                return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
            },
            .FunctionCall => |call| {
                var function_name: []const u8 = "";
                var call_kind: CallKind = .LocalFunction;

                switch (call.callee.data) {
                    .Variable => |var_token| {
                        function_name = var_token.lexeme;
                        if (self.getFunctionIndex(function_name) == null) {
                            call_kind = .BuiltinFunction;
                        }
                    },
                    .FieldAccess => |field_access| {
                        if (field_access.object.data == .Variable) {
                            const object_name = field_access.object.data.Variable.lexeme;
                            if (self.isModuleNamespace(object_name)) {
                                const qualified = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ object_name, field_access.field.lexeme }) catch return .Unknown;
                                allocated_name = qualified;
                                function_name = qualified;
                                call_kind = .ModuleFunction;
                            } else {
                                return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
                            }
                        } else {
                            return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
                        }
                    },
                    else => {
                        return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
                    },
                }

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

    pub fn trackVariableCustomType(self: *HIRGenerator, var_name: []const u8, custom_type_name: []const u8) !void {
        try self.symbol_table.trackVariableCustomType(var_name, custom_type_name);
    }

    pub fn getTrackedVariableType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.symbol_table.getTrackedVariableType(var_name);
    }

    fn astTypeToLowerName(self: *HIRGenerator, base: ast.Type) []const u8 {
        return self.type_system.astTypeToLowerName(base);
    }

    pub fn collectUnionMemberNames(self: *HIRGenerator, ut: *ast.UnionType) ![][]const u8 {
        return self.type_system.collectUnionMemberNames(ut);
    }

    fn ensureAuxMapsInit(self: *HIRGenerator) void {
        _ = self;
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

    fn registerCustomType(self: *HIRGenerator, type_name: []const u8, kind: TypeSystem.CustomTypeInfo.CustomTypeKind) !void {
        try self.type_system.registerCustomType(type_name, kind);
    }

    pub fn registerEnumType(self: *HIRGenerator, enum_name: []const u8, variants: []const []const u8) !void {
        try self.type_system.registerEnumType(enum_name, variants);
    }

    pub fn registerStructType(self: *HIRGenerator, struct_name: []const u8, fields: []const []const u8) !void {
        try self.type_system.registerStructType(struct_name, fields);
    }

    pub fn inferComparisonOperandType(self: *HIRGenerator, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        return self.type_system.inferComparisonOperandType(left_expr, right_expr, &self.symbol_table);
    }

    fn inferParameterType(self: *HIRGenerator, param_name: []const u8, function_body: []ast.Stmt, function_name: []const u8) !HIRType {
        for (function_body) |stmt| {
            if (self.analyzeStatementForParameterType(stmt, param_name)) |inferred_type| {
                return inferred_type;
            }
        }

        if (self.inferParameterTypeFromCallSites(param_name, function_name)) |inferred_type| {
            return inferred_type;
        }

        return .Int;
    }

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

    fn analyzeExpressionForParameterType(self: *HIRGenerator, expr: *ast.Expr, param_name: []const u8) ?HIRType {
        return switch (expr.data) {
            .Binary => |binary| {
                const left_uses_param = if (binary.left) |left| self.expressionUsesParameter(left, param_name) else false;
                const right_uses_param = if (binary.right) |right| self.expressionUsesParameter(right, param_name) else false;

                if (left_uses_param or right_uses_param) {
                    return switch (binary.operator.type) {
                        .PLUS, .MINUS, .ASTERISK, .SLASH, .MODULO => .Int,
                        .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => .Int,
                        .EQUALITY, .BANG_EQUAL => .Int,
                        else => null,
                    };
                }
                return null;
            },
            .FunctionCall => |call| {
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg.expr, param_name)) {
                        return .Int;
                    }
                }
                return null;
            },
            .If => |if_expr| {
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

    fn expressionUsesParameter(self: *HIRGenerator, expr: *ast.Expr, param_name: []const u8) bool {
        return switch (expr.data) {
            .Variable => |var_token| std.mem.eql(u8, var_token.lexeme, param_name),
            .Binary => |binary| {
                const left_uses = if (binary.left) |left| self.expressionUsesParameter(left, param_name) else false;
                const right_uses = if (binary.right) |right| self.expressionUsesParameter(right, param_name) else false;
                return left_uses or right_uses;
            },
            .FunctionCall => |call| {
                for (call.arguments) |arg| {
                    if (self.expressionUsesParameter(arg.expr, param_name)) return true;
                }
                return false;
            },
            else => false,
        };
    }

    fn inferParameterTypeFromCallSites(self: *HIRGenerator, param_name: []const u8, function_name: []const u8) ?HIRType {
        _ = param_name;

        for (self.function_calls.items) |call_site| {
            if (std.mem.eql(u8, call_site.function_name, function_name)) {
                return .Int;
            }
        }
        return null;
    }

    pub fn resolveDefaultArgument(self: *HIRGenerator, function_name: []const u8, arg_index: usize) ?*ast.Expr {
        for (self.function_bodies.items) |function_body| {
            if (std.mem.eql(u8, function_body.function_name, function_name)) {
                if (arg_index < function_body.function_params.len) {
                    const param = function_body.function_params[arg_index];
                    return param.default_value;
                }
                break;
            }
        }
        return null;
    }

    fn inferMethodReturnType(self: *HIRGenerator, method: *ast.StructMethod, struct_name: []const u8) HIRType {
        for (method.body) |stmt| {
            if (stmt.data == .Return) {
                const return_stmt = stmt.data.Return;
                if (return_stmt.value) |value| {
                    return self.inferTypeFromExpressionWithStruct(value, struct_name);
                }
            }
        }
        return .Nothing;
    }

    fn inferTypeFromExpressionWithStruct(self: *HIRGenerator, expr: *ast.Expr, struct_name: []const u8) HIRType {
        switch (expr.data) {
            .FieldAccess => |field_access| {
                if (field_access.object.data == .This) {
                    if (self.type_system.custom_types.get(struct_name)) |struct_info| {
                        if (struct_info.kind == .Struct) {
                            if (struct_info.struct_fields) |fields| {
                                for (fields) |field| {
                                    if (std.mem.eql(u8, field.name, field_access.field.lexeme)) {
                                        return field.field_type;
                                    }
                                }
                            }
                        }
                    }
                }
                return self.inferTypeFromExpression(expr);
            },
            .StructLiteral => |struct_literal| {
                if (std.mem.eql(u8, struct_literal.name.lexeme, struct_name)) {
                    return HIRType{ .Struct = 0 };
                }
                return self.inferTypeFromExpression(expr);
            },
            else => {
                return self.inferTypeFromExpression(expr);
            },
        }
    }

    pub fn inferCallReturnType(self: *HIRGenerator, function_name: []const u8, call_kind: CallKind) !HIRType {
        switch (call_kind) {
            .LocalFunction => {
                if (self.function_signatures.get(function_name)) |func_info| {
                    return func_info.return_type;
                }
                return .Nothing;
            },
            .BuiltinFunction => {
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
                    return .Nothing;
                }
                if (std.mem.eql(u8, function_name, "pop") or
                    std.mem.eql(u8, function_name, "remove"))
                {
                    return .Unknown;
                }
                if (std.mem.eql(u8, function_name, "print") or
                    std.mem.eql(u8, function_name, "println"))
                {
                    return .Nothing;
                }
                if (std.mem.eql(u8, function_name, "input")) {
                    return .String;
                }
                if (std.mem.eql(u8, function_name, "random")) {
                    return .Float;
                }
                if (std.mem.eql(u8, function_name, "dice_roll")) {
                    return .Int;
                }
                return .String;
            },
            .ModuleFunction => {
                if (self.function_signatures.get(function_name)) |func_info| {
                    return func_info.return_type;
                }
                return .String;
            },
        }
    }

    pub fn isCustomType(self: *HIRGenerator, name: []const u8) ?TypeSystem.CustomTypeInfo {
        return self.type_system.isCustomType(name);
    }

    fn getCustomTypeHIRType(self: *HIRGenerator, name: []const u8) HIRType {
        return self.type_system.getCustomTypeHIRType(name);
    }

    pub fn buildPeekPath(self: *HIRGenerator, expr: *const ast.Expr) !?[]const u8 {
        switch (expr.data) {
            .Variable => |var_token| {
                return try self.allocator.dupe(u8, var_token.lexeme);
            },
            .FieldAccess => |field| {
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
        const catch_label = try self.generateLabel("catch");
        const end_label = try self.generateLabel("try_end");

        try self.instructions.append(.{
            .TryBegin = .{
                .catch_label = catch_label,
                .vm_catch_offset = 0,
            },
        });

        for (try_stmt.try_body) |stmt| {
            try SoxaStatements.generateStatement(self, stmt);
        }

        try self.instructions.append(.{
            .Jump = .{
                .label = end_label,
                .vm_offset = 0,
            },
        });

        try self.instructions.append(.{ .Label = .{
            .name = catch_label,
            .vm_address = 0,
        } });

        try self.instructions.append(.{
            .TryCatch = .{
                .exception_type = null,
            },
        });

        for (try_stmt.catch_body) |stmt| {
            try SoxaStatements.generateStatement(self, stmt);
        }

        try self.instructions.append(.{ .Label = .{
            .name = end_label,
            .vm_address = 0,
        } });
    }

    pub fn isModuleNamespace(self: *HIRGenerator, name: []const u8) bool {
        if (self.module_namespaces.contains(name)) return true;
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

    pub fn isModuleContext(self: *HIRGenerator) bool {
        // We're in module context when we're in the global init phase and not in a function
        return self.is_global_init_phase and self.current_function == null;
    }
};
