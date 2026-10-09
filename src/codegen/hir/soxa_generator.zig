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
const Slot = SoxaTypes.Slot;
const ScopeKind = SoxaTypes.ScopeKind;
pub const HIRType = SoxaTypes.HIRType;
const CallKind = SoxaTypes.CallKind;
const HIRProgram = SoxaTypes.HIRProgram;

const ParamMutation = @import("param_mutation.zig");
const ResourceManager = @import("resource_manager.zig");
const LabelGenerator = ResourceManager.LabelGenerator;
const ConstantManager = ResourceManager.ConstantManager;
const TypeSystem = @import("type_system.zig").TypeSystem;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const semantic_module = @import("../../analysis/semantic/semantic.zig");
const SemanticAnalyzer = semantic_module.SemanticAnalyzer;
const StructMethodInfo = semantic_module.StructMethodInfo;
const StoreTarget = semantic_module.StoreTarget;
const Resolution = @import("../../analysis/semantic/resolution.zig").Resolution;
const BasicHandler = @import("expressions/basic.zig").BasicExpressionHandler;
const BinaryHandler = @import("expressions/binary.zig").BinaryExpressionHandler;
const ControlFlowHandler = @import("expressions/control_flow.zig").ControlFlowHandler;
const CollectionsHandler = @import("expressions/collections.zig").CollectionsHandler;
const CallsHandler = @import("expressions/calls.zig").CallsHandler;
const StructsHandler = @import("expressions/structs.zig").StructsHandler;
const AssignmentsHandler = @import("expressions/assignments.zig").AssignmentsHandler;
const IOHandler = @import("expressions/io.zig").IOHandler;
const ModuleCall = @import("module_call.zig");
const module_graph = @import("../../module/graph.zig");
const TypeRef = ast.TypeRef;

pub const TETRA_FALSE: u8 = 0;
pub const TETRA_TRUE: u8 = 1;
pub const TETRA_BOTH: u8 = 2;
pub const TETRA_NEITHER: u8 = 3;

pub const HIRGenerator = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    instructions: std.array_list.Managed(HIRInstruction),
    current_peek_expr: ?*ast.Expr = null,
    current_field_name: ?[]const u8 = null,
    string_pool: std.array_list.Managed([]const u8),
    reporter: *Reporter,
    /// A3: set while lowering the value expression of a `return` whose top-level
    /// expression constructs a struct or array. The construction emitter consumes
    /// it (clearing it for nested sub-expressions) and tags the produced object so
    /// the backend allocates it directly in the caller's arena — copy-free return.
    place_return_value: bool = false,
    /// B2: struct type names that reach a reflection site — `"{x}"` interpolation
    /// of a struct, `@string(struct)`, or `peek` — anywhere in the program. A
    /// reflected struct must keep its runtime descriptor registry entry; a
    /// scalar-only struct never reflected and never crossing a signature/container
    /// boundary may skip it. Populated during generation.
    reflected_structs: std.StringHashMap(void),
    /// B2: a group/unknown reflection target was seen; the reflection predicate
    /// cannot enumerate its members, so no struct may skip the descriptor.
    force_struct_descriptors: bool = false,

    constant_manager: ConstantManager,
    label_generator: LabelGenerator,
    type_system: TypeSystem,
    struct_methods: std.StringHashMap(std.StringHashMap(StructMethodInfo)),

    /// The `^` parameters of the function being lowered, by their binding's
    /// slot: the alias slot `BindAlias` bound each one to.
    alias_params: std.AutoHashMap(Slot, u32),
    /// The slot of the receiver `this` of the method being lowered.
    this_slot: ?Slot = null,
    /// The next alias slot `BindAlias` binds a `^` parameter or `this` to.
    next_alias_slot: u32 = 0,

    function_signatures: SoxaTypes.FunctionSignatureMap,
    function_bodies: std.array_list.Managed(FunctionBody),
    /// Function-table index by link name.
    function_indices: std.StringHashMap(u32),
    /// The Zig functions the program calls, by wrapper link name.
    zig_functions: std.StringHashMap(*const ast.ZigFnSig),
    /// The analysis this program is lowered from: every expression's type and
    /// every name's resolution.
    semantic: *const SemanticAnalyzer,
    current_function: ?[]const u8,
    current_function_return_type: HIRType,
    is_global_init_phase: bool,

    stats: HIRStats,


    /// The compilation's module graph (mangling final) and the entry record.
    /// Codegen identity for module-owned symbols is keyed by the *defining*
    /// record, never by a source-written alias.
    graph: *module_graph.ModuleGraph,
    entry_module: module_graph.ModuleId,
    /// The record whose body is currently being lowered.
    current_module: module_graph.ModuleId,

    loop_context_stack: std.array_list.Managed(LoopContext),
    current_function_scope_id: ?u32 = null,

    deferred_stack: std.array_list.Managed(DeferredBlock),
    loop_deferred_boundaries: std.array_list.Managed(usize),

    next_scope_id: u32 = 0,

    // Serial number for unnamed temporary variables materialized during codegen
    // (e.g. a struct-method receiver that is not a plain variable).
    next_temp_id: u32 = 0,

    /// The next slot `tempSlot` mints: above every storage id the analyzer
    /// issued, so a generator temporary never shares a binding's storage.
    next_temp_slot: Slot,

    pub const FunctionInfo = SoxaTypes.FunctionInfo;

    pub const FunctionBody = struct {
        function_info: FunctionInfo,
        statements: []ast.Stmt,
        start_instruction_index: u32,
        function_name: []const u8,
        function_params: []ast.FunctionParam,
        return_type_info: ast.TypeInfo,
        /// The record this body was defined in.
        module_id: module_graph.ModuleId,
        /// The body's internal identity `(defining record, declared name)`.
        key: module_graph.SymbolKey,
        /// The resolved parameter types, as analysis recorded them.
        param_types: []const ast.TypeInfo,
    };

    pub const LoopContext = struct {
        break_label: []const u8,
        continue_label: []const u8,
        loop_scope_id: u32,
        body_scope_id: u32,
        has_runtime_scope: bool,
    };

    pub const DeferredBlock = struct {
        actions: std.array_list.Managed(DeferredAction),

        pub fn init(allocator: std.mem.Allocator) DeferredBlock {
            return .{ .actions = std.array_list.Managed(DeferredAction).init(allocator) };
        }

        pub fn deinit(self: *DeferredBlock) void {
            self.actions.deinit();
        }
    };

    pub const DeferredAction = struct {
        expr: *ast.Expr,
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

    /// A generator for the analyzed program. The module graph's mangling is
    /// final: every type gets its canonical key, every global site its link
    /// name, and the analyzer's type and method tables their codegen view.
    pub fn init(io: std.Io, allocator: std.mem.Allocator, reporter: *Reporter, semantic: *SemanticAnalyzer) !HIRGenerator {
        var self = HIRGenerator{
            .io = io,
            .allocator = allocator,
            .instructions = std.array_list.Managed(HIRInstruction).init(allocator),
            .current_peek_expr = null,
            .string_pool = std.array_list.Managed([]const u8).init(allocator),
            .reporter = reporter,
            .constant_manager = ConstantManager.init(allocator),
            .label_generator = LabelGenerator.init(allocator),
            .type_system = TypeSystem.init(allocator, reporter, semantic, &semantic.union_table),
            .struct_methods = std.StringHashMap(std.StringHashMap(StructMethodInfo)).init(allocator),
            .alias_params = std.AutoHashMap(Slot, u32).init(allocator),
            .function_signatures = SoxaTypes.FunctionSignatureMap.init(allocator),
            .function_bodies = std.array_list.Managed(FunctionBody).init(allocator),
            .function_indices = std.StringHashMap(u32).init(allocator),
            .zig_functions = std.StringHashMap(*const ast.ZigFnSig).init(allocator),
            .semantic = semantic,
            .current_function = null,
            .current_function_return_type = .Nothing,
            .is_global_init_phase = false,
            .reflected_structs = std.StringHashMap(void).init(allocator),
            .graph = semantic.graph,
            .entry_module = semantic.entry_module,
            .current_module = semantic.entry_module,
            .stats = HIRStats.init(allocator),
            .loop_context_stack = std.array_list.Managed(LoopContext).init(allocator),
            .deferred_stack = std.array_list.Managed(DeferredBlock).init(allocator),
            .loop_deferred_boundaries = std.array_list.Managed(usize).init(allocator),
            .next_temp_slot = semantic.memory.scope_manager.next_storage_id,
        };
        try self.bindTypes();
        try self.bindGlobalLinkNames();
        return self;
    }

    /// Project the analyzer's type and method tables onto canonical keys —
    /// the identity codegen and the emitter name types by.
    fn bindTypes(self: *HIRGenerator) !void {
        var custom_it = self.semantic.custom_types.iterator();
        while (custom_it.next()) |entry| {
            const key = self.typeKey(entry.key_ptr.*);
            try self.type_system.custom_types.put(key, try self.codegenCustomType(entry.value_ptr.*));
        }
        var methods_it = self.semantic.struct_methods.iterator();
        while (methods_it.next()) |entry| {
            var table = std.StringHashMap(StructMethodInfo).init(self.allocator);
            var it = entry.value_ptr.iterator();
            while (it.next()) |method| try table.put(method.key_ptr.*, method.value_ptr.*);
            try self.struct_methods.put(self.typeKey(entry.key_ptr.*), table);
        }
    }

    /// The codegen view of an analyzed type: fields and variants with their
    /// HIR types, named by canonical key.
    fn codegenCustomType(self: *HIRGenerator, semantic_type: @import("../../types/types.zig").CustomTypeInfo) !TypeSystem.CustomTypeInfo {
        var hir_type = TypeSystem.CustomTypeInfo{
            .name = self.typeKey(semantic_type.ref),
            .kind = switch (semantic_type.kind) {
                .Struct => .Struct,
                .Enum => .Enum,
                .Group => .Group,
            },
            .enum_variants = null,
            .struct_fields = null,
            .group_members = null,
        };
        if (semantic_type.enum_variants) |variants| {
            const converted = try self.allocator.alloc(TypeSystem.CustomTypeInfo.EnumVariant, variants.len);
            for (variants, converted) |variant, *dest| dest.* = .{ .name = variant.name, .index = variant.index };
            hir_type.enum_variants = converted;
        }
        if (semantic_type.struct_fields) |fields| {
            const converted = try self.allocator.alloc(TypeSystem.CustomTypeInfo.StructField, fields.len);
            for (fields, converted) |field, *dest| dest.* = .{
                .name = field.name,
                .field_type = try self.lowerType(field.field_type_info),
                .index = field.index,
                .custom_type_name = self.typeKeyOf(field.field_type_info.*),
            };
            hir_type.struct_fields = converted;
        }
        if (semantic_type.group_members) |members| {
            const converted = try self.allocator.alloc(TypeSystem.CustomTypeInfo.GroupMemberSource, members.len);
            for (members, converted) |member, *dest| dest.* = .{
                .qualifier = member.qualifier,
                .key = self.typeKey(member.ref),
            };
            hir_type.group_members = converted;
        }
        return hir_type;
    }

    /// Give every declaration of and reference to a module-level global the
    /// global's link name. Codegen's storage is keyed by a variable's name, so
    /// after this two modules' same-named globals can never share a slot, and a
    /// namespace member access (`ns.counter`) is a plain reference to the
    /// global it resolved to.
    fn bindGlobalLinkNames(self: *HIRGenerator) !void {
        for (self.semantic.global_sites.values()) |site| {
            const link = try self.graph.mangle(self.allocator, site.symbol.module, .global, &.{site.symbol.name});
            switch (site.place) {
                .name => |token| token.lexeme = link,
                .member => |expr| {
                    var tok = expr.data.FieldAccess.field;
                    tok.lexeme = link;
                    expr.data = .{ .Variable = tok };
                },
            }
        }
    }

    /// The canonical codegen key of a named type. Every type that reaches
    /// codegen was registered by analysis, so it has one.
    pub fn typeKey(self: *const HIRGenerator, ref: TypeRef) []const u8 {
        return self.type_system.refKey(ref).?;
    }

    /// The canonical key of the named type `type_info` refers to, if any.
    pub fn typeKeyOf(self: *const HIRGenerator, type_info: ast.TypeInfo) ?[]const u8 {
        const custom = type_info.custom_type orelse return null;
        return self.typeKey(custom.resolved());
    }

    /// What analysis resolved `expr` to, if it names a module-level entity.
    pub fn resolutionOf(self: *const HIRGenerator, expr: *const ast.Expr) ?Resolution {
        return self.semantic.resolutionOf(expr);
    }

    pub fn deinit(self: *HIRGenerator) void {
        self.instructions.deinit();
        self.string_pool.deinit();
        self.constant_manager.deinit();
        self.type_system.deinit();
        var methods_it = self.struct_methods.valueIterator();
        while (methods_it.next()) |tbl| tbl.*.deinit();
        self.struct_methods.deinit();
        self.alias_params.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_indices.deinit();
        self.zig_functions.deinit();

        self.loop_context_stack.deinit();
        for (self.deferred_stack.items) |*block| {
            block.deinit();
        }
        self.deferred_stack.deinit();
        self.loop_deferred_boundaries.deinit();
    }

    pub inline fn pushLoopContext(
        self: *HIRGenerator,
        break_label: []const u8,
        continue_label: []const u8,
        loop_scope_id: u32,
        body_scope_id: u32,
        has_runtime_scope: bool,
    ) !void {
        try self.loop_context_stack.append(.{
            .break_label = break_label,
            .continue_label = continue_label,
            .loop_scope_id = loop_scope_id,
            .body_scope_id = body_scope_id,
            .has_runtime_scope = has_runtime_scope,
        });
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

    pub fn pushDeferredBlock(self: *HIRGenerator) !void {
        try self.deferred_stack.append(DeferredBlock.init(self.allocator));
    }

    pub fn popAndEmitDeferred(self: *HIRGenerator) !void {
        if (self.deferred_stack.items.len == 0) return;
        var block = self.deferred_stack.pop() orelse return;
        defer block.deinit();
        var j = block.actions.items.len;
        while (j > 0) {
            j -= 1;
            try self.generateExpression(block.actions.items[j].expr, false, false);
        }
    }

    pub fn emitDeferredToBoundary(self: *HIRGenerator, boundary_index: usize) !void {
        var i = self.deferred_stack.items.len;
        while (i > boundary_index) {
            i -= 1;
            const block = self.deferred_stack.items[i];
            var j = block.actions.items.len;
            while (j > 0) {
                j -= 1;
                try self.generateExpression(block.actions.items[j].expr, false, false);
            }
        }
    }

    pub fn emitAllDeferredForReturn(self: *HIRGenerator) !void {
        var i = self.deferred_stack.items.len;
        while (i > 0) {
            i -= 1;
            const block = self.deferred_stack.items[i];
            var j = block.actions.items.len;
            while (j > 0) {
                j -= 1;
                try self.generateExpression(block.actions.items[j].expr, false, false);
            }
        }
    }

    pub fn generateProgram(self: *HIRGenerator) !HIRProgram {
        const records = try self.programRecords();

        // Pass 1: every function and method of the program, so any call can
        // name any callee.
        try self.collectFunctionSignatures(records);

        // Pass 2: module globals, before the entry program runs.
        try self.generateGlobalInitialization(records);

        // Pass 3: the entry program.
        try self.generateMainProgram(self.graph.record(self.entry_module).statements());

        // Pass 4: function bodies.
        try self.generateFunctionBodies();

        const function_table = try self.buildFunctionTable();
        const instructions_slice = try self.instructions.toOwnedSlice();
        return HIRProgram{
            .instructions = instructions_slice,
            .constant_pool = try self.constant_manager.toOwnedSlice(),
            .string_pool = try self.string_pool.toOwnedSlice(),
            .function_table = function_table,
            .allocator = self.allocator,
            .reflected_structs = try self.cloneReflectedStructs(),
            .force_struct_descriptors = self.force_struct_descriptors,
            .zig_functions = try self.programZigFunctions(),
        };
    }

    /// The inline-Zig callees the program reached, with their wrappers'
    /// parameter types, for the emitter's declarations.
    fn programZigFunctions(self: *HIRGenerator) ![]const HIRProgram.ZigFunction {
        const functions = try self.allocator.alloc(HIRProgram.ZigFunction, self.zig_functions.count());
        var it = self.zig_functions.iterator();
        var i: usize = 0;
        while (it.next()) |entry| : (i += 1) {
            const sig = entry.value_ptr.*;
            const param_types = try self.allocator.alloc(HIRType, sig.param_types.len);
            for (sig.param_types, param_types) |param, *dest| dest.* = try self.lowerType(&param);
            functions[i] = .{ .link_name = entry.key_ptr.*, .param_types = param_types, .return_type = try self.lowerType(&sig.return_type) };
        }
        return functions;
    }

    /// Copy the reflected-struct key set into an owned map for the returned
    /// `HIRProgram`. The generator (and its map) is deinited as soon as
    /// `generateProgram` returns, so the program cannot borrow it. The keys are
    /// borrowed struct-table keys, which outlive the program.
    fn cloneReflectedStructs(self: *HIRGenerator) !std.StringHashMap(void) {
        var copy = std.StringHashMap(void).init(self.allocator);
        errdefer copy.deinit();
        try copy.ensureTotalCapacity(@intCast(self.reflected_structs.count()));
        var it = self.reflected_structs.keyIterator();
        while (it.next()) |key| copy.putAssumeCapacity(key.*, {});
        return copy;
    }

    /// The Doxa records whose code is part of the program — every record
    /// analysis checked — in stable-key order, so the emitted program never
    /// depends on the order modules were discovered in.
    fn programRecords(self: *HIRGenerator) ![]*module_graph.ModuleRecord {
        var records = std.array_list.Managed(*module_graph.ModuleRecord).init(self.allocator);
        for (self.graph.records.items) |record| {
            if (record.kind != .doxa or record.status != .Analyzed) continue;
            try records.append(record);
        }
        std.sort.pdq(*module_graph.ModuleRecord, records.items, {}, struct {
            fn lessThan(_: void, a: *module_graph.ModuleRecord, b: *module_graph.ModuleRecord) bool {
                return std.mem.lessThan(u8, a.stable_module_key, b.stable_module_key);
            }
        }.lessThan);
        return records.toOwnedSlice();
    }

    fn collectFunctionSignatures(self: *HIRGenerator, records: []const *module_graph.ModuleRecord) !void {
        for (records) |record| {
            for (record.statements()) |*stmt| {
                switch (stmt.data) {
                    .FunctionDecl => try self.registerFunctionSignature(record.id, stmt),
                    .Expression => |maybe_expr| if (maybe_expr) |expr| {
                        if (expr.data == .StructDecl) try self.registerStructMethodSignatures(&expr.data.StructDecl, record.id);
                    },
                    else => {},
                }
            }
        }
    }

    /// Register a top-level function under `(module, name)` with its link
    /// name and resolved signature.
    fn registerFunctionSignature(self: *HIRGenerator, module: module_graph.ModuleId, stmt: *ast.Stmt) !void {
        const func = &stmt.data.FunctionDecl;
        const key = module_graph.SymbolKey{ .module = module, .name = func.name.lexeme };
        const link_name = try self.graph.mangle(self.allocator, module, .function, &.{func.name.lexeme});
        const signature = self.declaredSignature(module, func.name.lexeme);

        var return_type = try self.lowerType(&func.return_type_info);
        // A function without an explicit `returns` takes the analyzer's
        // inferred return type.
        if (func.return_type_info.base == .Nothing) {
            if (self.semantic.function_return_types.get(stmt.base.id)) |inferred| {
                return_type = try self.lowerType(inferred);
            }
        }

        const param_is_alias = try self.allocator.alloc(bool, func.params.len);
        const param_is_readonly = try self.allocator.alloc(bool, func.params.len);
        const param_types = try self.allocator.alloc(HIRType, func.params.len);
        for (func.params, 0..) |param, i| {
            param_is_alias[i] = param.is_alias;
            param_is_readonly[i] = !ParamMutation.bodyMutatesVariable(func.body, param.name.lexeme);
            param_types[i] = try self.lowerType(&signature.params[i]);
        }

        const function_info = FunctionInfo{
            .name = link_name,
            .arity = @intCast(func.params.len),
            .return_type = return_type,
            .start_label = try self.label_generator.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{link_name})),
            .is_entry = func.is_entry,
            .param_is_alias = param_is_alias,
            .param_is_readonly = param_is_readonly,
            .param_types = param_types,
        };
        try self.addFunction(.{
            .function_info = function_info,
            .statements = func.body,
            .start_instruction_index = 0,
            .function_name = link_name,
            .function_params = func.params,
            .return_type_info = func.return_type_info,
            .module_id = module,
            .key = key,
            .param_types = signature.params,
        });
    }

    /// The analyzed signature of the top-level function `name` of `module`.
    fn declaredSignature(self: *HIRGenerator, module: module_graph.ModuleId, name: []const u8) *const ast.FunctionType {
        const variable = self.semantic.moduleScope(module).lookupLocalVariable(name).?;
        const storage = self.semantic.memory.scope_manager.value_storage.get(variable.storage_id).?;
        return storage.type_info.function_type.?;
    }

    fn addFunction(self: *HIRGenerator, body: FunctionBody) !void {
        const index: u32 = @intCast(self.function_bodies.items.len);
        try self.function_signatures.put(body.key, body.function_info);
        try self.function_bodies.append(body);
        try self.function_indices.put(body.function_info.name, index);
    }

    /// The callee for a Doxa function or method identified by `key`.
    pub fn doxaCallee(self: *HIRGenerator, key: module_graph.SymbolKey) !ModuleCall.Callee {
        const info = self.function_signatures.get(key) orelse return error.UnresolvedCallee;
        const index = self.function_indices.get(info.name).?;
        return .{
            .link_name = info.name,
            .kind = .DoxaFunction,
            .index = index,
            .params = self.function_bodies.items[index].param_types,
        };
    }

    /// The callee for a function of an inline `zig` block or a `.zig` file:
    /// the wrapper the inline-Zig compiler exports under the same link name.
    pub fn zigCallee(self: *HIRGenerator, symbol: module_graph.SymbolRef) !ModuleCall.Callee {
        const sig = self.graph.declOf(symbol).?.zig_function;
        const link_name = try self.graph.mangle(self.allocator, symbol.module, .function, &.{symbol.name});
        try self.zig_functions.put(link_name, sig);
        return .{ .link_name = link_name, .kind = .ZigFunction, .index = null, .params = sig.param_types };
    }

    /// The callee for a struct method.
    pub fn methodCallee(self: *HIRGenerator, method: Resolution.Method) !ModuleCall.Callee {
        return self.doxaCallee(.{ .module = method.owner.module, .name = try methodKeyName(self.allocator, method.owner.name, method.name) });
    }

    /// Pass 3: Generate function bodies AFTER main program
    fn generateFunctionBodies(self: *HIRGenerator) !void {
        for (self.function_bodies.items) |*function_body| {
            self.current_function = function_body.function_info.name;
            self.current_module = function_body.module_id;
            self.current_function_return_type = function_body.function_info.return_type;
            self.is_global_init_phase = false;

            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label } });

            const function_scope_id = self.nextScopeId();
            self.current_function_scope_id = function_scope_id;
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = function_scope_id, .var_count = 0 } });

            self.alias_params.clearRetainingCapacity();
            self.this_slot = null;
            const params = function_body.function_params;

            const is_method = function_body.function_info.arity > params.len;

            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                const param_info = function_body.param_types[param_index];
                const param_type = try self.lowerType(&param_info);

                const alias_lookup = if (is_method) param_index + 1 else param_index;

                if (function_body.function_info.param_is_alias[alias_lookup]) {
                    switch (param_type) {
                        .Map, .Function => return error.InvalidAliasType,
                        else => {},
                    }
                    // An alias of a struct or an enum lends a named type.
                    if ((param_info.base == .Struct or param_info.base == .Enum) and param_info.custom_type == null) return error.InvalidAliasType;

                    const alias_slot = self.allocAliasSlot();
                    try self.alias_params.put(try self.paramSlot(param), alias_slot);
                    try self.instructions.append(.{
                        .BindAlias = .{
                            .alias_name = param.name.lexeme,
                            .target_variable_name = param.name.lexeme,
                            .alias_slot = alias_slot,
                            .target_type = param_type,
                        },
                    });
                } else {
                    // A by-value heap parameter is deep-copied on entry so the
                    // callee cannot write through to the caller's object. When the
                    // body never writes through it, the copy is unobservable and
                    // only costs an O(n) clone plus an arena allocation per call.
                    // Whether the body mutates the parameter is decided once when
                    // the signature metadata is built (`param_is_readonly`), not
                    // re-derived per binding here.
                    try self.instructions.append(.{ .StoreVar = .{
                        .slot = try self.paramSlot(param),
                        .var_name = param.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = param_type,
                        .heap_copy = if (function_body.function_info.param_is_readonly[alias_lookup]) .keep else .snapshot,
                    } });
                }
            }

            // An instance method binds its receiver as the alias `this`.
            if (function_body.function_info.receiver) |struct_id| {
                const receiver_type = HIRType{ .Struct = struct_id };
                const alias_slot = self.allocAliasSlot();
                const this_slot = self.tempSlot();
                self.this_slot = this_slot;
                try self.alias_params.put(this_slot, alias_slot);
                try self.instructions.append(.{
                    .BindAlias = .{
                        .alias_name = "this",
                        .target_variable_name = "this",
                        .alias_slot = alias_slot,
                        .target_type = receiver_type,
                    },
                });
            }

            const body_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}_body", .{function_body.function_info.name}));
            try self.instructions.append(.{ .Label = .{ .name = body_label } });

            if (self.function_signatures.getPtr(function_body.key)) |func_info| {
                func_info.body_label = body_label;
            }
            var has_returned = false;
            try self.pushDeferredBlock();
            for (function_body.statements) |body_stmt| {
                if (has_returned) {
                    break;
                }

                try SoxaStatements.generateStatement(self, body_stmt);

                has_returned = self.statementAlwaysReturns(body_stmt);
            }

            try self.popAndEmitDeferred();

            var needs_implicit_return = true;

            if (has_returned) {
                needs_implicit_return = false;
            } else if (self.instructions.items.len > 0) {
                const last_instruction = self.instructions.items[self.instructions.items.len - 1];
                if (last_instruction == .Return) {
                    needs_implicit_return = false;
                }
            }

            if (needs_implicit_return) {
                const has_value = self.current_function_return_type != .Nothing;
                try self.instructions.append(.{ .Return = .{ .has_value = has_value, .return_type = self.current_function_return_type } });
            }

            if (!has_returned) {
                try self.instructions.append(.{ .ExitScope = .{ .scope_id = function_scope_id } });
            }
            self.current_function_scope_id = null;

            self.current_function = null;
            self.current_module = self.entry_module;
            self.current_function_return_type = .Nothing;
            self.current_function_scope_id = null;
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

        var entry_function: ?FunctionInfo = null;
        var entry_function_name: ?[]const u8 = null;
        for (self.function_bodies.items) |body| {
            if (body.function_info.is_entry) {
                entry_function = body.function_info;
                entry_function_name = body.function_info.name;
                break;
            }
        }

        if (entry_function) |entry_func| {
            const entry_function_index = if (entry_function_name) |name|
                self.getFunctionIndex(name)
            else
                null;

            const entry_call_instruction = HIRInstruction{
                .Call = .{
                    .function_index = entry_function_index,
                    .qualified_name = entry_function_name orelse "main",
                    .arg_count = 0,
                    .call_kind = .DoxaFunction,
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

    /// Pass 2: Initialize global variables at module level (before main program execution)
    fn generateGlobalInitialization(self: *HIRGenerator, records: []const *module_graph.ModuleRecord) !void {
        self.is_global_init_phase = true;
        defer self.is_global_init_phase = false;

        const previous_function = self.current_function;
        self.current_function = null;
        defer self.current_function = previous_function;

        // Each imported record's globals, in stable-key order. The entry
        // record's globals are initialized by the main program itself.
        for (records) |record| {
            if (record.id == self.entry_module) continue;
            self.current_module = record.id;
            for (record.statements()) |stmt| {
                if (stmt.data == .VarDecl) try SoxaStatements.generateStatement(self, stmt);
            }
        }
        self.current_module = self.entry_module;
    }

    /// Pass 4: Build function table from collected signatures
    fn buildFunctionTable(self: *HIRGenerator) ![]HIRProgram.HIRFunction {
        var function_table = std.array_list.Managed(HIRProgram.HIRFunction).init(self.allocator);

        for (self.function_bodies.items) |function_body| {
            const function_info = function_body.function_info;

            try function_table.append(HIRProgram.HIRFunction{
                .qualified_name = function_info.name,
                .receiver = function_info.receiver,
                .arity = function_info.arity,
                .return_type = function_info.return_type,
                .start_label = function_info.start_label,
                .body_label = function_info.body_label,
                .start_ip = 0,
                .body_ip = null,
                .is_entry = function_info.is_entry,
                .param_is_alias = function_info.param_is_alias,
                .param_is_readonly = function_info.param_is_readonly,
                .param_types = function_info.param_types,
            });
        }

        return try function_table.toOwnedSlice();
    }

    pub fn getFunctionIndex(self: *HIRGenerator, link_name: []const u8) ?u32 {
        return self.function_indices.get(link_name);
    }

    pub fn lowerType(self: *HIRGenerator, type_info: *const ast.TypeInfo) ErrorList!HIRType {
        return self.type_system.lowerType(type_info);
    }

    pub fn findFunctionBody(self: *HIRGenerator, link_name: []const u8) ?*FunctionBody {
        const index = self.function_indices.get(link_name) orelse return null;
        return &self.function_bodies.items[index];
    }

    /// The signature of a Doxa function by its link name.
    pub fn functionInfoByLink(self: *HIRGenerator, link_name: []const u8) ?FunctionInfo {
        const index = self.function_indices.get(link_name) orelse return null;
        return self.function_bodies.items[index].function_info;
    }

    pub const TETRA_AND_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 0, 0, 0, 0 },
        [4]u8{ 0, 1, 2, 3 },
        [4]u8{ 0, 2, 2, 0 },
        [4]u8{ 0, 3, 0, 3 },
    };

    pub const TETRA_OR_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 0, 1, 2, 3 },
        [4]u8{ 1, 1, 1, 1 },
        [4]u8{ 2, 1, 2, 2 },
        [4]u8{ 3, 1, 2, 3 },
    };

    pub const TETRA_NOT_LUT: [4]u8 = [4]u8{ 1, 0, 3, 2 }; // NOT lookup: false->true, true->false, both->neither, neither->both

    // IFF (if and only if): A ↔ B - true when A and B have same truth value
    pub const TETRA_IFF_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 0, 3, 2 },
        [4]u8{ 0, 1, 2, 3 },
        [4]u8{ 3, 2, 2, 3 },
        [4]u8{ 2, 3, 3, 2 },
    };

    // XOR (exclusive or): A ⊕ B - true when A and B have different truth values
    pub const TETRA_XOR_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 0, 1, 2, 3 },
        [4]u8{ 1, 0, 3, 2 },
        [4]u8{ 2, 3, 2, 3 },
        [4]u8{ 3, 2, 3, 2 },
    };

    // NAND: A ↑ B - NOT(A AND B)
    pub const TETRA_NAND_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 1, 1, 1 },
        [4]u8{ 1, 0, 3, 2 },
        [4]u8{ 1, 3, 3, 1 },
        [4]u8{ 1, 2, 1, 2 },
    };

    // NOR: A ↓ B - NOT(A OR B)
    pub const TETRA_NOR_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 0, 3, 2 },
        [4]u8{ 0, 0, 0, 0 },
        [4]u8{ 3, 0, 3, 3 },
        [4]u8{ 2, 0, 3, 2 },
    };

    // IMPLIES: A → B - NOT A OR B
    pub const TETRA_IMPLIES_LUT: [4][4]u8 = [4][4]u8{
        [4]u8{ 1, 1, 1, 1 },
        [4]u8{ 0, 1, 2, 3 },
        [4]u8{ 2, 1, 2, 2 },
        [4]u8{ 3, 1, 2, 3 },
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
            .This => try basic_handler.generateThis(),
            .Literal => |lit| try basic_handler.generateLiteral(lit, preserve_result, should_pop_after_use),
            .InterpolatedString => |template| try basic_handler.generateInterpolatedString(template, preserve_result, should_pop_after_use),
            .Variable => |var_token| try self.loadName(&expr.base, var_token.lexeme),
            .Grouping => |grouping| try basic_handler.generateGrouping(grouping, preserve_result),
            .EnumMember => try basic_handler.generateEnumMember(expr),
            .DefaultArgPlaceholder => try basic_handler.generateDefaultArgPlaceholder(),

            .Binary => try binary_handler.generateBinary(expr, should_pop_after_use),
            .Logical => |log| try binary_handler.generateLogical(log, should_pop_after_use),
            .Unary => |unary| try binary_handler.generateUnary(unary),

            .If => try control_flow_handler.generateIf(expr, preserve_result, should_pop_after_use),
            .Match => try control_flow_handler.generateMatch(expr, preserve_result),
            .Loop => |loop| try control_flow_handler.generateLoop(loop, preserve_result),
            .Block => try control_flow_handler.generateBlock(expr.data, preserve_result),
            .ReturnExpr => try control_flow_handler.generateReturn(expr.data),
            .Unreachable => try control_flow_handler.generateUnreachable(expr),
            .Cast => try control_flow_handler.generateCast(expr, preserve_result),

            .Array => try collections_handler.generateArray(expr, preserve_result),
            .Map => |map_expr| try collections_handler.generateMap(expr, map_expr.entries, null),
            .MapLiteral => |map_literal| try collections_handler.generateMap(expr, map_literal.entries, map_literal.else_value),
            .Index => try collections_handler.generateIndex(expr, preserve_result, should_pop_after_use),
            .IndexAssign => try collections_handler.generateIndexAssign(expr.data, preserve_result),
            .ForAll => try collections_handler.generateForAll(expr.data),
            .Exists => try collections_handler.generateExists(expr.data),
            .Increment => |operand| try collections_handler.generateIncrement(operand),
            .Decrement => |operand| try collections_handler.generateDecrement(operand),
            .Range => |range| try collections_handler.generateRange(.{ .start = range.start, .end = range.end }, preserve_result),

            .FunctionCall => try calls_handler.generateFunctionCall(expr.data, preserve_result, should_pop_after_use),
            .InternalCall => try calls_handler.generateInternalCall(expr, preserve_result),

            .StructLiteral => try structs_handler.generateStructLiteral(expr),
            .FieldAccess => |field| try structs_handler.generateFieldAccess(field),
            .FieldAssignment => try structs_handler.generateFieldAssignment(expr.data),
            .EnumDecl, .StructDecl, .GroupDecl => try structs_handler.generateTypeDecl(),

            .Assignment => try assignments_handler.generateAssignment(expr, preserve_result),

            .Print => unreachable, // @print is lowered to an InternalCall
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

    pub fn generateLabel(self: *HIRGenerator, prefix: []const u8) ![]const u8 {
        return self.label_generator.generateLabel(prefix);
    }

    /// Return a unique scope ID. Each call increments the counter so IDs
    /// are guaranteed not to collide with labels or with each other.
    pub fn nextScopeId(self: *HIRGenerator) u32 {
        const id = self.next_scope_id;
        self.next_scope_id += 1;
        return id;
    }

    /// Register each method of the struct `decl` declared in `module` under
    /// `(module, "Struct.method")`, with its link name, analyzed signature, and —
    /// for an instance method — its receiver.
    fn registerStructMethodSignatures(self: *HIRGenerator, decl: *ast.StructDecl, module: module_graph.ModuleId) !void {
        const ref = TypeRef{ .module = module, .name = decl.name.lexeme };
        const methods = self.semantic.struct_methods.get(ref).?;
        const struct_id = self.semantic.struct_table.idOf(ref).?;
        for (decl.methods) |method| {
            const info = methods.get(method.name.lexeme).?;
            const key_name = try methodKeyName(self.allocator, decl.name.lexeme, method.name.lexeme);
            const link_name = try self.graph.mangle(self.allocator, module, .method, &.{ decl.name.lexeme, method.name.lexeme });

            var arity: u32 = @intCast(method.params.len);
            if (!method.is_static) arity += 1;

            const param_is_alias = try self.allocator.alloc(bool, arity);
            const param_is_readonly = try self.allocator.alloc(bool, arity);
            const param_types = try self.allocator.alloc(HIRType, arity);
            var param_idx: usize = 0;
            if (!method.is_static) {
                param_is_alias[0] = true;
                param_is_readonly[0] = false;
                param_types[0] = HIRType{ .Struct = struct_id };
                param_idx = 1;
            }
            for (method.params, 0..) |param, i| {
                param_is_alias[param_idx] = param.is_alias;
                param_is_readonly[param_idx] = !ParamMutation.bodyMutatesVariable(method.body, param.name.lexeme);
                param_types[param_idx] = try self.lowerType(&info.signature.params[i]);
                param_idx += 1;
            }

            try self.addFunction(.{
                .function_info = .{
                    .name = link_name,
                    .receiver = if (method.is_static) null else struct_id,
                    .arity = arity,
                    .return_type = try self.lowerType(info.signature.return_type),
                    .start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{link_name})),
                    .is_entry = false,
                    .param_is_alias = param_is_alias,
                    .param_is_readonly = param_is_readonly,
                    .param_types = param_types,
                },
                .statements = method.body,
                .start_instruction_index = 0,
                .function_name = link_name,
                .function_params = method.params,
                .return_type_info = info.signature.return_type.*,
                .module_id = module,
                .key = .{ .module = module, .name = key_name },
                .param_types = info.signature.params,
            });
        }
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

    /// Push a struct-method receiver in the form the callee expects: the address
    /// of a slot holding the struct pointer, since `this` is an alias parameter
    /// and every method body dereferences its receiver (`load ptr, ptr`).
    ///
    /// A plain variable is pushed by its storage id directly. Any other receiver
    /// expression (nested field access like `pair.left`, a function-call result,
    /// ...) evaluates to the struct pointer itself, so it is first materialized
    /// into a temporary slot and the slot's address is pushed instead. Without
    /// this, the method would dereference the struct pointer as if it were a
    /// slot address and read garbage for `this.field`.
    pub fn pushStructReceiver(self: *HIRGenerator, receiver: *ast.Expr) !void {
        if (receiver.data == .Variable) {
            const var_token = receiver.data.Variable;

            try self.pushStorageOfName(&receiver.base, var_token.lexeme);
            return;
        }

        const temp_id = self.next_temp_id;
        self.next_temp_id += 1;
        const temp_name = try std.fmt.allocPrint(self.allocator, "__recv_{d}", .{temp_id});
        const temp_slot = self.tempSlot();
        try self.generateExpression(receiver, true, false);
        try self.instructions.append(.{
            .StoreVar = .{
                .slot = temp_slot,
                .var_name = temp_name,
                .scope_kind = .Local,
                .module_context = null,
                .expected_type = try self.typeOf(receiver),
                // The receiver already lives in this scope's arena or an
                // outer one, so a re-home could only keep its identity.
                .heap_copy = .keep,
            },
        });
        try self.instructions.append(.{
            .PushStorageId = .{
                .slot = temp_slot,
                .var_name = temp_name,
                .scope_kind = .Local,
            },
        });
    }

    pub fn tryGenerateTailCall(self: *HIRGenerator, expr: *ast.Expr) bool {
        return switch (expr.data) {
            .FunctionCall => ModuleCall.tryEmitTailCall(self, expr),
            else => false,
        };
    }

    /// B1: the field types (declaration order) of a struct-typed array element,
    /// resolved from the whole-program struct table by id. Null when the element
    /// is not a struct, or the table has no entry, so callers fall back to the
    /// box-pointer representation.
    pub fn elementStructFieldTypes(self: *HIRGenerator, element_type: HIRType) ?[]HIRType {
        if (element_type != .Struct or element_type.Struct == 0) return null;
        const fields = self.semantic.struct_table.fields(element_type.Struct) orelse return null;
        const out = self.allocator.alloc(HIRType, fields.len) catch return null;
        for (fields, 0..) |f, i| out[i] = f.hir_type;
        return out;
    }

    /// B1: the declaration name of a struct-typed array element (for backend
    /// field/metadata resolution). Null when the element is not a struct.
    pub fn elementStructTypeName(self: *HIRGenerator, element_type: HIRType) ?[]const u8 {
        if (element_type != .Struct or element_type.Struct == 0) return null;
        return self.semantic.struct_table.keyOf(element_type.Struct);
    }

    /// B2: record that a value of this type reaches a reflection site
    /// (`"{x}"` interpolation, `@string(x)`, or `peek`), transitively through the
    /// containers it can be printed as part of. A reflected struct must keep its
    /// descriptor registry entry. A group (whose members are not enumerated here)
    /// or unresolved type forces every struct to keep its descriptor.
    pub fn markReflectedType(self: *HIRGenerator, t: HIRType) void {
        switch (t) {
            .Struct => |sid| {
                if (sid == 0) {
                    self.force_struct_descriptors = true;
                    return;
                }
                if (self.semantic.struct_table.keyOf(sid)) |key| {
                    self.reflected_structs.put(key, {}) catch {};
                } else {
                    self.force_struct_descriptors = true;
                }
            },
            .Array => |inner| self.markReflectedType(inner.*),
            .Map => |kv| {
                self.markReflectedType(kv.key.*);
                self.markReflectedType(kv.value.*);
            },
            .Union => |u| for (u.members) |m| self.markReflectedType(m.*),
            .Group, .Unknown, .Poison => self.force_struct_descriptors = true,
            else => {},
        }
    }

    /// A3: lower the value expression of a `return`. When the top-level
    /// expression is a struct or array literal — a construction that can be born
    /// where the result must live — mark it so the backend allocates it directly
    /// in the caller's arena, turning clone-on-return into placement. Every other
    /// return value keeps the conservative clone path.
    pub fn generateReturnValue(self: *HIRGenerator, value: *ast.Expr) !void {
        const constructs = switch (value.data) {
            .StructLiteral, .Array => true,
            else => false,
        };
        if (!constructs) {
            try self.generateExpression(value, true, false);
            return;
        }
        const saved = self.place_return_value;
        self.place_return_value = true;
        defer self.place_return_value = saved;
        try self.generateExpression(value, true, false);
    }

    pub fn collectUnionMemberNamesFromHIRType(self: *HIRGenerator, hir_type: HIRType) ![][]const u8 {
        if (hir_type != .Union) return &[_][]const u8{};
        const union_members = hir_type.Union.members;
        var names = try self.allocator.alloc([]const u8, union_members.len);
        for (union_members, 0..) |member_ptr, i| {
            names[i] = try self.hirTypeToDisplayName(member_ptr.*);
        }
        return names;
    }

    pub fn hirTypeToDisplayName(self: *HIRGenerator, ty: HIRType) ![]const u8 {
        return switch (ty) {
            .Int => "int",
            .Float => "float",
            .String => "string",
            .Tetra => "tetra",
            .Byte => "byte",
            .Nothing => "nothing",
            .Array => |elem_ptr| blk: {
                const elem_name = try self.hirTypeToDisplayName(elem_ptr.*);
                break :blk try std.fmt.allocPrint(self.allocator, "{s}[]", .{elem_name});
            },
            .Map => "map",
            .Struct => |sid| blk: {
                if (self.semantic.struct_table.displayName(sid)) |name| break :blk name;
                break :blk try std.fmt.allocPrint(self.allocator, "(struct#{})", .{sid});
            },
            .Enum => |eid| blk: {
                if (self.semantic.enum_table.displayName(eid)) |name| break :blk name;
                break :blk try std.fmt.allocPrint(self.allocator, "(enum#{})", .{eid});
            },
            .Group => |gid| blk: {
                if (self.semantic.group_table.displayName(gid)) |name| break :blk name;
                break :blk try std.fmt.allocPrint(self.allocator, "(group#{})", .{gid});
            },
            .Function => "function",
            .Union => |u| blk: {
                if (u.members.len == 0) {
                    break :blk try std.fmt.allocPrint(self.allocator, "(union#{})", .{u.id});
                }
                var list = std.array_list.Managed(u8).init(self.allocator);
                errdefer list.deinit();
                for (u.members, 0..) |m, idx| {
                    if (idx > 0) try list.appendSlice(" | ");
                    const frag = try self.hirTypeToDisplayName(m.*);
                    try list.appendSlice(frag);
                }
                return try list.toOwnedSlice();
            },
            .Unknown => "unknown",
            .Poison => "poison",
        };
    }

    fn convertArrayStorageKind(kind: ast.ArrayStorageKind) SoxaTypes.ArrayStorageKind {
        return switch (kind) {
            .dynamic => .dynamic,
            .fixed => .fixed,
            .const_literal => .const_literal,
        };
    }

    pub fn storageKindFromTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) SoxaTypes.ArrayStorageKind {
        _ = self;
        return convertArrayStorageKind(type_info.array_storage);
    }

    /// The type of `expr`, as the semantic analyzer inferred it.
    ///
    /// This is the one place lowering learns an expression's type
    /// (plan/type-authority.md). The analyzer has visited every expression of
    /// a program that reached this stage and recorded its type per node, with
    /// narrowing already applied to that occurrence, so the generator lowers
    /// the answer instead of deriving a second one from the expression's
    /// shape. An expression the analyzer never typed is a compiler bug and
    /// fails the compile here; there is no fallback type.
    pub fn typeOf(self: *HIRGenerator, expr: *ast.Expr) ErrorList!HIRType {
        return self.type_system.lowerType(try self.typeInfoOf(expr));
    }

    /// The analyzer's type for `expr` as it spells it — the names a `@type`
    /// or a peek shows, the groups a union was written with. Every read of the
    /// analyzer's expression types in codegen goes through here.
    pub fn typeInfoOf(self: *HIRGenerator, expr: *ast.Expr) ErrorList!*const ast.TypeInfo {
        if (self.semantic.getCachedExprType(expr)) |type_info| return type_info;
        const location = expr.base.location();
        self.reporter.reportInternal(
            "no analyzed type for the {s} expression at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ @tagName(std.meta.activeTag(expr.data)), location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingExpressionType;
    }

    /// The type of the storage a store through `node` writes — a name, an
    /// assignment or a variable declaration — as the analyzer resolved it,
    /// beneath every narrowing view. A read of a name has `typeOf`; a store to
    /// it produces this. There is no fallback type.
    pub fn bindingTypeOf(self: *HIRGenerator, node: *const ast.Base) ErrorList!HIRType {
        return self.type_system.lowerType((try self.storeTarget(node)).slot);
    }

    /// What the name a store through `node` writes reads at that store: the
    /// narrowed member inside a view. A read-modify-write computes in it.
    pub fn bindingReadTypeOf(self: *HIRGenerator, node: *const ast.Base) ErrorList!HIRType {
        return self.type_system.lowerType((try self.storeTarget(node)).read);
    }

    /// The storage the name at `node` denotes: the binding beneath every view.
    pub fn slotOf(self: *HIRGenerator, node: *const ast.Base) ErrorList!Slot {
        return (try self.storeTarget(node)).storage;
    }

    /// Where a variable lives and what emitted code calls it.
    pub const Place = struct {
        slot: Slot,
        scope_kind: ScopeKind,
        var_name: []const u8,
    };

    /// The place of a binding with storage `slot`, spelled `name`; `global`
    /// says it is module-level. A module global is named by its link name.
    /// Top-level code lowers every other binding as a global too, so there its
    /// spelling is qualified by its slot to keep bindings that share a name
    /// apart; inside a function it is a local of the frame.
    pub fn placeOf(self: *HIRGenerator, slot: Slot, name: []const u8, global: bool) !Place {
        if (global) return .{ .slot = slot, .scope_kind = .GlobalLocal, .var_name = name };
        if (self.current_function != null) return .{ .slot = slot, .scope_kind = .Local, .var_name = name };
        return .{ .slot = slot, .scope_kind = .GlobalLocal, .var_name = try std.fmt.allocPrint(self.allocator, "{s}.{d}", .{ name, slot }) };
    }

    /// The place of the variable the name at `node` denotes.
    pub fn placeOfName(self: *HIRGenerator, node: *const ast.Base, name: []const u8) !Place {
        const target = try self.storeTarget(node);
        return self.placeOf(target.storage, name, target.global);
    }

    /// Push the value of the variable the name at `node` denotes, as what the
    /// name reads there. Inside a narrowing view the slot still holds the box;
    /// the load converts it to the member (or narrower box) the view proved.
    pub fn loadName(self: *HIRGenerator, node: *const ast.Base, name: []const u8) !void {
        const target = try self.storeTarget(node);
        const place = try self.placeOf(target.storage, name, target.global);
        if (self.alias_params.get(place.slot)) |alias_slot| {
            try self.instructions.append(.{ .LoadAlias = .{ .slot = place.slot, .var_name = name, .slot_index = alias_slot } });
        } else {
            try self.instructions.append(.{ .LoadVar = .{
                .slot = place.slot,
                .var_name = place.var_name,
                .scope_kind = place.scope_kind,
                .module_context = null,
            } });
        }
        try self.convertValue(try self.lowerType(target.slot), try self.lowerType(target.read));
    }

    /// Store the top of the stack, already of `slot_type`, into the variable
    /// the name at `node` denotes.
    pub fn storeName(self: *HIRGenerator, node: *const ast.Base, name: []const u8, slot_type: HIRType, heap_copy: SoxaTypes.HeapCopyKind) !void {
        const place = try self.placeOfName(node, name);
        if (self.alias_params.get(place.slot)) |alias_slot| {
            try self.instructions.append(.{ .StoreAlias = .{
                .var_name = name,
                .slot_index = alias_slot,
                .expected_type = slot_type,
                .heap_copy = heap_copy,
            } });
            return;
        }
        try self.storePlace(place, slot_type, heap_copy);
    }

    /// Push the address of the variable the name at `node` denotes. An alias
    /// (`^`) parameter is not backed by a local slot: its storage pointer is
    /// re-passed from its alias slot.
    /// A view that reads a box as a struct member (a narrowed receiver) pushes
    /// the address of the box's payload word, which holds that struct.
    pub fn pushStorageOfName(self: *HIRGenerator, node: *const ast.Base, name: []const u8) !void {
        const target = try self.storeTarget(node);
        const place = try self.placeOf(target.storage, name, target.global);
        const slot_type = try self.lowerType(target.slot);
        const read_type = try self.lowerType(target.read);
        try self.instructions.append(.{ .PushStorageId = .{
            .slot = place.slot,
            .var_name = place.var_name,
            .scope_kind = place.scope_kind,
            .alias_slot = self.alias_params.get(place.slot),
            .box_payload = if (slot_type.isBoxed() and read_type == .Struct) read_type.Struct else null,
        } });
    }

    /// Store the top of the stack, already of `slot_type`, into `place`.
    pub fn storePlace(self: *HIRGenerator, place: Place, slot_type: HIRType, heap_copy: SoxaTypes.HeapCopyKind) !void {
        try self.instructions.append(.{ .StoreVar = .{
            .slot = place.slot,
            .var_name = place.var_name,
            .scope_kind = place.scope_kind,
            .module_context = null,
            .expected_type = slot_type,
            .heap_copy = heap_copy,
        } });
    }

    /// Store the top of the stack, already of `slot_type`, back into the
    /// variable, alias parameter or `this` that `target` names.
    pub fn storeTo(self: *HIRGenerator, target: *ast.Expr, slot_type: HIRType, heap_copy: SoxaTypes.HeapCopyKind) !void {
        switch (target.data) {
            .Variable => |token| try self.storeName(&target.base, token.lexeme, slot_type, heap_copy),
            .This => {
                const this_slot = self.this_slot orelse return ErrorList.InvalidAliasArgument;
                try self.instructions.append(.{ .StoreAlias = .{
                    .var_name = "this",
                    .slot_index = self.alias_params.get(this_slot).?,
                    .expected_type = slot_type,
                    .heap_copy = heap_copy,
                } });
            },
            else => unreachable, // a store back targets a name or `this`
        }
    }

    fn allocAliasSlot(self: *HIRGenerator) u32 {
        defer self.next_alias_slot += 1;
        return self.next_alias_slot;
    }

    /// A slot for a value only the generator introduces (a receiver or alias
    /// box temporary, a method's `this`).
    pub fn tempSlot(self: *HIRGenerator) Slot {
        defer self.next_temp_slot += 1;
        return self.next_temp_slot;
    }

    /// The storage of a parameter's binding, which analysis recorded on it.
    pub fn paramSlot(self: *HIRGenerator, param: ast.FunctionParam) ErrorList!Slot {
        if (param.storage) |storage| return storage;
        self.reporter.reportInternal(
            "no analyzed binding for parameter '{s}' at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ param.name.lexeme, param.name.file, param.name.line, param.name.column },
            @src(),
        );
        return ErrorList.MissingBindingType;
    }

    fn storeTarget(self: *HIRGenerator, node: *const ast.Base) ErrorList!StoreTarget {
        if (self.semantic.getStoreTarget(node.id)) |target| return target;
        const location = node.location();
        self.reporter.reportInternal(
            "no analyzed binding for the store at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingBindingType;
    }

    /// Bring the value on top of the stack, of type `value_type`, to
    /// `target_type`: the type of the storage it is stored into, or of the
    /// expression whose value it becomes. A member becoming a union or group is
    /// boxed as that type, a box of another type is re-packed, and a box whose
    /// member a check has proved is read as that member. A number the analyzer
    /// admitted into another numeric type (an `int` literal for a `byte` or a
    /// `float`) is converted to it. Every store goes through here, so a store
    /// always receives a value of its slot's type (`verifyStore` checks it).
    pub fn convertValue(self: *HIRGenerator, value_type: HIRType, target_type: HIRType) !void {
        if (value_type.eql(target_type)) return;
        if (target_type.isBoxed()) {
            try self.instructions.append(.{ .Box = .{ .boxed_type = target_type } });
        } else if (value_type.isBoxed()) {
            try self.instructions.append(.{ .Unbox = .{ .member_type = target_type } });
        } else if (isNumber(value_type) and isNumber(target_type)) {
            try self.instructions.append(.{ .Convert = .{ .from_type = value_type, .to_type = target_type } });
        }
    }

    fn isNumber(t: HIRType) bool {
        return t == .Int or t == .Byte or t == .Float;
    }

    /// Convert the value on top of the stack — `target` after an in-place
    /// change — to the type of the storage `target` names, and return that
    /// type: a name's slot beneath any narrowing, or `target`'s own type for
    /// the receiver `this`.
    pub fn convertForStoreBack(self: *HIRGenerator, target: *ast.Expr) !HIRType {
        const read_type = try self.typeOf(target);
        if (target.data != .Variable) return read_type;
        const slot_type = try self.bindingTypeOf(&target.base);
        try self.convertValue(read_type, slot_type);
        return slot_type;
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

    /// What a resolved callee returns: its registered signature's type.
    pub fn calleeReturnType(self: *HIRGenerator, callee: ModuleCall.Callee) ErrorList!HIRType {
        return switch (callee.kind) {
            .DoxaFunction => self.functionInfoByLink(callee.link_name).?.return_type,
            .ZigFunction => try self.lowerType(&self.zig_functions.get(callee.link_name).?.return_type),
            .BuiltinFunction => unreachable, // a builtin is no resolved callee
        };
    }

    pub fn buildPeekPath(self: *HIRGenerator, expr: *const ast.Expr) !?[]const u8 {
        switch (expr.data) {
            .Variable => |var_token| {
                // A global carries its link name; a peek shows what was written.
                return try self.allocator.dupe(u8, module_graph.displayName(var_token.lexeme));
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

    pub fn isModuleContext(self: *HIRGenerator) bool {
        // We're in module context when we're in the global init phase and not in a function
        return self.is_global_init_phase and self.current_function == null;
    }
};

/// The internal key name of a method: `Struct.method`. `.` cannot occur in an
/// identifier, so a method key never meets a top-level function's.
fn methodKeyName(allocator: std.mem.Allocator, struct_name: []const u8, method_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}.{s}", .{ struct_name, method_name });
}
