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

const ParamMutation = @import("param_mutation.zig");
const ResourceManager = @import("resource_manager.zig");
const LabelGenerator = ResourceManager.LabelGenerator;
const ConstantManager = ResourceManager.ConstantManager;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const TypeSystem = @import("type_system.zig").TypeSystem;
const SlotManager = @import("slot_manager.zig").SlotManager;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const semantic_module = @import("../../analysis/semantic/semantic.zig");
const SemanticAnalyzer = semantic_module.SemanticAnalyzer;
const StructMethodInfo = semantic_module.StructMethodInfo;
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
const union_handling = @import("../../analysis/semantic/union_handling.zig");
const module_graph = @import("../../module/graph.zig");
const TypeRef = ast.TypeRef;

/// Whether `return` with no value appears anywhere in the statement list (including inside expr trees).
fn functionStmtsHaveBareReturn(stmts: []ast.Stmt) bool {
    for (stmts) |stmt| {
        if (stmtHasBareReturn(stmt)) return true;
    }
    return false;
}

fn stmtHasBareReturn(stmt: ast.Stmt) bool {
    switch (stmt.data) {
        .Return => |r| return r.value == null,
        .Expression => |opt| {
            if (opt) |e| return exprHasBareReturn(e);
            return false;
        },
        .FunctionDecl => |f| return functionStmtsHaveBareReturn(f.body),
        .VarDecl => |v| {
            if (v.initializer) |init| return exprHasBareReturn(init);
            return false;
        },
        else => return false,
    }
}

fn exprHasBareReturn(expr: *ast.Expr) bool {
    switch (expr.data) {
        .If => |i| {
            if (i.condition) |c| if (exprHasBareReturn(c)) return true;
            if (i.then_branch) |t| if (exprHasBareReturn(t)) return true;
            if (i.else_branch) |e| if (exprHasBareReturn(e)) return true;
            return false;
        },
        .Block => |b| {
            if (functionStmtsHaveBareReturn(b.statements)) return true;
            if (b.value) |v| if (exprHasBareReturn(v)) return true;
            return false;
        },
        .Grouping => |g| return if (g) |inner| exprHasBareReturn(inner) else false,
        .Loop => |l| return exprHasBareReturn(l.body),
        .Match => |m| {
            if (exprHasBareReturn(m.value)) return true;
            for (m.cases) |case| {
                if (exprHasBareReturn(case.body)) return true;
            }
            return false;
        },
        .Binary => |bin| {
            if (bin.left) |l| if (exprHasBareReturn(l)) return true;
            if (bin.right) |r| if (exprHasBareReturn(r)) return true;
            return false;
        },
        .Unary => |u| {
            if (u.right) |r| if (exprHasBareReturn(r)) return true;
            return false;
        },
        .Assignment => |a| {
            if (a.value) |v| if (exprHasBareReturn(v)) return true;
            return false;
        },
        .Array => |items| {
            for (items) |item| {
                if (exprHasBareReturn(item)) return true;
            }
            return false;
        },
        .FunctionCall => |c| {
            if (exprHasBareReturn(c.callee)) return true;
            for (c.arguments) |arg| {
                if (exprHasBareReturn(arg.expr)) return true;
            }
            return false;
        },
        else => return false,
    }
}

/// A declared parameter type as the signature table records it: primitives
/// plus the container and marker types the call lowering passes through
/// directly. A type the frontend could not resolve collapses to `Int`.
fn sanitizeParamType(t: HIRType) HIRType {
    return switch (t) {
        .Int, .Byte, .Float, .String, .Tetra, .Nothing,
        .Struct, .Enum, .Array, .Map, .Function, .Union, .Group,
        => t,
        else => .Int,
    };
}

/// Declared `returns A | B` with a bare `return` in the body is lowered as
/// `nothing | A | B`; a declared group with a bare `return` likewise becomes
/// `nothing | Group`.
fn effectiveReturnTypeForSignature(generator: *HIRGenerator, declared: ast.TypeInfo, body: []ast.Stmt) !ast.TypeInfo {
    const allocator = generator.allocator;
    if (declared.base == .Custom and declared.custom_type != null) {
        if (!functionStmtsHaveBareReturn(body)) return declared;
        const custom = generator.customTypeOf(declared) orelse return declared;
        if (custom.kind != .Group) return declared;
        const nothing_ptr = try allocator.create(ast.TypeInfo);
        nothing_ptr.* = .{ .base = .Nothing, .is_mutable = false };
        const group_type = try allocator.create(ast.TypeInfo);
        group_type.* = .{ .base = .Custom, .custom_type = declared.custom_type, .is_mutable = false };
        const new_types = try allocator.alloc(*ast.TypeInfo, 2);
        new_types[0] = nothing_ptr;
        new_types[1] = group_type;
        const new_ut = try allocator.create(ast.UnionType);
        new_ut.* = .{ .types = new_types, .current_type_index = 0 };
        return ast.TypeInfo{ .base = .Union, .union_type = new_ut, .is_mutable = declared.is_mutable };
    }
    if (declared.base != .Union or declared.union_type == null) return declared;
    if (!functionStmtsHaveBareReturn(body)) return declared;

    const flattened = try union_handling.flattenUnionType(allocator, declared.union_type.?);
    for (flattened.types) |m| {
        if (m.base == .Nothing) {
            return ast.TypeInfo{ .base = .Union, .union_type = flattened, .is_mutable = declared.is_mutable };
        }
    }

    const nothing_ptr = try allocator.create(ast.TypeInfo);
    nothing_ptr.* = .{ .base = .Nothing, .is_mutable = false };
    const new_types = try allocator.alloc(*ast.TypeInfo, flattened.types.len + 1);
    new_types[0] = nothing_ptr;
    @memcpy(new_types[1..], flattened.types);
    const new_ut = try allocator.create(ast.UnionType);
    new_ut.* = .{ .types = new_types, .current_type_index = flattened.current_type_index };
    return ast.TypeInfo{ .base = .Union, .union_type = new_ut, .is_mutable = declared.is_mutable };
}

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
    array_storage_override: ?SoxaTypes.ArrayStorageKind = null,
    // Declared element type for the array literal currently being lowered, when the
    // enclosing declaration has an explicit array annotation (e.g. byte[4]). Drives
    // comptime coercion + bounds-checking of literal elements. Null otherwise.
    array_element_type_override: ?SoxaTypes.HIRType = null,
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

    symbol_table: SymbolTable,
    constant_manager: ConstantManager,
    label_generator: LabelGenerator,
    type_system: TypeSystem,
    struct_methods: std.StringHashMap(std.StringHashMap(StructMethodInfo)),

    slot_manager: SlotManager,

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

    function_calls: std.array_list.Managed(FunctionCallSite),

    /// The compilation's module graph (mangling final) and the entry record.
    /// Codegen identity for module-owned symbols is keyed by the *defining*
    /// record, never by a source-written alias.
    graph: *module_graph.ModuleGraph,
    entry_module: module_graph.ModuleId,
    /// The record whose body is currently being lowered.
    current_module: module_graph.ModuleId,

    // When an `as` cast is a declaration initializer, the declared variable's
    // slot is pre-created so the cast can store the subject value into it before
    // running the then/else branches (making the binding readable inside them).
    cast_decl_var_index: ?u32 = null,
    cast_decl_var_name: ?[]const u8 = null,

    loop_context_stack: std.array_list.Managed(LoopContext),
    current_function_scope_id: ?u32 = null,

    deferred_stack: std.array_list.Managed(DeferredBlock),
    loop_deferred_boundaries: std.array_list.Managed(usize),

    is_generating_nested_array: bool = false,

    next_scope_id: u32 = 0,

    // Serial number for unnamed temporary variables materialized during codegen
    // (e.g. a struct-method receiver that is not a plain variable).
    next_temp_id: u32 = 0,

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

    pub const FunctionCallSite = struct {
        function_name: []const u8,
        is_tail_position: bool,
        instruction_index: u32,
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
    pub fn init(io: std.Io, allocator: std.mem.Allocator, reporter: *Reporter, semantic: *const SemanticAnalyzer) !HIRGenerator {
        var self = HIRGenerator{
            .io = io,
            .allocator = allocator,
            .instructions = std.array_list.Managed(HIRInstruction).init(allocator),
            .current_peek_expr = null,
            .string_pool = std.array_list.Managed([]const u8).init(allocator),
            .reporter = reporter,
            .array_storage_override = null,
            .array_element_type_override = null,
            .symbol_table = SymbolTable.init(allocator),
            .constant_manager = ConstantManager.init(allocator),
            .label_generator = LabelGenerator.init(allocator),
            .type_system = TypeSystem.init(allocator, reporter, semantic),
            .struct_methods = std.StringHashMap(std.StringHashMap(StructMethodInfo)).init(allocator),
            .slot_manager = SlotManager.init(allocator),
            .function_signatures = SoxaTypes.FunctionSignatureMap.init(allocator),
            .function_bodies = std.array_list.Managed(FunctionBody).init(allocator),
            .function_indices = std.StringHashMap(u32).init(allocator),
            .zig_functions = std.StringHashMap(*const ast.ZigFnSig).init(allocator),
            .semantic = semantic,
            .current_function = null,
            .current_function_return_type = .Nothing,
            .is_global_init_phase = false,
            .function_calls = std.array_list.Managed(FunctionCallSite).init(allocator),
            .reflected_structs = std.StringHashMap(void).init(allocator),
            .graph = semantic.graph,
            .entry_module = semantic.entry_module,
            .current_module = semantic.entry_module,
            .stats = HIRStats.init(allocator),
            .loop_context_stack = std.array_list.Managed(LoopContext).init(allocator),
            .deferred_stack = std.array_list.Managed(DeferredBlock).init(allocator),
            .loop_deferred_boundaries = std.array_list.Managed(usize).init(allocator),
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
                .field_type = self.convertTypeInfo(field.field_type_info.*),
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

    /// The codegen description of the named type `type_info` refers to.
    pub fn customTypeOf(self: *HIRGenerator, type_info: ast.TypeInfo) ?TypeSystem.CustomTypeInfo {
        const key = self.typeKeyOf(type_info) orelse return null;
        return self.type_system.custom_types.get(key);
    }

    /// What analysis resolved `expr` to, if it names a module-level entity.
    pub fn resolutionOf(self: *const HIRGenerator, expr: *const ast.Expr) ?Resolution {
        return self.semantic.resolutionOf(expr);
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
        self.slot_manager.deinit();
        self.function_signatures.deinit();
        self.function_bodies.deinit();
        self.function_indices.deinit();
        self.zig_functions.deinit();

        self.function_calls.deinit();
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
            for (sig.param_types, param_types) |param, *dest| dest.* = self.convertTypeInfo(param);
            functions[i] = .{ .link_name = entry.key_ptr.*, .param_types = param_types, .return_type = self.convertTypeInfo(sig.return_type) };
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

        const eff_rti = try effectiveReturnTypeForSignature(self, func.return_type_info, func.body);
        var return_type = self.convertTypeInfo(eff_rti);
        // A function without an explicit `returns` takes the analyzer's
        // inferred return type.
        if (func.return_type_info.base == .Nothing) {
            if (self.semantic.function_return_types.get(stmt.base.id)) |inferred| {
                return_type = self.convertTypeInfo(inferred.*);
            }
        }

        const param_is_alias = try self.allocator.alloc(bool, func.params.len);
        const param_is_readonly = try self.allocator.alloc(bool, func.params.len);
        const param_types = try self.allocator.alloc(HIRType, func.params.len);
        for (func.params, 0..) |param, i| {
            param_is_alias[i] = param.is_alias;
            param_is_readonly[i] = !ParamMutation.bodyMutatesVariable(func.body, param.name.lexeme);
            param_types[i] = sanitizeParamType(if (param.type_expr != null) self.convertTypeInfo(signature.params[i]) else .Int);
        }

        const function_info = FunctionInfo{
            .name = link_name,
            .arity = @intCast(func.params.len),
            .return_type = return_type,
            .start_label = try self.label_generator.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{link_name})),
            .local_var_count = 0,
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
            .return_type_info = eff_rti,
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
        // Global/module array tracking is the only array tracking that should
        // span functions; capture it once so each function body starts from a
        // clean per-function view (see `captureGlobalArrayTracking`).
        try self.symbol_table.captureGlobalArrayTracking();

        for (self.function_bodies.items) |*function_body| {
            self.current_function = function_body.function_info.name;
            self.current_module = function_body.module_id;
            self.current_function_return_type = function_body.function_info.return_type;
            self.is_global_init_phase = false;
            try self.symbol_table.enterFunctionScope(function_body.function_info.name);

            function_body.start_instruction_index = @intCast(self.instructions.items.len);
            try self.instructions.append(.{ .Label = .{ .name = function_body.function_info.start_label } });

            const function_scope_id = self.nextScopeId();
            self.current_function_scope_id = function_scope_id;
            try self.instructions.append(.{ .EnterScope = .{ .scope_id = function_scope_id, .var_count = 0 } });

            const params = function_body.function_params;

            const is_method = function_body.function_info.arity > params.len;

            var param_index = params.len;
            while (param_index > 0) {
                param_index -= 1;
                const param = params[param_index];

                const resolved = try self.resolveParameterType(param, function_body);
                const param_type = resolved.param_type;
                const declared_type_info = resolved.type_info;
                defer if (declared_type_info) |info| self.allocator.destroy(info);

                const alias_lookup = if (is_method) param_index + 1 else param_index;

                if (function_body.function_info.param_is_alias[alias_lookup]) {
                    switch (param_type) {
                        .Map, .Function => return error.InvalidAliasType,
                        else => {},
                    }
                    // An alias of a struct or an enum lends a named type; its
                    // type and custom-type key were tracked with the parameter.
                    if (declared_type_info) |info| {
                        if ((info.base == .Struct or info.base == .Enum) and info.custom_type == null) return error.InvalidAliasType;
                    }

                    try self.symbol_table.trackAliasParameter(param.name.lexeme);
                    const alias_slot = try self.slot_manager.allocateAliasSlot(param.name.lexeme, param_type);
                    try self.instructions.append(.{
                        .BindAlias = .{
                            .alias_name = param.name.lexeme,
                            .target_variable_name = param.name.lexeme,
                            .alias_slot = alias_slot,
                            .target_type = param_type,
                        },
                    });
                } else {
                    const var_idx = try self.symbol_table.createVariable(param.name.lexeme);
                    // A by-value heap parameter is deep-copied on entry so the
                    // callee cannot write through to the caller's object. When the
                    // body never writes through it, the copy is unobservable and
                    // only costs an O(n) clone plus an arena allocation per call.
                    // Whether the body mutates the parameter is decided once when
                    // the signature metadata is built (`param_is_readonly`), not
                    // re-derived per binding here.
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = param.name.lexeme,
                        .scope_kind = self.symbol_table.determineVariableScope(param.name.lexeme),
                        .module_context = null,
                        .expected_type = param_type,
                        .heap_copy = if (function_body.function_info.param_is_readonly[alias_lookup]) .keep else .snapshot,
                    } });
                }
            }

            // An instance method binds its receiver as the alias `this`.
            if (function_body.function_info.receiver) |struct_id| {
                const receiver_type = HIRType{ .Struct = struct_id };
                try self.trackVariableType("this", receiver_type);
                try self.symbol_table.trackAliasParameter("this");
                if (self.semantic.struct_table.keyOf(struct_id)) |key| try self.trackVariableCustomType("this", key);
                const alias_slot = try self.slot_manager.allocateAliasSlot("this", receiver_type);
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

            if (self.function_signatures.getPtr(function_body.key)) |func_info| {
                func_info.local_var_count = self.symbol_table.local_variable_count;
            }

            self.current_function = null;
            self.current_module = self.entry_module;
            self.current_function_return_type = .Nothing;
            self.current_function_scope_id = null;
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
                .local_var_count = function_info.local_var_count,
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

    pub fn convertTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) HIRType {
        return self.type_system.convertTypeInfo(type_info);
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
            .Variable => |var_token| try basic_handler.generateVariable(var_token),
            .Grouping => |grouping| try basic_handler.generateGrouping(grouping, preserve_result),
            .EnumMember => try basic_handler.generateEnumMember(expr),
            .DefaultArgPlaceholder => try basic_handler.generateDefaultArgPlaceholder(),

            .Binary => |bin| try binary_handler.generateBinary(bin, should_pop_after_use),
            .Logical => |log| try binary_handler.generateLogical(log, should_pop_after_use),
            .Unary => |unary| try binary_handler.generateUnary(unary),

            .If => |if_expr| try control_flow_handler.generateIf(if_expr, preserve_result, should_pop_after_use),
            .Match => |match_expr| try control_flow_handler.generateMatch(match_expr, preserve_result),
            .Loop => |loop| try control_flow_handler.generateLoop(loop, preserve_result),
            .Block => try control_flow_handler.generateBlock(expr.data, preserve_result),
            .ReturnExpr => try control_flow_handler.generateReturn(expr.data),
            .Unreachable => try control_flow_handler.generateUnreachable(expr),
            .Cast => try control_flow_handler.generateCast(expr.data, preserve_result),

            .Array => |elements| {
                if (self.is_generating_nested_array) {
                    try collections_handler.generateArrayInternal(elements, preserve_result);
                } else {
                    try collections_handler.generateArray(elements, preserve_result);
                }
            },
            .Map => |map_expr| try collections_handler.generateMap(map_expr.entries, null),
            .MapLiteral => |map_literal| try collections_handler.generateMap(map_literal.entries, map_literal.else_value),
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

            .Assignment => |assign| try assignments_handler.generateAssignment(assign, preserve_result),
            .CompoundAssign => |compound| try assignments_handler.generateCompoundAssign(compound, preserve_result),

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

    pub fn getOrCreateVariable(self: *HIRGenerator, name: []const u8) !u32 {
        return self.symbol_table.getOrCreateVariable(name);
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
            const eff_rti = try effectiveReturnTypeForSignature(self, info.signature.return_type.*, method.body);

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
                param_types[param_idx] = sanitizeParamType(if (param.type_expr != null) self.convertTypeInfo(info.signature.params[i]) else .Int);
                param_idx += 1;
            }

            try self.addFunction(.{
                .function_info = .{
                    .name = link_name,
                    .receiver = if (method.is_static) null else struct_id,
                    .arity = arity,
                    .return_type = self.convertTypeInfo(eff_rti),
                    .start_label = try self.generateLabel(try std.fmt.allocPrint(self.allocator, "func_{s}", .{link_name})),
                    .local_var_count = 0,
                    .is_entry = false,
                    .param_is_alias = param_is_alias,
                    .param_is_readonly = param_is_readonly,
                    .param_types = param_types,
                },
                .statements = method.body,
                .start_instruction_index = 0,
                .function_name = link_name,
                .function_params = method.params,
                .return_type_info = eff_rti,
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

            // An alias (`^`) parameter is not backed by a local slot; its storage
            // pointer lives in the alias-slot table. Pushing its variable index
            // would miss both slot maps and emit a null receiver, so dispatch on
            // the alias slot exactly as `this` does for instance methods.
            if (self.symbol_table.isAliasParameter(var_token.lexeme)) {
                const alias_slot = self.slot_manager.getAliasSlot(var_token.lexeme) orelse
                    return error.InvalidAliasArgument;
                try self.instructions.append(.{
                    .PushStorageId = .{
                        .var_index = alias_slot,
                        .var_name = var_token.lexeme,
                        .scope_kind = .Local,
                    },
                });
                return;
            }

            const var_idx = try self.getOrCreateVariable(var_token.lexeme);
            const scope_kind = self.symbol_table.determineVariableScope(var_token.lexeme);
            try self.instructions.append(.{
                .PushStorageId = .{
                    .var_index = var_idx,
                    .var_name = var_token.lexeme,
                    .scope_kind = scope_kind,
                },
            });
            return;
        }

        const temp_id = self.next_temp_id;
        self.next_temp_id += 1;
        const temp_name = try std.fmt.allocPrint(self.allocator, "__recv_{d}", .{temp_id});
        const temp_idx = try self.getOrCreateVariable(temp_name);
        try self.generateExpression(receiver, true, false);
        try self.instructions.append(.{
            .StoreVar = .{
                .var_index = temp_idx,
                .var_name = temp_name,
                .scope_kind = .Local,
                .module_context = null,
                .expected_type = HIRType{ .Struct = 0 },
            },
        });
        try self.instructions.append(.{
            .PushStorageId = .{
                .var_index = temp_idx,
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

    pub fn inferTypeFromLiteral(self: *HIRGenerator, literal: TokenLiteral) HIRType {
        return self.type_system.inferTypeFromLiteral(literal);
    }

    pub fn resolveFieldAccessType(self: *HIRGenerator, e: *ast.Expr) ?TypeSystem.FieldResolveResult {
        return self.type_system.resolveFieldAccessType(e, &self.symbol_table);
    }

    pub fn inferTypeFromExpression(self: *HIRGenerator, expr: *ast.Expr) HIRType {
        return self.type_system.inferTypeFromExpression(expr, &self.symbol_table);
    }

    pub fn trackVariableType(self: *HIRGenerator, var_name: []const u8, var_type: HIRType) !void {
        try self.symbol_table.trackVariableType(var_name, var_type);
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

    pub fn trackArrayElementType(self: *HIRGenerator, var_name: []const u8, elem_type: HIRType) !void {
        try self.symbol_table.trackArrayElementType(var_name, elem_type);
    }

    pub fn getTrackedArrayElementType(self: *HIRGenerator, var_name: []const u8) ?HIRType {
        return self.symbol_table.getTrackedArrayElementType(var_name);
    }

    pub fn trackArrayStorageKind(self: *HIRGenerator, var_name: []const u8, storage: SoxaTypes.ArrayStorageKind) !void {
        try self.symbol_table.trackArrayStorageKind(var_name, storage);
    }

    pub fn getTrackedArrayStorageKind(self: *HIRGenerator, var_name: []const u8) ?SoxaTypes.ArrayStorageKind {
        return self.symbol_table.getTrackedArrayStorageKind(var_name);
    }

    fn convertArrayStorageKind(kind: ast.ArrayStorageKind) SoxaTypes.ArrayStorageKind {
        return switch (kind) {
            .dynamic => .dynamic,
            .fixed => .fixed,
            .const_literal => .const_literal,
        };
    }

    fn resolveParameterType(
        self: *HIRGenerator,
        param: ast.FunctionParam,
        function_body: *const FunctionBody,
    ) !struct { param_type: HIRType, type_info: ?*ast.TypeInfo } {
        var declared_type_info: ?*ast.TypeInfo = null;
        var param_type: HIRType = .Unknown;
        if (param.type_expr) |type_expr| {
            const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
            declared_type_info = type_info_ptr;
            param_type = self.convertTypeInfo(type_info_ptr.*);
        } else {
            param_type = self.inferParameterType(param.name.lexeme, function_body.statements, function_body.function_name) catch .Int;
        }

        try self.trackVariableType(param.name.lexeme, param_type);
        if (declared_type_info) |info| {
            if (self.typeKeyOf(info.*)) |key| try self.trackVariableCustomType(param.name.lexeme, key);
        }

        if (param_type == .Array) {
            try self.trackArrayElementType(param.name.lexeme, param_type.Array.*);
            const storage_kind = if (declared_type_info) |info| convertArrayStorageKind(info.array_storage) else SoxaTypes.ArrayStorageKind.dynamic;
            try self.trackArrayStorageKind(param.name.lexeme, storage_kind);
        }

        return .{ .param_type = param_type, .type_info = declared_type_info };
    }

    pub fn storageKindFromTypeInfo(self: *HIRGenerator, type_info: ast.TypeInfo) SoxaTypes.ArrayStorageKind {
        _ = self;
        return convertArrayStorageKind(type_info.array_storage);
    }

    fn inferBinaryOpResultType(self: *HIRGenerator, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr) HIRType {
        return self.type_system.inferBinaryOpResultType(operator_type, left_expr, right_expr, &self.symbol_table);
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
        if (self.semantic.getCachedExprType(expr)) |type_info| {
            return self.type_system.convertTypeInfo(type_info.*);
        }
        const location = expr.base.location();
        self.reporter.reportInternal(
            "no analyzed type for the {s} expression at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ @tagName(std.meta.activeTag(expr.data)), location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingExpressionType;
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

    /// What a resolved callee returns: its registered signature's type.
    pub fn calleeReturnType(self: *HIRGenerator, callee: ModuleCall.Callee) HIRType {
        return switch (callee.kind) {
            .DoxaFunction => self.functionInfoByLink(callee.link_name).?.return_type,
            .ZigFunction => self.convertTypeInfo(self.zig_functions.get(callee.link_name).?.return_type),
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

    pub fn computeNumericCommonType(self: *HIRGenerator, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        return self.type_system.computeNumericCommonType(left_type, right_type, operator_type);
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
