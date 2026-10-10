const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Memory = @import("../../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;

const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const StructTable = @import("../../common/struct_table.zig").StructTable;
const EnumTable = @import("../../common/enum_table.zig").EnumTable;
const GroupTable = @import("../../common/group_table.zig").GroupTable;
const UnionTable = @import("../../common/union_table.zig").UnionTable;
const TypeLowering = @import("../../common/type_lowering.zig").TypeLowering;
const StructId = @import("../../codegen/hir/types.zig").StructId;

const Types = @import("../../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;
const TokenLiteral = Types.TokenLiteral;
const StructField = Types.StructField;

const HIRType = @import("../../codegen/hir/types.zig").HIRType;

const TokenImport = @import("../../types/token.zig");
const TokenType = TokenImport.TokenType;
const Token = TokenImport.Token;


const helpers = @import("./helpers.zig");
const names = @import("./names.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("eval_utils.zig");
const consteval = @import("../consteval.zig");
const union_handling = @import("union_handling.zig");
const infer_type = @import("./infer_type.zig");

const graph_mod = @import("../../module/graph.zig");
const ModuleGraph = graph_mod.ModuleGraph;
const ModuleRecord = graph_mod.ModuleRecord;
const ModuleId = graph_mod.ModuleId;
const ModuleLoader = @import("../../module/loader.zig").ModuleLoader;
const resolution = @import("resolution.zig");
const Resolution = resolution.Resolution;

/// A struct method's analyzer-level description. A private method is
/// module-private, like a private field.
pub const StructMethodInfo = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
    /// The method's full signature. Carrying the parameter list (not just the
    /// return type) is what lets a method call run the same argument
    /// validation a plain function call gets; without it a wrong arity or a
    /// wrong argument type on `x.method(...)` reaches codegen unchecked and
    /// mis-compiles instead of being diagnosed.
    signature: *ast.FunctionType,
};

const UsedBindings = std.HashMap(graph_mod.SymbolKey, void, graph_mod.SymbolKeyContext, std.hash_map.default_max_load_percentage);

//======================================================================

const NodeId = u32;

/// Semantic analysis of a whole program, one module record at a time.
///
/// Each record is analyzed in its own module scope (no parent: a record sees
/// its own declarations and its bindings, never another file's locals) and
/// moves through two stages the module graph drives:
///
/// - **Types** (`ensureTypes`): the record's declarations enter its module
///   scope — types registered by `TypeRef`, functions with their resolved
///   signatures, globals with their types. Another record's Types stage may
///   run in the middle of this one; a re-entry (a declaration cycle) reads the
///   placeholders registered so far.
/// - **Analysis** (`ensureAnalyzed`): the record's bodies are checked. Body
///   analysis may register another record's types but never analyzes another
///   record's bodies, so body-level cycles terminate by construction.
///
/// `analyzeProgram` analyzes the entry record and then every record a
/// reference materialized, until no new record joins the program.
/// A store through a name. `type_cache` holds what an expression reads; a
/// store also needs what it writes, which differs inside a narrowing view.
pub const StoreTarget = struct {
    /// The storage's type, beneath every narrowing view: what a store writes.
    slot: *ast.TypeInfo,
    /// What a read of the name sees at the store: the active view's type, or
    /// the slot's.
    read: *ast.TypeInfo,
    /// The storage's identity: the storage id of the binding beneath every
    /// view. Codegen keys a variable's slot by it, so two bindings that share
    /// a name never share storage.
    storage: u32,
    /// The storage is module-level: a global, not a local of the enclosing
    /// function.
    global: bool,
};

/// One parameter of a function or method signature the analyzer registered.
pub const ParamRef = struct {
    signature: *const ast.FunctionType,
    index: u32,
};

/// A member-typed variable lent to a union `^` parameter. Sound only if the
/// callee never stores another member into it, which is known once every
/// body is analyzed (`settleMemberLoans`).
pub const MemberLoan = struct {
    param: ParamRef,
    location: Location,
};

pub const SemanticAnalyzer = struct {
    in_loop_scope: bool = false,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),
    /// What a store through a name writes and what the name reads there, per
    /// `Variable` expression, assignment and variable declaration.
    store_targets: std.AutoHashMap(NodeId, StoreTarget),
    /// Member-preserving `^` parameters. A union `^` parameter preserves its
    /// member when every store into it goes through a narrowing of it (an
    /// `as` or a match arm), so it stores the member the narrowing proved.
    /// Such a parameter may be lent storage of any one member type, since the
    /// callee leaves that member in place. `member_mutators` holds the union
    /// `^` parameters a body was found to store another member into, directly
    /// or by lending them on to one that does (`member_relays`).
    member_mutators: std.AutoHashMap(ParamRef, void),
    member_relays: std.ArrayListUnmanaged(struct { from: ParamRef, to: ParamRef }) = .empty,
    member_loans: std.ArrayListUnmanaged(MemberLoan) = .empty,
    /// The signatures whose bodies were analyzed, so whose `member_mutators`
    /// verdicts are known. A loan through any other signature — a copy held
    /// by a function value — cannot be checked.
    checked_signatures: std.AutoHashMap(*const ast.FunctionType, void),
    /// The union `^` parameters of the body being analyzed, by storage.
    union_alias_params: std.AutoHashMap(u32, ParamRef),
    custom_types: graph_mod.TypeRefHashMap(CustomTypeInfo),
    struct_methods: graph_mod.TypeRefHashMap(std.StringHashMap(StructMethodInfo)),

    loader: *ModuleLoader,
    graph: *ModuleGraph,
    entry_module: ModuleId,
    /// The entry function's name, which is never reported unused.
    entry_point_name: ?[]const u8,
    /// The record whose declarations or bodies are being analyzed. Name
    /// resolution reads its bindings, and module-private access is decided
    /// against it.
    current_module: ModuleId,
    module_scopes: std.AutoHashMap(ModuleId, *Scope),
    /// What every name-bearing expression outside the local scope denotes.
    resolutions: resolution.ResolutionMap,
    /// Every declaration of and reference to a module-level global.
    global_sites: resolution.GlobalSites,
    /// Bindings referenced at least once, per record; the rest of the entry
    /// record's imports are reported unused.
    used_bindings: UsedBindings,
    /// Set while a record's bodies are analyzed: analysis never recurses into
    /// another record's bodies.
    analyzing: bool = false,

    function_return_types: std.AutoHashMap(NodeId, *ast.TypeInfo),
    /// Every `.Variant` shorthand inferred since the last analysis stage
    /// ended. A shorthand names no enum itself; its context types it
    /// (`helpers.contextualizeEnumMember`), and one left untyped is an error.
    bare_variants: std.ArrayListUnmanaged(*const ast.Expr) = .empty,
    /// The function whose body is being validated. Every `return` in it is
    /// checked against its declared return type where the return is analyzed.
    returning: ?Returning = null,
    current_initializing_var: ?[]const u8 = null,
    block_value_expected: bool = false,
    /// The struct whose method body is being checked (`this`).
    /// The struct whose method or function body is being checked: its private
    /// members are reachable here, through `this`.
    enclosing: ?Enclosing = null,
    struct_table: StructTable,
    enum_table: EnumTable,
    group_table: GroupTable,
    /// Every union type of the compilation; codegen extends it.
    union_table: UnionTable,

    pub fn init(
        allocator: std.mem.Allocator,
        reporter: *Reporter,
        memory: *MemoryManager,
        loader: *ModuleLoader,
        entry_module: ModuleId,
        entry_point_name: ?[]const u8,
    ) SemanticAnalyzer {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .fatal_error = false,
            .current_scope = null,
            .type_cache = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
            .store_targets = std.AutoHashMap(NodeId, StoreTarget).init(allocator),
            .member_mutators = std.AutoHashMap(ParamRef, void).init(allocator),
            .union_alias_params = std.AutoHashMap(u32, ParamRef).init(allocator),
            .checked_signatures = std.AutoHashMap(*const ast.FunctionType, void).init(allocator),
            .custom_types = graph_mod.TypeRefHashMap(CustomTypeInfo).init(allocator),
            .struct_methods = graph_mod.TypeRefHashMap(std.StringHashMap(StructMethodInfo)).init(allocator),
            .loader = loader,
            .graph = loader.graph,
            .entry_module = entry_module,
            .entry_point_name = entry_point_name,
            .current_module = entry_module,
            .module_scopes = std.AutoHashMap(ModuleId, *Scope).init(allocator),
            .resolutions = resolution.ResolutionMap.init(allocator),
            .global_sites = .empty,
            .used_bindings = UsedBindings.init(allocator),
            .function_return_types = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
            .struct_table = StructTable.init(allocator),
            .enum_table = EnumTable.init(allocator),
            .group_table = GroupTable.init(allocator),
            .union_table = UnionTable.init(allocator),
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        self.type_cache.deinit();
        self.store_targets.deinit();
        self.member_mutators.deinit();
        self.member_relays.deinit(self.allocator);
        self.member_loans.deinit(self.allocator);
        self.union_alias_params.deinit();
        self.checked_signatures.deinit();
        self.bare_variants.deinit(self.allocator);
        self.custom_types.deinit();
        var methods_it = self.struct_methods.valueIterator();
        while (methods_it.next()) |tbl| tbl.*.deinit();
        self.struct_methods.deinit();
        self.module_scopes.deinit();
        self.resolutions.deinit();
        self.global_sites.deinit(self.allocator);
        self.used_bindings.deinit();
        self.function_return_types.deinit();
        self.struct_table.deinit();
        self.enum_table.deinit();
        self.group_table.deinit();
        self.union_table.deinit();
    }

    pub fn getStructTable(self: *const SemanticAnalyzer) *const StructTable {
        return &self.struct_table;
    }

    pub fn getEnumTable(self: *const SemanticAnalyzer) *const EnumTable {
        return &self.enum_table;
    }

    pub fn getGroupTable(self: *const SemanticAnalyzer) *const GroupTable {
        return &self.group_table;
    }

    /// The tables a union orders its named members by.
    pub fn unionNames(self: *const SemanticAnalyzer) UnionTable.Names {
        return .{ .structs = &self.struct_table, .enums = &self.enum_table, .groups = &self.group_table };
    }

    /// Lowering for analysis, which assigns a named type its id on first
    /// sight: a type may be named before its module registers it.
    pub fn typeLowering(self: *SemanticAnalyzer) TypeLowering {
        return .{
            .allocator = self.allocator,
            .graph = self.graph,
            .names = self.unionNames(),
            .unions = &self.union_table,
            .assign = .{ .structs = &self.struct_table, .enums = &self.enum_table, .groups = &self.group_table },
        };
    }

    /// The type the analyzer inferred for `expr`, if it has visited it.
    /// Codegen reads `@`-call types from here rather than re-deriving them, so
    /// `inferBuiltinCall` stays the single authority for builtin typing.
    pub fn getCachedExprType(self: *const SemanticAnalyzer, expr: *const ast.Expr) ?*ast.TypeInfo {
        return self.type_cache.get(expr.base.id);
    }

    /// The store target `node` names, if it is a name, an assignment or a
    /// declaration the analyzer resolved.
    pub fn getStoreTarget(self: *const SemanticAnalyzer, node: NodeId) ?StoreTarget {
        return self.store_targets.get(node);
    }

    /// What `expr` was resolved to, if it names a module-level entity.
    pub fn resolutionOf(self: *const SemanticAnalyzer, expr: *const ast.Expr) ?Resolution {
        return self.resolutions.get(expr.base.id);
    }

    /// The registered description of `ref`, registering the defining record's
    /// types first.
    pub fn customType(self: *SemanticAnalyzer, ref: TypeRef) ErrorList!?CustomTypeInfo {
        try self.ensureTypes(ref.module);
        return self.custom_types.get(ref);
    }

    /// The method table of the struct `ref`, registering the defining record's
    /// types first.
    pub fn methodsOf(self: *SemanticAnalyzer, ref: TypeRef) ErrorList!?std.StringHashMap(StructMethodInfo) {
        try self.ensureTypes(ref.module);
        return self.struct_methods.get(ref);
    }

    fn isEnumTypeRequiringInitializer(self: *SemanticAnalyzer, type_info: *const ast.TypeInfo) bool {
        return eval.isEnumTypeRequiringInitializer(type_info, self);
    }

    // ── The per-record driver ───────────────────────────────────────────────

    /// Analyze the program: the entry record, then every record a reference
    /// materialized (its declarations were collected), until none is left. A
    /// record that is only interned — named by a `module` alias nothing reads
    /// through — is not part of the program.
    pub fn analyzeProgram(self: *SemanticAnalyzer) ErrorList!void {
        const entry_scope = try self.ensureModuleScope(self.entry_module);
        self.memory.scope_manager.root_scope = entry_scope;

        try self.ensureAnalyzed(self.entry_module);
        var progressed = true;
        while (progressed) {
            progressed = false;
            var i: usize = 0;
            while (i < self.graph.records.items.len) : (i += 1) {
                const record = self.graph.records.items[i];
                if (!record.status.atLeast(.DeclarationsCollected)) continue;
                if (record.status.atLeast(.Analyzed)) continue;
                try self.ensureAnalyzed(record.id);
                progressed = true;
            }
        }
        try self.settleMemberLoans();
        if (self.fatal_error) return error.SemanticError;

        try self.reportUnused(entry_scope);
        self.reportUnusedImports();
    }

    /// Every body is analyzed, so whether a union `^` parameter preserves its
    /// member is known: a parameter lent on to one that stores another member
    /// stores it too. A member-typed variable lent to a parameter that does
    /// not preserve its member is an error.
    fn settleMemberLoans(self: *SemanticAnalyzer) ErrorList!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.member_relays.items) |relay| {
                if (self.member_mutators.contains(relay.from)) continue;
                if (self.checked_signatures.contains(relay.to.signature) and !self.member_mutators.contains(relay.to)) continue;
                try self.member_mutators.put(relay.from, {});
                changed = true;
            }
        }
        for (self.member_loans.items) |loan| {
            if (!self.checked_signatures.contains(loan.param.signature)) {
                self.reporter.reportCompileError(
                    loan.location,
                    ErrorCode.INVALID_ALIAS_ARGUMENT,
                    "An alias argument lends its storage, so its type must be exactly the parameter's: this call's body is not known, so it may store a different member of its union",
                    .{},
                );
            } else if (self.member_mutators.contains(loan.param)) {
                self.reporter.reportCompileError(
                    loan.location,
                    ErrorCode.INVALID_ALIAS_ARGUMENT,
                    "An alias argument lends its storage, so its type must be exactly the parameter's: this parameter may store a different member of its union",
                    .{},
                );
            } else continue;
            self.fatal_error = true;
        }
    }

    /// The signature analysis registered for the function or method `name`:
    /// a method's from its struct's table, a function's from its binding.
    fn registeredSignature(self: *SemanticAnalyzer, name: []const u8, enclosing: ?Enclosing) ?*const ast.FunctionType {
        if (enclosing) |e| {
            const methods = self.struct_methods.getPtr(e.ref) orelse return null;
            return (methods.get(name) orelse return null).signature;
        }
        const binding = self.moduleScope(self.current_module).lookupLocalVariable(name) orelse return null;
        const storage = self.memory.scope_manager.value_storage.get(binding.storage_id) orelse return null;
        return storage.type_info.function_type;
    }

    /// Fix the program's link identities: every record's mangling tag and
    /// every type's canonical key. Lowering calls this once, before it reads
    /// either; no record may join the graph afterwards. Analysis alone (the
    /// language server) never fixes them, so it may keep loading records.
    pub fn finalizeLinkIdentities(self: *SemanticAnalyzer) !void {
        try self.graph.finalizeMangling();
        try self.struct_table.assignKeys(self.graph);
        try self.enum_table.assignKeys(self.graph);
        try self.group_table.assignKeys(self.graph);
        try helpers.lowerStructFieldTypes(self);
    }

    /// Register `id`'s declarations in its module scope (collecting its
    /// declarations first). A record whose registration is active yields the
    /// placeholders registered so far.
    pub fn ensureTypes(self: *SemanticAnalyzer, id: ModuleId) ErrorList!void {
        const record = try self.loader.ensureDeclarations(self.graph.record(id));
        switch (self.graph.runStage(record, .Types, self.reporter, self, typesStage)) {
            .ready, .in_progress => {},
            .failed => {
                self.fatal_error = true;
                return error.SemanticError;
            },
        }
    }

    /// Check `id`'s bodies. Only the program driver calls this: body analysis
    /// never reaches another record's bodies.
    fn ensureAnalyzed(self: *SemanticAnalyzer, id: ModuleId) ErrorList!void {
        std.debug.assert(!self.analyzing);
        try self.ensureTypes(id);
        switch (self.graph.runStage(self.graph.record(id), .Analysis, self.reporter, self, analysisStage)) {
            .ready => {},
            .in_progress => unreachable,
            .failed => {
                self.fatal_error = true;
                return error.SemanticError;
            },
        }
    }

    fn typesStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
        const self: *SemanticAnalyzer = @ptrCast(@alignCast(ctx));
        const saved = try self.enterModule(record.id);
        defer self.leaveModule(saved);
        return switch (record.kind) {
            .doxa => self.collectDeclarations(record.statements(), self.moduleScope(record.id)),
            .inline_zig, .zig_file => self.registerZigFunctions(record),
        };
    }

    fn analysisStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
        const self: *SemanticAnalyzer = @ptrCast(@alignCast(ctx));
        if (record.kind != .doxa) return;
        const saved = try self.enterModule(record.id);
        defer self.leaveModule(saved);
        self.analyzing = true;
        defer self.analyzing = false;
        const result = self.validateStatements(record.statements());
        self.reportUntypedVariants();
        return result;
    }

    /// A `.Variant` shorthand is typed only by an explicit context: a
    /// parameter, an annotated variable, an assignment, a return, or the other
    /// side of a comparison. Nothing is searched; one its context left untyped
    /// is an error where it is written.
    fn reportUntypedVariants(self: *SemanticAnalyzer) void {
        defer self.bare_variants.clearRetainingCapacity();
        for (self.bare_variants.items) |expr| {
            if (!helpers.isUntypedVariant(self.type_cache.get(expr.base.id).?)) continue;
            const variant = expr.data.EnumMember;
            self.reporter.reportCompileError(
                ast.SourceSpan.fromToken(variant).location,
                ErrorCode.UNKNOWN_TYPE,
                "'.{s}' needs its enum from context; annotate the target or write it through its enum (`Enum.{s}`)",
                .{ variant.lexeme, variant.lexeme },
            );
            self.fatal_error = true;
        }
    }

    /// Record a place that names a global, once per place.
    pub fn recordGlobalSite(self: *SemanticAnalyzer, site: resolution.GlobalSite) ErrorList!void {
        try self.global_sites.put(self.allocator, site.address(), site);
    }

    /// The analyzer state that belongs to one record's analysis. Another
    /// record's Types stage can run in the middle of a body, so entering a
    /// record saves this and leaving restores it.
    pub const Enclosing = struct {
        ref: TypeRef,
        /// A method has a receiver; a struct's function does not.
        has_this: bool,
    };

    /// A function body under validation: its name, and what it declares it
    /// returns (`nothing` without `returns`).
    pub const Returning = struct {
        function: Token,
        type: ast.TypeInfo,
    };

    const Context = struct {
        module: ModuleId,
        scope: ?*Scope,
        enclosing: ?Enclosing,
        returning: ?Returning,
        initializing_var: ?[]const u8,
        block_value_expected: bool,
        in_loop_scope: bool,
        analyzing: bool,
    };

    fn enterModule(self: *SemanticAnalyzer, id: ModuleId) ErrorList!Context {
        const saved = Context{
            .module = self.current_module,
            .scope = self.current_scope,
            .enclosing = self.enclosing,
            .returning = self.returning,
            .initializing_var = self.current_initializing_var,
            .block_value_expected = self.block_value_expected,
            .in_loop_scope = self.in_loop_scope,
            .analyzing = self.analyzing,
        };
        self.current_module = id;
        self.current_scope = try self.ensureModuleScope(id);
        self.enclosing = null;
        self.returning = null;
        self.current_initializing_var = null;
        self.block_value_expected = false;
        self.in_loop_scope = false;
        self.analyzing = false;
        return saved;
    }

    fn leaveModule(self: *SemanticAnalyzer, saved: Context) void {
        self.current_module = saved.module;
        self.current_scope = saved.scope;
        self.enclosing = saved.enclosing;
        self.returning = saved.returning;
        self.current_initializing_var = saved.initializing_var;
        self.block_value_expected = saved.block_value_expected;
        self.in_loop_scope = saved.in_loop_scope;
        self.analyzing = saved.analyzing;
    }

    /// The module scope of `id`: the root of everything declared in that file.
    /// It has no parent — a record never sees another file's names except
    /// through its bindings.
    pub fn moduleScope(self: *const SemanticAnalyzer, id: ModuleId) *Scope {
        return self.module_scopes.get(id).?;
    }

    fn ensureModuleScope(self: *SemanticAnalyzer, id: ModuleId) ErrorList!*Scope {
        if (self.module_scopes.get(id)) |scope| return scope;
        const scope = try self.memory.scope_manager.createScope(null, self.memory);
        try self.module_scopes.put(id, scope);
        return scope;
    }

    /// The Types stage of an inline `zig` block or a `.zig` file: resolve each
    /// signature's Doxa types and bind each function in the record's module
    /// scope, so `Z.fn` and an imported Zig function resolve like any other
    /// module function. A block resolves `DoxaEnum_X` through its owner's
    /// bindings; a `.zig` file has no Doxa bindings to resolve through.
    fn registerZigFunctions(self: *SemanticAnalyzer, record: *ModuleRecord) ErrorList!void {
        const unit = &record.zig.?;
        const scope = self.moduleScope(record.id);
        // Fallible returns resolve to `nothing | <enum>`; the wrapper needs the
        // enum's variants to synthesize the Zig error set the body returns
        // against. Their `path` spelling is captured before resolution replaces
        // `custom_type.written` with the resolved ref.
        var error_sets = std.array_list.Managed(graph_mod.ZigErrorSet).init(self.allocator);
        defer error_sets.deinit();
        for (unit.sigs) |*sig| {
            const error_path: ?[]const u8 = blk: {
                if (sig.return_type.base != .Union) break :blk null;
                for (sig.return_type.union_type.?.types) |member| {
                    if (member.base != .Enum) continue;
                    if (member.custom_type) |custom| switch (custom) {
                        .written => |written| break :blk written,
                        .ref => {},
                    };
                }
                break :blk null;
            };

            for (sig.param_types) |*param| try self.resolveZigType(record, param);
            try self.resolveZigType(record, &sig.return_type);

            if (error_path) |path| {
                const resolved = try self.zigErrorSetVariants(sig.return_type);
                var already = false;
                for (error_sets.items) |seen| {
                    if (std.mem.eql(u8, seen.path, path)) already = true;
                }
                if (!already) try error_sets.append(.{ .path = path, .ref = resolved.ref, .variants = resolved.variants });
            }

            const return_type = try ast.TypeInfo.createDefault(self.allocator);
            return_type.* = sig.return_type;
            const function_type = try self.allocator.create(ast.FunctionType);
            function_type.* = .{ .params = sig.param_types, .return_type = return_type };
            const type_info = try ast.TypeInfo.createDefault(self.allocator);
            type_info.* = .{ .base = .Function, .function_type = function_type, .is_mutable = false };
            const variable = try scope.createValueBinding(sig.name, .FUNCTION, type_info, true);
            variable.used = true;
        }
        if (error_sets.items.len > 0) unit.error_sets = try error_sets.toOwnedSlice();
    }

    /// The resolved identity and variant names of the enum member of a fallible
    /// return, in declaration order — the order a Zig error set's names must
    /// follow for the generated `name -> discriminant` switch to agree with
    /// Doxa.
    fn zigErrorSetVariants(self: *SemanticAnalyzer, return_type: ast.TypeInfo) ErrorList!struct {
        ref: ast.TypeRef,
        variants: []const []const u8,
    } {
        const enum_member = for (return_type.union_type.?.types) |member| {
            if (member.base == .Enum) break member;
        } else return error.UndefinedType;

        const ref = switch (enum_member.custom_type orelse return error.UndefinedType) {
            .ref => |r| r,
            .written => return error.UndefinedType,
        };
        const id = self.enum_table.idOf(ref) orelse return error.UndefinedType;
        const variants = self.enum_table.variants(id) orelse return error.UndefinedType;

        const variant_names = try self.allocator.alloc([]const u8, variants.len);
        for (variants, 0..) |variant, i| variant_names[i] = variant.name;
        return .{ .ref = ref, .variants = variant_names };
    }

    fn resolveZigType(self: *SemanticAnalyzer, record: *ModuleRecord, type_info: *ast.TypeInfo) ErrorList!void {
        const owner = record.owner orelse {
            if (hasWrittenType(type_info)) {
                self.reporter.reportCompileError(
                    record.zig.?.location,
                    ErrorCode.UNKNOWN_TYPE,
                    "Zig module '{s}' names a Doxa type; a `.zig` file has no Doxa bindings, so `DoxaEnum_<Name>` is only available in an inline `zig` block",
                    .{self.graph.moduleName(record.id)},
                );
                self.fatal_error = true;
                return error.UndefinedType;
            }
            return;
        };
        try names.resolveTypeInfo(self, type_info, owner, record.zig.?.location);
    }

    fn hasWrittenType(type_info: *const ast.TypeInfo) bool {
        if (type_info.custom_type) |custom| if (custom == .written) return true;
        if (type_info.array_type) |element| return hasWrittenType(element);
        return false;
    }

    /// A module namespace (`std`, `std.io`) is a compile-time construct, not a
    /// value, so it cannot be bound to a variable. Detect `const x is std.io`
    /// and report a clear error steering the user toward `import`. Returns true
    /// when a namespace binding was detected and reported.
    fn reportNamespaceBinding(self: *SemanticAnalyzer, decl: anytype, base: ast.Base) ErrorList!bool {
        const init_expr = decl.initializer orelse return false;
        const namespace = (try names.namespaceOf(self, init_expr)) orelse return false;

        self.reporter.reportCompileError(
            getLocationFromBase(base),
            ErrorCode.MODULE_NAMESPACE_NOT_A_VALUE,
            "Cannot bind module namespace '{s}' to a variable; use 'import {s} from ...' instead",
            .{ self.graph.moduleName(namespace), decl.name.lexeme },
        );
        self.fatal_error = true;
        return true;
    }

    /// Build the analyzer-level method table for the struct `ref` declared by
    /// `sd`. Every method is registered; a private one is module-private and
    /// rejected at a call site outside its module.
    fn registerStructMethods(self: *SemanticAnalyzer, sd: *ast.StructDecl, ref: TypeRef) ErrorList!void {
        var method_table = std.StringHashMap(StructMethodInfo).init(self.allocator);
        for (sd.methods) |m| {
            try names.resolveTypeInfo(self, &m.return_type_info, self.current_module, ast.SourceSpan.fromToken(sd.name).location);
            const return_type = try ast.TypeInfo.createDefault(self.allocator);
            return_type.* = if (m.return_type_info.base != .Nothing)
                m.return_type_info
            else
                .{ .base = .Nothing, .is_mutable = false };
            try method_table.put(m.name.lexeme, .{
                .name = m.name.lexeme,
                .is_public = m.is_public,
                .is_static = m.is_static,
                .signature = try self.methodSignature(m, return_type),
            });
        }
        try self.struct_methods.put(ref, method_table);
    }

    /// The method's callable signature: its resolved parameter types and alias
    /// flags, joined to its return type. An unannotated parameter resolves to
    /// `Nothing`, which `validateFunctionCallArguments` reads as "accepts
    /// anything" — the same treatment an unannotated function parameter gets.
    fn methodSignature(self: *SemanticAnalyzer, m: *ast.StructMethod, return_type: *ast.TypeInfo) ErrorList!*ast.FunctionType {
        const params = try self.allocator.alloc(ast.TypeInfo, m.params.len);
        const aliases = try self.allocator.alloc(bool, m.params.len);
        for (m.params, 0..) |param, i| {
            aliases[i] = param.is_alias;
            params[i] = if (param.type_expr) |type_expr| (try self.typeExprToTypeInfo(type_expr)).* else ast.TypeInfo{ .base = .Nothing };
        }
        const signature = try self.allocator.create(ast.FunctionType);
        signature.* = .{ .params = params, .return_type = return_type, .param_aliases = aliases };
        return signature;
    }

    fn reportUnused(self: *SemanticAnalyzer, scope: *Scope) ErrorList!void {
        if (scope.is_deinited) return;

        var it = scope.variables.valueIterator();
        while (it.next()) |variable_ptr| {
            const variable = variable_ptr.*;
            if (variable.used) continue;
            if (variable.is_alias) continue;

            if (variable.type == .FUNCTION) {
                if (self.entry_point_name) |entry| if (std.mem.eql(u8, variable.name, entry)) continue;
                self.reporter.reportWarning(variable.decl_location, ErrorCode.UNUSED_FUNCTION, "unused function '{s}'", .{variable.name});
            } else if (variable.is_param) {
                self.reporter.reportWarning(variable.decl_location, ErrorCode.UNUSED_PARAMETER, "unused parameter '{s}'", .{variable.name});
            } else {
                self.reporter.reportWarning(variable.decl_location, ErrorCode.UNUSED_VARIABLE, "unused variable '{s}'", .{variable.name});
            }
        }

        for (scope.children.items) |child| {
            try self.reportUnused(child);
        }
    }

    /// An `import` in the entry file whose name is never referenced. Unused
    /// reporting is a statement about the unit being compiled, so a library's
    /// imports are not reported.
    fn reportUnusedImports(self: *SemanticAnalyzer) void {
        for (self.graph.record(self.entry_module).statements()) |stmt| {
            if (stmt.data != .Import) continue;
            const import_info = stmt.data.Import;
            if (import_info.import_type != .Specific) continue;
            for (import_info.names) |name| {
                if (self.used_bindings.contains(.{ .module = self.entry_module, .name = name.lexeme })) continue;
                self.reporter.reportWarning(ast.SourceSpan.fromToken(name).location, ErrorCode.UNUSED_IMPORT, "unused import '{s}'", .{name.lexeme});
            }
        }
    }

    /// Register the declarations of `statements` in `scope`.
    ///
    /// At a record's top level (its module scope) this is the record's Types
    /// stage: types are registered by `TypeRef` first, so every signature and
    /// initializer can name them; then functions with their resolved
    /// signatures, so calls see them before their bodies are checked; then
    /// globals in order. A nested block registers its locals the same way. A
    /// type is declared only at file scope: elsewhere it would have no binding,
    /// and two blocks declaring the same name would share one identity.
    fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
        const at_file_scope = scope == self.moduleScope(self.current_module);

        // Types: structs and enums first, then groups, which flatten their
        // member types.
        for (statements) |*stmt| {
            switch (stmt.data) {
                .Expression => |maybe_expr| if (maybe_expr) |expr| switch (expr.data) {
                    .StructDecl => |*decl| try self.declareStruct(decl, scope, at_file_scope, stmt.base),
                    .EnumDecl => |*decl| try self.declareEnum(decl, scope, at_file_scope, stmt.base),
                    else => {},
                },
                .EnumDecl => |*decl| try self.declareEnum(decl, scope, at_file_scope, stmt.base),
                else => {},
            }
        }
        for (statements) |*stmt| {
            const decl = switch (stmt.data) {
                .GroupDecl => |*decl| decl,
                .Expression => |maybe_expr| blk: {
                    const expr = maybe_expr orelse continue;
                    if (expr.data != .GroupDecl) continue;
                    break :blk &expr.data.GroupDecl;
                },
                else => continue,
            };
            if (!at_file_scope) {
                try self.reportNestedDeclaration("Type", decl.name, stmt.base);
                continue;
            }
            try helpers.registerGroupType(self, .{ .module = self.current_module, .name = decl.name.lexeme }, decl.members);
        }

        // Functions, with resolved signatures, before any body or initializer
        // can call them.
        for (statements) |*stmt| {
            if (stmt.data != .FunctionDecl) continue;
            const func = &stmt.data.FunctionDecl;
            // A nested function is `validateStatements`' error.
            if (!at_file_scope) continue;
            const location = getLocationFromBase(stmt.base);

            const param_types = try self.allocator.alloc(ast.TypeInfo, func.params.len);
            const param_aliases = try self.allocator.alloc(bool, func.params.len);
            for (func.params, 0..) |param, i| {
                param_aliases[i] = param.is_alias;
                if (param.type_expr) |type_expr| {
                    param_types[i] = (try self.typeExprToTypeInfo(type_expr)).*;
                    if (param.is_alias) try self.checkAliasParameterType(param, param_types[i], location);
                } else {
                    param_types[i] = .{ .base = .Nothing };
                    if (param.is_alias) {
                        self.reporter.reportCompileError(
                            location,
                            ErrorCode.ALIAS_PARAMETER_REQUIRED,
                            "Alias parameter '{s}' requires an explicit type annotation",
                            .{param.name.lexeme},
                        );
                        self.fatal_error = true;
                    }
                }
            }
            try names.resolveTypeInfo(self, &func.return_type_info, self.current_module, location);

            const func_type = try self.allocator.create(ast.FunctionType);
            func_type.* = .{
                .params = param_types,
                .return_type = try ast.TypeInfo.createDefault(self.allocator),
                .param_aliases = param_aliases,
            };
            func_type.return_type.* = func.return_type_info;

            if (scope.lookupLocalVariable(func.name.lexeme) != null) continue;
            const func_type_info = try ast.TypeInfo.createDefault(self.allocator);
            func_type_info.* = .{ .base = .Function, .function_type = func_type };
            // A function binding carries its type; its value is its
            // declaration, which codegen reads from the AST.
            if (scope.createValueBinding(
                func.name.lexeme,
                .FUNCTION,
                func_type_info,
                true,
            )) |func_var| {
                func_var.recordDeclLocation(func.name);
            } else |err| {
                // Duplicates in a nested scope are reported where the scope is
                // validated; at file scope the loader already rejected them.
                if (err != error.DuplicateVariableName) return err;
            }
        }

        for (statements, 0..) |*stmt, i| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // A module namespace is not a value and cannot be bound to a variable.
                    if (try self.reportNamespaceBinding(decl, stmt.base)) continue;
                    const location = getLocationFromBase(stmt.base);

                    // An initializer is a value position, exactly as when the
                    // declaration is validated: its type is cached here.
                    const prev_bve = self.block_value_expected;
                    self.block_value_expected = decl.initializer != null;
                    defer self.block_value_expected = prev_bve;

                    const type_info = try ast.TypeInfo.createDefault(self.allocator);
                    errdefer self.allocator.destroy(type_info);

                    if (decl.type_info.base != .Nothing) {
                        // The annotation is resolved where it lives, so codegen
                        // reads the declared type's identity off the AST.
                        try names.resolveTypeInfo(self, &statements[i].data.VarDecl.type_info, self.current_module, location);
                        if (decl.type_expr) |type_expr| try names.resolveTypeExpr(self, type_expr, self.current_module);
                        const declared = statements[i].data.VarDecl.type_info;

                        // An incomplete array annotation (no element type) is
                        // completed from the initializer when there is one.
                        if (declared.base == .Array and declared.array_type == null and decl.initializer != null) {
                            type_info.* = (try infer_type.inferTypeFromExpr(self, decl.initializer.?)).*;
                        } else {
                            type_info.* = declared;
                            if (decl.type_expr) |type_expr| {
                                try self.resolveArraySizes(type_info, type_expr);
                                if (type_info.base == .Array) {
                                    statements[i].data.VarDecl.type_info.array_size = type_info.array_size;
                                    statements[i].data.VarDecl.type_info.array_storage = type_info.array_storage;
                                }
                            }
                        }
                        // Preserve mutability from the variable declaration (var vs const)
                        type_info.is_mutable = decl.type_info.is_mutable;
                    } else if (decl.initializer) |init_expr| {
                        const inferred = try infer_type.inferTypeFromExpr(self, init_expr);
                        // Deep copy the inferred type to avoid dangling internal pointers
                        type_info.* = try self.deepCopyTypeInfo(inferred.*);
                        // Preserve the mutability from the variable declaration, not from the initializer
                        type_info.is_mutable = decl.type_info.is_mutable;
                    } else {
                        self.reporter.reportCompileError(
                            location,
                            ErrorCode.VARIABLE_DECLARATION_MISSING_ANNOTATION,
                            "Variable declaration requires either type annotation (::) or initializer",
                            .{},
                        );
                        self.fatal_error = true;
                        continue;
                    }

                    if (decl.initializer) |init_expr| {
                        if (helpers.hasUninferredElement(type_info)) {
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.CANNOT_INFER_ARRAY_ELEMENT_TYPE,
                                "cannot infer element type of empty array literal; add an element type annotation (e.g. `int[]`)",
                                .{},
                            );
                            self.fatal_error = true;
                            continue;
                        }
                        self.tryTagConstLiteralArray(type_info, init_expr);
                    }

                    const token_type = eval.convertTypeToTokenType(type_info.base);

                    var comptime_value: ?TokenLiteral = null;
                    if (decl.initializer) |init_expr| {
                        // An annotated initializer is typed in its context.
                        if (decl.type_info.base != .Nothing) _ = try infer_type.inferTypeIn(self, init_expr, type_info);
                        comptime_value = self.constantInitializer(init_expr, !decl.type_info.is_mutable) catch continue;
                    } else if (self.isEnumTypeRequiringInitializer(type_info)) {
                        self.reporter.reportCompileError(location, ErrorCode.ENUM_REQUIRES_INITIALIZER, "Enum variables must be initialized", .{});
                        self.fatal_error = true;
                        continue;
                    }

                    // ENFORCE: nothing types must be const (unless they have an initializer)
                    if (type_info.base == .Nothing and type_info.is_mutable and decl.initializer == null) {
                        self.reporter.reportCompileError(location, ErrorCode.NOTHING_TYPE_MUST_BE_CONST, "Nothing type variables must be declared as 'const'", .{});
                        self.fatal_error = true;
                        continue;
                    }

                    self.checkFreshName(scope, decl.name);
                    const binding = if (decl.type_expr == null)
                        scope.createValueBindingAt(decl.name.lexeme, token_type, type_info, !type_info.is_mutable, decl.name)
                    else
                        scope.createValueBinding(decl.name.lexeme, token_type, type_info, !type_info.is_mutable);
                    if (binding) |declared| {
                        declared.recordDeclLocation(decl.name);
                        self.memory.scope_manager.value_storage.get(declared.storage_id).?.comptime_value = comptime_value;
                    } else |err| {
                        if (err != error.DuplicateVariableName) return err;
                        self.reporter.reportCompileError(location, ErrorCode.DUPLICATE_VARIABLE, "Duplicate variable name '{s}' in current scope", .{decl.name.lexeme});
                        self.fatal_error = true;
                        continue;
                    }
                    if (at_file_scope) {
                        try self.recordGlobalSite(.{
                            .symbol = .{
                                .module = self.current_module,
                                .name = decl.name.lexeme,
                                .kind = if (decl.type_info.is_mutable) .Variable else .Constant,
                            },
                            .place = .{ .name = &statements[i].data.VarDecl.name },
                        });
                    }
                },
                .Expression => |maybe_expr| {
                    const expr = maybe_expr orelse continue;
                    // Type declarations were registered above. Every other
                    // expression statement is analyzed now so that compiler
                    // methods like @push are lowered (e.g. InternalCall ->
                    // ArrayPush) even when their values are not used.
                    switch (expr.data) {
                        .StructDecl, .EnumDecl, .GroupDecl => {},
                        else => _ = try infer_type.inferTypeFromExpr(self, expr),
                    }
                },
                .FunctionDecl, .EnumDecl, .GroupDecl, .Import, .ZigDecl => {},
                .Return, .Continue, .Break, .Assert, .Defer, .Lift => {},
            }
        }

        // Return types: a function returns what it declares (`nothing` without
        // `returns`). Its body is checked against that when it is analyzed, in
        // its real scope; collection never walks a body.
        for (statements) |stmt| {
            const func = switch (stmt.data) {
                .FunctionDecl => |func| func,
                else => continue,
            };
            const func_var = scope.lookupLocalVariable(func.name.lexeme) orelse continue;
            const storage = self.memory.scope_manager.value_storage.get(func_var.storage_id) orelse continue;
            const func_type = storage.type_info.function_type orelse continue;
            func_type.return_type.* = if (func.return_type_info.base == .Nothing)
                .{ .base = .Nothing, .is_mutable = false }
            else
                func.return_type_info;
            try self.function_return_types.put(stmt.base.id, func_type.return_type);
        }

        for (statements) |*stmt| {
            if (stmt.data != .VarDecl) continue;
            if (stmt.data.VarDecl.type_expr == null) continue;
            if (stmt.data.VarDecl.type_info.base != .Array or stmt.data.VarDecl.type_info.array_size != null) continue;
            const variable = scope.lookupLocalVariable(stmt.data.VarDecl.name.lexeme) orelse continue;
            const storage = self.memory.scope_manager.value_storage.get(variable.storage_id) orelse continue;
            stmt.data.VarDecl.type_info.array_size = storage.type_info.array_size;
            stmt.data.VarDecl.type_info.array_storage = storage.type_info.array_storage;
        }
    }

    /// An alias parameter must name a concrete type a caller can lend: maps,
    /// functions, and anonymous structs or enums cannot be aliased.
    fn checkAliasParameterType(self: *SemanticAnalyzer, param: ast.FunctionParam, param_type: ast.TypeInfo, location: Location) ErrorList!void {
        const disallowed = switch (param_type.base) {
            .Map, .Function, .Struct, .Enum => true,
            // Arrays are allowed as they're perfect candidates for aliasing (avoid copying)
            // Unions are allowed so alias parameters can accept type-narrowed values via match
            else => false,
        };
        if (disallowed) {
            self.reporter.reportCompileError(
                location,
                ErrorCode.INVALID_ALIAS_PARAMETER,
                "Alias parameter '{s}' cannot have type {s}; use a named custom type (e.g., Person) or a primitive",
                .{ param.name.lexeme, @tagName(param_type.base) },
            );
            self.fatal_error = true;
        } else if (param_type.base == .Custom and param_type.custom_type == null) {
            self.reporter.reportCompileError(
                location,
                ErrorCode.INVALID_ALIAS_PARAMETER,
                "Alias parameter '{s}' must use a concrete type name",
                .{param.name.lexeme},
            );
            self.fatal_error = true;
        }
    }

    /// Register a struct declared at file scope: its fields resolved, its type
    /// bound in the module scope (a struct name is a value in a literal or a
    /// static call), and its methods.
    fn declareStruct(self: *SemanticAnalyzer, decl: *ast.StructDecl, scope: *Scope, at_file_scope: bool, base: ast.Base) ErrorList!void {
        if (!at_file_scope) return self.reportNestedDeclaration("Type", decl.name, base);
        const ref = TypeRef{ .module = self.current_module, .name = decl.name.lexeme };

        const fields = try self.allocator.alloc(ast.StructFieldType, decl.fields.len);
        for (decl.fields, fields) |field, *struct_field| {
            struct_field.* = .{
                .name = field.name.lexeme,
                .type_info = try self.fieldStorageType(try self.typeExprToTypeInfo(field.type_expr)),
                .is_public = field.is_public,
            };
        }
        try helpers.registerStructType(self, ref, fields);

        const type_info = try ast.TypeInfo.createDefault(self.allocator);
        type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .struct_fields = fields, .is_mutable = false };
        try self.bindTypeName(scope, decl.name, .STRUCT, type_info, base);
        try self.checkStructMemberNames(decl);
        try self.registerStructMethods(decl, ref);
    }

    /// The type a struct field written as `type` holds. A field is one word
    /// of its struct, so an array field holds a dynamic array: a fixed array
    /// stored into it is converted (`array.from_fixed`), and the field reads
    /// as `T[]` (`plan/register-hir.md`, Q2).
    fn fieldStorageType(self: *SemanticAnalyzer, written: *ast.TypeInfo) ErrorList!*ast.TypeInfo {
        if (written.base != .Array) return written;
        const element = written.array_type orelse return written;
        const out = try self.allocator.create(ast.TypeInfo);
        out.* = written.*;
        out.array_size = null;
        out.array_storage = .dynamic;
        out.array_type = try self.fieldStorageType(element);
        return out;
    }

    /// A struct names each member once: no two fields, no two methods or
    /// functions, and no method or function sharing a field's name.
    fn checkStructMemberNames(self: *SemanticAnalyzer, decl: *ast.StructDecl) ErrorList!void {
        var seen = std.StringHashMap(void).init(self.allocator);
        defer seen.deinit();
        for (decl.fields) |field| try self.claimStructMember(&seen, decl.name, field.name);
        for (decl.methods) |method| try self.claimStructMember(&seen, decl.name, method.name);
    }

    fn claimStructMember(self: *SemanticAnalyzer, seen: *std.StringHashMap(void), struct_name: Token, member: Token) ErrorList!void {
        if ((try seen.getOrPut(member.lexeme)).found_existing) {
            self.reporter.reportCompileError(
                ast.SourceSpan.fromToken(member).location,
                ErrorCode.DUPLICATE_VARIABLE,
                "struct '{s}' already has a member named '{s}'",
                .{ struct_name.lexeme, member.lexeme },
            );
            self.fatal_error = true;
        }
    }

    /// Register an enum declared at file scope and bind its name, so
    /// `Color.Red` resolves its qualifier.
    fn declareEnum(self: *SemanticAnalyzer, decl: *ast.EnumDecl, scope: *Scope, at_file_scope: bool, base: ast.Base) ErrorList!void {
        if (!at_file_scope) return self.reportNestedDeclaration("Type", decl.name, base);
        const ref = TypeRef{ .module = self.current_module, .name = decl.name.lexeme };

        const variant_names = try self.allocator.alloc([]const u8, decl.variants.len);
        for (decl.variants, variant_names) |variant, *name| name.* = variant.lexeme;
        try helpers.registerEnumType(self, ref, variant_names);

        const type_info = try ast.TypeInfo.createDefault(self.allocator);
        type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false };
        try self.bindTypeName(scope, decl.name, .ENUM, type_info, base);
    }

    fn bindTypeName(self: *SemanticAnalyzer, scope: *Scope, name: Token, kind: TokenType, type_info: *ast.TypeInfo, base: ast.Base) ErrorList!void {
        const variable = scope.createValueBinding(name.lexeme, kind, type_info, true) catch |err| {
            if (err != error.DuplicateVariableName) return err;
            self.reporter.reportCompileError(getLocationFromBase(base), ErrorCode.DUPLICATE_VARIABLE, "Duplicate type name '{s}' in current scope", .{name.lexeme});
            self.fatal_error = true;
            return;
        };
        variable.recordDeclLocation(name);
    }

    /// Types and functions are declared at file scope only.
    fn reportNestedDeclaration(self: *SemanticAnalyzer, comptime kind: []const u8, name: Token, base: ast.Base) ErrorList!void {
        self.reporter.reportCompileError(
            getLocationFromBase(base),
            ErrorCode.NESTED_DECLARATION,
            kind ++ " '{s}' must be declared at file scope",
            .{name.lexeme},
        );
        self.fatal_error = true;
    }

    pub fn validateStatements(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        var prev_was_terminator = false;
        for (statements, 0..) |stmt, i| {
            if (prev_was_terminator) {
                self.reporter.reportWarning(
                    getLocationFromBase(stmt.base),
                    ErrorCode.UNREACHABLE_CODE,
                    "unreachable code",
                    .{},
                );
            }

            switch (stmt.data) {
                .VarDecl => {
                    // Resolve non-literal fixed-array sizes (e.g. `byte[n]`, `byte[2+2]`)
                    // now that earlier consts in this scope are bound, and persist the
                    // result to the AST so codegen creates a correctly sized array.
                    if (statements[i].data.VarDecl.type_expr) |te| {
                        self.tryResolveArraySizes(&statements[i].data.VarDecl.type_info, te);
                    }
                    // A module namespace is not a value and cannot be bound to a variable.
                    if (try self.reportNamespaceBinding(statements[i].data.VarDecl, stmt.base)) continue;
                    const location = getLocationFromBase(stmt.base);
                    // The annotation is resolved where it lives, so codegen
                    // reads the declared type's identity off the AST.
                    if (statements[i].data.VarDecl.type_info.base != .Nothing) {
                        try names.resolveTypeInfo(self, &statements[i].data.VarDecl.type_info, self.current_module, location);
                        if (statements[i].data.VarDecl.type_expr) |type_expr| try names.resolveTypeExpr(self, type_expr, self.current_module);
                    }
                    const decl = statements[i].data.VarDecl;
                    const prev_bve = self.block_value_expected;
                    self.block_value_expected = decl.initializer != null;
                    defer self.block_value_expected = prev_bve;

                    const scope = self.current_scope.?;
                    const prev_initializing = self.current_initializing_var;
                    self.current_initializing_var = decl.name.lexeme;
                    defer self.current_initializing_var = prev_initializing;
                    // A declaration collectDeclarations already bound (a global)
                    // is only type-checked here. Only the current scope is
                    // consulted, so a local declaration may shadow an outer one.
                    if (scope.lookupLocalVariable(decl.name.lexeme) == null) {
                        const type_info = try ast.TypeInfo.createDefault(self.allocator);
                        errdefer self.allocator.destroy(type_info);

                        if (decl.type_info.base != .Nothing) {
                            // An incomplete array annotation (no element type) is
                            // completed from the initializer when there is one.
                            if (decl.type_info.base == .Array and decl.type_info.array_type == null and decl.initializer != null) {
                                type_info.* = (try infer_type.inferTypeFromExpr(self, decl.initializer.?)).*;
                            } else {
                                type_info.* = decl.type_info;
                            }
                            // Preserve mutability from the variable declaration (var vs const)
                            type_info.is_mutable = decl.type_info.is_mutable;
                        } else if (decl.initializer) |init_expr| {
                            type_info.* = (try infer_type.inferTypeFromExpr(self, init_expr)).*;
                            // Preserve the mutability from the variable declaration, not from the initializer
                            type_info.is_mutable = decl.type_info.is_mutable;
                        } else {
                            self.reporter.reportCompileError(
                                location,
                                ErrorCode.VARIABLE_DECLARATION_MISSING_ANNOTATION,
                                "Variable declaration requires either type annotation (::) or initializer",
                                .{},
                            );
                            self.fatal_error = true;
                            continue;
                        }

                        const token_type = eval.convertTypeToTokenType(type_info.base);

                        if (decl.initializer != null) {
                            if (helpers.hasUninferredElement(type_info)) {
                                self.reporter.reportCompileError(
                                    location,
                                    ErrorCode.CANNOT_INFER_ARRAY_ELEMENT_TYPE,
                                    "cannot infer element type of empty array literal; add an element type annotation (e.g. `int[]`)",
                                    .{},
                                );
                                self.fatal_error = true;
                                continue;
                            }
                        }

                        var comptime_value: ?TokenLiteral = null;
                        if (decl.initializer) |init_expr| {
                            // Typed in its context; see the module-level
                            // declarations above.
                            if (decl.type_info.base != .Nothing) _ = try infer_type.inferTypeIn(self, init_expr, type_info);
                            comptime_value = self.constantInitializer(init_expr, !decl.type_info.is_mutable) catch continue;
                        } else if (self.isEnumTypeRequiringInitializer(type_info)) {
                            self.reporter.reportCompileError(location, ErrorCode.ENUM_REQUIRES_INITIALIZER, "Enum variables must be initialized", .{});
                            self.fatal_error = true;
                            continue;
                        }

                        self.checkFreshName(scope, decl.name);
                        const binding = if (decl.type_expr == null)
                            scope.createValueBindingAt(decl.name.lexeme, token_type, type_info, !type_info.is_mutable, decl.name)
                        else
                            scope.createValueBinding(decl.name.lexeme, token_type, type_info, !type_info.is_mutable);
                        if (binding) |declared| {
                            declared.recordDeclLocation(decl.name);
                            self.memory.scope_manager.value_storage.get(declared.storage_id).?.comptime_value = comptime_value;
                        } else |err| {
                            if (err != error.DuplicateVariableName) return err;
                            self.reporter.reportCompileError(location, ErrorCode.DUPLICATE_VARIABLE, "Duplicate variable name '{s}' in current scope", .{decl.name.lexeme});
                            self.fatal_error = true;
                            continue;
                        }
                    }
                    // The declaration stores its binding's type, which an
                    // incomplete array annotation completes from the initializer.
                    const decl_binding = scope.lookupLocalVariable(decl.name.lexeme);
                    try names.recordStoreTarget(self, stmt.base.id, decl_binding, decl_binding, scope == self.moduleScope(self.current_module));

                    // Type checking
                    if (decl.initializer) |init_expr| {
                        const init_type = if (decl.type_info.base != .Nothing)
                            try infer_type.inferTypeIn(self, init_expr, &decl.type_info)
                        else
                            try infer_type.inferTypeFromExpr(self, init_expr);
                        if (decl.type_info.base != .Nothing) {
                            var declared = decl.type_info;
                            try helpers.unifyTypesExpr(self, &declared, init_type, init_expr, .{ .location = location });
                        }
                    }
                },
                .Expression => |expr| {
                    if (expr) |expression| {
                        switch (expression.data) {
                            // A type is declared at file scope; its methods are
                            // checked here, once, with the struct bound as the
                            // receiver context. Without this, unresolved names
                            // inside a method body never error and codegen
                            // silently emits a null-based access.
                            .StructDecl => |*struct_decl| {
                                if (self.current_scope != self.moduleScope(self.current_module)) {
                                    try self.reportNestedDeclaration("Type", struct_decl.name, stmt.base);
                                    continue;
                                }
                                const ref = TypeRef{ .module = self.current_module, .name = struct_decl.name.lexeme };
                                for (struct_decl.methods) |method| {
                                    try self.validateFunctionBodyWithStruct(
                                        method,
                                        .{ .location = getLocationFromBase(expression.base) },
                                        method.return_type_info,
                                        .{ .ref = ref, .has_this = !method.is_static },
                                    );
                                }
                            },
                            .EnumDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                                try self.reportNestedDeclaration("Type", decl.name, stmt.base);
                            },
                            .GroupDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                                try self.reportNestedDeclaration("Type", decl.name, stmt.base);
                            },
                            else => {
                                _ = try infer_type.inferTypeFromExpr(self, expression);
                                if (expression.data == .Unreachable) prev_was_terminator = true;
                            },
                        }
                    }
                },
                .Return => |return_stmt| {
                    if (return_stmt.value) |value| {
                        _ = try self.checkReturnValue(value, getLocationFromBase(stmt.base));
                    } else {
                        self.checkBareReturn(getLocationFromBase(stmt.base));
                    }
                    prev_was_terminator = true;
                },
                .FunctionDecl => |func| {
                    if (self.current_scope != self.moduleScope(self.current_module)) {
                        try self.reportNestedDeclaration("Function", func.name, stmt.base);
                        continue;
                    }
                    const location = ast.SourceSpan{ .location = getLocationFromBase(stmt.base) };
                    if (self.function_return_types.get(stmt.base.id)) |return_type| {
                        try self.validateFunctionBodyWithStruct(func, location, return_type.*, null);
                    } else {
                        try self.validateFunctionBodyWithStruct(func, location, func.return_type_info, null);
                    }
                },
                .Break, .Continue, .Lift => {
                    prev_was_terminator = true;
                },
                // A deferred expression runs at scope exit like any other
                // statement, so it is type-checked like one. Lowering reads its
                // operand types from this pass (plan/type-authority.md).
                .Defer => |deferred| {
                    _ = try infer_type.inferTypeFromExpr(self, deferred);
                },
                .EnumDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                    try self.reportNestedDeclaration("Type", decl.name, stmt.base);
                },
                .GroupDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                    try self.reportNestedDeclaration("Type", decl.name, stmt.base);
                },
                .Assert => |assertion| {
                    _ = try infer_type.inferTypeFromExpr(self, assertion.condition);
                    if (assertion.message) |message| _ = try infer_type.inferTypeFromExpr(self, message);
                },
                .ZigDecl, .Import => {},
            }
        }
    }

    fn deepCopyTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        var copied = type_info;

        // Clear pointer fields; we'll rebuild them freshly as needed
        copied.array_type = null;
        copied.struct_fields = null;
        copied.function_type = null;
        copied.variants = null;
        copied.union_type = null;
        copied.map_key_type = null;
        copied.map_value_type = null;

        switch (type_info.base) {
            .Array => {
                if (type_info.array_type) |elem| {
                    const elem_copy = try ast.TypeInfo.createDefault(self.allocator);
                    elem_copy.* = try self.deepCopyTypeInfo(elem.*);
                    copied.array_type = elem_copy;
                }
            },
            .Struct, .Custom => {
                if (type_info.struct_fields) |fields| {
                    const new_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                    for (fields, 0..) |field, i| {
                        const ti_copy = try ast.TypeInfo.createDefault(self.allocator);
                        ti_copy.* = try self.deepCopyTypeInfo(field.type_info.*);
                        new_fields[i] = .{ .name = field.name, .type_info = ti_copy };
                    }
                    copied.struct_fields = new_fields;
                }
            },
            .Function => {
                if (type_info.function_type) |fn_type| {
                    const params_copy = try self.allocator.alloc(ast.TypeInfo, fn_type.params.len);
                    for (fn_type.params, 0..) |p, i| {
                        params_copy[i] = try self.deepCopyTypeInfo(p);
                    }
                    const ret_copy = try ast.TypeInfo.createDefault(self.allocator);
                    ret_copy.* = try self.deepCopyTypeInfo(fn_type.return_type.*);
                    const fn_copy = try self.allocator.create(ast.FunctionType);
                    fn_copy.* = .{
                        .params = params_copy,
                        .return_type = ret_copy,
                        .param_aliases = if (fn_type.param_aliases) |pa| try self.allocator.dupe(bool, pa) else null,
                    };
                    copied.function_type = fn_copy;
                }
            },
            .Union => {
                if (type_info.union_type) |u| {
                    const new_types = try self.allocator.alloc(*ast.TypeInfo, u.types.len);
                    for (u.types, 0..) |member, i| {
                        const m_copy = try ast.TypeInfo.createDefault(self.allocator);
                        m_copy.* = try self.deepCopyTypeInfo(member.*);
                        new_types[i] = m_copy;
                    }
                    const u_copy = try self.allocator.create(ast.UnionType);
                    u_copy.* = .{ .types = new_types, .current_type_index = u.current_type_index };
                    copied.union_type = u_copy;
                }
            },
            .Enum => {
                if (type_info.variants) |v| {
                    // Duplicate outer slice only; inner strings are not owned here
                    copied.variants = try self.allocator.dupe([]const u8, v);
                }
            },
            .Map => {
                // Shallow copy key/value type pointers to avoid dereferencing
                // potentially invalid pointers from inference.
                copied.map_key_type = type_info.map_key_type;
                copied.map_value_type = type_info.map_value_type;
            },
            else => {},
        }

        return copied;
    }

    pub fn deepCopyTypeInfoPtr(self: *SemanticAnalyzer, src: *ast.TypeInfo) !*ast.TypeInfo {
        const copied = try ast.TypeInfo.createDefault(self.allocator);
        copied.* = try self.deepCopyTypeInfo(src.*);
        return copied;
    }

    /// Resolve, in place, what every pattern of `case` denotes, so the arm's
    /// narrowing here and its lowering in codegen read one answer. `group` is
    /// the subject's group and `subject_enum` its enum, when it is one.
    pub fn resolveMatchPatterns(self: *SemanticAnalyzer, case: *ast.MatchCase, group: ?TypeRef, subject_enum: ?TypeRef) ErrorList!void {
        const resolved = try self.allocator.alloc(ast.MatchCase.Resolved, case.patterns.len);
        for (case.patterns, resolved) |pattern, *slot| {
            slot.* = if (pattern.type != .IDENTIFIER or std.mem.eql(u8, pattern.lexeme, "else"))
                .token
            else if (subject_enum) |enum_ref|
                .{ .variant = enum_ref }
            else if (group) |group_ref|
                if (try self.groupMemberType(group_ref, pattern.lexeme)) |ref| .{ .type = ref } else .token
            else if (try names.resolveTypeName(self, pattern.lexeme, self.current_module)) |ref|
                .{ .type = ref }
            else
                .token;
        }
        for (case.path_patterns) |path| {
            if (path.tokens.len == 0) continue;
            resolved[path.pattern] = try self.resolvePathPattern(path, group);
            if (path.field_names.len > 0) try self.checkDestructure(path, resolved[path.pattern]);
        }
        case.resolved = resolved;
    }

    /// A destructuring path must name a struct declaring every field it binds.
    fn checkDestructure(self: *SemanticAnalyzer, path: ast.MatchCase.PathPattern, resolved: ast.MatchCase.Resolved) ErrorList!void {
        const location = ast.SourceSpan.fromToken(path.tokens[path.tokens.len - 1]).location;
        const struct_id = switch (resolved) {
            .type => |ref| blk: {
                try self.ensureTypes(ref.module);
                break :blk self.struct_table.idOf(ref);
            },
            .token, .variant => null,
        } orelse {
            self.reporter.reportCompileError(location, ErrorCode.UNKNOWN_TYPE, "Unknown struct type '{s}'", .{try self.joinTokens(path.tokens)});
            self.fatal_error = true;
            return;
        };
        const fields = self.struct_table.fields(struct_id) orelse &.{};
        for (path.field_names) |field_token| {
            for (fields) |field| {
                if (std.mem.eql(u8, field.name, field_token.lexeme)) break;
            } else {
                self.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(field_token).location,
                    ErrorCode.FIELD_NOT_FOUND,
                    "Struct '{s}' has no field '{s}'",
                    .{ resolved.type.name, field_token.lexeme },
                );
                self.fatal_error = true;
            }
        }
    }

    /// Narrow the matched variable within one match arm, then infer the arm's
    /// body. The arm's patterns were resolved by `resolveMatchPatterns`.
    pub fn inferMatchCaseTypeWithNarrow(self: *SemanticAnalyzer, case: ast.MatchCase, subject_type: *const ast.TypeInfo, matched_var_name: ?[]const u8) ErrorList!*ast.TypeInfo {
        // A per-case scope keeps narrowing from leaking out of the arm.
        const case_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer case_scope.deinit();

        const prev_scope = self.current_scope;
        self.current_scope = case_scope;
        defer self.current_scope = prev_scope;

        if (matched_var_name) |name| {
            // An arm narrows the subject only when it lists a single pattern: with
            // several, any of them could have selected it, so the body sees the
            // subject as declared. Codegen reads the narrowing from each read of
            // the subject; nothing re-derives it.
            const narrows = case.patterns.len == 1;

            if (case.path_patterns.len > 0) {
                const path = case.path_patterns[0];
                if (path.tokens.len >= 1) {
                    const member: ?TypeRef = switch (case.resolved[path.pattern]) {
                        .type => |ref| ref,
                        .token, .variant => null,
                    };
                    if (narrows) {
                        if (member) |ref| (try bindNarrowed(case_scope, name, .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false }, self.allocator)).is_view = true;
                    }

                    // Struct destructuring binds each field name as a local.
                    // These are the arm's own names, not the subject, so they
                    // bind whether or not the arm narrows.
                    if (path.field_names.len > 0) {
                        if (member) |ref| if (self.struct_table.idOf(ref)) |sid| {
                            const fields = self.struct_table.fields(sid) orelse &.{};
                            const storages = try self.allocator.alloc(u32, path.field_names.len);
                            for (path.field_names, storages) |field_token, *storage| {
                                // An undeclared field is `checkDestructure`'s
                                // error, which stops the compile before codegen.
                                const field = for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, field_token.lexeme)) break f;
                                } else continue;
                                self.checkFreshName(case_scope, field_token);
                                storage.* = (try bindNarrowed(case_scope, field_token.lexeme, field.type_info.*, self.allocator)).storage_id;
                            }
                            case.path_patterns[0].field_storages = storages;
                        };
                    }
                }
            } else if (narrows) {
                if (try self.patternType(case.patterns[0], case.resolved[0], subject_type)) |narrowed| {
                    (try bindNarrowed(case_scope, name, narrowed, self.allocator)).is_view = true;
                }
            }
        }

        const case_body_type = try infer_type.inferTypeFromExpr(self, case.body);
        if (matched_var_name) |name| case_scope.propagateUsedToParent(name);
        return case_body_type;
    }

    /// One name, one meaning: a declaration inside a function, a block or a
    /// pattern may not reuse a name already visible where it appears — an
    /// enclosing local or parameter, or a top-level name of the file (its
    /// declarations, imports and aliases). A sibling scope's binding is not
    /// visible, so it does not conflict. A duplicate within one scope is
    /// `DUPLICATE_VARIABLE`'s, and top-level names are checked against each
    /// other by the loader. The declaration still binds, so its uses resolve.
    pub fn checkFreshName(self: *SemanticAnalyzer, scope: *Scope, name: Token) void {
        if (scope == self.moduleScope(self.current_module)) return;
        const visible = if (scope.parent) |parent| parent.lookupVariable(name.lexeme) != null else false;
        if (!visible and !self.graph.record(self.current_module).bindings.contains(name.lexeme)) return;
        self.reporter.reportCompileError(
            ast.SourceSpan.fromToken(name).location,
            ErrorCode.SHADOWED_NAME,
            "'{s}' is already declared where this declaration is visible; a name has one meaning wherever it can be seen",
            .{name.lexeme},
        );
        self.fatal_error = true;
    }

    fn bindNarrowed(scope: *Scope, name: []const u8, narrowed: ast.TypeInfo, allocator: std.mem.Allocator) ErrorList!*Variable {
        const type_info = try ast.TypeInfo.createDefault(allocator);
        type_info.* = narrowed;
        return scope.createValueBinding(name, eval.convertTypeToTokenType(type_info.base), type_info, false);
    }

    /// What a match path denotes. Against a group subject the path names a
    /// member, split exactly the way codegen splits it: through the group
    /// qualifier when written through it (`Group.Member.Variant`), otherwise
    /// the token ahead of the variant. Otherwise the path spells a type
    /// outright — bare or module-qualified (`std.json.Node`) — or an enum
    /// variant (`ns.Color.Red`), whose enum is everything ahead of the variant.
    fn resolvePathPattern(self: *SemanticAnalyzer, path: ast.MatchCase.PathPattern, group: ?TypeRef) ErrorList!ast.MatchCase.Resolved {
        if (group) |group_ref| {
            const split = path.split(group_ref.name);
            if (try self.groupMemberType(group_ref, split.member.lexeme)) |ref| return .{ .type = ref };
        }
        if (try names.resolveTypeName(self, try self.joinTokens(path.tokens), self.current_module)) |ref| return .{ .type = ref };
        if (path.field_names.len > 0 or path.tokens.len < 2) return .token;
        const prefix = try self.joinTokens(path.tokens[0 .. path.tokens.len - 1]);
        const ref = (try names.resolveTypeName(self, prefix, self.current_module)) orelse return .token;
        const custom = (try self.customType(ref)) orelse return .token;
        return if (custom.kind == .Enum) .{ .variant = ref } else .token;
    }

    /// The member of `group` written with `qualifier`.
    pub fn groupMemberType(self: *SemanticAnalyzer, group: TypeRef, qualifier: []const u8) ErrorList!?TypeRef {
        try self.ensureTypes(group.module);
        const id = self.group_table.idOf(group) orelse return null;
        for (self.group_table.members(id) orelse &.{}) |member| {
            if (std.mem.eql(u8, member.qualifier, qualifier)) return member.ref;
        }
        return null;
    }

    fn joinTokens(self: *SemanticAnalyzer, tokens: []const ast.Token) ErrorList![]const u8 {
        var out = std.array_list.Managed(u8).init(self.allocator);
        for (tokens, 0..) |tok, i| {
            if (i > 0) try out.append('.');
            try out.appendSlice(tok.lexeme);
        }
        return out.toOwnedSlice();
    }

    /// The type a bare match pattern narrows to, or null when the pattern
    /// cannot be mapped to a type: binding the subject to `nothing` would make
    /// every use of it in the arm a type error. `resolved` is what analysis
    /// resolved the pattern to.
    fn patternType(self: *SemanticAnalyzer, pattern: Token, resolved: ast.MatchCase.Resolved, subject_type: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
        if (std.mem.indexOf(u8, pattern.lexeme, "[]") != null) return self.arrayPatternMember(pattern.lexeme, subject_type);
        return switch (pattern.type) {
            .INT_TYPE, .INT => .{ .base = .Int, .is_mutable = false },
            .FLOAT_TYPE, .FLOAT => .{ .base = .Float, .is_mutable = false },
            .STRING_TYPE, .STRING => .{ .base = .String, .is_mutable = false },
            .BYTE_TYPE, .BYTE => .{ .base = .Byte, .is_mutable = false },
            .TETRA_TYPE, .TETRA => .{ .base = .Tetra, .is_mutable = false },
            .NOTHING_TYPE, .NOTHING => .{ .base = .Nothing, .is_mutable = false },
            .DOT => .{ .base = .Enum, .is_mutable = false }, // an enum member pattern implies an enum
            .IDENTIFIER => switch (resolved) {
                .type => |ref| .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false },
                .variant => |ref| .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false },
                .token => null,
            },
            else => null,
        };
    }

    /// The member of `subject_type` an array pattern names. A pattern spells
    /// an array by its element (`string[]`, `Point[][]`), so the narrowed type
    /// is the subject's member of that spelling, element type and all. Null
    /// when the subject has no such member: the arm can never be selected.
    fn arrayPatternMember(self: *SemanticAnalyzer, spelling: []const u8, subject_type: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
        const members: []const *ast.TypeInfo = if (subject_type.union_type) |u| u.types else &.{};
        for (members) |member| {
            if (member.base != .Array) continue;
            var written: std.ArrayListUnmanaged(u8) = .empty;
            defer written.deinit(self.allocator);
            if (!try writeArraySpelling(self.allocator, &written, member)) continue;
            if (std.mem.eql(u8, written.items, spelling)) {
                var narrowed = member.*;
                narrowed.is_mutable = false;
                return narrowed;
            }
        }
        return null;
    }

    /// Spell an array type as a pattern writes it, or return false for an
    /// element no pattern can name.
    fn writeArraySpelling(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), t: *const ast.TypeInfo) !bool {
        const element = t.array_type orelse return false;
        if (element.base == .Array) {
            if (!try writeArraySpelling(allocator, out, element)) return false;
        } else if (element.custom_type) |custom| {
            try out.appendSlice(allocator, custom.displayName());
        } else {
            try out.appendSlice(allocator, switch (element.base) {
                .Int => "int",
                .Float => "float",
                .String => "string",
                .Byte => "byte",
                .Tetra => "tetra",
                else => return false,
            });
        }
        try out.appendSlice(allocator, "[]");
        return true;
    }

    /// Best-effort resolution of fixed-array sizes from their declared size
    /// expressions (literals, constant expressions, or references to bound
    /// consts), persisting the result into `type_info`. Unlike resolveArraySizes
    /// this never reports: if a size cannot be evaluated yet (e.g. during an
    /// early inference pass, before the referenced consts are bound) it is left
    /// unresolved for a later pass to fill in.
    fn tryResolveArraySizes(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, type_expr: *ast.TypeExpr) void {
        if (type_expr.data != .Array) return;
        const array_type = type_expr.data.Array;
        if (type_info.base == .Array and type_info.array_size == null) {
            if (array_type.size) |size_expr| switch (consteval.evaluate(size_expr, ConstantNames{ .analyzer = self })) {
                .value => |size| if (size == .int and size.int >= 0) {
                    type_info.array_size = @intCast(size.int);
                    type_info.array_storage = .fixed;
                },
                .not_constant, .fault => {},
            };
        }
        if (type_info.base == .Array) {
            if (type_info.array_type) |child| {
                self.tryResolveArraySizes(child, array_type.element_type);
            }
        }
    }

    fn resolveArraySizes(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, type_expr: *ast.TypeExpr) ErrorList!void {
        switch (type_expr.data) {
            .Array => |*array_type| {
                if (array_type.size) |size_expr| {
                    if (type_info.base == .Array and type_info.array_size == null) {
                        const location = getLocationFromBase(size_expr.base);
                        switch (consteval.evaluate(size_expr, ConstantNames{ .analyzer = self })) {
                            .value => |size| if (size == .int and size.int >= 0) {
                                type_info.array_size = @intCast(size.int);
                                type_info.array_storage = .fixed;
                            } else {
                                self.reporter.reportCompileError(location, ErrorCode.INVALID_ARRAY_TYPE, "array size must be a non-negative integer", .{});
                                self.fatal_error = true;
                            },
                            .not_constant => {
                                self.reporter.reportCompileError(location, ErrorCode.INVALID_ARRAY_TYPE, "array size must be a compile-time constant integer", .{});
                                self.fatal_error = true;
                            },
                            .fault => |fault| {
                                consteval.report(self.reporter, getLocationFromBase(fault.site.base), fault.fault);
                                self.fatal_error = true;
                            },
                        }
                    }
                }
                if (type_info.base == .Array) {
                    if (type_info.array_type) |child_type_info| {
                        try self.resolveArraySizes(child_type_info, array_type.element_type);
                    }
                }
            },
            .Struct => |fields| {
                if (type_info.base == .Struct or type_info.base == .Custom) {
                    if (type_info.struct_fields) |struct_fields| {
                        for (fields, struct_fields) |field, *sf| {
                            try self.resolveArraySizes(sf.type_info, field.type_expr);
                        }
                    }
                }
            },
            .Union => |types| {
                if (type_info.base == .Union) {
                    if (type_info.union_type) |union_type| {
                        for (types, union_type.types) |ut, ut_info| {
                            try self.resolveArraySizes(ut_info, ut);
                        }
                    }
                }
            },
            .Map => |*map| {
                if (type_info.base == .Map) {
                    if (type_info.map_key_type) |key_type_info| {
                        if (map.key_type) |key_type_expr| {
                            try self.resolveArraySizes(key_type_info, key_type_expr);
                        }
                    }
                    if (type_info.map_value_type) |value_type_info| {
                        try self.resolveArraySizes(value_type_info, map.value_type);
                    }
                }
            },
            else => {},
        }
    }

    fn tryTagConstLiteralArray(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, initializer: *ast.Expr) void {
        if (type_info.base != .Array) return;
        if (type_info.is_mutable) return;
        if (type_info.array_storage == .fixed) return;
        if (!self.isCompileTimeArrayLiteral(initializer)) return;

        type_info.array_storage = .const_literal;
        if (type_info.array_size == null) {
            type_info.array_size = self.literalArrayLength(initializer);
        }
    }

    fn isCompileTimeArrayLiteral(self: *SemanticAnalyzer, expr: *ast.Expr) bool {
        return switch (expr.data) {
            .Array => |elements| blk: {
                for (elements) |element| {
                    switch (element.data) {
                        .Literal => {},
                        .Array => if (!self.isCompileTimeArrayLiteral(element)) return false,
                        else => return false,
                    }
                }
                break :blk true;
            },
            else => false,
        };
    }

    fn literalArrayLength(_: *SemanticAnalyzer, expr: *ast.Expr) usize {
        // TODO: move to eval_utils.zig as standalone utility; self unused due to method call convention
        return switch (expr.data) {
            .Array => |elements| elements.len,
            else => 0,
        };
    }

    pub fn ensureDynamicArrayStorage(self: *SemanticAnalyzer, array_type: *const ast.TypeInfo, location: Location, usage: []const u8) bool {
        if (array_type.array_storage == .dynamic) return true;

        const storage_desc = switch (array_type.array_storage) {
            .fixed => "fixed-size",
            .const_literal => "const literal",
            .dynamic => unreachable,
        };

        self.reporter.reportCompileError(
            location,
            ErrorCode.ARRAY_REQUIRES_DYNAMIC_STORAGE,
            "{s} arrays cannot be used with {s}; dynamic storage is required",
            .{ storage_desc, usage },
        );
        self.fatal_error = true;
        return false;
    }

    // Instead of setting fatal_error = true immediately, collect errors
    // and continue analysis where possible
    /// The type an annotation names, resolved against the current record. The
    /// identity is also recorded on the annotation itself, so every later
    /// reader of the syntax sees the resolved type.
    pub fn typeExprToTypeInfo(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr) ErrorList!*ast.TypeInfo {
        try names.resolveTypeExpr(self, type_expr, self.current_module);
        return ast.typeInfoFromExpr(self.allocator, type_expr);
    }

    /// The value of a `const` initializer that is a constant expression;
    /// null for a `var`, whose value is never known at compile time, and for
    /// any other initializer. A fault is reported here.
    fn constantInitializer(self: *SemanticAnalyzer, init_expr: *const ast.Expr, is_const: bool) error{ConstantFault}!?TokenLiteral {
        if (!is_const) return null;
        return switch (consteval.evaluate(init_expr, ConstantNames{ .analyzer = self })) {
            .value => |value| value,
            .not_constant => null,
            .fault => |fault| {
                consteval.report(self.reporter, getLocationFromBase(fault.site.base), fault.fault);
                self.fatal_error = true;
                return error.ConstantFault;
            },
        };
    }

    /// What `consteval` asks of analysis: the constant a name holds, and the
    /// length of a fixed-size array.
    const ConstantNames = struct {
        analyzer: *SemanticAnalyzer,

        fn storageOf(self: ConstantNames, expr: *const ast.Expr) ?*Memory.ValueStorage {
            if (expr.data != .Variable) return null;
            const variable = (names.lookupVariable(self.analyzer, expr.data.Variable.lexeme) catch return null) orelse return null;
            return self.analyzer.memory.scope_manager.value_storage.get(variable.storage_id);
        }

        pub fn constantOf(self: ConstantNames, expr: *const ast.Expr) ?TokenLiteral {
            return (self.storageOf(expr) orelse return null).comptime_value;
        }

        pub fn fixedLengthOf(self: ConstantNames, expr: *const ast.Expr) ?i64 {
            const storage = self.storageOf(expr) orelse return null;
            if (storage.type_info.base != .Array) return null;
            return @intCast(storage.type_info.array_size orelse return null);
        }
    };

    fn validateFunctionBodyWithStruct(self: *SemanticAnalyzer, func: anytype, func_span: ast.SourceSpan, expected_return_type: ast.TypeInfo, enclosing: ?Enclosing) !void {
        // Create function scope with parameters
        const func_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

        // Add parameters to function scope
        for (func.params) |*param| {
            const param_type_info = if (param.type_expr) |type_expr|
                try self.typeExprToTypeInfo(type_expr)
            else
                try ast.TypeInfo.createDefault(self.allocator);

            // A default stands in for an argument the caller left out, so it
            // is typed against the parameter as an argument is.
            if (param.default_value) |default_value| {
                const default_type = try infer_type.inferTypeFromExpr(self, default_value);
                try helpers.unifyTypesExpr(self, param_type_info, default_type, default_value, .{ .location = getLocationFromBase(default_value.base) });
            }

            self.checkFreshName(func_scope, param.name);
            const param_var = func_scope.createValueBinding(
                param.name.lexeme,
                eval.convertTypeToTokenType(param_type_info.base),
                param_type_info,
                false, // Parameters are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        func_span.location,
                        ErrorCode.DUPLICATE_VARIABLE,
                        "Duplicate parameter name '{s}' in function '{s}'",
                        .{ param.name.lexeme, func.name.lexeme },
                    );
                    self.fatal_error = true;
                    return;
                } else {
                    return err;
                }
            };
            param_var.is_param = true;
            param_var.recordDeclLocation(param.name);
            param.storage = param_var.storage_id;
        }

        // The body's union `^` parameters, whose stores decide whether each
        // preserves its member.
        const outer_union_alias_params = self.union_alias_params;
        self.union_alias_params = std.AutoHashMap(u32, ParamRef).init(self.allocator);
        defer {
            self.union_alias_params.deinit();
            self.union_alias_params = outer_union_alias_params;
        }
        if (self.registeredSignature(func.name.lexeme, enclosing)) |signature| {
            try self.checked_signatures.put(signature, {});
            for (func.params, 0..) |param, i| {
                if (!param.is_alias or signature.params[i].base != .Union) continue;
                try self.union_alias_params.put(param.storage.?, .{ .signature = signature, .index = @intCast(i) });
            }
        }

        // Temporarily set current scope to function scope
        const prev_scope = self.current_scope;
        const prev_enclosing = self.enclosing;
        const prev_returning = self.returning;
        self.current_scope = func_scope;
        self.enclosing = enclosing;
        self.returning = .{ .function = func.name, .type = expected_return_type };
        defer self.returning = prev_returning;

        // Every `return` is checked against `expected_return_type` as the body
        // is validated; what is left is whether every path reaches one.
        try self.validateStatements(func.body);
        try self.validateReturnPaths(func.body, expected_return_type, func_span);

        // Restore previous scope and struct type
        self.current_scope = prev_scope;
        self.enclosing = prev_enclosing;
    }

    /// Report a function that declares a return type but has no path to a
    /// `return`. Return values are checked where each `return` is analyzed
    /// (`checkReturnValue`), not here.
    fn validateReturnPaths(self: *SemanticAnalyzer, body: []ast.Stmt, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!void {
        if (expected_return_type.base == .Nothing) return;
        if (try self.checkStatementsHaveReturns(body)) return;
        self.reporter.reportCompileError(
            func_span.location,
            ErrorCode.MISSING_RETURN_VALUE,
            "Function expects return value of type {s}; add an explicit return",
            .{@tagName(expected_return_type.base)},
        );
        self.fatal_error = true;
    }

    /// Infer a returned value and check it against the enclosing function's
    /// declared return type. A function without `returns` returns `nothing`,
    /// so a value returned from it is an error.
    pub fn checkReturnValue(self: *SemanticAnalyzer, value: *ast.Expr, location: Location) ErrorList!*ast.TypeInfo {
        const returning = self.returning orelse return infer_type.inferTypeFromExpr(self, value);
        const value_type = try infer_type.inferTypeIn(self, value, &returning.type);
        if (returning.type.base == .Nothing) {
            self.reporter.reportCompileError(
                location,
                ErrorCode.TYPE_MISMATCH,
                "'{s}' declares no `returns`, so it cannot return a value",
                .{returning.function.lexeme},
            );
            self.fatal_error = true;
            return value_type;
        }
        try self.validateReturnTypeCompatibility(&returning.type, value_type, value, .{ .location = location });
        return value_type;
    }

    /// A bare `return` returns `nothing`, so the function has to declare a
    /// return type that admits it: none at all, or a union with a `nothing`
    /// member. A function returns what it declares; nothing widens it.
    pub fn checkBareReturn(self: *SemanticAnalyzer, location: Location) void {
        const returning = self.returning orelse return;
        const declared = returning.type;
        if (admitsNothing(&declared)) return;
        self.reporter.reportCompileError(
            location,
            ErrorCode.TYPE_MISMATCH,
            "'{s}' returns {s}, so a bare `return` has no value to give; declare `returns nothing | {s}` to return nothing",
            .{ returning.function.lexeme, helpers.typeLabel(&declared), helpers.typeLabel(&declared) },
        );
        self.fatal_error = true;
    }

    fn checkStatementsHaveBreaks(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!bool {
        for (statements) |stmt| {
            switch (stmt.data) {
                .Break => return true,
                .Block => |block_stmts| {
                    if (try self.checkStatementsHaveBreaks(block_stmts)) return true;
                },
                .Expression => |expr| {
                    if (expr) |expression| {
                        if (try self.checkExpressionHasBreaks(expression)) return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    fn checkExpressionHasBreaks(self: *SemanticAnalyzer, expr: *ast.Expr) ErrorList!bool {
        switch (expr.data) {
            .Block => |block| {
                return self.checkStatementsHaveBreaks(block.statements);
            },
            .If => |if_data| {
                // Check both branches
                if (if_data.then_branch) |then_branch| {
                    if (try self.checkExpressionHasBreaks(then_branch)) return true;
                }
                if (if_data.else_branch) |else_branch| {
                    if (try self.checkExpressionHasBreaks(else_branch)) return true;
                }
                return false;
            },
            .Loop => |loop_data| {
                // Recursively check nested loops
                if (loop_data.body.data == .Block) {
                    return self.checkStatementsHaveBreaks(loop_data.body.data.Block.statements);
                } else {
                    return self.checkExpressionHasBreaks(loop_data.body);
                }
            },
            else => return false,
        }
    }

fn checkExpressionHasReturns(self: *SemanticAnalyzer, expr: *ast.Expr) ErrorList!bool {
            switch (expr.data) {
                .Block => |block| {
                    return self.checkStatementsHaveReturns(block.statements);
                },
                .If => |if_data| {
                    // Check both branches - if either has returns, the expression has returns
                    var has_returns = false;
                    if (if_data.then_branch) |then_branch| {
                        if (try self.checkExpressionHasReturns(then_branch)) has_returns = true;
                    }
                    if (if_data.else_branch) |else_branch| {
                        if (try self.checkExpressionHasReturns(else_branch)) has_returns = true;
                    }
                    return has_returns;
                },
                .Match => |match_expr| {
                    // A `match` carries returns when any arm does; the arms are
                    // where every path out of a `match`-shaped function returns.
                    for (match_expr.cases) |case| {
                        if (try self.checkExpressionHasReturns(case.body)) return true;
                    }
                    return false;
                },
                .Cast => |cast| {
                    // `x as T then .. else ..` is the narrowing form of a
                    // conditional: its branches are where those paths return.
                    if (cast.then_branch) |then_branch| {
                        if (try self.checkExpressionHasReturns(then_branch)) return true;
                    }
                    if (cast.else_branch) |else_branch| {
                        if (try self.checkExpressionHasReturns(else_branch)) return true;
                    }
                    return false;
                },
                .ReturnExpr => return true,
                .Unreachable => return true,
                .Loop => |loop_data| {
                    // For loops, check if the body contains return statements
                    if (loop_data.body.data == .Block) {
                        return self.checkStatementsHaveReturns(loop_data.body.data.Block.statements);
                    } else {
                        return self.checkExpressionHasReturns(loop_data.body);
                    }
                },
                else => return false,
            }
        }

    fn checkStatementsHaveReturns(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!bool {
        for (statements) |stmt| {
            switch (stmt.data) {
                .Return => return true,
                .Expression => |expr| {
                    if (expr) |expression| {
                        if (try self.checkExpressionHasReturns(expression)) return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    /// Whether a value of `t` may be `nothing`: `nothing` itself, or a union
    /// with a `nothing` member.
    fn admitsNothing(t: *const ast.TypeInfo) bool {
        return t.base == .Nothing or (t.base == .Union and unionHasNothing(t));
    }

    fn unionHasNothing(t: *const ast.TypeInfo) bool {
        const union_type = t.union_type orelse return false;
        for (union_type.types) |member| if (member.base == .Nothing) return true;
        return false;
    }

    /// Whether `expected` accepts `actual` as the same named type: an enum
    /// value matches a named enum, and either matches a group that contains it.
    /// This is what widens `nothing | error.IO` (an inline-Zig fallible return)
    /// into `nothing | error.StdError`, where the group's members are the enums.
    fn namedMemberAccepts(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *const ast.TypeInfo) bool {
        const expected_ref = switch (expected.custom_type orelse return false) {
            .ref => |r| r,
            .written => return false,
        };
        if (self.custom_types.get(expected_ref)) |custom| {
            if (custom.kind == .Enum and actual.base == .Enum) return true;
        }
        const actual_ref = switch (actual.custom_type orelse return false) {
            .ref => |r| r,
            .written => return false,
        };
        const group_id = self.group_table.idOf(expected_ref) orelse return false;
        const group_members = self.group_table.members(group_id) orelse return false;
        for (group_members) |member| {
            if (member.ref.eql(actual_ref)) return true;
        }
        return false;
    }

    fn validateReturnTypeCompatibility(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, actual_expr: ?*ast.Expr, span: ast.SourceSpan) !void {
        // `nothing` is returned only where the declared type admits it.
        if (actual.base == .Nothing or (actual.base == .Union and unionHasNothing(actual))) {
            if (!admitsNothing(expected)) {
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "this return can be `nothing`, which {s} does not admit; declare `returns nothing | {s}`",
                    .{ helpers.typeLabel(expected), helpers.typeLabel(expected) },
                );
                self.fatal_error = true;
                return;
            }
            if (actual.base == .Nothing) return;
        }
        // Handle union types for return type checking
        if (expected.base == .Union) {
            if (expected.union_type) |union_type| {

                // Check if actual type is a union
                if (actual.base == .Union) {
                    if (actual.union_type) |actual_union| {
                        // Check that every member of the actual union is compatible with the expected union
                        for (actual_union.types) |actual_member| {
                            // `nothing` was checked against the declared type above.
                            if (actual_member.base == .Nothing) continue;
                            var found_match = false;
                            for (union_type.types) |expected_member| {
                                if (actual_member.base == expected_member.base) {
                                    found_match = true;
                                    break;
                                }
                                // Allow enum literal (Enum) to match a Custom enum member in the expected union
                                if (expected_member.base == .Custom and actual_member.base == .Enum) {
                                    if (expected_member.custom_type) |custom| {
                                        if (self.custom_types.get(custom.resolved())) |ct| {
                                            if (ct.kind == .Enum) {
                                                found_match = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                                if (self.namedMemberAccepts(expected_member, actual_member)) {
                                    found_match = true;
                                    break;
                                }
                            }
                            if (!found_match) {
                                self.reporter.reportCompileError(
                                    span.location,
                                    ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
                                    "Return type {s} is not compatible with function's union return type",
                                    .{@tagName(actual_member.base)},
                                );
                                self.fatal_error = true;
                                return;
                            }
                        }
                        return; // All members are compatible
                    }
                } else {
                    // Single type - early accept for Nothing handled above
                    // Single type - check if it's a member of the expected union
                    var found_match = false;
                    for (union_type.types) |member_type| {
                        if (member_type.base == actual.base) {
                            found_match = true;
                            break;
                        }
                        // Allow enum literal (Enum) to match a Custom enum member in the expected union
                        if (member_type.base == .Custom and actual.base == .Enum) {
                            if (member_type.custom_type) |custom| {
                                if (self.custom_types.get(custom.resolved())) |ct| {
                                    if (ct.kind == .Enum) {
                                        found_match = true;
                                        break;
                                    }
                                }
                            }
                        }
                        if (self.namedMemberAccepts(member_type, actual)) {
                            found_match = true;
                            break;
                        }
                    }

                    if (!found_match) {
                        // Build a list of allowed types for the error message
                        var type_list = std.array_list.Managed(u8).init(self.allocator);
                        defer type_list.deinit();

                        for (union_type.types, 0..) |member_type, i| {
                            if (i > 0) try type_list.appendSlice(" | ");
                            try type_list.appendSlice(@tagName(member_type.base));
                        }

                        self.reporter.reportCompileError(
                            span.location,
                            ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
                            "Return type {s} is not compatible with function's union return type ({s})",
                            .{ @tagName(actual.base), type_list.items },
                        );
                        self.fatal_error = true;
                        return;
                    }
                }
            }
        } else {
            // Non-union expected type - use regular type unification
            try helpers.unifyTypesExpr(self, expected, actual, actual_expr, span);
        }
    }
};
