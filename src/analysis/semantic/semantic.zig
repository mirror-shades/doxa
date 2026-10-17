const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Memory = @import("../../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const Variable = Memory.Variable;

const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const StructTable = @import("../../common/struct_table.zig").StructTable;
const EnumTable = @import("../../common/enum_table.zig").EnumTable;
const GroupTable = @import("../../common/group_table.zig").GroupTable;
const UnionTable = @import("../../common/union_table.zig").UnionTable;
const TypeLowering = @import("../../common/type_lowering.zig").TypeLowering;

const Types = @import("../../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;

const TokenImport = @import("../../types/token.zig");
const Token = TokenImport.Token;

const helpers = @import("./helpers.zig");
const names = @import("./names.zig");
const eval = @import("eval_utils.zig");
const program = @import("program.zig");
const declarations = @import("declarations.zig");
const statements = @import("statements.zig");
const type_ops = @import("type_ops.zig");
const patterns = @import("patterns.zig");
const returns = @import("returns.zig");

const graph_mod = @import("../../module/graph.zig");
const ModuleGraph = graph_mod.ModuleGraph;
const ModuleId = graph_mod.ModuleId;
const ModuleLoader = @import("../../module/loader.zig").ModuleLoader;
const resolution = @import("resolution.zig");
const borrows = @import("borrows.zig");
const effects = @import("effects.zig");
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
    /// Granted by a statement immediately before it infers the expression
    /// whose value it discards; `inferTypeFromExpr` consumes it at entry, so
    /// every nested inference is a value position unless re-granted (an `if`
    /// re-grants its branches when it is itself discarded).
    discarded_grant: bool = false,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),
    /// What a store through a name writes and what the name reads there, per
    /// `Variable` expression, assignment and variable declaration.
    store_targets: std.AutoHashMap(NodeId, StoreTarget),
    /// The type of the value each owner holds for its `Held` reads: a
    /// `CompoundAssign`'s target, a `Loop`'s `over`. Keyed by owner id.
    held: std.AutoHashMap(NodeId, *ast.TypeInfo),
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
    /// The `each ... in ^path` borrows live where the analysis is, innermost
    /// last (`borrows.zig`).
    borrows: std.ArrayListUnmanaged(borrows.Borrow) = .empty,
    /// The signatures whose bodies were analyzed, so whose `member_mutators`
    /// verdicts are known. A loan through any other signature — a copy held
    /// by a function value — cannot be checked.
    checked_signatures: std.AutoHashMap(*const ast.FunctionType, void),
    /// What each body does to the storage it is handed: the parameters it
    /// writes through and the locals it lends (`effects.zig`).
    effects: effects.Effects = .{},
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
    /// Expressions whose type was incomplete when inferred (an array with no
    /// element type, say). A later context may still complete one in place;
    /// `reportIncompleteTypes` rejects the first that none did.
    incomplete: std.ArrayListUnmanaged(*const ast.Expr) = .empty,
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

    // Method modules: the analyzer's state stays here; each family of rules
    // lives in its own file (the same pattern as `codegen/hir/generator.zig`).
    pub const analyzeProgram = program.analyzeProgram;
    pub const finalizeLinkIdentities = program.finalizeLinkIdentities;
    pub const ensureTypes = program.ensureTypes;
    pub const closeBlockScope = program.closeBlockScope;
    pub const validateStatements = statements.validateStatements;
    pub const ensureDynamicArrayStorage = type_ops.ensureDynamicArrayStorage;
    pub const typeExprToTypeInfo = type_ops.typeExprToTypeInfo;
    pub const resolveMatchPatterns = patterns.resolveMatchPatterns;
    pub const inferMatchCaseTypeWithNarrow = patterns.inferMatchCaseTypeWithNarrow;
    pub const groupMemberType = patterns.groupMemberType;
    pub const checkFreshName = names.checkFreshName;
    pub const checkReturnValue = returns.checkReturnValue;
    pub const checkBareReturn = returns.checkBareReturn;

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
            .held = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
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
        self.held.deinit();
        self.member_mutators.deinit();
        self.member_relays.deinit(self.allocator);
        self.member_loans.deinit(self.allocator);
        self.borrows.deinit(self.allocator);
        self.union_alias_params.deinit();
        self.checked_signatures.deinit();
        self.effects.deinit(self.allocator);
        self.bare_variants.deinit(self.allocator);
        self.incomplete.deinit(self.allocator);
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

    pub fn isEnumTypeRequiringInitializer(self: *SemanticAnalyzer, type_info: *const ast.TypeInfo) bool {
        return eval.isEnumTypeRequiringInitializer(type_info, self);
    }

    // ── The per-record driver ───────────────────────────────────────────────

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

    /// The module scope of `id`: the root of everything declared in that file.
    /// It has no parent — a record never sees another file's names except
    /// through its bindings.
    pub fn moduleScope(self: *const SemanticAnalyzer, id: ModuleId) *Scope {
        return self.module_scopes.get(id).?;
    }
};
