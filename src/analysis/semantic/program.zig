//! The per-record driver: program analysis, module scopes, stage callbacks, and unused-name reporting.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Memory = @import("../../utils/memory.zig");
const Scope = Memory.Scope;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const helpers = @import("helpers.zig");
const names = @import("names.zig");
const graph_mod = @import("../../module/graph.zig");
const ModuleRecord = graph_mod.ModuleRecord;
const ModuleId = graph_mod.ModuleId;
const effects = @import("effects.zig");
const Types = @import("../../types/types.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const Enclosing = SemanticAnalyzer.Enclosing;
const Returning = SemanticAnalyzer.Returning;
const zig_interop = @import("zig_interop.zig");
const declarations = @import("declarations.zig");

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
/// Analyze the program: the entry record, then every record a reference
/// materialized (its declarations were collected), until none is left. A
/// record that is only interned — named by a `module` alias nothing reads
/// through — is not part of the program.
pub fn analyzeProgram(self: *SemanticAnalyzer) ErrorList!void {
    const entry_scope = try ensureModuleScope(self, self.entry_module);
    self.memory.scope_manager.root_scope = entry_scope;

    try ensureAnalyzed(self, self.entry_module);
    var progressed = true;
    while (progressed) {
        progressed = false;
        var i: usize = 0;
        while (i < self.graph.records.items.len) : (i += 1) {
            const record = self.graph.records.items[i];
            if (!record.status.atLeast(.DeclarationsCollected)) continue;
            if (record.status.atLeast(.Analyzed)) continue;
            try ensureAnalyzed(self, record.id);
            progressed = true;
        }
    }
    try settleMemberLoans(self);
    try self.effects.settle(self.allocator, &self.checked_signatures);
    if (self.fatal_error) return error.SemanticError;

    try reportUnused(self, entry_scope);
    reportUnusedImports(self);
}

/// Every body is analyzed, so whether a union `^` parameter preserves its
/// member is known: a parameter lent on to one that stores another member
/// stores it too. A member-typed variable lent to a parameter that does
/// not preserve its member is an error.
pub fn settleMemberLoans(self: *SemanticAnalyzer) ErrorList!void {
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
pub fn registeredSignature(self: *SemanticAnalyzer, name: []const u8, enclosing: ?Enclosing) ?*const ast.FunctionType {
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
pub fn ensureAnalyzed(self: *SemanticAnalyzer, id: ModuleId) ErrorList!void {
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

pub fn typesStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
    const self: *SemanticAnalyzer = @ptrCast(@alignCast(ctx));
    const saved = try enterModule(self, record.id);
    defer leaveModule(self, saved);
    return switch (record.kind) {
        .doxa => declarations.collectDeclarations(self, record.statements(), self.moduleScope(record.id)),
        .inline_zig, .zig_file => zig_interop.registerZigFunctions(self, record),
    };
}

pub fn analysisStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
    const self: *SemanticAnalyzer = @ptrCast(@alignCast(ctx));
    if (record.kind != .doxa) return;
    const saved = try enterModule(self, record.id);
    defer leaveModule(self, saved);
    self.analyzing = true;
    defer self.analyzing = false;
    const result = self.validateStatements(record.statements());
    reportUntypedVariants(self);
    return result;
}

/// A `.Variant` shorthand is typed only by an explicit context: a
/// parameter, an annotated variable, an assignment, a return, or the other
/// side of a comparison. Nothing is searched; one its context left untyped
/// is an error where it is written.
pub fn reportUntypedVariants(self: *SemanticAnalyzer) void {
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

pub fn enterModule(self: *SemanticAnalyzer, id: ModuleId) ErrorList!Context {
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
    self.current_scope = try ensureModuleScope(self, id);
    self.enclosing = null;
    self.returning = null;
    self.current_initializing_var = null;
    self.block_value_expected = false;
    self.in_loop_scope = false;
    self.analyzing = false;
    return saved;
}

pub fn leaveModule(self: *SemanticAnalyzer, saved: Context) void {
    self.current_module = saved.module;
    self.current_scope = saved.scope;
    self.enclosing = saved.enclosing;
    self.returning = saved.returning;
    self.current_initializing_var = saved.initializing_var;
    self.block_value_expected = saved.block_value_expected;
    self.in_loop_scope = saved.in_loop_scope;
    self.analyzing = saved.analyzing;
}

pub fn ensureModuleScope(self: *SemanticAnalyzer, id: ModuleId) ErrorList!*Scope {
    if (self.module_scopes.get(id)) |scope| return scope;
    const scope = try self.memory.scope_manager.createScope(null, self.memory);
    try self.module_scopes.put(id, scope);
    return scope;
}

pub fn reportUnused(self: *SemanticAnalyzer, scope: *Scope) ErrorList!void {
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
        try reportUnused(self, child);
    }
}

/// An `import` in the entry file whose name is never referenced. Unused
/// reporting is a statement about the unit being compiled, so a library's
/// imports are not reported.
pub fn reportUnusedImports(self: *SemanticAnalyzer) void {
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
