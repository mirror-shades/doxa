//! Name resolution: the one model by which semantic analysis turns a spelling
//! into an entity.
//!
//! A bare name resolves through the current scope chain (locals, then the
//! record's own top-level declarations in its module scope) and then through
//! the current record's bindings — imports and namespace aliases. There is no
//! global fallback. A qualified chain steps from a binding through public
//! surfaces only. Whatever a name resolves to outside the local scope is
//! recorded as a `Resolution` on the expression, so codegen never resolves a
//! spelling again.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const graph_mod = @import("../../module/graph.zig");
const ModuleId = graph_mod.ModuleId;
const SymbolRef = graph_mod.SymbolRef;
const BoundName = graph_mod.BoundName;
const Memory = @import("../../utils/memory.zig");
const Variable = Memory.Variable;
const Scope = Memory.Scope;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const eval = @import("eval_utils.zig");
const Resolution = @import("resolution.zig").Resolution;

/// The variable `name` denotes in the current record, or null. A namespace is
/// not a value and yields null; callers that accept one ask `namespaceOf`.
pub fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ErrorList!?*Variable {
    return (try resolveName(self, name)).variable;
}

pub const NameResult = struct {
    variable: ?*Variable = null,
    /// Set when the name denotes a module-level entity: a top-level
    /// declaration of the current record or anything reached through its
    /// bindings.
    symbol: ?SymbolRef = null,
};

/// Resolve a bare name to its variable and, for a module-level entity, its
/// defining symbol. A narrowing view is the variable it narrows: the view
/// supplies the type, the binding beneath every view the identity.
pub fn resolveName(self: *SemanticAnalyzer, name: []const u8) ErrorList!NameResult {
    if (self.current_scope) |scope| {
        if (scope.lookupVariable(name)) |variable| {
            scope.markUsed(name);
            const binding = underlyingBinding(scope, name) orelse
                // A view of a name no scope binds: an imported global.
                return .{ .variable = variable, .symbol = (try resolveBinding(self, name)).symbol };
            const module_scope = self.moduleScope(self.current_module);
            if (module_scope.lookupLocalVariable(name) != binding) return .{ .variable = variable };
            const bound = self.graph.record(self.current_module).bindings.get(name);
            return .{
                .variable = variable,
                .symbol = if (bound) |b| switch (b.binding) {
                    .symbol => |symbol| symbol,
                    .namespace => null,
                } else null,
            };
        }
    }
    return resolveBinding(self, name);
}

/// The nearest binding of `name` in the scope chain that is not a view.
fn underlyingBinding(scope: *Scope, name: []const u8) ?*Variable {
    var current: ?*Scope = scope;
    while (current) |s| : (current = s.parent) {
        if (s.lookupLocalVariable(name)) |variable| if (!variable.is_view) return variable;
    }
    return null;
}

/// A bare name the current record binds outside every scope: an import.
fn resolveBinding(self: *SemanticAnalyzer, name: []const u8) ErrorList!NameResult {
    const bound = self.graph.record(self.current_module).bindings.get(name) orelse return .{};
    const symbol = switch (bound.binding) {
        .symbol => |symbol| symbol,
        .namespace => return .{},
    };
    // An own declaration not yet in the module scope is a forward reference
    // to a later global, which a declaration cannot see.
    if (symbol.module == self.current_module) return .{};
    markUsed(self, self.current_module, name);
    return .{ .variable = try declaredVariable(self, symbol), .symbol = symbol };
}

/// Resolve a `Variable` expression and record what it denotes when it names a
/// module-level entity — the record's own globals included, which become
/// global sites. Locals are left to codegen's own storage tracking.
pub fn resolveVariableExpr(self: *SemanticAnalyzer, expr: *ast.Expr) ErrorList!?*Variable {
    const result = try resolveName(self, expr.data.Variable.lexeme);
    if (result.symbol) |symbol| {
        const resolved = symbolResolution(self, symbol).?;
        try annotate(self, expr, resolved);
        if (resolved == .global) {
            try self.recordGlobalSite(.{ .symbol = symbol, .place = .{ .name = &expr.data.Variable } });
        }
    }
    return result.variable;
}

/// Resolve the target of an assignment (`x is ...`, `x += ...`). A
/// module-level global is recorded as a global site.
pub fn resolveAssignmentTarget(self: *SemanticAnalyzer, target: *ast.Token) ErrorList!?*Variable {
    const result = try resolveName(self, target.lexeme);
    if (result.symbol) |symbol| {
        if (symbol.kind == .Variable or symbol.kind == .Constant) {
            try self.recordGlobalSite(.{ .symbol = symbol, .place = .{ .name = target } });
        }
    }
    return result.variable;
}

/// The resolution a module-level symbol earns at a use site.
pub fn symbolResolution(self: *SemanticAnalyzer, symbol: SymbolRef) ?Resolution {
    return switch (symbol.kind) {
        .Function => if (self.graph.record(symbol.module).kind == .doxa)
            .{ .function = symbol }
        else
            .{ .zig_function = symbol },
        .Type => .{ .type = symbol.typeRef() },
        .Variable, .Constant => .{ .global = symbol },
    };
}

/// The variable a module-level symbol is bound to in its defining record's
/// module scope. The defining record's types are registered first; a record
/// whose registration is active (a declaration cycle) yields its placeholders.
pub fn declaredVariable(self: *SemanticAnalyzer, symbol: SymbolRef) ErrorList!?*Variable {
    try self.ensureTypes(symbol.module);
    return self.moduleScope(symbol.module).lookupLocalVariable(symbol.name);
}

/// The record a pure module path denotes (`std`, `std.io`), or null. The
/// expression and each namespace step on the way are recorded as namespace
/// resolutions. A local of the same name shadows a namespace alias.
pub fn namespaceOf(self: *SemanticAnalyzer, expr: *ast.Expr) ErrorList!?ModuleId {
    switch (expr.data) {
        .Variable => |tok| {
            if (self.current_scope) |scope| {
                if (scope.lookupVariable(tok.lexeme) != null) return null;
            }
            const bound = self.graph.record(self.current_module).bindings.get(tok.lexeme) orelse return null;
            switch (bound.binding) {
                .namespace => |id| {
                    markUsed(self, self.current_module, tok.lexeme);
                    try annotate(self, expr, .{ .namespace = id });
                    return id;
                },
                .symbol => return null,
            }
        },
        .FieldAccess => |field| {
            const parent = (try namespaceOf(self, field.object)) orelse return null;
            const bound = (try self.loader.member(parent, field.field.lexeme)) orelse return null;
            switch (bound.binding) {
                .namespace => |id| {
                    try annotate(self, expr, .{ .namespace = id });
                    return id;
                },
                .symbol => return null,
            }
        },
        else => return null,
    }
}

/// What `namespace` publicly binds under `name`, or a located error naming
/// the namespace and the missing member.
pub fn member(self: *SemanticAnalyzer, namespace: ModuleId, name: ast.Token) ErrorList!BoundName {
    return (try self.loader.member(namespace, name.lexeme)) orelse {
        self.reporter.reportCompileError(
            ast.SourceSpan.fromToken(name).location,
            ErrorCode.FIELD_NOT_FOUND,
            "'{s}' has no public declaration '{s}'",
            .{ self.graph.moduleName(namespace), name.lexeme },
        );
        self.fatal_error = true;
        return error.FieldNotFound;
    };
}

/// The type a written name denotes in `module`: a type declared or imported
/// there, or a type reached through a namespace chain (`std.json.Node`). The
/// defining record's types are registered first, so every `TypeRef` analysis
/// hands out names a registered type (or, inside a declaration cycle, its
/// placeholder).
pub fn resolveTypeName(self: *SemanticAnalyzer, written: []const u8, module: ModuleId) ErrorList!?ast.TypeRef {
    var segments = std.mem.splitScalar(u8, written, '.');
    const first = segments.first();
    var bound = self.graph.record(module).bindings.get(first) orelse return null;
    markUsed(self, module, first);
    while (segments.next()) |segment| {
        const namespace = switch (bound.binding) {
            .namespace => |id| id,
            .symbol => return null,
        };
        bound = (try self.loader.member(namespace, segment)) orelse return null;
    }
    const symbol = switch (bound.binding) {
        .symbol => |symbol| symbol,
        .namespace => return null,
    };
    if (symbol.kind != .Type) return null;
    try self.ensureTypes(symbol.module);
    return symbol.typeRef();
}

/// Resolve every written type name in `type_info` against `module`, in place.
/// Declarations and annotations are resolved where they live, so codegen reads
/// identities straight off the AST.
pub fn resolveTypeInfo(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, module: ModuleId, location: ?Location) ErrorList!void {
    if (type_info.custom_type) |custom| switch (custom) {
        .written => |name| type_info.custom_type = .{ .ref = try requireType(self, name, module, location) },
        .ref => {},
    };
    if (type_info.array_type) |element| try resolveTypeInfo(self, element, module, location);
    if (type_info.struct_fields) |fields| {
        for (fields) |field| try resolveTypeInfo(self, field.type_info, module, location);
    }
    if (type_info.function_type) |function_type| {
        for (function_type.params) |*param| try resolveTypeInfo(self, param, module, location);
        try resolveTypeInfo(self, function_type.return_type, module, location);
    }
    if (type_info.union_type) |union_type| {
        for (union_type.types) |member_type| try resolveTypeInfo(self, member_type, module, location);
    }
    if (type_info.map_key_type) |key| try resolveTypeInfo(self, key, module, location);
    if (type_info.map_value_type) |value| try resolveTypeInfo(self, value, module, location);
}

/// Resolve every named type in an annotation against `module`, recording the
/// identity on the node.
pub fn resolveTypeExpr(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr, module: ModuleId) ErrorList!void {
    switch (type_expr.data) {
        .Custom => |*custom| {
            if (custom.ref == null) {
                custom.ref = try requireType(self, custom.name.lexeme, module, ast.SourceSpan.fromToken(custom.name).location);
            }
        },
        .Array => |array| try resolveTypeExpr(self, array.element_type, module),
        .Struct => |fields| for (fields) |field| try resolveTypeExpr(self, field.type_expr, module),
        .Union => |types| for (types) |member_type| try resolveTypeExpr(self, member_type, module),
        .Map => |map| {
            if (map.key_type) |key| try resolveTypeExpr(self, key, module);
            try resolveTypeExpr(self, map.value_type, module);
        },
        .Basic, .Enum => {},
    }
}

fn requireType(self: *SemanticAnalyzer, name: []const u8, module: ModuleId, location: ?Location) ErrorList!ast.TypeRef {
    return (try resolveTypeName(self, name, module)) orelse {
        self.reporter.reportCompileError(location, ErrorCode.UNKNOWN_TYPE, "Unknown type '{s}'", .{name});
        self.fatal_error = true;
        return error.UndefinedType;
    };
}

/// Record what `expr` denotes. A node is resolved once; a second, different
/// resolution of the same node is a compiler bug.
pub fn annotate(self: *SemanticAnalyzer, expr: *const ast.Expr, resolution: Resolution) ErrorList!void {
    try self.resolutions.put(expr.base.id, resolution);
}

/// A binding of `module` was referenced: it is not an unused import, and a
/// declaration of `module` by that name is not unused either.
pub fn markUsed(self: *SemanticAnalyzer, module: ModuleId, name: []const u8) void {
    self.used_bindings.put(.{ .module = module, .name = name }, {}) catch {};
    if (self.module_scopes.get(module)) |scope| scope.markUsed(name);
}

/// The closest visible name to an undefined `name`, for a "did you mean"
/// hint: scope names first, then the current record's bindings.
pub fn suggestName(self: *SemanticAnalyzer, name: []const u8) ?[]const u8 {
    var best: ?[]const u8 = null;
    var best_score: usize = std.math.maxInt(usize);

    var scope = self.current_scope;
    while (scope) |s| : (scope = s.parent) {
        var it = s.name_map.keyIterator();
        while (it.next()) |candidate| eval.updateBestSuggestion(name, candidate.*, &best, &best_score);
    }
    var bindings = self.graph.record(self.current_module).bindings.keyIterator();
    while (bindings.next()) |candidate| eval.updateBestSuggestion(name, candidate.*, &best, &best_score);

    const suggested = best orelse return null;
    const max_acceptable = @max(@as(usize, 2), name.len / 3);
    return if (best_score <= max_acceptable) suggested else null;
}
