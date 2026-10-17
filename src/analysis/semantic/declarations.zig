//! Declaration collection and registration: types, functions, globals, and struct members.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Memory = @import("../../utils/memory.zig");
const Scope = Memory.Scope;
const Variable = Memory.Variable;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const helpers = @import("helpers.zig");
const names = @import("names.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("eval_utils.zig");
const infer_type = @import("infer_type.zig");
const Types = @import("../../types/types.zig");
const TokenLiteral = Types.TokenLiteral;
const TokenImport = @import("../../types/token.zig");
const TokenType = TokenImport.TokenType;
const Token = TokenImport.Token;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const StructMethodInfo = @import("semantic.zig").StructMethodInfo;
const type_ops = @import("type_ops.zig");
const returns = @import("returns.zig");

/// A module namespace (`std`, `std.io`) is a compile-time construct, not a
/// value, so it cannot be bound to a variable. Detect `const x is std.io`
/// and report a clear error steering the user toward `import`. Returns true
/// when a namespace binding was detected and reported.
pub fn reportNamespaceBinding(self: *SemanticAnalyzer, decl: anytype, base: ast.Base) ErrorList!bool {
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
pub fn registerStructMethods(self: *SemanticAnalyzer, sd: *ast.StructDecl, ref: TypeRef) ErrorList!void {
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
            .signature = try methodSignature(self, m, return_type),
        });
    }
    try self.struct_methods.put(ref, method_table);
}

/// The method's callable signature: its resolved parameter types and alias
/// flags, joined to its return type. An unannotated parameter resolves to
/// `Nothing`, which `validateFunctionCallArguments` reads as "accepts
/// anything" — the same treatment an unannotated function parameter gets.
pub fn methodSignature(self: *SemanticAnalyzer, m: *ast.StructMethod, return_type: *ast.TypeInfo) ErrorList!*ast.FunctionType {
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

/// Register the declarations of `statements` in `scope`.
///
/// At a record's top level (its module scope) this is the record's Types
/// stage: types are registered by `TypeRef` first, so every signature and
/// initializer can name them; then functions with their resolved
/// signatures, so calls see them before their bodies are checked; then
/// globals in order. A nested block registers its locals the same way. A
/// type is declared only at file scope: elsewhere it would have no binding,
/// and two blocks declaring the same name would share one identity.
/// The group `stmt` declares, if it declares one.
fn groupDecl(stmt: *ast.Stmt) ?*ast.GroupDecl {
    return switch (stmt.data) {
        .GroupDecl => |*decl| decl,
        .Expression => |maybe_expr| {
            const expr = maybe_expr orelse return null;
            return if (expr.data == .GroupDecl) &expr.data.GroupDecl else null;
        },
        else => null,
    };
}

pub fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
    const at_file_scope = scope == self.moduleScope(self.current_module);

    // Types: structs and enums first, then groups, which flatten their
    // member types.
    for (statements) |*stmt| {
        switch (stmt.data) {
            .Expression => |maybe_expr| if (maybe_expr) |expr| switch (expr.data) {
                .StructDecl => |*decl| try declareStruct(self, decl, scope, at_file_scope, stmt.base),
                .EnumDecl => |*decl| try declareEnum(self, decl, scope, at_file_scope, stmt.base),
                else => {},
            },
            .EnumDecl => |*decl| try declareEnum(self, decl, scope, at_file_scope, stmt.base),
            else => {},
        }
    }
    // Every group is declared before any is flattened, so a group may
    // include one declared below it; flattening keeps declaration order, so
    // group ids do too.
    for (statements) |*stmt| {
        const decl = groupDecl(stmt) orelse continue;
        if (!at_file_scope) {
            try reportNestedDeclaration(self, "Type", decl.name);
            continue;
        }
        try helpers.declareGroupType(self, .{ .module = self.current_module, .name = decl.name.lexeme }, decl.members);
    }
    if (at_file_scope) for (statements) |*stmt| {
        const decl = groupDecl(stmt) orelse continue;
        try helpers.flattenGroupType(self, .{ .module = self.current_module, .name = decl.name.lexeme });
    };

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
                if (param.is_alias) try checkAliasParameterType(self, param, param_types[i], location);
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
                if (try reportNamespaceBinding(self, decl, stmt.base)) continue;
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
                            try type_ops.resolveArraySizes(self, type_info, type_expr);
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
                    type_info.* = try type_ops.deepCopyTypeInfo(self, inferred.*);
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
                    type_info.* = .{ .base = .Error, .is_mutable = decl.type_info.is_mutable };
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
                        // Still declared, as poison, so its uses report nothing more.
                        type_info.* = .{ .base = .Error, .is_mutable = decl.type_info.is_mutable };
                    }
                    type_ops.tryTagConstLiteralArray(self, type_info, init_expr);
                }

                var comptime_value: ?TokenLiteral = null;
                if (decl.initializer) |init_expr| {
                    // An annotated initializer is typed in its context.
                    if (decl.type_info.base != .Nothing) _ = try infer_type.inferTypeIn(self, init_expr, type_info);
                    comptime_value = type_ops.constantInitializer(self, init_expr, !decl.type_info.is_mutable) catch continue;
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
                    scope.createValueBindingAt(decl.name.lexeme, type_info, !type_info.is_mutable, decl.name)
                else
                    scope.createValueBinding(decl.name.lexeme, type_info, !type_info.is_mutable);
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
                    else => {
                        self.discarded_grant = true;
                        _ = try infer_type.inferTypeFromExpr(self, expr);
                    },
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

/// An alias parameter must name a concrete type a caller can lend:
/// functions, and anonymous structs or enums cannot be aliased.
pub fn checkAliasParameterType(self: *SemanticAnalyzer, param: ast.FunctionParam, param_type: ast.TypeInfo, location: Location) ErrorList!void {
    const disallowed = switch (param_type.base) {
        .Function, .Struct, .Enum => true,
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
pub fn declareStruct(self: *SemanticAnalyzer, decl: *ast.StructDecl, scope: *Scope, at_file_scope: bool, base: ast.Base) ErrorList!void {
    if (!at_file_scope) return reportNestedDeclaration(self, "Type", decl.name);
    const ref = TypeRef{ .module = self.current_module, .name = decl.name.lexeme };

    const fields = try self.allocator.alloc(ast.StructFieldType, decl.fields.len);
    for (decl.fields, fields) |field, *struct_field| {
        struct_field.* = .{
            .name = field.name.lexeme,
            .type_info = try fieldStorageType(self, try self.typeExprToTypeInfo(field.type_expr)),
            .is_public = field.is_public,
        };
    }
    try helpers.registerStructType(self, ref, fields);

    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .struct_fields = fields, .is_mutable = false };
    try bindTypeName(self, scope, decl.name, type_info, base);
    try checkStructMemberNames(self, decl);
    try registerStructMethods(self, decl, ref);
}

/// The type a struct field written as `type` holds. A field is one word
/// of its struct, so an array field holds a dynamic array: a fixed array
/// stored into it is converted (`array.from_fixed`), and the field reads
/// as `T[]` (`plan/register-hir.md`, Q2).
pub fn fieldStorageType(self: *SemanticAnalyzer, written: *ast.TypeInfo) ErrorList!*ast.TypeInfo {
    if (written.base != .Array) return written;
    const element = written.array_type orelse return written;
    const out = try self.allocator.create(ast.TypeInfo);
    out.* = written.*;
    out.array_size = null;
    out.array_storage = .dynamic;
    out.array_type = try fieldStorageType(self, element);
    return out;
}

/// A struct names each member once: no two fields, no two methods or
/// functions, and no method or function sharing a field's name.
pub fn checkStructMemberNames(self: *SemanticAnalyzer, decl: *ast.StructDecl) ErrorList!void {
    var seen = std.StringHashMap(void).init(self.allocator);
    defer seen.deinit();
    for (decl.fields) |field| try claimStructMember(self, &seen, decl.name, field.name);
    for (decl.methods) |method| try claimStructMember(self, &seen, decl.name, method.name);
}

pub fn claimStructMember(self: *SemanticAnalyzer, seen: *std.StringHashMap(void), struct_name: Token, member: Token) ErrorList!void {
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
pub fn declareEnum(self: *SemanticAnalyzer, decl: *ast.EnumDecl, scope: *Scope, at_file_scope: bool, base: ast.Base) ErrorList!void {
    if (!at_file_scope) return reportNestedDeclaration(self, "Type", decl.name);
    const ref = TypeRef{ .module = self.current_module, .name = decl.name.lexeme };

    const variant_names = try self.allocator.alloc([]const u8, decl.variants.len);
    for (decl.variants, variant_names) |variant, *name| name.* = variant.lexeme;
    try helpers.registerEnumType(self, ref, variant_names);

    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false };
    try bindTypeName(self, scope, decl.name, type_info, base);
}

pub fn bindTypeName(self: *SemanticAnalyzer, scope: *Scope, name: Token, type_info: *ast.TypeInfo, base: ast.Base) ErrorList!void {
    const variable = scope.createValueBinding(name.lexeme, type_info, true) catch |err| {
        if (err != error.DuplicateVariableName) return err;
        self.reporter.reportCompileError(getLocationFromBase(base), ErrorCode.DUPLICATE_VARIABLE, "Duplicate type name '{s}' in current scope", .{name.lexeme});
        self.fatal_error = true;
        return;
    };
    variable.recordDeclLocation(name);
}

/// Types and functions are declared at file scope only.
pub fn reportNestedDeclaration(self: *SemanticAnalyzer, comptime kind: []const u8, name: Token) ErrorList!void {
    self.reporter.reportCompileError(
        ast.SourceSpan.fromToken(name).location,
        ErrorCode.NESTED_DECLARATION,
        kind ++ " '{s}' must be declared at file scope",
        .{name.lexeme},
    );
    self.fatal_error = true;
}
