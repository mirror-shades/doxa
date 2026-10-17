//! Statement validation: declarations, expressions, returns, and control flow at statement level.

const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Memory = @import("../../utils/memory.zig");
const Variable = Memory.Variable;
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
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const declarations = @import("declarations.zig");
const type_ops = @import("type_ops.zig");
const returns = @import("returns.zig");

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
                    type_ops.tryResolveArraySizes(self, &statements[i].data.VarDecl.type_info, te);
                }
                // A module namespace is not a value and cannot be bound to a variable.
                if (try declarations.reportNamespaceBinding(self, statements[i].data.VarDecl, stmt.base)) continue;
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
                        comptime_value = type_ops.constantInitializer(self, init_expr, !decl.type_info.is_mutable) catch continue;
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
                                try declarations.reportNestedDeclaration(self, "Type", struct_decl.name);
                                continue;
                            }
                            const ref = TypeRef{ .module = self.current_module, .name = struct_decl.name.lexeme };
                            for (struct_decl.methods) |method| {
                                try returns.validateFunctionBodyWithStruct(
                                    self,
                                    method,
                                    .{ .location = getLocationFromBase(expression.base) },
                                    method.return_type_info,
                                    .{ .ref = ref, .has_this = !method.is_static },
                                );
                            }
                        },
                        .EnumDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                            try declarations.reportNestedDeclaration(self, "Type", decl.name);
                        },
                        .GroupDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                            try declarations.reportNestedDeclaration(self, "Type", decl.name);
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
                    try declarations.reportNestedDeclaration(self, "Function", func.name);
                    continue;
                }
                const location = ast.SourceSpan{ .location = getLocationFromBase(stmt.base) };
                if (self.function_return_types.get(stmt.base.id)) |return_type| {
                    try returns.validateFunctionBodyWithStruct(self, func, location, return_type.*, null);
                } else {
                    try returns.validateFunctionBodyWithStruct(self, func, location, func.return_type_info, null);
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
                try declarations.reportNestedDeclaration(self, "Type", decl.name);
            },
            .GroupDecl => |decl| if (self.current_scope != self.moduleScope(self.current_module)) {
                try declarations.reportNestedDeclaration(self, "Type", decl.name);
            },
            .Assert => |assertion| {
                _ = try infer_type.inferTypeFromExpr(self, assertion.condition);
                if (assertion.message) |message| _ = try infer_type.inferTypeFromExpr(self, message);
            },
            .ZigDecl, .Import => {},
        }
    }
}
