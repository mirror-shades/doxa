const std = @import("std");
const HIRGenerator = @import("soxa_generator.zig").HIRGenerator;
const ast = @import("../../ast/ast.zig");
const Location = @import("../../utils/reporting.zig").Location;
const ErrorList = @import("../../utils/errors.zig").ErrorList;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const HIRType = @import("soxa_generator.zig").HIRType;
const HIRValue = @import("soxa_generator.zig").HIRValue;
const HIRInstruction = @import("soxa_generator.zig").HIRInstruction;
const HIRMapEntry = @import("soxa_generator.zig").HIRMapEntry;
const SoxaTypes = @import("soxa_types.zig");
const ScopeKind = SoxaTypes.ScopeKind;

fn isLiteralExpression(expr: *ast.Expr) bool {
    return switch (expr.data) {
        .Literal => true,
        else => false,
    };
}

const ArrayElementInfo = struct {
    element_type: HIRType,
    nested_element_type: ?HIRType,
};

fn resolveArrayElementInfo(self: *HIRGenerator, element_info: ?*const ast.TypeInfo) ArrayElementInfo {
    if (element_info) |info| {
        const element_type = self.convertTypeInfo(info.*);
        const nested = SoxaTypes.arrayInnermostElementType(element_type);
        return .{ .element_type = element_type, .nested_element_type = nested };
    }

    return .{ .element_type = .Unknown, .nested_element_type = null };
}

const NestedSizes = struct {
    sizes: [4]u32 = [_]u32{0} ** 4,
    depth: u3 = 0,
    truncated: bool = false,
};

fn collectNestedSizes(array_type: ?*const ast.TypeInfo) NestedSizes {
    var result = NestedSizes{};
    var cursor = array_type;
    while (cursor) |info| {
        if (info.base == .Array and info.array_storage == .fixed) {
            if (info.array_size) |s| {
                if (result.depth < 4) {
                    result.sizes[result.depth] = @intCast(s);
                    result.depth += 1;
                } else {
                    result.truncated = true;
                }
            }
            cursor = info.array_type;
        } else {
            break;
        }
    }
    return result;
}

pub fn generateStatement(self: *HIRGenerator, stmt: ast.Stmt) (std.mem.Allocator.Error || ErrorList)!void {
    switch (stmt.data) {
        .Defer => |expr| {
            if (self.deferred_stack.items.len > 0) {
                try self.deferred_stack.items[self.deferred_stack.items.len - 1]
                    .actions.append(.{ .expr = expr });
            } else {
                try self.generateExpression(expr, false, false);
            }
        },
        .Lift => |lift_data| {
            // TODO: lift outside a block context should be a semantic error reported earlier
            try self.generateExpression(lift_data.value, false, true);
        },
        .ZigDecl => {
            // Inline zig modules are handled outside of normal statement lowering.
            // They only contribute external/module-callable functions.
        },
        // TODO: binary expressions don't pop their result when should_pop_after_use=true,
        // causing stack leaks when binary expressions appear as discarded statements in blocks
        .Expression => |expr| {
            if (expr) |e| {
                // Do not treat a trailing expression as an implicit function return.
                // Always generate as a regular expression statement here.
                try self.generateExpression(e, false, true);
            }
        },
        .Continue => {
            if (self.currentLoopContext()) |lc| {
                if (self.loop_deferred_boundaries.items.len > 0) {
                    const boundary = self.loop_deferred_boundaries.items[self.loop_deferred_boundaries.items.len - 1];
                    try self.emitDeferredToBoundary(boundary);
                }
                try self.instructions.append(.{ .Jump = .{ .label = lc.continue_label } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
                    .file_uri = stmt.base.location().file_uri,
                    .range = .{
                        .start_line = stmt.base.location().range.start_line,
                        .start_col = stmt.base.location().range.start_col,
                        .end_line = stmt.base.location().range.end_line,
                        .end_col = stmt.base.location().range.end_col,
                    },
                };
                self.reporter.reportCompileError(
                    location,
                    ErrorCode.CONTINUE_USED_OUTSIDE_OF_LOOP,
                    "'continue' used outside of a loop",
                    .{},
                );
            }
        },
        .Break => {
            if (self.currentLoopContext()) |lc| {
                if (self.loop_deferred_boundaries.items.len > 0) {
                    const boundary = self.loop_deferred_boundaries.items[self.loop_deferred_boundaries.items.len - 1];
                    try self.emitDeferredToBoundary(boundary);
                }
                try self.instructions.append(.{ .Jump = .{ .label = lc.break_label } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
                    .file_uri = stmt.base.location().file_uri,
                    .range = .{
                        .start_line = stmt.base.location().range.start_line,
                        .start_col = stmt.base.location().range.start_col,
                        .end_line = stmt.base.location().range.end_line,
                        .end_col = stmt.base.location().range.end_col,
                    },
                };
                self.reporter.reportCompileError(
                    location,
                    ErrorCode.BREAK_USED_OUTSIDE_OF_LOOP,
                    "'break' used outside of a loop",
                    .{},
                );
            }
        },
        .VarDecl => |decl| {
            var var_type: HIRType = .Nothing;
            var precreated_cast_idx: ?u32 = null;

            // The binding's named type, as analysis typed it: its annotation,
            // or else its initializer. Dispatch and peeks read it by key.
            const binding_type: ?ast.TypeInfo = if (decl.type_info.base != .Nothing)
                decl.type_info
            else if (decl.initializer) |init_expr|
                if (self.semantic.getCachedExprType(init_expr)) |init_type| init_type.* else null
            else
                null;
            const binding_key: ?[]const u8 = if (binding_type) |t| self.typeKeyOf(t) else null;

            if (decl.type_info.base != .Nothing) {
                var_type = switch (decl.type_info.base) {
                    .Int => .Int,
                    .Float => .Float,
                    .String => .String,
                    .Tetra => .Tetra,
                    .Byte => .Byte,
                    .Array => self.convertTypeInfo(decl.type_info),
                    .Union => blk: {
                        if (decl.type_info.union_type) |_| {
                            break :blk self.convertTypeInfo(decl.type_info);
                        }
                        break :blk .Unknown;
                    },
                    .Enum, .Struct, .Custom => if (binding_key) |key| self.type_system.customTypeForName(key) else .Nothing,
                    else => .Nothing,
                };
            }

            if (decl.initializer) |init_expr| {
                const previous_override = self.array_storage_override;
                defer self.array_storage_override = previous_override;
                const previous_element_override = self.array_element_type_override;
                defer self.array_element_type_override = previous_element_override;

                if (decl.type_info.base == .Array) {
                    self.array_storage_override = self.storageKindFromTypeInfo(decl.type_info);
                    self.array_element_type_override = resolveArrayElementInfo(self, decl.type_info.array_type).element_type;
                } else {
                    self.array_storage_override = null;
                    self.array_element_type_override = null;
                }

                // If the initializer is an `as` cast, pre-create the variable slot
                // and expose it so the cast can store the subject value into the
                // binding before its then/else branches run, making the declared
                // name readable (and narrowed) inside both branches.
                if (init_expr.data == .Cast) {
                    const idx = try self.symbol_table.createVariable(decl.name.lexeme);
                    precreated_cast_idx = idx;
                    self.cast_decl_var_index = idx;
                    self.cast_decl_var_name = decl.name.lexeme;
                }

                try self.generateExpression(init_expr, true, true);

                self.cast_decl_var_index = null;
                self.cast_decl_var_name = null;

                if (init_expr.data == .Array and decl.type_info.base == .Array) {
                    const elements_for_type_fix = init_expr.data.Array;
                    if (elements_for_type_fix.len == 0) {
                        const resolved = resolveArrayElementInfo(self, decl.type_info.array_type);
                        if (resolved.element_type != .Unknown and resolved.element_type != .Nothing) {
                            try self.instructions.append(.Pop);
                            // A fixed-size array has an immutable length, so `is []`
                            // zero-fills the declared dimensions rather than producing
                            // a zero-length array (which downstream indexing would then
                            // read out of bounds of).
                            const storage_kind = self.storageKindFromTypeInfo(decl.type_info);
                            const is_fixed = storage_kind == .fixed or storage_kind == .const_literal;
                            const size: u32 = if (is_fixed)
                                (if (decl.type_info.array_size) |s| @intCast(s) else 0)
                            else
                                0;
                            const nested = if (is_fixed) collectNestedSizes(decl.type_info.array_type) else NestedSizes{};
                            if (nested.truncated) {
                                self.reporter.reportCompileError(
                                    stmt.base.location(),
                                    ErrorCode.INVALID_ARRAY_TYPE,
                                    "nested arrays are limited to 4 levels",
                                    .{},
                                );
                            }
                            try self.instructions.append(.{ .ArrayNew = .{
                                .element_type = resolved.element_type,
                                .size = size,
                                .nested_element_type = resolved.nested_element_type,
                                .storage_kind = storage_kind,
                                .nested_sizes = nested.sizes,
                                .nested_depth = nested.depth,
                                .element_struct_field_types = self.elementStructFieldTypes(resolved.element_type),
                                .element_struct_type_name = self.elementStructTypeName(resolved.element_type),
                            } });
                            try self.trackArrayElementType(decl.name.lexeme, resolved.element_type);
                            // Preserve the declared array type for typed empty literals (e.g. int[] is []).
                            // Resetting to Nothing causes a later fallback inference from [] to degrade to nothing[].
                            var_type = self.convertTypeInfo(decl.type_info);
                        }
                    } else {
                        // Non-empty array: prefer the declared element type when the
                        // declaration carries an explicit annotation; fall back to
                        // inference from the literal otherwise.
                        const resolved = resolveArrayElementInfo(self, decl.type_info.array_type);
                        if (resolved.element_type != .Unknown and resolved.element_type != .Nothing) {
                            try self.trackArrayElementType(decl.name.lexeme, resolved.element_type);
                            var_type = self.convertTypeInfo(decl.type_info);
                        } else {
                            var_type = self.inferTypeFromExpression(init_expr);
                        }
                    }
                }

                if (var_type == .Nothing) {
                    var_type = self.inferTypeFromExpression(init_expr);

                    // If the variable is being assigned an array, track the element type
                    if (var_type == .Array and init_expr.data == .Variable) {
                        const source_var_name = init_expr.data.Variable.lexeme;
                        if (self.symbol_table.getTrackedArrayElementType(source_var_name)) |elem_type| {
                            try self.trackArrayElementType(decl.name.lexeme, elem_type);
                        }
                    }

                    if (var_type == .Union) {
                        const union_members = var_type.Union.members;
                        const member_names = try self.allocator.alloc([]const u8, union_members.len);
                        for (union_members, member_names) |member_type, *name| {
                            name.* = try self.hirTypeToDisplayName(member_type.*);
                        }

                        const var_index = try self.getOrCreateVariable(decl.name.lexeme);
                        try self.symbol_table.trackVariableUnionMembers(self.symbol_table.isLocalVariable(decl.name.lexeme), var_index, member_names);
                    }

                    // A named initializer type is the binding's type outright:
                    // the inferred HIR type may only carry a placeholder id.
                    if (binding_key) |key| var_type = self.type_system.customTypeForName(key);

                    if (var_type == .Group) {
                        const var_index = try self.getOrCreateVariable(decl.name.lexeme);
                        const member_names = try self.type_system.getGroupMemberNames(self.semantic.group_table.keyOf(var_type.Group).?);
                        if (member_names.len > 0) {
                            try self.symbol_table.trackVariableUnionMembers(self.symbol_table.isLocalVariable(decl.name.lexeme), var_index, member_names);
                        }
                    }
                }

                if (init_expr.data == .Array) {
                    const elements = init_expr.data.Array;
                    if (elements.len > 0) {
                        const elem_type: HIRType = switch (elements[0].data) {
                            .Literal => |lit| self.inferTypeFromLiteral(lit),
                            else => .Unknown,
                        };
                        if (elem_type != .Unknown) {
                            try self.trackArrayElementType(decl.name.lexeme, elem_type);
                        }
                    }
                }
            } else {
                if (decl.type_info.base == .Array) {
                    const size = if (decl.type_info.array_size) |s| @as(u32, @intCast(s)) else 0;
                    const resolved = resolveArrayElementInfo(self, decl.type_info.array_type);
                    const nested = collectNestedSizes(decl.type_info.array_type);

                    if (nested.truncated) {
                        self.reporter.reportCompileError(
                            stmt.base.location(),
                            ErrorCode.INVALID_ARRAY_TYPE,
                            "nested arrays are limited to 4 levels",
                            .{},
                        );
                    }

                    try self.instructions.append(.{ .ArrayNew = .{
                        .element_type = resolved.element_type,
                        .size = size,
                        .nested_element_type = resolved.nested_element_type,
                        .storage_kind = self.storageKindFromTypeInfo(decl.type_info),
                        .nested_sizes = nested.sizes,
                        .nested_depth = nested.depth,
                        .element_struct_field_types = self.elementStructFieldTypes(resolved.element_type),
                        .element_struct_type_name = self.elementStructTypeName(resolved.element_type),
                    } });

                    try self.trackArrayElementType(decl.name.lexeme, resolved.element_type);
                } else {
                    switch (var_type) {
                        .Int => {
                            const default_value = HIRValue{ .int = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Float => {
                            const default_value = HIRValue{ .float = 0.0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .String => {
                            const default_value = HIRValue{ .string = "" };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Tetra => {
                            const default_value = HIRValue{ .tetra = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Byte => {
                            const default_value = HIRValue{ .byte = 0 };
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                        .Array => {
                            const size = if (decl.type_info.array_size) |s| @as(u32, @intCast(s)) else 0;
                            var element_type: HIRType = .Unknown;
                            var nested_element_type: ?HIRType = null;
                            if (var_type == .Array) {
                                element_type = var_type.Array.*;
                                nested_element_type = SoxaTypes.arrayInnermostElementType(element_type);
                            }

                            const nested = collectNestedSizes(decl.type_info.array_type);

                            if (nested.truncated) {
                                self.reporter.reportCompileError(
                                    stmt.base.location(),
                                    ErrorCode.INVALID_ARRAY_TYPE,
                                    "nested arrays are limited to 4 levels",
                                    .{},
                                );
                            }

                            try self.instructions.append(.{ .ArrayNew = .{
                                .element_type = element_type,
                                .size = size,
                                .nested_element_type = nested_element_type,
                                .storage_kind = self.storageKindFromTypeInfo(decl.type_info),
                                .nested_sizes = nested.sizes,
                                .nested_depth = nested.depth,
                                .element_struct_field_types = self.elementStructFieldTypes(element_type),
                                .element_struct_type_name = self.elementStructTypeName(element_type),
                            } });

                            try self.trackArrayElementType(decl.name.lexeme, element_type);
                        },
                        else => {
                            // TODO(struct default): this arm absorbs every type
                            // with no materialized default — `.Struct` included —
                            // and pushes `nothing`. For a struct that value is
                            // then stored as a struct reference and renders as
                            // `zext {} 0 to i64`, which `zig cc` rejects, so
                            // `var p :: Point` fails to compile with a clang
                            // error instead of a Doxa diagnostic. What a struct
                            // local should hold (null, a synthesized default, or
                            // a rejection) is undecided: see
                            // plan/uninitialized-declarations.md. Whatever it
                            // becomes, this switch should be exhaustive so the
                            // next type without a default fails to compile
                            // rather than emit invalid IR.
                            const default_value = HIRValue.nothing;
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                    }
                }
            }

            if (decl.type_info.base == .Array) {
                try self.trackArrayStorageKind(decl.name.lexeme, self.storageKindFromTypeInfo(decl.type_info));
            } else switch (var_type) {
                .Array => try self.trackArrayStorageKind(decl.name.lexeme, SoxaTypes.ArrayStorageKind.dynamic),
                else => {},
            }

            try self.trackVariableType(decl.name.lexeme, var_type);
            if (var_type == .Array) {
                try self.trackArrayElementType(decl.name.lexeme, var_type.Array.*);
            }

            if (binding_key) |key| try self.trackVariableCustomType(decl.name.lexeme, key);

            const var_idx = precreated_cast_idx orelse try self.symbol_table.createVariable(decl.name.lexeme);
            const is_module_ctx = self.current_function == null and self.isModuleContext();

            if (decl.type_info.base == .Union) {
                if (decl.type_info.union_type) |ut| {
                    const list = try self.collectUnionMemberNames(ut);
                    try self.symbol_table.trackVariableUnionMembers(self.symbol_table.isLocalVariable(decl.name.lexeme), var_idx, list);
                }
            }

            if (self.current_function == null) {
                try self.instructions.append(.Dup);
            }

            if (!decl.type_info.is_mutable) {
                // Check if the initializer is a literal (compile-time constant)
                var is_literal = if (decl.initializer) |init_expr| isLiteralExpression(init_expr) else false;
                // Union types need the canonical value wrapper even for literals
                if (var_type == .Union) {
                    is_literal = false;
                }

                const scope_kind = self.symbol_table.determineVariableScopeWithModuleContext(decl.name.lexeme, is_module_ctx);

                if (is_literal) {
                    // For literal constants, use StoreDecl with is_const
                    try self.instructions.append(.{ .StoreDecl = .{
                        .var_index = var_idx,
                        .var_name = decl.name.lexeme,
                        .scope_kind = scope_kind,
                        .module_context = null,
                        .declared_type = var_type,
                        .is_const = true,
                    } });
                } else {
                    // For non-literal const declarations, use StoreDecl with is_const = true
                    try self.instructions.append(.{ .StoreDecl = .{
                        .var_index = var_idx,
                        .var_name = decl.name.lexeme,
                        .scope_kind = scope_kind,
                        .module_context = null,
                        .declared_type = var_type,
                        .is_const = true,
                    } });
                }
                if (self.current_function == null) {
                    try self.instructions.append(.Pop);
                }
            } else {
                const scope_kind = self.symbol_table.determineVariableScopeWithModuleContext(decl.name.lexeme, is_module_ctx);

                try self.instructions.append(.{ .StoreDecl = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = scope_kind,
                    .module_context = null,
                    .declared_type = var_type,
                    .is_const = !decl.type_info.is_mutable,
                } });
                if (self.current_function == null) {
                    try self.instructions.append(.Pop);
                }
            }
        },
        .FunctionDecl => {},
        .Return => |ret| {
            if (ret.value) |value| {
                try self.generateReturnValue(value);
            } else {
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            }

            try self.emitAllDeferredForReturn();

            try self.instructions.append(.{ .Return = .{ .has_value = ret.value != null, .return_type = self.current_function_return_type } });

            if (self.current_function_scope_id) |scope_id| {
                try self.instructions.append(.{ .ExitScope = .{ .scope_id = scope_id } });
            }
        },
        // Type declarations are compile time only: analysis registered them.
        .EnumDecl, .GroupDecl => {},
        .Assert => |assert_stmt| {
            try self.generateExpression(assert_stmt.condition, true, true);

            const success_label = try self.generateLabel("assert_success");
            const failure_label = try self.generateLabel("assert_failure");

            try self.instructions.append(.{
                .JumpCond = .{
                    .label_true = success_label,
                    .label_false = failure_label,
                    .condition_type = .Tetra,
                },
            });

            try self.instructions.append(.{ .Label = .{ .name = failure_label } });

            if (assert_stmt.message) |msg| {
                try self.generateExpression(msg, true, true);
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = true,
                } });
            } else {
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = false,
                } });
            }
            try self.instructions.append(.{ .Label = .{ .name = success_label } });
        },
        .MapLiteral => |map_literal| {
            // Generate else value first if it exists
            if (map_literal.else_value) |else_expr| {
                try self.generateExpression(else_expr, true, false);
            }

            var reverse_i = map_literal.entries.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const entry = map_literal.entries[reverse_i];
                try self.generateExpression(entry.key, true, false);
                try self.generateExpression(entry.value, true, false);
            }

            const dummy_entries = try self.allocator.alloc(HIRMapEntry, map_literal.entries.len);
            for (dummy_entries) |*entry| {
                const nothing_key = try self.allocator.create(HIRValue);
                nothing_key.* = .{ .nothing = .{} };
                const nothing_value = try self.allocator.create(HIRValue);
                nothing_value.* = .{ .nothing = .{} };
                entry.* = HIRMapEntry{
                    .key = nothing_key,
                    .value = nothing_value,
                };
            }

            const map_instruction = HIRInstruction{
                .Map = .{
                    .entries = dummy_entries,
                    .key_type = .String,
                    .value_type = .Unknown,
                    .has_else_value = map_literal.else_value != null,
                },
            };

            try self.instructions.append(map_instruction);
        },
        // Imports were bound by the module loader: compile time only.
        .Import => {},
    }
}
