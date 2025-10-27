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
const ScopeKind = @import("soxa_types.zig").ScopeKind;

pub fn generateStatement(self: *HIRGenerator, stmt: ast.Stmt) (std.mem.Allocator.Error || ErrorList)!void {
    switch (stmt.data) {
        .Expression => |expr| {
            if (expr) |e| {
                // Do not treat a trailing expression as an implicit function return.
                // Always generate as a regular expression statement here.
                try self.generateExpression(e, false, true);
            }
        },
        .Continue => {
            if (self.currentLoopContext()) |lc| {
                try self.instructions.append(.{ .Jump = .{ .label = lc.continue_label, .vm_offset = 0 } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
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
                try self.instructions.append(.{ .Jump = .{ .label = lc.break_label, .vm_offset = 0 } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
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

            var custom_type_name: ?[]const u8 = null;
            if (decl.type_info.base != .Nothing) {
                var_type = switch (decl.type_info.base) {
                    .Int => .Int,
                    .Float => .Float,
                    .String => .String,
                    .Tetra => .Tetra,
                    .Byte => .Byte,
                    .Array => blk: {
                        // For now, create a simple array type with Int element type
                        // This should be improved to infer the actual element type
                        const element_type = self.allocator.create(HIRType) catch return;
                        element_type.* = .Int;
                        break :blk HIRType{ .Array = element_type };
                    },
                    .Union => blk: {
                        if (decl.type_info.union_type) |_| {
                            break :blk .Unknown;
                        }
                        break :blk .Nothing;
                    },
                    .Enum => blk: {
                        custom_type_name = decl.type_info.custom_type;
                        break :blk HIRType{ .Enum = 0 };
                    },
                    .Struct => blk: {
                        custom_type_name = decl.type_info.custom_type;
                        break :blk HIRType{ .Struct = 0 };
                    },
                    .Custom => blk: {
                        if (decl.type_info.custom_type) |type_name| {
                            if (self.type_system.custom_types.get(type_name)) |custom_type| {
                                if (custom_type.kind == .Enum) {
                                    custom_type_name = type_name;
                                    break :blk HIRType{ .Enum = 0 };
                                } else if (custom_type.kind == .Struct) {
                                    try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                    break :blk HIRType{ .Struct = 0 };
                                }
                            }
                            try self.trackVariableCustomType(decl.name.lexeme, type_name);
                            break :blk HIRType{ .Struct = 0 };
                        }
                        break :blk .Nothing;
                    },
                    else => .Nothing,
                };
            }

            if (decl.initializer) |init_expr| {
                if (init_expr.data == .FieldAccess) {
                    const fa = init_expr.data.FieldAccess;
                    if (fa.object.data == .Variable) {
                        const root_alias = fa.object.data.Variable.lexeme;
                        if (self.isModuleNamespace(root_alias)) {
                            if (std.mem.eql(u8, fa.field.lexeme, "raylib") or std.mem.eql(u8, fa.field.lexeme, "doxa")) {
                                if (self.module_namespaces.get(root_alias)) |_| {
                                    const nested_name = if (std.mem.eql(u8, fa.field.lexeme, "raylib")) "graphics.raylib" else "graphics.doxa";
                                    const nested_info: ast.ModuleInfo = .{
                                        .name = nested_name,
                                        .imports = &[_]ast.ImportInfo{},
                                        .ast = null,
                                        .file_path = nested_name,
                                        .symbols = null,
                                    };
                                    try self.module_namespaces.put(decl.name.lexeme, nested_info);
                                    return;
                                }
                            }
                        }
                    }
                }

                const old_enum_context = self.current_enum_type;
                if (custom_type_name != null) {
                    self.current_enum_type = custom_type_name;
                }

                try self.generateExpression(init_expr, true, true);

                self.current_enum_type = old_enum_context;

                if (init_expr.data == .Array and decl.type_info.base == .Array) {
                    const elements_for_type_fix = init_expr.data.Array;
                    if (elements_for_type_fix.len == 0) {
                        if (decl.type_info.array_type) |annot_elem| {
                            var element_type_fix: HIRType = .Unknown;
                            var nested_element_type_fix: ?HIRType = null;
                            if (annot_elem.base == .Array) {
                                element_type_fix = .Unknown;
                                if (annot_elem.array_type) |inner| {
                                    nested_element_type_fix = switch (inner.base) {
                                        .Int => .Int,
                                        .Float => .Float,
                                        .String => .String,
                                        .Byte => .Byte,
                                        .Tetra => .Tetra,
                                        .Array => .Unknown,
                                        else => .Unknown,
                                    };
                                }
                            } else {
                                element_type_fix = switch (annot_elem.base) {
                                    .Int => .Int,
                                    .Float => .Float,
                                    .String => .String,
                                    .Byte => .Byte,
                                    .Tetra => .Tetra,
                                    else => .Unknown,
                                };
                            }

                            if (element_type_fix != .Unknown and element_type_fix != .Nothing) {
                                try self.instructions.append(.Pop);
                                try self.instructions.append(.{ .ArrayNew = .{
                                    .element_type = element_type_fix,
                                    .size = 0,
                                    .nested_element_type = nested_element_type_fix,
                                } });
                                try self.trackArrayElementType(decl.name.lexeme, element_type_fix);
                                var_type = HIRType.Nothing;
                            }
                        }
                    } else {
                        // Non-empty array - the expression has already been generated
                        // Just set the var_type to Array
                        const element_type = self.allocator.create(HIRType) catch return;
                        element_type.* = .Int; // For now, assume Int elements
                        var_type = HIRType{ .Array = element_type };
                    }
                }

                if (var_type == .Nothing) {
                    var_type = self.inferTypeFromExpression(init_expr);
                    if (var_type == .Union) {
                        const union_members = var_type.Union;
                        const member_names = try self.allocator.alloc([]const u8, union_members.len);
                        for (union_members, 0..) |member_type, i| {
                            member_names[i] = switch (member_type.*) {
                                .Byte => "byte",
                                .Int => "int",
                                .Float => "float",
                                .String => "string",
                                .Tetra => "tetra",
                                .Nothing => "nothing",
                                .Enum => blk: {
                                    if (self.type_system.custom_types.get("ValueError")) |_| {
                                        break :blk "ValueError";
                                    }
                                    break :blk "custom";
                                },
                                else => "unknown",
                            };
                        }

                        const var_index = try self.getOrCreateVariable(decl.name.lexeme);
                        try self.symbol_table.trackVariableUnionMembersByIndex(var_index, member_names);
                    }

                    if (var_type == .Struct and init_expr.data == .StructLiteral) {
                        const struct_lit = init_expr.data.StructLiteral;
                        try self.trackVariableCustomType(decl.name.lexeme, struct_lit.name.lexeme);
                    }

                    if (var_type == .Enum and init_expr.data == .FieldAccess) {
                        const fa = init_expr.data.FieldAccess;
                        if (fa.object.data == .Variable) {
                            const enum_type_name = fa.object.data.Variable.lexeme;
                            if (self.isCustomType(enum_type_name)) |ct_enum| {
                                if (ct_enum.kind == .Enum) {
                                    try self.trackVariableCustomType(decl.name.lexeme, enum_type_name);
                                }
                            }
                        }
                    }

                    if (init_expr.data == .FunctionCall) {
                        const call = init_expr.data.FunctionCall;
                        if (call.callee.data == .FieldAccess) {
                            const callee_field = call.callee.data.FieldAccess;
                            if (callee_field.object.data == .Variable and std.mem.eql(u8, callee_field.field.lexeme, "New")) {
                                const type_name = callee_field.object.data.Variable.lexeme;
                                if (self.isCustomType(type_name)) |ct_new| {
                                    if (ct_new.kind == .Struct) {
                                        try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                        var_type = HIRType{ .Struct = 0 };
                                    }
                                }
                            }
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
                    const element_type: HIRType = if (decl.type_info.array_type) |at| switch (at.base) {
                        .Byte => .Byte,
                        .Int => .Int,
                        .Float => .Float,
                        .String => .String,
                        .Tetra => .Tetra,
                        else => .Nothing,
                    } else .Nothing;

                    try self.instructions.append(.{ .ArrayNew = .{
                        .element_type = element_type,
                        .size = size,
                    } });

                    try self.trackArrayElementType(decl.name.lexeme, element_type);
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
                            const element_type: HIRType = if (decl.type_info.array_type) |at| switch (at.base) {
                                .Byte => .Byte,
                                .Int => .Int,
                                .Float => .Float,
                                .String => .String,
                                .Tetra => .Tetra,
                                else => .Nothing,
                            } else .Nothing;

                            try self.instructions.append(.{ .ArrayNew = .{
                                .element_type = element_type,
                                .size = size,
                            } });

                            try self.trackArrayElementType(decl.name.lexeme, element_type);
                        },
                        else => {
                            const default_value = HIRValue.nothing;
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                    }
                }
            }

            try self.trackVariableType(decl.name.lexeme, var_type);

            if (custom_type_name) |custom_type| {
                try self.trackVariableCustomType(decl.name.lexeme, custom_type);
            }

            const var_idx = try self.symbol_table.createVariable(decl.name.lexeme);

            if (decl.type_info.base == .Union) {
                if (decl.type_info.union_type) |ut| {
                    const list = try self.collectUnionMemberNames(ut);
                    try self.symbol_table.trackVariableUnionMembersByIndex(var_idx, list);
                }
            }

            if (self.current_function == null) {
                try self.instructions.append(.Dup);
            }

            if (!decl.type_info.is_mutable) {
                const is_module_ctx = self.current_function == null and self.isModuleContext();
                const scope_kind = self.symbol_table.determineVariableScopeWithModuleContext(decl.name.lexeme, is_module_ctx);

                try self.instructions.append(.{ .StoreConst = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = scope_kind,
                    .module_context = null,
                } });
                if (self.current_function == null) {
                    try self.instructions.append(.Pop);
                }
            } else {
                const is_module_ctx = self.current_function == null and self.isModuleContext();
                const scope_kind = self.symbol_table.determineVariableScopeWithModuleContext(decl.name.lexeme, is_module_ctx);

                // Debug output
                if (std.mem.eql(u8, decl.name.lexeme, "LiteralToToken")) {
                    std.debug.print("Processing LiteralToToken: is_module_ctx={}, scope_kind={s}\n", .{ is_module_ctx, @tagName(scope_kind) });
                }

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
                try self.generateExpression(value, true, true);
                const inferred_ret_type = self.inferTypeFromExpression(value);
                try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = inferred_ret_type } });
            } else {
                try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
            }
        },
        .EnumDecl => |enum_decl| {
            var variant_names = try self.allocator.alloc([]const u8, enum_decl.variants.len);
            for (enum_decl.variants, 0..) |variant_token, i| {
                variant_names[i] = variant_token.lexeme;
            }
            try self.registerEnumType(enum_decl.name.lexeme, variant_names);

            const var_idx = try self.getOrCreateVariable(enum_decl.name.lexeme);
            try self.trackVariableType(enum_decl.name.lexeme, HIRType{ .Enum = 0 });

            const enum_type_value = HIRValue{ .string = enum_decl.name.lexeme };
            const const_idx = try self.addConstant(enum_type_value);
            try self.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
            try self.instructions.append(.{ .StoreConst = .{
                .var_index = var_idx,
                .var_name = enum_decl.name.lexeme,
                .scope_kind = if (self.current_function == null or self.is_global_init_phase) .ModuleGlobal else .Local,
                .module_context = null,
            } });
        },
        .Assert => |assert_stmt| {
            try self.generateExpression(assert_stmt.condition, true, true);

            const success_label = try self.generateLabel("assert_success");
            const failure_label = try self.generateLabel("assert_failure");

            try self.instructions.append(.{
                .JumpCond = .{
                    .label_true = success_label,
                    .label_false = failure_label,
                    .vm_offset = 0,
                    .condition_type = .Tetra,
                },
            });

            try self.instructions.append(.{ .Label = .{ .name = failure_label, .vm_address = 0 } });

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
            try self.instructions.append(.{ .Label = .{ .name = success_label, .vm_address = 0 } });
        },
        .MapLiteral => |entries| {
            var reverse_i = entries.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const entry = entries[reverse_i];
                try self.generateExpression(entry.key, true, false);
                try self.generateExpression(entry.value, true, false);
            }

            const dummy_entries = try self.allocator.alloc(HIRMapEntry, entries.len);
            for (dummy_entries) |*entry| {
                entry.* = HIRMapEntry{
                    .key = HIRValue.nothing,
                    .value = HIRValue.nothing,
                };
            }

            const map_instruction = HIRInstruction{
                .Map = .{
                    .entries = dummy_entries,
                    .key_type = .String,
                    .value_type = .Unknown,
                },
            };

            try self.instructions.append(map_instruction);
        },
        else => {
            self.reporter.reportCompileError(
                stmt.base.location(),
                ErrorCode.UNHANDLED_STATEMENT_TYPE,
                "Unhandled statement type: {}",
                .{stmt.data},
            );
        },
    }
}
