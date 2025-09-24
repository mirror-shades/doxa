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

            // NEW: Determine the variable's type for tracking
            var var_type: HIRType = .Nothing;

            // FIXED: Prioritize explicit type annotation over inference
            var custom_type_name: ?[]const u8 = null;
            if (decl.type_info.base != .Nothing) {
                // Use explicit type annotation first
                var_type = switch (decl.type_info.base) {
                    .Int => .Int,
                    .Float => .Float,
                    .String => .String,
                    .Tetra => .Tetra,
                    .Byte => .Byte,
                    .Array => .Unknown, // Array type requires element type info
                    .Union => blk: {
                        // Handle union types properly - use .Union as the type, not the first member
                        if (decl.type_info.union_type) |ut| {
                            // Record union member names (lowercase) for this variable (flatten nested unions)
                            const list = try self.collectUnionMemberNames(ut);

                            try self.symbol_table.trackVariableUnionMembers(decl.name.lexeme, list);
                            var maybe_index: ?u32 = null;
                            maybe_index = self.symbol_table.getVariable(decl.name.lexeme);
                            if (maybe_index) |var_index| {
                                try self.symbol_table.trackVariableUnionMembersByIndex(var_index, list);
                            }

                            // Use .Union as the type, not the first member's type
                            break :blk .Unknown; // Union type requires member type info
                        }
                        break :blk .Nothing;
                    },
                    .Enum => blk: {
                        // Extract the actual enum type name from the custom_type field
                        custom_type_name = decl.type_info.custom_type;
                        break :blk HIRType{ .Enum = 0 };
                    },
                    .Struct => blk: {
                        // Extract the actual struct type name from the custom_type field
                        custom_type_name = decl.type_info.custom_type;
                        break :blk HIRType{ .Struct = 0 };
                    },
                    .Custom => blk: {
                        // CRITICAL FIX: Handle Custom type - check if it's an enum or struct
                        if (decl.type_info.custom_type) |type_name| {
                            // Check if this custom type is a registered enum
                            if (self.type_system.custom_types.get(type_name)) |custom_type| {
                                if (custom_type.kind == .Enum) {
                                    custom_type_name = type_name;
                                    break :blk HIRType{ .Enum = 0 };
                                } else if (custom_type.kind == .Struct) {
                                    // Track the struct type name for instances
                                    try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                    break :blk HIRType{ .Struct = 0 };
                                }
                            }
                            // If not found in custom_types, assume it's a struct and track it
                            try self.trackVariableCustomType(decl.name.lexeme, type_name);
                            break :blk HIRType{ .Struct = 0 };
                        }
                        // If we can't determine the custom type, it's unknown
                        break :blk .Nothing; // Unknown custom type
                    },
                    else => .Nothing,
                };
            }

            // Generate the initializer expression with enum type context
            if (decl.initializer) |init_expr| {
                // Special case: namespace aliasing like `const rl is g.raylib`
                // If initializer is a module namespace field (graphics.raylib / graphics.doxa),
                // register LHS name as a module namespace alias and emit no runtime code.
                if (init_expr.data == .FieldAccess) {
                    const fa = init_expr.data.FieldAccess;
                    if (fa.object.data == .Variable) {
                        const root_alias = fa.object.data.Variable.lexeme;
                        if (self.isModuleNamespace(root_alias)) {
                            if (std.mem.eql(u8, fa.field.lexeme, "raylib") or std.mem.eql(u8, fa.field.lexeme, "doxa")) {
                                if (self.module_namespaces.get(root_alias)) |_| {
                                    // Create a minimal nested ModuleInfo to tag this alias as graphics.<sub>
                                    const nested_name = if (std.mem.eql(u8, fa.field.lexeme, "raylib")) "graphics.raylib" else "graphics.doxa";
                                    const nested_info: @import("../../ast/ast.zig").ModuleInfo = .{
                                        .name = nested_name,
                                        .imports = &[_]@import("../../ast/ast.zig").ImportInfo{},
                                        .ast = null,
                                        .file_path = nested_name,
                                        .symbols = null,
                                    };
                                    try self.module_namespaces.put(decl.name.lexeme, nested_info);
                                    // Do not generate any runtime instructions for this aliasing assignment
                                    return;
                                }
                            }
                        }
                    }
                }

                // Set enum type context if we're declaring an enum variable
                const old_enum_context = self.current_enum_type;
                if (custom_type_name != null) {
                    self.current_enum_type = custom_type_name;
                }

                // Var declaration initializer must leave a value on the stack for StoreVar
                // Preserve the result so the subsequent store can consume it
                try self.generateExpression(init_expr, true, true);

                // Restore previous enum context
                self.current_enum_type = old_enum_context;

                // If initializer is an empty array literal and we have explicit array annotation,
                // rebuild a typed empty array matching the annotation so type displays as int[]/float[]/... not nothing[]
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
                                // Replace the untyped empty array with a typed empty array
                                try self.instructions.append(.Pop);
                                try self.instructions.append(.{ .ArrayNew = .{
                                    .element_type = element_type_fix,
                                    .size = 0,
                                    .nested_element_type = nested_element_type_fix,
                                } });
                                // Track element type for subsequent indexing
                                try self.trackArrayElementType(decl.name.lexeme, element_type_fix);
                                // Ensure var_type reflects array for downstream StoreVar expected_type
                                var_type = HIRType.Nothing;
                            }
                        }
                    }
                }

                // Only infer type if no explicit annotation was provided
                if (var_type == .Nothing) {
                    // std.debug.print("DEBUG: SoxaStatements - inferring type for variable '{s}' with init_expr type: {any}\n", .{ decl.name.lexeme, init_expr.data });
                    var_type = self.inferTypeFromExpression(init_expr);
                    // std.debug.print("DEBUG: SoxaStatements - inferred type: {any}\n", .{var_type});

                    // NEW: Extract union member names from inferred union types
                    if (var_type == .Union) {
                        const union_members = var_type.Union;
                        // Extract member names from union type
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
                                    // Look up the actual enum type name from custom types
                                    // For @byte function, this should be ValueError
                                    if (self.type_system.custom_types.get("ValueError")) |_| {
                                        break :blk "ValueError";
                                    }
                                    // Fallback to generic enum name
                                    break :blk "custom";
                                },
                                else => "unknown",
                            };
                        }

                        // Track union members for this variable
                        try self.symbol_table.trackVariableUnionMembers(decl.name.lexeme, member_names);

                        // Create the variable first if it doesn't exist, then track by index
                        const var_index = try self.getOrCreateVariable(decl.name.lexeme);
                        try self.symbol_table.trackVariableUnionMembersByIndex(var_index, member_names);
                    }

                    // FIXED: Extract custom type name from struct literals
                    if (var_type == .Struct and init_expr.data == .StructLiteral) {
                        const struct_lit = init_expr.data.StructLiteral;
                        try self.trackVariableCustomType(decl.name.lexeme, struct_lit.name.lexeme);
                    }

                    // Detect enum member initialization like Color.Red and remember enum name
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

                    // NEW: Detect struct constructor sugar calls like TypeName.New(...)
                    // and record the variable's struct type so instance methods resolve.
                    if (init_expr.data == .FunctionCall) {
                        const call = init_expr.data.FunctionCall;
                        if (call.callee.data == .FieldAccess) {
                            const callee_field = call.callee.data.FieldAccess;
                            if (callee_field.object.data == .Variable and std.mem.eql(u8, callee_field.field.lexeme, "New")) {
                                const type_name = callee_field.object.data.Variable.lexeme;
                                if (self.isCustomType(type_name)) |ct_new| {
                                    if (ct_new.kind == .Struct) {
                                        // Track custom type name (e.g., point -> Point) and set var type
                                        try self.trackVariableCustomType(decl.name.lexeme, type_name);
                                        var_type = HIRType{ .Struct = 0 };
                                    }
                                }
                            }
                        }
                    }
                }

                // NEW: If initializer is an array literal, record its element type for downstream inference
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
                // No initializer - push default value based on type

                // Special case: Handle arrays directly from type_info
                if (decl.type_info.base == .Array) {
                    // Create an array with the proper size and element type
                    const size = if (decl.type_info.array_size) |s| @as(u32, @intCast(s)) else 0;
                    const element_type: HIRType = if (decl.type_info.array_type) |at| switch (at.base) {
                        .Byte => .Byte,
                        .Int => .Int,
                        .Float => .Float,
                        .String => .String,
                        .Tetra => .Tetra,
                        else => .Nothing,
                    } else .Nothing;

                    // Create the array
                    try self.instructions.append(.{ .ArrayNew = .{
                        .element_type = element_type,
                        .size = size,
                    } });

                    // Track element type for this variable
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
                            // Create an array with the proper size and element type
                            const size = if (decl.type_info.array_size) |s| @as(u32, @intCast(s)) else 0;
                            const element_type: HIRType = if (decl.type_info.array_type) |at| switch (at.base) {
                                .Byte => .Byte,
                                .Int => .Int,
                                .Float => .Float,
                                .String => .String,
                                .Tetra => .Tetra,
                                else => .Nothing,
                            } else .Nothing;

                            // DEBUG: Print array declaration info
                            std.debug.print("DEBUG: Generating ArrayNew for {any} with size={any}, element_type={any}\n", .{ decl.name.lexeme, size, @tagName(element_type) });

                            // Create the array
                            try self.instructions.append(.{ .ArrayNew = .{
                                .element_type = element_type,
                                .size = size,
                            } });

                            // Track element type for this variable
                            try self.trackArrayElementType(decl.name.lexeme, element_type);
                        },
                        else => {
                            const default_value = HIRValue.nothing;
                            const const_idx = try self.addConstant(default_value);
                            try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                        },
                    }
                } // Close the else block for array handling

                // Store into the declared variable now so it has a concrete value and index
                // Only perform this pre-store in global scope where we intentionally
                // keep a duplicate around for subsequent global initialization steps.
                if (self.current_function == null) {
                    const var_idx2 = try self.getOrCreateVariable(decl.name.lexeme);
                    // Duplicate so subsequent code can see the value if needed (globals only)
                    try self.instructions.append(.Dup);
                    try self.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx2,
                        .var_name = decl.name.lexeme,
                        .scope_kind = if (self.current_function == null or self.is_global_init_phase) .ModuleGlobal else .Local,
                        .module_context = null,
                        .expected_type = var_type,
                    } });
                }

                // Also record union members by index if present by name (only if pre-stored)
                if (self.current_function == null) {
                    if (self.symbol_table.getUnionMembersByName(decl.name.lexeme)) |members0| {
                        // Use the same variable index that was already created above
                        if (self.symbol_table.getVariable(decl.name.lexeme)) |idx| {
                            try self.symbol_table.trackVariableUnionMembersByIndex(idx, members0);
                        }
                    }
                }
            }

            // NEW: Track the variable's type
            // std.debug.print("DEBUG: SoxaStatements - tracking variable '{s}' with type: {any}\n", .{ decl.name.lexeme, var_type });
            try self.trackVariableType(decl.name.lexeme, var_type);

            // NEW: Track custom type name for enums/structs
            if (custom_type_name) |custom_type| {
                try self.trackVariableCustomType(decl.name.lexeme, custom_type);
            }

            // Store variable (single instruction). Duplicate value only at global scope
            const var_idx = try self.getOrCreateVariable(decl.name.lexeme);
            if (self.current_function == null) {
                try self.instructions.append(.Dup);
            }

            // Use StoreConst for constant declarations, StoreVar for variables
            if (!decl.type_info.is_mutable) {
                // Determine scope based on current function context and global init phase
                // This is more reliable than semantic analyzer for determining scope
                const scope_kind = if (self.current_function == null or self.is_global_init_phase) ScopeKind.ModuleGlobal else ScopeKind.Local;

                try self.instructions.append(.{ .StoreConst = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = scope_kind,
                    .module_context = null,
                } });
                // In global scope, we duplicated the initializer; pop it to keep stack balanced
                if (self.current_function == null) {
                    try self.instructions.append(.Pop);
                }
            } else {
                // Determine scope based on current function context and global init phase
                // This is more reliable than semantic analyzer for determining scope
                const scope_kind = if (self.current_function == null or self.is_global_init_phase) ScopeKind.ModuleGlobal else ScopeKind.Local;

                try self.instructions.append(.{ .StoreVar = .{
                    .var_index = var_idx,
                    .var_name = decl.name.lexeme,
                    .scope_kind = scope_kind,
                    .module_context = null,
                    .expected_type = var_type,
                } });
                // In global scope, we duplicated the initializer; pop it to keep stack balanced
                if (self.current_function == null) {
                    try self.instructions.append(.Pop);
                }
            }

            // Ensure union member metadata is recorded by variable index (only for union declarations)
            if (decl.type_info.base == .Union) {
                if (self.symbol_table.getUnionMembersByName(decl.name.lexeme)) |members_for_var| {
                    // Update/insert index-based mapping so later peeks can find it reliably
                    _ = try self.symbol_table.trackVariableUnionMembersByIndex(var_idx, members_for_var);
                }
            }

            // Additionally, if initializer returns a union via builtins, record union members for this variable
            if (decl.initializer) |init_expr_union| {
                if (init_expr_union.data == .BuiltinCall) {
                    const bc = init_expr_union.data.BuiltinCall;
                    if (std.mem.eql(u8, bc.function.lexeme, "int")) {
                        const members = try self.allocator.alloc([]const u8, 2);
                        members[0] = "int";
                        members[1] = "ValueError";
                        try self.trackVariableUnionMembersByIndex(var_idx, members);
                    }
                }
            }

            // No extra value should remain on stack now; nothing to pop
        },
        .FunctionDecl => {
            // Skip - function declarations are handled in multi-pass approach
        },
        .Return => |ret| {
            if (ret.value) |value| {
                // TAIL CALL OPTIMIZATION: Disabled for now due to issues
                // if (self.tryGenerateTailCall(value)) {
                //     return; // Tail call replaces both Call and Return
                // } else {
                // Regular return with value
                try self.generateExpression(value, true, true);
                // Infer return type from the returned expression to avoid relying on signature inference
                const inferred_ret_type = self.inferTypeFromExpression(value);
                try self.instructions.append(.{ .Return = .{ .has_value = true, .return_type = inferred_ret_type } });
                // }
            } else {
                try self.instructions.append(.{ .Return = .{ .has_value = false, .return_type = .Nothing } });
            }
        },
        .EnumDecl => |enum_decl| {
            // NEW: Register enum type with variants for proper index calculation
            var variant_names = try self.allocator.alloc([]const u8, enum_decl.variants.len);
            for (enum_decl.variants, 0..) |variant_token, i| {
                variant_names[i] = variant_token.lexeme;
            }
            try self.registerEnumType(enum_decl.name.lexeme, variant_names);

            // Register the enum type name as a special variable so Color.Red works
            const var_idx = try self.getOrCreateVariable(enum_decl.name.lexeme);
            try self.trackVariableType(enum_decl.name.lexeme, HIRType{ .Enum = 0 });

            // Create a special enum type value and store it
            const enum_type_value = HIRValue{ .string = enum_decl.name.lexeme }; // Simple representation for now
            const const_idx = try self.addConstant(enum_type_value);
            try self.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
            try self.instructions.append(.{ .StoreConst = .{
                .var_index = var_idx,
                .var_name = enum_decl.name.lexeme,
                .scope_kind = if (self.current_function == null or self.is_global_init_phase) .ModuleGlobal else .Local,
                .module_context = null,
            } });

            // Enum declarations don't generate runtime instructions, they're compile-time only
        },
        .Try => |try_stmt| {
            try self.generateTryStmt(try_stmt);
        },
        .Assert => |assert_stmt| {
            // Generate the condition expression
            try self.generateExpression(assert_stmt.condition, true, true);

            // Create labels for control flow
            const success_label = try self.generateLabel("assert_success");
            const failure_label = try self.generateLabel("assert_failure");

            // Jump to success label if condition is true, fall through to failure if false
            try self.instructions.append(.{
                .JumpCond = .{
                    .label_true = success_label,
                    .label_false = failure_label,
                    .vm_offset = 0, // Will be patched
                    .condition_type = .Tetra,
                },
            });

            // Failure label - condition was false
            try self.instructions.append(.{ .Label = .{ .name = failure_label, .vm_address = 0 } });

            // Handle assert message if provided
            if (assert_stmt.message) |msg| {
                // Generate message expression (will be on stack for AssertFail to use)
                try self.generateExpression(msg, true, true);
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = true,
                } });
            } else {
                // No message provided
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = false,
                } });
            }

            // Success label - continue execution
            try self.instructions.append(.{ .Label = .{ .name = success_label, .vm_address = 0 } });
        },
        .MapLiteral => |entries| {
            // Generate each key-value pair in reverse order (for stack-based construction)
            var reverse_i = entries.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const entry = entries[reverse_i];

                // Generate key first, then value (they'll be popped in reverse order)
                try self.generateExpression(entry.key, true, false);
                try self.generateExpression(entry.value, true, false);
            }

            // Create HIRMapEntry array with the right size (will be populated by VM)
            const dummy_entries = try self.allocator.alloc(HIRMapEntry, entries.len);
            // Initialize with dummy values (VM will replace with actual values from stack)
            for (dummy_entries) |*entry| {
                entry.* = HIRMapEntry{
                    .key = HIRValue.nothing,
                    .value = HIRValue.nothing,
                };
            }

            const map_instruction = HIRInstruction{
                .Map = .{
                    .entries = dummy_entries,
                    .key_type = .String, // Assume string keys for now
                    .value_type = .Unknown, // Will be inferred from values
                },
            };

            // Generate HIR Map instruction
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
