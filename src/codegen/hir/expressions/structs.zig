const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const Location = @import("../../../utils/reporting.zig").Location;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// A3: a struct may be constructed directly in the caller's arena only when every
/// field is a value the structural clone can reproduce with a per-field clone into
/// that arena — scalars pass through, strings are copied. Fields that are
/// themselves arrays, structs, maps, unions, or functions are excluded so the
/// placement never stores a reference into the callee arena.
fn fieldsAllowCallerPlacement(field_types: []const HIRType) bool {
    for (field_types) |t| {
        switch (t) {
            .Int, .Byte, .Float, .Tetra, .Enum, .Nothing, .String => {},
            else => return false,
        }
    }
    return true;
}


/// Handle struct operations, field access, and type declarations
pub const StructsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) StructsHandler {
        return .{ .generator = generator };
    }

    fn resolveStructIdFromName(self: *StructsHandler, type_name: []const u8) u32 {
        const st = self.generator.type_system.structTypeForName(type_name);
        if (st == .Struct and st.Struct != 0) return st.Struct;
        return 0;
    }

    /// Where a field lives: the struct that declares it, its index there, and
    /// its type.
    pub const FieldSlot = struct {
        struct_id: u32,
        index: u32,
        hir_type: HIRType,
    };

    /// The slot `object.field` names. The object's type is the analyzer's: a
    /// struct, or a group whose single struct member declaring the field is
    /// the one read. Anything else reaching lowering is a compiler bug.
    pub fn fieldSlot(self: *StructsHandler, object: *ast.Expr, field: ast.Token) ErrorList!FieldSlot {
        const semantic = self.generator.semantic;
        const object_type = semantic.getCachedExprType(object) orelse return self.unresolvedField(field);
        const custom = object_type.custom_type orelse return self.unresolvedField(field);
        const ref = custom.resolved();
        const struct_id = if (semantic.struct_table.idOf(ref)) |id|
            id
        else if (semantic.group_table.idOf(ref)) |group_id|
            self.generator.type_system.groupMemberStructForField(group_id, field.lexeme) orelse return self.unresolvedField(field)
        else
            return self.unresolvedField(field);
        for (semantic.struct_table.fields(struct_id).?) |declared| {
            if (std.mem.eql(u8, declared.name, field.lexeme)) {
                return .{ .struct_id = struct_id, .index = declared.index, .hir_type = declared.hir_type };
            }
        }
        return self.unresolvedField(field);
    }

    fn unresolvedField(self: *StructsHandler, field: ast.Token) ErrorList {
        const location = ast.SourceSpan.fromToken(field).location;
        self.generator.reporter.reportInternal(
            "no analyzed struct declares the field '{s}' at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ field.lexeme, location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingExpressionType;
    }

    /// Generate HIR for struct literal expressions. The literal's type is the
    /// struct analysis resolved its name to.
    pub fn generateStructLiteral(self: *StructsHandler, expr: *ast.Expr) !void {
        const struct_data = expr.data.StructLiteral;
        const struct_key = switch (self.generator.resolutionOf(expr).?) {
            .type => |ref| self.generator.typeKey(ref),
            else => unreachable,
        };

        // A3: consume a pending return-placement intent. Clearing it before the
        // fields are lowered keeps nested literals in the callee arena (where the
        // caller-side clone can fold them into the placed object's arena).
        const place_intent = self.generator.place_return_value;
        self.generator.place_return_value = false;

        // Track field types for type checking
        const field_types = try self.generator.allocator.alloc(HIRType, struct_data.fields.len);
        defer self.generator.allocator.free(field_types);
        const field_names = try self.generator.allocator.alloc([]const u8, struct_data.fields.len);
        defer self.generator.allocator.free(field_names);

        // Prefer the struct declaration's field types over literal inference.
        // Literal inference degrades for empty arrays (`includes is []` loses the
        // `string[]` element type) and enum/struct fields, so the declared type
        // keeps downstream codegen (field metadata, `@push`, array gets) precise.
        const declared_types = blk: {
            const ct = self.generator.type_system.custom_types.get(struct_key) orelse break :blk null;
            if (ct.kind != .Struct or ct.struct_fields == null) break :blk null;
            break :blk ct.struct_fields.?;
        };

        // Reject literals that do not match the declared struct shape. Construction
        // lays the struct out from the field order seen here, while field access
        // (especially `this.field` in methods) follows the declaration; an
        // undeclared field, a missing one, or a reordered literal would silently
        // desynchronize the two. This guard covers the entry file and every
        // imported module alike, so the mistake is a compile error with a precise
        // message instead of a segmentation fault.
        var emit_declared_order = false;
        if (declared_types) |dfields| {
            emit_declared_order = try self.validateLiteralFields(struct_data.name, struct_data.fields, dfields);
        }

        if (emit_declared_order) {
            // Emit fields in DECLARED order so the runtime memory layout always
            // agrees with field access, regardless of how the literal orders them.
            // `validateLiteralFields` guarantees every declared field is present.
            const dfields = declared_types.?;
            var reverse_i = dfields.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const decl_field = dfields[reverse_i];
                const lit_field = findLiteralField(struct_data.fields, decl_field.name) orelse unreachable;
                try self.emitStructLiteralField(lit_field.value, decl_field.name, decl_field.field_type, reverse_i, field_types, field_names);
            }
        } else {
            // The declared shape is unknown, or the literal is invalid (errors are
            // already reported and abort the pipeline after HIR generation), so
            // fall back to emitting the literal's own field order.
            var reverse_i = struct_data.fields.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const field = struct_data.fields[reverse_i];
                const field_type = self.declaredFieldType(struct_key, field.name.lexeme) orelse .Unknown;
                try self.emitStructLiteralField(field.value, field.name.lexeme, field_type, reverse_i, field_types, field_names);
            }
        }

        // Generate StructNew instruction with field types
        const struct_id = self.resolveStructIdFromName(struct_key);
        try self.generator.instructions.append(.{
            .StructNew = .{
                .type_name = struct_key,
                .struct_id = struct_id,
                .field_count = @intCast(struct_data.fields.len),
                .field_names = try self.generator.allocator.dupe([]const u8, field_names),
                .field_types = try self.generator.allocator.dupe(HIRType, field_types),
                .place_in_caller = place_intent and fieldsAllowCallerPlacement(field_types),
            },
        });

        // Result is on the stack
    }

    /// Emit the value of a single struct-literal field, carrying any declared
    /// field type into the value expression, then record the field's type and
    /// name for the enclosing `StructNew`.
    fn emitStructLiteralField(
        self: *StructsHandler,
        value_expr: *ast.Expr,
        field_name: []const u8,
        field_type: HIRType,
        reverse_i: usize,
        field_types: []HIRType,
        field_names: [][]const u8,
    ) ErrorList!void {
        // Array-typed fields thread their declared element type down so empty
        // literals (`[]`) produce a correctly-tagged runtime array, not `Unknown`.
        const prev_override = self.generator.array_storage_override;
        defer self.generator.array_storage_override = prev_override;
        const prev_element_override = self.generator.array_element_type_override;
        defer self.generator.array_element_type_override = prev_element_override;
        if (field_type == .Array) {
            self.generator.array_storage_override = null;
            self.generator.array_element_type_override = field_type.Array.*;
        } else {
            self.generator.array_storage_override = null;
            self.generator.array_element_type_override = null;
        }
        try self.generator.generateExpression(value_expr, true, false);

        field_types[reverse_i] = if (field_type != .Unknown and field_type != .Nothing)
            field_type
        else
            self.generator.inferTypeFromExpression(value_expr);
        field_names[reverse_i] = field_name;

        // Push field name as constant
        const field_name_const = try self.generator.addConstant(HIRValue{ .string = field_name });
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = field_name }, .constant_id = field_name_const } });
    }

    /// A field's declared HIR type on the struct `struct_key`.
    fn declaredFieldType(self: *StructsHandler, struct_key: []const u8, field_name: []const u8) ?HIRType {
        const ct = self.generator.type_system.custom_types.get(struct_key) orelse return null;
        for (ct.struct_fields orelse &.{}) |f| {
            if (std.mem.eql(u8, f.name, field_name)) return f.field_type;
        }
        return null;
    }

    /// Check a struct literal against the declared struct fields, reporting a
    /// compile error for every undeclared or duplicate field and for a
    /// field-count mismatch. Returns `false` when the literal cannot be mapped
    /// onto the declaration (so a layout built from it would not match access).
    fn validateLiteralFields(
        self: *StructsHandler,
        struct_name: ast.Token,
        literal_fields: []const *ast.StructInstanceField,
        declared_fields: []const @import("../type_system.zig").TypeSystem.CustomTypeInfo.StructField,
    ) ErrorList!bool {
        var valid = true;

        if (literal_fields.len != declared_fields.len) {
            self.generator.reporter.reportCompileError(
                ast.SourceSpan.fromToken(struct_name).location,
                ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                "struct '{s}' expects {d} field{s}, but this literal provides {d}",
                .{
                    struct_name.lexeme,
                    declared_fields.len,
                    if (declared_fields.len == 1) "" else "s",
                    literal_fields.len,
                },
            );
            valid = false;
        }

        // Every literal field must be declared, and every declared field must be
        // present in the literal. Together with the count check this also rejects
        // duplicate literal fields, so the two sides always match one-to-one.
        for (literal_fields) |lit_field| {
            var found = false;
            for (declared_fields) |decl_field| {
                if (std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) {
                    found = true;
                    break;
                }
            }
            if (found) continue;

            const declared_list = try self.declaredFieldList(declared_fields);
            defer self.generator.allocator.free(declared_list);
            self.generator.reporter.reportCompileError(
                ast.SourceSpan.fromToken(lit_field.name).location,
                ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                "struct '{s}' has no field '{s}'; declared fields: {s}",
                .{ struct_name.lexeme, lit_field.name.lexeme, declared_list },
            );
            valid = false;
        }

        for (declared_fields) |decl_field| {
            var found = false;
            for (literal_fields) |lit_field| {
                if (std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                self.generator.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(struct_name).location,
                    ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                    "struct '{s}' is missing field '{s}'",
                    .{ struct_name.lexeme, decl_field.name },
                );
                valid = false;
            }
        }

        return valid;
    }

    /// Join declared struct field names into a human-readable list for error
    /// messages, e.g. `name, entry_point, output`.
    fn declaredFieldList(
        self: *StructsHandler,
        declared_fields: []const @import("../type_system.zig").TypeSystem.CustomTypeInfo.StructField,
    ) ![]u8 {
        const allocator = self.generator.allocator;
        var list = std.array_list.Managed(u8).init(allocator);
        errdefer list.deinit();
        for (declared_fields, 0..) |field, i| {
            if (i > 0) try list.appendSlice(", ");
            try list.appendSlice(field.name);
        }
        if (declared_fields.len == 0) try list.appendSlice("(none)");
        return list.toOwnedSlice();
    }

    /// Generate HIR for field access expressions
    pub fn generateFieldAccess(self: *StructsHandler, field: ast.FieldAccess) !void {
        // `Color.Red` (however `Color` is reached): the qualifier names an enum,
        // so the access is that variant's constant.
        if (self.generator.resolutionOf(field.object)) |resolved| {
            if (resolved == .type) {
                const key = self.generator.typeKey(resolved.type);
                const custom_type = self.generator.type_system.custom_types.get(key).?;
                if (custom_type.kind == .Enum) {
                    const variant_index = try self.resolveEnumVariantIndex(key, field.field);
                    const enum_value = HIRValue{
                        .enum_variant = HIREnum{
                            .type_name = key,
                            .variant_name = field.field.lexeme,
                            .variant_index = variant_index,
                            .path = null,
                        },
                    };
                    const const_idx = try self.generator.addConstant(enum_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                    return;
                }
            }
        }

        const obj_type = self.generator.inferTypeFromExpression(field.object);
        try self.generator.generateExpression(field.object, true, false);

        const slot = try self.fieldSlot(field.object, field.field);
        try self.generator.instructions.append(.{
            .GetField = .{
                .field_name = field.field.lexeme,
                .container_type = obj_type,
                .struct_id = slot.struct_id,
                .field_index = slot.index,
                .field_type = slot.hir_type,
                .field_for_peek = false,
                .nested_struct_id = null,
            },
        });
    }

    fn resolveEnumVariantIndex(self: *StructsHandler, enum_key: []const u8, variant_token: ast.Token) ErrorList!u32 {
        const custom_type = self.generator.type_system.custom_types.get(enum_key).?;
        if (custom_type.getEnumVariantIndex(variant_token.lexeme)) |index| return index;
        self.generator.reporter.reportCompileError(
            ast.SourceSpan.fromToken(variant_token).location,
            ErrorCode.VARIABLE_NOT_FOUND,
            "Unknown enum variant '{s}' for enum '{s}'",
            .{ variant_token.lexeme, @import("../../../module/graph.zig").displayName(enum_key) },
        );
        return ErrorList.InvalidEnumVariant;
    }

    /// Generate HIR for field assignment expressions
    pub fn generateFieldAssignment(self: *StructsHandler, field_assign: ast.Expr.Data) !void {
        const assign_data = field_assign.FieldAssignment;

        // Check if this is a nested field assignment (e.g., mike.person.age is 26)
        if (assign_data.object.data == .FieldAccess) {
            // This is a nested field assignment - handle it specially
            const outer_field = assign_data.object.data.FieldAccess;

            // Generate code to load base variable, modify nested field, and store back
            // For mike.person.age is 26:
            // 1. Load mike
            // 2. Get person field
            // 3. Duplicate it
            // 4. Generate value (26)
            // 5. Set age field on the duplicate
            // 6. Store the modified person back to mike.person

            // Generate base object (mike)
            try self.generator.generateExpression(outer_field.object, true, false);

            // Get the outer field (person)
            const outer_slot = try self.fieldSlot(outer_field.object, outer_field.field);
            const outer_container_type = self.generator.inferTypeFromExpression(outer_field.object);
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = outer_container_type,
                    .struct_id = outer_slot.struct_id,
                    .field_index = outer_slot.index,
                    .field_type = .Unknown,
                    .field_for_peek = false,
                    .nested_struct_id = null,
                },
            });

            // Duplicate the nested struct so we can modify it
            try self.generator.instructions.append(.Dup);

            // Generate value expression (26)
            try self.generator.generateExpression(assign_data.value, true, false);

            // Set the inner field (age) on the duplicate
            const inner_slot = try self.fieldSlot(assign_data.object, assign_data.field);
            const inner_container_type = self.generator.inferTypeFromExpression(assign_data.object);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = inner_container_type,
                    .struct_id = inner_slot.struct_id,
                    .field_index = inner_slot.index,
                    .field_type = .Unknown,
                    .nested_struct_id = null,
                },
            });

            // Now we need to store the modified nested struct back to the original
            // Generate base object again (mike or this)
            try self.generator.generateExpression(outer_field.object, true, false);

            // Swap the modified nested struct to the top of the stack
            try self.generator.instructions.append(.Swap);

            // Set the outer field (person) with the modified struct
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = outer_container_type,
                    .struct_id = outer_slot.struct_id,
                    .field_index = outer_slot.index,
                    .field_type = .Unknown,
                    .nested_struct_id = null,
                },
            });

            // Store the result back to the base variable/alias
            switch (outer_field.object.data) {
                .Variable => |tok| {
                    const var_name = tok.lexeme;
                    const var_index = try self.generator.getOrCreateVariable(var_name);
                    const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                    try self.generator.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_index,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = expected_type,
                            .heap_copy = .keep,
                        },
                    });
                },
                .This => {
                    const var_index = try self.generator.getOrCreateVariable("this");
                    // 'this' is always a struct alias in instance methods
                    try self.generator.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_index,
                            .var_name = "this",
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = HIRType{ .Struct = 0 },
                            .heap_copy = .keep,
                        },
                    });
                },
                else => {},
            }
        } else {
            const is_this_target = assign_data.object.data == .This;

            if (is_this_target) {
                try self.generator.generateExpression(assign_data.value, true, false);
                try self.generator.generateExpression(assign_data.object, true, false);
                try self.generator.instructions.append(.Swap);
            } else {
                try self.generator.generateExpression(assign_data.object, true, false);
                try self.generator.generateExpression(assign_data.value, true, false);
            }

            const slot = try self.fieldSlot(assign_data.object, assign_data.field);
            const assign_container_type = self.generator.inferTypeFromExpression(assign_data.object);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = assign_container_type,
                    .struct_id = slot.struct_id,
                    .field_index = slot.index,
                    .field_type = .Unknown,
                    .nested_struct_id = null,
                },
            });

            // If assigning to a variable/alias field, persist the modified struct back
            switch (assign_data.object.data) {
                .Variable => |tok| {
                    const var_name = tok.lexeme;
                    const var_index = try self.generator.getOrCreateVariable(var_name);
                    const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                    try self.generator.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_index,
                            .var_name = var_name,
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = expected_type,
                            .heap_copy = .keep,
                        },
                    });
                },
                .This => {
                    if (self.generator.symbol_table.isAliasParameter("this")) {
                        if (self.generator.slot_manager.getAliasSlot("this")) |alias_slot| {
                            try self.generator.instructions.append(.{
                                .StoreAlias = .{
                                    .slot_index = alias_slot,
                                    .var_name = "this",
                                    .expected_type = HIRType{ .Struct = 0 },
                                },
                            });
                        } else {
                            const var_index = try self.generator.getOrCreateVariable("this");
                            try self.generator.instructions.append(.{
                                .StoreVar = .{
                                    .var_index = var_index,
                                    .var_name = "this",
                                    .scope_kind = .Local,
                                    .module_context = null,
                                    .expected_type = HIRType{ .Struct = 0 },
                                    .heap_copy = .keep,
                                },
                            });
                        }
                    } else {
                        const var_index = try self.generator.getOrCreateVariable("this");
                        try self.generator.instructions.append(.{
                            .StoreVar = .{
                                .var_index = var_index,
                                .var_name = "this",
                                .scope_kind = .Local,
                                .module_context = null,
                                .expected_type = HIRType{ .Struct = 0 },
                                .heap_copy = .keep,
                            },
                        });
                    }
                },
                else => {},
            }
        }
    }

    /// A type declaration is compile time only: its type was registered by
    /// analysis. As an expression statement it yields `nothing`.
    pub fn generateTypeDecl(self: *StructsHandler) !void {
        const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
    }
};

/// Look up a struct-literal field by name, returning its value expression.
fn findLiteralField(literal_fields: []const *ast.StructInstanceField, name: []const u8) ?*ast.StructInstanceField {
    for (literal_fields) |lit_field| {
        if (std.mem.eql(u8, lit_field.name.lexeme, name)) return lit_field;
    }
    return null;
}
