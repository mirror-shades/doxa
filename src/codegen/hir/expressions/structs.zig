const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;

/// Handle struct operations, field access, and type declarations
pub const StructsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) StructsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for struct literal expressions
    pub fn generateStructLiteral(self: *StructsHandler, struct_lit: ast.Expr.Data) !void {
        const struct_data = struct_lit.StructLiteral;

        // Track field types for type checking
        var field_types = try self.generator.allocator.alloc(HIRType, struct_data.fields.len);
        defer self.generator.allocator.free(field_types);

        // Generate field values and names in reverse order for stack-based construction
        var reverse_i = struct_data.fields.len;
        while (reverse_i > 0) {
            reverse_i -= 1;
            const field = struct_data.fields[reverse_i];

            // If we know the struct type and this field's declared custom type name refers
            // to an enum, set the enum context so `.FOO` lowers to an enum value, not string
            const previous_enum_context = self.generator.current_enum_type;
            if (self.generator.type_system.custom_types.get(struct_data.name.lexeme)) |ctype| {
                if (ctype.kind == .Struct) {
                    if (ctype.struct_fields) |cfields| {
                        // Find matching field by name
                        for (cfields) |cf| {
                            if (std.mem.eql(u8, cf.name, field.name.lexeme)) {
                                if (cf.custom_type_name) |ct_name| {
                                    if (self.generator.type_system.custom_types.get(ct_name)) |maybe_enum| {
                                        if (maybe_enum.kind == .Enum) {
                                            self.generator.current_enum_type = ct_name;
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
            }

            // Generate field value with possible enum context
            try self.generator.generateExpression(field.value, true, false);
            // Restore enum context
            self.generator.current_enum_type = previous_enum_context;

            // Infer and store field type
            field_types[reverse_i] = self.generator.inferTypeFromExpression(field.value);

            // Push field name as constant
            const field_name_const = try self.generator.addConstant(HIRValue{ .string = field.name.lexeme });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = field.name.lexeme }, .constant_id = field_name_const } });
        }

        // Generate StructNew instruction with field types
        try self.generator.instructions.append(.{
            .StructNew = .{
                .type_name = struct_data.name.lexeme,
                .field_count = @intCast(struct_data.fields.len),
                .field_types = try self.generator.allocator.dupe(HIRType, field_types),
                .size_bytes = 0, // Size will be calculated by VM
            },
        });

        // Result is on the stack
    }

    /// Helper function to resolve field index and struct name from struct type and field name
    fn resolveFieldIndexAndStructName(self: *StructsHandler, object_expr: *ast.Expr, field_name: []const u8) struct { field_index: u32, struct_name: ?[]const u8 } {
        // First resolve the object's type to get the container struct name
        if (self.generator.type_system.resolveFieldAccessType(object_expr, &self.generator.symbol_table)) |resolve_result| {
            if (resolve_result.custom_type_name) |container_struct_name| {
                if (self.generator.type_system.custom_types.get(container_struct_name)) |custom_type| {
                    if (custom_type.kind == .Struct) {
                        if (custom_type.getStructFieldIndex(field_name)) |resolved_index| {
                            // Get the field's custom type name if it's a struct
                            var field_struct_name: ?[]const u8 = null;
                            if (custom_type.struct_fields) |fields| {
                                for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, field_name)) {
                                        field_struct_name = f.custom_type_name;
                                        break;
                                    }
                                }
                            }
                            return .{ .field_index = resolved_index, .struct_name = field_struct_name };
                        }
                    }
                }
            }
        }
        return .{ .field_index = 0, .struct_name = null }; // Fallback to 0 if resolution fails
    }

    /// Helper function to resolve field index from struct type and field name
    fn resolveFieldIndex(self: *StructsHandler, object_expr: *ast.Expr, field_name: []const u8) u32 {
        return self.resolveFieldIndexAndStructName(object_expr, field_name).field_index;
    }

    /// Generate HIR for field access expressions
    pub fn generateFieldAccess(self: *StructsHandler, field: ast.FieldAccess) !void {
        var handled_as_enum_member: bool = false;
        // Check the type of the object being accessed first
        const obj_type = self.generator.inferTypeFromExpression(field.object);

        if (field.object.data == .Variable) {
            const var_token = field.object.data.Variable;
            // Check if this variable name matches a registered enum type
            if (self.generator.type_system.custom_types.get(var_token.lexeme)) |custom_type| {
                if (custom_type.kind == .Enum) {
                    // This is Color.Blue syntax - generate enum variant
                    const variant_index = custom_type.getEnumVariantIndex(field.field.lexeme) orelse 0;

                    const enum_value = HIRValue{
                        .enum_variant = HIREnum{
                            .type_name = var_token.lexeme,
                            .variant_name = field.field.lexeme,
                            .variant_index = variant_index,
                            .path = null,
                        },
                    };
                    const const_idx = try self.generator.addConstant(enum_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = enum_value, .constant_id = const_idx } });
                    handled_as_enum_member = true;
                }
            }
        }

        if (!handled_as_enum_member) {
            // Handle enum member access (e.g., Color.Red)
            try self.generator.generateExpression(field.object, true, false);

            // Resolve the struct type name and field index
            const resolved = self.resolveFieldIndexAndStructName(field.object, field.field.lexeme);

            // Now, the original logic for FieldAccess (non-enum)
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = field.field.lexeme,
                    .container_type = obj_type, // Use the inferred object type
                    .field_index = resolved.field_index, // Resolved from type system
                    .field_for_peek = false, // Default
                    .field_struct_name = resolved.struct_name, // Struct name if field is a struct
                },
            });
        }
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
            const outer_resolved_get = self.resolveFieldIndexAndStructName(outer_field.object, outer_field.field.lexeme);
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = outer_resolved_get.field_index,
                    .field_for_peek = false,
                    .field_struct_name = outer_resolved_get.struct_name,
                },
            });

            // Duplicate the nested struct so we can modify it
            try self.generator.instructions.append(.Dup);

            // Generate value expression (26)
            try self.generator.generateExpression(assign_data.value, true, false);

            // Set the inner field (age) on the duplicate
            // For nested field access, we need to resolve the field index from the outer field's type
            var inner_field_index: u32 = 0;
            var inner_field_struct_name: ?[]const u8 = null;

            // Try to resolve from the outer field's type
            if (self.generator.type_system.resolveFieldAccessType(outer_field.object, &self.generator.symbol_table)) |resolve_result| {
                if (resolve_result.custom_type_name) |struct_name| {
                    if (self.generator.type_system.custom_types.get(struct_name)) |custom_type| {
                        if (custom_type.kind == .Struct) {
                            if (custom_type.struct_fields) |fields| {
                                for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, outer_field.field.lexeme)) {
                                        if (f.custom_type_name) |inner_struct_name| {
                                            if (self.generator.type_system.custom_types.get(inner_struct_name)) |inner_custom_type| {
                                                if (inner_custom_type.kind == .Struct) {
                                                    if (inner_custom_type.getStructFieldIndex(assign_data.field.lexeme)) |resolved_index| {
                                                        inner_field_index = resolved_index;
                                                        // Get the field's custom type name if it's a struct
                                                        if (inner_custom_type.struct_fields) |inner_fields| {
                                                            for (inner_fields) |inner_f| {
                                                                if (std.mem.eql(u8, inner_f.name, assign_data.field.lexeme)) {
                                                                    inner_field_struct_name = inner_f.custom_type_name;
                                                                    break;
                                                                }
                                                            }
                                                        }
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = inner_field_index,
                    .field_struct_name = inner_field_struct_name,
                },
            });

            // Now we need to store the modified nested struct back to the original
            // Generate base object again (mike or this)
            try self.generator.generateExpression(outer_field.object, true, false);

            // Swap the modified nested struct to the top of the stack
            try self.generator.instructions.append(.Swap);

            // Set the outer field (person) with the modified struct
            const outer_resolved_set = self.resolveFieldIndexAndStructName(outer_field.object, outer_field.field.lexeme);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = outer_resolved_set.field_index,
                    .field_struct_name = outer_resolved_set.struct_name,
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
                        },
                    });
                },
                else => {},
            }
        } else {
            // Regular field assignment - generate object expression
            try self.generator.generateExpression(assign_data.object, true, false);

            // Generate value expression
            try self.generator.generateExpression(assign_data.value, true, false);

            // Resolve field index and struct name from type system
            const resolved = self.resolveFieldIndexAndStructName(assign_data.object, assign_data.field.lexeme);

            // Generate SetField instruction
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = resolved.field_index,
                    .field_struct_name = resolved.struct_name,
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
                        },
                    });
                },
                .This => {
                    const var_index = try self.generator.getOrCreateVariable("this");
                    try self.generator.instructions.append(.{
                        .StoreVar = .{
                            .var_index = var_index,
                            .var_name = "this",
                            .scope_kind = .Local,
                            .module_context = null,
                            .expected_type = HIRType{ .Struct = 0 },
                        },
                    });
                },
                else => {},
            }
        }
    }

    /// Generate HIR for enum declarations
    pub fn generateEnumDecl(self: *StructsHandler, enum_decl: ast.Expr.Data) !void {
        const enum_data = enum_decl.EnumDecl;

        // NEW: Register enum type with variants for proper index calculation
        var variant_names = try self.generator.allocator.alloc([]const u8, enum_data.variants.len);
        for (enum_data.variants, 0..) |variant_token, i| {
            variant_names[i] = variant_token.lexeme;
        }
        try self.generator.registerEnumType(enum_data.name.lexeme, variant_names);

        // Register the enum type name as a special variable so Color.Red works
        const var_idx = try self.generator.getOrCreateVariable(enum_data.name.lexeme);
        try self.generator.trackVariableType(enum_data.name.lexeme, HIRType{ .Enum = 0 });

        // Create a special enum type value and store it
        const enum_type_value = HIRValue{ .string = enum_data.name.lexeme }; // Simple representation for now
        const const_idx = try self.generator.addConstant(enum_type_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
        try self.generator.instructions.append(.{ .StoreConst = .{
            .var_index = var_idx,
            .var_name = enum_data.name.lexeme,
            .scope_kind = self.generator.symbol_table.determineVariableScope(enum_data.name.lexeme),
            .module_context = null,
        } });
    }

    /// Generate HIR for struct declarations
    pub fn generateStructDecl(self: *StructsHandler, struct_decl: ast.Expr.Data) !void {
        const struct_data = struct_decl.StructDecl;

        // NEW: Register struct type with fields for proper field access
        var field_names = try self.generator.allocator.alloc([]const u8, struct_data.fields.len);
        for (struct_data.fields, 0..) |field_ptr, i| {
            field_names[i] = field_ptr.name.lexeme;
        }
        try self.generator.registerStructType(struct_data.name.lexeme, field_names);

        // Struct declarations don't generate runtime instructions, they're compile-time only
        // Push nothing as a placeholder value
        const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
    }
};
