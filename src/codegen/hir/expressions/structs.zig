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

    /// Generate HIR for field access expressions
    pub fn generateFieldAccess(self: *StructsHandler, field: ast.FieldAccess) !void {
        // Handle module namespace field access like g.raylib
        if (field.object.data == .Variable) {
            const obj_name = field.object.data.Variable.lexeme;
            if (self.generator.isModuleNamespace(obj_name)) {
                // Handle g.raylib or g.doxa - these are module namespace references
                if (self.generator.module_namespaces.get(obj_name)) |mi| {
                    const root_name = if (mi.name.len > 0) mi.name else mi.file_path; // e.g., "graphics" or "graphics.raylib"
                    if (std.mem.eql(u8, root_name, "graphics")) {
                        const sub_ns = field.field.lexeme; // e.g., "raylib" or "doxa"
                        if (std.mem.eql(u8, sub_ns, "raylib") or std.mem.eql(u8, sub_ns, "doxa")) {
                            // For module namespace field access like g.raylib, we need to handle this differently
                            // Instead of pushing a constant and then doing field access, we need to generate
                            // a special instruction that the VM can handle for module namespace field access
                            const full_name = try std.fmt.allocPrint(self.generator.allocator, "graphics.{s}", .{sub_ns});
                            const const_idx = try self.generator.addConstant(HIRValue{ .string = full_name });
                            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = full_name }, .constant_id = const_idx } });
                            return;
                        }
                    } else if (std.mem.startsWith(u8, root_name, "graphics.")) {
                        // Handle aliased module namespace field access like rl.SKYBLUE
                        // where rl is aliased to graphics.raylib
                        const full_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ root_name, field.field.lexeme });
                        const const_idx = try self.generator.addConstant(HIRValue{ .string = full_name });
                        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = full_name }, .constant_id = const_idx } });
                        return;
                    }
                }
            }
        }

        // Handle nested module constants: graphics.raylib.SKYBLUE
        if (field.object.data == .FieldAccess) {
            const inner = field.object.data.FieldAccess;
            if (inner.object.data == .Variable and self.generator.isModuleNamespace(inner.object.data.Variable.lexeme)) {
                if (self.generator.module_namespaces.get(inner.object.data.Variable.lexeme)) |mi| {
                    const root_name = if (mi.name.len > 0) mi.name else mi.file_path; // e.g., "graphics"
                    const sub_ns = inner.field.lexeme; // e.g., "raylib"
                    if (std.mem.eql(u8, root_name, "graphics") and std.mem.eql(u8, sub_ns, "raylib")) {
                        const full_name = try std.fmt.allocPrint(self.generator.allocator, "graphics.raylib.{s}", .{field.field.lexeme});
                        const const_idx = try self.generator.addConstant(HIRValue{ .string = full_name });
                        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = full_name }, .constant_id = const_idx } });
                        return;
                    }
                }
            }
        }

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

            // Now, the original logic for FieldAccess (non-enum)
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = field.field.lexeme,
                    .container_type = obj_type, // Use the inferred object type
                    .field_index = 0, // VM will resolve
                    .field_for_peek = false, // Default
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
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = .Struct,
                    .field_index = 0,
                    .field_for_peek = false,
                },
            });

            // Duplicate the nested struct so we can modify it
            try self.generator.instructions.append(.Dup);

            // Generate value expression (26)
            try self.generator.generateExpression(assign_data.value, true, false);

            // Set the inner field (age) on the duplicate
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = .Struct,
                    .field_index = 0,
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
                    .container_type = .Struct,
                    .field_index = 0,
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
                            .expected_type = .Struct,
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

            // Generate SetField instruction
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = .Struct,
                    .field_index = 0, // Index will be resolved by VM
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
                            .expected_type = .Struct,
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
        try self.generator.trackVariableType(enum_data.name.lexeme, .Enum);

        // Create a special enum type value and store it
        const enum_type_value = HIRValue{ .string = enum_data.name.lexeme }; // Simple representation for now
        const const_idx = try self.generator.addConstant(enum_type_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
        try self.generator.instructions.append(.{ .StoreConst = .{
            .var_index = var_idx,
            .var_name = enum_data.name.lexeme,
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
