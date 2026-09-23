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
const import_parser = @import("../../../parser/import_parser.zig");

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

    pub fn resolveStructIdFromType(self: *StructsHandler, container_type: HIRType, fallback_name: ?[]const u8) u32 {
        if (container_type == .Struct and container_type.Struct != 0) return container_type.Struct;
        if (fallback_name) |name| return self.resolveStructIdFromName(name);
        return 0;
    }

    /// Generate HIR for struct literal expressions
    pub fn generateStructLiteral(self: *StructsHandler, struct_lit: ast.Expr.Data) !void {
        const struct_data = struct_lit.StructLiteral;

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
            const ct = self.generator.type_system.custom_types.get(struct_data.name.lexeme) orelse break :blk null;
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
                try self.emitStructLiteralField(lit_field.value, decl_field.name, decl_field.field_type, decl_field.custom_type_name, reverse_i, field_types, field_names);
            }
        } else {
            // The declared shape is unknown, or the literal is invalid (errors are
            // already reported and abort the pipeline after HIR generation), so
            // fall back to emitting the literal's own field order.
            var reverse_i = struct_data.fields.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const field = struct_data.fields[reverse_i];
                const info = self.declaredFieldInfo(struct_data.name.lexeme, field.name.lexeme);
                const field_type = if (info) |i| i.field_type else HIRType{ .Unknown = {} };
                const custom_type_name = if (info) |i| i.custom_type_name else null;
                try self.emitStructLiteralField(field.value, field.name.lexeme, field_type, custom_type_name, reverse_i, field_types, field_names);
            }
        }

        // Generate StructNew instruction with field types
        const struct_id = self.resolveStructIdFromName(struct_data.name.lexeme);
        try self.generator.instructions.append(.{
            .StructNew = .{
                .type_name = struct_data.name.lexeme,
                .struct_id = struct_id,
                .field_count = @intCast(struct_data.fields.len),
                .field_names = try self.generator.allocator.dupe([]const u8, field_names),
                .field_types = try self.generator.allocator.dupe(HIRType, field_types),
            },
        });

        // Result is on the stack
    }

    /// Emit the value of a single struct-literal field, carrying any declared
    /// field type and enum context into the value expression, then record the
    /// field's type and name for the enclosing `StructNew`.
    fn emitStructLiteralField(
        self: *StructsHandler,
        value_expr: *ast.Expr,
        field_name: []const u8,
        field_type: HIRType,
        custom_type_name: ?[]const u8,
        reverse_i: usize,
        field_types: []HIRType,
        field_names: [][]const u8,
    ) ErrorList!void {
        // If this field's declared type refers to an enum, set the enum context so
        // `.FOO` lowers to an enum value rather than a string.
        const previous_enum_context = self.generator.current_enum_type;
        if (custom_type_name) |ct_name| {
            if (self.generator.type_system.custom_types.get(ct_name)) |maybe_enum| {
                if (maybe_enum.kind == .Enum) {
                    self.generator.current_enum_type = ct_name;
                }
            }
        }

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
        // Restore enum context
        self.generator.current_enum_type = previous_enum_context;

        field_types[reverse_i] = if (field_type != .Unknown and field_type != .Nothing)
            field_type
        else
            self.generator.inferTypeFromExpression(value_expr);
        field_names[reverse_i] = field_name;

        // Push field name as constant
        const field_name_const = try self.generator.addConstant(HIRValue{ .string = field_name });
        try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = field_name }, .constant_id = field_name_const } });
    }

    /// Look up a field's declared HIR type and custom-type name on a struct, when
    /// the struct's declaration is known.
    fn declaredFieldInfo(self: *StructsHandler, struct_name: []const u8, field_name: []const u8) ?struct { field_type: HIRType, custom_type_name: ?[]const u8 } {
        if (self.generator.type_system.custom_types.get(struct_name)) |ct| {
            if (ct.kind == .Struct) {
                if (ct.struct_fields) |fields| {
                    for (fields) |f| {
                        if (std.mem.eql(u8, f.name, field_name)) {
                            return .{ .field_type = f.field_type, .custom_type_name = f.custom_type_name };
                        }
                    }
                }
            }
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

    /// Helper function to resolve field index and struct name from struct type and field name
    pub fn resolveFieldIndexAndStructName(self: *StructsHandler, object_expr: *ast.Expr, field_name: []const u8) struct { field_index: u32, struct_name: ?[]const u8 } {
        // 1) Prefer a precise resolution path using the object's concrete struct name
        //    when we have one (plain struct variables, `this`, etc.).
        if (self.generator.type_system.resolveFieldAccessType(object_expr, &self.generator.symbol_table)) |resolve_result| {
            if (resolve_result.custom_type_name) |container_struct_name| {
                if (self.generator.type_system.custom_types.get(container_struct_name)) |custom_type| {
                    if (custom_type.kind == .Struct) {
                        if (custom_type.getStructFieldIndex(field_name)) |resolved_index| {
                            // Get the field's custom type name if it's itself a struct/enum.
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

        // 2) If we still don't know the container name, try the semantic struct table
        //    using the *inferred* type of the object expression. This is the key path
        //    for array-of-struct indexing like `zoo[0].name`, where `zoo[0]` has a
        //    StructId but no explicit custom-type tracking.
        if (self.generator.type_system.struct_table) |const_table| {
            const obj_type = self.generator.inferTypeFromExpression(object_expr);
            if (obj_type == .Struct) {
                const sid = obj_type.Struct;
                if (const_table.fields(sid)) |fields| {
                    for (fields) |f| {
                        if (std.mem.eql(u8, f.name, field_name)) {
                            return .{
                                .field_index = f.index,
                                // The semantic struct table does not currently track a
                                // separate custom type name for nested structs here; the
                                // HIR type system will recover that when needed.
                                .struct_name = null,
                            };
                        }
                    }
                }
            }
        }

        // 3) As an absolute last resort, fall back to a best-effort search across all
        //    known struct types. This keeps us from crashing in obscure cases, but the
        //    result may be imprecise, so it should be rare after the above attempts.
        var it = self.generator.type_system.custom_types.iterator();
        while (it.next()) |entry| {
            const ct = entry.value_ptr.*;
            if (ct.kind != .Struct or ct.struct_fields == null) continue;

            const fields = ct.struct_fields.?;
            for (fields) |f| {
                if (std.mem.eql(u8, f.name, field_name)) {
                    return .{
                        .field_index = f.index,
                        .struct_name = f.custom_type_name,
                    };
                }
            }
        }

        // Final fallback: index 0 with no struct name. This should be extremely rare
        // after the semantic-table and custom-type lookups, and only exists to keep
        // the compiler from crashing.
        return .{ .field_index = 0, .struct_name = null };
    }

    /// Helper function to resolve field index from struct type and field name
    fn resolveFieldIndex(self: *StructsHandler, object_expr: *ast.Expr, field_name: []const u8) u32 {
        return self.resolveFieldIndexAndStructName(object_expr, field_name).field_index;
    }

    /// Generate HIR for field access expressions
    pub fn generateFieldAccess(self: *StructsHandler, field: ast.FieldAccess) !void {
        var handled_as_enum_member: bool = false;

        // Resolve module-scoped chained accesses at compile time
        // (e.g. error.Common.InvalidArgument -> enum variant const)
        if (try self.resolveModuleChainedType(field)) |hir_value| {
            const const_idx = try self.generator.addConstant(hir_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = hir_value, .constant_id = const_idx } });
            return;
        }

        // Check the type of the object being accessed first
        const obj_type = self.generator.inferTypeFromExpression(field.object);

        if (field.object.data == .Variable) {
            const var_token = field.object.data.Variable;
            // Check if this variable name matches a registered enum type
            if (self.generator.type_system.custom_types.get(var_token.lexeme)) |custom_type| {
                if (custom_type.kind == .Enum) {
                    // This is Color.Blue syntax - generate enum variant
                    const variant_index = try self.resolveEnumVariantIndex(var_token.lexeme, field.field);

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

            // Try to resolve a precise field type for this access so that downstream
            // stages (LLVM + peek) know, for example, that `zoo[0].name` is a string
            // and `zoo[0].animal_type` is a concrete enum.
            var resolved_field_type: HIRType = .Unknown;
            // Prefer semantic struct-table metadata when the container is a struct;
            // this path is especially important for array-of-struct indexing where
            // the base expression is something like `zoo[0]`.
            if (obj_type == .Struct) {
                if (self.generator.type_system.struct_table) |const_table| {
                    const sid = obj_type.Struct;
                    if (const_table.fields(sid)) |fields| {
                        // First, trust the resolved.field_index when in range.
                        if (resolved.field_index < fields.len) {
                            resolved_field_type = fields[resolved.field_index].hir_type;
                        } else {
                            // Fallback: search by name to stay robust if the index
                            // ever drifts, rather than silently picking the wrong slot.
                            for (fields) |f| {
                                if (std.mem.eql(u8, f.name, field.field.lexeme)) {
                                    resolved_field_type = f.hir_type;
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            // As a secondary fallback, ask the type system's higher-level resolver
            // using a synthetic FieldAccess expression. This covers cases where
            // custom-type tracking (enums/nested structs) provides a more precise
            // HIRType than the raw struct-table entry alone.
            if (resolved_field_type == .Unknown) {
                var fake_expr = ast.Expr{
                    .base = undefined,
                    .data = .{ .FieldAccess = .{ .object = field.object, .field = field.field } },
                };
                if (self.generator.type_system.resolveFieldAccessType(&fake_expr, &self.generator.symbol_table)) |res| {
                    resolved_field_type = res.t;
                }
            }

            // Now, the original logic for FieldAccess (non-enum)
            const struct_id = self.resolveStructIdFromType(obj_type, resolved.struct_name);
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = field.field.lexeme,
                    .container_type = obj_type, // Use the inferred object type
                    .struct_id = struct_id,
                    .field_index = resolved.field_index, // Resolved from type system
                    .field_type = resolved_field_type,
                    .field_for_peek = false, // Default
                    .nested_struct_id = null,
                },
            });
        }
    }

    fn resolveEnumVariantIndex(self: *StructsHandler, enum_type_name: []const u8, variant_token: ast.Token) ErrorList!u32 {
        const location = Location{
            .file = variant_token.file,
            .file_uri = variant_token.file_uri,
            .range = .{
                .start_line = variant_token.line,
                .start_col = variant_token.column,
                .end_line = variant_token.line,
                .end_col = variant_token.column + variant_token.lexeme.len,
            },
        };

        if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type| {
            if (custom_type.kind != .Enum) {
                self.generator.reporter.reportCompileError(
                    location,
                    ErrorCode.TYPE_MISMATCH,
                    "'{s}' is not an enum type",
                    .{enum_type_name},
                );
                return ErrorList.TypeMismatch;
            }

            if (custom_type.getEnumVariantIndex(variant_token.lexeme)) |index| {
                return index;
            }

            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.VARIABLE_NOT_FOUND,
                "Unknown enum variant '{s}' for enum '{s}'",
                .{ variant_token.lexeme, enum_type_name },
            );
            return ErrorList.InvalidEnumVariant;
        }

        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.UNKNOWN_TYPE,
            "Unknown enum type '{s}'",
            .{enum_type_name},
        );
        return ErrorList.UnknownCustomType;
    }

    /// Walk a FieldAccess chain rooted at a module namespace (e.g. error.Common.InvalidArgument)
    /// and resolve the whole chain at compile time to either an enum variant value or a type
    /// reference. Returns null when the chain cannot be resolved this way and the standard
    /// LoadModule + GetField path should be used instead.
    fn resolveModuleChainedType(self: *StructsHandler, root_access: ast.FieldAccess) !?HIRValue {
        const allocator = self.generator.allocator;

        // Collect the field access chain from root to leaf (outermost to innermost)
        var chain = std.array_list.Managed([]const u8).init(allocator);
        defer chain.deinit();

        var current: *ast.Expr = root_access.object;
        try chain.append(root_access.field.lexeme); // leaf field

        while (current.data == .FieldAccess) {
            const inner_fa = current.data.FieldAccess;
            try chain.append(inner_fa.field.lexeme);
            current = inner_fa.object;
        }

        if (current.data != .Variable) return null;
        const module_name = current.data.Variable.lexeme;
        if (!self.generator.isModuleNamespace(module_name)) return null;

        // Reverse so we go from outermost to leaf: module_name -> field1 -> field2 ...
        std.mem.reverse([]const u8, chain.items);

        // Try to resolve using imported_symbols first
        if (self.generator.imported_symbols) |imported_symbols| {
            if (try self.resolveViaImportedSymbols(module_name, chain.items, imported_symbols)) |hv| {
                return hv;
            }
        }

        // Fallback: use custom_types (types registered by name, e.g. "Common", "IO")
        // This handles the case where the module's types were registered directly.
        if (chain.items.len >= 2) {
            const type_name = chain.items[0];
            if (self.generator.type_system.custom_types.get(type_name)) |ct| {
                if (ct.kind == .Enum) {
                    const variant_name = chain.items[1];
                    if (ct.getEnumVariantIndex(variant_name)) |variant_idx| {
                        return HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = type_name,
                                .variant_name = variant_name,
                                .variant_index = variant_idx,
                                .path = null,
                            },
                        };
                    }
                }
            }
        } else if (chain.items.len == 1) {
            const type_name = chain.items[0];
            if (self.generator.type_system.custom_types.get(type_name)) |_| {
                return HIRValue{ .string = type_name };
            }
        }

        return null;
    }

    fn resolveViaImportedSymbols(self: *StructsHandler, module_name: []const u8, chain: []const []const u8, imported_symbols: std.StringHashMap(import_parser.ImportedSymbol)) !?HIRValue {
        const allocator = self.generator.allocator;

        var accumulated = try std.array_list.Managed(u8).initCapacity(allocator, module_name.len + 128);
        defer accumulated.deinit();
        try accumulated.appendSlice(module_name);

        for (chain, 0..) |step, i| {
            try accumulated.appendSlice(".");
            try accumulated.appendSlice(step);
            const qualified = accumulated.items;

            if (i == chain.len - 1) {
                // Final step: could be a variant of the parent type
                if (chain.len >= 2) {
                    const last_dot = std.mem.lastIndexOfScalar(u8, qualified, '.') orelse continue;
                    const parent_qualified = qualified[0..last_dot];
                    if (imported_symbols.get(parent_qualified)) |parent_sym| {
                        if (parent_sym.kind == .Enum and parent_sym.enum_role == .Type) {
                            const etn = parent_sym.enum_type_name orelse continue;
                            if (self.generator.type_system.custom_types.get(etn)) |ct| {
                                if (ct.kind == .Enum) {
                                    if (ct.getEnumVariantIndex(step)) |variant_idx| {
                                        return HIRValue{
                                            .enum_variant = HIREnum{
                                                .type_name = etn,
                                                .variant_name = step,
                                                .variant_index = variant_idx,
                                                .path = null,
                                            },
                                        };
                                    }
                                }
                            }
                        }
                    }
                }
                // Check if the final step itself is a known type
                if (imported_symbols.get(qualified)) |sym| {
                    if (sym.enum_role == .Type) {
                        if (sym.kind == .Enum) {
                            return HIRValue{ .string = sym.enum_type_name orelse step };
                        } else if (sym.kind == .Group) {
                            return HIRValue{ .string = sym.name };
                        }
                    }
                }
            }
        }

        // Shortcut for single-level access: "module.field"
        if (chain.len == 1) {
            const leaf = chain[0];
            const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, leaf });
            defer allocator.free(qualified);
            if (imported_symbols.get(qualified)) |sym| {
                if (sym.enum_role == .Type) {
                    if (sym.kind == .Enum) {
                        return HIRValue{ .string = sym.enum_type_name orelse leaf };
                    } else if (sym.kind == .Group) {
                        return HIRValue{ .string = sym.name };
                    }
                }
            }
        }

        return null;
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
            const outer_container_type = self.generator.inferTypeFromExpression(outer_field.object);
            const outer_struct_id = self.resolveStructIdFromType(outer_container_type, outer_resolved_get.struct_name);
            try self.generator.instructions.append(.{
                .GetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = outer_container_type,
                    .struct_id = outer_struct_id,
                    .field_index = outer_resolved_get.field_index,
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
            // For nested field access, we need to resolve the field index from the outer field's type
            var inner_field_index: u32 = 0;
            var inner_field_struct_name: ?[]const u8 = null;
            var inner_container_name: ?[]const u8 = null;

            // Try to resolve from the outer field's type
            if (self.generator.type_system.resolveFieldAccessType(outer_field.object, &self.generator.symbol_table)) |resolve_result| {
                if (resolve_result.custom_type_name) |struct_name| {
                    if (self.generator.type_system.custom_types.get(struct_name)) |custom_type| {
                        if (custom_type.kind == .Struct) {
                            if (custom_type.struct_fields) |fields| {
                                for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, outer_field.field.lexeme)) {
                                        if (f.custom_type_name) |inner_struct_name| {
                                            inner_container_name = inner_struct_name;
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

            const inner_container_type = self.generator.inferTypeFromExpression(assign_data.object);
            const inner_struct_id = self.resolveStructIdFromType(inner_container_type, inner_container_name);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = inner_container_type,
                    .struct_id = inner_struct_id,
                    .field_index = inner_field_index,
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
            const outer_resolved_set = self.resolveFieldIndexAndStructName(outer_field.object, outer_field.field.lexeme);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = outer_field.field.lexeme,
                    .container_type = outer_container_type,
                    .struct_id = outer_struct_id,
                    .field_index = outer_resolved_set.field_index,
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

            // Resolve field index and struct name from type system
            const resolved = self.resolveFieldIndexAndStructName(assign_data.object, assign_data.field.lexeme);

            // Generate SetField instruction
            const assign_container_type = self.generator.inferTypeFromExpression(assign_data.object);
            const assign_struct_id = self.resolveStructIdFromType(assign_container_type, resolved.struct_name);
            try self.generator.instructions.append(.{
                .SetField = .{
                    .field_name = assign_data.field.lexeme,
                    .container_type = assign_container_type,
                    .struct_id = assign_struct_id,
                    .field_index = resolved.field_index,
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
        const enum_type_value = HIRValue{ .string = enum_data.name.lexeme }; // TODO: richer enum type representation
        const const_idx = try self.generator.addConstant(enum_type_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = enum_type_value, .constant_id = const_idx } });
        try self.generator.instructions.append(.{ .StoreDecl = .{
            .var_index = var_idx,
            .var_name = enum_data.name.lexeme,
            .scope_kind = self.generator.symbol_table.determineVariableScope(enum_data.name.lexeme),
            .module_context = null,
            .declared_type = HIRType{ .Enum = 0 },
            .is_const = true,
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

/// Look up a struct-literal field by name, returning its value expression.
fn findLiteralField(literal_fields: []const *ast.StructInstanceField, name: []const u8) ?*ast.StructInstanceField {
    for (literal_fields) |lit_field| {
        if (std.mem.eql(u8, lit_field.name.lexeme, name)) return lit_field;
    }
    return null;
}
