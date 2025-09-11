const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const Variable = @import("../../utils/memory.zig").Variable;
const import_parser = @import("../../parser/import_parser.zig");
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const CustomTypeInfo = @import("semantic.zig").SemanticAnalyzer.CustomTypeInfo;
const Memory = @import("../../utils/memory.zig");
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;

/// Helper function to get location from AST Base, handling optional spans
pub fn getLocationFromBase(base: ast.Base) Reporting.Location {
    if (base.span) |span| {
        return span.location;
    } else {
        // Synthetic node - return default location
        return .{
            .file = "",
            .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
        };
    }
}

pub fn unifyTypes(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
    // Handle union types first - check if actual type is compatible with the expected union
    if (expected.base == .Union) {
        if (expected.union_type) |exp_union| {
            if (actual.base == .Union) {
                // Union-to-union compatibility: every member of actual must be allowed by expected
                if (actual.union_type) |act_union| {
                    for (act_union.types) |act_member| {
                        var member_allowed = false;
                        for (exp_union.types) |exp_member| {
                            if (exp_member.base == act_member.base) {
                                member_allowed = true;
                                break;
                            }
                        }
                        if (!member_allowed) {
                            // Build expected list for error message
                            var type_list = std.ArrayList(u8).init(self.allocator);
                            defer type_list.deinit();
                            for (exp_union.types, 0..) |m, i| {
                                if (i > 0) try type_list.appendSlice(" | ");
                                try type_list.appendSlice(@tagName(m.base));
                            }
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.TYPE_MISMATCH,
                                "Type mismatch: expected union ({s}), got {s}",
                                .{ type_list.items, @tagName(act_member.base) },
                            );
                            self.fatal_error = true;
                            return;
                        }
                    }
                    return; // All actual members are allowed by expected
                }
                // If actual has no union_type detail, accept conservatively
                return;
            } else {
                // Non-union on the right: check membership in expected union
                var found_match = false;
                for (exp_union.types) |member_type| {
                    if (member_type.base == actual.base) {
                        found_match = true;
                        break;
                    }
                    // Allow Enum types to be compatible with Custom types when they refer to the same enum
                    if (member_type.base == .Custom and actual.base == .Enum) {
                        if (member_type.custom_type) |custom_name| {
                            // Check if this is the same enum type
                            if (self.custom_types.get(custom_name)) |custom_type| {
                                if (custom_type.kind == .Enum) {
                                    found_match = true;
                                    break;
                                }
                            }
                        }
                    }
                }
                if (found_match) {
                    return;
                }

                // Build a list of allowed types for the error message
                var type_list = std.ArrayList(u8).init(self.allocator);
                defer type_list.deinit();
                for (exp_union.types, 0..) |member_type, i| {
                    if (i > 0) try type_list.appendSlice(" | ");
                    try type_list.appendSlice(@tagName(member_type.base));
                }
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "Type mismatch: expected union ({s}), got {s}",
                    .{ type_list.items, @tagName(actual.base) },
                );
                self.fatal_error = true;
                return;
            }
        }
    }

    if (expected.base != actual.base) {

        // Special cases for type conversions
        if (expected.base == .Float and (actual.base == .Int or actual.base == .Byte)) {
            return; // Allow implicit int/byte to float
        }
        if (expected.base == .Byte and actual.base == .Int) {
            return; // Allow implicit int to byte
        }
        if (expected.base == .Int and actual.base == .Byte) {
            return; // Allow byte to int conversion (always safe)
        }
        // Allow enum variant literals (Enum) to be assigned to variables of enum types (Custom)
        if (expected.base == .Custom and actual.base == .Enum) {
            return; // Allow enum variant literals to be assigned to enum type variables
        }
        // Allow struct literals (Custom) to be assigned to variables of struct types (Custom)
        if (expected.base == .Custom and actual.base == .Custom) {
            return; // Allow struct literals to be assigned to struct type variables
        }
        // Allow Custom and Struct types to be compatible when they refer to the same user-defined struct
        if ((expected.base == .Custom and actual.base == .Struct) or
            (expected.base == .Struct and actual.base == .Custom))
        {
            // Check if they refer to the same custom type
            if (expected.custom_type != null and actual.custom_type != null and
                std.mem.eql(u8, expected.custom_type.?, actual.custom_type.?))
            {
                return; // They refer to the same custom type
            }
        }

        self.reporter.reportCompileError(
            span.location,
            ErrorCode.TYPE_MISMATCH,
            "Type mismatch: expected {s}, got {s}",
            .{ @tagName(expected.base), @tagName(actual.base) },
        );
        self.fatal_error = true;
    } else {}

    // Handle complex type unification (arrays, structs, etc)
    switch (expected.base) {
        .Array => {
            if (expected.array_type) |exp_elem| {
                if (actual.array_type) |act_elem| {
                    try unifyTypes(self, exp_elem, act_elem, span);
                }
            }
        },
        .Struct, .Custom => {
            if (expected.struct_fields) |exp_fields| {
                if (actual.struct_fields) |act_fields| {
                    if (exp_fields.len != act_fields.len) {
                        self.reporter.reportCompileError(
                            span.location,
                            ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                            "Struct field count mismatch: expected {}, got {}",
                            .{ exp_fields.len, act_fields.len },
                        );
                        self.fatal_error = true;
                        return;
                    }
                    for (exp_fields, act_fields) |exp_field, act_field| {
                        if (!std.mem.eql(u8, exp_field.name, act_field.name)) {
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                                "Struct field name mismatch: expected '{s}', got '{s}'",
                                .{ exp_field.name, act_field.name },
                            );
                            self.fatal_error = true;
                            return;
                        }
                        try unifyTypes(self, exp_field.type_info, act_field.type_info, span);
                    }
                }
            }
        },
        else => {},
    }
}

pub fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
    if (self.current_scope) |scope| {
        const result = scope.lookupVariable(name);
        if (result != null) return result;
    }

    // Check for imported symbols if not found in scope
    if (self.parser) |parser| {
        if (parser.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(name)) |imported_symbol| {
                // Create a variable for the imported symbol
                return createImportedSymbolVariable(self, name, imported_symbol);
            }
        }

        // Check for module namespaces
        if (parser.module_namespaces.contains(name)) {

            // Create a variable for the module namespace
            return createModuleNamespaceVariable(self, name);
        }
    }

    return null;
}

pub fn createModuleNamespaceVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
    // Create a TypeInfo for the module namespace
    const type_info = self.allocator.create(ast.TypeInfo) catch return null;
    errdefer self.allocator.destroy(type_info);

    type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false, .custom_type = name };

    // Convert TypeInfo to TokenType
    const token_type = convertTypeToTokenType(self, type_info.base);

    // Create a Variable object for the module namespace using the scope's createValueBinding
    if (self.current_scope) |scope| {
        // Create a placeholder value for the module namespace
        const placeholder_value = TokenLiteral{ .nothing = {} };

        const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
        return variable;
    }

    return null;
}

pub fn isModuleNamespace(self: *SemanticAnalyzer, name: []const u8) bool {
    if (self.parser) |parser| {
        if (parser.module_namespaces.contains(name)) {
            return true;
        }
    }
    return false;
}

pub fn handleModuleFieldAccess(self: *SemanticAnalyzer, module_name: []const u8, field_name: []const u8, span: ast.SourceSpan) !*ast.TypeInfo {
    var type_info = try self.allocator.create(ast.TypeInfo);
    errdefer self.allocator.destroy(type_info);

    if (self.parser) |parser| {
        // Look for the field in the module's imported symbols
        if (parser.imported_symbols) |imported_symbols| {
            const full_name = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name }) catch {
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.INTERNAL_ERROR,
                    "Internal error: Could not format module field name",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            };
            defer self.allocator.free(full_name);

            if (imported_symbols.get(full_name)) |imported_symbol| {
                // Return appropriate type based on the imported symbol kind
                switch (imported_symbol.kind) {
                    .Function => {
                        // Create return type
                        const return_type = self.allocator.create(ast.TypeInfo) catch return type_info;
                        return_type.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };

                        // Create function type
                        const function_type = self.allocator.create(ast.FunctionType) catch return type_info;
                        function_type.* = ast.FunctionType{
                            .params = &[_]ast.TypeInfo{},
                            .return_type = return_type,
                        };

                        type_info.* = ast.TypeInfo{
                            .base = .Function,
                            .is_mutable = false,
                            .function_type = function_type,
                        };
                    },
                    .Variable => type_info.* = ast.TypeInfo{ .base = .Int, .is_mutable = false },
                    .Struct => type_info.* = ast.TypeInfo{ .base = .Struct, .is_mutable = false },
                    .Enum => type_info.* = ast.TypeInfo{ .base = .Enum, .is_mutable = false },
                    .Type => type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false },
                    .Import => type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false },
                }
                return type_info;
            }
        }
    }

    // Field not found in module
    self.reporter.reportCompileError(
        span.location,
        ErrorCode.FIELD_NOT_FOUND,
        "Field '{s}' not found in module '{s}'",
        .{ field_name, module_name },
    );
    self.fatal_error = true;
    type_info.base = .Nothing;
    return type_info;
}

pub fn createImportedSymbolVariable(self: *SemanticAnalyzer, name: []const u8, imported_symbol: import_parser.ImportedSymbol) ?*Variable {
    // Create a TypeInfo based on the imported symbol kind
    const type_info = self.allocator.create(ast.TypeInfo) catch return null;
    errdefer self.allocator.destroy(type_info);

    const ti = switch (imported_symbol.kind) {
        .Function => blk: {
            // Build a best-effort function type using imported metadata when available;
            // fall back to scanning module_namespaces.
            var built_func_type: ?*ast.FunctionType = null;

            // If import captured param_count/return_type_info, prefer that
            const maybe_param_count = imported_symbol.param_count;
            const maybe_return_info = imported_symbol.return_type_info;
            if (maybe_param_count != null or maybe_return_info != null) {
                const pc: usize = if (maybe_param_count) |x| @intCast(x) else 0;
                if (self.allocator.alloc(ast.TypeInfo, pc) catch null) |params_buf| {
                    // Default parameter types to Nothing when unknown
                    for (params_buf) |*ti| ti.* = ast.TypeInfo{ .base = .Nothing };
                    if (self.allocator.create(ast.TypeInfo) catch null) |ret_ptr| {
                        ret_ptr.* = if (maybe_return_info) |ri| ri else ast.TypeInfo{ .base = .Nothing };
                        if (self.allocator.create(ast.FunctionType) catch null) |ft_ptr| {
                            ft_ptr.* = ast.FunctionType{ .params = params_buf, .return_type = ret_ptr };
                            built_func_type = ft_ptr;
                        }
                    }
                }
            }

            if (built_func_type == null) {
                if (self.parser) |parser| {
                    var it = parser.module_namespaces.iterator();
                    while (it.next()) |entry| {
                        const module_info = entry.value_ptr.*;
                        if (module_info.ast) |module_ast| {
                            if (module_ast.data == .Block) {
                                const stmts = module_ast.data.Block.statements;
                                for (stmts) |s| {
                                    switch (s.data) {
                                        .FunctionDecl => |f| {
                                            if (!f.is_public) continue;
                                            if (!std.mem.eql(u8, f.name.lexeme, name)) continue;
                                            // Found matching function; construct FunctionType from its params/return
                                            const ft = self.allocator.create(ast.FunctionType) catch break;
                                            // Duplicate param TypeInfos into a flat slice as FunctionType expects []TypeInfo
                                            var params_list = std.ArrayList(ast.TypeInfo).init(self.allocator);
                                            errdefer params_list.deinit();
                                            for (f.params) |p| {
                                                const ti_ptr = if (p.type_expr) |texpr| (self.typeExprToTypeInfo(texpr) catch null) else null;
                                                const ti = if (ti_ptr) |tmp| tmp.* else ast.TypeInfo{ .base = .Nothing };
                                                params_list.append(ti) catch break;
                                            }
                                            const params_slice = params_list.toOwnedSlice() catch break;
                                            const ret_ptr = self.allocator.create(ast.TypeInfo) catch break;
                                            ret_ptr.* = f.return_type_info;
                                            ft.* = ast.FunctionType{ .params = params_slice, .return_type = ret_ptr };
                                            built_func_type = ft;
                                            break;
                                        },
                                        else => {},
                                    }
                                }
                            }
                        }
                        if (built_func_type != null) break;
                    }
                }
            }

            // Fallback if not found: zero params and int return (legacy behavior)
            if (built_func_type == null) {
                const return_type = self.allocator.create(ast.TypeInfo) catch return null;
                return_type.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };
                const function_type = self.allocator.create(ast.FunctionType) catch return null;
                function_type.* = ast.FunctionType{ .params = &[_]ast.TypeInfo{}, .return_type = return_type };
                built_func_type = function_type;
            }

            break :blk ast.TypeInfo{ .base = .Function, .is_mutable = false, .function_type = built_func_type };
        },
        .Variable => ast.TypeInfo{ .base = .Int, .is_mutable = false },
        .Struct => ast.TypeInfo{ .base = .Struct, .is_mutable = false },
        .Enum => ast.TypeInfo{ .base = .Enum, .is_mutable = false },
        .Type => ast.TypeInfo{ .base = .Custom, .is_mutable = false },
        .Import => ast.TypeInfo{ .base = .Custom, .is_mutable = false },
    };

    type_info.* = ti;

    // Convert TypeInfo to TokenType
    const token_type = convertTypeToTokenType(self, type_info.base);

    // Create a Variable object for the imported symbol using the scope's createValueBinding
    if (self.current_scope) |scope| {
        // Create a placeholder value for the imported symbol
        const placeholder_value = TokenLiteral{ .nothing = {} };

        const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
        return variable;
    }

    return null;
}

pub fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) TokenType {
    _ = self;
    return switch (base_type) {
        .Int => .INT,
        .Float => .FLOAT,
        .String => .STRING,
        .Tetra => .TETRA,
        .Byte => .BYTE,
        .Array => .ARRAY,
        .Function => .FUNCTION,
        .Struct => .STRUCT,
        .Nothing => .NOTHING,
        .Map => .MAP,
        .Custom => .CUSTOM,
        .Enum => .ENUM,
        .Union => .UNION,
    };
}

pub fn createUnionType(self: *SemanticAnalyzer, types: []*ast.TypeInfo) !*ast.TypeInfo {
    // Flatten any existing unions in the input types
    var flat_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
    defer flat_types.deinit();

    for (types) |type_info| {
        if (type_info.base == .Union) {
            if (type_info.union_type) |union_type| {
                const flattened = try flattenUnionType(self, union_type);
                for (flattened.types) |member| {
                    // Check for duplicates
                    var is_duplicate = false;
                    for (flat_types.items) |existing| {
                        if (existing.base == member.base) {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if (!is_duplicate) {
                        try flat_types.append(member);
                    }
                }
            }
        } else {
            // Check for duplicates
            var is_duplicate = false;
            for (flat_types.items) |existing| {
                if (existing.base == type_info.base) {
                    is_duplicate = true;
                    break;
                }
            }
            if (!is_duplicate) {
                try flat_types.append(type_info);
            }
        }
    }

    // If only one type remains, return it directly (no need for union)
    if (flat_types.items.len == 1) {
        return flat_types.items[0];
    }

    // Create union type
    const union_type = try self.allocator.create(ast.UnionType);
    union_type.* = .{
        .types = try flat_types.toOwnedSlice(),
        .current_type_index = 0,
    };

    const type_info = try self.allocator.create(ast.TypeInfo);
    type_info.* = .{
        .base = .Union,
        .union_type = union_type,
        .is_mutable = false,
    };

    return type_info;
}

pub fn unionContainsNothing(self: *SemanticAnalyzer, union_type_info: ast.TypeInfo) bool {
    _ = self;
    if (union_type_info.base != .Union) return false;
    if (union_type_info.union_type) |union_type| {
        for (union_type.types) |member_type| {
            if (member_type.base == .Nothing) return true;
        }
    }
    return false;
}

pub fn registerCustomType(self: *SemanticAnalyzer, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, type_name),
        .kind = kind,
    };
    try self.custom_types.put(type_name, custom_type);
}

pub fn registerEnumType(self: *SemanticAnalyzer, enum_name: []const u8, variants: []const []const u8) !void {
    var enum_variants = try self.allocator.alloc(CustomTypeInfo.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, index| {
        enum_variants[index] = CustomTypeInfo.EnumVariant{
            .name = try self.allocator.dupe(u8, variant_name),
            .index = @intCast(index),
        };
    }

    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, enum_name),
        .kind = .Enum,
        .enum_variants = enum_variants,
    };
    try self.custom_types.put(enum_name, custom_type);

    // Also register into runtime memory manager for VM access
    var mem_enum_variants = try self.allocator.alloc(Memory.CustomTypeInfo.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, i| {
        mem_enum_variants[i] = Memory.CustomTypeInfo.EnumVariant{
            .name = try self.allocator.dupe(u8, variant_name),
            .index = @intCast(i),
        };
    }
    const mem_enum = Memory.CustomTypeInfo{
        .name = try self.allocator.dupe(u8, enum_name),
        .kind = .Enum,
        .enum_variants = mem_enum_variants,
        .struct_fields = null,
    };
    try self.memory.registerCustomType(mem_enum);
}

pub fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
    // If a richer struct definition already exists (e.g., from a StructDecl with visibility),
    // do not overwrite it in the semantic registry. Still register to runtime memory manager below.
    const already_registered = self.custom_types.contains(struct_name);

    var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
    for (fields, 0..) |field, index| {
        // Convert ast.Type to HIRType
        var custom_type_name: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            custom_type_name = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        // Preserve full field type info (no lossy HIR conversion here)
        const full_type_info = try self.allocator.create(ast.TypeInfo);
        full_type_info.* = field.type_info.*;
        struct_fields[index] = CustomTypeInfo.StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type_info = full_type_info,
            .custom_type_name = custom_type_name,
            .index = @intCast(index),
            .is_public = false,
        };
    }

    if (!already_registered) {
        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
    }

    // Also register into runtime memory manager for VM access
    var mem_fields = try self.allocator.alloc(Memory.CustomTypeInfo.StructField, fields.len);
    for (fields, 0..) |field, i| {
        const mapped_hir_type: HIRType = switch (field.type_info.base) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            .Array => .Array,
            .Struct, .Custom => .Struct,
            .Map => .Map,
            .Enum => .Enum,
            .Function => .Function,
            .Union => .Union,
        };
        var ctn: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            ctn = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        mem_fields[i] = Memory.CustomTypeInfo.StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type = mapped_hir_type,
            .custom_type_name = ctn,
            .index = @intCast(i),
        };
    }
    const mem_struct = Memory.CustomTypeInfo{
        .name = try self.allocator.dupe(u8, struct_name),
        .kind = .Struct,
        .enum_variants = null,
        .struct_fields = mem_fields,
    };
    try self.memory.registerCustomType(mem_struct);
}

pub fn flattenUnionType(self: *SemanticAnalyzer, union_type: *ast.UnionType) !*ast.UnionType {
    var flat_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
    defer flat_types.deinit();

    for (union_type.types) |member_type| {
        if (member_type.base == .Union) {
            if (member_type.union_type) |nested_union| {
                // Recursively flatten nested union
                const flattened_nested = try flattenUnionType(self, nested_union);
                for (flattened_nested.types) |nested_member| {
                    // Check for duplicates before adding
                    var is_duplicate = false;
                    for (flat_types.items) |existing_type| {
                        if (existing_type.base == nested_member.base) {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if (!is_duplicate) {
                        try flat_types.append(nested_member);
                    }
                }
            }
        } else {
            // Check for duplicates before adding
            var is_duplicate = false;
            for (flat_types.items) |existing_type| {
                if (existing_type.base == member_type.base) {
                    is_duplicate = true;
                    break;
                }
            }
            if (!is_duplicate) {
                try flat_types.append(member_type);
            }
        }
    }

    const flattened_union = try self.allocator.create(ast.UnionType);
    flattened_union.* = .{
        .types = try flat_types.toOwnedSlice(),
        .current_type_index = union_type.current_type_index,
    };

    return flattened_union;
}
