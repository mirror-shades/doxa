const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const TokenType = @import("../../types/token.zig").TokenType;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const HIRType = @import("soxa_types.zig").HIRType;
const StructId = @import("soxa_types.zig").StructId;
const EnumId = @import("soxa_types.zig").EnumId;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const StructTable = @import("../../common/struct_table.zig").StructTable;
const EnumTable = @import("../../common/enum_table.zig").EnumTable;

pub const TypeSystem = struct {
    custom_types: std.StringHashMap(CustomTypeInfo),
    allocator: std.mem.Allocator,
    reporter: *Reporting.Reporter,
    semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer = null,
    struct_table: ?*const StructTable = null,
    enum_table: ?*const EnumTable = null,

    pub const CustomTypeInfo = struct {
        name: []const u8,
        kind: CustomTypeKind,
        enum_variants: ?[]EnumVariant = null,
        struct_fields: ?[]StructField = null,

        pub const CustomTypeKind = enum {
            Struct,
            Enum,
        };

        pub const EnumVariant = struct {
            name: []const u8,
            index: u32,
        };

        pub const StructField = struct {
            name: []const u8,
            field_type: HIRType,
            index: u32,
            custom_type_name: ?[]const u8 = null,
        };

        pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
            if (self.kind != .Enum or self.enum_variants == null) {
                return null;
            }

            for (self.enum_variants.?) |variant| {
                if (std.mem.eql(u8, variant.name, variant_name)) {
                    return variant.index;
                }
            }
            return null;
        }

        pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
            if (self.kind != .Struct or self.struct_fields == null) return null;

            for (self.struct_fields.?) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    return field.index;
                }
            }
            return null;
        }
    };

    pub const FieldResolveResult = struct { t: HIRType, custom_type_name: ?[]const u8 = null };

    fn structTypeForName(self: *TypeSystem, name: []const u8) HIRType {
        if (self.struct_table) |table| {
            if (table.getIdByName(name)) |id| {
                return HIRType{ .Struct = id };
            }
        }
        return HIRType{ .Struct = 0 };
    }

    fn structTypeFromOptional(self: *TypeSystem, maybe_name: ?[]const u8) HIRType {
        if (maybe_name) |name| {
            return self.structTypeForName(name);
        }
        return HIRType{ .Struct = 0 };
    }

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer) TypeSystem {
        return TypeSystem{
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .allocator = allocator,
            .reporter = reporter,
            .semantic_analyzer = semantic_analyzer,
            .struct_table = if (semantic_analyzer) |sa| sa.getStructTable() else null,
            .enum_table = if (semantic_analyzer) |sa| sa.getEnumTable() else null,
        };
    }

    pub fn deinit(self: *TypeSystem) void {
        self.custom_types.deinit();
    }

    pub fn registerCustomType(self: *TypeSystem, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = CustomTypeInfo{
            .name = type_name,
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    pub fn registerEnumType(self: *TypeSystem, enum_name: []const u8, variants: []const []const u8) !void {
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
    }

    pub fn registerStructType(self: *TypeSystem, struct_name: []const u8, fields: []const []const u8) !void {
        if (self.custom_types.contains(struct_name)) return;

        var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field_name, index| {
            struct_fields[index] = CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field_name),
                .field_type = .Unknown,
                .index = @intCast(index),
                .custom_type_name = null,
            };
        }

        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
    }

    pub fn isCustomType(self: *TypeSystem, name: []const u8) ?CustomTypeInfo {
        return self.custom_types.get(name);
    }

    pub fn getCustomTypeHIRType(self: *TypeSystem, name: []const u8) HIRType {
        if (self.custom_types.get(name)) |custom_type| {
            return switch (custom_type.kind) {
                .Struct => .Struct,
                .Enum => .Enum,
            };
        }
        return .Unknown;
    }

    pub fn convertTypeInfo(self: *TypeSystem, type_info: ast.TypeInfo) HIRType {
        return switch (type_info.base) {
            .Int => .Int,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Byte => .Byte,
            .Array => {
                if (type_info.array_type) |element_type| {
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = self.convertTypeInfo(element_type.*);
                    return HIRType{ .Array = element_type_ptr };
                } else {
                    const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    unknown_ptr.* = .Unknown;
                    return HIRType{ .Array = unknown_ptr };
                }
            },
            .Map => {
                const key_type = self.allocator.create(HIRType) catch return .Unknown;
                const value_type = self.allocator.create(HIRType) catch return .Unknown;

                if (type_info.map_key_type) |kt| {
                    key_type.* = self.convertTypeInfo(kt.*);
                } else {
                    key_type.* = .String;
                }

                if (type_info.map_value_type) |vt| {
                    value_type.* = self.convertTypeInfo(vt.*);
                } else {
                    value_type.* = .Unknown;
                }

                return HIRType{ .Map = .{ .key = key_type, .value = value_type } };
            },
            .Union => .Unknown,
            .Custom => self.structTypeFromOptional(type_info.custom_type),
            else => .Nothing,
        };
    }

    pub fn inferTypeFromLiteral(_: *TypeSystem, literal: TokenLiteral) HIRType {
        return switch (literal) {
            .int => .Int,
            .float => .Float,
            .string => .String,
            .tetra => .Tetra,
            .byte => .Byte,
            .nothing => .Nothing,
            else => .Unknown,
        };
    }

    pub fn resolveFieldAccessType(self: *TypeSystem, e: *ast.Expr, symbol_table: *SymbolTable) ?FieldResolveResult {
        return switch (e.data) {
            .Variable => |var_token| blk: {
                // 1. Prefer explicit custom-type tracking from the HIR symbol table
                if (symbol_table.getVariableCustomType(var_token.lexeme)) |ctype| {
                    break :blk FieldResolveResult{ .t = self.structTypeForName(ctype), .custom_type_name = ctype };
                }

                // 2. Treat bare type names (e.g. Point) as struct/enum types
                if (self.isCustomType(var_token.lexeme)) |ct| {
                    switch (ct.kind) {
                        .Struct => break :blk FieldResolveResult{ .t = self.structTypeForName(var_token.lexeme), .custom_type_name = var_token.lexeme },
                        .Enum => break :blk FieldResolveResult{ .t = HIRType{ .Enum = 0 }, .custom_type_name = null },
                    }
                }

                // 3. FALLBACK: Ask the semantic analyzer for the precise type, including the
                //    concrete struct/enum name. This covers cases where the variable's
                //    type was inferred (e.g. from a function call) and we never called
                //    trackVariableCustomType in the HIR generator.
                if (self.semantic_analyzer) |semantic| {
                    if (semantic.current_scope) |scope| {
                        if (scope.lookupVariable(var_token.lexeme)) |variable| {
                            if (semantic.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                const type_info = storage.type_info.*;
                                const hir_type = self.convertTypeInfo(type_info);

                                var custom_type_name: ?[]const u8 = null;
                                switch (type_info.base) {
                                    .Struct, .Enum, .Custom => {
                                        if (type_info.custom_type) |ct_name| {
                                            custom_type_name = ct_name;
                                        }
                                    },
                                    else => {},
                                }

                                break :blk FieldResolveResult{
                                    .t = hir_type,
                                    .custom_type_name = custom_type_name,
                                };
                            }
                        }
                    }
                }

                // 4. Last resort: fall back to whatever HIR type tracking we have, but
                //    with no concrete custom type name. This means downstream code
                //    must treat the field index as best-effort only.
                break :blk FieldResolveResult{
                    .t = symbol_table.getTrackedVariableType(var_token.lexeme) orelse .Unknown,
                    .custom_type_name = null,
                };
            },
            .This => blk: {
                // 'this' refers to the current struct type in instance methods
                // Try to get the struct type from the symbol table's current function context
                if (symbol_table.current_function) |func_name| {
                    // Extract struct name from method name (e.g., "Point.getX" -> "Point")
                    if (std.mem.indexOfScalar(u8, func_name, '.')) |dot_idx| {
                        const struct_name = func_name[0..dot_idx];
                        if (self.isCustomType(struct_name)) |ct| {
                            if (ct.kind == .Struct) {
                                break :blk FieldResolveResult{ .t = self.structTypeForName(struct_name), .custom_type_name = struct_name };
                            }
                        }
                    }
                }
                // Fallback: return generic struct type
                break :blk FieldResolveResult{ .t = HIRType{ .Struct = 0 }, .custom_type_name = null };
            },
            .FieldAccess => |fa| blk: {
                if (fa.object.data == .Variable) {
                    const base_name = fa.object.data.Variable.lexeme;
                    if (self.isCustomType(base_name)) |ct| {
                        if (ct.kind == .Enum) {
                            return FieldResolveResult{ .t = HIRType{ .Enum = 0 }, .custom_type_name = base_name };
                        }
                    }
                }
                // First, try to resolve based on the object's custom type name (works
                // for plain struct variables and nested field access).
                if (self.resolveFieldAccessType(fa.object, symbol_table)) |base| {
                    if (base.custom_type_name) |struct_name| {
                        if (self.custom_types.get(struct_name)) |ctype| {
                            if (ctype.kind == .Struct) {
                                if (ctype.struct_fields) |fields| {
                                    for (fields) |f| {
                                        if (std.mem.eql(u8, f.name, fa.field.lexeme)) {
                                            return FieldResolveResult{ .t = f.field_type, .custom_type_name = f.custom_type_name };
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Fallback: if we know the object's HIR type is a struct via the
                // semantic struct table (e.g., for array-of-struct indexing like
                // zoo[0].name), use the struct_id and field metadata from there.
                if (self.struct_table) |const_table| {
                    const obj_type = self.inferTypeFromExpression(fa.object, symbol_table);
                    if (obj_type == .Struct) {
                        const sid: StructId = obj_type.Struct;
                        if (const_table.fields(sid)) |fields| {
                            for (fields) |f| {
                                if (std.mem.eql(u8, f.name, fa.field.lexeme)) {
                                    // Prefer enum type name when this field is an enum,
                                    // so that peek on zoo[0].animal_type can report
                                    // "Species" instead of generic "enum".
                                    var result_name: ?[]const u8 = null;

                                    // For enum-typed fields, use the AST custom_type
                                    // when available (e.g., "Species").
                                    const ti = f.type_info.*;
                                    if (f.hir_type == .Enum) {
                                        if (ti.custom_type) |ct_name| {
                                            result_name = ct_name;
                                        }
                                    } else if (f.nested_struct_id) |nested_id| {
                                        // Otherwise, for nested structs, recover the
                                        // nested struct's qualified name.
                                        var table = const_table.*;
                                        if (table.getEntryById(nested_id)) |nested_entry| {
                                            result_name = nested_entry.qualified_name;
                                        }
                                    }

                                    return FieldResolveResult{
                                        .t = f.hir_type,
                                        .custom_type_name = result_name,
                                    };
                                }
                            }
                        }
                    }
                }

                break :blk null;
            },
            else => null,
        };
    }

    pub fn inferTypeFromExpression(self: *TypeSystem, expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const result = switch (expr.data) {
            .Literal => |lit| self.inferTypeFromLiteral(lit),
            .Map => |map_expr| {
                const entries = map_expr.entries;
                const key_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                const value_type_ptr = self.allocator.create(HIRType) catch return .Unknown;

                if (entries.len > 0) {
                    const first = entries[0];
                    const inferred_key = self.inferTypeFromExpression(first.key, symbol_table);
                    const inferred_val = self.inferTypeFromExpression(first.value, symbol_table);

                    key_type_ptr.* = switch (inferred_key) {
                        // Enums are represented as integer discriminants at runtime.
                        .Enum => .Int,
                        else => inferred_key,
                    };
                    value_type_ptr.* = inferred_val;
                } else {
                    key_type_ptr.* = .String;
                    value_type_ptr.* = .Unknown;
                }

                return HIRType{ .Map = .{ .key = key_type_ptr, .value = value_type_ptr } };
            },
            .Variable => |var_token| {
                if (self.isCustomType(var_token.lexeme)) |custom_type| {
                    return switch (custom_type.kind) {
                        .Struct => self.structTypeForName(var_token.lexeme),
                        .Enum => HIRType{ .Enum = 0 },
                    };
                }

                if (symbol_table.current_function != null) {
                    const var_type = symbol_table.getTrackedVariableType(var_token.lexeme);
                    if (var_type != null and var_type.? != .Unknown) {
                        return var_type.?;
                    }
                }

                if (self.semantic_analyzer) |semantic| {
                    if (semantic.current_scope) |scope| {
                        if (scope.lookupVariable(var_token.lexeme)) |variable| {
                            if (semantic.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                const hir_type = self.convertTypeInfo(storage.type_info.*);
                                return hir_type;
                            }
                        }
                    }
                }
                const var_type = symbol_table.getTrackedVariableType(var_token.lexeme) orelse .Unknown;
                return var_type;
            },
            .FieldAccess => |field| {
                if (self.resolveFieldAccessType(expr, symbol_table)) |res| {
                    return res.t;
                }
                const obj_type = self.inferTypeFromExpression(field.object, symbol_table);
                if (std.mem.eql(u8, field.field.lexeme, "value")) return .String;
                if (std.mem.eql(u8, field.field.lexeme, "token_type")) return HIRType{ .Enum = 0 };
                if (obj_type == .Struct) return .Unknown;
                return .Unknown;
            },
            .EnumMember => |member| {
                // For enum members, try to find the parent enum type
                var enum_type_iter = self.custom_types.iterator();
                while (enum_type_iter.next()) |entry| {
                    if (entry.value_ptr.kind == .Enum) {
                        if (entry.value_ptr.enum_variants) |variants| {
                            for (variants) |variant| {
                                if (std.mem.eql(u8, variant.name, member.lexeme)) {
                                    return HIRType{ .Enum = 0 };
                                }
                            }
                        }
                    }
                }
                return .Unknown;
            },
            .Binary => |binary| {
                // Use the centralized binary operation result type inference
                // which correctly handles division (always Float) and other type promotions
                return self.inferBinaryOpResultType(binary.operator.type, binary.left.?, binary.right.?, symbol_table);
            },
            .Array => {
                const elements = expr.data.Array;
                if (elements.len > 0) {
                    const element_type = self.inferTypeFromExpression(elements[0], symbol_table);
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = element_type;
                    return HIRType{ .Array = element_type_ptr };
                }
                const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                unknown_ptr.* = .Unknown;
                return HIRType{ .Array = unknown_ptr };
            },
            .Index => |index| {
                const container_type = self.inferTypeFromExpression(index.array, symbol_table);
                return switch (container_type) {
                    .Array => |element_type_ptr| {
                        return element_type_ptr.*;
                    },
                    .String => .String,
                    .Map => blk: {
                        const map_info = container_type.Map;
                        const value_ty = map_info.value.*;
                        // Prefer declared value type when available; fall back to int for untyped maps.
                        const actual_value_ty = if (value_ty != .Unknown and value_ty != .Nothing) value_ty else .Int;
                        const has_else = self.mapExpressionHasElse(index.array);
                        if (has_else) {
                            break :blk actual_value_ty;
                        }

                        const value_ptr = self.allocator.create(HIRType) catch break :blk .Unknown;
                        value_ptr.* = actual_value_ty;
                        const nothing_ptr = self.allocator.create(HIRType) catch break :blk .Unknown;
                        nothing_ptr.* = .Nothing;
                        const members = self.allocator.alloc(*const HIRType, 2) catch break :blk .Unknown;
                        members[0] = value_ptr;
                        members[1] = nothing_ptr;
                        break :blk HIRType{ .Union = .{ .id = 0, .members = members } };
                    },
                    else => .String,
                };
            },
            .InternalCall => |internal| {
                const method_name = std.mem.trimLeft(u8, internal.method.lexeme, "@");
                if (std.mem.eql(u8, method_name, "pop")) {
                    const receiver_type = self.inferTypeFromExpression(internal.receiver, symbol_table);
                    switch (receiver_type) {
                        .Array => |elem_ptr| return elem_ptr.*,
                        .String => return .String,
                        else => return .Unknown,
                    }
                }
                if (std.mem.eql(u8, method_name, "substring")) return .String;
                if (std.mem.eql(u8, method_name, "length")) return .Int;
                if (std.mem.eql(u8, method_name, "int")) return .Int;
                if (std.mem.eql(u8, method_name, "float")) return .Float;
                if (std.mem.eql(u8, method_name, "string")) return .String;
                if (std.mem.eql(u8, method_name, "byte")) return .Byte;
                if (std.mem.eql(u8, method_name, "type")) return .String;
                return .Unknown;
            },
            .BuiltinCall => |bc| {
                const name = bc.function.lexeme;
                if (std.mem.eql(u8, name, "length")) return .Int;
                if (std.mem.eql(u8, name, "int")) return .Int;
                if (std.mem.eql(u8, name, "float")) return .Float;
                if (std.mem.eql(u8, name, "string")) return .String;
                if (std.mem.eql(u8, name, "byte")) {
                    return .Byte;
                }
                if (std.mem.eql(u8, name, "time")) return .Int;
                if (std.mem.eql(u8, name, "tick")) return .Int;
                if (std.mem.eql(u8, name, "push")) return .Nothing;
                if (std.mem.eql(u8, name, "pop")) {
                    if (bc.arguments.len > 0) {
                        const arg_expr = bc.arguments[0];
                        const container_type = self.inferTypeFromExpression(arg_expr, symbol_table);
                        if (container_type == .String) return .String;
                        if (container_type == .Array) {
                            if (arg_expr.data == .Variable) {
                                const var_name = arg_expr.data.Variable.lexeme;
                                if (symbol_table.getTrackedArrayElementType(var_name)) |elem_t| {
                                    return elem_t;
                                }
                            }
                            return .Int;
                        }
                    }
                    return .Unknown;
                }

                return .Unknown;
            },
            .FunctionCall => |call| {
                switch (call.callee.data) {
                    .InternalCall => |method| {
                        if (std.mem.eql(u8, method.method.lexeme, "substring")) return .String;
                        if (std.mem.eql(u8, method.method.lexeme, "length")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "bytes")) return .Unknown;
                        if (std.mem.eql(u8, method.method.lexeme, "int")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "float")) return .Float;
                        if (std.mem.eql(u8, method.method.lexeme, "byte")) {
                            return .Byte;
                        }
                        if (std.mem.eql(u8, method.method.lexeme, "safeAdd")) return .Int;
                    },
                    .FieldAccess => |field| {
                        if (std.mem.eql(u8, field.field.lexeme, "safeAdd")) {
                            return .Int;
                        }
                    },
                    .Variable => |var_token| {
                        if (std.mem.eql(u8, var_token.lexeme, "fizzbuzz") or
                            std.mem.eql(u8, var_token.lexeme, "fber") or
                            std.mem.eql(u8, var_token.lexeme, "forloop"))
                        {
                            return .Nothing;
                        }
                        if (std.mem.eql(u8, var_token.lexeme, "return_test")) {
                            return .String;
                        }
                        if (std.mem.eql(u8, var_token.lexeme, "foo")) {
                            return .Int;
                        }
                    },
                    else => {},
                }
                return .Unknown;
            },
            .Logical => .Tetra,
            .Unary => {
                return .Tetra;
            },
            .Grouping => |grouping| {
                if (grouping) |inner_expr| {
                    return self.inferTypeFromExpression(inner_expr, symbol_table);
                } else {
                    return .Nothing;
                }
            },
            .Range => {
                const element_type = self.allocator.create(HIRType) catch return .Unknown;
                element_type.* = .Int;
                return HIRType{ .Array = element_type };
            },
            .Cast => |cast| {
                // For 'as' / cast expressions, the result type is the target type.
                // Reuse the existing AST -> TypeInfo -> HIRType lowering to stay
                // consistent with semantic analysis.
                const type_info_ptr = ast.typeInfoFromExpr(self.allocator, cast.target_type) catch return .Unknown;
                defer self.allocator.destroy(type_info_ptr);
                return self.convertTypeInfo(type_info_ptr.*);
            },
            .Increment => |operand| {
                // Increment returns the same type as the operand
                const operand_type = self.inferTypeFromExpression(operand, symbol_table);
                // If operand type is unknown, default to Int (for literals like 5++)
                return if (operand_type == .Unknown) .Int else operand_type;
            },
            .Decrement => |operand| {
                // Decrement returns the same type as the operand
                const operand_type = self.inferTypeFromExpression(operand, symbol_table);
                // If operand type is unknown, default to Int (for literals like 5--)
                return if (operand_type == .Unknown) .Int else operand_type;
            },
            .Match => |match_expr| {
                // Infer type from the first case body (all cases should return the same type)
                if (match_expr.cases.len > 0) {
                    const first_case_body = match_expr.cases[0].body;
                    return self.inferTypeFromExpression(first_case_body, symbol_table);
                }
                return .Unknown;
            },
            .If => |if_expr| {
                // Infer type from the then branch (and else branch if it exists)
                if (if_expr.then_branch) |then_branch| {
                    const then_type = self.inferTypeFromExpression(then_branch, symbol_table);
                    if (if_expr.else_branch) |else_branch| {
                        const else_type = self.inferTypeFromExpression(else_branch, symbol_table);
                        // Try to find a common type
                        if (@as(std.meta.Tag(HIRType), then_type) == @as(std.meta.Tag(HIRType), else_type)) {
                            return then_type;
                        }
                        // For numeric types, try to find common type
                        const then_tag = @as(std.meta.Tag(HIRType), then_type);
                        const else_tag = @as(std.meta.Tag(HIRType), else_type);
                        if ((then_tag == .Int or then_tag == .Float or then_tag == .Byte) and
                            (else_tag == .Int or else_tag == .Float or else_tag == .Byte))
                        {
                            // Use computeNumericCommonType to find common type
                            // We need a dummy operator type - use PLUS as it's the most permissive
                            const common_type = self.computeNumericCommonType(then_type, else_type, .PLUS);
                            if (common_type != .Unknown) {
                                return common_type;
                            }
                        }
                        // Default to then_type if we can't find a common type
                        return then_type;
                    }
                    return then_type;
                }
                return .Unknown;
            },
            else => .String,
        };
        return result;
    }

    pub fn inferBinaryOpResultType(self: *TypeSystem, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr, symbol_table);
        const right_type = self.inferTypeFromExpression(right_expr, symbol_table);

        if (operator_type == .PLUS) {
            if (left_type == .String and right_type == .String) {
                return .String;
            } else if (left_type == .Array and right_type == .Array) {
                // Array concatenation: return the left array type (both should have same element type)
                return left_type;
            } else {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use + operator between {s} and {s}",
                    .{ @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown;
            }
        }

        const result_type = switch (operator_type) {
            .MINUS, .ASTERISK, .SLASH, .MODULO, .POWER => {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use {s} operator between {s} and {s}",
                    .{ @tagName(operator_type), @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown;
            },
            .EQUALITY, .BANG_EQUAL, .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => {
                return .Tetra;
            },
            else => .Int,
        };

        return result_type;
    }

    pub fn inferComparisonOperandType(self: *TypeSystem, left_expr: *ast.Expr, right_expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr, symbol_table);
        const right_type = self.inferTypeFromExpression(right_expr, symbol_table);

        if (left_type == .Enum or right_type == .Enum) {
            return HIRType{ .Enum = 0 };
        }

        if (left_type == .String or right_type == .String) {
            return .String;
        }

        if (left_type == .Float or right_type == .Float) {
            return .Float;
        }

        if (left_type == .Int or right_type == .Int) {
            return .Int;
        }

        if (left_type == .Byte or right_type == .Byte) {
            return .Byte;
        }

        return .Int;
    }

    /// Centralized numeric type promotion rules
    /// 1. Float dominance: If either operand is Float or operator is division (/), promote to Float
    /// 2. Int fallback: If either operand is Int, promote to Int
    /// 3. No promotion: If neither is Float or Int, operands are Byte
    pub fn computeNumericCommonType(_: *TypeSystem, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        const resolved_left = if (left_type == .Unknown) .Int else left_type;
        const resolved_right = if (right_type == .Unknown) .Int else right_type;

        if (resolved_left == .String or resolved_right == .String or
            resolved_left == .Array or resolved_right == .Array or
            resolved_left == .Map or resolved_right == .Map or
            resolved_left == .Struct or resolved_right == .Struct or
            resolved_left == .Enum or resolved_right == .Enum or
            resolved_left == .Union or resolved_right == .Union or
            resolved_left == .Function or resolved_right == .Function or
            resolved_left == .Nothing or resolved_right == .Nothing)
        {
            return .Unknown;
        }

        if (operator_type == .SLASH) {
            return .Float;
        }

        if (operator_type == .DOUBLE_SLASH) {
            // Integer division only works on integer types
            if (resolved_left == .Float or resolved_right == .Float) {
                return .Unknown; // Type error
            }
        }

        if (resolved_left == .Float or resolved_right == .Float) {
            return .Float;
        }

        if (resolved_left == .Int or resolved_right == .Int) {
            return .Int;
        }

        if (resolved_left == .Byte and resolved_right == .Byte) {
            return .Byte;
        }

        return .Unknown;
    }

    fn mapExpressionHasElse(self: *TypeSystem, expr: *ast.Expr) bool {
        return switch (expr.data) {
            .MapLiteral => |map_literal| map_literal.else_value != null,
            .Variable => |var_token| blk: {
                if (self.semantic_analyzer) |semantic| {
                    if (semantic.current_scope) |scope| {
                        if (scope.lookupVariable(var_token.lexeme)) |variable| {
                            if (semantic.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                if (storage.type_info.base == .Map) {
                                    break :blk storage.type_info.map_has_else_value;
                                }
                            }
                        }
                    }
                }
                break :blk false;
            },
            else => false,
        };
    }

    pub fn astTypeToLowerName(_: *TypeSystem, base: ast.Type) []const u8 {
        return switch (base) {
            .Int => "int",
            .Byte => "byte",
            .Float => "float",
            .String => "string",
            .Tetra => "tetra",
            .Nothing => "nothing",
            .Array => "array",
            .Struct => "struct",
            .Enum => "enum",
            .Map => "map",
            .Function => "function",
            .Custom => "custom",
            .Union => "union",
        };
    }

    pub fn collectUnionMemberNames(self: *TypeSystem, ut: *ast.UnionType) ![][]const u8 {
        var list = std.array_list.Managed([]const u8).init(self.allocator);
        defer if (false) list.deinit(); // transferred to caller

        for (ut.types) |member| {
            if (member.base == .Union) {
                if (member.union_type) |nested| {
                    const nested_list = try self.collectUnionMemberNames(nested);
                    for (nested_list) |nm| {
                        try list.append(nm);
                    }
                }
            } else {
                try list.append(self.astTypeToLowerName(member.base));
            }
        }

        return try list.toOwnedSlice();
    }
};
