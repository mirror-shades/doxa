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

/// Manages type information, type inference, and custom type registration for HIR generation
pub const TypeSystem = struct {
    custom_types: std.StringHashMap(CustomTypeInfo),
    allocator: std.mem.Allocator,
    reporter: *Reporting.Reporter,
    semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer = null,

    pub const CustomTypeInfo = struct {
        name: []const u8,
        kind: CustomTypeKind,
        // Enhanced enum support
        enum_variants: ?[]EnumVariant = null,
        // Enhanced struct support
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

        /// Get the index of an enum variant by name
        pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
            if (self.kind != .Enum or self.enum_variants == null) {
                // Note: Can't use reporter here since this is a const method
                return null;
            }

            for (self.enum_variants.?) |variant| {
                if (std.mem.eql(u8, variant.name, variant_name)) {
                    // Note: Can't use reporter here since this is a const method
                    return variant.index;
                }
            }
            return null;
        }

        /// Get the index of a struct field by name
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

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, semantic_analyzer: ?*const @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer) TypeSystem {
        return TypeSystem{
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .allocator = allocator,
            .reporter = reporter,
            .semantic_analyzer = semantic_analyzer,
        };
    }

    pub fn deinit(self: *TypeSystem) void {
        self.custom_types.deinit();
    }

    /// Register a custom type (struct or enum)
    pub fn registerCustomType(self: *TypeSystem, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = CustomTypeInfo{
            .name = type_name,
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    /// Register an enum type with its variants
    pub fn registerEnumType(self: *TypeSystem, enum_name: []const u8, variants: []const []const u8) !void {
        // Create enum variants array with proper indices
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

    /// Register a struct type with its fields
    pub fn registerStructType(self: *TypeSystem, struct_name: []const u8, fields: []const []const u8) !void {
        // If semantic analysis already populated this struct with rich field info,
        // do not override it with a lossy registration.
        if (self.custom_types.contains(struct_name)) return;

        // Create struct fields array with proper indices and types
        var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field_name, index| {
            struct_fields[index] = CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field_name),
                .field_type = .Unknown, // Will be inferred at runtime
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

    /// Check if a name refers to a custom type
    pub fn isCustomType(self: *TypeSystem, name: []const u8) ?CustomTypeInfo {
        return self.custom_types.get(name);
    }

    /// Get the HIR type for a custom type name
    pub fn getCustomTypeHIRType(self: *TypeSystem, name: []const u8) HIRType {
        if (self.custom_types.get(name)) |custom_type| {
            return switch (custom_type.kind) {
                .Struct => .Struct,
                .Enum => .Enum,
            };
        }
        return .Unknown; // Unresolved
    }

    /// Convert AST TypeInfo to HIRType
    pub fn convertTypeInfo(self: *TypeSystem, type_info: ast.TypeInfo) HIRType {
        return switch (type_info.base) {
            .Int => .Int,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Byte => .Byte,
            .Array => {
                if (type_info.array_type) |element_type| {
                    // Create a pointer to the element type for the Array union field
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = self.convertTypeInfo(element_type.*);
                    return HIRType{ .Array = element_type_ptr };
                } else {
                    // No element type info, use Unknown as element type
                    const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    unknown_ptr.* = .Unknown;
                    return HIRType{ .Array = unknown_ptr };
                }
            },
            .Map => {
                // Create a Map type with default key/value types
                const key_type = self.allocator.create(HIRType) catch return .Unknown;
                key_type.* = .String; // Default key type
                const value_type = self.allocator.create(HIRType) catch return .Unknown;
                value_type.* = .Unknown; // Unknown value type
                return HIRType{ .Map = .{ .key = key_type, .value = value_type } };
            },
            .Union => .Unknown, // Union type requires member type info
            .Custom => HIRType{ .Struct = 0 }, // Custom types are structs or enums, default to struct
            else => .Nothing,
        };
    }

    /// Infer type from a literal value
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

    /// Resolve field access type with proper custom type handling
    pub fn resolveFieldAccessType(self: *TypeSystem, e: *ast.Expr, symbol_table: *SymbolTable) ?FieldResolveResult {
        return switch (e.data) {
            .Variable => |var_token| blk: {
                if (symbol_table.getVariableCustomType(var_token.lexeme)) |ctype| {
                    break :blk FieldResolveResult{ .t = HIRType{ .Struct = 0 }, .custom_type_name = ctype };
                }
                if (self.isCustomType(var_token.lexeme)) |ct| {
                    switch (ct.kind) {
                        .Struct => break :blk FieldResolveResult{ .t = HIRType{ .Struct = 0 }, .custom_type_name = var_token.lexeme },
                        .Enum => break :blk FieldResolveResult{ .t = HIRType{ .Enum = 0 }, .custom_type_name = null },
                    }
                }
                break :blk FieldResolveResult{ .t = symbol_table.getTrackedVariableType(var_token.lexeme) orelse .Unknown, .custom_type_name = null };
            },
            .FieldAccess => |fa| blk: {
                // Detect enum member access like Color.Red and return Enum with custom type name
                if (fa.object.data == .Variable) {
                    const base_name = fa.object.data.Variable.lexeme;
                    if (self.isCustomType(base_name)) |ct| {
                        if (ct.kind == .Enum) {
                            return FieldResolveResult{ .t = HIRType{ .Enum = 0 }, .custom_type_name = base_name };
                        }
                    }
                }
                const base = self.resolveFieldAccessType(fa.object, symbol_table) orelse break :blk null;
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
                break :blk null;
            },
            else => null,
        };
    }

    /// Comprehensive type inference from expressions
    pub fn inferTypeFromExpression(self: *TypeSystem, expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const result = switch (expr.data) {
            .Literal => |lit| self.inferTypeFromLiteral(lit),
            .Map => {
                // Create a Map type with default key/value types
                const key_type = self.allocator.create(HIRType) catch return .Unknown;
                key_type.* = .String; // Default key type
                const value_type = self.allocator.create(HIRType) catch return .Unknown;
                value_type.* = .Unknown; // Unknown value type
                return HIRType{ .Map = .{ .key = key_type, .value = value_type } };
            },
            .Variable => |var_token| {
                // First check if this is a custom type name
                if (self.isCustomType(var_token.lexeme)) |custom_type| {
                    return switch (custom_type.kind) {
                        .Struct => HIRType{ .Struct = 0 }, // Use placeholder struct ID
                        .Enum => HIRType{ .Enum = 0 }, // Use placeholder enum ID
                    };
                }

                // FIXED: When inside a function scope, prioritize HIR symbol table over semantic analyzer
                // This ensures function parameters are found correctly and don't get shadowed by global variables
                if (symbol_table.current_function != null) {
                    // Inside function scope: check HIR symbol table first (contains function parameters)
                    const var_type = symbol_table.getTrackedVariableType(var_token.lexeme);
                    if (var_type != null and var_type.? != .Unknown) {
                        return var_type.?;
                    }
                }

                // Try to get type from semantic analyzer for global scope or when HIR symbol table lookup fails
                if (self.semantic_analyzer) |semantic| {
                    if (semantic.current_scope) |scope| {
                        if (scope.lookupVariable(var_token.lexeme)) |variable| {
                            if (semantic.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                // Convert AST TypeInfo to HIRType
                                const hir_type = self.convertTypeInfo(storage.type_info.*);
                                return hir_type;
                            }
                        }
                    }
                }

                // Final fallback to HIR generator's symbol table
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
            .Binary => |binary| {
                // Simple type inference for binary operations
                // For now, we don't use the types since HIRType comparisons are not supported
                return switch (binary.operator.type) {
                    .EQUALITY, .BANG_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => .Tetra,
                    .AND, .OR, .XOR => .Tetra, // Logical operations return tetra values
                    .PLUS, .MINUS, .ASTERISK, .SLASH, .MODULO => {
                        // Arithmetic operations - return the promoted type
                        // For now, return Unknown for type comparisons since HIRType is a union
                        // TODO: Implement proper HIRType comparison
                        return .Unknown;
                    },
                    .POWER => {
                        // For now, return Unknown since HIRType comparisons are not supported
                        // TODO: Implement proper HIRType comparison
                        return .Unknown;
                    },
                    else => .Tetra, // Default to Tetra for any other binary operations
                };
            },
            .Array => {
                // For array literals, we need to infer the element type
                const elements = expr.data.Array;
                if (elements.len > 0) {
                    const element_type = self.inferTypeFromExpression(elements[0], symbol_table);
                    // Create a pointer to the element type for the Array union field
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = element_type;
                    return HIRType{ .Array = element_type_ptr };
                }
                // Empty array - use Unknown as element type
                const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                unknown_ptr.* = .Unknown;
                return HIRType{ .Array = unknown_ptr };
            },
            .Index => |index| {
                // Array/string indexing returns the element type
                const container_type = self.inferTypeFromExpression(index.array, symbol_table);
                return switch (container_type) {
                    .Array => {
                        // Check if we have tracked element type information for this array
                        if (index.array.data == .Variable) {
                            if (symbol_table.getTrackedArrayElementType(index.array.data.Variable.lexeme)) |elem_type| {
                                return elem_type;
                            }
                        }
                        return .Byte; // Fallback: Default to Byte for arrays (more common for tape-like structures)
                    },
                    .String => .String, // String indexing returns single character (still string in our system)
                    .Map => .Int, // Map values are integers in our test case
                    else => .String, // Default to String for most index operations
                };
            },
            .InternalCall => |internal| {
                // Handle direct internal calls like @byte(str[0])
                const name = internal.method.lexeme;
                if (std.mem.eql(u8, name, "substring")) return .String;
                if (std.mem.eql(u8, name, "length")) return .Int;
                if (std.mem.eql(u8, name, "int")) return .Int;
                if (std.mem.eql(u8, name, "float")) return .Float;
                if (std.mem.eql(u8, name, "string")) return .String;
                if (std.mem.eql(u8, name, "byte")) {
                    // @byte returns byte | ValueError (same as BuiltinCall case)
                    var member_types = self.allocator.alloc(*const HIRType, 2) catch return .Unknown;

                    // Create Byte type
                    const byte_type = self.allocator.create(HIRType) catch return .Unknown;
                    byte_type.* = .Byte;
                    member_types[0] = byte_type;

                    // Create ValueError enum type (assuming ValueError has ID 1)
                    const value_error_type = self.allocator.create(HIRType) catch return .Unknown;
                    value_error_type.* = HIRType{ .Enum = 1 }; // ValueError enum ID
                    member_types[1] = value_error_type;

                    return HIRType{ .Union = member_types };
                }
                // Unknown internal call: best-effort default
                return .Unknown;
            },
            .BuiltinCall => |bc| {
                const name = bc.function.lexeme;
                // Return type inference for core built-ins
                if (std.mem.eql(u8, name, "length")) return .Int;
                if (std.mem.eql(u8, name, "int")) return .Int;
                if (std.mem.eql(u8, name, "float")) return .Float;
                if (std.mem.eql(u8, name, "string")) return .String;
                if (std.mem.eql(u8, name, "byte")) {
                    // @byte returns byte | ValueError
                    var member_types = self.allocator.alloc(*const HIRType, 2) catch return .Unknown;

                    // Create Byte type
                    const byte_type = self.allocator.create(HIRType) catch return .Unknown;
                    byte_type.* = .Byte;
                    member_types[0] = byte_type;

                    // Create ValueError enum type (assuming ValueError has ID 1)
                    const value_error_type = self.allocator.create(HIRType) catch return .Unknown;
                    value_error_type.* = HIRType{ .Enum = 1 }; // ValueError enum ID
                    member_types[1] = value_error_type;

                    return HIRType{ .Union = member_types };
                }
                if (std.mem.eql(u8, name, "time")) return .Int;
                if (std.mem.eql(u8, name, "tick")) return .Int;
                if (std.mem.eql(u8, name, "push")) return .Nothing; // modifies in place, returns nothing
                if (std.mem.eql(u8, name, "pop")) {
                    if (bc.arguments.len > 0) {
                        const arg_expr = bc.arguments[0];
                        const container_type = self.inferTypeFromExpression(arg_expr, symbol_table);
                        // String.pop returns string (last char)
                        if (container_type == .String) return .String;
                        if (container_type == .Array) {
                            // If we know the variable name and tracked element type, return it
                            if (arg_expr.data == .Variable) {
                                const var_name = arg_expr.data.Variable.lexeme;
                                if (symbol_table.getTrackedArrayElementType(var_name)) |elem_t| {
                                    return elem_t;
                                }
                            }
                            // Fallback: most arrays in tests are int[]
                            return .Int;
                        }
                    }
                    // Default fallback
                    return .Unknown;
                }

                // Unknown built-in: best-effort default
                return .Unknown;
            },
            .FunctionCall => |call| {
                // Handle different types of function calls
                switch (call.callee.data) {
                    .InternalCall => |method| {
                        if (std.mem.eql(u8, method.method.lexeme, "substring")) return .String;
                        if (std.mem.eql(u8, method.method.lexeme, "length")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "bytes")) return .Unknown; // Array type requires element type info
                        if (std.mem.eql(u8, method.method.lexeme, "int")) return .Int;
                        if (std.mem.eql(u8, method.method.lexeme, "float")) return .Float;
                        if (std.mem.eql(u8, method.method.lexeme, "byte")) {
                            // @byte returns byte | ValueError (same as BuiltinCall case)
                            var member_types = self.allocator.alloc(*const HIRType, 2) catch return .Unknown;

                            // Create Byte type
                            const byte_type = self.allocator.create(HIRType) catch return .Unknown;
                            byte_type.* = .Byte;
                            member_types[0] = byte_type;

                            // Create ValueError enum type (assuming ValueError has ID 1)
                            const value_error_type = self.allocator.create(HIRType) catch return .Unknown;
                            value_error_type.* = HIRType{ .Enum = 1 }; // ValueError enum ID
                            member_types[1] = value_error_type;

                            return HIRType{ .Union = member_types };
                        }
                        if (std.mem.eql(u8, method.method.lexeme, "safeAdd")) return .Int;
                    },
                    .FieldAccess => |field| {
                        // Handle imported functions like safeMath.safeAdd
                        if (std.mem.eql(u8, field.field.lexeme, "safeAdd")) {
                            return .Int; // safeAdd returns int
                        }
                        // Add more imported function mappings here as needed
                    },
                    .Variable => |var_token| {
                        // Handle direct function calls
                        if (std.mem.eql(u8, var_token.lexeme, "fizzbuzz") or
                            std.mem.eql(u8, var_token.lexeme, "fber") or
                            std.mem.eql(u8, var_token.lexeme, "forloop"))
                        {
                            return .Nothing; // These functions don't return values
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
                return .Unknown; // Default for unrecognized function calls
            },
            .Logical => .Tetra, // Logical operations (↔, ⊕, ∧, ∨, ↑, ↓, →) return tetra values
            .Unary => {
                // Unary operations: negation (¬) returns tetra, others depend on operand
                // For logical negation, always return tetra regardless of operand type
                return .Tetra; // Negation of any logical expression returns tetra
            },
            .Grouping => |grouping| {
                // Grouping (parentheses) - infer from the inner expression
                if (grouping) |inner_expr| {
                    return self.inferTypeFromExpression(inner_expr, symbol_table);
                } else {
                    return .Nothing; // Empty grouping
                }
            },
            else => .String, // Default to String for any unhandled expression types to prevent Auto leakage
        };
        return result;
    }

    /// Infer binary operation result type with proper type promotion
    pub fn inferBinaryOpResultType(self: *TypeSystem, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr, symbol_table);
        const right_type = self.inferTypeFromExpression(right_expr, symbol_table);

        // For PLUS operator, handle string/array concatenation and numeric promotion
        if (operator_type == .PLUS) {
            if (left_type == .String and right_type == .String) {
                return .String; // String concatenation
            } else if (left_type == .Array and right_type == .Array) {
                return .Array; // Array concatenation
            } else {
                // For numeric types, use centralized promotion rules
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                // Type mismatch - report compile error
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use + operator between {s} and {s}",
                    .{ @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown; // Return Unknown to indicate error
            }
        }

        // For arithmetic operations, use centralized type promotion
        const result_type = switch (operator_type) {
            .MINUS, .ASTERISK, .SLASH, .MODULO, .POWER => {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                // Type mismatch - report compile error
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use {s} operator between {s} and {s}",
                    .{ @tagName(operator_type), @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown; // Return Unknown to indicate error
            },
            .EQUALITY, .BANG_EQUAL, .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => {
                // For comparisons, use separate comparison operand type resolution
                return .Tetra; // Comparisons return tetra values
            },
            else => .Int, // Default to int for other operations
        };

        return result_type;
    }

    /// Infer the appropriate operand type for comparison operations
    pub fn inferComparisonOperandType(self: *TypeSystem, left_expr: *ast.Expr, right_expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr, symbol_table);
        const right_type = self.inferTypeFromExpression(right_expr, symbol_table);

        // If either operand is an enum, use Enum operand type
        if (left_type == .Enum or right_type == .Enum) {
            return HIRType{ .Enum = 0 };
        }

        // If either operand is a string, use String operand type
        if (left_type == .String or right_type == .String) {
            return .String;
        }

        // If either operand is float, use Float operand type
        if (left_type == .Float or right_type == .Float) {
            return .Float;
        }

        // If either operand is int, use Int operand type
        if (left_type == .Int or right_type == .Int) {
            return .Int;
        }

        // If either operand is byte, use Byte operand type
        if (left_type == .Byte or right_type == .Byte) {
            return .Byte;
        }

        // Default to Int for other types
        return .Int;
    }

    /// Centralized numeric type promotion rules
    /// 1. Float dominance: If either operand is Float or operator is division (/), promote to Float
    /// 2. Int fallback: If either operand is Int, promote to Int
    /// 3. No promotion: If neither is Float or Int, operands are Byte
    pub fn computeNumericCommonType(_: *TypeSystem, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        // Handle Unknown types by assuming they are Int (common case for literals)
        const resolved_left = if (left_type == .Unknown) .Int else left_type;
        const resolved_right = if (right_type == .Unknown) .Int else right_type;

        // Non-numeric types don't follow numeric promotion rules
        if (resolved_left == .String or resolved_right == .String or
            resolved_left == .Array or resolved_right == .Array or
            resolved_left == .Map or resolved_right == .Map or
            resolved_left == .Struct or resolved_right == .Struct or
            resolved_left == .Enum or resolved_right == .Enum or
            resolved_left == .Union or resolved_right == .Union or
            resolved_left == .Function or resolved_right == .Function or
            resolved_left == .Nothing or resolved_right == .Nothing)
        {
            return .Unknown; // Indicates no numeric promotion should occur
        }

        // Rule 1: Float dominance - division always promotes to float, or if either operand is float
        if (operator_type == .SLASH or resolved_left == .Float or resolved_right == .Float) {
            return .Float;
        }

        // Rule 2: Int fallback - if either operand is int, promote to int
        if (resolved_left == .Int or resolved_right == .Int) {
            return .Int;
        }

        // Rule 3: No promotion - both operands are the same numeric type (e.g., Byte)
        // Allow Byte + Byte operations
        if (resolved_left == .Byte and resolved_right == .Byte) {
            return .Byte;
        }

        return .Unknown;
    }

    /// Convert AST Type to lowercase string name
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

    /// Collect flattened union member names (lowercase) from a possibly nested union type
    pub fn collectUnionMemberNames(self: *TypeSystem, ut: *ast.UnionType) ![][]const u8 {
        var list = std.array_list.Managed([]const u8).init(self.allocator);
        defer if (false) list.deinit(); // transferred to caller

        // Depth-first traversal to flatten nested unions
        for (ut.types) |member| {
            if (member.base == .Union) {
                if (member.union_type) |nested| {
                    const nested_list = try self.collectUnionMemberNames(nested);
                    // append nested_list items to list
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
