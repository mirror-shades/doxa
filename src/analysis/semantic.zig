const std = @import("std");
const ast = @import("../ast/ast.zig");
const Memory = @import("../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;
const Parser = @import("../parser/parser_types.zig").Parser;
const import_parser = @import("../parser/import_parser.zig");

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorList = Reporting.ErrorList;

const Types = @import("../types/types.zig");
const TokenLiteral = Types.TokenLiteral;

const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;

// Fix the import at the top to make Token public
const Token = @import("../types/token.zig").Token;

// Import HIRType for custom type tracking
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;

//======================================================================

const NodeId = u32; // Or whatever your AST uses

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),

    // NEW: Track custom types for HIR generation
    custom_types: std.StringHashMap(CustomTypeInfo),

    // NEW: Track function return types for inference
    function_return_types: std.AutoHashMap(NodeId, *ast.TypeInfo),
    // NEW: Track return statements in current function for inference
    current_function_returns: std.ArrayList(*ast.TypeInfo),

    // NEW: Reference to parser for accessing imported symbols
    parser: ?*const Parser = null,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, memory: *MemoryManager, parser: ?*const Parser) SemanticAnalyzer {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .fatal_error = false,
            .current_scope = null,
            .type_cache = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .function_return_types = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
            .current_function_returns = std.ArrayList(*ast.TypeInfo).init(allocator),
            .parser = parser,
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        self.type_cache.deinit();
        self.custom_types.deinit();
        self.function_return_types.deinit();
        self.current_function_returns.deinit();
    }

    // NEW: Export custom type information for HIR generation
    pub fn getCustomTypes(self: *SemanticAnalyzer) std.StringHashMap(CustomTypeInfo) {
        return self.custom_types;
    }

    // NEW: Flatten union types to ensure no nested unions
    fn flattenUnionType(self: *SemanticAnalyzer, union_type: *ast.UnionType) !*ast.UnionType {
        var flat_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
        defer flat_types.deinit();

        for (union_type.types) |member_type| {
            if (member_type.base == .Union) {
                if (member_type.union_type) |nested_union| {
                    // Recursively flatten nested union
                    const flattened_nested = try self.flattenUnionType(nested_union);
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

    // NEW: Create a union type from multiple types
    fn createUnionType(self: *SemanticAnalyzer, types: []*ast.TypeInfo) !*ast.TypeInfo {
        // Flatten any existing unions in the input types
        var flat_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
        defer flat_types.deinit();

        for (types) |type_info| {
            if (type_info.base == .Union) {
                if (type_info.union_type) |union_type| {
                    const flattened = try self.flattenUnionType(union_type);
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

    // NEW: Register a custom type during semantic analysis
    fn registerCustomType(self: *SemanticAnalyzer, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, type_name),
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    // NEW: Register enum with variants
    fn registerEnumType(self: *SemanticAnalyzer, enum_name: []const u8, variants: []const []const u8) !void {
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

    // NEW: Register struct with fields
    fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
        var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field, index| {
            // Convert ast.Type to HIRType
            const hir_field_type = self.astTypeToHirType(field.type_info.base);
            var custom_type_name: ?[]const u8 = null;
            if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
                custom_type_name = try self.allocator.dupe(u8, field.type_info.custom_type.?);
            }
            struct_fields[index] = CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field.name),
                .field_type = hir_field_type,
                .custom_type_name = custom_type_name,
                .index = @intCast(index),
            };
        }

        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
    }

    // NEW: Custom type info structure (matching HIR generator)
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
            custom_type_name: ?[]const u8 = null, // For custom types like Person
            index: u32,
        };
    };

    // Helper function to convert HIRType to ast.Type
    fn hirTypeToAstType(self: *SemanticAnalyzer, hir_type: HIRType) ast.Type {
        _ = self;
        return switch (hir_type) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            .Array => .Array,
            .Struct => .Struct,
            .Map => .Map,
            .Enum => .Enum,
            .Function => .Function,
            .Auto => .Nothing, // Auto types default to Nothing in semantic analysis
        };
    }

    // Helper function to convert ast.Type to HIRType
    fn astTypeToHirType(self: *SemanticAnalyzer, ast_type: ast.Type) HIRType {
        _ = self;
        return switch (ast_type) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            .Array => .Array,
            .Struct => .Struct,
            .Map => .Map,
            .Enum => .Enum,
            .Function => .Function,
            .Custom => .Struct, // Custom types are typically structs
            .Alias => .Auto, // Aliases need to be resolved
            .Union => .Auto, // Unions need special handling
        };
    }

    // Helper function to convert SemanticAnalyzer.CustomTypeInfo to HIRGenerator.CustomTypeInfo
    pub fn convertCustomTypeInfo(semantic_type: CustomTypeInfo, allocator: std.mem.Allocator) !@import("../codegen/hir/soxa_generator.zig").HIRGenerator.CustomTypeInfo {
        var hir_type = @import("../codegen/hir/soxa_generator.zig").HIRGenerator.CustomTypeInfo{
            .name = semantic_type.name,
            .kind = switch (semantic_type.kind) {
                .Struct => .Struct,
                .Enum => .Enum,
            },
            .enum_variants = null,
            .struct_fields = null,
        };

        // Convert enum variants if present
        if (semantic_type.enum_variants) |variants| {
            const converted_variants = try allocator.alloc(@import("../codegen/hir/soxa_generator.zig").HIRGenerator.CustomTypeInfo.EnumVariant, variants.len);
            for (variants, 0..) |variant, i| {
                converted_variants[i] = .{
                    .name = variant.name,
                    .index = variant.index,
                };
            }
            hir_type.enum_variants = converted_variants;
        }

        // Convert struct fields if present
        if (semantic_type.struct_fields) |fields| {
            const converted_fields = try allocator.alloc(@import("../codegen/hir/soxa_generator.zig").HIRGenerator.CustomTypeInfo.StructField, fields.len);
            for (fields, 0..) |field, i| {
                converted_fields[i] = .{
                    .name = field.name,
                    .field_type = field.field_type,
                    .index = field.index,
                };
            }
            hir_type.struct_fields = converted_fields;
        }

        return hir_type;
    }

    pub fn analyze(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        const root_scope = try self.memory.scope_manager.createScope(null, self.memory);
        self.memory.scope_manager.root_scope = root_scope;
        self.current_scope = root_scope;

        try self.collectDeclarations(statements, root_scope);

        if (self.fatal_error) {
            return error.SemanticError;
        }

        try self.validateStatements(statements);

        // Debug: Print all custom types
        // std.debug.print("Custom types in semantic analyzer:\n", .{});
        // var custom_types_iter = self.custom_types.iterator();
        // while (custom_types_iter.next()) |entry| {
        //     std.debug.print("  '{s}': kind={s}\n", .{ entry.key_ptr.*, @tagName(entry.value_ptr.kind) });
        //     if (entry.value_ptr.kind == .Struct and entry.value_ptr.struct_fields != null) {
        //         std.debug.print("    Fields:\n", .{});
        //         for (entry.value_ptr.struct_fields.?) |field| {
        //             std.debug.print("      '{s}': type={s}", .{ field.name, @tagName(field.field_type) });
        //             if (field.custom_type_name) |custom_name| {
        //                 std.debug.print(", custom_type='{s}'", .{custom_name});
        //             }
        //             std.debug.print("\n", .{});
        //         }
        //     }
        // }
    }

    fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // Create TypeInfo
                    const type_info = try self.allocator.create(ast.TypeInfo);
                    errdefer self.allocator.destroy(type_info);

                    // Check if we have an explicit type annotation
                    if (decl.type_info.base != .Nothing) {
                        // Use explicit type, but resolve custom types
                        type_info.* = try self.resolveTypeInfo(decl.type_info);
                    } else if (decl.initializer) |init_expr| {
                        // Infer from initializer
                        const inferred = try self.inferTypeFromExpr(init_expr);
                        type_info.* = inferred.*;
                        // Preserve the mutability from the variable declaration, not from the initializer
                        type_info.is_mutable = decl.type_info.is_mutable;
                    } else {
                        // No type annotation and no initializer - this is invalid
                        self.reporter.reportCompileError(
                            stmt.base.span.start,
                            "Variable declaration requires either type annotation (::) or initializer",
                            .{},
                        );
                        self.fatal_error = true;
                        continue;
                    }

                    // Convert TypeInfo to TokenType
                    const token_type = self.convertTypeToTokenType(type_info.base);

                    // Get the actual value from initializer or use default for uninitialized variables
                    var value = if (decl.initializer) |init_expr|
                        try self.evaluateExpression(init_expr)
                    else
                        // Only use defaults for uninitialized variables
                        switch (type_info.base) {
                            .Int => TokenLiteral{ .int = 0 },
                            .Float => TokenLiteral{ .float = 0.0 },
                            .String => TokenLiteral{ .string = "" },
                            .Tetra => TokenLiteral{ .tetra = .false },
                            .Byte => TokenLiteral{ .byte = 0 },
                            else => TokenLiteral{ .nothing = {} },
                        };

                    // Convert value to match the declared type
                    value = try self.convertValueToType(value, type_info.base);

                    // ENFORCE: nothing types must be const
                    if (type_info.base == .Nothing and type_info.is_mutable) {
                        self.reporter.reportCompileError(
                            stmt.base.span.start,
                            "Nothing type variables must be declared as 'const'",
                            .{},
                        );
                        self.fatal_error = true;
                        continue;
                    }

                    _ = scope.createValueBinding(
                        decl.name.lexeme,
                        value,
                        token_type,
                        type_info,
                        !type_info.is_mutable,
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate variable name '{s}' in current scope",
                                .{decl.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };
                },
                .Block => |block_stmts| {
                    const block_scope = try self.memory.scope_manager.createScope(scope, self.memory);
                    try self.collectDeclarations(block_stmts, block_scope);
                    block_scope.deinit();
                },
                .FunctionDecl => |func| {
                    // NEW: Start collecting return types for this function
                    self.current_function_returns.clearRetainingCapacity();

                    // Create function type
                    const func_type = try self.allocator.create(ast.FunctionType);

                    // NEW: Infer return type if not explicitly provided
                    var inferred_return_type = func.return_type_info;
                    if (func.return_type_info.base == .Nothing) {
                        // No explicit return type - infer from function body
                        const inferred = try self.inferFunctionReturnType(func);
                        inferred_return_type = inferred.*;
                    } else {
                        // Explicit return type - flatten if it's a union
                        if (func.return_type_info.base == .Union) {
                            if (func.return_type_info.union_type) |union_type| {
                                const flattened = try self.flattenUnionType(union_type);
                                inferred_return_type = ast.TypeInfo{
                                    .base = .Union,
                                    .union_type = flattened,
                                    .is_mutable = false,
                                };
                            }
                        }
                    }

                    // Set up function type with inferred return type
                    func_type.* = .{
                        .params = &.{}, // TODO: Add parameter types
                        .return_type = try self.allocator.create(ast.TypeInfo),
                    };
                    func_type.return_type.* = inferred_return_type;

                    // Add function to current scope
                    const func_type_info = try self.allocator.create(ast.TypeInfo);
                    func_type_info.* = .{ .base = .Function, .function_type = func_type };

                    var env = @import("../interpreter/environment.zig").init(
                        self.allocator,
                        null, // no enclosing environment for function declarations
                        false, // debug disabled
                        self.memory,
                    );
                    _ = scope.createValueBinding(
                        func.name.lexeme,
                        TokenLiteral{ .function = .{
                            .params = func.params,
                            .body = func.body,
                            .closure = &env,
                            .defining_module = null,
                        } },
                        .FUNCTION,
                        func_type_info,
                        true,
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate function name '{s}' in current scope",
                                .{func.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };

                    // Store the inferred return type for this function
                    try self.function_return_types.put(stmt.base.id, func_type.return_type);

                    // NEW: Validate function body with inferred return type
                    try self.validateFunctionBody(func, stmt.base.span, inferred_return_type);
                },
                .EnumDecl => |enum_decl| {
                    // Register the enum type in the current scope
                    const enum_type_info = try self.allocator.create(ast.TypeInfo);
                    enum_type_info.* = .{ .base = .Enum, .custom_type = enum_decl.name.lexeme, .is_mutable = false };

                    // Create a placeholder value for the enum type
                    const enum_value = TokenLiteral{ .string = enum_decl.name.lexeme };

                    _ = scope.createValueBinding(
                        enum_decl.name.lexeme,
                        enum_value,
                        .ENUM,
                        enum_type_info,
                        true, // Enum types are constants
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Duplicate enum name '{s}' in current scope",
                                .{enum_decl.name.lexeme},
                            );
                            self.fatal_error = true;
                            continue;
                        } else {
                            return err;
                        }
                    };

                    // NEW: Register enum type for HIR generation
                    const variants = enum_decl.variants;
                    const variant_names = try self.allocator.alloc([]const u8, variants.len);
                    for (variants, variant_names) |variant, *name| {
                        name.* = variant.lexeme;
                    }
                    try self.registerEnumType(enum_decl.name.lexeme, variant_names);
                },

                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        // Handle struct declarations that are expressions
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.data.StructDecl;

                            // Convert struct fields to TypeInfo format
                            const struct_fields = try self.allocator.alloc(ast.StructFieldType, struct_decl.fields.len);
                            for (struct_decl.fields, struct_fields) |field, *struct_field| {
                                const field_type_info = try ast.typeInfoFromExpr(self.allocator, field.type_expr);
                                struct_field.* = .{
                                    .name = field.name.lexeme,
                                    .type_info = field_type_info,
                                };
                            }

                            // Register the struct type in the current scope
                            const struct_type_info = try self.allocator.create(ast.TypeInfo);
                            struct_type_info.* = .{ .base = .Struct, .custom_type = struct_decl.name.lexeme, .struct_fields = struct_fields, .is_mutable = false };

                            // Create a placeholder value for the struct type
                            const struct_value = TokenLiteral{ .string = struct_decl.name.lexeme };

                            _ = scope.createValueBinding(
                                struct_decl.name.lexeme,
                                struct_value,
                                .STRUCT,
                                struct_type_info,
                                true, // Struct types are constants
                            ) catch |err| {
                                if (err == error.DuplicateVariableName) {
                                    self.reporter.reportCompileError(
                                        stmt.base.span.start,
                                        "Duplicate struct name '{s}' in current scope",
                                        .{struct_decl.name.lexeme},
                                    );
                                    self.fatal_error = true;
                                    continue;
                                } else {
                                    return err;
                                }
                            };

                            // NEW: Register struct type for HIR generation
                            try self.registerStructType(struct_decl.name.lexeme, struct_fields);
                        }
                    }
                },
                .Import => |import_info| {
                    // NEW: Validate import
                    try self.validateImport(import_info, stmt.base.span);
                },
                // TODO: Handle other declarations...
                else => {},
            }
        }
    }

    fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) TokenType {
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
            .Alias => .ALIAS,
            .Enum => .ENUM,
            .Union => .UNION,
        };
    }

    fn validateStatements(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // For function bodies, we need to add local variables to the function scope
                    // For global scope, variables are already added during collectDeclarations
                    if (self.current_scope) |scope| {
                        // Check if this variable is already in the scope (from collectDeclarations)
                        if (scope.lookupVariable(decl.name.lexeme) == null) {
                            // This is a local variable in a function body that wasn't added during collection
                            // Create TypeInfo for the variable
                            const type_info = try self.allocator.create(ast.TypeInfo);
                            errdefer self.allocator.destroy(type_info);

                            // Check if we have an explicit type annotation
                            if (decl.type_info.base != .Nothing) {
                                // Use explicit type, but resolve custom types
                                type_info.* = try self.resolveTypeInfo(decl.type_info);
                            } else if (decl.initializer) |init_expr| {
                                // Infer from initializer
                                const inferred = try self.inferTypeFromExpr(init_expr);
                                type_info.* = inferred.*;
                            } else {
                                // No type annotation and no initializer - this is invalid
                                self.reporter.reportCompileError(
                                    stmt.base.span.start,
                                    "Variable declaration requires either type annotation (::) or initializer",
                                    .{},
                                );
                                self.fatal_error = true;
                                continue;
                            }

                            // Convert TypeInfo to TokenType
                            const token_type = self.convertTypeToTokenType(type_info.base);

                            // Get the actual value from initializer or use default for uninitialized variables
                            var value = if (decl.initializer) |init_expr|
                                try self.evaluateExpression(init_expr)
                            else
                                // Only use defaults for uninitialized variables
                                switch (type_info.base) {
                                    .Int => TokenLiteral{ .int = 0 },
                                    .Float => TokenLiteral{ .float = 0.0 },
                                    .String => TokenLiteral{ .string = "" },
                                    .Tetra => TokenLiteral{ .tetra = .false },
                                    .Byte => TokenLiteral{ .byte = 0 },
                                    else => TokenLiteral{ .nothing = {} },
                                };

                            // Convert value to match the declared type
                            value = try self.convertValueToType(value, type_info.base);

                            _ = scope.createValueBinding(
                                decl.name.lexeme,
                                value,
                                token_type,
                                type_info,
                                !type_info.is_mutable,
                            ) catch |err| {
                                if (err == error.DuplicateVariableName) {
                                    self.reporter.reportCompileError(
                                        stmt.base.span.start,
                                        "Duplicate variable name '{s}' in current scope",
                                        .{decl.name.lexeme},
                                    );
                                    self.fatal_error = true;
                                    continue;
                                } else {
                                    return err;
                                }
                            };
                        }
                    }

                    // Type checking
                    if (decl.initializer) |init_expr| {
                        const init_type = try self.inferTypeFromExpr(init_expr);
                        if (decl.type_info.base != .Nothing) {
                            // Check initializer matches declared type
                            // Resolve the declared type first, then create a mutable copy for unifyTypes
                            const resolved_type = try self.resolveTypeInfo(decl.type_info);
                            var decl_type_copy = resolved_type;
                            try self.unifyTypes(&decl_type_copy, init_type, stmt.base.span);
                        }
                    }
                },
                .Block => |block_stmts| {
                    const prev_scope = self.current_scope;
                    self.current_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                    defer {
                        self.current_scope = prev_scope;
                        // Scope cleanup handled by memory manager
                    }
                    try self.validateStatements(block_stmts);
                },
                .Expression => |expr| {
                    if (expr) |expression| {
                        // Check if this is a ForEach expression that needs special handling
                        if (expression.data == .ForEach) {
                            try self.validateForEachExpression(expression.data.ForEach, stmt.base.span);
                        } else {
                            // Validate the expression, which will trigger type checking for assignments
                            _ = try self.inferTypeFromExpr(expression);
                        }
                    }
                },
                .Return => |return_stmt| {
                    if (return_stmt.value) |value| {
                        // Validate return value type
                        _ = try self.inferTypeFromExpr(value);
                    }
                },
                .FunctionDecl => |func| {
                    // Function declarations are already validated in collectDeclarations
                    // Just validate the body here with the stored return type
                    if (self.function_return_types.get(stmt.base.id)) |return_type| {
                        try self.validateFunctionBody(func, stmt.base.span, return_type.*);
                    } else {
                        // Fallback to original return type if not found
                        try self.validateFunctionBody(func, stmt.base.span, func.return_type_info);
                    }
                },
                // TODO: Handle other statements...
                else => {},
            }
        }
    }

    fn unifyTypes(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
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
                                    span.start,
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
                    }
                    if (found_match) return;

                    // Build a list of allowed types for the error message
                    var type_list = std.ArrayList(u8).init(self.allocator);
                    defer type_list.deinit();
                    for (exp_union.types, 0..) |member_type, i| {
                        if (i > 0) try type_list.appendSlice(" | ");
                        try type_list.appendSlice(@tagName(member_type.base));
                    }
                    self.reporter.reportCompileError(
                        span.start,
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
                // Allow int to byte conversion if the value is in range
                // We'll need to check the actual value during evaluation
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
                span.start,
                "Type mismatch: expected {s}, got {s}",
                .{ @tagName(expected.base), @tagName(actual.base) },
            );
            self.fatal_error = true;
        }

        // Handle complex type unification (arrays, structs, etc)
        switch (expected.base) {
            .Array => {
                if (expected.array_type) |exp_elem| {
                    if (actual.array_type) |act_elem| {
                        try self.unifyTypes(exp_elem, act_elem, span);
                    }
                }
            },
            .Struct, .Custom => {
                if (expected.struct_fields) |exp_fields| {
                    if (actual.struct_fields) |act_fields| {
                        if (exp_fields.len != act_fields.len) {
                            self.reporter.reportCompileError(
                                span.start,
                                "Struct field count mismatch: expected {}, got {}",
                                .{ exp_fields.len, act_fields.len },
                            );
                            self.fatal_error = true;
                            return;
                        }
                        for (exp_fields, act_fields) |exp_field, act_field| {
                            if (!std.mem.eql(u8, exp_field.name, act_field.name)) {
                                self.reporter.reportCompileError(
                                    span.start,
                                    "Struct field name mismatch: expected '{s}', got '{s}'",
                                    .{ exp_field.name, act_field.name },
                                );
                                self.fatal_error = true;
                                return;
                            }
                            try self.unifyTypes(exp_field.type_info, act_field.type_info, span);
                        }
                    }
                }
            },
            // Handle other complex types...
            else => {},
        }
    }

    fn deepCopyTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        var copied = type_info;

        // Deep copy struct fields if present
        if (type_info.struct_fields) |fields| {
            const new_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
            for (fields, new_fields) |field, *new_field| {
                // For struct fields, we don't need to create new TypeInfo objects
                // since the field types are already properly allocated
                new_field.* = .{
                    .name = field.name,
                    .type_info = field.type_info, // Just copy the pointer
                };
            }
            copied.struct_fields = new_fields;
        }

        return copied;
    }

    fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) !*ast.TypeInfo {
        // Check cache first
        if (self.type_cache.get(expr.base.id)) |cached| {
            return cached;
        }

        var type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        switch (expr.data) {
            .Literal => |lit| {
                type_info.inferFrom(lit); // TokenLiteral is already the right type
            },
            .Binary => |bin| {
                const left_type = try self.inferTypeFromExpr(bin.left.?);
                const right_type = try self.inferTypeFromExpr(bin.right.?);

                // Operator-specific type rules
                const op = bin.operator.lexeme;

                if (std.mem.eql(u8, op, "/")) {
                    // Division always returns float
                    if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Division requires numeric operands, got {s}",
                            .{@tagName(left_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Division requires numeric operands, got {s}",
                            .{@tagName(right_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = .{ .base = .Float };
                } else if (std.mem.eql(u8, op, "%")) {
                    // Modulo for integers and bytes
                    if ((left_type.base != .Int and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Modulo requires integer or byte operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    // Return type follows same promotion rules as arithmetic
                    if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else if (std.mem.eql(u8, op, "+") or std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                    // Arithmetic with type promotion
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Arithmetic requires numeric operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // Type promotion rules: Float > Int > Byte
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else if (std.mem.eql(u8, op, "**") or std.mem.eql(u8, op, "^")) {
                    // Exponentiation promotes to float for safety
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Exponentiation requires numeric operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = .{ .base = .Float };
                } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
                    std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "equals") or std.mem.eql(u8, op, "!="))
                {
                    // Comparison operators return boolean
                    // Allow mixed numeric types for comparison
                    const left_numeric = (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte);
                    const right_numeric = (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte);

                    if (left_numeric and right_numeric) {
                        // Both are numeric - allow comparison
                        type_info.* = .{ .base = .Tetra };
                    } else if (left_type.base == right_type.base) {
                        // Same type - allow comparison
                        type_info.* = .{ .base = .Tetra };
                    } else if ((left_type.base == .Custom and right_type.base == .Enum) or
                        (left_type.base == .Enum and right_type.base == .Custom))
                    {
                        // Allow enum variable (Custom) to be compared with enum literal (Enum)
                        type_info.* = .{ .base = .Tetra };
                    } else {
                        // Incompatible types for comparison
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Cannot compare {s} with {s}",
                            .{ @tagName(left_type.base), @tagName(right_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    // Default case: unify types and use left type
                    try self.unifyTypes(left_type, right_type, expr.base.span);
                    type_info.* = left_type.*;
                }
            },
            .Variable => |var_token| {
                // Look up in current and parent scopes
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        type_info.* = storage.type_info.*;
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Internal error: Variable storage not found",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Undefined variable",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            },
            .Unary => |unary| {
                const operand_type = try self.inferTypeFromExpr(unary.right.?);
                // Handle unary operators: -, !, ~, etc.
                type_info.* = operand_type.*;
            },
            .Call => |call| {
                // Check if this is a method call (callee is FieldAccess)
                if (call.callee.data == .FieldAccess) {
                    const field_access = call.callee.data.FieldAccess;
                    const object_type = try self.inferTypeFromExpr(field_access.object);
                    const method_name = field_access.field.lexeme;

                    // Check if this is a module function call (e.g., safeMath.safeAdd)
                    if (field_access.object.data == .Variable) {
                        const object_name = field_access.object.data.Variable.lexeme;
                        if (self.isModuleNamespace(object_name)) {
                            // This is a module function call, not a method call
                            // Return a generic type for now - the actual type will be resolved during code generation
                            type_info.* = .{ .base = .Int }; // Assume module functions return int for now
                            return type_info;
                        }
                    }

                    // Handle method calls based on object type
                    switch (object_type.base) {
                        .Array => {
                            // Handle array methods
                            if (std.mem.eql(u8, method_name, "push")) {
                                type_info.* = .{ .base = .Nothing }; // push returns nothing
                            } else if (std.mem.eql(u8, method_name, "pop")) {
                                if (object_type.array_type) |elem_type| {
                                    type_info.* = elem_type.*;
                                } else {
                                    type_info.* = .{ .base = .Nothing };
                                }
                            } else if (std.mem.eql(u8, method_name, "length")) {
                                type_info.* = .{ .base = .Int };
                            } else {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Unknown array method '{s}'",
                                    .{method_name},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                            }
                        },
                        .String => {
                            // Handle string methods
                            if (std.mem.eql(u8, method_name, "length")) {
                                type_info.* = .{ .base = .Int };
                            } else if (std.mem.eql(u8, method_name, "bytes")) {
                                type_info.* = .{ .base = .Array };
                            } else {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Unknown string method '{s}'",
                                    .{method_name},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                            }
                        },
                        .Map => {
                            // Handle map methods
                            if (std.mem.eql(u8, method_name, "get")) {
                                type_info.* = .{ .base = .Nothing }; // TODO: Return actual value type
                            } else if (std.mem.eql(u8, method_name, "set")) {
                                type_info.* = .{ .base = .Nothing };
                            } else {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Unknown map method '{s}'",
                                    .{method_name},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                            }
                        },
                        else => {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Cannot call method '{s}' on type {s}",
                                .{ method_name, @tagName(object_type.base) },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        },
                    }
                } else {
                    // Regular function call
                    const callee_type = try self.inferTypeFromExpr(call.callee);
                    if (callee_type.base == .Function) {
                        if (callee_type.function_type) |func_type| {
                            type_info.* = func_type.return_type.*;
                        } else {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Function type has no return type information",
                                .{},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Cannot call non-function type {s}",
                            .{@tagName(callee_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                }
            },
            .Index => |index| {
                const array_type = try self.inferTypeFromExpr(index.array);
                const index_type = try self.inferTypeFromExpr(index.index);

                if (array_type.base == .Array) {
                    // Array indexing
                    if (index_type.base != .Int) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Array index must be integer, got {s}",
                            .{@tagName(index_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    if (array_type.array_type) |elem_type| {
                        type_info.* = elem_type.*;
                    } else {
                        type_info.base = .Nothing;
                    }
                } else if (array_type.base == .Map) {
                    // Map indexing
                    if (index_type.base != .String) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Map key must be string, got {s}",
                            .{@tagName(index_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // For now, assume map values are strings
                    type_info.* = .{ .base = .String };
                } else if (array_type.base == .String) {
                    // String indexing
                    if (index_type.base != .Int) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "String index must be integer, got {s}",
                            .{@tagName(index_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    type_info.* = .{ .base = .String }; // String indexing returns a character (string)
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot index non-array/map/string type {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            },
            .FieldAccess => |field| {
                const object_type = try self.inferTypeFromExpr(field.object);
                if (object_type.base == .Struct) {
                    if (object_type.struct_fields) |fields| {
                        for (fields) |struct_field| {
                            if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {

                                // Copy the field type info
                                type_info.* = struct_field.type_info.*;

                                // If this field is a struct type, look up its fields from custom_types
                                if (type_info.base == .Custom and type_info.custom_type != null) {
                                    if (self.custom_types.get(type_info.custom_type.?)) |custom_type| {
                                        if (custom_type.kind == .Struct) {
                                            // Convert CustomTypeInfo.StructField to ast.StructFieldType
                                            const struct_fields = try self.allocator.alloc(ast.StructFieldType, custom_type.struct_fields.?.len);
                                            for (custom_type.struct_fields.?, 0..) |custom_field, i| {
                                                const field_type_info = try self.allocator.create(ast.TypeInfo);
                                                field_type_info.* = .{ .base = self.hirTypeToAstType(custom_field.field_type) };
                                                struct_fields[i] = .{
                                                    .name = custom_field.name,
                                                    .type_info = field_type_info,
                                                };
                                            }
                                            type_info.* = .{ .base = .Struct, .custom_type = type_info.custom_type, .struct_fields = struct_fields, .is_mutable = false };
                                        }
                                    }
                                }
                                break;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Field '{s}' not found in struct",
                                .{field.field.lexeme},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Struct has no fields defined",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (object_type.base == .Custom) {
                    // Check if this is a struct type by looking up in custom_types
                    if (object_type.custom_type) |custom_type_name| {
                        // First check if this is a module namespace
                        if (self.isModuleNamespace(custom_type_name)) {
                            // Handle module namespace access
                            return self.handleModuleFieldAccess(custom_type_name, field.field.lexeme, expr.base.span);
                        }

                        if (self.custom_types.get(custom_type_name)) |custom_type| {
                            if (custom_type.kind == .Struct) {
                                // This is a struct, handle field access
                                if (custom_type.struct_fields) |fields| {
                                    for (fields) |struct_field| {
                                        if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                                            if (struct_field.field_type == .Struct and struct_field.custom_type_name != null) {
                                                // This is a custom struct field
                                                type_info.* = .{ .base = .Custom, .custom_type = struct_field.custom_type_name };
                                            } else {
                                                type_info.* = .{ .base = self.hirTypeToAstType(struct_field.field_type) };
                                            }
                                            break;
                                        }
                                    } else {
                                        self.reporter.reportCompileError(
                                            expr.base.span.start,
                                            "Field '{s}' not found in struct '{s}'",
                                            .{ field.field.lexeme, custom_type_name },
                                        );
                                        self.fatal_error = true;
                                        type_info.base = .Nothing;
                                        return type_info;
                                    }
                                } else {
                                    self.reporter.reportCompileError(
                                        expr.base.span.start,
                                        "Struct '{s}' has no fields defined",
                                        .{custom_type_name},
                                    );
                                    self.fatal_error = true;
                                    type_info.base = .Nothing;
                                    return type_info;
                                }
                            } else {
                                // Not a struct, treat as enum variant access
                                type_info.* = .{ .base = .Enum };
                            }
                        } else {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Undefined type '{s}'",
                                .{custom_type_name},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        // No custom type name, treat as enum variant access
                        type_info.* = .{ .base = .Enum };
                    }
                } else if (object_type.base == .Enum) {
                    // Allow enum variant access: Color.Red
                    // You may want to check if the variant exists, but for now just return Enum type
                    type_info.* = .{ .base = .Enum };
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot access field on non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            },
            .Array => |elements| {
                if (elements.len == 0) {
                    type_info.* = .{ .base = .Array, .array_type = null };
                } else {
                    const first_type = try self.inferTypeFromExpr(elements[0]);
                    for (elements[1..]) |element| {
                        const element_type = try self.inferTypeFromExpr(element);
                        try self.unifyTypes(first_type, element_type, expr.base.span);
                    }
                    const array_type = try self.allocator.create(ast.TypeInfo);
                    array_type.* = first_type.*;
                    type_info.* = .{ .base = .Array, .array_type = array_type };
                }
            },
            .Struct => |fields| {
                const struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                for (fields, struct_fields) |field, *struct_field| {
                    const field_type = try self.inferTypeFromExpr(field.value);
                    struct_field.* = .{
                        .name = field.name.lexeme,
                        .type_info = field_type,
                    };
                }
                type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
            },
            .If => |if_expr| {
                const condition_type = try self.inferTypeFromExpr(if_expr.condition.?);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                const then_type = try self.inferTypeFromExpr(if_expr.then_branch.?);
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferTypeFromExpr(else_branch);

                    // Special handling for peek expressions - allow different types
                    // since peek is used for output/printing and the actual return value
                    // should be the same as the expression being peeked
                    const then_is_peek = if_expr.then_branch.?.data == .Peek;
                    const else_is_peek = else_branch.data == .Peek;

                    // New: if one branch is a peek and the other is implicit nothing, allow the peek branch type
                    if ((then_is_peek and else_type.base == .Nothing) or (else_is_peek and then_type.base == .Nothing)) {
                        // Prefer the branch that actually peeks a value
                        type_info.* = if (then_is_peek) then_type.* else else_type.*;
                    } else if (then_is_peek and else_is_peek) {
                        // Both are peek expressions - use the then type as the result type
                        // This allows different types to be peeked in different branches
                        type_info.* = then_type.*;
                    } else {
                        // At least one is not a peek expression - unify types as usual
                        try self.unifyTypes(then_type, else_type, expr.base.span);
                        type_info.* = then_type.*;
                    }
                } else {
                    type_info.* = then_type.*;
                }
            },
            .Logical => |logical| {
                const left_type = try self.inferTypeFromExpr(logical.left);
                const right_type = try self.inferTypeFromExpr(logical.right);

                if (left_type.base != .Tetra or right_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Logical operators require boolean operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra };
            },
            .While => |while_expr| {
                const condition_type = try self.inferTypeFromExpr(while_expr.condition);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "While condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                _ = try self.inferTypeFromExpr(while_expr.body);
                type_info.* = .{ .base = .Nothing }; // While expressions have no value
            },
            .For => |for_expr| {
                // Create a scope for the loop variables
                const loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                const previous_scope = self.current_scope;
                self.current_scope = loop_scope;
                defer {
                    self.current_scope = previous_scope;
                    loop_scope.deinit();
                }

                // Process the initializer if present
                if (for_expr.initializer) |init_stmt| {
                    try self.validateStatements(&[_]ast.Stmt{init_stmt.*});
                }

                // Check the condition
                if (for_expr.condition) |condition| {
                    const condition_type = try self.inferTypeFromExpr(condition);
                    if (condition_type.base != .Tetra) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "For condition must be boolean, got {s}",
                            .{@tagName(condition_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }

                // Process the body
                _ = try self.inferTypeFromExpr(for_expr.body);
                type_info.* = .{ .base = .Nothing }; // For expressions have no value
            },
            .ForEach => |foreach_expr| {
                const array_type = try self.inferTypeFromExpr(foreach_expr.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForEach requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Create a scope for the loop variables
                const loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

                // Infer the element type from the array type
                var element_type = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                if (array_type.array_type) |element_type_info| {
                    element_type = element_type_info.*;
                }

                // Add the item variable to the loop scope
                _ = loop_scope.createValueBinding(
                    foreach_expr.item_name.lexeme,
                    TokenLiteral{ .nothing = {} }, // Will be set at runtime
                    self.convertTypeToTokenType(element_type.base),
                    &element_type,
                    false, // Loop variables are mutable
                ) catch |err| {
                    if (err == error.DuplicateVariableName) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Duplicate loop variable name '{s}'",
                            .{foreach_expr.item_name.lexeme},
                        );
                        self.fatal_error = true;
                        loop_scope.deinit();
                        return type_info;
                    } else {
                        loop_scope.deinit();
                        return err;
                    }
                };

                // Add the index variable to the loop scope if present
                if (foreach_expr.index_name) |index_name| {
                    var index_type_info = ast.TypeInfo{ .base = .Int, .is_mutable = false };
                    _ = loop_scope.createValueBinding(
                        index_name.lexeme,
                        TokenLiteral{ .nothing = {} }, // Will be set at runtime
                        .INT, // Index is always an integer
                        &index_type_info,
                        false, // Loop variables are mutable
                    ) catch |err| {
                        if (err == error.DuplicateVariableName) {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Duplicate loop variable name '{s}'",
                                .{index_name.lexeme},
                            );
                            self.fatal_error = true;
                            loop_scope.deinit();
                            return type_info;
                        } else {
                            loop_scope.deinit();
                            return err;
                        }
                    };
                }

                // Temporarily set current scope to loop scope
                const prev_scope = self.current_scope;
                self.current_scope = loop_scope;

                // Analyze the body statements in the loop scope
                for (foreach_expr.body) |stmt| {
                    try self.validateStatements(&[_]ast.Stmt{stmt});
                }

                // Restore previous scope and clean up loop scope
                self.current_scope = prev_scope;
                loop_scope.deinit();
            },
            .Match => |match_expr| {
                _ = try self.inferTypeFromExpr(match_expr.value);

                // Analyze all cases and create a union type if they have different types
                if (match_expr.cases.len > 0) {
                    // If the match value is a simple variable, we can narrow it inside cases
                    var matched_var_name: ?[]const u8 = null;
                    if (match_expr.value.data == .Variable) {
                        matched_var_name = match_expr.value.data.Variable.lexeme;
                    }

                    // Infer first case with narrowing
                    const first_case_type = try self.inferMatchCaseTypeWithNarrow(match_expr.cases[0], matched_var_name);

                    // Check if all cases have the same type
                    var all_same_type = true;
                    var union_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
                    defer union_types.deinit();

                    try union_types.append(first_case_type);

                    for (match_expr.cases[1..]) |case| {
                        const case_type = try self.inferMatchCaseTypeWithNarrow(case, matched_var_name);
                        if (case_type.base != first_case_type.base) {
                            all_same_type = false;
                        }
                        try union_types.append(case_type);
                    }

                    if (all_same_type) {
                        type_info.* = first_case_type.*;
                    } else {
                        // Create a union type from all case types
                        const union_type_array = try self.allocator.alloc(*ast.TypeInfo, union_types.items.len);
                        for (union_types.items, union_type_array) |item, *dest| {
                            dest.* = item;
                        }

                        const union_type = try self.allocator.create(ast.UnionType);
                        union_type.* = .{ .types = union_type_array };

                        type_info.* = .{ .base = .Union, .union_type = union_type };
                    }
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .Grouping => |grouped_expr| {
                if (grouped_expr) |expr_in_parens| {
                    type_info.* = (try self.inferTypeFromExpr(expr_in_parens)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .Assignment => |assign| {
                if (assign.value) |value| {
                    const value_type = try self.inferTypeFromExpr(value);
                    // Look up variable in scope
                    if (self.lookupVariable(assign.name.lexeme)) |variable| {
                        // Check if variable is mutable
                        if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                            if (storage.constant) {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(storage.type_info, value_type, expr.base.span);
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Undefined variable '{s}'",
                            .{assign.name.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }
                type_info.* = .{ .base = .Nothing }; // Assignment expressions have no value
            },
            .CompoundAssign => |compound_assign| {
                if (compound_assign.value) |value| {
                    const value_type = try self.inferTypeFromExpr(value);
                    // Look up variable in scope
                    if (self.lookupVariable(compound_assign.name.lexeme)) |variable| {
                        // Check if variable is mutable
                        if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                            if (storage.constant) {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{compound_assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(storage.type_info, value_type, expr.base.span);
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Undefined variable '{s}'",
                            .{compound_assign.name.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }
                type_info.* = .{ .base = .Nothing }; // Compound assignment expressions have no value
            },
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    type_info.* = (try self.inferTypeFromExpr(value)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .TypeOf => |type_of_expr| {
                _ = try self.inferTypeFromExpr(type_of_expr);
                type_info.* = .{ .base = .String }; // typeof returns a string representation
            },
            .LengthOf => |length_of_expr| {
                const array_type = try self.inferTypeFromExpr(length_of_expr);
                if (array_type.base != .Array and array_type.base != .String) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "LengthOf requires array or string type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Int }; // LengthOf returns an integer
            },
            .BytesOf => |bytes_of_expr| {
                const value_type = try self.inferTypeFromExpr(bytes_of_expr);
                if (value_type.base != .String) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "BytesOf requires string type, got {s}",
                        .{@tagName(value_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Array, .element_type = .Byte }; // BytesOf returns an array of bytes
            },
            .Map => |map_entries| {
                if (map_entries.len == 0) {
                    type_info.* = .{ .base = .Map };
                } else {
                    // Analyze key and value types from first entry
                    const first_key_type = try self.inferTypeFromExpr(map_entries[0].key);
                    const first_value_type = try self.inferTypeFromExpr(map_entries[0].value);

                    // Ensure all keys and values have consistent types
                    for (map_entries[1..]) |entry| {
                        const key_type = try self.inferTypeFromExpr(entry.key);
                        const value_type = try self.inferTypeFromExpr(entry.value);
                        try self.unifyTypes(first_key_type, key_type, expr.base.span);
                        try self.unifyTypes(first_value_type, value_type, expr.base.span);
                    }

                    type_info.* = .{ .base = .Map };
                }
            },
            .ArrayPush => |array_push| {
                const array_type = try self.inferTypeFromExpr(array_push.array);
                const element_type = try self.inferTypeFromExpr(array_push.element);

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayPush requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |expected_elem_type| {
                    try self.unifyTypes(expected_elem_type, element_type, expr.base.span);
                }

                type_info.* = .{ .base = .Nothing }; // ArrayPush has no return value
            },
            .ArrayLength => |array_length| {
                const array_type = try self.inferTypeFromExpr(array_length.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayLength requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Int }; // ArrayLength returns an integer
            },
            .ArrayPop => |array_pop| {
                const array_type = try self.inferTypeFromExpr(array_pop.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayPop requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    type_info.* = elem_type.*; // ArrayPop returns the element type
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .ArrayIsEmpty => |array_is_empty| {
                const array_type = try self.inferTypeFromExpr(array_is_empty.array);
                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayIsEmpty requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Tetra }; // ArrayIsEmpty returns a boolean
            },
            .ArrayConcat => |array_concat| {
                const array1_type = try self.inferTypeFromExpr(array_concat.array);
                const array2_type = try self.inferTypeFromExpr(array_concat.array2);

                if (array1_type.base != .Array or array2_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ArrayConcat requires array types",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Ensure both arrays have compatible element types
                if (array1_type.array_type) |elem1| {
                    if (array2_type.array_type) |elem2| {
                        try self.unifyTypes(elem1, elem2, expr.base.span);
                        type_info.* = array1_type.*; // Return type of first array
                    } else {
                        type_info.* = array1_type.*;
                    }
                } else {
                    type_info.* = array2_type.*;
                }
            },
            .MethodCall => |method_call| {
                const receiver_type = try self.inferTypeFromExpr(method_call.receiver);

                // For now, assume method calls return the same type as the receiver
                // In a more sophisticated implementation, you'd look up method signatures
                type_info.* = receiver_type.*;
            },
            .EnumMember => {
                // Enum members have the enum type
                type_info.* = .{ .base = .Enum };
            },
            .DefaultArgPlaceholder => {
                type_info.* = .{ .base = .Nothing };
            },
            .Input => {
                type_info.* = .{ .base = .String }; // Input returns a string
            },
            .Peek => |_peek| {
                const expr_type = try self.inferTypeFromExpr(_peek.expr);
                type_info.* = expr_type.*; // Peek returns the same type as the expression
            },
            .PeekStruct => |_peek_struct| {
                const expr_type = try self.inferTypeFromExpr(_peek_struct.expr);
                type_info.* = expr_type.*; // PeekStruct returns the same type as the expression
            },
            .IndexAssign => |index_assign| {
                const array_type = try self.inferTypeFromExpr(index_assign.array);
                const index_type = try self.inferTypeFromExpr(index_assign.index);
                const value_type = try self.inferTypeFromExpr(index_assign.value);

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot assign to index of non-array type {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Array index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    try self.unifyTypes(elem_type, value_type, expr.base.span);
                }

                type_info.* = .{ .base = .Nothing }; // IndexAssign has no return value
            },
            .FieldAssignment => |field_assign| {
                const object_type = try self.inferTypeFromExpr(field_assign.object);
                const value_type = try self.inferTypeFromExpr(field_assign.value);

                if (object_type.base != .Struct and object_type.base != .Custom) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Cannot assign to field of non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (object_type.base == .Struct and object_type.struct_fields != null) {
                    const fields = object_type.struct_fields.?;
                    for (fields) |struct_field| {
                        if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                            try self.unifyTypes(struct_field.type_info, value_type, expr.base.span);
                            break;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Field '{s}' not found in struct",
                            .{field_assign.field.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (object_type.base == .Custom and object_type.custom_type != null) {
                    const custom_type_name = object_type.custom_type.?;
                    if (self.custom_types.get(custom_type_name)) |custom_type| {
                        if (custom_type.kind == .Struct and custom_type.struct_fields != null) {
                            const fields = custom_type.struct_fields.?;
                            for (fields) |struct_field| {
                                if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                                    // For field assignment, we just need to validate that the value type is compatible
                                    // We can't do full type unification since we don't have the full type info
                                    break;
                                }
                            } else {
                                self.reporter.reportCompileError(
                                    expr.base.span.start,
                                    "Field '{s}' not found in struct '{s}'",
                                    .{ field_assign.field.lexeme, custom_type_name },
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                expr.base.span.start,
                                "Cannot assign to field of non-struct type {s}",
                                .{@tagName(object_type.base)},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Undefined type '{s}'",
                            .{custom_type_name},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                }

                type_info.* = .{ .base = .Nothing }; // FieldAssignment has no return value
            },
            .Exists => |exists| {
                const array_type = try self.inferTypeFromExpr(exists.array);

                // Create a temporary scope for the bound variable
                const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

                // Add the bound variable to the quantifier scope
                // Infer the type from the array element type
                var bound_var_type = if (array_type.array_type) |elem_type|
                    elem_type.*
                else
                    ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

                _ = quantifier_scope.createValueBinding(
                    exists.variable.lexeme, // "e"
                    TokenLiteral{ .nothing = {} },
                    self.convertTypeToTokenType(bound_var_type.base),
                    &bound_var_type,
                    true, // Bound variables are mutable
                ) catch |err| {
                    if (err == error.DuplicateVariableName) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Duplicate bound variable name '{s}' in exists quantifier",
                            .{exists.variable.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        quantifier_scope.deinit();
                        return type_info;
                    } else {
                        quantifier_scope.deinit();
                        return err;
                    }
                };

                // Temporarily set current scope to quantifier scope for condition analysis
                const prev_scope = self.current_scope;
                self.current_scope = quantifier_scope;

                const condition_type = try self.inferTypeFromExpr(exists.condition);

                // Restore previous scope
                self.current_scope = prev_scope;

                // Clean up the quantifier scope
                quantifier_scope.deinit();

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Exists requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Exists condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra }; // Exists returns a boolean
            },
            .ForAll => |for_all| {
                const array_type = try self.inferTypeFromExpr(for_all.array);

                // Create a temporary scope for the bound variable
                const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

                // Add the bound variable to the quantifier scope
                // Infer the type from the array element type
                var bound_var_type = if (array_type.array_type) |elem_type|
                    elem_type.*
                else
                    ast.TypeInfo{ .base = .Int }; // Default to int if we can't infer

                _ = quantifier_scope.createValueBinding(
                    for_all.variable.lexeme, // "u"
                    TokenLiteral{ .nothing = {} },
                    self.convertTypeToTokenType(bound_var_type.base),
                    &bound_var_type,
                    true, // Bound variables are mutable
                ) catch |err| {
                    if (err == error.DuplicateVariableName) {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Duplicate bound variable name '{s}' in forall quantifier",
                            .{for_all.variable.lexeme},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        quantifier_scope.deinit();
                        return type_info;
                    } else {
                        quantifier_scope.deinit();
                        return err;
                    }
                };

                // Temporarily set current scope to quantifier scope for condition analysis
                const prev_scope = self.current_scope;
                self.current_scope = quantifier_scope;

                const condition_type = try self.inferTypeFromExpr(for_all.condition);

                // Restore previous scope
                self.current_scope = prev_scope;

                // Clean up the quantifier scope
                quantifier_scope.deinit();

                if (array_type.base != .Array) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForAll requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "ForAll condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra }; // ForAll returns a boolean
            },
            .Assert => |assert| {
                const condition_type = try self.inferTypeFromExpr(assert.condition);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Assert condition must be boolean, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Nothing }; // Assert has no return value
            },
            .StructDecl => {
                type_info.* = .{ .base = .Struct };
            },
            .StructLiteral => |struct_lit| {
                // Look up the struct declaration to get canonical TypeInfo
                if (self.lookupVariable(struct_lit.name.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        if (storage.type_info.base == .Struct) {
                            // Just point to the canonical TypeInfo
                            type_info.* = storage.type_info.*;
                        } else {
                            // Fallback to basic struct type
                            type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
                        }
                    } else {
                        type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
                    }
                } else {
                    type_info.* = .{ .base = .Custom, .custom_type = struct_lit.name.lexeme };
                }
            },
            .EnumDecl => {
                type_info.* = .{ .base = .Enum };
            },
            .ArrayType => {
                type_info.* = .{ .base = .Array };
            },
            .FunctionExpr => {
                type_info.* = .{ .base = .Function };
            },
            .Block => |block| {
                if (block.value) |value| {
                    type_info.* = (try self.inferTypeFromExpr(value)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            },
            .TypeExpr => |type_expr| {
                // Convert TypeExpr to TypeInfo
                const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                type_info.* = type_info_ptr.*;
                self.allocator.destroy(type_info_ptr);
            },
            .Cast => |cast| {
                // For cast expressions (value as Type), the result type is the target type
                // Parse the target type from the cast expression
                const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
                type_info.* = target_type_info.*;

                // Also validate that the value being cast is a union type
                const value_type = try self.inferTypeFromExpr(cast.value);
                if (value_type.base != .Union) {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Type casting 'as' can only be used with union types, got {s}",
                        .{@tagName(value_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            },
        }

        // Cache the result
        try self.type_cache.put(expr.base.id, type_info);
        return type_info;
    }

    // Narrow the matched variable type within a single match case, then infer the body type.
    fn inferMatchCaseTypeWithNarrow(self: *SemanticAnalyzer, case: ast.MatchCase, matched_var_name: ?[]const u8) ErrorList!*ast.TypeInfo {
        // Create a per-case scope so narrowing does not leak out of the case
        const case_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer case_scope.deinit();

        const prev_scope = self.current_scope;
        self.current_scope = case_scope;
        defer self.current_scope = prev_scope;

        if (matched_var_name) |name| {
            // Build a narrowed TypeInfo from the pattern
            const narrow_info = try self.allocator.create(ast.TypeInfo);
            narrow_info.* = switch (case.pattern.type) {
                .INT_TYPE => .{ .base = .Int, .is_mutable = false },
                .FLOAT_TYPE => .{ .base = .Float, .is_mutable = false },
                .STRING_TYPE => .{ .base = .String, .is_mutable = false },
                .BYTE_TYPE => .{ .base = .Byte, .is_mutable = false },
                .TETRA_TYPE => .{ .base = .Tetra, .is_mutable = false },
                .NOTHING_TYPE, .NOTHING => .{ .base = .Nothing, .is_mutable = false },
                .DOT => .{ .base = .Enum, .is_mutable = false }, // enum member pattern implies enum
                else => blk: {
                    if (case.pattern.type == .IDENTIFIER and !std.mem.eql(u8, case.pattern.lexeme, "else")) {
                        break :blk .{ .base = .Custom, .custom_type = case.pattern.lexeme, .is_mutable = false };
                    }
                    // For else or unknown, skip narrowing
                    break :blk .{ .base = .Nothing, .is_mutable = false };
                },
            };

            const token_type: TokenType = self.convertTypeToTokenType(narrow_info.base);

            // Shadow the variable in this case scope with the narrowed type
            _ = case_scope.createValueBinding(
                name,
                TokenLiteral{ .nothing = {} },
                token_type,
                narrow_info,
                false,
            ) catch {};
        }

        return try self.inferTypeFromExpr(case.body);
    }

    fn resolveTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        // If this is a custom type, check if it refers to a declared struct or enum type
        if (type_info.base == .Custom) {
            if (type_info.custom_type) |custom_type_name| {
                if (self.lookupVariable(custom_type_name)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |_| {
                        // If the custom type refers to a struct or enum declaration, keep it as Custom
                        // This is the correct behavior - variables of struct/enum types should be Custom
                        return type_info;
                    }
                }
            }
        }

        // Handle union types - resolve and flatten each member type
        if (type_info.base == .Union) {
            if (type_info.union_type) |union_type| {

                // Use the new flattening function
                const flattened_union = try self.flattenUnionType(union_type);

                return ast.TypeInfo{
                    .base = .Union,
                    .union_type = flattened_union,
                    .is_mutable = type_info.is_mutable,
                };
            }
        }

        // Return the original type info if no resolution needed
        return type_info;
    }

    fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
        if (self.current_scope) |scope| {
            const result = scope.lookupVariable(name);
            if (result != null) return result;
        }

        // Check for imported symbols if not found in scope
        if (self.parser) |parser| {
            if (parser.imported_symbols) |imported_symbols| {
                if (imported_symbols.get(name)) |imported_symbol| {
                    // Create a variable for the imported symbol
                    return self.createImportedSymbolVariable(name, imported_symbol);
                }
            }

            // Check for module namespaces
            if (parser.module_namespaces.contains(name)) {

                // Create a variable for the module namespace
                return self.createModuleNamespaceVariable(name);
            }
        }

        return null;
    }

    // Helper function to create a Variable for a module namespace
    fn createModuleNamespaceVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
        // Create a TypeInfo for the module namespace
        const type_info = self.allocator.create(ast.TypeInfo) catch return null;
        errdefer self.allocator.destroy(type_info);

        type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false, .custom_type = name };

        // Convert TypeInfo to TokenType
        const token_type = self.convertTypeToTokenType(type_info.base);

        // Create a Variable object for the module namespace using the scope's createValueBinding
        if (self.current_scope) |scope| {
            // Create a placeholder value for the module namespace
            const placeholder_value = TokenLiteral{ .nothing = {} };

            const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
            return variable;
        }

        return null;
    }

    // Helper function to check if a name is a module namespace
    fn isModuleNamespace(self: *SemanticAnalyzer, name: []const u8) bool {
        if (self.parser) |parser| {
            if (parser.module_namespaces.contains(name)) {
                return true;
            }
        }
        return false;
    }

    // Helper function to handle module field access (e.g., math.add)
    fn handleModuleFieldAccess(self: *SemanticAnalyzer, module_name: []const u8, field_name: []const u8, span: ast.SourceSpan) !*ast.TypeInfo {
        var type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        if (self.parser) |parser| {
            // Look for the field in the module's imported symbols
            if (parser.imported_symbols) |imported_symbols| {
                const full_name = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name }) catch {
                    self.reporter.reportCompileError(
                        span.start,
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
            span.start,
            "Field '{s}' not found in module '{s}'",
            .{ field_name, module_name },
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return type_info;
    }

    // Helper function to create a Variable for an imported symbol
    fn createImportedSymbolVariable(self: *SemanticAnalyzer, name: []const u8, imported_symbol: import_parser.ImportedSymbol) ?*Variable {
        // Create a TypeInfo based on the imported symbol kind
        const type_info = self.allocator.create(ast.TypeInfo) catch return null;
        errdefer self.allocator.destroy(type_info);

        switch (imported_symbol.kind) {
            .Function => {
                // Create return type
                const return_type = self.allocator.create(ast.TypeInfo) catch return null;
                return_type.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };

                // Create function type
                const function_type = self.allocator.create(ast.FunctionType) catch return null;
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

        // Convert TypeInfo to TokenType
        const token_type = self.convertTypeToTokenType(type_info.base);

        // Create a Variable object for the imported symbol using the scope's createValueBinding
        if (self.current_scope) |scope| {
            // Create a placeholder value for the imported symbol
            const placeholder_value = TokenLiteral{ .nothing = {} };

            const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
            return variable;
        }

        return null;
    }

    // Instead of setting fatal_error = true immediately, collect errors
    // and continue analysis where possible
    pub fn reportTypeError(self: *SemanticAnalyzer, span: ast.SourceSpan, comptime fmt: []const u8, args: anytype) void {
        self.reporter.reportCompileError(span.start, fmt, args);
        // Only set fatal_error for critical errors that prevent further analysis
    }

    fn typeExprToTypeInfo(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr) !*ast.TypeInfo {
        const type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        switch (type_expr.data) {
            .Basic => |basic| {
                type_info.* = .{ .base = switch (basic) {
                    .Integer => .Int,
                    .Byte => .Byte,
                    .Float => .Float,
                    .String => .String,
                    .Tetra => .Tetra,
                    .Nothing => .Nothing,
                } };
            },
            .Custom => |custom| {
                type_info.* = .{ .base = .Custom, .custom_type = custom.lexeme };
            },
            .Array => |array| {
                const element_type = try self.typeExprToTypeInfo(array.element_type);
                type_info.* = .{ .base = .Array, .array_type = element_type };
            },
            .Struct => |fields| {
                var struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                for (fields, 0..) |field, i| {
                    const field_type = try self.typeExprToTypeInfo(field.type_expr);
                    struct_fields[i] = .{
                        .name = field.name.lexeme,
                        .type_info = field_type,
                    };
                }
                type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
            },
            .Enum => |_| {
                type_info.* = .{ .base = .Nothing };
            },
            .Union => |types| {
                var union_types = try self.allocator.alloc(*ast.TypeInfo, types.len);
                for (types, 0..) |union_type_expr, i| {
                    union_types[i] = try self.typeExprToTypeInfo(union_type_expr);
                }
                const union_type = try self.allocator.create(ast.UnionType);
                union_type.* = .{
                    .types = union_types,
                    .current_type_index = 0,
                };
                type_info.* = .{ .base = .Union, .union_type = union_type };
            },
        }

        return type_info;
    }

    // Add this new function to evaluate expressions and get their values
    fn evaluateExpression(self: *SemanticAnalyzer, expr: *ast.Expr) !TokenLiteral {
        switch (expr.data) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |bin| {
                const left_value = try self.evaluateExpression(bin.left.?);
                const right_value = try self.evaluateExpression(bin.right.?);

                return self.evaluateBinaryOp(left_value, bin.operator, right_value);
            },
            .Unary => |unary| {
                const operand_value = try self.evaluateExpression(unary.right.?);
                return self.evaluateUnaryOp(unary.operator, operand_value);
            },
            .Grouping => |grouped_expr| {
                if (grouped_expr) |expr_in_parens| {
                    return self.evaluateExpression(expr_in_parens);
                } else {
                    return TokenLiteral{ .nothing = {} };
                }
            },
            .Variable => |var_token| {
                // Look up variable in current scope
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        return storage.value;
                    }
                }
                // Return default value if variable not found (shouldn't happen in well-formed code)
                return TokenLiteral{ .nothing = {} };
            },
            .Cast => |cast| {
                // TODO: Implement evaluation for cast expressions
                // For now, just evaluate the value
                return try self.evaluateExpression(cast.value);
            },
            else => {
                // For complex expressions that can't be evaluated at compile time,
                // return a default value based on the inferred type
                const inferred_type = try self.inferTypeFromExpr(expr);
                return switch (inferred_type.base) {
                    .Int => TokenLiteral{ .int = 0 },
                    .Float => TokenLiteral{ .float = 0.0 },
                    .String => TokenLiteral{ .string = "" },
                    .Tetra => TokenLiteral{ .tetra = .false },
                    .Byte => TokenLiteral{ .byte = 0 },
                    else => TokenLiteral{ .nothing = {} },
                };
            },
        }
    }

    fn evaluateBinaryOp(self: *SemanticAnalyzer, left: TokenLiteral, operator: Token, right: TokenLiteral) TokenLiteral {
        _ = self; // Unused parameter

        return switch (operator.type) {
            .PLUS => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l + r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                    .byte => |r| TokenLiteral{ .int = l + r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l + r },
                    .byte => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l + r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                    .byte => |r| TokenLiteral{ .byte = l + r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .MINUS => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l - r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                    .byte => |r| TokenLiteral{ .int = l - r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l - r },
                    .byte => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l - r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                    .byte => |r| TokenLiteral{ .byte = l - r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .ASTERISK => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l * r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                    .byte => |r| TokenLiteral{ .int = l * r },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l * r },
                    .byte => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = l * r },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                    .byte => |r| TokenLiteral{ .byte = l * r },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .SLASH => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                    .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = l / r },
                    .byte => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                    .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .MODULO => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    .byte => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .int = @mod(l, r) }, // Fixed: use @mod for signed integers
                    .byte => |r| TokenLiteral{ .byte = l % r }, // This is fine since bytes are unsigned
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            .POWER => switch (left) {
                .int => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .float => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, l, r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                .byte => |l| switch (right) {
                    .int => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                    .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                    else => TokenLiteral{ .nothing = {} },
                },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        };
    }

    fn evaluateUnaryOp(self: *SemanticAnalyzer, operator: Token, operand: TokenLiteral) TokenLiteral {
        _ = self; // Unused parameter

        return switch (operator.type) {
            .MINUS => switch (operand) {
                .int => |i| TokenLiteral{ .int = -i },
                .float => |f| TokenLiteral{ .float = -f },
                .byte => |b| TokenLiteral{ .int = -@as(i32, b) },
                else => TokenLiteral{ .nothing = {} },
            },
            .BANG => switch (operand) {
                .tetra => |t| TokenLiteral{ .tetra = switch (t) {
                    .true => .false,
                    .false => .true,
                    .both => .neither,
                    .neither => .both,
                } },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        };
    }

    // Add this function to convert values to the expected type
    fn convertValueToType(self: *SemanticAnalyzer, value: TokenLiteral, expected_type: ast.Type) !TokenLiteral {
        _ = self; // Unused parameter

        return switch (expected_type) {
            .Int => switch (value) {
                .int => value,
                .byte => |b| TokenLiteral{ .int = b },
                .float => |f| TokenLiteral{ .int = @intFromFloat(f) },
                else => value, // Keep as is for other types
            },
            .Byte => switch (value) {
                .int => |i| {
                    if (i >= 0 and i <= 255) {
                        return TokenLiteral{ .byte = @intCast(i) };
                    } else {
                        // This should be caught by type checking, but handle gracefully
                        return TokenLiteral{ .byte = 0 };
                    }
                },
                .byte => value,
                .float => |f| {
                    const int_val = @as(i32, @intFromFloat(f)); // Fixed: use @intFromFloat instead of @as
                    if (int_val >= 0 and int_val <= 255) {
                        return TokenLiteral{ .byte = @intCast(int_val) };
                    } else {
                        return TokenLiteral{ .byte = 0 };
                    }
                },
                else => value, // Keep as is for other types
            },
            .Float => switch (value) {
                .int => |i| TokenLiteral{ .float = @floatFromInt(i) },
                .byte => |b| TokenLiteral{ .float = @floatFromInt(b) },
                .float => value,
                else => value, // Keep as is for other types
            },
            else => value, // No conversion needed for other types
        };
    }

    fn handleTypeOf(self: *SemanticAnalyzer, expr: *ast.Expr) !TokenLiteral {
        const type_info = try self.inferTypeFromExpr(expr);
        defer self.allocator.destroy(type_info);

        const type_string = switch (type_info.base) {
            .Int => "int",
            .Float => "float",
            .Byte => "byte",
            .String => "string",
            .Tetra => "tetra",
            .Array => "array",
            .Function => "function",
            .Struct => "struct",
            .Map => "map",
            .Enum => "enum",
            .Union => "union",
            .Nothing => "nothing",
            else => "unknown",
        };

        return TokenLiteral{ .string = type_string };
    }

    // NEW: Function body validation
    fn validateFunctionBody(self: *SemanticAnalyzer, func: anytype, func_span: ast.SourceSpan, expected_return_type: ast.TypeInfo) !void {
        // Create function scope with parameters
        const func_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer func_scope.deinit();

        // Add parameters to function scope
        for (func.params) |param| {
            const param_type_info = if (param.type_expr) |type_expr|
                try ast.typeInfoFromExpr(self.allocator, type_expr)
            else
                try self.allocator.create(ast.TypeInfo);

            if (param.type_expr == null) {
                param_type_info.* = .{ .base = .Nothing }; // Default to nothing if no type specified
            }

            _ = func_scope.createValueBinding(
                param.name.lexeme,
                TokenLiteral{ .nothing = {} }, // Parameters get their values at call time
                self.convertTypeToTokenType(param_type_info.base),
                param_type_info,
                false, // Parameters are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        func_span.start,
                        "Duplicate parameter name '{s}' in function '{s}'",
                        .{ param.name.lexeme, func.name.lexeme },
                    );
                    self.fatal_error = true;
                    return;
                } else {
                    return err;
                }
            };
        }

        // Temporarily set current scope to function scope
        const prev_scope = self.current_scope;
        self.current_scope = func_scope;

        // Validate function body statements
        try self.validateStatements(func.body);

        // NEW: Return path analysis with enhanced union subtyping
        _ = try self.validateReturnPaths(func.body, expected_return_type, func_span);

        // Restore previous scope
        self.current_scope = prev_scope;
    }

    // NEW: Return path analysis with enhanced union subtyping
    fn validateReturnPaths(self: *SemanticAnalyzer, body: []ast.Stmt, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) !bool {
        var has_return = false;
        var has_return_with_value = false;
        var has_return_without_value = false;
        var all_paths_return = true;

        for (body) |stmt| {
            switch (stmt.data) {
                .Return => |return_stmt| {
                    has_return = true;
                    if (return_stmt.value) |value| {
                        has_return_with_value = true;
                        const return_type = try self.inferTypeFromExpr(value);
                        try self.validateReturnTypeCompatibility(&expected_return_type, return_type, stmt.base.span);
                    } else {
                        has_return_without_value = true;
                        if (expected_return_type.base != .Nothing) {
                            self.reporter.reportCompileError(
                                stmt.base.span.start,
                                "Function expects return value of type {s}, but return statement has no value",
                                .{@tagName(expected_return_type.base)},
                            );
                            self.fatal_error = true;
                        }
                    }
                },
                .Block => |block_stmts| {
                    // Recursively check blocks for return statements
                    const block_returns = try self.validateReturnPaths(block_stmts, expected_return_type, func_span);
                    if (block_returns) {
                        has_return = true;
                        has_return_with_value = true; // Assume blocks with returns have values
                    } else {
                        all_paths_return = false;
                    }
                },
                .Expression => |expr| {
                    if (expr) |expression| {

                        // Check if this is an if expression that might return
                        if (expression.data == .If) {
                            const if_returns = try self.validateIfExpressionReturns(expression, expected_return_type, func_span);
                            if (if_returns) {
                                has_return = true;
                                has_return_with_value = true; // If expressions that return have values
                            } else {
                                all_paths_return = false;
                            }
                        }
                    }
                },
                else => {},
            }
        }

        // Check for missing return statements
        if (expected_return_type.base != .Nothing and !has_return_with_value) {
            self.reporter.reportCompileError(
                func_span.start,
                "Function expects return value of type {s}, but no return statement with value found",
                .{@tagName(expected_return_type.base)},
            );
            self.fatal_error = true;
        }

        if (expected_return_type.base == .Nothing and has_return_with_value) {
            self.reporter.reportCompileError(
                func_span.start,
                "Function expects no return value, but return statement has value",
                .{},
            );
            self.fatal_error = true;
        }

        return has_return or all_paths_return;
    }

    // NEW: Validate if expressions for return paths
    fn validateIfExpressionReturns(self: *SemanticAnalyzer, if_expr: *ast.Expr, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!bool {
        const if_data = if_expr.data.If;

        // Check then branch
        const then_returns = if (if_data.then_branch) |then_branch|
            try self.validateExpressionReturns(then_branch, expected_return_type, func_span)
        else
            false;

        // Check else branch
        const else_returns = if (if_data.else_branch) |else_branch|
            try self.validateExpressionReturns(else_branch, expected_return_type, func_span)
        else
            false;

        // Both branches must return for the if to guarantee a return
        return then_returns and else_returns;
    }

    // NEW: Validate ForEach expressions with proper scope management
    fn validateForEachExpression(self: *SemanticAnalyzer, foreach_expr: ast.ForEachExpr, span: ast.SourceSpan) ErrorList!void {
        // Validate the array expression
        const array_type = try self.inferTypeFromExpr(foreach_expr.array);
        if (array_type.base != .Array) {
            self.reporter.reportCompileError(
                span.start,
                "ForEach requires array type, got {s}",
                .{@tagName(array_type.base)},
            );
            self.fatal_error = true;
            return;
        }

        // Create a scope for the loop variables
        const loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

        // Infer the element type from the array type
        var element_type = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
        if (array_type.array_type) |element_type_info| {
            element_type = element_type_info.*;
        }

        // Add the item variable to the loop scope
        _ = loop_scope.createValueBinding(
            foreach_expr.item_name.lexeme,
            TokenLiteral{ .nothing = {} }, // Will be set at runtime
            self.convertTypeToTokenType(element_type.base),
            &element_type,
            false, // Loop variables are mutable
        ) catch |err| {
            if (err == error.DuplicateVariableName) {
                self.reporter.reportCompileError(
                    span.start,
                    "Duplicate loop variable name '{s}'",
                    .{foreach_expr.item_name.lexeme},
                );
                self.fatal_error = true;
                loop_scope.deinit();
                return;
            } else {
                loop_scope.deinit();
                return err;
            }
        };

        // Add the index variable to the loop scope if present
        if (foreach_expr.index_name) |index_name| {
            var index_type_info = ast.TypeInfo{ .base = .Int, .is_mutable = false };
            _ = loop_scope.createValueBinding(
                index_name.lexeme,
                TokenLiteral{ .nothing = {} }, // Will be set at runtime
                .INT, // Index is always an integer
                &index_type_info,
                false, // Loop variables are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        span.start,
                        "Duplicate loop variable name '{s}'",
                        .{index_name.lexeme},
                    );
                    self.fatal_error = true;
                    loop_scope.deinit();
                    return error.DuplicateVariableName;
                } else {
                    loop_scope.deinit();
                    return err;
                }
            };
        }

        // Temporarily set current scope to loop scope
        const prev_scope = self.current_scope;
        self.current_scope = loop_scope;

        // Analyze the body statements in the loop scope
        for (foreach_expr.body) |stmt| {
            try self.validateStatements(&[_]ast.Stmt{stmt});
        }

        // Restore previous scope and clean up loop scope
        self.current_scope = prev_scope;
        loop_scope.deinit();
    }

    // NEW: Validate if an expression returns a value
    fn validateExpressionReturns(self: *SemanticAnalyzer, expr: *ast.Expr, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!bool {
        switch (expr.data) {
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const return_type = try self.inferTypeFromExpr(value);
                    try self.validateReturnTypeCompatibility(&expected_return_type, return_type, expr.base.span);
                    return true;
                } else {
                    return expected_return_type.base == .Nothing;
                }
            },
            .If => {
                return self.validateIfExpressionReturns(expr, expected_return_type, func_span);
            },
            .Block => |block| {
                // Check statements in the block for return statements
                const block_returns = try self.validateReturnPaths(block.statements, expected_return_type, func_span);

                // Also check the final value expression if present
                if (block.value) |value| {
                    const value_returns = try self.validateExpressionReturns(value, expected_return_type, func_span);
                    return block_returns or value_returns;
                } else {
                    return block_returns;
                }
            },
            else => {
                // Other expressions don't return values in the control flow sense
                return false;
            },
        }
    }

    // NEW: Import validation
    fn validateImport(self: *SemanticAnalyzer, import_info: ast.ImportInfo, import_span: ast.SourceSpan) !void {
        // Check if module file exists
        const module_exists = self.checkModuleExists(import_info.module_path);
        if (!module_exists) {
            self.reporter.reportCompileError(
                import_span.start,
                "Module file not found: '{s}'",
                .{import_info.module_path},
            );
            self.fatal_error = true;
            return;
        }

        // Check for circular imports
        if (self.isCircularImport(import_info.module_path)) {
            self.reporter.reportCompileError(
                import_span.start,
                "Circular import detected: '{s}'",
                .{import_info.module_path},
            );
            self.fatal_error = true;
            return;
        }

        // If specific symbol is requested, validate it exists
        if (import_info.specific_symbol) |symbol_name| {
            const symbol_exists = self.checkSymbolExists(import_info.module_path, symbol_name);
            if (!symbol_exists) {
                self.reporter.reportCompileError(
                    import_span.start,
                    "Symbol '{s}' not found in module '{s}'",
                    .{ symbol_name, import_info.module_path },
                );
                self.fatal_error = true;
                return;
            }
        }
    }

    // Helper: Check if module file exists
    fn checkModuleExists(_: *SemanticAnalyzer, _: []const u8) bool {
        // TODO: Implement actual file system check
        // For now, assume all modules exist (they should be validated during parsing)
        return true;
    }

    // Helper: Check for circular imports
    fn isCircularImport(_: *SemanticAnalyzer, _: []const u8) bool {
        // TODO: Implement circular import detection
        // This would require tracking the import chain
        return false;
    }

    // Helper: Check if symbol exists in module
    fn checkSymbolExists(_: *SemanticAnalyzer, _: []const u8, _: []const u8) bool {
        // TODO: Implement symbol existence check
        // This would require loading the module and checking its public symbols
        return true;
    }

    // NEW: Infer return type from function body
    fn inferFunctionReturnType(self: *SemanticAnalyzer, func: anytype) ErrorList!*ast.TypeInfo {
        var return_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
        defer return_types.deinit();

        // Create function scope with parameters for return type inference
        const func_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
        defer func_scope.deinit();

        // Add parameters to function scope
        for (func.params) |param| {
            const param_type_info = if (param.type_expr) |type_expr|
                try ast.typeInfoFromExpr(self.allocator, type_expr)
            else
                try self.allocator.create(ast.TypeInfo);

            if (param.type_expr == null) {
                param_type_info.* = .{ .base = .Nothing }; // Default to nothing if no type specified
            }

            _ = func_scope.createValueBinding(
                param.name.lexeme,
                TokenLiteral{ .nothing = {} }, // Parameters get their values at call time
                self.convertTypeToTokenType(param_type_info.base),
                param_type_info,
                false, // Parameters are mutable
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    // This shouldn't happen during inference, but handle gracefully
                    continue;
                } else {
                    return err;
                }
            };
        }

        // Temporarily set current scope to function scope for return type inference
        const prev_scope = self.current_scope;
        self.current_scope = func_scope;

        // Collect all return statement types
        try self.collectReturnTypes(func.body, &return_types);

        // Restore previous scope
        self.current_scope = prev_scope;

        if (return_types.items.len == 0) {
            // No return statements - function returns Nothing
            const type_info = try self.allocator.create(ast.TypeInfo);
            type_info.* = .{ .base = .Nothing, .is_mutable = false };
            return type_info;
        }

        if (return_types.items.len == 1) {
            // Single return type - return it directly
            return return_types.items[0];
        }

        // Multiple return types - create a union
        return try self.createUnionType(return_types.items);
    }

    // NEW: Collect return types from function body
    fn collectReturnTypes(self: *SemanticAnalyzer, statements: []ast.Stmt, return_types: *std.ArrayList(*ast.TypeInfo)) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .Return => |return_stmt| {
                    if (return_stmt.value) |value| {
                        const return_type = try self.inferTypeFromExpr(value);
                        try return_types.append(return_type);
                    } else {
                        // Return without value - add Nothing type
                        const nothing_type = try self.allocator.create(ast.TypeInfo);
                        nothing_type.* = .{ .base = .Nothing, .is_mutable = false };
                        try return_types.append(nothing_type);
                    }
                },
                .Block => |block_stmts| {
                    try self.collectReturnTypes(block_stmts, return_types);
                },
                .Expression => |expr| {
                    if (expr) |expression| {

                        // Check if this is a return expression
                        if (expression.data == .ReturnExpr) {
                            const return_expr = expression.data.ReturnExpr;
                            if (return_expr.value) |value| {
                                const return_type = try self.inferTypeFromExpr(value);
                                try return_types.append(return_type);
                            } else {
                                // Return without value - add Nothing type
                                const nothing_type = try self.allocator.create(ast.TypeInfo);
                                nothing_type.* = .{ .base = .Nothing, .is_mutable = false };
                                try return_types.append(nothing_type);
                            }
                        }
                        // Check if this is an if expression that might return
                        else if (expression.data == .If) {
                            try self.collectReturnTypesFromIf(expression, return_types);
                        }
                        // Check if this is a block expression
                        else if (expression.data == .Block) {
                            const block_expr = expression.data.Block;
                            if (block_expr.value) |value| {
                                try self.collectReturnTypesFromExpr(value, return_types);
                            }
                        }
                    }
                },
                else => {},
            }
        }
    }

    // NEW: Collect return types from if expressions
    fn collectReturnTypesFromIf(self: *SemanticAnalyzer, if_expr: *ast.Expr, return_types: *std.ArrayList(*ast.TypeInfo)) ErrorList!void {
        const if_data = if_expr.data.If;

        // Check then branch
        if (if_data.then_branch) |then_branch| {
            try self.collectReturnTypesFromExpr(then_branch, return_types);
        }

        // Check else branch
        if (if_data.else_branch) |else_branch| {
            try self.collectReturnTypesFromExpr(else_branch, return_types);
        }
    }

    // NEW: Collect return types from expressions
    fn collectReturnTypesFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr, return_types: *std.ArrayList(*ast.TypeInfo)) ErrorList!void {
        switch (expr.data) {
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const return_type = try self.inferTypeFromExpr(value);
                    try return_types.append(return_type);
                } else {
                    const nothing_type = try self.allocator.create(ast.TypeInfo);
                    nothing_type.* = .{ .base = .Nothing, .is_mutable = false };
                    try return_types.append(nothing_type);
                }
            },
            .If => {
                try self.collectReturnTypesFromIf(expr, return_types);
            },
            .Block => |block| {
                // Check statements in the block for return statements
                try self.collectReturnTypes(block.statements, return_types);

                // Also check the final value expression if present
                if (block.value) |value| {
                    try self.collectReturnTypesFromExpr(value, return_types);
                }
            },
            else => {
                // Other expressions don't return values in the control flow sense
            },
        }
    }

    // NEW: Validate return type compatibility with union subtyping
    fn validateReturnTypeCompatibility(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
        // Handle union types for return type checking
        if (expected.base == .Union) {
            if (expected.union_type) |union_type| {

                // Check if actual type is a union
                if (actual.base == .Union) {
                    if (actual.union_type) |actual_union| {
                        // Check that every member of the actual union is compatible with the expected union
                        for (actual_union.types) |actual_member| {
                            var found_match = false;
                            for (union_type.types) |expected_member| {
                                if (actual_member.base == expected_member.base) {
                                    found_match = true;
                                    break;
                                }
                            }
                            if (!found_match) {
                                self.reporter.reportCompileError(
                                    span.start,
                                    "Return type {s} is not compatible with function's union return type",
                                    .{@tagName(actual_member.base)},
                                );
                                self.fatal_error = true;
                                return;
                            }
                        }
                        return; // All members are compatible
                    }
                } else {
                    // Single type - check if it's a member of the expected union
                    var found_match = false;
                    for (union_type.types) |member_type| {
                        if (member_type.base == actual.base) {
                            found_match = true;
                            break;
                        }
                        // Allow Custom and Struct types to be compatible when they refer to the same user-defined struct
                        if ((member_type.base == .Custom and actual.base == .Struct) or
                            (member_type.base == .Struct and actual.base == .Custom))
                        {
                            // Check if they refer to the same custom type
                            if (member_type.custom_type != null and actual.custom_type != null and
                                std.mem.eql(u8, member_type.custom_type.?, actual.custom_type.?))
                            {
                                found_match = true;
                                break;
                            }
                        }
                    }

                    if (!found_match) {
                        // Build a list of allowed types for the error message
                        var type_list = std.ArrayList(u8).init(self.allocator);
                        defer type_list.deinit();

                        for (union_type.types, 0..) |member_type, i| {
                            if (i > 0) try type_list.appendSlice(" | ");
                            try type_list.appendSlice(@tagName(member_type.base));
                        }

                        self.reporter.reportCompileError(
                            span.start,
                            "Return type {s} is not compatible with function's union return type ({s})",
                            .{ @tagName(actual.base), type_list.items },
                        );
                        self.fatal_error = true;
                        return;
                    }
                }
            }
        } else {
            // Non-union expected type - use regular type unification
            try self.unifyTypes(expected, actual, span);
        }
    }
};
