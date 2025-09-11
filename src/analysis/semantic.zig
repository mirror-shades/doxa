const std = @import("std");
const ast = @import("../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;
const Parser = @import("../parser/parser_types.zig").Parser;
const import_parser = @import("../parser/import_parser.zig");

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

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

/// Helper function to get location from AST Base, handling optional spans
fn getLocationFromBase(base: ast.Base) Reporting.Location {
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

pub const SemanticAnalyzer = struct {
    in_loop_scope: bool = false,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),
    custom_types: std.StringHashMap(CustomTypeInfo),
    struct_methods: std.StringHashMap(std.StringHashMap(@This().StructMethodInfo)),
    function_return_types: std.AutoHashMap(NodeId, *ast.TypeInfo),
    current_function_returns: std.ArrayList(*ast.TypeInfo),
    current_initializing_var: ?[]const u8 = null,
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
            .struct_methods = std.StringHashMap(std.StringHashMap(@This().StructMethodInfo)).init(allocator),
            .function_return_types = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
            .current_function_returns = std.ArrayList(*ast.TypeInfo).init(allocator),
            .parser = parser,
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        self.type_cache.deinit();
        self.custom_types.deinit();
        var methods_it = self.struct_methods.valueIterator();
        while (methods_it.next()) |tbl| tbl.*.deinit();
        self.struct_methods.deinit();
        self.function_return_types.deinit();
        self.current_function_returns.deinit();
    }

    pub const StructMethodInfo = struct {
        name: []const u8,
        is_public: bool,
        is_static: bool,
        return_type: *ast.TypeInfo,
    };

    // NEW: Export custom type information for HIR generation
    pub fn getCustomTypes(self: *SemanticAnalyzer) std.StringHashMap(CustomTypeInfo) {
        return self.custom_types;
    }

    // NEW: Export struct methods for HIR generation
    pub fn getStructMethods(self: *SemanticAnalyzer) std.StringHashMap(std.StringHashMap(@This().StructMethodInfo)) {
        return self.struct_methods;
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

    /// Check if a union type contains Nothing as one of its members
    fn unionContainsNothing(self: *SemanticAnalyzer, union_type_info: ast.TypeInfo) bool {
        _ = self;
        if (union_type_info.base != .Union) return false;
        if (union_type_info.union_type) |union_type| {
            for (union_type.types) |member_type| {
                if (member_type.base == .Nothing) return true;
            }
        }
        return false;
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

    // NEW: Register struct with fields
    fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
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

    // Inject compiler-provided enums (called once per program)
    fn ensureBuiltinEnums(self: *SemanticAnalyzer) !void {
        if (!self.custom_types.contains("NumberError")) {
            const variants = [_][]const u8{ "ParseFailed", "Overflow", "Underflow" };
            try self.registerEnumType("NumberError", &variants);
        }
        if (!self.custom_types.contains("IndexError")) {
            const variants = [_][]const u8{"OutOfBounds"};
            try self.registerEnumType("IndexError", &variants);
        }
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
            field_type_info: *ast.TypeInfo,
            custom_type_name: ?[]const u8 = null, // For custom types like Person
            index: u32,
            is_public: bool = false,
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
            .Union => .Union,
            .Unknown => .Nothing, // TODO: This feels hacky
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
            .Union => .Union, // Unions need special handling
        };
    }

    // Helper function to convert SemanticAnalyzer.CustomTypeInfo to TypeSystem.CustomTypeInfo
    pub fn convertCustomTypeInfo(semantic_type: CustomTypeInfo, allocator: std.mem.Allocator) !@import("../codegen/hir/type_system.zig").TypeSystem.CustomTypeInfo {
        var hir_type = @import("../codegen/hir/type_system.zig").TypeSystem.CustomTypeInfo{
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
            const converted_variants = try allocator.alloc(@import("../codegen/hir/type_system.zig").TypeSystem.CustomTypeInfo.EnumVariant, variants.len);
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
            const converted_fields = try allocator.alloc(@import("../codegen/hir/type_system.zig").TypeSystem.CustomTypeInfo.StructField, fields.len);
            for (fields, 0..) |field, i| {
                // Map to coarse HIRType here without relying on self
                const mapped_hir_type: HIRType = switch (field.field_type_info.base) {
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
                converted_fields[i] = .{
                    .name = field.name,
                    .field_type = mapped_hir_type,
                    .index = field.index,
                    .custom_type_name = field.custom_type_name,
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

        // Inject compiler-provided enums (shared error categories)
        try self.ensureBuiltinEnums();

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
        // First pass: pre-register all function declarations so calls can see them
        for (statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl => |func| {
                    // Create function type
                    const func_type = try self.allocator.create(ast.FunctionType);

                    // Infer or use explicit return type
                    var inferred_return_type = func.return_type_info;
                    if (func.return_type_info.base == .Nothing) {
                        const inferred = try self.inferFunctionReturnType(func);
                        inferred_return_type = inferred.*;
                    } else if (func.return_type_info.base == .Union) {
                        if (func.return_type_info.union_type) |union_type| {
                            const flattened = try self.flattenUnionType(union_type);
                            inferred_return_type = ast.TypeInfo{
                                .base = .Union,
                                .union_type = flattened,
                                .is_mutable = false,
                            };
                        }
                    }

                    // Build parameter types and track aliases
                    var param_types = try self.allocator.alloc(ast.TypeInfo, func.params.len);
                    var param_aliases = try self.allocator.alloc(bool, func.params.len);
                    for (func.params, 0..) |param, i| {
                        param_aliases[i] = param.is_alias;
                        if (param.type_expr) |type_expr| {
                            const param_type_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                            defer self.allocator.destroy(param_type_ptr);

                            var resolved_param_type = try self.resolveTypeInfo(param_type_ptr.*);
                            if (resolved_param_type.base == .Union) {
                                if (resolved_param_type.union_type) |u| {
                                    const flattened = try self.flattenUnionType(u);
                                    resolved_param_type = ast.TypeInfo{
                                        .base = .Union,
                                        .union_type = flattened,
                                        .is_mutable = false,
                                    };
                                }
                            }
                            param_types[i] = resolved_param_type;
                        } else {
                            param_types[i] = .{ .base = .Nothing };
                        }
                    }

                    // Set up function type
                    func_type.* = .{
                        .params = param_types,
                        .return_type = try self.allocator.create(ast.TypeInfo),
                        .param_aliases = param_aliases,
                    };
                    func_type.return_type.* = inferred_return_type;

                    // Add function to current scope if not already present
                    if (scope.lookupVariable(func.name.lexeme) == null) {
                        const func_type_info = try self.allocator.create(ast.TypeInfo);
                        func_type_info.* = .{ .base = .Function, .function_type = func_type };

                        var env = @import("../interpreter/environment.zig").init(
                            self.allocator,
                            null,
                            false,
                            self.memory,
                        );
                        _ = scope.createValueBinding(
                            func.name.lexeme,
                            TokenLiteral{ .function = .{ .params = func.params, .body = func.body, .closure = &env, .defining_module = null } },
                            .FUNCTION,
                            func_type_info,
                            true,
                        ) catch |err| {
                            if (err == error.DuplicateVariableName) {
                                // Ignore duplicates in pre-pass; they will be reported later
                            } else {
                                return err;
                            }
                        };

                        try self.function_return_types.put(stmt.base.id, func_type.return_type);
                    }
                },
                else => {},
            }
        }

        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // Create TypeInfo
                    const type_info = try self.allocator.create(ast.TypeInfo);
                    errdefer self.allocator.destroy(type_info);

                    // Check if we have an explicit type annotation
                    if (decl.type_info.base != .Nothing) {
                        // If parser provided an incomplete array type (no element), prefer inferring from initializer if present
                        if (decl.type_info.base == .Array and decl.type_info.array_type == null) {
                            if (decl.initializer) |init_expr| {
                                const inferred = try self.inferTypeFromExpr(init_expr);
                                type_info.* = inferred.*;
                            } else {
                                // Use explicit type, but resolve custom types
                                type_info.* = try self.resolveTypeInfo(decl.type_info);
                            }
                        } else {
                            // Use explicit type, but resolve custom types
                            type_info.* = try self.resolveTypeInfo(decl.type_info);
                        }
                        // Preserve mutability from the variable declaration (var vs const)
                        type_info.is_mutable = decl.type_info.is_mutable;
                    } else if (decl.initializer) |init_expr| {
                        // Infer from initializer
                        const inferred = try self.inferTypeFromExpr(init_expr);
                        // Deep copy the inferred type to avoid dangling internal pointers
                        type_info.* = try self.deepCopyTypeInfo(inferred.*);
                        // Preserve the mutability from the variable declaration, not from the initializer
                        type_info.is_mutable = decl.type_info.is_mutable;
                    } else {
                        // No type annotation and no initializer - this is invalid
                        const location = getLocationFromBase(stmt.base);
                        self.reporter.reportCompileError(
                            location,
                            ErrorCode.VARIABLE_DECLARATION_MISSING_ANNOTATION,
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
                            .Union => if (type_info.union_type) |ut| self.getUnionDefaultValue(ut) else TokenLiteral{ .nothing = {} },
                            else => TokenLiteral{ .nothing = {} },
                        };

                    // Convert value to match the declared type
                    value = try self.convertValueToType(value, type_info.base);

                    // ENFORCE: nothing types must be const
                    if (type_info.base == .Nothing and type_info.is_mutable) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(stmt.base),
                            ErrorCode.NOTHING_TYPE_MUST_BE_CONST,
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
                                getLocationFromBase(stmt.base),
                                ErrorCode.DUPLICATE_VARIABLE,
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
                .FunctionDecl => |_| {
                    // Already registered in pre-pass. Validation will occur later.
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
                                getLocationFromBase(stmt.base),
                                ErrorCode.DUPLICATE_VARIABLE,
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
                        // Ensure expression statements are semantically analyzed so that
                        // compiler methods like @push are lowered (e.g., InternalCall -> ArrayPush)
                        // even when their values are not used.
                        _ = try self.inferTypeFromExpr(expr);

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
                                // Propagate field visibility for later checks
                                _ = field.is_public; // captured via struct_decl when building CustomTypeInfo below
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
                                        getLocationFromBase(stmt.base),
                                        ErrorCode.DUPLICATE_VARIABLE,
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

                            // Override visibility flags in the registered struct with those declared
                            if (self.custom_types.get(struct_decl.name.lexeme)) |*ct| {
                                if (ct.kind == .Struct and ct.struct_fields != null) {
                                    const sf = ct.struct_fields.?;
                                    var field_index_map = std.StringHashMap(usize).init(self.allocator);
                                    defer field_index_map.deinit();
                                    for (struct_decl.fields, 0..) |fptr, i| {
                                        _ = try field_index_map.put(fptr.name.lexeme, i);
                                    }
                                    for (sf) |*sfld| {
                                        if (field_index_map.get(sfld.name)) |i| {
                                            // Use parser flag is_public from AST field list
                                            const declared_public = struct_decl.fields[i].is_public;
                                            sfld.is_public = declared_public;
                                        }
                                    }
                                }
                            }

                            // Register struct methods (static and instance)
                            var method_table = std.StringHashMap(@This().StructMethodInfo).init(self.allocator);
                            defer {
                                // If we inserted the table into struct_methods we must not deinit here
                                // Deinit only if not stored due to error
                            }
                            for (struct_decl.methods) |m| {
                                // Determine return type: use declared, or default for instance methods to the struct type when missing
                                var ret_type_ptr: *ast.TypeInfo = undefined;
                                if (m.return_type_info.base != .Nothing) {
                                    const rtp = try self.allocator.create(ast.TypeInfo);
                                    rtp.* = try self.resolveTypeInfo(m.return_type_info);
                                    ret_type_ptr = rtp;
                                } else {
                                    // Heuristic: if instance method without explicit return, default to this struct type
                                    const rtp = try self.allocator.create(ast.TypeInfo);
                                    rtp.* = .{ .base = .Struct, .custom_type = struct_decl.name.lexeme, .struct_fields = struct_fields, .is_mutable = false };
                                    ret_type_ptr = rtp;
                                }

                                const mi = @This().StructMethodInfo{
                                    .name = m.name.lexeme,
                                    .is_public = m.is_public,
                                    .is_static = m.is_static,
                                    .return_type = ret_type_ptr,
                                };
                                // Last one wins if duplicate
                                _ = try method_table.put(m.name.lexeme, mi);
                            }
                            // Store/merge into analyzer's method registry
                            if (self.struct_methods.getPtr(struct_decl.name.lexeme)) |existing| {
                                var it = method_table.iterator();
                                while (it.next()) |entry| {
                                    _ = try existing.put(entry.key_ptr.*, entry.value_ptr.*);
                                }
                                // method_table will be deinit'd by defer but entries are copied
                            } else {
                                try self.struct_methods.put(struct_decl.name.lexeme, method_table);
                                // Prevent defer cleanup for stored table
                                // Note: intentionally not calling method_table.deinit()
                            }
                        }
                    }
                },
                .Import => |import_info| {
                    // NEW: Validate import
                    try self.validateImport(import_info, .{ .location = getLocationFromBase(stmt.base) });
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
                                // If parser provided an incomplete array type (no element), prefer inferring from initializer if present
                                if (decl.type_info.base == .Array and decl.type_info.array_type == null) {
                                    if (decl.initializer) |init_expr| {
                                        const inferred = try self.inferTypeFromExpr(init_expr);
                                        type_info.* = inferred.*;
                                    } else {
                                        // Use explicit type, but resolve custom types
                                        type_info.* = try self.resolveTypeInfo(decl.type_info);
                                    }
                                } else {
                                    // Use explicit type, but resolve custom types
                                    type_info.* = try self.resolveTypeInfo(decl.type_info);
                                }
                                // Preserve mutability from the variable declaration (var vs const)
                                type_info.is_mutable = decl.type_info.is_mutable;
                            } else if (decl.initializer) |init_expr| {
                                // Infer from initializer
                                const inferred = try self.inferTypeFromExpr(init_expr);
                                type_info.* = inferred.*;
                            } else {
                                // No type annotation and no initializer - this is invalid
                                self.reporter.reportCompileError(
                                    getLocationFromBase(stmt.base),
                                    ErrorCode.VARIABLE_DECLARATION_MISSING_ANNOTATION,
                                    "Variable declaration requires either type annotation (::) or initializer",
                                    .{},
                                );
                                self.fatal_error = true;
                                continue;
                            }

                            const token_type = self.convertTypeToTokenType(type_info.base);

                            var value: TokenLiteral = undefined;

                            if (decl.initializer) |init_expr| {
                                self.current_initializing_var = decl.name.lexeme;
                                defer {
                                    self.current_initializing_var = null;
                                }

                                value = try self.evaluateExpression(init_expr);
                            } else {
                                // Only use defaults for uninitialized variables
                                value = switch (type_info.base) {
                                    .Int => TokenLiteral{ .int = 0 },
                                    .Float => TokenLiteral{ .float = 0.0 },
                                    .String => TokenLiteral{ .string = "" },
                                    .Tetra => TokenLiteral{ .tetra = .false },
                                    .Byte => TokenLiteral{ .byte = 0 },
                                    .Union => if (type_info.union_type) |ut|
                                        self.getUnionDefaultValue(ut)
                                    else
                                        TokenLiteral{ .nothing = {} },
                                    else => TokenLiteral{ .nothing = {} },
                                };
                            }

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
                                        getLocationFromBase(stmt.base),
                                        ErrorCode.DUPLICATE_VARIABLE,
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
                            try self.unifyTypes(&decl_type_copy, init_type, .{ .location = getLocationFromBase(stmt.base) });
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
                        _ = try self.inferTypeFromExpr(expression);
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
                        try self.validateFunctionBody(func, .{ .location = getLocationFromBase(stmt.base) }, return_type.*);
                    } else {
                        // Fallback to original return type if not found
                        try self.validateFunctionBody(func, .{ .location = getLocationFromBase(stmt.base) }, func.return_type_info);
                    }
                },
                .MapLiteral => |map_entries| {
                    // Validate map literal entries
                    for (map_entries) |entry| {
                        _ = try self.inferTypeFromExpr(entry.key);
                        _ = try self.inferTypeFromExpr(entry.value);
                    }
                },
                // TODO: Handle other statements...
                else => {},
            }
        }
    }

    // Default value for unions: use the first member's default
    fn getUnionDefaultValue(self: *SemanticAnalyzer, union_type: *ast.UnionType) TokenLiteral {
        _ = self;
        if (union_type.types.len == 0) {
            return TokenLiteral{ .nothing = {} };
        }

        const first_type = union_type.types[0];
        return switch (first_type.base) {
            .Int => TokenLiteral{ .int = 0 },
            .Float => TokenLiteral{ .float = 0.0 },
            .String => TokenLiteral{ .string = "" },
            .Tetra => TokenLiteral{ .tetra = .false },
            .Byte => TokenLiteral{ .byte = 0 },
            .Nothing => TokenLiteral{ .nothing = {} },
            else => TokenLiteral{ .nothing = {} },
        };
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
                        try self.unifyTypes(exp_elem, act_elem, span);
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

        // Clear pointer fields; we'll rebuild them freshly as needed
        copied.array_type = null;
        copied.struct_fields = null;
        copied.function_type = null;
        copied.variants = null;
        copied.union_type = null;
        copied.map_key_type = null;
        copied.map_value_type = null;

        switch (type_info.base) {
            .Array => {
                if (type_info.array_type) |elem| {
                    const elem_copy = try self.allocator.create(ast.TypeInfo);
                    elem_copy.* = try self.deepCopyTypeInfo(elem.*);
                    copied.array_type = elem_copy;
                }
            },
            .Struct, .Custom => {
                if (type_info.struct_fields) |fields| {
                    const new_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                    for (fields, 0..) |field, i| {
                        const ti_copy = try self.allocator.create(ast.TypeInfo);
                        ti_copy.* = try self.deepCopyTypeInfo(field.type_info.*);
                        new_fields[i] = .{ .name = field.name, .type_info = ti_copy };
                    }
                    copied.struct_fields = new_fields;
                }
            },
            .Function => {
                if (type_info.function_type) |fn_type| {
                    const params_copy = try self.allocator.alloc(ast.TypeInfo, fn_type.params.len);
                    for (fn_type.params, 0..) |p, i| {
                        params_copy[i] = try self.deepCopyTypeInfo(p);
                    }
                    const ret_copy = try self.allocator.create(ast.TypeInfo);
                    ret_copy.* = try self.deepCopyTypeInfo(fn_type.return_type.*);
                    const fn_copy = try self.allocator.create(ast.FunctionType);
                    fn_copy.* = .{
                        .params = params_copy,
                        .return_type = ret_copy,
                        .param_aliases = if (fn_type.param_aliases) |pa| try self.allocator.dupe(bool, pa) else null,
                    };
                    copied.function_type = fn_copy;
                }
            },
            .Union => {
                if (type_info.union_type) |u| {
                    const new_types = try self.allocator.alloc(*ast.TypeInfo, u.types.len);
                    for (u.types, 0..) |member, i| {
                        const m_copy = try self.allocator.create(ast.TypeInfo);
                        m_copy.* = try self.deepCopyTypeInfo(member.*);
                        new_types[i] = m_copy;
                    }
                    const u_copy = try self.allocator.create(ast.UnionType);
                    u_copy.* = .{ .types = new_types, .current_type_index = u.current_type_index };
                    copied.union_type = u_copy;
                }
            },
            .Enum => {
                if (type_info.variants) |v| {
                    // Duplicate outer slice only; inner strings are not owned here
                    copied.variants = try self.allocator.dupe([]const u8, v);
                }
            },
            .Map => {
                // Shallow copy key/value type pointers to avoid dereferencing
                // potentially invalid pointers from inference.
                copied.map_key_type = type_info.map_key_type;
                copied.map_value_type = type_info.map_value_type;
            },
            else => {},
        }

        return copied;
    }

    fn deepCopyTypeInfoPtr(self: *SemanticAnalyzer, src: *ast.TypeInfo) !*ast.TypeInfo {
        const copied = try self.allocator.create(ast.TypeInfo);
        copied.* = try self.deepCopyTypeInfo(src.*);
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
            .Map => |entries| {
                // Minimal inference for map literals: infer key/value from first entry if present.
                type_info.* = .{ .base = .Map };
                if (entries.len > 0) {
                    const first_key_type = try self.inferTypeFromExpr(entries[0].key);
                    const first_val_type = try self.inferTypeFromExpr(entries[0].value);
                    // Reuse inferred pointers to avoid unnecessary deep copies and allocations.
                    type_info.map_key_type = first_key_type;
                    type_info.map_value_type = first_val_type;
                }
            },
            .Literal => |lit| {
                type_info.inferFrom(lit); // TokenLiteral is already the right type
            },
            .Break => {
                type_info.base = .Nothing;
            },
            .Increment => {
                type_info.base = .Int;
            },
            .Decrement => {
                type_info.base = .Int;
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
                            getLocationFromBase(expr.base),
                            ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                            "Division requires numeric operands, got {s}",
                            .{@tagName(left_type.base)},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
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
                            getLocationFromBase(expr.base),
                            ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
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
                } else if (std.mem.eql(u8, op, "+")) {
                    // Handle string and array concatenation first
                    if (left_type.base == .String and right_type.base == .String) {
                        type_info.* = .{ .base = .String }; // String concatenation
                    } else if (left_type.base == .Array and right_type.base == .Array) {
                        type_info.* = .{ .base = .Array }; // Array concatenation
                    } else if (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte or
                        right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte)
                    {
                        // Numeric addition with type promotion
                        // Type promotion rules: Float > Int > Byte
                        if (left_type.base == .Float or right_type.base == .Float) {
                            type_info.* = .{ .base = .Float };
                        } else if (left_type.base == .Int or right_type.base == .Int) {
                            type_info.* = .{ .base = .Int };
                        } else {
                            type_info.* = .{ .base = .Byte };
                        }
                    } else {
                        // Type mismatch - report compile error
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.TYPE_MISMATCH,
                            "Cannot use + operator between {s} and {s}. Both operands must be the same type.",
                            .{ @tagName(left_type.base), @tagName(right_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                    // Arithmetic with type promotion
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
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
                } else if (std.mem.eql(u8, op, "**")) {
                    // Power operator with type promotion
                    if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                        (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                    {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                            "Power operator requires numeric operands",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // Power operator type promotion: Float > Int > Byte
                    // If either operand is Float, result is Float
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
                    std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "equals") or std.mem.eql(u8, op, "!="))
                {
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
                            getLocationFromBase(expr.base),
                            ErrorCode.INCOMPATIBLE_TYPES_FOR_COMPARISON,
                            "Cannot compare {s} with {s}",
                            .{ @tagName(left_type.base), @tagName(right_type.base) },
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else {
                    // Default case: location is cast to the AST type
                    try self.unifyTypes(left_type, right_type, .{ .location = getLocationFromBase(expr.base) });
                    type_info.* = left_type.*;
                }
            },
            .Variable => |var_token| {
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        type_info.* = storage.type_info.*;
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INTERNAL_ERROR,
                            "Internal error: Variable storage not found",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.VARIABLE_NOT_FOUND,
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
            .FunctionCall => |function_call| {
                // Check if this is a method call (callee is FieldAccess)
                if (function_call.callee.data == .FieldAccess) {
                    const field_access = function_call.callee.data.FieldAccess;
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

                        // Check if this is a static method call on a struct type (e.g., Point.New)
                        if (self.custom_types.get(object_name)) |custom_type| {
                            if (custom_type.kind == .Struct) {
                                if (self.struct_methods.get(object_name)) |method_table| {
                                    if (method_table.get(method_name)) |method_info| {
                                        if (method_info.is_static) {
                                            type_info.* = method_info.return_type.*;
                                            return type_info;
                                        }
                                    }
                                }
                            }
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
                                    getLocationFromBase(expr.base),
                                    ErrorCode.UNKNOWN_METHOD,
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
                                    getLocationFromBase(expr.base),
                                    ErrorCode.UNKNOWN_METHOD,
                                    "Unknown string method '{s}'",
                                    .{method_name},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                            }
                        },
                        .Map => {
                            // Disallow dot-methods on maps; use bracket indexing instead
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.UNKNOWN_METHOD,
                                "Maps do not have methods; use indexing (map[key]) or assignment (map[key] is value)",
                                .{},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        },
                        .Struct, .Custom => {
                            // Handle both Struct and Custom types for method resolution
                            var struct_name: ?[]const u8 = null;
                            if (object_type.base == .Custom) {
                                if (object_type.custom_type) |ct_name| {
                                    struct_name = ct_name;
                                }
                            } else if (object_type.base == .Struct) {
                                if (object_type.custom_type) |ct_name| {
                                    struct_name = ct_name;
                                }
                            }

                            // Prefer registered methods over fields when resolving a call
                            if (struct_name) |name| {
                                if (self.struct_methods.get(name)) |tbl| {
                                    if (tbl.get(method_name)) |mi| {
                                        type_info.* = mi.return_type.*;
                                        return type_info;
                                    }
                                }
                            }

                            // Fallback: allow calling function-typed fields as methods (if any)
                            if (object_type.struct_fields) |fields| {
                                for (fields) |field| {
                                    if (std.mem.eql(u8, field.name, method_name) and field.type_info.base == .Function) {
                                        type_info.* = field.type_info.*;
                                        return type_info;
                                    }
                                }
                            }

                            // If we reach here, no method matched
                            const display_name = struct_name orelse @as([]const u8, "<struct>");
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.UNKNOWN_METHOD,
                                "Unknown method '{s}' on struct '{s}'",
                                .{ method_name, display_name },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        },
                        else => {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.CANNOT_CALL_METHOD_ON_TYPE,
                                "Cannot call method '{s}' on type {s}",
                                .{ method_name, @tagName(object_type.base) },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        },
                    }
                } else {
                    // Regular function call
                    const callee_type = try self.inferTypeFromExpr(function_call.callee);
                    if (callee_type.base == .Function) {
                        if (callee_type.function_type) |func_type| {
                            // Validate argument count allowing default placeholders (~)
                            const expected_arg_count: usize = func_type.params.len;
                            var provided_arg_count: usize = 0;
                            for (function_call.arguments) |arg_expr_it| {
                                if (arg_expr_it.expr.data != .DefaultArgPlaceholder) provided_arg_count += 1;
                            }
                            if (provided_arg_count > expected_arg_count) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(expr.base),
                                    ErrorCode.ARGUMENT_COUNT_MISMATCH,
                                    "Argument count mismatch: expected at most {}, got {}",
                                    .{ expected_arg_count, provided_arg_count },
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }

                            // Validate each non-placeholder argument type against parameter type
                            var param_index: usize = 0;
                            for (function_call.arguments) |arg_expr_it| {
                                if (arg_expr_it.expr.data == .DefaultArgPlaceholder) {
                                    if (param_index < expected_arg_count) param_index += 1;
                                    continue;
                                }
                                if (param_index >= expected_arg_count) break;

                                // Check if parameter requires alias but argument is not marked as alias
                                if (func_type.param_aliases != null and func_type.param_aliases.?[param_index] and !arg_expr_it.is_alias) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(arg_expr_it.expr.base),
                                        ErrorCode.ALIAS_PARAMETER_REQUIRED,
                                        "Function parameter requires an alias argument (use ^ before the argument)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.base = .Nothing;
                                    return type_info;
                                }

                                // Check if parameter is not alias but argument is marked as alias
                                if (func_type.param_aliases != null and !func_type.param_aliases.?[param_index] and arg_expr_it.is_alias) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(arg_expr_it.expr.base),
                                        ErrorCode.ALIAS_ARGUMENT_NOT_NEEDED,
                                        "Function parameter does not require an alias argument (remove ^ before the argument)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.base = .Nothing;
                                    return type_info;
                                }

                                const arg_type = try self.inferTypeFromExpr(arg_expr_it.expr);
                                if (func_type.params[param_index].base != .Nothing) {
                                    try self.unifyTypes(&func_type.params[param_index], arg_type, .{ .location = getLocationFromBase(expr.base) });
                                }
                                param_index += 1;
                            }

                            // Propagate return type
                            type_info.* = func_type.return_type.*;
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_FUNCTION_TYPE,
                                "Function type has no return type information",
                                .{},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_FUNCTION_CALL,
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
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_ARRAY_INDEX_TYPE,
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
                        // FIX: If no array_type is set, try to infer from the variable declaration
                        // This handles cases like "var tape :: byte[] is [...]" where the type annotation
                        // should be used to determine the return type
                        if (index.array.data == .Variable) {
                            const var_name = index.array.data.Variable.lexeme;
                            if (self.current_scope) |scope| {
                                if (scope.lookupVariable(var_name)) |variable| {
                                    // Access the type info through the scope manager's value storage
                                    if (scope.manager.value_storage.get(variable.storage_id)) |storage| {
                                        if (storage.type_info.base == .Array) {
                                            if (storage.type_info.array_type) |declared_elem_type| {
                                                type_info.* = declared_elem_type.*;
                                                return type_info;
                                            } else {}
                                        }
                                    } else {}
                                } else {}
                            } else {}
                        } else {}
                        type_info.base = .Nothing;
                    }
                } else if (array_type.base == .Map) {
                    // Map indexing - allow string, int, or enum keys, but not unions
                    if (index_type.base != .String and index_type.base != .Int and index_type.base != .Enum) {
                        if (index_type.base == .Union) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_MAP_KEY_TYPE,
                                "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                                .{},
                            );
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_MAP_KEY_TYPE,
                                "Map key must be string, int, or enum, got {s}",
                                .{@tagName(index_type.base)},
                            );
                        }
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }

                    // Use the stored map value type; this must be present if inference worked
                    if (array_type.map_value_type) |value_type| {
                        type_info.* = value_type.*;
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE, // reuse for now; dedicated code can be added later
                            "Map value type not inferred; declare explicit 'returns <Type>' or add a first entry to infer",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (array_type.base == .String) {
                    // String indexing
                    if (index_type.base != .Int) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_STRING_INDEX_TYPE,
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
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_INDEX_TYPE,
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

                // First check if the object type is Custom and needs to be resolved to a struct
                var resolved_object_type = object_type;
                if (object_type.base == .Custom) {
                    if (object_type.custom_type) |custom_type_name| {
                        // First check if this is a module namespace
                        if (self.isModuleNamespace(custom_type_name)) {
                            // Handle module namespace access
                            return self.handleModuleFieldAccess(custom_type_name, field.field.lexeme, .{ .location = getLocationFromBase(expr.base) });
                        }

                        if (self.custom_types.get(custom_type_name)) |custom_type| {
                            if (custom_type.kind == .Struct) {
                                // Convert the Custom type to a Struct type for consistent handling
                                const struct_fields = try self.allocator.alloc(ast.StructFieldType, custom_type.struct_fields.?.len);
                                for (custom_type.struct_fields.?, 0..) |custom_field, i| {
                                    struct_fields[i] = .{
                                        .name = custom_field.name,
                                        .type_info = custom_field.field_type_info,
                                    };
                                }
                                const resolved_type_info = try self.allocator.create(ast.TypeInfo);
                                resolved_type_info.* = ast.TypeInfo{ .base = .Struct, .custom_type = custom_type_name, .struct_fields = struct_fields, .is_mutable = object_type.is_mutable };
                                resolved_object_type = resolved_type_info;
                            } else {
                                // Not a struct, treat as enum variant access
                                type_info.* = .{ .base = .Enum };
                                return type_info;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TYPE_MISMATCH,
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
                        return type_info;
                    }
                }

                // Now handle field access on the resolved object type
                if (resolved_object_type.base == .Struct) {
                    if (resolved_object_type.struct_fields) |fields| {
                        for (fields) |struct_field| {
                            if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {

                                // Copy the field type info
                                type_info.* = struct_field.type_info.*;

                                // If this field is a struct type, look up its fields from custom_types
                                if (type_info.base == .Custom and type_info.custom_type != null) {
                                    if (self.custom_types.get(type_info.custom_type.?)) |custom_type| {
                                        if (custom_type.kind == .Struct) {
                                            // Convert CustomTypeInfo.StructField to ast.StructFieldType
                                            const struct_fields_inner = try self.allocator.alloc(ast.StructFieldType, custom_type.struct_fields.?.len);
                                            for (custom_type.struct_fields.?, 0..) |custom_field, i| {
                                                struct_fields_inner[i] = .{
                                                    .name = custom_field.name,
                                                    .type_info = custom_field.field_type_info,
                                                };
                                            }
                                            type_info.* = .{ .base = .Struct, .custom_type = type_info.custom_type, .struct_fields = struct_fields_inner, .is_mutable = false };
                                        }
                                    }
                                }

                                // Enforce private field access: if this struct originates from a named custom type
                                // and the field is marked private, disallow access from outside that type context.
                                if (resolved_object_type.custom_type) |owner_name| {
                                    if (self.custom_types.get(owner_name)) |owner_ct| {
                                        if (owner_ct.kind == .Struct and owner_ct.struct_fields != null) {
                                            const owner_fields = owner_ct.struct_fields.?;
                                            // Find visibility for this field by name
                                            var is_public_field = false; // private by default unless marked public
                                            for (owner_fields) |ofld| {
                                                if (std.mem.eql(u8, ofld.name, struct_field.name)) {
                                                    is_public_field = ofld.is_public;
                                                    break;
                                                }
                                            }
                                            if (!is_public_field) {
                                                // Basic rule: only allow private access via 'this' receiver in methods of the same struct
                                                const accessed_via_this = (field.object.data == .This);
                                                if (!accessed_via_this) {
                                                    self.reporter.reportCompileError(
                                                        getLocationFromBase(expr.base),
                                                        ErrorCode.PRIVATE_FIELD_ACCESS,
                                                        "Cannot access private field '{s}' of struct '{s}'",
                                                        .{ struct_field.name, owner_name },
                                                    );
                                                    self.fatal_error = true;
                                                    type_info.base = .Nothing;
                                                    return type_info;
                                                }
                                            }
                                        }
                                    }
                                }
                                return type_info;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.FIELD_NOT_FOUND,
                                "Field '{s}' not found in struct{s}",
                                .{ field.field.lexeme, if (resolved_object_type.custom_type) |name| std.fmt.allocPrint(self.allocator, " '{s}'", .{name}) catch "" else "" },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.STRUCT_HAS_NO_FIELDS,
                            "Struct{s} has no fields defined",
                            .{if (resolved_object_type.custom_type) |name| std.fmt.allocPrint(self.allocator, " '{s}'", .{name}) catch "" else ""},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                } else if (resolved_object_type.base == .Enum) {
                    // Allow enum variant access: Color.Red
                    // You may want to check if the variant exists, but for now just return Enum type
                    type_info.* = .{ .base = .Enum };
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                        "Cannot access field on non-struct type {s}",
                        .{@tagName(resolved_object_type.base)},
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
                        try self.unifyTypes(first_type, element_type, .{ .location = getLocationFromBase(expr.base) });
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
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_CONDITION_TYPE,
                        "Condition must be tetra, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                const then_type = try self.inferTypeFromExpr(if_expr.then_branch.?);
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.inferTypeFromExpr(else_branch);

                    // If the parser produced an implicit else that is a Nothing literal,
                    // treat this as if there is no else-branch to avoid forcing unification
                    // of a concrete then-type with Nothing in statement contexts.
                    const else_is_implicit_nothing = (else_branch.data == .Literal and else_type.base == .Nothing);
                    if (else_is_implicit_nothing) {
                        type_info.* = then_type.*;
                        return type_info;
                    }

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
                        // If one branch is effectively "no value" (Nothing) and the other produces a value,
                        // prefer the non-Nothing type without forcing unification. This covers cases where one
                        // branch contains control-flow like return/print and the other is a statement.
                        if (then_type.base == .Nothing and else_type.base != .Nothing) {
                            type_info.* = else_type.*;
                        } else if (else_type.base == .Nothing and then_type.base != .Nothing) {
                            type_info.* = then_type.*;
                        } else if (then_type.base != else_type.base) {
                            // As a fallback when branches differ, allow a union of both branch types
                            var members = [_]*ast.TypeInfo{ then_type, else_type };
                            const u = try self.createUnionType(members[0..]);
                            type_info.* = u.*;
                        } else {
                            // Types match - propagate
                            type_info.* = then_type.*;
                        }
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
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Logical operators require tetra operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra };
            },
            // (Loop case handled earlier in this switch)
            .Match => |match_expr| {
                _ = try self.inferTypeFromExpr(match_expr.value);

                // Analyze all cases and create a union type if they have different types
                if (match_expr.cases.len > 0) {
                    // If the match value is a simple variable, we can narrow it inside cases
                    var matched_var_name: ?[]const u8 = null;
                    if (match_expr.value.data == .Variable) {
                        matched_var_name = match_expr.value.data.Variable.lexeme;
                    }

                    // For match expressions used as statements, analyze case bodies without type narrowing
                    // This allows assignments in match cases to work properly
                    var union_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
                    defer union_types.deinit();

                    // Infer each case body type
                    for (match_expr.cases) |case| {
                        const case_type = try self.inferTypeFromExpr(case.body);
                        try union_types.append(case_type);
                    }

                    // If all cases return the same type, use that type
                    if (union_types.items.len > 0) {
                        var all_same_type = true;
                        const first_type = union_types.items[0];

                        for (union_types.items[1..]) |case_type| {
                            if (case_type.base != first_type.base) {
                                all_same_type = false;
                                break;
                            }
                        }

                        if (all_same_type) {
                            type_info.* = first_type.*;
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
                                    getLocationFromBase(expr.base),
                                    ErrorCode.INVALID_ASSIGNMENT_TARGET,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(storage.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.VARIABLE_NOT_FOUND,
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
                                    getLocationFromBase(expr.base),
                                    ErrorCode.INVALID_ASSIGNMENT_TARGET,
                                    "Cannot assign to immutable variable '{s}'",
                                    .{compound_assign.name.lexeme},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            // Check type compatibility
                            try self.unifyTypes(storage.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.VARIABLE_NOT_FOUND,
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
            .BuiltinCall => |bc| {
                const fname = bc.function.lexeme;

                // Default result on error
                type_info.* = .{ .base = .Nothing };

                // Helper to assert arity
                const requireArity = struct {
                    fn check(sem: *SemanticAnalyzer, e: *ast.Expr, got: usize, expect: usize, name: []const u8) void {
                        if (got != expect) {
                            sem.reporter.reportCompileError(
                                getLocationFromBase(e.base),
                                ErrorCode.INVALID_ARGUMENT_COUNT,
                                "@{s} requires exactly {d} argument(s)",
                                .{ name, expect },
                            );
                            sem.fatal_error = true;
                        }
                    }
                };

                if (std.mem.eql(u8, fname, "length")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    const t0 = try self.inferTypeFromExpr(bc.arguments[0]);
                    if (t0.base != .Array and t0.base != .String) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(bc.arguments[0].base),
                            ErrorCode.INVALID_ARRAY_TYPE,
                            "@length requires array or string, got {s}",
                            .{@tagName(t0.base)},
                        );
                        self.fatal_error = true;
                        return type_info;
                    }
                    type_info.* = .{ .base = .Int };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "push")) {
                    requireArity.check(self, expr, bc.arguments.len, 2, fname);
                    if (bc.arguments.len != 2) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    const val_t = try self.inferTypeFromExpr(bc.arguments[1]);
                    if (coll_t.base == .Array) {
                        if (coll_t.array_type) |elem| try self.unifyTypes(elem, val_t, .{ .location = getLocationFromBase(bc.arguments[1].base) });
                    } else if (coll_t.base != .String) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@push requires array or string, got {s}", .{@tagName(coll_t.base)});
                        self.fatal_error = true;
                    }
                    return type_info;
                } else if (std.mem.eql(u8, fname, "pop")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    if (coll_t.base == .Array) {
                        if (coll_t.array_type) |elem| type_info.* = elem.*;
                        return type_info;
                    } else if (coll_t.base == .String) {
                        type_info.* = .{ .base = .String };
                        return type_info;
                    } else {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@pop requires array or string, got {s}", .{@tagName(coll_t.base)});
                        self.fatal_error = true;
                        return type_info;
                    }
                } else if (std.mem.eql(u8, fname, "insert")) {
                    requireArity.check(self, expr, bc.arguments.len, 3, fname);
                    if (bc.arguments.len != 3) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    const idx_t = try self.inferTypeFromExpr(bc.arguments[1]);
                    if (idx_t.base != .Int) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@insert index must be int, got {s}", .{@tagName(idx_t.base)});
                        self.fatal_error = true;
                        return type_info;
                    }
                    if (coll_t.base == .Array) {
                        const val_t = try self.inferTypeFromExpr(bc.arguments[2]);
                        if (coll_t.array_type) |elem| try self.unifyTypes(elem, val_t, .{ .location = getLocationFromBase(bc.arguments[2].base) });
                    } else if (coll_t.base != .String) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@insert requires array or string, got {s}", .{@tagName(coll_t.base)});
                        self.fatal_error = true;
                        return type_info;
                    }
                    return type_info;
                } else if (std.mem.eql(u8, fname, "remove")) {
                    requireArity.check(self, expr, bc.arguments.len, 2, fname);
                    if (bc.arguments.len != 2) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    const idx_t = try self.inferTypeFromExpr(bc.arguments[1]);
                    if (idx_t.base != .Int) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@remove index must be int, got {s}", .{@tagName(idx_t.base)});
                        self.fatal_error = true;
                        return type_info;
                    }
                    if (coll_t.base == .Array) {
                        if (coll_t.array_type) |elem| {
                            type_info.* = elem.*;
                        } else {
                            type_info.* = .{ .base = .Nothing };
                        }
                        return type_info;
                    } else if (coll_t.base == .String) {
                        type_info.* = .{ .base = .Byte };
                        return type_info;
                    }
                    self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@remove requires array or string, got {s}", .{@tagName(coll_t.base)});
                    self.fatal_error = true;
                    return type_info;
                } else if (std.mem.eql(u8, fname, "clear")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    if (coll_t.base != .Array and coll_t.base != .String) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@clear requires array or string, got {s}", .{@tagName(coll_t.base)});
                        self.fatal_error = true;
                    }
                    return type_info;
                } else if (std.mem.eql(u8, fname, "find")) {
                    requireArity.check(self, expr, bc.arguments.len, 2, fname);
                    if (bc.arguments.len != 2) return type_info;
                    type_info.* = .{ .base = .Int };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "slice")) {
                    requireArity.check(self, expr, bc.arguments.len, 3, fname);
                    if (bc.arguments.len != 3) return type_info;
                    const coll_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    const start_t = try self.inferTypeFromExpr(bc.arguments[1]);
                    const len_t = try self.inferTypeFromExpr(bc.arguments[2]);
                    if (start_t.base != .Int or len_t.base != .Int) {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[1].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice start/length must be ints", .{});
                        self.fatal_error = true;
                        return type_info;
                    }
                    if (coll_t.base == .String) {
                        type_info.* = .{ .base = .String };
                    } else if (coll_t.base == .Array) {
                        if (coll_t.array_type) |elem| {
                            const new_elem = try self.allocator.create(ast.TypeInfo);
                            new_elem.* = elem.*;
                            type_info.* = .{ .base = .Array, .array_type = new_elem };
                        } else {
                            type_info.* = .{ .base = .Array };
                        }
                    } else {
                        self.reporter.reportCompileError(getLocationFromBase(bc.arguments[0].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice requires array or string, got {s}", .{@tagName(coll_t.base)});
                        self.fatal_error = true;
                    }
                    return type_info;
                } else if (std.mem.eql(u8, fname, "copy")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    const v_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    type_info.* = v_t.*;
                    return type_info;
                } else if (std.mem.eql(u8, fname, "string")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    type_info.* = .{ .base = .String };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "int")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    type_info.* = .{ .base = .Int };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "float")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    type_info.* = .{ .base = .Float };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "byte")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    const a0_t = try self.inferTypeFromExpr(bc.arguments[0]);
                    if (a0_t.base == .String) {
                        const elem = try self.allocator.create(ast.TypeInfo);
                        elem.* = .{ .base = .Byte };
                        type_info.* = .{ .base = .Array, .array_type = elem };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                    return type_info;
                } else if (std.mem.eql(u8, fname, "type")) {
                    requireArity.check(self, expr, bc.arguments.len, 1, fname);
                    if (bc.arguments.len != 1) return type_info;
                    type_info.* = .{ .base = .String };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "input")) {
                    requireArity.check(self, expr, bc.arguments.len, 0, fname);
                    if (bc.arguments.len != 0) return type_info;
                    type_info.* = .{ .base = .String };
                    return type_info;
                } else if (std.mem.eql(u8, fname, "syscall")) {
                    // Not fully specified; return opaque
                    type_info.* = .{ .base = .Tetra };
                    return type_info;
                }

                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.NOT_IMPLEMENTED, "Unknown builtin '@{s}'", .{fname});
                self.fatal_error = true;
                return type_info;
            },
            .InternalCall => |method_call| {
                var receiver_type = try self.inferTypeFromExpr(method_call.receiver);
                const method_name = method_call.method.lexeme;

                // Validate receiver type based on method
                switch (method_call.method.type) {
                    // Array methods
                    .PUSH, .POP, .INSERT, .REMOVE, .CLEAR, .FIND => {
                        // If inference yielded Nothing but the receiver is a known variable,
                        // fall back to its declared type. This helps for statement-level method calls
                        // where the receiver was initialized to a default (e.g., empty array) and
                        // type info exists in storage.
                        if (receiver_type.base == .Nothing and method_call.receiver.data == .Variable) {
                            const var_name = method_call.receiver.data.Variable.lexeme;
                            if (self.lookupVariable(var_name)) |variable| {
                                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                    receiver_type = storage.type_info;
                                }
                            }
                        }
                        const allow_string_for_method = receiver_type.base == .String and (method_call.method.type == .PUSH or method_call.method.type == .POP or method_call.method.type == .INSERT or method_call.method.type == .REMOVE or method_call.method.type == .CLEAR or method_call.method.type == .FIND);
                        if (receiver_type.base != .Array and !allow_string_for_method) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(method_call.receiver.base),
                                ErrorCode.INVALID_ARRAY_TYPE,
                                "Cannot call array method '{s}' on non-array type {s}",
                                .{ method_name, @tagName(receiver_type.base) },
                            );
                            self.fatal_error = true;
                            type_info.* = .{ .base = .Nothing };
                            return type_info;
                        }

                        // Transform into appropriate array operation
                        switch (method_call.method.type) {
                            .PUSH => {
                                if (method_call.arguments.len != 1) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_ARRAY_TYPE,
                                        "@push requires exactly one argument",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                const value_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (receiver_type.base == .Array) {
                                    // Check element type matches array
                                    if (receiver_type.array_type) |elem_type| {
                                        try self.unifyTypes(elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[0].base) });
                                    }
                                } else if (receiver_type.base == .String) {
                                    // String concatenation: require value to be String
                                    if (value_type.base != .String) {
                                        self.reporter.reportCompileError(
                                            getLocationFromBase(method_call.arguments[0].base),
                                            ErrorCode.TYPE_MISMATCH,
                                            "@push on string requires string value, got {s}",
                                            .{@tagName(value_type.base)},
                                        );
                                        self.fatal_error = true;
                                        type_info.* = .{ .base = .Nothing };
                                        return type_info;
                                    }
                                }

                                // Lower to BuiltinCall: @push(array, element)
                                var args = try self.allocator.alloc(*ast.Expr, 2);
                                args[0] = method_call.receiver;
                                args[1] = method_call.arguments[0];
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            .POP => {
                                if (method_call.arguments.len != 0) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_COUNT,
                                        "@pop requires no arguments",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Lower to BuiltinCall: @pop(array)
                                var args = try self.allocator.alloc(*ast.Expr, 1);
                                args[0] = method_call.receiver;
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            .INSERT => {
                                if (method_call.arguments.len != 2) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_COUNT,
                                        "@insert requires exactly two arguments (index, element)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Check index is integer
                                const index_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (index_type.base != .Int) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                                        "@insert index must be integer, got {s}",
                                        .{@tagName(index_type.base)},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Check element type matches array
                                const value_type = try self.inferTypeFromExpr(method_call.arguments[1]);
                                if (receiver_type.array_type) |elem_type| {
                                    try self.unifyTypes(elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[1].base) });
                                }

                                // Lower to BuiltinCall: @insert(array, index, element)
                                var args = try self.allocator.alloc(*ast.Expr, 3);
                                args[0] = method_call.receiver;
                                args[1] = method_call.arguments[0];
                                args[2] = method_call.arguments[1];
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            .REMOVE => {
                                if (method_call.arguments.len != 1) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_COUNT,
                                        "@remove requires exactly one argument (index)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Check index is integer
                                const index_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (index_type.base != .Int) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                                        "@remove index must be integer, got {s}",
                                        .{@tagName(index_type.base)},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Lower to BuiltinCall: @remove(array, index)
                                var args = try self.allocator.alloc(*ast.Expr, 2);
                                args[0] = method_call.receiver;
                                args[1] = method_call.arguments[0];
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            .CLEAR => {
                                if (method_call.arguments.len != 0) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_COUNT,
                                        "@clear requires no arguments",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Lower to BuiltinCall: @clear(array)
                                var args = try self.allocator.alloc(*ast.Expr, 1);
                                args[0] = method_call.receiver;
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            .FIND => {
                                if (method_call.arguments.len != 1) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_COUNT,
                                        "@index requires exactly one argument (element)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }

                                // Check element type matches array
                                const value_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (receiver_type.array_type) |elem_type| {
                                    try self.unifyTypes(elem_type, value_type, .{ .location = getLocationFromBase(method_call.arguments[0].base) });
                                }

                                // Lower to BuiltinCall: @find(array, element)
                                var args = try self.allocator.alloc(*ast.Expr, 2);
                                args[0] = method_call.receiver;
                                args[1] = method_call.arguments[0];
                                expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                return try self.inferTypeFromExpr(expr);
                            },
                            else => {
                                // For now, other array methods not implemented
                                self.reporter.reportCompileError(
                                    getLocationFromBase(method_call.receiver.base),
                                    ErrorCode.NOT_IMPLEMENTED,
                                    "Array method '{s}' not yet implemented",
                                    .{method_name},
                                );
                                self.fatal_error = true;
                                type_info.* = .{ .base = .Nothing };
                            },
                        }
                    },

                    // Type method (legacy) — convert to BuiltinCall("type", receiver)
                    .TYPE => {
                        var args = try self.allocator.alloc(*ast.Expr, 1);
                        args[0] = method_call.receiver;
                        expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                        return try self.inferTypeFromExpr(expr);
                    },

                    // Built-in simple methods (legacy) — map to BuiltinCall
                    .LENGTH => {
                        var args = try self.allocator.alloc(*ast.Expr, 1);
                        args[0] = method_call.receiver;
                        expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                        return try self.inferTypeFromExpr(expr);
                    },
                    .BYTES => {
                        var args = try self.allocator.alloc(*ast.Expr, 1);
                        args[0] = method_call.receiver;
                        expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                        return try self.inferTypeFromExpr(expr);
                    },

                    // String methods
                    .TOSTRING, .TOINT, .TOFLOAT, .TOBYTE => {
                        switch (method_call.method.type) {
                            .TOSTRING => {
                                // Any type can be converted to string
                                type_info.* = .{ .base = .String };
                            },
                            .TOINT, .TOFLOAT, .TOBYTE => {
                                // Check receiver is string
                                if (receiver_type.base != .String) {
                                    // Non-string receivers are allowed for numeric conversions; set return type directly
                                    switch (method_call.method.type) {
                                        .TOINT => type_info.* = .{ .base = .Int },
                                        .TOFLOAT => type_info.* = .{ .base = .Float },
                                        .TOBYTE => type_info.* = .{ .base = .Byte },
                                        else => unreachable,
                                    }
                                    return type_info;
                                }

                                // For string receivers, handle parse semantics and unions where applicable
                                // For now, do not fold literals; keep as builtin for uniformity
                                if (method_call.method.type == .TOINT and method_call.receiver.data == .Literal) {
                                    var args = try self.allocator.alloc(*ast.Expr, 1);
                                    args[0] = method_call.receiver;
                                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                    const int_t = try self.allocator.create(ast.TypeInfo);
                                    int_t.* = .{ .base = .Int };
                                    const err_t = try self.allocator.create(ast.TypeInfo);
                                    err_t.* = .{ .base = .Custom, .custom_type = "NumberError" };
                                    var union_members = [_]*ast.TypeInfo{ int_t, err_t };
                                    const u = try self.createUnionType(union_members[0..]);
                                    type_info.* = u.*;
                                    return type_info;
                                }

                                // Lower to builtin for runtime string->int conversion when receiver is not literal
                                if (method_call.method.type == .TOINT) {
                                    // Return type: int | NumberError
                                    const int_t = try self.allocator.create(ast.TypeInfo);
                                    int_t.* = .{ .base = .Int };
                                    const err_t = try self.allocator.create(ast.TypeInfo);
                                    err_t.* = .{ .base = .Custom, .custom_type = "NumberError" };
                                    var union_members = [_]*ast.TypeInfo{ int_t, err_t };
                                    const u = try self.createUnionType(union_members[0..]);
                                    var args = try self.allocator.alloc(*ast.Expr, 1);
                                    args[0] = method_call.receiver;
                                    expr.data = .{ .BuiltinCall = .{ .function = method_call.method, .arguments = args } };
                                    type_info.* = u.*;
                                    return type_info;
                                }
                                if (method_call.method.type == .TOFLOAT) {
                                    // Return type: float (no typed error union in current VM path)
                                    type_info.* = .{ .base = .Float };
                                    return type_info;
                                }
                                if (method_call.method.type == .TOBYTE) {
                                    // If receiver is string, @byte(string) -> byte[]
                                    if (receiver_type.base == .String) {
                                        const elem = try self.allocator.create(ast.TypeInfo);
                                        elem.* = .{ .base = .Byte };
                                        type_info.* = .{ .base = .Array, .array_type = elem };
                                    } else {
                                        // numeric -> byte
                                        type_info.* = .{ .base = .Byte };
                                    }
                                    return type_info;
                                }
                            },
                            else => unreachable,
                        }
                    },

                    // I/O methods
                    .READ, .WRITE, .SYSCALL => {
                        switch (method_call.method.type) {
                            .READ => {
                                // Check path argument is string
                                if (method_call.arguments.len != 1) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_TYPE,
                                        "@read requires exactly one string argument (path)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                const path_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (path_type.base != .String) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_FILE_PATH_TYPE,
                                        "@read path must be string, got {s}",
                                        .{@tagName(path_type.base)},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                type_info.* = .{ .base = .String };
                            },
                            .WRITE => {
                                // Check path and content arguments are strings
                                if (method_call.arguments.len != 2) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_TYPE,
                                        "@write requires two arguments (path, content)",
                                        .{},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                const path_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                const content_type = try self.inferTypeFromExpr(method_call.arguments[1]);
                                if (path_type.base != .String or content_type.base != .String) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_FILE_PATH_TYPE,
                                        "@write arguments must be strings, got {s} and {s}",
                                        .{ @tagName(path_type.base), @tagName(content_type.base) },
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                type_info.* = .{ .base = .Tetra }; // Returns success/failure
                            },
                            .SYSCALL => {
                                // Check command argument is string
                                if (method_call.arguments.len != 1) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.receiver.base),
                                        ErrorCode.INVALID_ARGUMENT_TYPE,
                                        "@{s} requires exactly one string argument (command)",
                                        .{method_name},
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                const cmd_type = try self.inferTypeFromExpr(method_call.arguments[0]);
                                if (cmd_type.base != .String) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(method_call.arguments[0].base),
                                        ErrorCode.INVALID_COMMAND_TYPE,
                                        "@{s} command must be string, got {s}",
                                        .{ method_name, @tagName(cmd_type.base) },
                                    );
                                    self.fatal_error = true;
                                    type_info.* = .{ .base = .Nothing };
                                    return type_info;
                                }
                                type_info.* = .{ .base = if (method_call.method.type == .SYSCALL) .String else .Tetra };
                            },
                            else => unreachable,
                        }
                    },

                    // Copy/clone methods
                    .SHALLOW => {
                        // Check receiver is array, string or map
                        if (receiver_type.base != .Array and receiver_type.base != .String and receiver_type.base != .Map) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(method_call.receiver.base),
                                ErrorCode.INVALID_ARRAY_TYPE,
                                "Cannot {s} type {s} - only arrays, strings and maps can be {s}d",
                                .{ method_name, @tagName(receiver_type.base), method_name },
                            );
                            self.fatal_error = true;
                            type_info.* = .{ .base = .Nothing };
                            return type_info;
                        }
                        type_info.* = receiver_type.*; // Returns same type as input
                    },

                    // Other methods
                    else => {
                        self.reporter.reportCompileError(
                            getLocationFromBase(method_call.receiver.base),
                            ErrorCode.NOT_IMPLEMENTED,
                            "Unknown method '{s}'",
                            .{method_name},
                        );
                        self.fatal_error = true;
                        type_info.* = .{ .base = .Nothing };
                    },
                }
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
            .Print => {},
            .IndexAssign => |index_assign| {
                const array_type = try self.inferTypeFromExpr(index_assign.array);
                const index_type = try self.inferTypeFromExpr(index_assign.index);
                const value_type = try self.inferTypeFromExpr(index_assign.value);

                if (array_type.base != .Array and array_type.base != .Map) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_INDEX_TYPE,
                        "Cannot assign to index of non-array/non-map type {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // For arrays, index must be Int; for maps, index can be Int or String
                if (array_type.base == .Array and index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                        "Array index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                } else if (array_type.base == .Map and index_type.base != .Int and index_type.base != .String and index_type.base != .Enum) {
                    if (index_type.base == .Union) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                            .{},
                        );
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map key must be integer, string, or enum, got {s}",
                            .{@tagName(index_type.base)},
                        );
                    }
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    try self.unifyTypes(elem_type, value_type, .{ .location = getLocationFromBase(expr.base) });
                }

                type_info.* = .{ .base = .Nothing }; // IndexAssign has no return value
            },
            .FieldAssignment => |field_assign| {
                const object_type = try self.inferTypeFromExpr(field_assign.object);
                const value_type = try self.inferTypeFromExpr(field_assign.value);

                if (object_type.base != .Struct and object_type.base != .Custom) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
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
                            try self.unifyTypes(struct_field.type_info, value_type, .{ .location = getLocationFromBase(expr.base) });
                            break;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.FIELD_NOT_FOUND,
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
                                    getLocationFromBase(expr.base),
                                    ErrorCode.FIELD_NOT_FOUND,
                                    "Field '{s}' not found in struct '{s}'",
                                    .{ field_assign.field.lexeme, custom_type_name },
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                        } else {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                                "Cannot assign to field of non-struct type {s}",
                                .{@tagName(object_type.base)},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.FIELD_NOT_FOUND,
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
                            getLocationFromBase(expr.base),
                            ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
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
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ARRAY_TYPE,
                        "Exists requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_CONDITION_TYPE,
                        "Exists condition must be tetra, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra };
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
                            getLocationFromBase(expr.base),
                            ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
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
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ARRAY_TYPE,
                        "ForAll requires array type, got {s}",
                        .{@tagName(array_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_CONDITION_TYPE,
                        "ForAll condition must be tetra, got {s}",
                        .{@tagName(condition_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .Tetra };
            },
            .Assert => |assert| {
                const condition_type = try self.inferTypeFromExpr(assert.condition);
                if (condition_type.base != .Tetra) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_CONDITION_TYPE,
                        "Assert condition must be tetra, got {s}",
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

                            // Validate struct literal fields against declared struct fields
                            if (type_info.struct_fields) |decl_fields| {
                                // 1) Check field count matches
                                if (decl_fields.len != struct_lit.fields.len) {
                                    self.reporter.reportCompileError(
                                        getLocationFromBase(expr.base),
                                        ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                                        "Struct field count mismatch: expected {}, got {}",
                                        .{ decl_fields.len, struct_lit.fields.len },
                                    );
                                    self.fatal_error = true;
                                }

                                // 2) For each provided field, ensure it exists and types unify
                                for (struct_lit.fields) |lit_field| {
                                    var found = false;
                                    for (decl_fields) |decl_field| {
                                        if (std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) {
                                            found = true;
                                            const lit_type = try self.inferTypeFromExpr(lit_field.value);
                                            try self.unifyTypes(
                                                decl_field.type_info,
                                                lit_type,
                                                ast.SourceSpan.fromToken(lit_field.name),
                                            );
                                            break;
                                        }
                                    }
                                    if (!found) {
                                        self.reporter.reportCompileError(
                                            getLocationFromBase(lit_field.value.base),
                                            ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                                            "Field '{s}' not found in struct '{s}'",
                                            .{ lit_field.name.lexeme, struct_lit.name.lexeme },
                                        );
                                        self.fatal_error = true;
                                    }
                                }
                            }
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
            .Block => |block| {
                // Ensure statements inside a block expression are validated so that
                // statement-level transformations (e.g., @push lowering) occur within blocks
                const prev_scope = self.current_scope;
                const block_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                self.current_scope = block_scope;
                defer {
                    self.current_scope = prev_scope;
                    block_scope.deinit();
                }

                try self.validateStatements(block.statements);

                // Always analyze the block's final value expression (if any) so that
                // expression-level transformations (like lowering compiler methods) occur
                // even inside loop scopes. When in a loop, we still report the type as Nothing.
                var value_type_ptr: ?*ast.TypeInfo = null;
                if (block.value) |value_expr| {
                    value_type_ptr = try self.inferTypeFromExpr(value_expr);
                }

                if (self.in_loop_scope) {
                    // If we are in a loop scope, the block usually has no value in terms of control flow.
                    // However, if the block contains an expression that returns a value (e.g., an error),
                    // we should propagate that type.
                    if (value_type_ptr) |vt| {
                        type_info.* = vt.*;
                    } else {
                        type_info.* = .{ .base = .Nothing };
                    }
                } else if (value_type_ptr) |vt| {
                    type_info.* = vt.*;
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
                // Enforce: 'as' requires an else; then is optional
                if (cast.else_branch == null) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.EXPECTED_CLOSING_BRACE,
                        "'as' requires an 'else' block",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                // For cast expressions (value as Type), the result type is the target type
                // Parse the target type from the cast expression
                const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
                type_info.* = target_type_info.*;

                // Also validate that the value being cast is a union type
                const value_type = try self.inferTypeFromExpr(cast.value);
                if (value_type.base != .Union) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Type casting 'as' can only be used with union types, got {s}",
                        .{@tagName(value_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                // Handle the then branch in a new scope with narrowed type
                var then_type: ?*ast.TypeInfo = null;
                if (cast.then_branch) |then_expr| {
                    // Create a scope for the then branch where we know the type
                    const then_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                    defer then_scope.deinit();

                    const prev_scope = self.current_scope;
                    self.current_scope = then_scope;
                    defer self.current_scope = prev_scope;

                    // If the cast value is a variable, shadow it with the narrowed type
                    if (cast.value.data == .Variable) {
                        const var_name = cast.value.data.Variable.lexeme;
                        const token_type = self.convertTypeToTokenType(target_type_info.base);
                        _ = then_scope.createValueBinding(
                            var_name,
                            TokenLiteral{ .nothing = {} },
                            token_type,
                            target_type_info,
                            false,
                        ) catch {};
                    } else if (cast.value.data == .FieldAccess) {
                        const field_access = cast.value.data.FieldAccess;
                        // Only support narrowing when object is a simple variable (e.g., token.value)
                        if (field_access.object.data == .Variable) {
                            const obj_name = field_access.object.data.Variable.lexeme;
                            const fld_name = field_access.field.lexeme;
                            // Lookup original variable type
                            if (self.lookupVariable(obj_name)) |variable| {
                                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                                    const original_type = storage.type_info.*;
                                    // Build a struct type view of the object
                                    var struct_fields: ?[]ast.StructFieldType = null;
                                    if (original_type.base == .Struct and original_type.struct_fields != null) {
                                        struct_fields = original_type.struct_fields.?;
                                    } else if (original_type.base == .Custom and original_type.custom_type != null) {
                                        // Expand custom struct fields from registry
                                        if (self.custom_types.get(original_type.custom_type.?)) |custom_type| {
                                            if (custom_type.kind == .Struct and custom_type.struct_fields != null) {
                                                const ct_fields = custom_type.struct_fields.?;
                                                const new_sf = try self.allocator.alloc(ast.StructFieldType, ct_fields.len);
                                                for (ct_fields, 0..) |ct_field, i| {
                                                    new_sf[i] = .{ .name = ct_field.name, .type_info = ct_field.field_type_info };
                                                }
                                                struct_fields = new_sf;
                                            }
                                        }
                                    }
                                    if (struct_fields) |fields_arr| {
                                        // Duplicate and narrow the specific field
                                        const dup_fields = try self.allocator.alloc(ast.StructFieldType, fields_arr.len);
                                        for (fields_arr, 0..) |sf, i| {
                                            if (std.mem.eql(u8, sf.name, fld_name)) {
                                                dup_fields[i] = .{ .name = sf.name, .type_info = target_type_info };
                                            } else {
                                                dup_fields[i] = sf;
                                            }
                                        }
                                        const narrowed_struct = try self.allocator.create(ast.TypeInfo);
                                        narrowed_struct.* = .{ .base = .Struct, .struct_fields = dup_fields };
                                        _ = then_scope.createValueBinding(
                                            obj_name,
                                            TokenLiteral{ .nothing = {} },
                                            .STRUCT,
                                            narrowed_struct,
                                            false,
                                        ) catch {};
                                    }
                                }
                            }
                        }
                    }
                    // Analyze the then branch with the narrowed type
                    then_type = try self.inferTypeFromExpr(then_expr);
                }

                // Handle the else branch (no type narrowing)
                var else_type: ?*ast.TypeInfo = null;
                if (cast.else_branch) |else_expr| {
                    else_type = try self.inferTypeFromExpr(else_expr);
                    // Treat control-flow exits (e.g., return ...) as Nothing for type inference
                    if (else_expr.data == .ReturnExpr) {
                        const nothing_type = try self.allocator.create(ast.TypeInfo);
                        nothing_type.* = .{ .base = .Nothing };
                        else_type = nothing_type;
                    }
                }

                // For an as expression with then/else, first set the type to the target type
                // This ensures proper type inference in the branches
                type_info.* = target_type_info.*;

                // For an as expression with then/else, the type depends on the branch types
                if (then_type) |tt| {
                    if (else_type) |et| {
                        // Both then and else exist: result can be either branch
                        if (tt.base == et.base) {
                            type_info.* = tt.*;
                        } else {
                            var types = [_]*ast.TypeInfo{ tt, et };
                            const union_type = try self.createUnionType(&types);
                            type_info.* = union_type.*;
                        }
                    } else {
                        // Only then branch exists: result type is the then-type
                        type_info.* = tt.*;
                    }
                } else if (else_type) |et| {
                    // Else-only: if else yields Nothing (control-flow exit), result is target type
                    if (et.base == .Nothing) {
                        type_info.* = target_type_info.*;
                    } else if (target_type_info.base == et.base) {
                        type_info.* = target_type_info.*;
                    } else {
                        var types = [_]*ast.TypeInfo{ target_type_info, et };
                        const union_type = try self.createUnionType(&types);
                        type_info.* = union_type.*;
                    }
                } else {
                    // No branches: type remains the target type
                    type_info.* = target_type_info.*;
                }
            },
            .Loop => |loop| {
                // Create outer loop scope (for loop-level variables)
                const outer_loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                const prev_scope = self.current_scope;
                self.current_scope = outer_loop_scope;
                defer {
                    self.current_scope = prev_scope;
                    outer_loop_scope.deinit();
                }

                const prev_in_loop_scope = self.in_loop_scope;
                self.in_loop_scope = true;
                defer self.in_loop_scope = prev_in_loop_scope;

                // Handle optional initializer statement
                if (loop.var_decl) |vd| {
                    var single: [1]ast.Stmt = .{vd.*};
                    try self.validateStatements(single[0..]);
                }

                // Analyze optional condition
                if (loop.condition) |cond| {
                    _ = try self.inferTypeFromExpr(cond);
                }

                // Create a new scope for EACH ITERATION
                const iteration_scope = try self.memory.scope_manager.createScope(outer_loop_scope, self.memory);
                defer iteration_scope.deinit();

                // Rebind loop variable for each iteration
                if (loop.var_decl) |vd| {
                    // Save current scope
                    const saved_scope = self.current_scope;
                    self.current_scope = iteration_scope;

                    // Re-analyze variable declaration in iteration scope
                    var single: [1]ast.Stmt = .{vd.*};
                    try self.validateStatements(single[0..]);

                    // Restore scope
                    self.current_scope = saved_scope;
                }

                // Analyze loop body in iteration scope
                {
                    const saved_scope = self.current_scope;
                    self.current_scope = iteration_scope;
                    defer self.current_scope = saved_scope;

                    _ = try self.inferTypeFromExpr(loop.body);
                }

                // Analyze optional step
                if (loop.step) |stp| {
                    _ = try self.inferTypeFromExpr(stp);
                }

                // Loops do not yield a value
                type_info.* = .{ .base = .Nothing };
            },
            .This => {
                // 'this' refers to the current struct instance in method context
                // For now, return a generic struct type - this will be resolved during code generation
                // TODO: Implement proper 'this' type resolution based on the current method context
                type_info.* = .{ .base = .Struct };
            },
            // No ForEach expression variant; 'each' is desugared to a Loop statement
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
                .INT_TYPE, .INT => .{ .base = .Int, .is_mutable = false },
                .FLOAT_TYPE, .FLOAT => .{ .base = .Float, .is_mutable = false },
                .STRING_TYPE, .STRING => .{ .base = .String, .is_mutable = false },
                .BYTE_TYPE, .BYTE => .{ .base = .Byte, .is_mutable = false },
                .TETRA_TYPE, .TETRA => .{ .base = .Tetra, .is_mutable = false },
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

    // Helper function to create a Variable for an imported symbol
    fn createImportedSymbolVariable(self: *SemanticAnalyzer, name: []const u8, imported_symbol: import_parser.ImportedSymbol) ?*Variable {
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
        self.reporter.reportCompileError(span.location, fmt, args);
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
                if (self.current_initializing_var) |current_var| {
                    if (std.mem.eql(u8, var_token.lexeme, current_var)) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.SELF_REFERENTIAL_INITIALIZER,
                            "Variable '{s}' cannot reference itself in its own initializer",
                            .{current_var},
                        );
                        self.fatal_error = true;
                        return TokenLiteral{ .nothing = {} };
                    }
                }

                // Original variable lookup logic
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        return storage.value;
                    }
                }

                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNDEFINED_VARIABLE,
                    "Undefined variable: '{s}'",
                    .{var_token.lexeme},
                );
                self.fatal_error = true;
                return TokenLiteral{ .nothing = {} };
            },
            .Cast => |cast| {
                // Evaluate the value first
                const value_literal = try self.evaluateExpression(cast.value);

                // Determine target type
                const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
                defer self.allocator.destroy(target_type_info);

                // Helper: check if the evaluated value matches the target type
                const matches_target: bool = switch (target_type_info.base) {
                    .Int => switch (value_literal) {
                        .int => true,
                        else => false,
                    },
                    .Float => switch (value_literal) {
                        .float => true,
                        else => false,
                    },
                    .Byte => switch (value_literal) {
                        .byte => true,
                        else => false,
                    },
                    .String => switch (value_literal) {
                        .string => true,
                        else => false,
                    },
                    .Tetra => switch (value_literal) {
                        .tetra => true,
                        else => false,
                    },
                    .Nothing => switch (value_literal) {
                        .nothing => true,
                        else => false,
                    },
                    else => false,
                };

                // If we have branches, pick one at evaluation time
                if (cast.then_branch != null or cast.else_branch != null) {
                    if (matches_target) {
                        if (cast.then_branch) |then_expr| {
                            return try self.evaluateExpression(then_expr);
                        }
                        // No then branch; fall through
                    } else {
                        if (cast.else_branch) |else_expr| {
                            return try self.evaluateExpression(else_expr);
                        }
                        // No else branch; fall through
                    }
                    // If the relevant branch is missing, return nothing
                    return TokenLiteral{ .nothing = {} };
                }

                // No branches: return original value (narrowing only affects type, not value)
                return value_literal;
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
                    .int => |r| blk: {
                        // If both operands are integers and the exponent is non-negative, return an integer
                        if (r >= 0) {
                            break :blk TokenLiteral{ .int = std.math.pow(i64, l, @as(i64, @intCast(r))) };
                        }
                        // Fall back to float for negative exponents
                        break :blk TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) };
                    },
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
            .Custom => "custom",
            .Nothing => "nothing",
            else => blk: {
                // Fallback: try evaluating the expression and derive type from its runtime value
                const val = try self.evaluateExpression(expr);
                break :blk switch (val) {
                    .int => "int",
                    .float => "float",
                    .byte => "byte",
                    .string => "string",
                    .tetra => "tetra",
                    .nothing => "nothing",
                    else => "unknown",
                };
            },
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
                        func_span.location,
                        ErrorCode.DUPLICATE_VARIABLE,
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

        for (body) |stmt| {
            switch (stmt.data) {
                .Return => |return_stmt| {
                    has_return = true;
                    if (return_stmt.value) |value| {
                        has_return_with_value = true;
                        const return_type = try self.inferTypeFromExpr(value);
                        try self.validateReturnTypeCompatibility(&expected_return_type, return_type, .{ .location = getLocationFromBase(stmt.base) });
                    } else {
                        has_return_without_value = true;
                        // Allow bare 'return' for Nothing type or Union types containing Nothing
                        const is_nothing_compatible = expected_return_type.base == .Nothing or
                            (expected_return_type.base == .Union and self.unionContainsNothing(expected_return_type));
                        if (!is_nothing_compatible) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(stmt.base),
                                ErrorCode.MISSING_RETURN_VALUE,
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
                    }
                },
                .Expression => |expr| {
                    if (expr) |expression| {

                        // Check for return expressions embedded inside this expression (e.g., blocks or nested ifs)
                        var expr_return_types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
                        defer expr_return_types.deinit();
                        try self.collectReturnTypesFromExpr(expression, &expr_return_types);
                        if (expr_return_types.items.len > 0) {
                            has_return = true;
                            for (expr_return_types.items) |ret_type| {
                                if (ret_type.base == .Nothing) {
                                    has_return_without_value = true;
                                    // Allow bare 'return' for Nothing type or Union types containing Nothing
                                    const is_nothing_compatible = expected_return_type.base == .Nothing or
                                        (expected_return_type.base == .Union and self.unionContainsNothing(expected_return_type));
                                    if (!is_nothing_compatible) {
                                        self.reporter.reportCompileError(
                                            getLocationFromBase(stmt.base),
                                            ErrorCode.MISSING_RETURN_VALUE,
                                            "Function expects return value of type {s}, but return statement has no value",
                                            .{@tagName(expected_return_type.base)},
                                        );
                                        self.fatal_error = true;
                                    }
                                } else {
                                    has_return_with_value = true;
                                    try self.validateReturnTypeCompatibility(&expected_return_type, ret_type, .{ .location = getLocationFromBase(stmt.base) });
                                }
                            }
                        } else if (expression.data == .If) {
                            // Check if this is an if expression that might return
                            const if_returns = try self.validateIfExpressionReturns(expression, expected_return_type, func_span);
                            if (if_returns) {
                                has_return = true;
                            }
                        }
                    }
                },
                else => {},
            }
        }

        // Determine the final expression only if the last statement is an expression.
        // Do NOT scan past a trailing non-expression like 'return;'.
        var last_expr: ?*ast.Expr = null;
        if (body.len > 0) {
            const last_stmt = body[body.len - 1];
            switch (last_stmt.data) {
                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        last_expr = expr;
                    }
                },
                else => {
                    last_expr = null;
                },
            }
        }

        // Error cases:
        // 1. Function expects a return value but none found
        // Relax: if there is any return (even nested), don't force an implicit-final-expression check here
        if (expected_return_type.base != .Nothing and !has_return_with_value and !has_return) {
            if (last_expr) |expr| {
                const return_type = try self.inferTypeFromExpr(expr);
                try self.validateReturnTypeCompatibility(&expected_return_type, return_type, func_span);
                // Treat as having a return with value via implicit final expression
                has_return_with_value = true;
            } else {
                // Temporarily relax: don't error here; allow later phases to validate
                // This avoids false positives when returns are present but not surfaced as statements
            }
        }
        // 2. Function has no explicit return type but has a value-producing final expression
        // Only check this if there are explicit return statements that would conflict
        else if (expected_return_type.base == .Nothing and last_expr != null and has_return_with_value) {
            const expr_type = try self.inferTypeFromExpr(last_expr.?);
            if (expr_type.base != .Nothing) {
                self.reporter.reportCompileError(
                    func_span.location,
                    ErrorCode.MISSING_RETURN_TYPE,
                    "Function has no return type specified but final expression produces value of type {s}",
                    .{@tagName(expr_type.base)},
                );
                self.fatal_error = true;
            }
        }

        if (expected_return_type.base == .Nothing and has_return_with_value) {
            self.reporter.reportCompileError(
                func_span.location,
                ErrorCode.MISSING_RETURN_VALUE,
                "Function expects no return value, but return statement has value",
                .{},
            );
            self.fatal_error = true;
        }

        return has_return;
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
    fn isLoopScope(self: *SemanticAnalyzer) bool {
        return self.in_loop_scope;
    }

    // NEW: Validate if an expression returns a value
    fn validateExpressionReturns(self: *SemanticAnalyzer, expr: *ast.Expr, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!bool {
        switch (expr.data) {
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const return_type = try self.inferTypeFromExpr(value);
                    try self.validateReturnTypeCompatibility(&expected_return_type, return_type, .{ .location = getLocationFromBase(expr.base) });
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
                import_span.location,
                ErrorCode.MODULE_NOT_FOUND,
                "Module file not found: '{s}'",
                .{import_info.module_path},
            );
            self.fatal_error = true;
            return;
        }

        // Check for circular imports
        if (self.isCircularImport(import_info.module_path)) {
            self.reporter.reportCompileError(
                import_span.location,
                ErrorCode.CIRCULAR_IMPORT,
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
                    import_span.location,
                    ErrorCode.SYMBOL_NOT_FOUND_IN_IMPORT_MODULE,
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
                        // Only collect return types from explicit return expressions
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
                        // Don't treat other expressions as return values
                        // Only explicit return statements should contribute to return type inference
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
            // No ForEach expression variant; 'each' is desugared to a Loop statement
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
                                // Allow enum literal (Enum) to match a Custom enum member in the expected union
                                if (expected_member.base == .Custom and actual_member.base == .Enum) {
                                    if (expected_member.custom_type) |custom_name| {
                                        if (self.custom_types.get(custom_name)) |ct| {
                                            if (ct.kind == .Enum) {
                                                found_match = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                            if (!found_match) {
                                self.reporter.reportCompileError(
                                    span.location,
                                    ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
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
                        // Allow enum literal (Enum) to match a Custom enum member in the expected union
                        if (member_type.base == .Custom and actual.base == .Enum) {
                            if (member_type.custom_type) |custom_name| {
                                if (self.custom_types.get(custom_name)) |ct| {
                                    if (ct.kind == .Enum) {
                                        found_match = true;
                                        break;
                                    }
                                }
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
                            span.location,
                            ErrorCode.INVALID_RETURN_TYPE_FOR_UNION,
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

// Expose StructMethodInfo at module scope for codegen imports
pub const StructMethodInfo = SemanticAnalyzer.StructMethodInfo;
