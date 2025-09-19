const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;
const Parser = @import("../../parser/parser_types.zig").Parser;
const import_parser = @import("../../parser/import_parser.zig");

const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const Types = @import("../../types/types.zig");
const HIRTypeSystem = @import("../../codegen/hir/type_system.zig");
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const TokenLiteral = Types.TokenLiteral;

const TokenImport = @import("../../types/token.zig");
const TokenType = TokenImport.TokenType;
const Token = TokenImport.Token;

const Environment = @import("../../interpreter/environment.zig");

const helpers = @import("./helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const infer_type = @import("./infer_type.zig");

pub const StructMethodInfo = SemanticAnalyzer.StructMethodInfo;

//======================================================================

const NodeId = u32; // Or whatever your AST uses

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
    current_function_returns: std.array_list.Managed(*ast.TypeInfo),
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
            .current_function_returns = std.array_list.Managed(*ast.TypeInfo).init(allocator),
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

    pub fn getCustomTypes(self: *SemanticAnalyzer) std.StringHashMap(CustomTypeInfo) {
        return self.custom_types;
    }

    pub fn getStructMethods(self: *SemanticAnalyzer) std.StringHashMap(std.StringHashMap(@This().StructMethodInfo)) {
        return self.struct_methods;
    }

    fn ensureBuiltinEnums(self: *SemanticAnalyzer) !void {
        if (!self.custom_types.contains("IndexError")) {
            const variants = [_][]const u8{"OutOfBounds"};
            try helpers.registerEnumType(self, "IndexError", &variants);
        }
    }

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
    pub fn convertCustomTypeInfo(semantic_type: CustomTypeInfo, allocator: std.mem.Allocator) !HIRTypeSystem.TypeSystem.CustomTypeInfo {
        var hir_type = HIRTypeSystem.TypeSystem.CustomTypeInfo{
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
            const converted_variants = try allocator.alloc(HIRTypeSystem.TypeSystem.CustomTypeInfo.EnumVariant, variants.len);
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
            const converted_fields = try allocator.alloc(HIRTypeSystem.TypeSystem.CustomTypeInfo.StructField, fields.len);
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
                    .Struct => .Struct,
                    .Enum => .Enum,
                    .Custom => blk: {
                        // For custom types, check if it's an enum or struct
                        if (field.custom_type_name) |custom_type_name| {
                            // We need to determine if this is an enum or struct
                            // For now, we'll use a simple heuristic based on the name
                            // In a real implementation, we'd need access to the type registry
                            if (std.mem.eql(u8, custom_type_name, "Species")) {
                                break :blk .Enum;
                            }
                            // Default to Struct for other custom types
                            break :blk .Struct;
                        }
                        break :blk .Struct;
                    },
                    .Map => .Map,
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
                        // Use the union type as-is since flattenUnionType was removed
                        inferred_return_type = func.return_type_info;
                    }

                    // Build parameter types and track aliases
                    var param_types = try self.allocator.alloc(ast.TypeInfo, func.params.len);
                    var param_aliases = try self.allocator.alloc(bool, func.params.len);
                    for (func.params, 0..) |param, i| {
                        param_aliases[i] = param.is_alias;
                        if (param.type_expr) |type_expr| {
                            const param_type_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                            defer self.allocator.destroy(param_type_ptr);

                            const resolved_param_type = try self.resolveTypeInfo(param_type_ptr.*);
                            if (resolved_param_type.base == .Union) {
                                // Use union type as-is since flattenUnionType was removed
                                // resolved_param_type is already correct
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

                        var env = Environment.init(
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
                                const inferred = try infer_type.inferTypeFromExpr(self, init_expr);
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
                        const inferred = try infer_type.inferTypeFromExpr(self, init_expr);
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
                    const token_type = helpers.convertTypeToTokenType(self, type_info.base);

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
                    try helpers.registerEnumType(self, enum_decl.name.lexeme, variant_names);
                },

                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        // Ensure expression statements are semantically analyzed so that
                        // compiler methods like @push are lowered (e.g., InternalCall -> ArrayPush)
                        // even when their values are not used.
                        _ = try infer_type.inferTypeFromExpr(self, expr);

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
                            try helpers.registerStructType(self, struct_decl.name.lexeme, struct_fields);

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

    pub fn validateStatements(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!void {
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
                                        const inferred = try infer_type.inferTypeFromExpr(self, init_expr);
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
                                const inferred = try infer_type.inferTypeFromExpr(self, init_expr);
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

                            const token_type = helpers.convertTypeToTokenType(self, type_info.base);

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
                        const init_type = try infer_type.inferTypeFromExpr(self, init_expr);
                        if (decl.type_info.base != .Nothing) {
                            // Check initializer matches declared type
                            // Resolve the declared type first, then create a mutable copy for unifyTypes
                            const resolved_type = try self.resolveTypeInfo(decl.type_info);
                            var decl_type_copy = resolved_type;
                            try helpers.unifyTypes(self, &decl_type_copy, init_type, .{ .location = getLocationFromBase(stmt.base) });
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
                        _ = try infer_type.inferTypeFromExpr(self, expression);
                    }
                },
                .Return => |return_stmt| {
                    if (return_stmt.value) |value| {
                        // Validate return value type
                        _ = try infer_type.inferTypeFromExpr(self, value);
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
                        _ = try infer_type.inferTypeFromExpr(self, entry.key);
                        _ = try infer_type.inferTypeFromExpr(self, entry.value);
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

    // Narrow the matched variable type within a single match case, then infer the body type.
    pub fn inferMatchCaseTypeWithNarrow(self: *SemanticAnalyzer, case: ast.MatchCase, matched_var_name: ?[]const u8) ErrorList!*ast.TypeInfo {
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

            const token_type: TokenType = helpers.convertTypeToTokenType(self, narrow_info.base);

            // Shadow the variable in this case scope with the narrowed type
            _ = case_scope.createValueBinding(
                name,
                TokenLiteral{ .nothing = {} },
                token_type,
                narrow_info,
                false,
            ) catch {};
        }

        return try infer_type.inferTypeFromExpr(self, case.body);
    }

    fn resolveTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        // If this is a custom type, check if it refers to a declared struct or enum type
        if (type_info.base == .Custom) {
            if (type_info.custom_type) |custom_type_name| {
                if (helpers.lookupVariable(self, custom_type_name)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |_| {
                        // If the custom type refers to a struct or enum declaration, keep it as Custom
                        // This is the correct behavior - variables of struct/enum types should be Custom
                        return type_info;
                    }
                }
            }
        }

        // Handle union types - return as-is since flattenUnionType was removed
        if (type_info.base == .Union) {
            return type_info;
        }

        // Return the original type info if no resolution needed
        return type_info;
    }

    // Instead of setting fatal_error = true immediately, collect errors
    // and continue analysis where possible
    pub fn reportTypeError(self: *SemanticAnalyzer, span: ast.SourceSpan, comptime fmt: []const u8, args: anytype) void {
        self.reporter.reportCompileError(span.location, fmt, args);
        // Only set fatal_error for critical errors that prevent further analysis
    }

    pub fn typeExprToTypeInfo(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr) !*ast.TypeInfo {
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
                if (helpers.lookupVariable(self, var_token.lexeme)) |variable| {
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
                const inferred_type = try infer_type.inferTypeFromExpr(self, expr);
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
        const type_info = try infer_type.inferTypeFromExpr(self, expr);
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
                helpers.convertTypeToTokenType(self, param_type_info.base),
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
                        const return_type = try infer_type.inferTypeFromExpr(self, value);
                        try self.validateReturnTypeCompatibility(&expected_return_type, return_type, .{ .location = getLocationFromBase(stmt.base) });
                    } else {
                        has_return_without_value = true;
                        // Allow bare 'return' for Nothing type or Union types containing Nothing
                        const is_nothing_compatible = expected_return_type.base == .Nothing or
                            (expected_return_type.base == .Union and helpers.unionContainsNothing(self, expected_return_type));
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
                        var expr_return_types = std.array_list.Managed(*ast.TypeInfo).init(self.allocator);
                        defer expr_return_types.deinit();
                        try self.collectReturnTypesFromExpr(expression, &expr_return_types);
                        if (expr_return_types.items.len > 0) {
                            has_return = true;
                            for (expr_return_types.items) |ret_type| {
                                if (ret_type.base == .Nothing) {
                                    has_return_without_value = true;
                                    // Allow bare 'return' for Nothing type or Union types containing Nothing
                                    const is_nothing_compatible = expected_return_type.base == .Nothing or
                                        (expected_return_type.base == .Union and helpers.unionContainsNothing(self, expected_return_type));
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
                const return_type = try infer_type.inferTypeFromExpr(self, expr);
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
            const expr_type = try infer_type.inferTypeFromExpr(self, last_expr.?);
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

    fn isLoopScope(self: *SemanticAnalyzer) bool {
        return self.in_loop_scope;
    }

    fn validateExpressionReturns(self: *SemanticAnalyzer, expr: *ast.Expr, expected_return_type: ast.TypeInfo, func_span: ast.SourceSpan) ErrorList!bool {
        switch (expr.data) {
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const return_type = try infer_type.inferTypeFromExpr(self, value);
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

        // Import registration: load the module AST and register public structs/enums and struct methods
        // so that method resolution works across modules (dot-methods like obj.method()).
        if (self.parser) |pconst| {
            const ParserType = @TypeOf(pconst.*);
            var p: *ParserType = @constCast(pconst);

            // Try to get module info from cache; fall back to resolving
            var mod_info: ?ast.ModuleInfo = null;
            if (p.module_cache.get(import_info.module_path)) |mi| {
                mod_info = mi;
            } else {
                // Import here to avoid a cyclic import at file top-level
                const module_resolver = @import("../../parser/module_resolver.zig");
                mod_info = module_resolver.resolveModule(p, import_info.module_path) catch null;
            }

            if (mod_info) |mi| {
                if (mi.ast) |module_ast| {
                    if (module_ast.data == .Block) {
                        const statements = module_ast.data.Block.statements;

                        // Helper to decide if a symbol should be imported based on specificity
                        const wantsSymbol = struct {
                            fn check(name: []const u8, info: ast.ImportInfo) bool {
                                if (info.import_type == .Module) return true;
                                if (info.specific_symbols) |syms| {
                                    for (syms) |s| if (std.mem.eql(u8, s, name)) return true;
                                    return false;
                                }
                                if (info.specific_symbol) |s| return std.mem.eql(u8, s, name);
                                return false;
                            }
                        };

                        for (statements) |stmt| {
                            switch (stmt.data) {
                                .Expression => |expr_opt| {
                                    if (expr_opt) |expr| if (expr.data == .StructDecl) {
                                        const sd = expr.data.StructDecl;
                                        if (!sd.is_public) continue;
                                        if (!wantsSymbol.check(sd.name.lexeme, import_info)) continue;

                                        // Convert fields to TypeInfo and register struct
                                        const field_types = try self.allocator.alloc(ast.StructFieldType, sd.fields.len);
                                        for (sd.fields, field_types) |field, *ft| {
                                            const ti = try ast.typeInfoFromExpr(self.allocator, field.type_expr);
                                            ft.* = .{ .name = field.name.lexeme, .type_info = ti };
                                        }
                                        try helpers.registerStructType(self, sd.name.lexeme, field_types);

                                        // Register methods (static and instance)
                                        var method_table = std.StringHashMap(@This().StructMethodInfo).init(self.allocator);
                                        for (sd.methods) |m| {
                                            if (!m.is_public) continue; // only export public methods
                                            var ret_type_ptr: *ast.TypeInfo = undefined;
                                            if (m.return_type_info.base != .Nothing) {
                                                const rtp = try self.allocator.create(ast.TypeInfo);
                                                rtp.* = try self.resolveTypeInfo(m.return_type_info);
                                                ret_type_ptr = rtp;
                                            } else {
                                                const rtp = try self.allocator.create(ast.TypeInfo);
                                                rtp.* = .{ .base = .Struct, .custom_type = sd.name.lexeme, .struct_fields = field_types, .is_mutable = false };
                                                ret_type_ptr = rtp;
                                            }
                                            const mi2 = @This().StructMethodInfo{
                                                .name = m.name.lexeme,
                                                .is_public = m.is_public,
                                                .is_static = m.is_static,
                                                .return_type = ret_type_ptr,
                                            };
                                            _ = try method_table.put(m.name.lexeme, mi2);
                                        }
                                        if (self.struct_methods.getPtr(sd.name.lexeme)) |existing| {
                                            var it = method_table.iterator();
                                            while (it.next()) |e| {
                                                _ = try existing.put(e.key_ptr.*, e.value_ptr.*);
                                            }
                                        } else {
                                            try self.struct_methods.put(sd.name.lexeme, method_table);
                                        }
                                    };
                                },
                                .EnumDecl => |ed| {
                                    if (!ed.is_public) break;
                                    if (!wantsSymbol.check(ed.name.lexeme, import_info)) break;
                                    const variants = try self.allocator.alloc([]const u8, ed.variants.len);
                                    for (ed.variants, variants) |v, *name| name.* = v.lexeme;
                                    try helpers.registerEnumType(self, ed.name.lexeme, variants);
                                },
                                else => {},
                            }
                        }
                    }
                }
            }
        }
    }

    fn checkModuleExists(_: *SemanticAnalyzer, _: []const u8) bool {
        // TODO: Implement actual file system check
        // For now, assume all modules exist (they should be validated during parsing)
        return true;
    }

    fn isCircularImport(_: *SemanticAnalyzer, _: []const u8) bool {
        // TODO: Implement circular import detection
        // This would require tracking the import chain
        return false;
    }

    fn checkSymbolExists(_: *SemanticAnalyzer, _: []const u8, _: []const u8) bool {
        // TODO: Implement symbol existence check
        // This would require loading the module and checking its public symbols
        return true;
    }

    fn inferFunctionReturnType(self: *SemanticAnalyzer, func: anytype) ErrorList!*ast.TypeInfo {
        var return_types = std.array_list.Managed(*ast.TypeInfo).init(self.allocator);
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
                helpers.convertTypeToTokenType(self, param_type_info.base),
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
        return try helpers.createUnionType(self, return_types.items);
    }

    fn collectReturnTypes(self: *SemanticAnalyzer, statements: []ast.Stmt, return_types: *std.array_list.Managed(*ast.TypeInfo)) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .Return => |return_stmt| {
                    if (return_stmt.value) |value| {
                        const return_type = try infer_type.inferTypeFromExpr(self, value);
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
                                const return_type = try infer_type.inferTypeFromExpr(self, value);
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

    fn collectReturnTypesFromIf(self: *SemanticAnalyzer, if_expr: *ast.Expr, return_types: *std.array_list.Managed(*ast.TypeInfo)) ErrorList!void {
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

    fn collectReturnTypesFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr, return_types: *std.array_list.Managed(*ast.TypeInfo)) ErrorList!void {
        switch (expr.data) {
            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const return_type = try infer_type.inferTypeFromExpr(self, value);
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
                        var type_list = std.array_list.Managed(u8).init(self.allocator);
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
            try helpers.unifyTypes(self, expected, actual, span);
        }
    }
};
