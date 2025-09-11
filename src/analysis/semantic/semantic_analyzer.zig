const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const Parser = @import("../../parser/parser_types.zig").Parser;
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const types = @import("types.zig");
const union_handling = @import("union_handling.zig");
const type_analysis = @import("type_analysis.zig");
const validation = @import("validation.zig");
const declaration_collection = @import("declaration_collection.zig");
const scope_management = @import("scope_management.zig");

/// Main semantic analyzer that orchestrates all analysis phases
pub const SemanticAnalyzer = struct {
    in_loop_scope: bool = false,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(u32, *ast.TypeInfo),
    custom_types: std.StringHashMap(types.CustomTypeInfo),
    struct_methods: std.StringHashMap(std.StringHashMap(types.StructMethodInfo)),
    function_return_types: std.AutoHashMap(u32, *ast.TypeInfo),
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
            .type_cache = std.AutoHashMap(u32, *ast.TypeInfo).init(allocator),
            .custom_types = std.StringHashMap(types.CustomTypeInfo).init(allocator),
            .struct_methods = std.StringHashMap(std.StringHashMap(types.StructMethodInfo)).init(allocator),
            .function_return_types = std.AutoHashMap(u32, *ast.TypeInfo).init(allocator),
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

    // NEW: Export custom type information for HIR generation
    pub fn getCustomTypes(self: *SemanticAnalyzer) std.StringHashMap(types.CustomTypeInfo) {
        return self.custom_types;
    }

    // NEW: Export struct methods for HIR generation
    pub fn getStructMethods(self: *SemanticAnalyzer) std.StringHashMap(std.StringHashMap(types.StructMethodInfo)) {
        return self.struct_methods;
    }

    // NEW: Register a custom type during semantic analysis
    fn registerCustomType(self: *SemanticAnalyzer, type_name: []const u8, kind: types.CustomTypeInfo.CustomTypeKind) !void {
        const custom_type = types.CustomTypeInfo{
            .name = try self.allocator.dupe(u8, type_name),
            .kind = kind,
        };
        try self.custom_types.put(type_name, custom_type);
    }

    // NEW: Register enum with variants
    fn registerEnumType(self: *SemanticAnalyzer, enum_name: []const u8, variants: []const []const u8) !void {
        var enum_variants = try self.allocator.alloc(types.CustomTypeInfo.EnumVariant, variants.len);
        for (variants, 0..) |variant_name, index| {
            enum_variants[index] = types.CustomTypeInfo.EnumVariant{
                .name = try self.allocator.dupe(u8, variant_name),
                .index = @intCast(index),
            };
        }

        const custom_type = types.CustomTypeInfo{
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

        const mem_custom_type = Memory.CustomTypeInfo{
            .name = try self.allocator.dupe(u8, enum_name),
            .kind = .Enum,
            .enum_variants = mem_enum_variants,
        };
        try self.memory.type_registry.put(enum_name, mem_custom_type);
    }

    // NEW: Register struct with fields
    fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
        var struct_fields = try self.allocator.alloc(types.CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field, index| {
            struct_fields[index] = types.CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field.name),
                .field_type_info = field.type_info,
                .custom_type_name = if (field.type_info.custom_type) |custom_name|
                    try self.allocator.dupe(u8, custom_name)
                else
                    null,
                .index = @intCast(index),
                .is_public = false, // Default to private
            };
        }

        const custom_type = types.CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);

        // Also register into runtime memory manager for VM access
        var mem_struct_fields = try self.allocator.alloc(Memory.CustomTypeInfo.StructField, fields.len);
        for (fields, 0..) |field, i| {
            mem_struct_fields[i] = Memory.CustomTypeInfo.StructField{
                .name = try self.allocator.dupe(u8, field.name),
                .field_type = try self.convertTypeInfoToMemoryType(field.type_info),
                .custom_type_name = if (field.type_info.custom_type) |custom_name|
                    try self.allocator.dupe(u8, custom_name)
                else
                    null,
                .index = @intCast(i),
                .is_public = false,
            };
        }

        const mem_custom_type = Memory.CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = mem_struct_fields,
        };
        try self.memory.type_registry.put(struct_name, mem_custom_type);
    }

    // Helper function to convert TypeInfo to Memory.CustomTypeInfo.Type
    fn convertTypeInfoToMemoryType(self: *SemanticAnalyzer, type_info: *ast.TypeInfo) !Memory.CustomTypeInfo.Type {
        _ = self;
        return switch (type_info.base) {
            .Int => .Int,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Byte => .Byte,
            .Nothing => .Nothing,
            .Array => .Array,
            .Struct => .Struct,
            .Map => .Map,
            .Enum => .Enum,
            .Function => .Function,
            .Union => .Union,
            .Custom => .Custom,
        };
    }

    // Convert CustomTypeInfo to TypeSystem.CustomTypeInfo
    pub fn convertCustomTypeInfo(semantic_type: types.CustomTypeInfo, allocator: std.mem.Allocator) !@import("../../codegen/hir/type_system.zig").TypeSystem.CustomTypeInfo {
        return types.convertCustomTypeInfo(semantic_type, allocator);
    }

    // Inject compiler-provided enums (shared error categories)
    fn ensureBuiltinEnums(self: *SemanticAnalyzer) !void {
        // Register built-in enums that are always available
        const error_variants = [_][]const u8{ "SyntaxError", "TypeError", "RuntimeError", "InternalError" };
        try self.registerEnumType("Error", &error_variants);
    }

    /// Main analysis function that orchestrates all phases
    pub fn analyze(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        const root_scope = try self.memory.scope_manager.createScope(null, self.memory);
        self.memory.scope_manager.root_scope = root_scope;
        self.current_scope = root_scope;

        // Inject compiler-provided enums (shared error categories)
        try self.ensureBuiltinEnums();

        // Create context for declaration collection
        var decl_ctx = declaration_collection.DeclarationCollectionContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.function_return_types,
            self.current_function_returns,
            self.parser,
            &self.fatal_error,
        );

        try declaration_collection.collectDeclarations(&decl_ctx, statements, root_scope);

        if (self.fatal_error) {
            return error.SemanticError;
        }

        // Create context for validation
        var validation_ctx = validation.ValidationContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
            self.current_initializing_var,
        );

        try validation.validateStatements(&validation_ctx, statements);
    }

    // Delegate to modular functions
    pub fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) !*ast.TypeInfo {
        const type_ctx = type_analysis.TypeAnalysisContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
        );
        return type_analysis.inferTypeFromExpr(&type_ctx, expr);
    }

    pub fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Memory.Variable {
        return scope_management.lookupVariable(
            self.current_scope,
            self.parser,
            self.allocator,
            self.reporter,
            name,
        );
    }

    pub fn unifyTypes(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
        const type_ctx = type_analysis.TypeAnalysisContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
        );
        return type_analysis.unifyTypes(&type_ctx, expected, actual, span);
    }

    pub fn resolveTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        const type_ctx = type_analysis.TypeAnalysisContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
        );
        return type_analysis.resolveTypeInfo(&type_ctx, type_info);
    }

    pub fn deepCopyTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
        const type_ctx = type_analysis.TypeAnalysisContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
        );
        return type_analysis.deepCopyTypeInfo(&type_ctx, type_info);
    }

    pub fn deepCopyTypeInfoPtr(self: *SemanticAnalyzer, src: *ast.TypeInfo) !*ast.TypeInfo {
        const type_ctx = type_analysis.TypeAnalysisContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
        );
        return type_analysis.deepCopyTypeInfoPtr(&type_ctx, src);
    }

    pub fn flattenUnionType(self: *SemanticAnalyzer, union_type: *ast.UnionType) !*ast.UnionType {
        return union_handling.flattenUnionType(self.allocator, union_type);
    }

    pub fn createUnionType(self: *SemanticAnalyzer, type_list: []*ast.TypeInfo) !*ast.TypeInfo {
        return union_handling.createUnionType(self.allocator, type_list);
    }

    pub fn unionContainsNothing(self: *SemanticAnalyzer, union_type_info: ast.TypeInfo) bool {
        _ = self;
        return union_handling.unionContainsNothing(union_type_info);
    }

    pub fn getUnionDefaultValue(self: *SemanticAnalyzer, union_type: *ast.UnionType) @import("../../types/types.zig").TokenLiteral {
        _ = self;
        return union_handling.getUnionDefaultValue(union_type);
    }

    pub fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) @import("../../types/token.zig").TokenType {
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

    // Additional methods that were in the original file
    pub fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
        const decl_ctx = declaration_collection.DeclarationCollectionContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.function_return_types,
            self.current_function_returns,
            self.parser,
            &self.fatal_error,
        );
        return declaration_collection.collectDeclarations(&decl_ctx, statements, scope);
    }

    pub fn validateStatements(self: *SemanticAnalyzer, statements: []const ast.Stmt) ErrorList!void {
        const validation_ctx = validation.ValidationContext.init(
            self.allocator,
            self.reporter,
            self.memory,
            self.current_scope,
            self.type_cache,
            self.custom_types,
            self.struct_methods,
            self.parser,
            &self.fatal_error,
            self.current_initializing_var,
        );
        return validation.validateStatements(&validation_ctx, statements);
    }
};
