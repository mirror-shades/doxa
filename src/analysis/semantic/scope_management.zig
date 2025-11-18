const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const Variable = Memory.Variable;
const Parser = @import("../../parser/parser_types.zig").Parser;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;

/// Look up a variable in the current scope and imported symbols
pub fn lookupVariable(
    current_scope: ?*Memory.Scope,
    parser: ?*const Parser,
    allocator: std.mem.Allocator,
    _: *Reporter,
    name: []const u8,
) ?*Variable {
    if (current_scope) |scope| {
        const result = scope.lookupVariable(name);
        if (result != null) return result;
    }

    // Check for imported symbols if not found in scope
    if (parser) |p| {
        if (p.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(name)) |imported_symbol| {
                // Create a variable for the imported symbol
                return createImportedSymbolVariable(allocator, name, imported_symbol);
            }
        }

        // Check for module namespaces
        if (p.module_namespaces.contains(name)) {
            // Create a variable for the module namespace
            return createModuleNamespaceVariable(current_scope, allocator, name);
        }
    }

    return null;
}

/// Helper function to create a Variable for a module namespace
pub fn createModuleNamespaceVariable(
    current_scope: ?*Memory.Scope,
    allocator: std.mem.Allocator,
    name: []const u8,
) ?*Variable {
    // Create a TypeInfo for the module namespace
    const type_info = ast.TypeInfo.createDefault(allocator) catch return null;
    errdefer allocator.destroy(type_info);

    type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false, .custom_type = name };

    // Convert TypeInfo to TokenType
    const token_type = convertTypeToTokenType(type_info.base);

    // Create a Variable object for the module namespace using the scope's createValueBinding
    if (current_scope) |scope| {
        // Create a placeholder value for the module namespace
        const placeholder_value = TokenLiteral{ .nothing = {} };

        const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
        return variable;
    }

    return null;
}

/// Helper function to check if a name is a module namespace
pub fn isModuleNamespace(parser: ?*const Parser, name: []const u8) bool {
    if (parser) |p| {
        if (p.module_namespaces.contains(name)) {
            return true;
        }
    }
    return false;
}

/// Helper function to handle module field access (e.g., math.add)
pub fn handleModuleFieldAccess(
    parser: ?*const Parser,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    module_name: []const u8,
    field_name: []const u8,
    span: ast.SourceSpan,
) !*ast.TypeInfo {
    var type_info = try ast.TypeInfo.createDefault(allocator);
    errdefer allocator.destroy(type_info);

    if (parser) |p| {
        // Look for the field in the module's imported symbols
        if (p.imported_symbols) |imported_symbols| {
            const full_name = std.fmt.allocPrint(allocator, "{s}.{s}", .{ module_name, field_name }) catch {
                reporter.reportCompileError(
                    span.location,
                    ErrorCode.INTERNAL_ERROR,
                    "Internal error: Could not format module field name",
                    .{},
                );
                type_info.base = .Nothing;
                return type_info;
            };
            defer allocator.free(full_name);

            if (imported_symbols.get(full_name)) |imported_symbol| {
                // Return appropriate type based on the imported symbol kind
                switch (imported_symbol.kind) {
                    .Function => {
                        // Try to find the actual function definition to get its return type
                        var found_func_type: ?*ast.FunctionType = null;

                        // Look through module namespaces to find the actual function
                        if (parser) |parser_ptr| {
                            var it = parser_ptr.module_namespaces.iterator();
                            while (it.next()) |entry| {
                                const module_info = entry.value_ptr.*;
                                if (module_info.ast) |module_ast| {
                                    if (module_ast.data == .Block) {
                                        const stmts = module_ast.data.Block.statements;
                                        for (stmts) |s| {
                                            switch (s.data) {
                                                .FunctionDecl => |f| {
                                                    if (!f.is_public) continue;
                                                    if (!std.mem.eql(u8, f.name.lexeme, field_name)) continue;

                                                    // Found matching function; construct FunctionType from its params/return
                                                    const ft = allocator.create(ast.FunctionType) catch break;

                                                    // Duplicate param TypeInfos into a flat slice
                                                    var params_list = std.array_list.Managed(ast.TypeInfo).init(allocator);
                                                    errdefer params_list.deinit();
                                                    for (f.params) |_| {
                                                        const ti = ast.TypeInfo{ .base = .Nothing };
                                                        params_list.append(ti) catch break;
                                                    }
                                                    const params_slice = params_list.toOwnedSlice() catch break;

                                                    // Use the actual return type from the function declaration
                                                    const ret_ptr = ast.TypeInfo.createDefault(allocator) catch break;
                                                    ret_ptr.* = f.return_type_info;

                                                    ft.* = ast.FunctionType{ .params = params_slice, .return_type = ret_ptr };
                                                    found_func_type = ft;
                                                    break;
                                                },
                                                else => {},
                                            }
                                        }
                                    }
                                }
                                if (found_func_type != null) break;
                            }
                        }

                        // Fallback if not found: zero params and nothing return (safer than Int)
                        if (found_func_type == null) {
                            const return_type = ast.TypeInfo.createDefault(allocator) catch return type_info;
                            return_type.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                            const function_type = allocator.create(ast.FunctionType) catch return type_info;
                            function_type.* = ast.FunctionType{ .params = &[_]ast.TypeInfo{}, .return_type = return_type };
                            found_func_type = function_type;
                        }

                        type_info.* = ast.TypeInfo{
                            .base = .Function,
                            .is_mutable = false,
                            .function_type = found_func_type,
                        };
                        return type_info;
                    },
                    .Variable => {
                        // For now, assume variables are integers
                        type_info.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };
                        return type_info;
                    },
                }
            }
        }
    }

    // If not found, report error
    reporter.reportCompileError(
        span.location,
        ErrorCode.UNDEFINED_VARIABLE,
        "Module '{s}' has no field '{s}'",
        .{ module_name, field_name },
    );
    type_info.base = .Nothing;
    return type_info;
}

/// Helper function to create a Variable for an imported symbol
fn createImportedSymbolVariable(
    allocator: std.mem.Allocator,
    name: []const u8,
    imported_symbol: @import("../../parser/import_parser.zig").ImportedSymbol,
) ?*Variable {
    _ = name; // May be used in the future

    // Create appropriate TypeInfo based on symbol kind
    const type_info = ast.TypeInfo.createDefault(allocator) catch return null;
    errdefer allocator.destroy(type_info);

    switch (imported_symbol.kind) {
        .Function => {
            // Fallback: zero params and nothing return (safer than Int)
            const return_type = ast.TypeInfo.createDefault(allocator) catch {
                allocator.destroy(type_info);
                return null;
            };
            return_type.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };

            const function_type = allocator.create(ast.FunctionType) catch {
                allocator.destroy(type_info);
                allocator.destroy(return_type);
                return null;
            };
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
        .Variable => {
            // Assume imported variables are integers for now
            type_info.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };
        },
        .Enum => {
            // Assume imported enums are custom types for now
            type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false };
        },
        .Struct => {
            // Assume imported structs are custom types for now
            type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false };
        },
        .Type => {
            // Assume imported types are custom types for now
            type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false };
        },
        .Import => {
            // Assume imported modules are custom types for now
            type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false };
        },
    }

    // Note: This function doesn't have access to a scope, so we can't create a Variable directly
    // The caller should handle this case appropriately
    return null;
}

/// Convert TypeInfo base type to TokenType
fn convertTypeToTokenType(base_type: ast.Type) TokenType {
    return switch (base_type) {
        .Int => .INT,
        .Float => .FLOAT,
        .String => .STRING,
        .Tetra => .TETRA,
        .Byte => .BYTE,
        .Nothing => .NOTHING,
        .Array => .ARRAY,
        .Struct => .STRUCT,
        .Map => .MAP,
        .Enum => .ENUM,
        .Function => .FUNCTION,
        .Union => .UNION,
        .Custom => .CUSTOM,
    };
}
