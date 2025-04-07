const std = @import("std");
const Parser = @import("parser_types.zig").Parser;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Reporting = @import("../reporting.zig").Reporting;
const ErrorList = @import("../reporting.zig").ErrorList;

// Struct to track imported symbols
pub const ImportedSymbol = struct {
    kind: enum { Function, Enum, Struct, Variable, Type },
    name: []const u8,
    original_module: []const u8,
};

pub fn parseImportStmt(self: *Parser) !ast.Stmt {
    self.advance(); // consume 'import'

    // Parse module path
    if (self.peek().type != .STRING) {
        return error.ExpectedString;
    }

    // Get the module path and strip the quotes
    var module_path = self.peek().lexeme;
    if (module_path.len >= 2 and module_path[0] == '"' and module_path[module_path.len - 1] == '"') {
        module_path = module_path[1 .. module_path.len - 1];
    }

    self.advance();

    // Must have 'is' keyword
    if (self.peek().type != .ASSIGN) {
        return error.ExpectedAssignmentOperator;
    }
    self.advance();

    // Parse namespace alone or namespace.symbol
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const namespace = self.peek().lexeme;
    self.advance();

    // Check if there's a dot (for selective import)
    var specific_symbol: ?[]const u8 = null;
    if (self.peek().type == .DOT) {
        self.advance(); // consume dot

        // Get the specific symbol
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        specific_symbol = self.peek().lexeme;
        self.advance();
    }

    // Semicolon check
    if (self.peek().type != .SEMICOLON) {
        return error.ExpectedSemicolon;
    }
    self.advance();

    // Important: Load and process the module before continuing with main file parsing
    try self.loadAndRegisterModule(module_path, namespace, specific_symbol);

    return ast.Stmt{
        .Import = .{
            .module_path = module_path,
            .namespace_alias = namespace,
            .specific_symbol = specific_symbol,
        },
    };
}

// New function to load and register a module's symbols
pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
    // Get module info - this will parse the module if not already cached
    const module_info = try self.resolveModule(module_path);

    // Register the module in namespaces map
    try self.module_namespaces.put(namespace, module_info);

    // If we have the full module AST available (from module_info),
    // we can scan it for public symbols and register them
    if (module_info.ast) |module_ast| {
        try registerPublicSymbols(self, module_ast, module_path, namespace, specific_symbol);
    }
}

// Function to scan a module's AST and register its public symbols
fn registerPublicSymbols(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
    // Initialize symbol table for this module if needed
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(ImportedSymbol).init(self.allocator);
    }

    std.debug.print("Module AST type: {s}\n", .{@tagName(module_ast.*)});

    switch (module_ast.*) {
        .Block => {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt) {
                    // Handle enum declarations
                    .EnumDecl => |enum_decl| {
                        const is_public = enum_decl.is_public;
                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, enum_decl.name.lexeme });

                            // Only register if we want all symbols or this specific one
                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, enum_decl.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Enum,
                                    .name = enum_decl.name.lexeme,
                                    .original_module = module_path,
                                });

                                // Also register each enum variant for easy access
                                for (enum_decl.variants) |variant| {
                                    const variant_full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ full_name, variant.lexeme });
                                    try self.imported_symbols.?.put(variant_full_name, .{
                                        .kind = .Enum,
                                        .name = variant.lexeme,
                                        .original_module = module_path,
                                    });
                                }
                            }
                        }
                    },
                    // Handle function declarations
                    .Function => |func| {
                        const is_public = func.is_public;
                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, func.name.lexeme });

                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, func.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                });
                            }
                        }
                    },
                    // Handle struct declarations
                    .Expression => |expr| {
                        if (expr.* == .StructDecl) {
                            const struct_decl = expr.StructDecl;
                            const is_public = struct_decl.is_public;
                            if (is_public) {
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, struct_decl.name.lexeme });

                                if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, struct_decl.name.lexeme)) {
                                    try self.imported_symbols.?.put(full_name, .{
                                        .kind = .Struct,
                                        .name = struct_decl.name.lexeme,
                                        .original_module = module_path,
                                    });
                                }
                            }
                        }
                    },
                    .VarDecl => |var_decl| {
                        std.debug.print("VarDecl: {s}\n", .{var_decl.name.lexeme});
                        const is_public = var_decl.is_public;
                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, var_decl.name.lexeme });
                            std.debug.print("Importing variable: {s}\n", .{full_name});
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Variable,
                                .name = var_decl.name.lexeme,
                                .original_module = module_path,
                            });
                        }
                    },
                    else => {}, // Skip other types of statements
                }
            }
        },
        .Function => {
            // Handle if the module is represented as a function
            if (module_ast.Function.is_public) {
                // Process and register the function
            }
        },
        // ADD CASES FOR OTHER POSSIBLE MODULE TYPES
        else => {
            std.debug.print("Unhandled module AST type: {s}\n", .{@tagName(module_ast.*)});
        },
    }
}

// Helper to check if a symbol is imported and from which module
pub fn findImportedSymbol(self: *Parser, name: []const u8) ?ImportedSymbol {
    if (self.imported_symbols) |symbols| {
        return symbols.get(name);
    }
    return null;
}
