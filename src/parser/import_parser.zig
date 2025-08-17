const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const printDebug = std.debug.print;

// Struct to track imported symbols
pub const ImportedSymbol = struct {
    kind: enum { Function, Enum, Struct, Variable, Type, Import },
    name: []const u8,
    original_module: []const u8,
    namespace_alias: ?[]const u8 = null,
    // Optional function metadata for accurate arity/typing
    param_count: ?u32 = null,
    return_type_info: ?ast.TypeInfo = null,
};

// New function to handle module imports: "module math from "./math.doxa";"
pub fn parseModuleStmt(self: *Parser) !ast.Stmt {
    self.advance(); // consume 'module'

    // Parse namespace
    if (self.peek().type != .IDENTIFIER) {
        printDebug("use the syntax: \n'module moduleName from \"./file.doxa\";'\n", .{});
        return error.ExpectedIdentifier;
    }
    const namespace = self.peek().lexeme;
    self.advance();

    // Must have 'from' keyword
    if (self.peek().type != .FROM) {
        printDebug("use the syntax: \n'module moduleName from \"./file.doxa\";'\n", .{});
        return error.ImportMustHaveFrom;
    }
    self.advance();

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

    // Semicolon check
    if (self.peek().type != .NEWLINE) {
        return error.ExpectedSemicolon;
    }
    self.advance();

    // Load and register the entire module as a namespace
    try self.loadAndRegisterModule(module_path, namespace, null);
    if (self.debug_enabled) {
        std.debug.print("Parser: module alias '{s}' -> {s}\n", .{ namespace, module_path });
    }

    // Record this module import so transitive imports are visible during
    // module info extraction (since top-level imports are consumed in pass 1)
    // This associates the current file with the imported path under the alias.
    // Safe to ignore errors here; symbol resolution will still attempt other paths.
    _ = self.recordModuleImport(self.current_file, namespace, module_path) catch {};

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Import = .{
                .import_type = .Module,
                .module_path = module_path,
                .namespace_alias = namespace,
                .specific_symbols = null,
                .specific_symbol = null,
            },
        },
    };
}

// Updated function to handle specific imports: "import add from "./math.doxa";" or "import add, subtract from "./math.doxa";"
pub fn parseImportStmt(self: *Parser) !ast.Stmt {
    self.advance(); // consume 'import'

    // Parse specific symbols (one or more)
    var symbols = std.ArrayList([]const u8).init(self.allocator);
    defer symbols.deinit();

    // Parse first symbol
    if (self.peek().type != .IDENTIFIER) {
        printDebug("use the syntax: \n'import symbolName from \"./file.doxa\";'\n", .{});
        return error.ExpectedIdentifier;
    }
    try symbols.append(self.peek().lexeme);
    self.advance();

    // Parse additional symbols if comma-separated
    while (self.peek().type == .COMMA) {
        self.advance(); // consume comma

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        try symbols.append(self.peek().lexeme);
        self.advance();
    }

    // Must have 'from' keyword
    if (self.peek().type != .FROM) {
        printDebug("use the syntax: \n'import symbolName from \"./file.doxa\";'\n", .{});
        return error.ImportMustHaveFrom;
    }
    self.advance();

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

    // Semicolon check
    if (self.peek().type != .NEWLINE) {
        return error.ExpectedSemicolon;
    }
    self.advance();

    // Convert symbols to owned slice
    const owned_symbols = try self.allocator.dupe([]const u8, symbols.items);

    // Load and register specific symbols from the module
    for (owned_symbols) |symbol| {
        try self.loadAndRegisterSpecificSymbol(module_path, symbol);
    }
    if (self.debug_enabled) {
        for (owned_symbols) |symbol| {
            std.debug.print("Parser: import symbol '{s}' from {s}\n", .{ symbol, module_path });
        }
    }

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Import = .{
                .import_type = .Specific,
                .module_path = module_path,
                .namespace_alias = null,
                .specific_symbols = owned_symbols,
                .specific_symbol = if (owned_symbols.len == 1) owned_symbols[0] else null, // Legacy support
            },
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

// New function to load and register specific symbols (for specific imports)
pub fn loadAndRegisterSpecificSymbol(self: *Parser, module_path: []const u8, symbol_name: []const u8) !void {
    // Get module info - this will parse the module if not already cached
    const module_info = try self.resolveModule(module_path);

    // If we have the full module AST available (from module_info),
    // we can scan it for the specific symbol and register it
    if (module_info.ast) |module_ast| {
        try registerSpecificSymbol(self, module_ast, module_path, symbol_name);
    }
}

// Function to scan a module's AST and register a specific symbol
fn registerSpecificSymbol(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, symbol_name: []const u8) !void {
    // Initialize symbol table for this module if needed
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(ImportedSymbol).init(self.allocator);
    }

    switch (module_ast.*) {
        .Block => {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt.data) {
                    // Handle function declarations
                    .Function => |func| {
                        const is_public = func.is_public;
                        if (is_public and std.mem.eql(u8, func.name.lexeme, symbol_name)) {
                            // Register the symbol directly (not with namespace prefix)
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Function,
                                .name = func.name.lexeme,
                                .original_module = module_path,
                                .param_count = @intCast(func.params.len),
                                .return_type_info = func.return_type_info,
                            });
                            return; // Found the symbol, we're done
                        }
                    },
                    // Handle variable declarations
                    .VarDecl => |var_decl| {
                        const is_public = var_decl.is_public;
                        if (is_public and std.mem.eql(u8, var_decl.name.lexeme, symbol_name)) {
                            // Register the symbol directly (not with namespace prefix)
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Variable,
                                .name = var_decl.name.lexeme,
                                .original_module = module_path,
                            });
                            return; // Found the symbol, we're done
                        }
                    },
                    // Handle enum declarations
                    .EnumDecl => |enum_decl| {
                        const is_public = enum_decl.is_public;
                        if (is_public and std.mem.eql(u8, enum_decl.name.lexeme, symbol_name)) {
                            // Register the symbol directly (not with namespace prefix)
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Enum,
                                .name = enum_decl.name.lexeme,
                                .original_module = module_path,
                            });
                            return; // Found the symbol, we're done
                        }
                    },
                    // Handle struct declarations
                    .Expression => |expr| {
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.StructDecl;
                            const is_public = struct_decl.is_public;
                            if (is_public and std.mem.eql(u8, struct_decl.name.lexeme, symbol_name)) {
                                // Register the symbol directly (not with namespace prefix)
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Struct,
                                    .name = struct_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return; // Found the symbol, we're done
                            }
                        }
                    },
                    else => {}, // Skip other types of statements
                }
            }
        },
        else => {}, // Skip other AST types
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
                switch (stmt.data) {
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
                        if (expr.data == .StructDecl) {
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
                    .Import => |import_info| {
                        if (self.debug_enabled) {
                            std.debug.print("Found import: {s} as {?s}\n", .{ import_info.module_path, import_info.namespace_alias });
                        }
                        try self.imported_symbols.?.put(import_info.module_path, .{
                            .kind = .Import,
                            .name = import_info.module_path,
                            .original_module = module_path,
                            .namespace_alias = import_info.namespace_alias,
                        });

                        // Also register the imported module's namespace so codegen can resolve module calls
                        if (import_info.namespace_alias) |alias| {
                            // Skip self-imports
                            if (!std.mem.eql(u8, import_info.module_path, module_path)) {
                                // Resolve the imported module and put into namespaces map
                                const imported_info = self.resolveModule(import_info.module_path) catch |err| blk: {
                                    if (self.debug_enabled) {
                                        std.debug.print("Failed to resolve imported module {s}: {}\n", .{ import_info.module_path, err });
                                    }
                                    break :blk null;
                                };
                                if (imported_info) |mi| {
                                    _ = self.module_namespaces.put(alias, mi) catch |err| {
                                        if (self.debug_enabled) {
                                            std.debug.print("Failed to register namespace {s}: {}\n", .{ alias, err });
                                        }
                                    };
                                }
                            }
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
