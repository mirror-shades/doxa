const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("parser_types.zig").Parser;
const token = @import("../token.zig");
const ErrorList = @import("../reporting.zig").ErrorList;
const symbol_table = @import("./symbol_table.zig").SymbolTable;

pub const ModuleMode = enum {
    Safe,
    Normal,
};

pub const ModuleInfo = struct {
    name: []const u8,
    mode: ModuleMode,
    imports: []const ast.ImportInfo,
};

pub fn parseModuleDecl(self: *Parser) ErrorList!ast.Stmt {
    self.advance(); // consume 'module' keyword

    // Parse module name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedModuleName;
    }
    const name = self.peek();
    self.advance();

    // Check for safe modifier
    var is_safe = false;
    if (self.peek().type == .SAFE) {
        is_safe = true;
        self.advance();
    }

    // Parse imports
    var imports = std.ArrayList(ast.ImportInfo).init(self.allocator);
    defer imports.deinit();

    while (self.peek().type == .IMPORT) {
        self.advance();

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedImportName;
        }

        const import_name = self.peek();
        self.advance();

        // Verify import compatibility
        if (is_safe) {
            // Safe modules can only import other safe modules
            const imported_module = try self.resolveModule(import_name.lexeme);
            if (imported_module.mode != .Safe) {
                return error.UnsafeImportInSafeModule;
            }
        }

        try imports.append(.{
            .module_name = import_name.lexeme,
            .mode = if (is_safe) .Safe else .Normal,
        });
    }

    return ast.Stmt{ .Module = .{
        .name = name,
        .is_safe = is_safe,
        .imports = try imports.toOwnedSlice(),
    } };
}

// In your parser
pub fn parseImport(self: *Parser) !ast.Stmt {
    self.advance(); // consume 'import'

    // Parse the import path string literal
    if (self.peek().type != .STRING) {
        return error.ExpectedStringLiteral;
    }
    const path = self.peek().lexeme;
    self.advance();

    var import_name: token.Token = undefined;
    var specific_symbol: ?[]const u8 = null;

    if (self.peek().type == .AS) {
        self.advance();
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        import_name = self.peek();
        self.advance();
    } else {
        // Use filename as default name
        import_name = .{
            .type = .IDENTIFIER,
            .lexeme = std.fs.path.stem(path),
            .line = self.previous().line,
            .column = self.previous().column,
            .literal = .{ .nothing = {} },
        };
    }

    // Handle specific symbol import
    if (self.peek().type == .DOT) {
        self.advance();
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        specific_symbol = self.peek().lexeme;
        self.advance();
    }

    return ast.Stmt{ .Import = .{
        .module_name = import_name,
        .path = path,
        .specific_symbol = specific_symbol,
    } };
}

pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
    _ = self;
    _ = module_name;
    // Implement module file loading logic
    // This might involve searching in specific directories, handling file extensions, etc.
    // Return the source code as a string
}

pub fn extractModuleInfo(self: *Parser) ErrorList!ModuleInfo {
    _ = self;
    // Extract module information from the parsed AST
    // This includes module name, safety mode, imports, etc.
}

pub fn resolveImports(self: *Parser) !void {
    var it = self.symbol_table.symbols.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.kind == .Import) {
            const import = entry.value_ptr.kind.Import;
            const module = try self.loadModule(import.module_path);

            if (import.imported_symbol) |symbol_name| {
                // Import specific symbol
                const symbol = module.symbol_table.lookup(symbol_name) orelse
                    return error.SymbolNotFound;
                if (!symbol.is_public) {
                    return error.SymbolNotPublic;
                }
                try self.symbol_table.addSymbol(symbol);
            } else {
                // Import whole module
                var module_it = module.symbol_table.symbols.iterator();
                while (module_it.next()) |module_symbol| {
                    if (module_symbol.value_ptr.is_public) {
                        // Add with module prefix
                        const prefixed_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ entry.key_ptr.*, module_symbol.key_ptr.* });
                        try self.symbol_table.addSymbol(.{
                            .name = prefixed_name,
                            .is_public = false,
                            .declared_at = module_symbol.value_ptr.declared_at,
                            .kind = module_symbol.value_ptr.kind,
                        });
                    }
                }
            }
        }
    }
}
