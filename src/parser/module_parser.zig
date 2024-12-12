const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("parser_types.zig").Parser;
const token = @import("../token.zig");
const ErrorList = @import("../reporting.zig").ErrorList;

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

pub fn parseImport(self: *Parser, name: token.Token) ErrorList!ast.Stmt {
    _ = self;
    return ast.Stmt{ .Import = .{
        .module_name = name,
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
