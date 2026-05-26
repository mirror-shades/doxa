const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub const ImportedSymbol = struct {
    pub const EnumRole = enum { Type, Variant };

    kind: enum { Function, Enum, Struct, Variable, Type, Import, Group },
    name: []const u8,
    original_module: []const u8,
    namespace_alias: ?[]const u8 = null,
    param_count: ?u32 = null,
    param_types: ?[]ast.TypeInfo = null,
    return_type_info: ?ast.TypeInfo = null,
    enum_role: ?EnumRole = null,
    enum_type_name: ?[]const u8 = null,
};

pub fn parseModuleStmt(self: *Parser, is_public: bool) !ast.Stmt {
    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_MODULE_NAME, "expected module name after 'module' keyword", .{});
        return error.ExpectedModuleName;
    }
    const namespace = self.peek().lexeme;
    self.advance();

    if (self.peek().type != .FROM) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.MISSING_FROM_KEYWORD, "missing 'from' keyword after module name", .{});
        return error.MissingFromKeyword;
    }
    self.advance();

    if (self.peek().type != .STRING and self.peek().type != .SINGLE_STRING and self.peek().type != .STD) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_MODULE_PATH, "expected string literal with module path after 'from' keyword", .{});
        return error.ExpectedModulePath;
    }

    var module_path: []const u8 = undefined;
    if (self.peek().type == .STRING or self.peek().type == .SINGLE_STRING) {
        module_path = self.peek().lexeme;
        if (module_path.len >= 2) {
            module_path = module_path[1 .. module_path.len - 1];
        }
        self.advance();
    } else if (self.peek().type == .STD) {
        self.advance(); // @std
        if (self.peek().type != .LEFT_PAREN) return error.ExpectedLeftParen;
        self.advance(); // (
        if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
        self.advance(); // )
        module_path = try resolveStdPath(self.allocator);
    }

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .EOF and self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

    try self.registerModuleAlias(self.current_file, namespace, module_path, is_public);

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
                .is_public = is_public,
            },
        },
    };
}

pub fn parseImportStmt(self: *Parser, is_public: bool) !ast.Stmt {
    self.advance();

    var symbols = std.array_list.Managed([]const u8).init(self.allocator);
    defer symbols.deinit();

    if (self.peek().type != .IDENTIFIER) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_IMPORT_SYMBOL, "expected symbol name after 'import' keyword", .{});
        return error.ExpectedImportSymbol;
    }
    try symbols.append(self.peek().lexeme);
    self.advance();

    while (self.peek().type == .COMMA) {
        self.advance();

        if (self.peek().type != .IDENTIFIER) {
            const current_token = self.peek();
            const location = Location{
                .file = self.current_file,
                .file_uri = self.current_file_uri,
                .range = .{
                    .start_line = current_token.line,
                    .start_col = current_token.column,
                    .end_line = current_token.line,
                    .end_col = current_token.column,
                },
            };
            self.reporter.reportCompileError(location, ErrorCode.EXPECTED_IMPORT_SYMBOL, "expected symbol name after comma in import list", .{});
            return error.ExpectedImportSymbol;
        }
        try symbols.append(self.peek().lexeme);
        self.advance();
    }

    if (self.peek().type != .FROM) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.MISSING_FROM_KEYWORD, "missing 'from' keyword after import list", .{});
        return error.MissingFromKeyword;
    }
    self.advance();

    if (self.peek().type != .STRING and self.peek().type != .SINGLE_STRING) {
        const current_token = self.peek();
        const location = Location{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .range = .{
                .start_line = current_token.line,
                .start_col = current_token.column,
                .end_line = current_token.line,
                .end_col = current_token.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_MODULE_PATH, "expected string literal with module path after 'from' keyword", .{});
        return error.ExpectedModulePath;
    }

    var module_path = self.peek().lexeme;
    if (module_path.len >= 2) {
        module_path = module_path[1 .. module_path.len - 1];
    }
    self.advance();

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .EOF and self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

    const owned_symbols = try self.allocator.dupe([]const u8, symbols.items);

    for (owned_symbols) |symbol| {
        try self.recordSpecificImport(self.current_file, module_path, symbol, is_public);
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
                .specific_symbol = if (owned_symbols.len == 1) owned_symbols[0] else null,
                .is_public = is_public,
            },
        },
    };
}

pub fn findImportedSymbol(self: *Parser, name: []const u8) ?ImportedSymbol {
    if (self.imported_symbols) |symbols| {
        return symbols.get(name);
    }
    return null;
}

fn resolveStdPath(allocator: std.mem.Allocator) ![]const u8 {
    const exe_dir = std.fs.selfExeDirPathAlloc(allocator) catch return error.ModuleNotFound;
    defer allocator.free(exe_dir);
    return std.fs.path.join(allocator, &.{ exe_dir, "..", "lib", "std", "std.doxa" });
}
