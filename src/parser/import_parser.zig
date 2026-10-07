const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const module_graph = @import("../module/graph.zig");
const token = @import("../types/token.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

/// Parse the module source that follows a `from` keyword: either a `"string"`
/// path or the `@std()` intrinsic. Assumes the caller has consumed `from`.
fn parseModuleSource(self: *Parser) ErrorList![]const u8 {
    if (self.peek().type != .STRING and self.peek().type != .STD) {
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

    if (self.peek().type == .STRING) {
        var module_path = self.peek().lexeme;
        if (module_path.len >= 2) {
            module_path = module_path[1 .. module_path.len - 1];
        }
        self.advance();
        return module_path;
    }

    // @std()
    self.advance(); // @std
    if (self.peek().type != .LEFT_PAREN) return error.ExpectedLeftParen;
    self.advance(); // (
    if (self.peek().type != .RIGHT_PAREN) return error.ExpectedRightParen;
    self.advance(); // )
    return module_graph.std_specifier;
}

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
    const namespace_token = self.peek();
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

    const module_path = try parseModuleSource(self);

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .EOF and self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

    const names = try self.allocator.alloc(token.Token, 1);
    names[0] = namespace_token;

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(namespace_token),
        },
        .data = .{
            .Import = .{
                .import_type = .Module,
                .module_path = module_path,
                .names = names,
                .is_public = is_public,
            },
        },
    };
}

pub fn parseImportStmt(self: *Parser, is_public: bool) !ast.Stmt {
    self.advance();

    var names = std.array_list.Managed(token.Token).init(self.allocator);
    errdefer names.deinit();

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
    try names.append(self.peek());
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
        try names.append(self.peek());
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

    const module_path = try parseModuleSource(self);

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE and self.peek().type != .EOF and self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedNewline;
    }
    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

    const first = names.items[0];
    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(first),
        },
        .data = .{
            .Import = .{
                .import_type = .Specific,
                .module_path = module_path,
                .names = try names.toOwnedSlice(),
                .is_public = is_public,
            },
        },
    };
}
