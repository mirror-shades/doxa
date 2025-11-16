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
const module_resolver = @import("module_resolver.zig");

pub const ImportedSymbol = struct {
    kind: enum { Function, Enum, Struct, Variable, Type, Import },
    name: []const u8,
    original_module: []const u8,
    namespace_alias: ?[]const u8 = null,
    param_count: ?u32 = null,
    param_types: ?[]ast.TypeInfo = null,
    return_type_info: ?ast.TypeInfo = null,
};

pub fn parseModuleStmt(self: *Parser) !ast.Stmt {
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

    if (self.peek().type != .STRING) {
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
    if (module_path.len >= 2 and module_path[0] == '"' and module_path[module_path.len - 1] == '"') {
        module_path = module_path[1 .. module_path.len - 1];
    }
    self.advance();

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE) {
        return error.ExpectedNewline;
    }
    self.advance();

    try self.loadAndRegisterModule(module_path, namespace, null);

    // Safe to ignore errors here; symbol resolution will still attempt other paths.
    _ = module_resolver.recordModuleImport(self, self.current_file, namespace, module_path) catch {};

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

pub fn parseImportStmt(self: *Parser) !ast.Stmt {
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

    if (self.peek().type != .STRING) {
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
    if (module_path.len >= 2 and module_path[0] == '"' and module_path[module_path.len - 1] == '"') {
        module_path = module_path[1 .. module_path.len - 1];
    }
    self.advance();

    if (self.peek().type == .SEMICOLON) {
        self.advance();
    }
    if (self.peek().type != .NEWLINE) {
        return error.ExpectedNewline;
    }
    self.advance();

    const owned_symbols = try self.allocator.dupe([]const u8, symbols.items);

    for (owned_symbols) |symbol| {
        try self.loadAndRegisterSpecificSymbol(module_path, symbol);
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
            },
        },
    };
}

pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
    const module_info = try module_resolver.resolveModule(self, module_path);

    try self.module_namespaces.put(namespace, module_info);

    if (module_info.ast) |module_ast| {
        try registerPublicSymbols(self, module_ast, module_path, namespace, specific_symbol);
    }
}

pub fn loadAndRegisterSpecificSymbol(self: *Parser, module_path: []const u8, symbol_name: []const u8) !void {
    const module_info = try module_resolver.resolveModule(self, module_path);

    if (module_info.ast) |module_ast| {
        try registerSpecificSymbol(self, module_ast, module_path, symbol_name);
    }
}

fn registerSpecificSymbol(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, symbol_name: []const u8) !void {
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(ImportedSymbol).init(self.allocator);
    }

    switch (module_ast.*) {
        .Block => {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt.data) {
                    .FunctionDecl => |func| {
                        const is_public = func.is_public;
                        if (is_public and std.mem.eql(u8, func.name.lexeme, symbol_name)) {
                            var param_types: ?[]ast.TypeInfo = null;
                            if (func.params.len > 0) {
                                param_types = try self.allocator.alloc(ast.TypeInfo, func.params.len);
                                for (func.params, 0..) |param, i| {
                                    if (param.type_expr) |type_expr| {
                                        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                                        param_types.?[i] = type_info_ptr.*;
                                        self.allocator.destroy(type_info_ptr);
                                    } else {
                                        param_types.?[i] = .{ .base = .Nothing };
                                    }
                                }
                            }
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Function,
                                .name = func.name.lexeme,
                                .original_module = module_path,
                                .param_count = @intCast(func.params.len),
                                .param_types = param_types,
                                .return_type_info = func.return_type_info,
                            });
                            return;
                        }
                    },
                    .VarDecl => |var_decl| {
                        const is_public = var_decl.is_public;
                        if (is_public and std.mem.eql(u8, var_decl.name.lexeme, symbol_name)) {
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Variable,
                                .name = var_decl.name.lexeme,
                                .original_module = module_path,
                            });
                            return;
                        }
                    },
                    .EnumDecl => |enum_decl| {
                        const is_public = enum_decl.is_public;
                        if (is_public and std.mem.eql(u8, enum_decl.name.lexeme, symbol_name)) {
                            try self.imported_symbols.?.put(symbol_name, .{
                                .kind = .Enum,
                                .name = enum_decl.name.lexeme,
                                .original_module = module_path,
                            });
                            return;
                        }
                    },
                    .Expression => |expr| {
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.StructDecl;
                            const is_public = struct_decl.is_public;
                            if (is_public and std.mem.eql(u8, struct_decl.name.lexeme, symbol_name)) {
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Struct,
                                    .name = struct_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return;
                            }
                        }
                    },
                    else => {},
                }
            }
        },
        else => {},
    }
}

pub fn findImportedSymbol(self: *Parser, name: []const u8) ?ImportedSymbol {
    if (self.imported_symbols) |symbols| {
        return symbols.get(name);
    }
    return null;
}

fn registerPublicSymbols(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(ImportedSymbol).init(self.allocator);
    }
    if (module_ast.data == .Block) {
        const statements = module_ast.data.Block.statements;

        for (statements) |stmt| {
            switch (stmt.data) {
                .EnumDecl => |enum_decl| {
                    const is_public = enum_decl.is_public;

                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, enum_decl.name.lexeme });

                        if (specific_symbol) |specific| {
                            if (std.mem.eql(u8, specific, enum_decl.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Enum,
                                    .name = enum_decl.name.lexeme,
                                    .original_module = module_path,
                                });

                                for (enum_decl.variants) |variant| {
                                    const variant_full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ full_name, variant.lexeme });
                                    try self.imported_symbols.?.put(variant_full_name, .{
                                        .kind = .Enum,
                                        .name = variant.lexeme,
                                        .original_module = module_path,
                                    });
                                }
                            }
                        } else {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Enum,
                                .name = enum_decl.name.lexeme,
                                .original_module = module_path,
                            });

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
                .Expression => |expr_opt| {
                    if (expr_opt) |expr| {
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.data.StructDecl;
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
                    }
                },
                .VarDecl => |var_decl| {
                    const is_public = var_decl.is_public;
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, var_decl.name.lexeme });

                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, var_decl.name.lexeme)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Variable,
                                .name = var_decl.name.lexeme,
                                .original_module = module_path,
                            });
                        }
                    }
                },
                .Import => |import_info| {
                    try self.imported_symbols.?.put(import_info.module_path, .{
                        .kind = .Import,
                        .name = import_info.module_path,
                        .original_module = module_path,
                        .namespace_alias = import_info.namespace_alias,
                    });
                },
                else => {},
            }
        }
    }
}
