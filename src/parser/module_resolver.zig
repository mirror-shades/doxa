const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");
const ModuleInfo = ast.ModuleInfo;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const import_parser = @import("import_parser.zig");

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const Parser = @import("parser_types.zig").Parser;
const ModuleResolutionStatus = @import("parser_types.zig").ModuleResolutionStatus;
const ImportStackEntry = @import("parser_types.zig").ImportStackEntry;

pub fn resolveModule(self: *Parser, module_name: []const u8) ErrorList!ast.ModuleInfo {
    // Built-in module: raylib
    if (std.mem.eql(u8, module_name, "graphics")) {
        var imports = std.array_list.Managed(ast.ImportInfo).init(self.allocator);
        defer imports.deinit();

        const info = ast.ModuleInfo{
            .name = "graphics",
            .imports = try imports.toOwnedSlice(),
            .ast = null,
            .file_path = "graphics",
            .symbols = null,
        };
        try self.module_cache.put("graphics", info);
        return info;
    }
    // Normalize the module path for consistent tracking
    const normalized_path = try normalizeModulePath(self, module_name);
    defer self.allocator.free(normalized_path);

    // Check if we're already processing this module (circular import)
    if (self.module_resolution_status.get(normalized_path)) |status| {
        switch (status) {
            .IN_PROGRESS => {
                // Circular import detected! Build error message with full chain
                return self.reportCircularImport(normalized_path);
            },
            .COMPLETED => {
                // Module is already resolved, return cached result
                return self.module_cache.get(normalized_path).?;
            },
            .NOT_STARTED => {
                // This shouldn't happen, but handle gracefully
            },
        }
    }

    // Check module cache (for completed modules)
    if (self.module_cache.get(normalized_path)) |info| {
        return info;
    }

    // Mark this module as being processed
    try self.module_resolution_status.put(normalized_path, .IN_PROGRESS);

    // Add to import stack for tracking
    const current_file_copy = try self.allocator.dupe(u8, self.current_file);
    try self.import_stack.append(.{
        .module_path = try self.allocator.dupe(u8, normalized_path),
        .imported_from = if (self.import_stack.items.len > 0) current_file_copy else null,
    });

    // Ensure we clean up the import stack on exit
    defer {
        _ = self.import_stack.pop();
    }

    // Load and parse module file - get both source and resolved path
    const module_data = self.loadModuleSourceWithPath(module_name) catch |err| {
        // Mark as not started since we failed to load
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

    var module_lexer = LexicalAnalyzer.init(self.allocator, module_data.source, module_name, self.reporter);
    defer module_lexer.deinit();

    try module_lexer.initKeywords();
    const tokens = module_lexer.lexTokens() catch |err| {
        // Mark as not started since we failed to lex
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

    var new_parser = Parser.init(self.allocator, tokens.items, module_data.resolved_path, self.reporter);

    // Copy the resolution status and import stack to the new parser, but exclude the current module
    new_parser.module_resolution_status = std.StringHashMap(ModuleResolutionStatus).init(self.allocator);
    var it = self.module_resolution_status.iterator();
    while (it.next()) |entry| {
        if (!std.mem.eql(u8, entry.key_ptr.*, normalized_path)) {
            try new_parser.module_resolution_status.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }
    new_parser.import_stack = try self.import_stack.clone();

    // Parse the module file
    const module_statements = new_parser.execute() catch |err| {
        // Mark as not started since we failed to parse
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

    // Create a block expression to hold the module statements
    const module_block = try self.allocator.create(ast.Expr);
    module_block.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(self.peek()),
        },
        .data = .{
            .Block = .{
                .statements = module_statements,
                .value = null,
            },
        },
    };

    // Extract module info and cache it
    const info = try extractModuleInfoWithParser(self, module_block, module_data.resolved_path, null, &new_parser);

    // Mark as completed and cache the result
    try self.module_resolution_status.put(normalized_path, .COMPLETED);
    try self.module_cache.put(normalized_path, info);

    return info;
}

fn normalizeModulePath(self: *Parser, module_path: []const u8) ![]const u8 {
    // Remove ./ prefix if present
    var clean_path = module_path;
    if (std.mem.startsWith(u8, clean_path, "./")) {
        clean_path = clean_path[2..];
    }

    // Add .doxa extension if not present
    if (!std.mem.endsWith(u8, clean_path, ".doxa")) {
        return try std.fmt.allocPrint(self.allocator, "{s}.doxa", .{clean_path});
    }

    return try self.allocator.dupe(u8, clean_path);
}

fn reportCircularImport(self: *Parser, current_module: []const u8) ErrorList!ast.ModuleInfo {
    var error_msg = std.array_list.Managed(u8).init(self.allocator);
    defer error_msg.deinit();

    try error_msg.appendSlice("Circular import detected:\n");

    // Show the import chain
    for (self.import_stack.items) |entry| {
        try error_msg.appendSlice("  ");
        try error_msg.appendSlice(entry.module_path);
        try error_msg.appendSlice(" imports\n");
    }

    // Show the circular dependency
    try error_msg.appendSlice("  ");
    try error_msg.appendSlice(current_module);
    try error_msg.appendSlice(" (circular dependency)\n");

    // Report the error with correct format
    self.reporter.reportCompileError(null, null, "{s}", .{error_msg.items});

    return error.CircularImport;
}

const ModuleData = struct {
    source: []const u8,
    resolved_path: []const u8,
};

pub fn loadModuleSourceWithPath(self: *Parser, module_name: []const u8) ErrorList!ModuleData {
    var clean_name = module_name;
    if (std.mem.startsWith(u8, clean_name, "./")) {
        clean_name = clean_name[2..];
    }

    // Check if the module name already has an extension
    const has_doxa_ext = std.mem.endsWith(u8, clean_name, ".doxa");
    const has_extension = has_doxa_ext;

    // Get the directory of the current file
    const current_dir = std.fs.path.dirname(self.current_file) orelse ".";

    const readFileContentsWithPath = struct {
        fn read(alloc: std.mem.Allocator, file_path: []const u8) !ModuleData {
            var file = try std.fs.cwd().openFile(file_path, .{});
            defer file.close();

            const size = try file.getEndPos();
            const buffer = try alloc.alloc(u8, size);
            errdefer alloc.free(buffer);

            const bytes_read = try file.readAll(buffer);
            if (bytes_read != size) {
                alloc.free(buffer);
                return error.IncompleteRead;
            }

            // Create a copy of the path for the resolved_path
            const path_copy = try alloc.dupe(u8, file_path);

            return ModuleData{
                .source = buffer,
                .resolved_path = path_copy,
            };
        }
    }.read;

    // Try several file locations in order
    var err: anyerror = error.FileNotFound;

    // Prefer resolving relative to the current file first
    // 0a. Same directory as current file with potential existing extension
    if (has_extension) {
        var same_dir_exact_path_pre = std.array_list.Managed(u8).init(self.allocator);
        defer same_dir_exact_path_pre.deinit();
        try same_dir_exact_path_pre.appendSlice(current_dir);
        try same_dir_exact_path_pre.appendSlice("/");
        try same_dir_exact_path_pre.appendSlice(clean_name);
        if (readFileContentsWithPath(self.allocator, same_dir_exact_path_pre.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }
    }

    // 0b. Same directory as current file with .doxa extension
    var same_dir_path_pre = std.array_list.Managed(u8).init(self.allocator);
    defer same_dir_path_pre.deinit();
    try same_dir_path_pre.appendSlice(current_dir);
    try same_dir_path_pre.appendSlice("/");
    try same_dir_path_pre.appendSlice(clean_name);
    if (!has_extension) try same_dir_path_pre.appendSlice(".doxa");
    if (readFileContentsWithPath(self.allocator, same_dir_path_pre.items)) |data| {
        return data;
    } else |e| {
        err = e;
    }

    // 1. First try directly with the given name (possibly including extension)
    if (has_extension) {
        var direct_path = std.array_list.Managed(u8).init(self.allocator);
        defer direct_path.deinit();
        try direct_path.appendSlice(clean_name);

        if (readFileContentsWithPath(self.allocator, direct_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }
    }

    // 2. Try in CWD with .doxa extension
    var cwd_doxa_path = std.array_list.Managed(u8).init(self.allocator);
    defer cwd_doxa_path.deinit();
    try cwd_doxa_path.appendSlice(clean_name);
    if (!has_extension) try cwd_doxa_path.appendSlice(".doxa");

    if (readFileContentsWithPath(self.allocator, cwd_doxa_path.items)) |data| {
        return data;
    } else |e| {
        err = e;
    }

    // 3. Try the same directory as the current file with potential existing extension
    if (has_extension) {
        var same_dir_exact_path = std.array_list.Managed(u8).init(self.allocator);
        defer same_dir_exact_path.deinit();
        try same_dir_exact_path.appendSlice(current_dir);
        try same_dir_exact_path.appendSlice("/");
        try same_dir_exact_path.appendSlice(clean_name);

        if (readFileContentsWithPath(self.allocator, same_dir_exact_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }
    }

    // 4. Try the same directory as the current file with .doxa extension
    var same_dir_path = std.array_list.Managed(u8).init(self.allocator);
    defer same_dir_path.deinit();
    try same_dir_path.appendSlice(current_dir);
    try same_dir_path.appendSlice("/");
    try same_dir_path.appendSlice(clean_name);
    if (!has_extension) try same_dir_path.appendSlice(".doxa");

    if (readFileContentsWithPath(self.allocator, same_dir_path.items)) |data| {
        return data;
    } else |e| {
        err = e;
    }

    // 5. Try the modules subdirectory (with potential existing extension)
    if (has_extension) {
        var modules_exact_path = std.array_list.Managed(u8).init(self.allocator);
        defer modules_exact_path.deinit();
        try modules_exact_path.appendSlice(current_dir);
        try modules_exact_path.appendSlice("/modules/");
        try modules_exact_path.appendSlice(clean_name);

        if (readFileContentsWithPath(self.allocator, modules_exact_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }
    }

    // 6. Try the modules subdirectory with .doxa extension
    var modules_path = std.array_list.Managed(u8).init(self.allocator);
    defer modules_path.deinit();
    try modules_path.appendSlice(current_dir);
    try modules_path.appendSlice("/modules/");
    try modules_path.appendSlice(clean_name);
    if (!has_extension) try modules_path.appendSlice(".doxa");

    if (readFileContentsWithPath(self.allocator, modules_path.items)) |data| {
        return data;
    } else |e| {
        err = e;
    }

    return error.ModuleNotFound;
}

pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
    const data = try self.loadModuleSourceWithPath(module_name);
    return data.source;
}

pub fn extractModuleInfo(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, specific_symbol: ?[]const u8) !ast.ModuleInfo {
    var imports = std.array_list.Managed(ast.ImportInfo).init(self.allocator);
    var name: []const u8 = "";

    // Extract module name from path
    var path_iter = std.mem.splitSequence(u8, module_path, "/");
    var last_part: []const u8 = "";
    while (path_iter.next()) |part| {
        last_part = part;
    }

    if (std.mem.endsWith(u8, last_part, ".doxa")) {
        name = last_part[0 .. last_part.len - 5];
    } else {
        name = last_part;
    }

    // Initialize symbol table for all module symbols (both public and private)
    var module_symbols = std.StringHashMap(ast.ModuleSymbol).init(self.allocator);

    // Initialize symbol table for this module if needed
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
    }

    // If the module AST is a block, process its statements
    if (module_ast.data == .Block) {
        const statements = module_ast.data.Block.statements;

        for (statements, 0..) |stmt, i| {
            switch (stmt.data) {
                .Module => |module| {
                    // If we find a module declaration, use its name instead
                    name = module.name.lexeme;
                    for (module.imports) |import| {
                        try imports.append(import);
                    }
                },
                .Import => |import_info| {
                    // Add import to the imports list
                    try imports.append(import_info);
                },
                .VarDecl => |var_decl| {
                    const is_public = var_decl.is_public;
                    const symbol_name = var_decl.name.lexeme;

                    // Add this symbol to the module's symbol table (public or private)
                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Variable,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    // Also register in the imported symbols if it's public
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        // Only register if we want all symbols or this specific one
                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Variable,
                                .name = symbol_name,
                                .original_module = module_path,
                            });
                        }
                    }
                },
                .FunctionDecl => |func| {
                    const is_public = func.is_public;
                    const symbol_name = func.name.lexeme;

                    // Add this function to the module's symbol table (public or private)
                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Function,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    // Also register in the imported symbols if it's public
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Function,
                                .name = symbol_name,
                                .original_module = module_path,
                                .param_count = @intCast(func.params.len),
                                .return_type_info = func.return_type_info,
                            });
                        }
                    }
                },
                .Expression => |expr_opt| {
                    if (expr_opt) |expr| {
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.data.StructDecl;
                            const is_public = struct_decl.is_public;
                            const symbol_name = struct_decl.name.lexeme;

                            // Add this struct to the module's symbol table (public or private)
                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Struct,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

                            // Also register in the imported symbols if it's public
                            if (is_public) {
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                                if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                                    try self.imported_symbols.?.put(full_name, .{
                                        .kind = .Struct,
                                        .name = symbol_name,
                                        .original_module = module_path,
                                    });
                                }
                            }
                        } else if (expr.data == .EnumDecl) {
                            const enum_decl = expr.data.EnumDecl;
                            const is_public = enum_decl.is_public;
                            const symbol_name = enum_decl.name.lexeme;

                            // Add this enum to the module's symbol table (public or private)
                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Enum,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

                            // Also register in the imported symbols if it's public
                            if (is_public) {
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                                if (specific_symbol) |specific| {
                                    if (std.mem.eql(u8, specific, enum_decl.name.lexeme)) {
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
                                } else {
                                    // No specific symbol specified, register all
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
                        }
                    }
                },
                else => {}, // Skip other types of statements
            }
        }
    }

    return ast.ModuleInfo{
        .name = name,
        .imports = try imports.toOwnedSlice(),
        .ast = module_ast, // Store the full module AST for later reference
        .file_path = module_path, // Store the file path for detecting self-imports
        .symbols = module_symbols, // Store all symbols (public and private)
    };
}

pub fn extractModuleInfoWithParser(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, specific_symbol: ?[]const u8, module_parser: *Parser) !ast.ModuleInfo {
    var imports = std.array_list.Managed(ast.ImportInfo).init(self.allocator);
    var name: []const u8 = "";

    // Extract module name from path
    var path_iter = std.mem.splitSequence(u8, module_path, "/");
    var last_part: []const u8 = "";
    while (path_iter.next()) |part| {
        last_part = part;
    }

    if (std.mem.endsWith(u8, last_part, ".doxa")) {
        name = last_part[0 .. last_part.len - 5];
    } else {
        name = last_part;
    }

    // Extract imports from the module parser's tracking data
    // Try multiple keys since the imports might be recorded under different paths
    var found_imports = false;

    // First try with the exact module path
    if (module_parser.module_imports.get(module_path)) |module_import_map| {
        var import_it = module_import_map.iterator();
        while (import_it.next()) |entry| {
            const alias = entry.key_ptr.*;
            const imported_module_path = entry.value_ptr.*;

            try imports.append(ast.ImportInfo{
                .import_type = .Module,
                .module_path = imported_module_path,
                .namespace_alias = alias,
                .specific_symbols = null,
                .specific_symbol = null,
            });
        }
        found_imports = true;
    }

    // If not found, try with the module parser's current_file path
    if (!found_imports) {
        if (module_parser.module_imports.get(module_parser.current_file)) |module_import_map| {
            var import_it = module_import_map.iterator();
            while (import_it.next()) |entry| {
                const alias = entry.key_ptr.*;
                const imported_module_path = entry.value_ptr.*;

                try imports.append(ast.ImportInfo{
                    .import_type = .Module,
                    .module_path = imported_module_path,
                    .namespace_alias = alias,
                    .specific_symbols = null,
                    .specific_symbol = null,
                });
            }
            found_imports = true;
        }
    }

    // Initialize symbol table for all module symbols (both public and private)
    var module_symbols = std.StringHashMap(ast.ModuleSymbol).init(self.allocator);

    // Initialize symbol table for this module if needed
    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
    }

    // If the module AST is a block, process its statements
    if (module_ast.data == .Block) {
        const statements = module_ast.data.Block.statements;

        for (statements, 0..) |stmt, i| {
            switch (stmt.data) {
                .Module => |module| {
                    // If we find a module declaration, use its name instead
                    name = module.name.lexeme;
                    for (module.imports) |import| {
                        try imports.append(import);
                    }
                },
                .Import => |import_info| {
                    try imports.append(import_info);
                },
                .VarDecl => |var_decl| {
                    const is_public = var_decl.is_public;
                    const symbol_name = var_decl.name.lexeme;

                    // Add this symbol to the module's symbol table (public or private)
                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Variable,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    // Also register in the imported symbols if it's public
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        // Only register if we want all symbols or this specific one
                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Variable,
                                .name = symbol_name,
                                .original_module = module_path,
                            });
                        }
                    }
                },
                .FunctionDecl => |func| {
                    const is_public = func.is_public;
                    const symbol_name = func.name.lexeme;

                    // Add this function to the module's symbol table (public or private)
                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Function,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    // Also register in the imported symbols if it's public
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Function,
                                .name = symbol_name,
                                .original_module = module_path,
                                .param_count = @intCast(func.params.len),
                                .return_type_info = func.return_type_info,
                            });
                        }
                    }
                },
                .Expression => |expr_opt| {
                    if (expr_opt) |expr| {
                        if (expr.data == .StructDecl) {
                            const struct_decl = expr.data.StructDecl;
                            const is_public = struct_decl.is_public;
                            const symbol_name = struct_decl.name.lexeme;

                            // Add this struct to the module's symbol table (public or private)
                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Struct,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

                            // Also register in the imported symbols if it's public
                            if (is_public) {
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                                if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
                                    try self.imported_symbols.?.put(full_name, .{
                                        .kind = .Struct,
                                        .name = symbol_name,
                                        .original_module = module_path,
                                    });
                                }
                            }
                        }
                    }
                },
                .EnumDecl => |enum_decl| {
                    const is_public = enum_decl.is_public;
                    const symbol_name = enum_decl.name.lexeme;

                    // Add this enum to the module's symbol table (public or private)
                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Enum,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    // Also register in the imported symbols if it's public
                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
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
                else => {}, // Skip other types of statements
            }
        }
    }

    return ast.ModuleInfo{
        .name = name,
        .imports = try imports.toOwnedSlice(),
        .ast = module_ast, // Store the full module AST for later reference
        .file_path = module_path, // Store the file path for detecting self-imports
        .symbols = module_symbols, // Store all symbols (public and private)
    };
}

pub fn recordModuleImport(self: *Parser, importer_path: []const u8, alias: []const u8, imported_path: []const u8) !void {
    // Get or create the mapping for this module
    var module_aliases = try self.module_imports.getOrPut(importer_path);
    if (!module_aliases.found_existing) {
        module_aliases.value_ptr.* = std.StringHashMap([]const u8).init(self.allocator);
    }

    // Record that this module imports the other module under this alias
    try module_aliases.value_ptr.put(alias, imported_path);
}
