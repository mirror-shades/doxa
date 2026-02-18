const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");
const ModuleInfo = ast.ModuleInfo;
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const import_parser = @import("import_parser.zig");
const inline_zig = @import("inline_zig.zig");

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
    const normalized_path = try normalizeModulePath(self, module_name);
    defer self.allocator.free(normalized_path);

    if (self.module_resolution_status.get(normalized_path)) |status| {
        switch (status) {
            .IN_PROGRESS => {
                return self.reportCircularImport(normalized_path);
            },
            .COMPLETED => {
                return self.module_cache.get(normalized_path).?;
            },
            .NOT_STARTED => {
                unreachable;
            },
        }
    }

    if (self.module_cache.get(normalized_path)) |info| {
        return info;
    }

    const status_key = try self.allocator.dupe(u8, normalized_path);
    try self.module_resolution_status.put(status_key, .IN_PROGRESS);

    const current_file_copy = try self.allocator.dupe(u8, self.current_file);
    try self.import_stack.append(.{
        .module_path = try self.allocator.dupe(u8, normalized_path),
        .imported_from = if (self.import_stack.items.len > 0) current_file_copy else null,
    });

    defer {
        _ = self.import_stack.pop();
    }

    const module_data = self.loadModuleSourceWithPath(module_name) catch |err| {
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

    var module_lexer = try LexicalAnalyzer.init(self.allocator, module_data.source, module_data.resolved_path, self.reporter);
    defer module_lexer.deinit();

    try module_lexer.initKeywords();
    const tokens = module_lexer.lexTokens() catch |err| {
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

    const module_uri = try self.reporter.ensureFileUri(module_data.resolved_path);
    var new_parser = Parser.init(self.allocator, tokens.items, module_data.resolved_path, module_uri, self.reporter);

    new_parser.module_resolution_status = std.StringHashMap(ModuleResolutionStatus).init(self.allocator);
    var it = self.module_resolution_status.iterator();
    while (it.next()) |entry| {
        if (!std.mem.eql(u8, entry.key_ptr.*, normalized_path)) {
            try new_parser.module_resolution_status.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }
    new_parser.import_stack = try self.import_stack.clone();

    const module_statements = new_parser.execute() catch |err| {
        _ = self.module_resolution_status.remove(normalized_path);
        return err;
    };

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

    const info = try extractModuleInfoWithParser(self, module_block, module_data.resolved_path, null, &new_parser);

    if (self.module_resolution_status.getPtr(normalized_path)) |status_ptr| {
        status_ptr.* = .COMPLETED;
    } else {
        const completed_key = try self.allocator.dupe(u8, normalized_path);
        try self.module_resolution_status.put(completed_key, .COMPLETED);
    }

    const cache_key = try self.allocator.dupe(u8, normalized_path);
    try self.module_cache.put(cache_key, info);

    return info;
}

fn normalizeModulePath(self: *Parser, module_path: []const u8) ![]const u8 {
    var clean_path = module_path;
    if (std.mem.startsWith(u8, clean_path, "./")) {
        clean_path = clean_path[2..];
    }

    if (!std.mem.endsWith(u8, clean_path, ".doxa")) {
        return try std.fmt.allocPrint(self.allocator, "{s}.doxa", .{clean_path});
    }

    return try self.allocator.dupe(u8, clean_path);
}

fn reportCircularImport(self: *Parser, current_module: []const u8) ErrorList!ast.ModuleInfo {
    var error_msg = std.array_list.Managed(u8).init(self.allocator);
    defer error_msg.deinit();

    try error_msg.appendSlice("Circular import detected:\n");

    for (self.import_stack.items) |entry| {
        try error_msg.appendSlice("  ");
        try error_msg.appendSlice(entry.module_path);
        try error_msg.appendSlice(" imports\n");
    }

    try error_msg.appendSlice("  ");
    try error_msg.appendSlice(current_module);
    try error_msg.appendSlice(" (circular dependency)\n");

    self.reporter.reportCompileError(null, null, "{s}", .{error_msg.items});

    return error.CircularImport;
}

const ModuleData = struct {
    source: []const u8,
    resolved_path: []const u8,
};

fn isReservedStdModule(module_name: []const u8) bool {
    return std.mem.eql(u8, module_name, "std");
}

fn readModuleDataFromFile(alloc: std.mem.Allocator, file_path: []const u8) !ModuleData {
    var file = if (std.fs.path.isAbsolute(file_path))
        try std.fs.openFileAbsolute(file_path, .{})
    else
        try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    if (file_size > std.math.maxInt(usize)) {
        return error.FileTooLarge;
    }
    const size: usize = @intCast(file_size);
    const buffer = try alloc.alloc(u8, size);
    errdefer alloc.free(buffer);

    const bytes_read = try file.readAll(buffer);
    if (bytes_read != size) {
        alloc.free(buffer);
        return error.IncompleteRead;
    }

    const path_copy = try alloc.dupe(u8, file_path);

    return ModuleData{
        .source = buffer,
        .resolved_path = path_copy,
    };
}

fn tryLoadBundledStdModule(alloc: std.mem.Allocator) !?ModuleData {
    const exe_dir = std.fs.selfExeDirPathAlloc(alloc) catch return null;
    defer alloc.free(exe_dir);

    const bundled_path = try std.fs.path.join(alloc, &.{ exe_dir, "..", "lib", "std", "std.doxa" });
    defer alloc.free(bundled_path);
    if (readModuleDataFromFile(alloc, bundled_path)) |data| {
        return data;
    } else |_| {}

    return null;
}

pub fn loadModuleSourceWithPath(self: *Parser, module_name: []const u8) ErrorList!ModuleData {
    var clean_name = module_name;
    if (std.mem.startsWith(u8, clean_name, "./")) {
        clean_name = clean_name[2..];
    }

    if (isReservedStdModule(clean_name)) {
        if (try tryLoadBundledStdModule(self.allocator)) |data| {
            return data;
        }
        return error.ModuleNotFound;
    }

    const has_doxa_ext = std.mem.endsWith(u8, clean_name, ".doxa");
    const has_extension = has_doxa_ext;

    const current_dir = std.fs.path.dirname(self.current_file) orelse ".";

    const readFileContentsWithPath = readModuleDataFromFile;

    var err: anyerror = error.FileNotFound;

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

    var cwd_doxa_path = std.array_list.Managed(u8).init(self.allocator);
    defer cwd_doxa_path.deinit();
    try cwd_doxa_path.appendSlice(clean_name);
    if (!has_extension) try cwd_doxa_path.appendSlice(".doxa");

    if (readFileContentsWithPath(self.allocator, cwd_doxa_path.items)) |data| {
        return data;
    } else |e| {
        err = e;
    }

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

    var module_symbols = std.StringHashMap(ast.ModuleSymbol).init(self.allocator);

    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
    }

    if (module_ast.data == .Block) {
        const statements = module_ast.data.Block.statements;

        for (statements, 0..) |stmt, i| {
            switch (stmt.data) {
                .Module => |module| {
                    name = module.name.lexeme;
                    for (module.imports) |import| {
                        try imports.append(import);
                    }
                },
                .ZigDecl => |zig_decl| {
                    const sigs = try inline_zig.sanitizeAndExtract(self.allocator, zig_decl.source);
                    defer inline_zig.deinitSigsShallow(self.allocator, sigs);

                    for (sigs) |sig| {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ zig_decl.name.lexeme, sig.name });
                        if (!self.imported_symbols.?.contains(full_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Function,
                                .name = sig.name,
                                .original_module = module_path,
                                .param_count = @intCast(sig.param_types.len),
                                .param_types = sig.param_types,
                                .return_type_info = sig.return_type,
                            });
                        }
                    }
                },
                .Import => |import_info| {
                    try imports.append(import_info);
                },
                .VarDecl => |var_decl| {
                    const is_public = var_decl.is_public;
                    const symbol_name = var_decl.name.lexeme;

                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Variable,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

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

                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Function,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

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

                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Struct,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

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

                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Enum,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

                            if (is_public) {
                                const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

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
                        }
                    }
                },
                else => {},
            }
        }
    }

    return ast.ModuleInfo{
        .name = name,
        .imports = try imports.toOwnedSlice(),
        .ast = module_ast,
        .file_path = module_path,
        .symbols = module_symbols,
    };
}

pub fn extractModuleInfoWithParser(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, specific_symbol: ?[]const u8, module_parser: *Parser) !ast.ModuleInfo {
    var imports = std.array_list.Managed(ast.ImportInfo).init(self.allocator);
    var name: []const u8 = "";

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

    var found_imports = false;

    if (module_parser.module_imports.get(module_path)) |module_import_map| {
        var import_it = module_import_map.iterator();
        while (import_it.next()) |entry| {
            const alias = entry.key_ptr.*;
            const import_entry = entry.value_ptr.*;

            try imports.append(ast.ImportInfo{
                .import_type = .Module,
                .module_path = import_entry.imported_path,
                .namespace_alias = alias,
                .specific_symbols = null,
                .specific_symbol = null,
                .is_public = import_entry.is_public,
            });
        }
        found_imports = true;
    }

    if (!found_imports) {
        if (module_parser.module_imports.get(module_parser.current_file)) |module_import_map| {
            var import_it = module_import_map.iterator();
            while (import_it.next()) |entry| {
                const alias = entry.key_ptr.*;
                const import_entry = entry.value_ptr.*;

                try imports.append(ast.ImportInfo{
                    .import_type = .Module,
                    .module_path = import_entry.imported_path,
                    .namespace_alias = alias,
                    .specific_symbols = null,
                    .specific_symbol = null,
                    .is_public = import_entry.is_public,
                });
            }
            found_imports = true;
        }
    }

    var module_symbols = std.StringHashMap(ast.ModuleSymbol).init(self.allocator);

    if (self.imported_symbols == null) {
        self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
    }

    if (module_ast.data == .Block) {
        const statements = module_ast.data.Block.statements;

        for (statements, 0..) |stmt, i| {
            switch (stmt.data) {
                .Module => |module| {
                    name = module.name.lexeme;
                    for (module.imports) |import| {
                        try imports.append(import);
                    }
                },
                .ZigDecl => |zig_decl| {
                    const sigs = try inline_zig.sanitizeAndExtract(self.allocator, zig_decl.source);
                    defer inline_zig.deinitSigsShallow(self.allocator, sigs);

                    for (sigs) |sig| {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ zig_decl.name.lexeme, sig.name });
                        if (!self.imported_symbols.?.contains(full_name)) {
                            try self.imported_symbols.?.put(full_name, .{
                                .kind = .Function,
                                .name = sig.name,
                                .original_module = module_path,
                                .param_count = @intCast(sig.param_types.len),
                                .param_types = sig.param_types,
                                .return_type_info = sig.return_type,
                            });
                        }
                    }
                },
                .Import => |import_info| {
                    try imports.append(import_info);
                },
                .VarDecl => |var_decl| {
                    const is_public = var_decl.is_public;
                    const symbol_name = var_decl.name.lexeme;

                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Variable,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

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

                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Function,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

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

                            try module_symbols.put(symbol_name, .{
                                .name = symbol_name,
                                .kind = .Struct,
                                .is_public = is_public,
                                .stmt_index = i,
                            });

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

                    try module_symbols.put(symbol_name, .{
                        .name = symbol_name,
                        .kind = .Enum,
                        .is_public = is_public,
                        .stmt_index = i,
                    });

                    if (is_public) {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, symbol_name });

                        if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, symbol_name)) {
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
                else => {},
            }
        }
    }

    return ast.ModuleInfo{
        .name = name,
        .imports = try imports.toOwnedSlice(),
        .ast = module_ast,
        .file_path = module_path,
        .symbols = module_symbols,
    };
}

pub fn recordModuleImport(self: *Parser, importer_path: []const u8, alias: []const u8, imported_path: []const u8, is_public: bool) !void {
    var module_aliases = try self.module_imports.getOrPut(importer_path);
    if (!module_aliases.found_existing) {
        module_aliases.value_ptr.* = std.StringHashMap(@import("parser_types.zig").ModuleImportEntry).init(self.allocator);
    }

    try module_aliases.value_ptr.put(alias, .{
        .imported_path = imported_path,
        .is_public = is_public,
    });
}
