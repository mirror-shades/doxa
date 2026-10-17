//! Building the editor symbol index from an analyzed document.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Types = @import("../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;
const SemanticAnalyzer = @import("../analysis/semantic/semantic.zig").SemanticAnalyzer;
const StructMethodInfo = @import("../analysis/semantic/semantic.zig").StructMethodInfo;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const model = @import("model.zig");
const render = @import("render.zig");

/// Index what the document can name: the types and namespaces its own
/// record binds, each namespace's public members, and its declarations.
pub fn populate(index: *model.SymbolIndex, semantic: *SemanticAnalyzer, memory_manager: *MemoryManager, statements: []ast.Stmt) void {
    index.clear();

    // AST signatures must land first: `addType` enriches each method with
    // the parameter list collected here.
    collectAstSignatures(index, statements);

    const entry = semantic.graph.record(semantic.entry_module);
    var binding_it = entry.bindings.iterator();
    while (binding_it.next()) |binding| {
        const name = binding.key_ptr.*;
        switch (binding.value_ptr.binding) {
            .symbol => |symbol| {
                if (symbol.kind != .Type) continue;
                const ref = symbol.typeRef();
                const cti = semantic.custom_types.get(ref) orelse continue;
                addType(index, name, cti, semantic.struct_methods.get(ref)) catch continue;
            },
            .namespace => |id| {
                index.addModule(name) catch continue;
                const namespace = semantic.graph.record(id);
                var member_it = namespace.public_bindings.keyIterator();
                while (member_it.next()) |member| addModuleMember(index, name, member.*) catch continue;
            },
        }
    }

    populateSymbolIndexFromScope(index, memory_manager) catch {};

    for (statements) |stmt| {
        populateSymbolEntry(index, stmt) catch {};
    }
}

fn shortName(qualified: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, qualified, '.')) |dot| return qualified[dot + 1 ..];
    return qualified;
}

fn addCallable(
    index: *model.SymbolIndex,
    qualified: []const u8,
    params: []ast.FunctionParam,
    return_type_info: ast.TypeInfo,
) !void {
    const alloc = index.arena.allocator();

    var cached_params = std.array_list.Managed(model.CachedParam).init(alloc);
    for (params) |param| {
        try cached_params.append(.{
            .name = try alloc.dupe(u8, param.name.lexeme),
            .type_text = try render.renderTypeExpr(alloc, param.type_expr),
            .alias = param.is_alias,
        });
    }

    const callable = try alloc.create(model.CachedCallable);
    callable.* = .{
        .name = try alloc.dupe(u8, shortName(qualified)),
        .qualified = try alloc.dupe(u8, qualified),
        .params = try cached_params.toOwnedSlice(),
        .return_type = try render.renderTypeInfo(alloc, return_type_info),
    };
    try index.callables.put(callable.qualified, callable);
}

/// Index a type under the name the document binds it by.
fn addType(index: *model.SymbolIndex, bound_name: []const u8, cti: CustomTypeInfo, methods: ?std.StringHashMap(StructMethodInfo)) !void {
    const alloc = index.arena.allocator();
    const name = try alloc.dupe(u8, bound_name);

    var cached_fields = std.array_list.Managed(model.CachedField).init(alloc);
    if (cti.struct_fields) |fields| {
        for (fields) |field| {
            const type_str = render.packLspTypeString(field.field_type_info);
            try cached_fields.append(.{
                .name = try alloc.dupe(u8, field.name),
                .type_name = try alloc.dupe(u8, type_str),
                .is_public = field.is_public,
            });
        }
    }

    var cached_enum_variants = std.array_list.Managed([]const u8).init(alloc);
    if (cti.enum_variants) |variants| {
        for (variants) |v| {
            try cached_enum_variants.append(try alloc.dupe(u8, v.name));
        }
    }

    var cached_methods = std.array_list.Managed(model.CachedMethod).init(alloc);
    if (methods) |methods_map| {
        var method_it = methods_map.iterator();
        while (method_it.next()) |entry| {
            const method_name = entry.value_ptr.name;
            const qualified = try std.fmt.allocPrint(alloc, "{s}.{s}", .{ cti.ref.name, method_name });
            var params: []model.CachedParam = &.{};
            var return_type: []const u8 = "";
            if (index.callables.get(qualified)) |callable| {
                params = callable.params;
                return_type = callable.return_type;
            }
            try cached_methods.append(.{
                .name = try alloc.dupe(u8, method_name),
                .is_public = entry.value_ptr.is_public,
                .is_static = entry.value_ptr.is_static,
                .params = params,
                .return_type = return_type,
            });
        }
    }

    try index.types.put(name, .{
        .name = name,
        .kind = cti.kind,
        .fields = try cached_fields.toOwnedSlice(),
        .methods = try cached_methods.toOwnedSlice(),
        .enum_variants = try cached_enum_variants.toOwnedSlice(),
    });
}

/// Walks the document AST once and records the parameter lists and return
/// types of every top-level function and struct method. The semantic
/// analyzer only tracks method names, so this is the sole source of user
/// signature data for hover, signature help, and snippets.
fn collectAstSignatures(index: *model.SymbolIndex, statements: []ast.Stmt) void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .FunctionDecl => |f| {
                addCallable(index, f.name.lexeme, f.params, f.return_type_info) catch {};
            },
            .Expression => |maybe_expr| {
                const expr = maybe_expr orelse continue;
                switch (expr.data) {
                    .StructDecl => |struct_decl| {
                        for (struct_decl.methods) |method| {
                            const alloc = index.arena.allocator();
                            const qualified = std.fmt.allocPrint(
                                alloc,
                                "{s}.{s}",
                                .{ struct_decl.name.lexeme, method.name.lexeme },
                            ) catch continue;
                            addCallable(index, qualified, method.params, method.return_type_info) catch {};
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}

fn populateSymbolEntry(index: *model.SymbolIndex, stmt: ast.Stmt) !void {
    const alloc = index.arena.allocator();
    const base = &stmt.base;
    const loc = base.location();
    const entry = model.SymbolEntry{
        .name = undefined,
        .kind = 0,
        .start_line = loc.range.start_line,
        .start_character = loc.range.start_col,
        .end_line = loc.range.end_line,
        .end_character = loc.range.end_col,
    };

    switch (stmt.data) {
        .FunctionDecl => |f| {
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, f.name.lexeme),
                .kind = 12,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .VarDecl => |v| {
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, v.name.lexeme),
                .kind = 13,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .EnumDecl => |e| {
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, e.name.lexeme),
                .kind = 10,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .GroupDecl => |g| {
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, g.name.lexeme),
                .kind = 23,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .Import => |import| {
            if (import.import_type != .Module) return;
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, import.names[0].lexeme),
                .kind = 2,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .ZigDecl => |z| {
            try index.symbols.append(.{
                .name = try alloc.dupe(u8, z.name.lexeme),
                .kind = 2,
                .start_line = entry.start_line,
                .start_character = entry.start_character,
                .end_line = entry.end_line,
                .end_character = entry.end_character,
            });
        },
        .Expression => |maybe_expr| {
            if (maybe_expr) |expr| {
                switch (expr.data) {
                    .StructDecl => |s| {
                        try index.symbols.append(.{
                            .name = try alloc.dupe(u8, s.name.lexeme),
                            .kind = 23,
                            .start_line = entry.start_line,
                            .start_character = entry.start_character,
                            .end_line = entry.end_line,
                            .end_character = entry.end_character,
                        });
                    },
                    .EnumDecl => |e| {
                        try index.symbols.append(.{
                            .name = try alloc.dupe(u8, e.name.lexeme),
                            .kind = 10,
                            .start_line = entry.start_line,
                            .start_character = entry.start_character,
                            .end_line = entry.end_line,
                            .end_character = entry.end_character,
                        });
                    },
                    .GroupDecl => |g| {
                        try index.symbols.append(.{
                            .name = try alloc.dupe(u8, g.name.lexeme),
                            .kind = 23,
                            .start_line = entry.start_line,
                            .start_character = entry.start_character,
                            .end_line = entry.end_line,
                            .end_character = entry.end_character,
                        });
                    },
                    else => {},
                }
            }
        },
        else => {},
    }
}

fn addModuleMember(index: *model.SymbolIndex, module_name: []const u8, member_name: []const u8) !void {
    const alloc = index.arena.allocator();
    const gop = try index.module_members.getOrPut(try alloc.dupe(u8, module_name));
    if (!gop.found_existing) {
        gop.value_ptr.* = std.array_list.Managed([]const u8).init(alloc);
    }
    try gop.value_ptr.append(try alloc.dupe(u8, member_name));
}

fn populateSymbolIndexFromScope(index: *model.SymbolIndex, memory_manager: *MemoryManager) !void {
    const alloc = index.arena.allocator();
    for (memory_manager.scope_pool.items) |scope| {
        if (scope.is_deinited) continue;
        var it = scope.name_map.iterator();
        while (it.next()) |entry| {
            const var_name = entry.key_ptr.*;
            const var_ptr = entry.value_ptr.*;
            if (scope.manager.value_storage.get(var_ptr.storage_id)) |storage| {
                const type_str = render.packLspTypeString(storage.type_info);
                try index.addVariable(var_name, type_str);

                // Inferred declarations carry their position; render a type
                // inlay hint just past the name.
                if (var_ptr.decl_line > 0) {
                    const label = try std.fmt.allocPrint(alloc, ":: {s}", .{render.inlayTypeText(storage.type_info)});
                    try index.inlay_hints.append(.{
                        .line = var_ptr.decl_line - 1,
                        .character = var_ptr.decl_column - 1 + var_name.len,
                        .label = label,
                    });
                }
            }
        }
    }
}
