const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const Variable = @import("../../utils/memory.zig").Variable;
const import_parser = @import("../../parser/import_parser.zig");
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const CustomTypeInfo = @import("semantic.zig").SemanticAnalyzer.CustomTypeInfo;
const Memory = @import("../../utils/memory.zig");
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const HIREnum = @import("../../codegen/hir/soxa_types.zig").HIREnum;

/// Helper: structural equality for TypeInfo (avoid collapsing by .base only)
fn typesEqual(a: *const ast.TypeInfo, b: *const ast.TypeInfo) bool {
    if (a.base != b.base) return false;

    switch (a.base) {
        .Int, .Byte, .Float, .String, .Tetra, .Nothing => return true,

        .Enum => {
            // For now, just compare custom type names if available
            if (a.custom_type == null or b.custom_type == null) return false;
            return std.mem.eql(u8, a.custom_type.?, b.custom_type.?);
        },

        .Struct => {
            // For now, just compare custom type names if available
            if (a.custom_type == null or b.custom_type == null) return false;
            return std.mem.eql(u8, a.custom_type.?, b.custom_type.?);
        },

        .Custom => {
            // If both have custom_type, they must match
            if (a.custom_type != null and b.custom_type != null) {
                return std.mem.eql(u8, a.custom_type.?, b.custom_type.?);
            }
            // If one has custom_type and the other doesn't, allow it for enum literals
            // This handles cases like Color (enum type) vs .Blue (enum literal)
            return true;
        },

        .Array => {
            // If both have array_type, they must match
            if (a.array_type != null and b.array_type != null) {
                return typesEqual(a.array_type.?, b.array_type.?);
            }
            // If one or both don't have array_type, allow it for array literals
            // This handles cases where array literals don't have element type info
            return true;
        },

        .Map => {
            if (a.map_key_type == null or b.map_key_type == null or a.map_value_type == null or b.map_value_type == null) return false;
            return typesEqual(a.map_key_type.?, b.map_key_type.?) and typesEqual(a.map_value_type.?, b.map_value_type.?);
        },

        .Function => {
            if (a.function_type == null or b.function_type == null) return false;
            const af = a.function_type.?;
            const bf = b.function_type.?;
            if (af.params.len != bf.params.len) return false;
            for (af.params, bf.params) |ap, bp| {
                if (!typesEqual(&ap, &bp)) return false;
            }
            return typesEqual(af.return_type, bf.return_type);
        },

        .Union => {
            // Should be flattened before compare; compare as sets (order-independent)
            if (a.union_type == null or b.union_type == null) return false;
            const au = a.union_type.?;
            const bu = b.union_type.?;
            if (au.types.len != bu.types.len) return false;

            // For each a-member, find a structurally equal b-member
            var matched: usize = 0;
            for (au.types) |amt| {
                var found = false;
                for (bu.types) |bmt| {
                    if (typesEqual(amt, bmt)) {
                        found = true;
                        break;
                    }
                }
                if (found) matched += 1 else return false;
            }
            return matched == au.types.len;
        },
    }
}

/// Helper: canonicalize a slice of *TypeInfo (dedup + stable order)
fn canonicalizeUnion(
    allocator: std.mem.Allocator,
    members: []const *ast.TypeInfo,
) ![]*ast.TypeInfo {
    var list = std.ArrayListUnmanaged(*ast.TypeInfo){};
    defer list.deinit(allocator);

    // dedup (structural)
    outer: for (members) |m| {
        for (list.items) |e| if (typesEqual(e, m)) continue :outer;
        try list.append(allocator, m);
    }

    // stable order by base enum value, with tie-breakers
    std.sort.pdq(*ast.TypeInfo, list.items, {}, struct {
        fn lessThan(_: void, a: *ast.TypeInfo, b: *ast.TypeInfo) bool {
            const ba = @intFromEnum(a.base);
            const bb = @intFromEnum(b.base);
            if (ba != bb) return ba < bb;

            // tie-breakers for deterministic printing
            switch (a.base) {
                .Enum => return std.mem.order(u8, a.custom_type orelse "", b.custom_type orelse "") == .lt,
                .Custom => {
                    if (a.custom_type == null or b.custom_type == null) return false;
                    return std.mem.lessThan(u8, a.custom_type.?, b.custom_type.?);
                },
                .Array => {
                    if (a.array_type == null or b.array_type == null) return false;
                    // Not ideal, but keeps it deterministic: compare base of element
                    return @intFromEnum(a.array_type.?.base) < @intFromEnum(b.array_type.?.base);
                },
                else => return false,
            }
        }
    }.lessThan);

    return try list.toOwnedSlice(allocator);
}

/// Centralized AST→HIR lowering (best-effort; plug IDs if you have them)
fn lowerAstTypeToHIR(self: *SemanticAnalyzer, ti: *const ast.TypeInfo) !HIRType {
    return switch (ti.base) {
        .Int => HIRType.Int,
        .Byte => HIRType.Byte,
        .Float => HIRType.Float,
        .String => HIRType.String,
        .Tetra => HIRType.Tetra,
        .Nothing => HIRType.Nothing,

        .Array => blk: {
            // If your HIRType.Array expects a pointer, create/own it accordingly.
            // Fallback: if you only have HIRType.Array *without payload, adjust here.
            if (ti.array_type) |elem| {
                const elem_hir = try lowerAstTypeToHIR(self, elem);
                const elem_ptr = try self.allocator.create(HIRType);
                elem_ptr.* = elem_hir;
                break :blk HIRType{ .Array = elem_ptr };
            } else {
                // No element info → leave as a generic Array; if you have Error, prefer it.
                break :blk HIRType.Nothing; // or .Error if available
            }
        },

        .Map => blk: {
            if (ti.map_key_type != null and ti.map_value_type != null) {
                const key_hir = try lowerAstTypeToHIR(self, ti.map_key_type.?);
                const val_hir = try lowerAstTypeToHIR(self, ti.map_value_type.?);
                const key_ptr = try self.allocator.create(HIRType);
                key_ptr.* = key_hir;
                const val_ptr = try self.allocator.create(HIRType);
                val_ptr.* = val_hir;
                break :blk HIRType{ .Map = .{ .key = key_ptr, .value = val_ptr } };
            } else {
                // Create generic map with unknown key/value types
                const key_ptr = try self.allocator.create(HIRType);
                key_ptr.* = .Unknown;
                const val_ptr = try self.allocator.create(HIRType);
                val_ptr.* = .Unknown;
                break :blk HIRType{ .Map = .{ .key = key_ptr, .value = val_ptr } };
            }
        },

        .Enum => blk: {
            // For now, use placeholder enum ID
            // TODO: Map to real enum ID if available
            break :blk HIRType{ .Enum = 0 };
        },

        .Custom => blk: {
            // Distinguish Struct vs Enum by consulting self.custom_types
            if (ti.custom_type) |name| {
                if (self.custom_types.get(name)) |ct| {
                    switch (ct.kind) {
                        .Struct => {
                            // If you have struct ids, map here: HIRType{ .Struct = sid }
                            break :blk HIRType{ .Struct = 0 };
                        },
                        .Enum => {
                            // Map to real enum id if you can
                            break :blk HIRType{ .Enum = 0 };
                        },
                    }
                }
            }
            // Unknown custom type → prefer a poison type in HIR if you have one
            break :blk HIRType.Nothing;
        },

        .Function => blk: {
            if (ti.function_type) |ft| {
                // Convert params & return
                var params_list = std.ArrayListUnmanaged(*const HIRType){};
                defer params_list.deinit(self.allocator);
                for (ft.params) |p| {
                    const ph = try lowerAstTypeToHIR(self, &p);
                    const pptr = try self.allocator.create(HIRType);
                    pptr.* = ph;
                    try params_list.append(self.allocator, pptr);
                }
                const ret_h = try lowerAstTypeToHIR(self, ft.return_type);
                const ret_ptr = try self.allocator.create(HIRType);
                ret_ptr.* = ret_h;
                break :blk HIRType{ .Function = .{
                    .params = try params_list.toOwnedSlice(self.allocator),
                    .ret = ret_ptr,
                } };
            }
            break :blk HIRType{ .Function = .{ .params = &[_]*const HIRType{}, .ret = &HIRType{ .Unknown = {} } } };
        },

        .Struct => blk: {
            // If you have struct ids, map here: HIRType{ .Struct = sid }
            break :blk HIRType{ .Struct = 0 };
        },

        .Union => blk: {
            const ut = ti.union_type.?;
            const flat = try flattenUnionType(self, ut);

            // Lower members
            var lowered = std.ArrayListUnmanaged(*const HIRType){};
            defer lowered.deinit(self.allocator);
            for (flat.types) |mt| {
                const mh = try lowerAstTypeToHIR(self, mt);
                const mptr = try self.allocator.create(HIRType);
                mptr.* = mh;
                try lowered.append(self.allocator, mptr);
            }
            // Union expects []const *const HIRType, so use the pointers directly
            break :blk HIRType{ .Union = try lowered.toOwnedSlice(self.allocator) };
        },
    };
}

/// REWRITE: better union flatten
pub fn flattenUnionType(self: *SemanticAnalyzer, union_type: *ast.UnionType) !*ast.UnionType {
    var scratch = std.ArrayListUnmanaged(*ast.TypeInfo){};
    defer scratch.deinit(self.allocator);

    // Gather members (recursively flatten)
    for (union_type.types) |member_type| {
        if (member_type.base == .Union) {
            if (member_type.union_type) |nested_union| {
                const nested_flat = try flattenUnionType(self, nested_union);
                for (nested_flat.types) |nm| try scratch.append(self.allocator, nm);
            } else {
                try scratch.append(self.allocator, member_type);
            }
        } else {
            try scratch.append(self.allocator, member_type);
        }
    }

    // Canonicalize (structural dedup + stable order)
    const unique = try canonicalizeUnion(self.allocator, scratch.items);

    const flattened = try self.allocator.create(ast.UnionType);
    flattened.* = .{
        .types = unique,
        .current_type_index = null, // order changed; don't carry index
    };
    return flattened;
}

/// REWRITE: createUnionType uses the same canonicalization
pub fn createUnionType(self: *SemanticAnalyzer, types: []*ast.TypeInfo) !*ast.TypeInfo {
    // First flatten any unions inside inputs
    var flat = std.ArrayListUnmanaged(*ast.TypeInfo){};
    defer flat.deinit(self.allocator);

    for (types) |ti| {
        if (ti.base == .Union) {
            if (ti.union_type) |u| {
                const f = try flattenUnionType(self, u);
                for (f.types) |m| try flat.append(self.allocator, m);
            } else {
                try flat.append(self.allocator, ti);
            }
        } else {
            try flat.append(self.allocator, ti);
        }
    }

    const unique = try canonicalizeUnion(self.allocator, flat.items);

    if (unique.len == 1) {
        // Single type → not a union
        return unique[0];
    }

    const ut = try self.allocator.create(ast.UnionType);
    ut.* = .{ .types = unique, .current_type_index = null };

    const out = try self.allocator.create(ast.TypeInfo);
    out.* = .{ .base = .Union, .union_type = ut, .is_mutable = false };
    return out;
}

/// REWRITE: unionContainsNothing
pub fn unionContainsNothing(self: *SemanticAnalyzer, union_type_info: ast.TypeInfo) bool {
    _ = self;
    if (union_type_info.base != .Union) return false;
    if (union_type_info.union_type) |u| {
        for (u.types) |mt| if (mt.base == .Nothing) return true;
    }
    return false;
}

/// REWRITE: unifyTypes uses structural checks
pub fn unifyTypes(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
    // If expected is union, actual must be a member (or a subset if it's a union)
    if (expected.base == .Union) {
        if (expected.union_type) |exp_u| {
            if (actual.base == .Union) {
                if (actual.union_type) |act_u| {
                    // Every actual member must be allowed by expected
                    for (act_u.types) |act_m| {
                        var allowed = false;
                        for (exp_u.types) |exp_m| {
                            if (typesEqual(exp_m, act_m)) {
                                allowed = true;
                                break;
                            }
                        }
                        if (!allowed) {
                            // Build expected list (pretty)
                            var list = std.ArrayListUnmanaged(u8){};
                            defer list.deinit(self.allocator);
                            for (exp_u.types, 0..) |m, i| {
                                if (i > 0) list.appendSlice(self.allocator, " | ") catch {};
                                list.appendSlice(self.allocator, @tagName(m.base)) catch {};
                            }
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.TYPE_MISMATCH,
                                "Type mismatch: expected union ({s}), got member of kind {s}",
                                .{ list.items, @tagName(act_m.base) },
                            );
                            self.fatal_error = true;
                            return;
                        }
                    }
                    return;
                }
                // Handle union with null union_type if needed
            } else {
                // Non-union actual must match one member structurally
                for (exp_u.types) |m| {
                    if (typesEqual(m, actual)) return;
                }
                var list = std.ArrayListUnmanaged(u8){};
                defer list.deinit(self.allocator);
                for (exp_u.types, 0..) |m, i| {
                    if (i > 0) list.appendSlice(self.allocator, " | ") catch {};
                    list.appendSlice(self.allocator, @tagName(m.base)) catch {};
                }
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "Type mismatch: expected union ({s}), got {s}",
                    .{ list.items, @tagName(actual.base) },
                );
                self.fatal_error = true;
                return;
            }
        }
    }

    // Non-union expected: structural equality or permitted implicit conversions
    if (!typesEqual(expected, actual)) {
        // Implicit conversions (adjust to taste)
        if (expected.base == .Float and (actual.base == .Int or actual.base == .Byte)) return;
        if (expected.base == .Byte and actual.base == .Int) return;
        if (expected.base == .Int and actual.base == .Byte) return;

        // Allow enum variant literal to enum type
        if (expected.base == .Enum and actual.base == .Enum) {
            if (expected.custom_type != null and actual.custom_type != null and
                std.mem.eql(u8, expected.custom_type.?, actual.custom_type.?)) return;
        }
        // Allow custom/struct compatibility by name (if both named)
        if ((expected.base == .Custom and actual.base == .Custom) and
            expected.custom_type != null and actual.custom_type != null and
            std.mem.eql(u8, expected.custom_type.?, actual.custom_type.?)) return;

        // Otherwise mismatch
        self.reporter.reportCompileError(
            span.location,
            ErrorCode.TYPE_MISMATCH,
            "Type mismatch: expected {s}, got {s}",
            .{ @tagName(expected.base), @tagName(actual.base) },
        );
        self.fatal_error = true;
        return;
    }

    // Complex shape recursion
    switch (expected.base) {
        .Array => {
            if (expected.array_type) |e| if (actual.array_type) |a| try unifyTypes(self, e, a, span);
        },
        .Struct, .Custom => {
            if (expected.struct_fields) |efs| {
                if (actual.struct_fields) |afs| {
                    if (efs.len != afs.len) {
                        self.reporter.reportCompileError(
                            span.location,
                            ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                            "Struct field count mismatch: expected {}, got {}",
                            .{ efs.len, afs.len },
                        );
                        self.fatal_error = true;
                        return;
                    }
                    for (efs, afs) |ef, af| {
                        if (!std.mem.eql(u8, ef.name, af.name)) {
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                                "Struct field name mismatch: expected '{s}', got '{s}'",
                                .{ ef.name, af.name },
                            );
                            self.fatal_error = true;
                            return;
                        }
                        try unifyTypes(self, ef.type_info, af.type_info, span);
                    }
                }
            }
        },
        else => {},
    }
}

/// unchanged
pub fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
    if (self.current_scope) |scope| {
        const result = scope.lookupVariable(name);
        if (result != null) return result;
    }
    if (self.parser) |parser| {
        if (parser.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(name)) |imported_symbol| {
                return createImportedSymbolVariable(self, name, imported_symbol);
            }
        }
        if (parser.module_namespaces.contains(name)) {
            return createModuleNamespaceVariable(self, name);
        }
    }
    return null;
}

pub fn createModuleNamespaceVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
    const type_info = self.allocator.create(ast.TypeInfo) catch return null;
    errdefer self.allocator.destroy(type_info);

    type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false, .custom_type = name };
    const token_type = convertTypeToTokenType(self, type_info.base);

    if (self.current_scope) |scope| {
        const placeholder_value = TokenLiteral{ .nothing = {} };
        const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
        return variable;
    }
    return null;
}

pub fn isModuleNamespace(self: *SemanticAnalyzer, name: []const u8) bool {
    if (self.parser) |parser| {
        if (parser.module_namespaces.contains(name)) return true;

        if (std.mem.indexOfScalar(u8, name, '.')) |dot_idx| {
            const root = name[0..dot_idx];
            if (parser.module_namespaces.contains(root)) return true;
            var it = parser.module_namespaces.iterator();
            while (it.next()) |entry| {
                const mi = entry.value_ptr.*;
                if (std.mem.eql(u8, mi.name, root) or std.mem.eql(u8, mi.file_path, root)) return true;
            }
        }
    }
    return false;
}

/// unchanged domain logic (minor nits left as-is)
pub fn handleModuleFieldAccess(self: *SemanticAnalyzer, module_name: []const u8, field_name: []const u8, span: ast.SourceSpan) !*ast.TypeInfo {
    var type_info = try self.allocator.create(ast.TypeInfo);
    errdefer self.allocator.destroy(type_info);

    if (self.parser) |parser| {
        // Special-case built-in "graphics" virtual submodules
        // Allow alias-based access like `rl.raylib` and `rl.doxa`, and nested like `graphics.raylib.SKYBLUE`
        var root_alias: []const u8 = module_name;
        var subpath: ?[]const u8 = null;
        if (std.mem.indexOfScalar(u8, module_name, '.')) |dot_idx| {
            root_alias = module_name[0..dot_idx];
            subpath = module_name[dot_idx + 1 ..];
        }

        // Look for the field in the module's imported symbols
        if (parser.imported_symbols) |imported_symbols| {
            const full_name = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name }) catch {
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.INTERNAL_ERROR,
                    "Internal error: Could not format module field name",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            };
            defer self.allocator.free(full_name);

            if (imported_symbols.get(full_name)) |imported_symbol| {
                // Return appropriate type based on the imported symbol kind
                switch (imported_symbol.kind) {
                    .Function => {
                        // Try to find the actual function definition to get its return type
                        var found_func_type: ?*ast.FunctionType = null;

                        // Look through module namespaces to find the actual function
                        var it = parser.module_namespaces.iterator();
                        while (it.next()) |entry| {
                            const module_info = entry.value_ptr.*;
                            if (module_info.ast) |module_ast| {
                                if (module_ast.data == .Block) {
                                    const stmts = module_ast.data.Block.statements;
                                    for (stmts) |s| {
                                        switch (s.data) {
                                            .FunctionDecl => |f| {
                                                if (!f.is_public) continue;
                                                if (!std.mem.eql(u8, f.name.lexeme, field_name)) continue;

                                                // Found matching function; construct FunctionType from its params/return
                                                const ft = self.allocator.create(ast.FunctionType) catch break;

                                                // Duplicate param TypeInfos into a flat slice
                                                var params_list = std.array_list.Managed(ast.TypeInfo).init(self.allocator);
                                                errdefer params_list.deinit();
                                                for (f.params) |p| {
                                                    const ti_ptr = if (p.type_expr) |texpr| (self.typeExprToTypeInfo(texpr) catch null) else null;
                                                    const ti = if (ti_ptr) |tmp| tmp.* else ast.TypeInfo{ .base = .Nothing };
                                                    params_list.append(ti) catch break;
                                                }
                                                const params_slice = params_list.toOwnedSlice() catch break;

                                                // Use the actual return type from the function declaration
                                                const ret_ptr = self.allocator.create(ast.TypeInfo) catch break;
                                                ret_ptr.* = f.return_type_info;

                                                ft.* = ast.FunctionType{ .params = params_slice, .return_type = ret_ptr };
                                                found_func_type = ft;
                                                break;
                                            },
                                            else => {},
                                        }
                                    }
                                }
                            }
                            if (found_func_type != null) break;
                        }

                        // Fallback if not found: zero params and nothing return (safer than Int)
                        if (found_func_type == null) {
                            const return_type = self.allocator.create(ast.TypeInfo) catch return type_info;
                            return_type.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                            const function_type = self.allocator.create(ast.FunctionType) catch return type_info;
                            function_type.* = ast.FunctionType{ .params = &[_]ast.TypeInfo{}, .return_type = return_type };
                            found_func_type = function_type;
                        }

                        type_info.* = ast.TypeInfo{
                            .base = .Function,
                            .is_mutable = false,
                            .function_type = found_func_type,
                        };
                    },
                    .Variable => {
                        // Look up the actual variable type from the module's AST
                        var found_var_type: ?ast.TypeInfo = null;

                        // Look through module namespaces to find the actual variable
                        var it = parser.module_namespaces.iterator();
                        while (it.next()) |entry| {
                            const module_info = entry.value_ptr.*;
                            if (module_info.ast) |module_ast| {
                                if (module_ast.data == .Block) {
                                    const stmts = module_ast.data.Block.statements;
                                    for (stmts) |s| {
                                        switch (s.data) {
                                            .VarDecl => |v| {
                                                if (!v.is_public) continue;
                                                if (!std.mem.eql(u8, v.name.lexeme, field_name)) continue;

                                                // Found matching variable; use its type info
                                                found_var_type = v.type_info;
                                                break;
                                            },
                                            else => {},
                                        }
                                    }
                                }
                            }
                            if (found_var_type != null) break;
                        }

                        if (found_var_type) |var_type| {
                            type_info.* = var_type;
                        } else {
                            // Fallback: default to Int
                            type_info.* = ast.TypeInfo{ .base = .Int, .is_mutable = false };
                        }
                    },
                    .Struct => type_info.* = ast.TypeInfo{ .base = .Struct, .is_mutable = false },
                    .Enum => type_info.* = ast.TypeInfo{ .base = .Enum, .is_mutable = false },
                    .Type => type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false },
                    .Import => type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false },
                }
                return type_info;
            }
        }
    }

    // Field not found in module
    self.reporter.reportCompileError(
        span.location,
        ErrorCode.FIELD_NOT_FOUND,
        "Field '{s}' not found in module '{s}'",
        .{ field_name, module_name },
    );
    self.fatal_error = true;
    type_info.base = .Nothing;
    return error.NotImplemented;
}

pub fn createImportedSymbolVariable(self: *SemanticAnalyzer, name: []const u8, imported_symbol: import_parser.ImportedSymbol) ?*Variable {
    // Create a TypeInfo based on the imported symbol kind
    const type_info = self.allocator.create(ast.TypeInfo) catch return null;
    errdefer self.allocator.destroy(type_info);

    const ti = switch (imported_symbol.kind) {
        .Function => blk: {
            // Build a best-effort function type using imported metadata when available;
            // fall back to scanning module_namespaces.
            var built_func_type: ?*ast.FunctionType = null;

            // If import captured param_count/return_type_info, prefer that
            const maybe_param_count = imported_symbol.param_count;
            const maybe_return_info = imported_symbol.return_type_info;
            if (maybe_param_count != null or maybe_return_info != null) {
                const pc: usize = if (maybe_param_count) |x| @intCast(x) else 0;
                if (self.allocator.alloc(ast.TypeInfo, pc) catch null) |params_buf| {
                    // Default parameter types to Nothing when unknown
                    for (params_buf) |*ti| ti.* = ast.TypeInfo{ .base = .Nothing };
                    if (self.allocator.create(ast.TypeInfo) catch null) |ret_ptr| {
                        ret_ptr.* = if (maybe_return_info) |ri| ri else ast.TypeInfo{ .base = .Nothing };
                        if (self.allocator.create(ast.FunctionType) catch null) |ft_ptr| {
                            ft_ptr.* = ast.FunctionType{ .params = params_buf, .return_type = ret_ptr };
                            built_func_type = ft_ptr;
                        }
                    }
                }
            }

            if (built_func_type == null) {
                if (self.parser) |parser| {
                    var it = parser.module_namespaces.iterator();
                    while (it.next()) |entry| {
                        const module_info = entry.value_ptr.*;
                        if (module_info.ast) |module_ast| {
                            if (module_ast.data == .Block) {
                                const stmts = module_ast.data.Block.statements;
                                for (stmts) |s| {
                                    switch (s.data) {
                                        .FunctionDecl => |f| {
                                            if (!f.is_public) continue;
                                            if (!std.mem.eql(u8, f.name.lexeme, name)) continue;
                                            // Found matching function; construct FunctionType from its params/return
                                            const ft = self.allocator.create(ast.FunctionType) catch break;
                                            // Duplicate param TypeInfos into a flat slice as FunctionType expects []TypeInfo
                                            var params_list = std.array_list.Managed(ast.TypeInfo).init(self.allocator);
                                            errdefer params_list.deinit();
                                            for (f.params) |p| {
                                                const ti_ptr = if (p.type_expr) |texpr| (self.typeExprToTypeInfo(texpr) catch null) else null;
                                                const ti = if (ti_ptr) |tmp| tmp.* else ast.TypeInfo{ .base = .Nothing };
                                                params_list.append(ti) catch break;
                                            }
                                            const params_slice = params_list.toOwnedSlice() catch break;
                                            const ret_ptr = self.allocator.create(ast.TypeInfo) catch break;
                                            ret_ptr.* = f.return_type_info;
                                            ft.* = ast.FunctionType{ .params = params_slice, .return_type = ret_ptr };
                                            built_func_type = ft;
                                            break;
                                        },
                                        else => {},
                                    }
                                }
                            }
                        }
                        if (built_func_type != null) break;
                    }
                }
            }

            // Fallback if not found: zero params and nothing return (safer than Int)
            if (built_func_type == null) {
                const return_type = self.allocator.create(ast.TypeInfo) catch return null;
                return_type.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                const function_type = self.allocator.create(ast.FunctionType) catch return null;
                function_type.* = ast.FunctionType{ .params = &[_]ast.TypeInfo{}, .return_type = return_type };
                built_func_type = function_type;
            }

            break :blk ast.TypeInfo{ .base = .Function, .is_mutable = false, .function_type = built_func_type };
        },
        .Variable => ast.TypeInfo{ .base = .Int, .is_mutable = false },
        .Struct => ast.TypeInfo{ .base = .Struct, .is_mutable = false },
        .Enum => ast.TypeInfo{ .base = .Enum, .is_mutable = false },
        .Type => ast.TypeInfo{ .base = .Custom, .is_mutable = false },
        .Import => ast.TypeInfo{ .base = .Custom, .is_mutable = false },
    };

    type_info.* = ti;

    // Convert TypeInfo to TokenType
    const token_type = convertTypeToTokenType(self, type_info.base);

    // Create a Variable object for the imported symbol using the scope's createValueBinding
    if (self.current_scope) |scope| {
        // Create a placeholder value for the imported symbol
        const placeholder_value = TokenLiteral{ .nothing = {} };

        const variable = scope.createValueBinding(name, placeholder_value, token_type, type_info, false) catch return null;
        return variable;
    }

    return null;
}

pub fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) TokenType {
    _ = self;
    return switch (base_type) {
        .Int => .INT,
        .Float => .FLOAT,
        .String => .STRING,
        .Tetra => .TETRA,
        .Byte => .BYTE,
        .Array => .ARRAY,
        .Function => .FUNCTION,
        .Struct => .STRUCT,
        .Nothing => .NOTHING,
        .Map => .MAP,
        .Custom => .CUSTOM,
        .Enum => .ENUM,
        .Union => .UNION,
    };
}

/// REWRITE: struct fields — use centralized lowering (no Unknown sprinkling)
pub fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
    const already_registered = self.custom_types.contains(struct_name);

    var struct_fields = try self.allocator.alloc(CustomTypeInfo.StructField, fields.len);
    for (fields, 0..) |field, index| {
        var custom_type_name: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            custom_type_name = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        const full_type_info = try self.allocator.create(ast.TypeInfo);
        full_type_info.* = field.type_info.*; // NOTE: shallow copy; deep-copy if needed
        struct_fields[index] = CustomTypeInfo.StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type_info = full_type_info,
            .custom_type_name = custom_type_name,
            .index = @intCast(index),
            .is_public = false,
        };
    }

    if (!already_registered) {
        const custom_type = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, struct_name),
            .kind = .Struct,
            .struct_fields = struct_fields,
        };
        try self.custom_types.put(struct_name, custom_type);
    } else {
        // Update existing placeholder with concrete struct fields and kind
        if (self.custom_types.getPtr(struct_name)) |ct_ptr| {
            ct_ptr.kind = .Struct;
            ct_ptr.struct_fields = struct_fields;
        }
    }

    // Runtime memory registration with proper HIR lowering
    var mem_fields = try self.allocator.alloc(Memory.CustomTypeInfo.StructField, fields.len);
    for (fields, 0..) |field, i| {
        const mapped_hir_type = try lowerAstTypeToHIR(self, field.type_info);
        var ctn: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            ctn = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        mem_fields[i] = Memory.CustomTypeInfo.StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type = mapped_hir_type,
            .custom_type_name = ctn,
            .index = @intCast(i),
        };
    }

    const mem_struct = Memory.CustomTypeInfo{
        .name = try self.allocator.dupe(u8, struct_name),
        .kind = .Struct,
        .enum_variants = null,
        .struct_fields = mem_fields,
    };
    try self.memory.registerCustomType(mem_struct);
}

pub fn registerCustomType(self: *SemanticAnalyzer, type_name: []const u8, kind: CustomTypeInfo.CustomTypeKind) !void {
    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, type_name),
        .kind = kind,
    };
    try self.custom_types.put(type_name, custom_type);
}

pub fn registerEnumType(self: *SemanticAnalyzer, enum_name: []const u8, variants: []const []const u8) !void {
    var enum_variants = try self.allocator.alloc(CustomTypeInfo.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, index| {
        enum_variants[index] = CustomTypeInfo.EnumVariant{
            .name = try self.allocator.dupe(u8, variant_name),
            .index = @intCast(index),
        };
    }

    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, enum_name),
        .kind = .Enum,
        .enum_variants = enum_variants,
    };
    try self.custom_types.put(enum_name, custom_type);

    // Mirror to VM memory
    var mem_enum_variants = try self.allocator.alloc(Memory.CustomTypeInfo.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, i| {
        mem_enum_variants[i] = Memory.CustomTypeInfo.EnumVariant{
            .name = try self.allocator.dupe(u8, variant_name),
            .index = @intCast(i),
        };
    }
    const mem_enum = Memory.CustomTypeInfo{
        .name = try self.allocator.dupe(u8, enum_name),
        .kind = .Enum,
        .enum_variants = mem_enum_variants,
        .struct_fields = null,
    };
    try self.memory.registerCustomType(mem_enum);
}

/// Helper function to get location from AST base
pub fn getLocationFromBase(base: ast.Base) Reporting.Location {
    return base.location();
}
