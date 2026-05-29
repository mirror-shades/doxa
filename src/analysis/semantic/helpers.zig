const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const Variable = @import("../../utils/memory.zig").Variable;
const import_parser = @import("../../parser/import_parser.zig");
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const Token = @import("../../types/token.zig").Token;
const StructTable = @import("../../common/struct_table.zig").StructTable;
const EnumTable = @import("../../common/enum_table.zig").EnumTable;
const GroupTable = @import("../../common/group_table.zig").GroupTable;
const Types = @import("../../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;
const StructField = Types.StructField;
const Memory = @import("../../utils/memory.zig");
const HIRTypeModule = @import("../../codegen/hir/soxa_types.zig");
const HIRType = HIRTypeModule.HIRType;
const UnionId = HIRTypeModule.UnionId;
const StructId = HIRTypeModule.StructId;
const HIREnum = @import("../../codegen/hir/soxa_values.zig").HIREnum;

/// Helper: structural equality for TypeInfo (avoid collapsing by .base only)
pub fn typesEqual(self: *const SemanticAnalyzer, a: *const ast.TypeInfo, b: *const ast.TypeInfo) bool {
    if (a.base != b.base) return false;

    switch (a.base) {
        .Int, .Byte, .Float, .String, .Tetra, .Nothing => return true,

        .Enum => {
            // For now, just compare custom type names if available
            if (a.custom_type == null or b.custom_type == null) return false;
            const a_name = self.resolveTypeAlias(a.custom_type.?);
            const b_name = self.resolveTypeAlias(b.custom_type.?);
            return std.mem.eql(u8, a_name, b_name);
        },

        .Struct => {
            // For now, just compare custom type names if available
            if (a.custom_type == null or b.custom_type == null) return false;
            const a_name = self.resolveTypeAlias(a.custom_type.?);
            const b_name = self.resolveTypeAlias(b.custom_type.?);
            return std.mem.eql(u8, a_name, b_name);
        },

        .Custom => {
            // If both have custom_type, they must match
            if (a.custom_type != null and b.custom_type != null) {
                const a_name = self.resolveTypeAlias(a.custom_type.?);
                const b_name = self.resolveTypeAlias(b.custom_type.?);
                return std.mem.eql(u8, a_name, b_name);
            }
            // If one has custom_type and the other doesn't, allow it for enum literals
            // This handles cases like Color (enum type) vs .Blue (enum literal)
            return true;
        },

        .Array => {
            // If both have array_type, they must match
            if (a.array_type != null and b.array_type != null) {
                return typesEqual(self, a.array_type.?, b.array_type.?);
            }
            // If one or both don't have array_type, allow it for array literals
            // This handles cases where array literals don't have element type info
            return true;
        },

        .Map => {
            if (a.map_key_type == null or b.map_key_type == null or a.map_value_type == null or b.map_value_type == null) return false;
            return typesEqual(self, a.map_key_type.?, b.map_key_type.?) and typesEqual(self, a.map_value_type.?, b.map_value_type.?);
        },

        .Function => {
            if (a.function_type == null or b.function_type == null) return false;
            const af = a.function_type.?;
            const bf = b.function_type.?;
            if (af.params.len != bf.params.len) return false;
            for (af.params, bf.params) |ap, bp| {
                if (!typesEqual(self, &ap, &bp)) return false;
            }
            return typesEqual(self, af.return_type, bf.return_type);
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
                    if (typesEqual(self, amt, bmt)) {
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

fn typeLabel(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |name| {
        return name;
    }
    return @tagName(type_info.base);
}

fn typeMatchesUnionMember(self: *const SemanticAnalyzer, exp_member: *const ast.TypeInfo, actual_member: *const ast.TypeInfo) bool {
    if (typesEqual(self, exp_member, actual_member)) return true;
    // Allow group member widening: if exp_member is a group, check membership
    if (exp_member.base == .Custom and exp_member.custom_type != null) {
        if (self.custom_types.get(exp_member.custom_type.?)) |ct| {
            if (ct.kind == .Group) {
                return typeIsGroupMember(self, exp_member.custom_type.?, actual_member);
            }
        }
    }
    return false;
}

fn typeIsGroupMember(self: *const SemanticAnalyzer, group_name: []const u8, actual: *const ast.TypeInfo) bool {
    const group_id = self.group_table.getIdByName(group_name) orelse return false;
    const members = self.group_table.members(group_id) orelse return false;

    const actual_name = if (actual.custom_type) |n| self.resolveTypeAlias(n) else return false;

    for (members) |member| {
        const member_type_name = switch (member.kind) {
            .Enum => if (self.enum_table.getName(member.id)) |n| n else continue,
            .Struct => if (self.struct_table.getName(member.id)) |n| n else continue,
            .Group => if (self.group_table.getName(member.id)) |n| n else continue,
        };
        if (std.mem.eql(u8, member_type_name, actual_name)) return true;
        if (std.mem.lastIndexOfScalar(u8, member_type_name, '.')) |dot| {
            if (std.mem.eql(u8, member_type_name[dot + 1 ..], actual_name)) return true;
        }
        if (std.mem.eql(u8, member.qualifier, actual_name)) return true;
    }
    return false;
}

/// Helper: canonicalize a slice of *TypeInfo (dedup + stable order)
fn canonicalizeUnion(
    self: *const SemanticAnalyzer,
    allocator: std.mem.Allocator,
    members: []const *ast.TypeInfo,
) ![]*ast.TypeInfo {
    var list = std.ArrayListUnmanaged(*ast.TypeInfo){};
    defer list.deinit(allocator);

    // dedup (structural)
    outer: for (members) |m| {
        for (list.items) |e| if (typesEqual(self, e, m)) continue :outer;
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

/// Assign or look up a stable id for a canonical union type within this
/// SemanticAnalyzer instance. The key is the canonical ast.UnionType pointer
/// (after flattening/canonicalization), so structurally distinct unions get
/// distinct ids.
pub fn getOrAssignUnionId(self: *SemanticAnalyzer, ut: *ast.UnionType) !UnionId {
    if (self.union_ids.get(ut)) |existing| {
        return existing;
    }
    const id: UnionId = self.next_union_id;
    self.next_union_id += 1;
    try self.union_ids.put(ut, id);
    return id;
}

pub fn structIdForName(self: *SemanticAnalyzer, type_name: []const u8) ?StructId {
    const resolved = self.resolveTypeAlias(type_name);
    return self.struct_table.getIdByName(resolved);
}

pub fn structIdFromTypeInfo(self: *SemanticAnalyzer, ti: *const ast.TypeInfo) ?StructId {
    switch (ti.base) {
        .Struct, .Custom => {
            if (ti.custom_type) |name| {
                const resolved = self.resolveTypeAlias(name);
                if (self.custom_types.get(resolved)) |ct| {
                    if (ct.kind != .Struct) return null;
                }
                return self.struct_table.getIdByName(resolved);
            }
            return null;
        },
        else => return null,
    }
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
            // TODO: Map to real enum ID if/when enum metadata table exists
            break :blk HIRType{ .Enum = 0 };
        },

        .Custom => blk: {
            if (ti.custom_type) |name| {
                const resolved = self.resolveTypeAlias(name);
                if (self.custom_types.get(resolved)) |ct| {
                    switch (ct.kind) {
                        .Struct => {
                            if (structIdForName(self, resolved)) |sid| {
                                break :blk HIRType{ .Struct = sid };
                            }
                            break :blk HIRType{ .Struct = 0 };
                        },
                        .Enum => break :blk HIRType{ .Enum = 0 },
                        .Group => break :blk HIRType{ .Group = self.group_table.getIdByName(resolved) orelse 0 },
                    }
                }
            }
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
            if (structIdFromTypeInfo(self, ti)) |sid| {
                break :blk HIRType{ .Struct = sid };
            }
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
            // Register union type and attach a stable id
            const union_id = try getOrAssignUnionId(self, flat);
            const members_slice = try lowered.toOwnedSlice(self.allocator);
            break :blk HIRType{ .Union = .{ .id = union_id, .members = members_slice } };
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
    const unique = try canonicalizeUnion(self, self.allocator, scratch.items);

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

    const unique = try canonicalizeUnion(self, self.allocator, flat.items);

    if (unique.len == 1) {
        // Single type → not a union
        return unique[0];
    }

    const ut = try self.allocator.create(ast.UnionType);
    ut.* = .{ .types = unique, .current_type_index = null };

    const out = try ast.TypeInfo.createDefault(self.allocator);
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
    // WORKAROUND: If expected is Custom and actual is Union with matching Custom type, allow it
    if (expected.base == .Custom and actual.base == .Union) {
        if (actual.union_type) |union_type| {
            // Check if any union member matches the expected custom type
            for (union_type.types) |member_type| {
                if (member_type.base == .Custom and
                    expected.custom_type != null and
                    member_type.custom_type != null and
                    std.mem.eql(u8, expected.custom_type.?, member_type.custom_type.?))
                {
                    // Found matching custom type in union, accept it
                    return;
                }
            }
        }
    }

    // GROUP WIDENING: If expected is a group, any member type is assignable
    if (expected.base == .Custom and expected.custom_type != null) {
        if (self.custom_types.get(expected.custom_type.?)) |ct| {
            if (ct.kind == .Group) {
                if (typeIsGroupMember(self, expected.custom_type.?, actual)) return;
                // Also allow if actual is a union where every member is a group member
                if (actual.base == .Union) {
                    if (actual.union_type) |act_u| {
                        var all_allowed = true;
                        for (act_u.types) |act_m| {
                            if (!typeIsGroupMember(self, expected.custom_type.?, act_m)) {
                                all_allowed = false;
                                break;
                            }
                        }
                        if (all_allowed) return;
                    }
                }
            }
        }
    }

    // If expected is union, actual must be a member (or a subset if it's a union)
    if (expected.base == .Union) {
        if (expected.union_type) |exp_u| {
            if (actual.base == .Union) {
                if (actual.union_type) |act_u| {
                    // Every actual member must be allowed by expected
                    for (act_u.types) |act_m| {
                        var allowed = false;
                        for (exp_u.types) |exp_m| {
                            if (typeMatchesUnionMember(self, exp_m, act_m)) {
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
                                list.appendSlice(self.allocator, typeLabel(m)) catch {};
                            }
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.TYPE_MISMATCH,
                                "Type mismatch: expected union ({s}), got member of kind {s}",
                                .{ list.items, typeLabel(act_m) },
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
                    if (typeMatchesUnionMember(self, m, actual)) return;
                }
                var list = std.ArrayListUnmanaged(u8){};
                defer list.deinit(self.allocator);
                for (exp_u.types, 0..) |m, i| {
                    if (i > 0) list.appendSlice(self.allocator, " | ") catch {};
                    list.appendSlice(self.allocator, typeLabel(m)) catch {};
                }
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "Type mismatch: expected union ({s}), got {s}",
                    .{ list.items, typeLabel(actual) },
                );
                self.fatal_error = true;
                return;
            }
        }
    }

    // Non-union expected: structural equality or permitted implicit conversions
    if (!typesEqual(self, expected, actual)) {
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
            std.mem.eql(u8, expected.custom_type.?, actual.custom_type.?))
        {
            return;
        }

        // Otherwise mismatch
        self.reporter.reportCompileError(
            span.location,
            ErrorCode.TYPE_MISMATCH,
            "{s} is not assignable to type {s}",
            .{ typeLabel(actual), typeLabel(expected) },
        );
        self.fatal_error = true;
        return;
    }

    // Complex shape recursion
    switch (expected.base) {
        .Array => {
            if (expected.array_type) |e|
                if (actual.array_type) |a|
                    try unifyTypes(self, e, a, span);
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
        const parser_mut: *@import("../../parser/parser_types.zig").Parser = @constCast(parser);
        _ = parser_mut.ensureImportedSymbol(name) catch |err| {
            reportLazyModuleError(self, null, err);
            return null;
        };

        if (parser.imported_symbols) |imported_symbols| {
            if (imported_symbols.get(name)) |imported_symbol| {
                return createImportedSymbolVariable(self, name, imported_symbol);
            }
        }
        _ = parser_mut.ensureModuleNamespace(name) catch |err| {
            reportLazyModuleError(self, null, err);
            return null;
        };
        if (parser.module_namespaces.contains(name)) {
            return createModuleNamespaceVariable(self, name);
        }
    }
    return null;
}

pub fn suggestVariableName(self: *SemanticAnalyzer, name: []const u8) ?[]const u8 {
    var best: ?[]const u8 = null;
    var best_score: usize = std.math.maxInt(usize);

    var scope = self.current_scope;
    while (scope) |s| {
        var it = s.name_map.iterator();
        while (it.next()) |entry| {
            const candidate = entry.key_ptr.*;
            updateBestSuggestion(name, candidate, &best, &best_score);
        }
        scope = s.parent;
    }

    if (self.parser) |parser| {
        if (parser.imported_symbols) |imported_symbols| {
            var import_it = imported_symbols.iterator();
            while (import_it.next()) |entry| {
                const candidate = entry.key_ptr.*;
                updateBestSuggestion(name, candidate, &best, &best_score);
            }
        }

        var module_it = parser.module_namespaces.iterator();
        while (module_it.next()) |entry| {
            const candidate = entry.key_ptr.*;
            updateBestSuggestion(name, candidate, &best, &best_score);
        }
    }

    if (best) |suggested| {
        const max_acceptable = @max(@as(usize, 2), name.len / 3);
        if (best_score <= max_acceptable) {
            return suggested;
        }
    }

    return null;
}

fn updateBestSuggestion(name: []const u8, candidate: []const u8, best: *?[]const u8, best_score: *usize) void {
    if (candidate.len == 0) return;
    if (std.mem.eql(u8, name, candidate)) return;

    const score = nameDistanceScore(name, candidate);
    if (score < best_score.*) {
        best_score.* = score;
        best.* = candidate;
        return;
    }

    if (score == best_score.* and best.* != null) {
        if (std.mem.lessThan(u8, candidate, best.*.?)) {
            best.* = candidate;
        }
    }
}

fn nameDistanceScore(a: []const u8, b: []const u8) usize {
    const min_len = @min(a.len, b.len);
    var mismatches: usize = 0;
    var i: usize = 0;
    while (i < min_len) : (i += 1) {
        const ac = std.ascii.toLower(a[i]);
        const bc = std.ascii.toLower(b[i]);
        if (ac != bc) mismatches += 1;
    }

    const len_penalty = if (a.len > b.len) a.len - b.len else b.len - a.len;
    return mismatches + (len_penalty * 2);
}

pub fn createModuleNamespaceVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
    const type_info = ast.TypeInfo.createDefault(self.allocator) catch return null;
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

fn reportLazyModuleError(self: *SemanticAnalyzer, loc: ?Reporting.Location, err: anyerror) void {
    switch (err) {
        error.ModuleNotFound => {
            self.reporter.reportCompileError(loc, ErrorCode.MODULE_NOT_FOUND, "Module could not be resolved during lazy loading", .{});
            self.fatal_error = true;
        },
        error.CircularImport => {
            self.reporter.reportCompileError(loc, ErrorCode.CIRCULAR_IMPORT, "Circular import detected during lazy loading", .{});
            self.fatal_error = true;
        },
        else => {
            self.reporter.reportCompileError(loc, ErrorCode.INTERNAL_ERROR, "Module resolution failed during lazy loading: {s}", .{@errorName(err)});
            self.fatal_error = true;
        },
    }
}

pub fn isModuleNamespace(self: *SemanticAnalyzer, name: []const u8) bool {
    if (self.parser) |parser| {
        const parser_mut: *@import("../../parser/parser_types.zig").Parser = @constCast(parser);
        _ = parser_mut.ensureModuleNamespace(name) catch |err| {
            reportLazyModuleError(self, null, err);
            return false;
        };
        if (parser.module_namespaces.contains(name)) return true;

        if (std.mem.indexOfScalar(u8, name, '.')) |dot_idx| {
            const root = name[0..dot_idx];
            _ = parser_mut.ensureModuleNamespace(root) catch |err| {
                reportLazyModuleError(self, null, err);
                return false;
            };
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
    var type_info = try ast.TypeInfo.createDefault(self.allocator);
    errdefer self.allocator.destroy(type_info);

    if (self.parser) |parser| {
        const parser_mut: *@import("../../parser/parser_types.zig").Parser = @constCast(parser);
        _ = parser_mut.ensureModuleNamespace(module_name) catch |err| {
            reportLazyModuleError(self, span.location, err);
            return err;
        };
        if (parser.module_namespaces.get(module_name)) |module_info| {
            for (module_info.imports) |import_info| {
                if (!import_info.is_public or import_info.import_type != .Module) continue;
                if (import_info.namespace_alias) |alias| {
                    if (std.mem.eql(u8, alias, field_name)) {
                        _ = parser_mut.ensureNestedModuleNamespace(module_name, field_name) catch |err| {
                            reportLazyModuleError(self, span.location, err);
                            return err;
                        };
                        const qualified = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name });
                        type_info.* = ast.TypeInfo{
                            .base = .Custom,
                            .is_mutable = false,
                            .custom_type = qualified,
                        };
                        return type_info;
                    }
                }
            }
        }

        const nested_name = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name }) catch null;
        if (nested_name) |qualified| {
            defer self.allocator.free(qualified);
            _ = parser_mut.ensureNestedModuleNamespace(module_name, field_name) catch |err| {
                reportLazyModuleError(self, span.location, err);
                return err;
            };
            if (parser.module_namespaces.contains(qualified)) {
                const owned = try self.allocator.dupe(u8, qualified);
                type_info.* = ast.TypeInfo{
                    .base = .Custom,
                    .is_mutable = false,
                    .custom_type = owned,
                };
                return type_info;
            }
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
                        // Prefer imported metadata when present (works for inline zig modules too).
                        if (imported_symbol.param_count != null or imported_symbol.return_type_info != null) {
                            const pc: usize = if (imported_symbol.param_count) |x| @intCast(x) else 0;
                            var params_buf = try self.allocator.alloc(ast.TypeInfo, pc);
                            if (imported_symbol.param_types) |pts| {
                                const n = @min(pts.len, params_buf.len);
                                @memcpy(params_buf[0..n], pts[0..n]);
                                // Fill remainder with Nothing if needed
                                for (params_buf[n..]) |*ti| ti.* = ast.TypeInfo{ .base = .Nothing };
                            } else {
                                for (params_buf) |*ti| ti.* = ast.TypeInfo{ .base = .Nothing };
                            }
                            const ret_ptr = try ast.TypeInfo.createDefault(self.allocator);
                            ret_ptr.* = if (imported_symbol.return_type_info) |ri| ri else ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                            const ft_ptr = try self.allocator.create(ast.FunctionType);
                            ft_ptr.* = ast.FunctionType{ .params = params_buf, .return_type = ret_ptr };
                            type_info.* = ast.TypeInfo{ .base = .Function, .is_mutable = false, .function_type = ft_ptr };
                            return type_info;
                        }

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
                                                const ret_ptr = ast.TypeInfo.createDefault(self.allocator) catch break;
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
                            const return_type = ast.TypeInfo.createDefault(self.allocator) catch return type_info;
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
                            // Fallback: unknown imported variable type should not coerce to int.
                            type_info.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                        }
                    },
                    .Struct => type_info.* = ast.TypeInfo{ .base = .Struct, .is_mutable = false },
                    .Enum => type_info.* = ast.TypeInfo{ .base = .Enum, .is_mutable = false },
                    .Group => type_info.* = ast.TypeInfo{ .base = .Custom, .is_mutable = false },
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
    const type_info = ast.TypeInfo.createDefault(self.allocator) catch return null;
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
                    if (ast.TypeInfo.createDefault(self.allocator) catch null) |ret_ptr| {
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
                                            const ret_ptr = ast.TypeInfo.createDefault(self.allocator) catch break;
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
                const return_type = ast.TypeInfo.createDefault(self.allocator) catch return null;
                return_type.* = ast.TypeInfo{ .base = .Nothing, .is_mutable = false };
                const function_type = self.allocator.create(ast.FunctionType) catch return null;
                function_type.* = ast.FunctionType{ .params = &[_]ast.TypeInfo{}, .return_type = return_type };
                built_func_type = function_type;
            }

            break :blk ast.TypeInfo{ .base = .Function, .is_mutable = false, .function_type = built_func_type };
        },
        .Variable => ast.TypeInfo{ .base = .Nothing, .is_mutable = false },
        .Struct => ast.TypeInfo{ .base = .Struct, .is_mutable = false },
        .Enum => ast.TypeInfo{ .base = .Enum, .is_mutable = false },
        .Group => ast.TypeInfo{ .base = .Custom, .is_mutable = false },
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

pub fn registerStructType(self: *SemanticAnalyzer, struct_name: []const u8, fields: []const ast.StructFieldType) !void {
    const already_registered = self.custom_types.contains(struct_name);

    var struct_fields = try self.allocator.alloc(StructField, fields.len);
    for (fields, 0..) |field, index| {
        var custom_type_name: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            custom_type_name = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        const full_type_info = try ast.TypeInfo.createDefault(self.allocator);
        full_type_info.* = field.type_info.*; // NOTE: shallow copy; deep-copy if needed
        struct_fields[index] = StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type_info = full_type_info,
            .custom_type_name = custom_type_name,
            .index = @intCast(index),
            .is_public = field.is_public,
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

    var table_inputs = try self.allocator.alloc(StructTable.FieldInput, fields.len);
    defer self.allocator.free(table_inputs);
    for (fields, 0..) |field, idx| {
        table_inputs[idx] = .{
            .name = field.name,
            .type_info = field.type_info,
        };
    }
    const struct_id = try self.struct_table.registerStruct(struct_name, table_inputs);

    // Runtime memory registration with proper HIR lowering
    var mem_fields = try self.allocator.alloc(StructField, fields.len);
    for (fields, 0..) |field, i| {
        const mapped_hir_type = try lowerAstTypeToHIR(self, field.type_info);
        var ctn: ?[]const u8 = null;
        if (field.type_info.base == .Custom and field.type_info.custom_type != null) {
            ctn = try self.allocator.dupe(u8, field.type_info.custom_type.?);
        }
        mem_fields[i] = StructField{
            .name = try self.allocator.dupe(u8, field.name),
            .field_type_info = field.type_info,
            .custom_type_name = ctn,
            .index = @intCast(i),
            .is_public = field.is_public,
        };

        self.struct_table.setFieldHIRType(struct_id, @intCast(i), mapped_hir_type);
        if (structIdFromTypeInfo(self, field.type_info)) |nested_struct_id| {
            self.struct_table.setNestedStructId(struct_id, @intCast(i), nested_struct_id);
        }
    }

    const mem_struct = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, struct_name),
        .kind = .Struct,
        .enum_variants = null,
        .struct_fields = mem_fields,
    };
    try self.memory.registerCustomType(mem_struct);
}

pub fn registerCustomType(self: *SemanticAnalyzer, type_name: []const u8, kind: Types.CustomTypeKind) !void {
    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, type_name),
        .kind = kind,
    };
    try self.custom_types.put(type_name, custom_type);
}

pub fn registerEnumType(self: *SemanticAnalyzer, enum_name: []const u8, variants: []const []const u8) !void {
    var enum_variants = try self.allocator.alloc(Types.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, index| {
        enum_variants[index] = Types.EnumVariant{
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
    var mem_enum_variants = try self.allocator.alloc(Types.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, i| {
        mem_enum_variants[i] = Types.EnumVariant{
            .name = try self.allocator.dupe(u8, variant_name),
            .index = @intCast(i),
        };
    }
    const mem_enum = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, enum_name),
        .kind = .Enum,
        .enum_variants = mem_enum_variants,
        .struct_fields = null,
    };
    try self.memory.registerCustomType(mem_enum);

    // Also mirror into the global EnumTable so later stages (HIR/LLVM/VM)
    // can refer to enums by ID in a uniform way, just like structs.
    if (@hasField(@TypeOf(self.*), "enum_table")) {
        // Ignore errors here; the semantic analyzer already has the
        // authoritative variant list, and EnumTable is just a helper.
        _ = self.enum_table.registerEnum(enum_name, variants) catch {};
    }
}

pub fn registerGroupType(self: *SemanticAnalyzer, group_name: []const u8, members: []const ast.GroupMember) !void {
    if (members.len == 0) {
        self.reporter.reportCompileError(
            .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
            ErrorCode.EXPECTED_EXPRESSION,
            "Group '{s}' must have at least one member",
            .{group_name},
        );
        self.fatal_error = true;
        return;
    }

    var member_infos = try self.allocator.alloc(Types.GroupMemberSource, members.len);
    errdefer self.allocator.free(member_infos);

    for (members, 0..) |member, mi| {
        const qualified_name = try buildQualifiedName(self.allocator, member.path);
        defer self.allocator.free(qualified_name);

        member_infos[mi] = .{
            .qualifier = try self.allocator.dupe(u8, member.qualifier),
            .source_name = try self.allocator.dupe(u8, qualified_name),
        };
    }

    const custom_type = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, group_name),
        .kind = .Group,
        .group_members = member_infos,
    };
    try self.custom_types.put(group_name, custom_type);

    const mem_group = CustomTypeInfo{
        .name = try self.allocator.dupe(u8, group_name),
        .kind = .Group,
        .group_members = member_infos,
    };
    try self.memory.registerCustomType(mem_group);

    // Flatten and register in GroupTable
    var flat_members = std.ArrayListUnmanaged(GroupTable.Member){};
    defer flat_members.deinit(self.allocator);

    var visited = std.StringHashMapUnmanaged(void){};
    defer visited.deinit(self.allocator);

    var seen = std.AutoHashMapUnmanaged(MemberKey, void){};
    defer seen.deinit(self.allocator);

    try flattenGroupMembers(self, members, &flat_members, &visited, &seen);

    // Check for duplicate qualifiers after flattening
    var qualifiers = std.StringHashMapUnmanaged(void){};
    defer qualifiers.deinit(self.allocator);
    for (flat_members.items) |fm| {
        if (qualifiers.contains(fm.qualifier)) {
            self.reporter.reportCompileError(
                .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
                ErrorCode.TYPE_MISMATCH,
                "Group '{s}' has duplicate qualifier '{s}'",
                .{ group_name, fm.qualifier },
            );
            self.fatal_error = true;
        }
        try qualifiers.put(self.allocator, fm.qualifier, {});
    }

    _ = try self.group_table.registerGroup(group_name, flat_members.items);
}

const MemberKey = struct {
    kind: GroupTable.MemberKind,
    id: u32,
};

fn flattenGroupMembers(
    self: *SemanticAnalyzer,
    members: []const ast.GroupMember,
    flat: *std.ArrayListUnmanaged(GroupTable.Member),
    visited: *std.StringHashMapUnmanaged(void),
    seen: *std.AutoHashMapUnmanaged(MemberKey, void),
) !void {
    for (members) |member| {
        const qualified_name = try buildQualifiedName(self.allocator, member.path);
        defer self.allocator.free(qualified_name);

        // Resolve the member type
        const ct = self.custom_types.get(qualified_name) orelse self.custom_types.get(member.qualifier);
        if (ct == null) {
            self.reporter.reportCompileError(
                .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
                ErrorCode.UNKNOWN_TYPE,
                "Group member '{s}' is not a declared type",
                .{qualified_name},
            );
            self.fatal_error = true;
            continue;
        }

        switch (ct.?.kind) {
            .Enum => {
                const id = self.enum_table.getIdByName(qualified_name) orelse self.enum_table.getIdByName(member.qualifier) orelse 0;
                const key = MemberKey{ .kind = .Enum, .id = id };
                if (seen.contains(key)) continue;
                try seen.put(self.allocator, key, {});
                try flat.append(self.allocator, .{
                    .qualifier = try self.allocator.dupe(u8, member.qualifier),
                    .kind = .Enum,
                    .id = id,
                });
            },
            .Struct => {
                const id = self.struct_table.getIdByName(qualified_name) orelse self.struct_table.getIdByName(member.qualifier) orelse 0;
                const key = MemberKey{ .kind = .Struct, .id = id };
                if (seen.contains(key)) continue;
                try seen.put(self.allocator, key, {});
                try flat.append(self.allocator, .{
                    .qualifier = try self.allocator.dupe(u8, member.qualifier),
                    .kind = .Struct,
                    .id = id,
                });
            },
            .Group => {
                // Cycle detection
                if (visited.contains(qualified_name)) {
                    self.reporter.reportCompileError(
                        .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
                        ErrorCode.TYPE_MISMATCH,
                        "Cycle detected in group: '{s}' includes itself transitively",
                        .{qualified_name},
                    );
                    self.fatal_error = true;
                    continue;
                }
                try visited.put(self.allocator, qualified_name, {});

                if (ct.?.group_members) |nested_members| {
                    try flattenGroupMemberSources(self, nested_members, flat, visited, seen);
                }
            },
        }
    }
}

fn flattenGroupMemberSources(
    self: *SemanticAnalyzer,
    members: []const Types.GroupMemberSource,
    flat: *std.ArrayListUnmanaged(GroupTable.Member),
    visited: *std.StringHashMapUnmanaged(void),
    seen: *std.AutoHashMapUnmanaged(MemberKey, void),
) !void {
    for (members) |member| {
        const qualified_name = member.source_name;

        const ct = self.custom_types.get(qualified_name) orelse self.custom_types.get(member.qualifier);
        if (ct == null) {
            self.reporter.reportCompileError(
                .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
                ErrorCode.UNKNOWN_TYPE,
                "Group member '{s}' is not a declared type",
                .{qualified_name},
            );
            self.fatal_error = true;
            continue;
        }

        switch (ct.?.kind) {
            .Enum => {
                const id = self.enum_table.getIdByName(qualified_name) orelse self.enum_table.getIdByName(member.qualifier) orelse 0;
                const key = MemberKey{ .kind = .Enum, .id = id };
                if (seen.contains(key)) continue;
                try seen.put(self.allocator, key, {});
                try flat.append(self.allocator, .{
                    .qualifier = try self.allocator.dupe(u8, member.qualifier),
                    .kind = .Enum,
                    .id = id,
                });
            },
            .Struct => {
                const id = self.struct_table.getIdByName(qualified_name) orelse self.struct_table.getIdByName(member.qualifier) orelse 0;
                const key = MemberKey{ .kind = .Struct, .id = id };
                if (seen.contains(key)) continue;
                try seen.put(self.allocator, key, {});
                try flat.append(self.allocator, .{
                    .qualifier = try self.allocator.dupe(u8, member.qualifier),
                    .kind = .Struct,
                    .id = id,
                });
            },
            .Group => {
                if (visited.contains(qualified_name)) {
                    self.reporter.reportCompileError(
                        .{ .file = "", .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } },
                        ErrorCode.TYPE_MISMATCH,
                        "Cycle detected in group: '{s}' includes itself transitively",
                        .{qualified_name},
                    );
                    self.fatal_error = true;
                    continue;
                }
                try visited.put(self.allocator, qualified_name, {});

                if (ct.?.group_members) |nested_members| {
                    try flattenGroupMemberSources(self, nested_members, flat, visited, seen);
                }
            },
        }
    }
}

fn buildQualifiedName(allocator: std.mem.Allocator, path: []const Token) ![]const u8 {
    if (path.len == 0) return "";
    var total_len: usize = path[0].lexeme.len;
    for (path[1..]) |token| {
        total_len += 1 + token.lexeme.len;
    }
    var result = try allocator.alloc(u8, total_len);
    @memcpy(result[0..path[0].lexeme.len], path[0].lexeme);
    var offset: usize = path[0].lexeme.len;
    for (path[1..]) |token| {
        result[offset] = '.';
        offset += 1;
        @memcpy(result[offset .. offset + token.lexeme.len], token.lexeme);
        offset += token.lexeme.len;
    }
    return result;
}

/// Helper function to get location from AST base
pub fn getLocationFromBase(base: ast.Base) Reporting.Location {
    return base.location();
}


