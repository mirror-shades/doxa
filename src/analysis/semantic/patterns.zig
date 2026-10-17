//! Match patterns, narrowing views, and group member lookup.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Memory = @import("../../utils/memory.zig");
const Scope = Memory.Scope;
const Variable = Memory.Variable;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const names = @import("names.zig");
const eval = @import("eval_utils.zig");
const infer_type = @import("infer_type.zig");
const TokenImport = @import("../../types/token.zig");
const Token = TokenImport.Token;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;

/// Resolve, in place, what every pattern of `case` denotes, so the arm's
/// narrowing here and its lowering in codegen read one answer. `group` is
/// the subject's group and `subject_enum` its enum, when it is one.
pub fn resolveMatchPatterns(self: *SemanticAnalyzer, case: *ast.MatchCase, group: ?TypeRef, subject_enum: ?TypeRef) ErrorList!void {
    const resolved = try self.allocator.alloc(ast.MatchCase.Resolved, case.patterns.len);
    for (case.patterns, resolved) |pattern, *slot| {
        slot.* = if (pattern.type != .IDENTIFIER or std.mem.eql(u8, pattern.lexeme, "else"))
            .token
        else if (subject_enum) |enum_ref|
            .{ .variant = enum_ref }
        else if (group) |group_ref|
            if (try self.groupMemberType(group_ref, pattern.lexeme)) |ref| .{ .type = ref } else .token
        else if (try names.resolveTypeName(self, pattern.lexeme, self.current_module)) |ref|
            .{ .type = ref }
        else
            .token;
    }
    for (case.path_patterns) |path| {
        if (path.tokens.len == 0) continue;
        resolved[path.pattern] = try resolvePathPattern(self, path, group);
        if (path.field_names.len > 0) try checkDestructure(self, path, resolved[path.pattern]);
    }
    case.resolved = resolved;
}

/// A destructuring path must name a struct declaring every field it binds.
pub fn checkDestructure(self: *SemanticAnalyzer, path: ast.MatchCase.PathPattern, resolved: ast.MatchCase.Resolved) ErrorList!void {
    const location = ast.SourceSpan.fromToken(path.tokens[path.tokens.len - 1]).location;
    const struct_id = switch (resolved) {
        .type => |ref| blk: {
            try self.ensureTypes(ref.module);
            break :blk self.struct_table.idOf(ref);
        },
        .token, .variant => null,
    } orelse {
        self.reporter.reportCompileError(location, ErrorCode.UNKNOWN_TYPE, "Unknown struct type '{s}'", .{try joinTokens(self, path.tokens)});
        self.fatal_error = true;
        return;
    };
    const fields = self.struct_table.fields(struct_id) orelse &.{};
    for (path.field_names) |field_token| {
        for (fields) |field| {
            if (std.mem.eql(u8, field.name, field_token.lexeme)) break;
        } else {
            self.reporter.reportCompileError(
                ast.SourceSpan.fromToken(field_token).location,
                ErrorCode.FIELD_NOT_FOUND,
                "Struct '{s}' has no field '{s}'",
                .{ resolved.type.name, field_token.lexeme },
            );
            self.fatal_error = true;
        }
    }
}

/// Narrow the matched variable within one match arm, then infer the arm's
/// body. The arm's patterns were resolved by `resolveMatchPatterns`.
pub fn inferMatchCaseTypeWithNarrow(self: *SemanticAnalyzer, case: ast.MatchCase, subject_type: *const ast.TypeInfo, matched_var_name: ?[]const u8) ErrorList!*ast.TypeInfo {
    // A per-case scope keeps narrowing from leaking out of the arm.
    const case_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
    defer case_scope.deinit();

    const prev_scope = self.current_scope;
    self.current_scope = case_scope;
    defer self.current_scope = prev_scope;

    if (matched_var_name) |name| {
        // An arm narrows the subject only when it lists a single pattern: with
        // several, any of them could have selected it, so the body sees the
        // subject as declared. Codegen reads the narrowing from each read of
        // the subject; nothing re-derives it.
        const narrows = case.patterns.len == 1;

        if (case.path_patterns.len > 0) {
            const path = case.path_patterns[0];
            if (path.tokens.len >= 1) {
                const member: ?TypeRef = switch (case.resolved[path.pattern]) {
                    .type => |ref| ref,
                    .token, .variant => null,
                };
                if (narrows) {
                    if (member) |ref| (try bindNarrowed(case_scope, name, .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false }, self.allocator)).is_view = true;
                }

                // Struct destructuring binds each field name as a local.
                // These are the arm's own names, not the subject, so they
                // bind whether or not the arm narrows.
                if (path.field_names.len > 0) {
                    if (member) |ref| if (self.struct_table.idOf(ref)) |sid| {
                        const fields = self.struct_table.fields(sid) orelse &.{};
                        const storages = try self.allocator.alloc(u32, path.field_names.len);
                        for (path.field_names, storages) |field_token, *storage| {
                            // An undeclared field is `checkDestructure`'s
                            // error, which stops the compile before codegen.
                            const field = for (fields) |f| {
                                if (std.mem.eql(u8, f.name, field_token.lexeme)) break f;
                            } else continue;
                            self.checkFreshName(case_scope, field_token);
                            storage.* = (try bindNarrowed(case_scope, field_token.lexeme, field.type_info.*, self.allocator)).storage_id;
                        }
                        case.path_patterns[0].field_storages = storages;
                    };
                }
            }
        } else if (narrows) {
            if (try patternType(self, case.patterns[0], case.resolved[0], subject_type)) |narrowed| {
                (try bindNarrowed(case_scope, name, narrowed, self.allocator)).is_view = true;
            }
        }
    }

    const case_body_type = try infer_type.inferTypeFromExpr(self, case.body);
    if (matched_var_name) |name| case_scope.propagateUsedToParent(name);
    return case_body_type;
}

pub fn bindNarrowed(scope: *Scope, name: []const u8, narrowed: ast.TypeInfo, allocator: std.mem.Allocator) ErrorList!*Variable {
    const type_info = try ast.TypeInfo.createDefault(allocator);
    type_info.* = narrowed;
    return scope.createValueBinding(name, eval.convertTypeToTokenType(type_info.base), type_info, false);
}

/// What a match path denotes. Against a group subject the path names a
/// member, split exactly the way codegen splits it: through the group
/// qualifier when written through it (`Group.Member.Variant`), otherwise
/// the token ahead of the variant. Otherwise the path spells a type
/// outright — bare or module-qualified (`std.json.Node`) — or an enum
/// variant (`ns.Color.Red`), whose enum is everything ahead of the variant.
pub fn resolvePathPattern(self: *SemanticAnalyzer, path: ast.MatchCase.PathPattern, group: ?TypeRef) ErrorList!ast.MatchCase.Resolved {
    if (group) |group_ref| {
        const split = path.split(group_ref.name);
        if (try self.groupMemberType(group_ref, split.member.lexeme)) |ref| return .{ .type = ref };
    }
    if (try names.resolveTypeName(self, try joinTokens(self, path.tokens), self.current_module)) |ref| return .{ .type = ref };
    if (path.field_names.len > 0 or path.tokens.len < 2) return .token;
    const prefix = try joinTokens(self, path.tokens[0 .. path.tokens.len - 1]);
    const ref = (try names.resolveTypeName(self, prefix, self.current_module)) orelse return .token;
    const custom = (try self.customType(ref)) orelse return .token;
    return if (custom.kind == .Enum) .{ .variant = ref } else .token;
}

/// The member of `group` written with `qualifier`.
pub fn groupMemberType(self: *SemanticAnalyzer, group: TypeRef, qualifier: []const u8) ErrorList!?TypeRef {
    try self.ensureTypes(group.module);
    const id = self.group_table.idOf(group) orelse return null;
    for (self.group_table.members(id) orelse &.{}) |member| {
        if (std.mem.eql(u8, member.qualifier, qualifier)) return member.ref;
    }
    return null;
}

pub fn joinTokens(self: *SemanticAnalyzer, tokens: []const ast.Token) ErrorList![]const u8 {
    var out = std.array_list.Managed(u8).init(self.allocator);
    for (tokens, 0..) |tok, i| {
        if (i > 0) try out.append('.');
        try out.appendSlice(tok.lexeme);
    }
    return out.toOwnedSlice();
}

/// The type a bare match pattern narrows to, or null when the pattern
/// cannot be mapped to a type: binding the subject to `nothing` would make
/// every use of it in the arm a type error. `resolved` is what analysis
/// resolved the pattern to.
pub fn patternType(self: *SemanticAnalyzer, pattern: Token, resolved: ast.MatchCase.Resolved, subject_type: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
    if (std.mem.indexOf(u8, pattern.lexeme, "[]") != null) return arrayPatternMember(self, pattern.lexeme, subject_type);
    return switch (pattern.type) {
        .INT_TYPE, .INT => .{ .base = .Int, .is_mutable = false },
        .FLOAT_TYPE, .FLOAT => .{ .base = .Float, .is_mutable = false },
        .STRING_TYPE, .STRING => .{ .base = .String, .is_mutable = false },
        .BYTE_TYPE, .BYTE => .{ .base = .Byte, .is_mutable = false },
        .TETRA_TYPE, .TETRA => .{ .base = .Tetra, .is_mutable = false },
        .NOTHING_TYPE, .NOTHING => .{ .base = .Nothing, .is_mutable = false },
        .DOT => .{ .base = .Enum, .is_mutable = false }, // an enum member pattern implies an enum
        .IDENTIFIER => switch (resolved) {
            .type => |ref| .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false },
            .variant => |ref| .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false },
            .token => null,
        },
        else => null,
    };
}

/// The member of `subject_type` an array pattern names. A pattern spells
/// an array by its element (`string[]`, `Point[][]`), so the narrowed type
/// is the subject's member of that spelling, element type and all. Null
/// when the subject has no such member: the arm can never be selected.
pub fn arrayPatternMember(self: *SemanticAnalyzer, spelling: []const u8, subject_type: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
    const members: []const *ast.TypeInfo = if (subject_type.union_type) |u| u.types else &.{};
    for (members) |member| {
        if (member.base != .Array) continue;
        var written: std.ArrayListUnmanaged(u8) = .empty;
        defer written.deinit(self.allocator);
        if (!try writeArraySpelling(self.allocator, &written, member)) continue;
        if (std.mem.eql(u8, written.items, spelling)) {
            var narrowed = member.*;
            narrowed.is_mutable = false;
            return narrowed;
        }
    }
    return null;
}

/// Spell an array type as a pattern writes it, or return false for an
/// element no pattern can name.
pub fn writeArraySpelling(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), t: *const ast.TypeInfo) !bool {
    const element = t.array_type orelse return false;
    if (element.base == .Array) {
        if (!try writeArraySpelling(allocator, out, element)) return false;
    } else if (element.custom_type) |custom| {
        try out.appendSlice(allocator, custom.displayName());
    } else {
        try out.appendSlice(allocator, switch (element.base) {
            .Int => "int",
            .Float => "float",
            .String => "string",
            .Byte => "byte",
            .Tetra => "tetra",
            else => return false,
        });
    }
    try out.appendSlice(allocator, "[]");
    return true;
}
